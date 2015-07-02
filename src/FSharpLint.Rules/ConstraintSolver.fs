(*
    FSharpLint, a linter for F#.
    Copyright (C) 2014 Matthew Mcveigh

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

namespace FSharpLint.Rules

module ConstraintSolver =
    
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.Range
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open Microsoft.Z3
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration
    open FSharpLint.Framework.LoadVisitors

    type Scope = System.Collections.Generic.Dictionary<string, int>

    type Scopes = Scope list

    type SymbolTable private (parentScopes:Scopes) = 
        let scopes = Scope()::parentScopes

        let scopeLevel = List.length scopes

        let generateSymbol identifier numberOfLocalIdentsShadowed scopeLevel =
            sprintf "%s_%d_%d" identifier numberOfLocalIdentsShadowed scopeLevel

        let numberOfLocalDeclarations (scope:Scope) identifier =
            match scope.TryGetValue identifier with
                | (true, numberOfIdentsShadowed) -> numberOfIdentsShadowed
                | (false, _) -> 0

        member this.GetSymbol(identifier) =
            let rec getSymbol = function
                | (scope:Scope)::parentScopes ->
                    let numLocalDecl = numberOfLocalDeclarations scope identifier

                    if numLocalDecl > 0 then
                        let scopeLevel = List.length parentScopes + 1
                        Some(generateSymbol identifier numLocalDecl scopeLevel)
                    else
                        getSymbol parentScopes
                | [] -> None

            getSymbol scopes

        member this.AddDeclaration(identifier) =
            match scopes with
                | scope::_ -> 
                    let numLocalDecl = numberOfLocalDeclarations scope identifier
                
                    if numLocalDecl > 0 then
                        scope.[identifier] <- numLocalDecl + 1
                    else
                        scope.Add(identifier, 1)
                        
                    generateSymbol identifier (numLocalDecl + 1) scopeLevel
                | [] -> 
                    System.InvalidOperationException("There were no scopes in the symbol table.")
                        |> raise

        member this.NewChildScope() =
            SymbolTable(scopes)

        static member NewRootScopeSymbolTable() = 
            SymbolTable([])

    [<Literal>]
    let AnalyserName = "ConstraintSolver"

    let isRuleEnabled config ruleName =
        isRuleEnabled config AnalyserName ruleName |> Option.isSome

    let rec visitor (context:Context) visitorInfo checkFile (astNode:CurrentNode) = 
        Continue

    let initialVisitor visitorInfo checkFile (astNode:CurrentNode) = 
        use context = new Context()
        ContinueWithVisitor (visitor context visitorInfo checkFile)

    type RegisterBindingVisitor() = 
        let plugin =
            {
                Name = AnalyserName
                Visitor = Ast(initialVisitor)
            }

        interface IRegisterPlugin with
            member this.RegisterPlugin with get() = plugin