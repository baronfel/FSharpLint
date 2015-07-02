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

module TestConstraintSolver

open NUnit.Framework
open FSharpLint.Rules.ConstraintSolver
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.LoadVisitors

let config = 
    Map.ofList 
        [ 
            (AnalyserName, 
                { 
                    Rules = Map.ofList []
                    Settings = Map.ofList []
                }) 
        ]

[<TestFixture>]
type TestConstraintSolverRules() =
    inherit TestRuleBase.TestRuleBase(Ast(initialVisitor), config)

    [<Test>]
    member this.TestAddSymbol() = 
        let symbolTable = SymbolTable.NewRootScopeSymbolTable()

        let symbol = symbolTable.AddDeclaration "ident"

        Assert.AreEqual("ident_1_1", symbol)

    [<Test>]
    member this.TestAddShadowedSymbol() = 
        let symbolTable = SymbolTable.NewRootScopeSymbolTable()

        let symbol = symbolTable.AddDeclaration "ident"

        let shadowingSymbol = symbolTable.AddDeclaration "ident"

        Assert.AreEqual("ident_2_1", shadowingSymbol)

    [<Test>]
    member this.TestAddSymbolToChildScope() = 
        let symbolTable = SymbolTable.NewRootScopeSymbolTable()

        let childScope = symbolTable.NewChildScope()

        childScope.AddDeclaration "ident" |> ignore

        Assert.AreEqual(Some("ident_1_2"), childScope.GetSymbol "ident")

    [<Test>]
    member this.TestGetSymbolInParentScope() = 
        let symbolTable = SymbolTable.NewRootScopeSymbolTable()

        symbolTable.AddDeclaration "ident" |> ignore

        let childScope = symbolTable.NewChildScope()

        Assert.AreEqual(Some("ident_1_1"), childScope.GetSymbol "ident")

    [<Test>]
    member this.TestGetSymbolInNoScope() = 
        let symbolTable = SymbolTable.NewRootScopeSymbolTable()

        let symbol = symbolTable.GetSymbol "ident"

        Option.isNone symbol |> Assert.IsTrue

    [<Test>]
    member this.TestSymbolInChildScopeCannotBeAccessedInParentSymbolTable() = 
        let symbolTable = SymbolTable.NewRootScopeSymbolTable()

        let childScope = symbolTable.NewChildScope()

        childScope.AddDeclaration "ident" |> ignore

        let symbol = symbolTable.GetSymbol "ident"

        Option.isNone symbol |> Assert.IsTrue