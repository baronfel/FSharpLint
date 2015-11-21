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

module TestExpressionUtilities

open NUnit.Framework
open FSharpLint.Framework.ExpressionUtilities
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Configuration
open FSharpLint.Framework.ParseFile
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

let stringToExpression str =
    let stubConfig =
        { UseTypeChecker = Some(false)
          IgnoreFiles = 
            Some({ Update = IgnoreFiles.Overwrite
                   Files = []
                   Content = "" })
          Analysers = Map.ofList [] }

    let (|Module|_|) x =
        match x with
        | SynModuleOrNamespace(_, _, SynModuleDecl.DoExpr(_, app, _)::_, _, _, _, _) ->
            Some(app)
        | _ -> None

    match parseSource str stubConfig (FSharpChecker.Create()) with
    | ParseFileResult.Success(result) ->
        match result.Ast with
        | ParsedInput.ImplFile(x) -> 
            match x with 
            | ParsedImplFileInput(_, _, _, _, _, Module(app)::_, _) -> 
                app 
            | _ -> failwith "Expected at least one module or namespace."
        | _ -> failwith "Expected an implementation file."
    | _ -> failwith "Failed to parse input."

let astNodeName = string >> (fun x -> x.Substring(x.LastIndexOf("+") + 1))

[<TestFixture>]
type TestExpressionUtilities() =

    [<Test>]
    member __.``Flatten with right pipe adds lhs to end of function application.``() = 
        let result =
            stringToExpression "x |> List.map (fun x -> x)"
            |> flattenFunctionApplication
            |> List.map astNodeName

        Assert.AreEqual(["LongIdent"; "Lambda"; "Ident"], result)

    [<Test>]
    member __.``Flatten with left pipe adds rhs to end of function application.``() = 
        let result =
            stringToExpression "List.map (fun x -> x) <| x"
            |> flattenFunctionApplication
            |> List.map astNodeName

        Assert.AreEqual(["LongIdent"; "Lambda"; "Ident"], result)

    [<Test>]
    member __.``Flatten with right pipe adds lhs to end of function application no matter the number of arguments on rhs.``() = 
        let result =
            stringToExpression "x |> List.map (fun x -> x) 1"
            |> flattenFunctionApplication
            |> List.map astNodeName
            
        Assert.AreEqual(["LongIdent"; "Lambda"; "Const"; "Ident"], result)

    [<Test>]
    member __.``Flatten with binary operator on lhs of right pipe.``() = 
        let result =
            stringToExpression "x::[] |> List.map (fun x -> x)"
            |> flattenFunctionApplication
            |> List.map astNodeName
            
        Assert.AreEqual(["LongIdent"; "Lambda"; "App"], result)

    [<Test>]
    member __.``Flatten with function application on lhs of right pipe.``() = 
        let result =
            stringToExpression "(foo x) |> List.map (fun x -> x)"
            |> flattenFunctionApplication
            |> List.map astNodeName
            
        Assert.AreEqual(["LongIdent"; "Lambda"; "App"], result)

    [<Test>]
    member __.``Flatten with multiple right pipes.``() = 
        let result =
            stringToExpression "x |> foo |> List.map (fun x -> x)"
            |> flattenFunctionApplication
            |> List.map astNodeName
            
        Assert.AreEqual(["LongIdent"; "Lambda"; "App"], result)

    [<Test>]
    member __.``Flatten with multiple left pipes.``() = 
        let result =
            stringToExpression "List.map (fun x -> x) <| 1 <| x"
            |> flattenFunctionApplication
            |> List.map astNodeName
            
        Assert.AreEqual(["LongIdent"; "Lambda"; "Const"; "Ident"], result)