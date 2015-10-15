﻿(*
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

namespace FSharpLint.Framework

module ExpressionUtilities =

    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.SourceCodeServices

    let (|Identifier|_|) = function
        | SynExpr.Ident(ident) -> Some([ident], ident.idRange)
        | SynExpr.LongIdent(_, longIdent, _, _) -> Some(longIdent.Lid, longIdent.Range)
        | _ -> None

    let getSymbolFromIdent (checkFile:FSharpCheckFileResults option) expr =
        match checkFile, expr with
            | Some(checkFile), Identifier(ident, range) ->
                let identNames = ident |> List.map (fun x -> x.idText)

                checkFile.GetSymbolUseAtLocation(
                    range.StartLine, 
                    range.EndColumn, 
                    "", 
                    identNames) |> Async.RunSynchronously
            | _ -> None

    /// Converts an operator name e.g. op_Add to the operator symbol e.g. +
    let identAsDecompiledOpName (ident:Ident) =
        if ident.idText.StartsWith("op_") then
            Microsoft.FSharp.Compiler.PrettyNaming.DecompileOpName ident.idText
        else 
            ident.idText

    /// Extracts an expression from parentheses e.g. ((x + 4)) -> x + 4
    let rec removeParens = function
        | SynExpr.Paren(x, _, _, _) -> removeParens x
        | x -> x
        
    let flattenFunctionApplication expr =
        let rec flattenFunctionApplication exprs = function
            | SynExpr.App(_, _, x, y, _) -> 
                match removeParens x with
                | SynExpr.App(_, true, SynExpr.Ident(op), rightExpr, _) ->
                    match identAsDecompiledOpName op with
                    | "|>" | "||>" | "|||>" ->
                        let flattened = flattenFunctionApplication [] y
                        flattened@[rightExpr]
                    | "<|" | "<||" | "<|||" ->
                        let flattened = flattenFunctionApplication [] rightExpr
                        flattened@[y]
                    | _ -> 
                        flattenFunctionApplication (removeParens y::exprs) (removeParens x)
                | _ -> 
                    flattenFunctionApplication (removeParens y::exprs) (removeParens x)
            | x -> 
                x::exprs

        flattenFunctionApplication [] expr