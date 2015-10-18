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

namespace FSharpLint.Rules

module CyclomaticComplexity =
    
    open Microsoft.FSharp.Compiler.Ast
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.Configuration
    open FSharpLint.Framework.LoadVisitors

    [<Literal>]
    let AnalyserName = "CyclomaticComplexity"

    let configMaxCyclomaticComplexity config =
        match isAnalyserEnabled config AnalyserName with
            | Some(analyserSettings) when analyserSettings.ContainsKey "MaxCyclomaticComplexity" ->
                match analyserSettings.["MaxCyclomaticComplexity"] with
                    | MaxCyclomaticComplexity(l) -> Some(l)
                    | _ -> None
            | Some(_)
            | None -> None

    let configIncludeMatchStatements config =
        match isAnalyserEnabled config AnalyserName with
            | Some(analyserSettings) when analyserSettings.ContainsKey "IncludeMatchStatements" ->
                match analyserSettings.["IncludeMatchStatements"] with
                    | IncludeMatchStatements(includeMatchStatements) -> includeMatchStatements
                    | _ -> true
            | Some(_)
            | None -> true
    
    let rec countDecisionPathsVisitor visitorInfo checkFile range count astNode : VisitorResult = 
        let visitor = countDecisionPathsVisitor visitorInfo checkFile range
        let finishedWalk = finishedWalk visitorInfo range

        match astNode.Node with
            | AstNode.Expression(expression) ->
                match expression with
                    | SynExpr.For(_)
                    | SynExpr.ForEach(_)
                    | SynExpr.While(_)
                    | SynExpr.IfThenElse(_)  -> 
                        WalkWithVisitor(visitor (count + 1), finishedWalk (count + 1))
                    | SynExpr.Ident(ident) 
                            when 
                                ident.idText = "op_BooleanAnd" || 
                                ident.idText = "op_BooleanOr" ->

                        WalkWithVisitor(visitor (count + 1), finishedWalk (count + 1))
                    | SynExpr.Match(_, _, matchClauses, _, _) 
                            when configIncludeMatchStatements visitorInfo.Config -> 

                        let count = count + List.length matchClauses - 1
                        WalkWithVisitor(visitor count, finishedWalk count)
                    | _ -> 
                        WalkWithVisitor(visitor count, finishedWalk count)
            | _ -> 
                WalkWithVisitor(visitor count, finishedWalk count)
    and
        finishedWalk visitorInfo range count () = 
            match configMaxCyclomaticComplexity visitorInfo.Config with
                | Some(maxCount) when count > maxCount ->
                    let errorFormatString = FSharpLint.Framework.Resources.GetString("RulesCyclomaticComplexityError")
                    let error = System.String.Format(errorFormatString, count, maxCount)

                    visitorInfo.PostError range error
                | _ -> ()
    and 
        findBindingVisitor visitorInfo checkFile (astNode:CurrentNode) : VisitorResult =
            match astNode.Node with
                | AstNode.Binding(SynBinding.Binding(_, _, _, _, _, _, _, _, _, expr, _, _)) when astNode.IsSuppressed(AnalyserName) |> not ->
                    let getVisitorForChild _ child =
                        match child with
                            | AstNode.Expression(_) ->
                                Some(countDecisionPathsVisitor visitorInfo checkFile expr.Range 0)
                            | _ -> 
                                Some(findBindingVisitor visitorInfo checkFile)

                    ContinueWithVisitorsForChildren(getVisitorForChild)
                | _ -> Continue

    type RegisterCyclomaticComplexityVisitor() = 
        let plugin =
            {
                Name = AnalyserName
                Visitor = Ast(findBindingVisitor)
            }

        interface IRegisterPlugin with
            member __.RegisterPlugin _ = plugin