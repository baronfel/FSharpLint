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
 
namespace FSharpLint.Framework
 
module HintMatcher =
 
    open System.Diagnostics
    open Microsoft.FSharp.Compiler.Ast
    open Microsoft.FSharp.Compiler.PrettyNaming
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open FSharpLint.Framework
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework.HintParser
    open FSharpLint.Framework.Configuration
    open FSharpLint.Framework.LoadVisitors
    open FSharpLint.Framework.ExpressionUtilities
 
    [<Literal>]
    let AnalyserName = "Hints"
 
    let rec extractSimplePatterns = function
        | SynSimplePats.SimplePats(simplePatterns, _) -> 
            simplePatterns
        | SynSimplePats.Typed(simplePatterns, _, _) -> 
            extractSimplePatterns simplePatterns
 
    let rec extractIdent = function
        | SynSimplePat.Id(ident, _, isCompilerGenerated, _, _, _) -> (ident, isCompilerGenerated)
        | SynSimplePat.Attrib(simplePattern, _, _)
        | SynSimplePat.Typed(simplePattern, _, _) -> extractIdent simplePattern
        
    [<RequireQualifiedAccess>]
    type LambdaArgumentMatch =
        | Variable of char * string
        /// bool = whether or not argument was actually a wildcard (could be false if the hint argument is a wildcard but the actual argument was not).
        | Wildcard of bool
        | NoMatch
 
    let matchLambdaArgument argument argumentToMatch = 
        match extractSimplePatterns argument with
        | [] -> LambdaArgumentMatch.NoMatch
        | simplePattern::_ ->
            let identifier, isCompilerGenerated = extractIdent simplePattern
 
            let isWildcard = isCompilerGenerated && identifier.idText.StartsWith("_")
 
            match argumentToMatch with
            | Argument.Variable(variable) when not isWildcard -> 
                LambdaArgumentMatch.Variable(variable, identifier.idText)
            | Argument.Wildcard -> 
                LambdaArgumentMatch.Wildcard(isWildcard)
            | _ -> LambdaArgumentMatch.NoMatch            
            
    [<RequireQualifiedAccess>]
    type LambdaMatch =
        | Match of SynExpr * Map<char, string> * int
        | NoMatch
 
    let matchLambdaArguments arguments lambdaExpr =
        let rec matchLambdaArguments arguments matches numberOfWildcards = function
            | SynExpr.Lambda(_, _, argument, expr, _) -> 
                match arguments with
                | head :: tail ->
                    let atEndOfArguments = List.isEmpty tail
                    
                    match matchLambdaArgument argument head with
                    | LambdaArgumentMatch.Variable(var, ident) ->
                        let matches = (var, ident)::matches
                        if atEndOfArguments then
                            LambdaMatch.Match(expr, Map.ofList matches, numberOfWildcards)
                        else
                            matchLambdaArguments tail matches numberOfWildcards expr
                    | LambdaArgumentMatch.Wildcard(isWildcard) ->
                        let numberOfWildcards = numberOfWildcards + if isWildcard then 1 else 0
 
                        if atEndOfArguments then
                            LambdaMatch.Match(expr, Map.ofList matches, numberOfWildcards)
                        else
                            matchLambdaArguments tail matches numberOfWildcards expr
                    | LambdaArgumentMatch.NoMatch ->
                        LambdaMatch.NoMatch
                | [] -> 
                    LambdaMatch.NoMatch
            | _ -> 
                LambdaMatch.NoMatch
 
        matchLambdaArguments arguments [] 0 lambdaExpr
 
    /// Converts a SynConst (FSharp AST) into a Constant (hint AST).
    let matchConst = function
        | SynConst.Bool(x) -> Some(Constant.Bool(x))
        | SynConst.Int16(x) -> Some(Constant.Int16(x))
        | SynConst.Int32(x) -> Some(Constant.Int32(x))
        | SynConst.Int64(x) -> Some(Constant.Int64(x))
        | SynConst.UInt16(x) -> Some(Constant.UInt16(x))
        | SynConst.UInt32(x) -> Some(Constant.UInt32(x))
        | SynConst.UInt64(x) -> Some(Constant.UInt64(x))
        | SynConst.Byte(x) -> Some(Constant.Byte(x))
        | SynConst.Bytes(x, _) -> Some(Constant.Bytes(x))
        | SynConst.Char(x) -> Some(Constant.Char(x))
        | SynConst.Decimal(x) -> Some(Constant.Decimal(x))
        | SynConst.Double(x) -> Some(Constant.Double(x))
        | SynConst.SByte(x) -> Some(Constant.SByte(x))
        | SynConst.Single(x) -> Some(Constant.Single(x))
        | SynConst.String(x, _) -> Some(Constant.String(x))
        | SynConst.UIntPtr(x) -> Some(Constant.UIntPtr(unativeint x))
        | SynConst.IntPtr(x) -> Some(Constant.IntPtr(nativeint x))
        | SynConst.UserNum(x, endChar) -> 
            Some(Constant.UserNum(System.Numerics.BigInteger.Parse(x), endChar.[0]))
        | SynConst.Unit -> Some(Constant.Unit)
        | SynConst.UInt16s(_)
        | SynConst.Measure(_) -> None
 
    let lambdaArgumentsToString (arguments:Argument list) = 
        let argumentToString = function
            | Argument.Wildcard -> "_"
            | Argument.Variable(x) -> x.ToString()
 
        arguments
        |> List.map argumentToString
        |> String.concat " "
 
    let constantToString = function
        | Constant.Bool(x) -> if x then "true" else "false"
        | Constant.Int16(x) -> x.ToString() + "s"
        | Constant.Int32(x) -> x.ToString()
        | Constant.Int64(x) -> x.ToString() + "L"
        | Constant.UInt16(x) -> x.ToString() + "us"
        | Constant.UInt32(x) -> x.ToString() + "u"
        | Constant.UInt64(x) -> x.ToString() + "UL"
        | Constant.Byte(x) -> x.ToString() + "uy"
        | Constant.Bytes(x) -> x.ToString()
        | Constant.Char(x) -> "'" + x.ToString() + "'"
        | Constant.Decimal(x) -> x.ToString() + "m"
        | Constant.Double(x) -> x.ToString()
        | Constant.SByte(x) -> x.ToString() + "y"
        | Constant.Single(x) -> x.ToString() + "f"
        | Constant.String(x) -> "\"" + x + "\""
        | Constant.UIntPtr(x) -> x.ToString()
        | Constant.IntPtr(x) -> x.ToString()
        | Constant.UserNum(x, _) -> x.ToString()
        | Constant.Unit -> "()"
 
    let private surroundExpressionsString hintToString left right sep expressions =
        let inside =
            expressions 
            |> List.map hintToString
            |> String.concat sep
 
        left + inside + right
 
    let rec hintToString = function
        | Expression.Variable(x) -> x.ToString()
        | Expression.Wildcard -> "_"
        | Expression.Constant(constant) -> 
            constantToString constant
        | Expression.Identifier(identifier) ->
            identifier
            |> List.map DemangleOperatorName
            |> String.concat "."
        | Expression.FunctionApplication(expressions) ->
            expressions |> surroundExpressionsString hintToString "" "" " "
        | Expression.InfixOperator(operator, leftHint, rightHint) ->
            hintToString leftHint + operator + hintToString rightHint
        | Expression.PrefixOperator(operator, hint) ->
            operator + hintToString hint
        | Expression.Parentheses(hint) -> 
            "(" + hintToString hint + ")"
        | Expression.Lambda(lambda) -> 
            "fun " + lambdaArgumentsToString lambda.Arguments + " -> " + hintToString lambda.Body
        | Expression.Tuple(expressions) ->
            expressions |> surroundExpressionsString hintToString "(" ")" ","
        | Expression.List(expressions) ->
            expressions |> surroundExpressionsString hintToString "[" "]" ";"
        | Expression.Array(expressions) ->
            expressions |> surroundExpressionsString hintToString "[|" "|]" ";"
        | Expression.If(cond, expr, None) ->
            "if " + hintToString cond + " then " + hintToString expr
        | Expression.If(cond, expr, Some(elseExpr)) ->
            "if " + hintToString cond + " then " + hintToString expr +
            " else " + hintToString elseExpr
        | Expression.Null -> "null"
 
    let hintError hint visitorInfo range =
        let matched = hintToString hint.Match
 
        let error =
            match hint.Suggestion with
            | Suggestion.Expr(expr) -> 
                let suggestion = hintToString expr
                let errorFormatString = Resources.GetString("RulesHintRefactor")
                System.String.Format(errorFormatString, matched, suggestion)
            | Suggestion.Message(message) -> 
                let errorFormatString = Resources.GetString("RulesHintSuggestion")
                System.String.Format(errorFormatString, matched, message)
 
        visitorInfo.PostError range error
 
    let getMethodParameters (checkFile:FSharpCheckFileResults) (methodIdent:LongIdentWithDots) =
        let symbol =
            checkFile.GetSymbolUseAtLocation(
                methodIdent.Range.StartLine,
                methodIdent.Range.EndColumn,
                "", 
                methodIdent.Lid |> List.map (fun x -> x.idText))
                |> Async.RunSynchronously
 
        match symbol with
        | Some(symbol) when (symbol.Symbol :? FSharpMemberOrFunctionOrValue) -> 
            let symbol = symbol.Symbol :?> FSharpMemberOrFunctionOrValue
 
            if symbol.IsMember && (not << Seq.isEmpty) symbol.CurriedParameterGroups then
                symbol.CurriedParameterGroups.[0] |> Some
            else
                None
        | _ -> None

    /// Check a lambda function can be replaced with a function,
    /// it will not be if the lambda is automatically getting
    /// converted to a delegate type e.g. Func<T>.
    let lambdaCanBeReplacedWithFunction checkFile breadcrumbs (expr:SynExpr) =
        let isParameterDelegateType index methodIdent =
            match checkFile with
            | Some(checkFile) ->
                let parameters = getMethodParameters checkFile methodIdent
 
                match parameters with
                | Some(parameters) when index < Seq.length parameters ->
                    let parameter = parameters.[index]
 
                    parameter.Type.HasTypeDefinition &&
                    parameter.Type.TypeDefinition.IsDelegate
                | _ -> false
            | None ->
                /// When we're unable to check the parameters 
                /// fallback to say it is delegate type.
                true
 
        match breadcrumbs with
        | AstNode.Expression(SynExpr.Tuple(exprs, _, _))::_::AstNode.Expression(SynExpr.App(ExprAtomicFlag.Atomic, _, SynExpr.DotGet(_, _, methodIdent, _), _, _))::_ 
        | AstNode.Expression(SynExpr.Tuple(exprs, _, _))::_::AstNode.Expression(SynExpr.App(ExprAtomicFlag.Atomic, _, SynExpr.LongIdent(_, methodIdent, _, _), _, _))::_ -> 
            let index = exprs |> List.tryFindIndex (fun x -> x.Range = expr.Range)
 
            match index with
            | Some(index) -> not <| isParameterDelegateType index methodIdent
            | None -> false
        | AstNode.Expression(lambdaExpr)::AstNode.Expression(SynExpr.App(ExprAtomicFlag.Atomic, _, SynExpr.DotGet(_, _, methodIdent, _), arg, _))::_
        | AstNode.Expression(lambdaExpr)::AstNode.Expression(SynExpr.App(ExprAtomicFlag.Atomic, _, SynExpr.LongIdent(_, methodIdent, _, _), arg, _))::_ when arg.Range = lambdaExpr.Range -> 
            not <| isParameterDelegateType 0 methodIdent
        | _ -> true

    type private MaybeBuilder() =
        member __.Bind(v, f) = Option.bind f v
        member __.Return v = Some v
        member __.ReturnFrom o = o

    let private maybe = MaybeBuilder()
 
    module MatchExpression =
 
        type Arguments =
            { VisitorInfo: VisitorInfo
              LambdaArguments: Map<char, string>
              OriginalExpression: SynExpr
              Expression: SynExpr
              CheckFile: FSharpCheckFileResults option
              Hints: MergeSyntaxTrees.Node list
              FSharpCheckFileResults: FSharpCheckFileResults option
              Breadcrumbs: AstNode list }
 
            with 
                member this.SubHint(expr, hints) =
                    { this with Expression = expr; Hints = hints }
 
        let private matchExpr = function
            | SynExpr.Ident(ident) -> 
                let ident = identAsDecompiledOpName ident
                Some(MergeSyntaxTrees.Identifier([ident]))
            | SynExpr.LongIdent(_, ident, _, _) ->
                let identifier = ident.Lid |> List.map (fun x -> x.idText)
                Some(MergeSyntaxTrees.Identifier(identifier))
            | SynExpr.Const(constant, _) -> 
                matchConst constant |> Option.map MergeSyntaxTrees.Constant
            | SynExpr.Null(_) ->
                Some(MergeSyntaxTrees.Null)
            | _ -> None
 
        /// A match clause is generated by the compiler for each wildcard argument, 
        /// this function extracts the body expression of the lambda from those statements.
        let private removeAutoGeneratedMatchesFromLambda numberOfWildcards =
            let rec removeMatchClauses wildcard = function
                | x when wildcard = numberOfWildcards -> Some(x)
                | SynExpr.Match(SequencePointInfoForBinding.NoSequencePointAtInvisibleBinding, 
                                _, 
                                [SynMatchClause.Clause(SynPat.Wild(_), _, expr, _, _)], _, _) ->
                    removeMatchClauses (wildcard + 1) expr
                | _ -> None
 
            removeMatchClauses 0
 
        let private (|PossiblyInMethod|PossiblyInConstructor|NotInMethod|) breadcrumbs =
            let (|PossiblyMethodCallOrConstructor|_|) = function
                | SynExpr.App(_, false, _, _, _) -> Some()
                | _ -> None
 
            match breadcrumbs with
            | AstNode.Expression(SynExpr.Tuple(_))::_::AstNode.Expression(SynExpr.New(_))::_
            | _::AstNode.Expression(SynExpr.New(_))::_ ->
                PossiblyInConstructor
            | _::AstNode.Expression(PossiblyMethodCallOrConstructor)::_
            | AstNode.Expression(SynExpr.Tuple(_))::_::AstNode.Expression(PossiblyMethodCallOrConstructor)::_ -> 
                PossiblyInMethod
            | _ -> NotInMethod
 
        /// Check that an infix equality operation is not actually the assignment of a value to a property in a constructor
        /// or a named parameter in a method call.
        let private notPropertyInitialisationOrNamedParameter arguments leftExpr opExpr =
            match (leftExpr, opExpr) with 
            | SynExpr.Ident(ident), SynExpr.Ident(opIdent) when opIdent.idText = "op_Equality" ->
                match arguments.FSharpCheckFileResults with
                | Some(checkFile) ->
                    let symbolUse = 
                        checkFile.GetSymbolUseAtLocation(ident.idRange.StartLine, 
                                                         ident.idRange.EndColumn, 
                                                         "", 
                                                         [ident.idText])
                        |> Async.RunSynchronously
                                    
                    match symbolUse with
                    | Some(symbolUse) ->
                        match symbolUse.Symbol with
                        | :? FSharpParameter -> false
                        | :? FSharpMemberOrFunctionOrValue as x -> not x.IsProperty
                        | _ -> true
                    | None -> true
                | None -> 
                    /// Check if in `new` expr or function application (either could be a constructor).
                    match arguments.Breadcrumbs with
                    | PossiblyInMethod 
                    | PossiblyInConstructor -> 
                        false
                    | _ -> true
            | _ -> true
 
        let rec matchHintExpr arguments : MergeSyntaxTrees.Node list option =
            let expr = removeParens arguments.Expression
            let arguments = { arguments with Expression = expr }
 
            let matches =
                arguments.Hints
                |> List.map (fun x ->
                match x.Match with
                | MergeSyntaxTrees.Variable(variable) when arguments.LambdaArguments |> Map.containsKey variable ->
                    match expr with
                    | SynExpr.Ident(identifier) when identifier.idText = arguments.LambdaArguments.[variable] -> 
                        Some(x, x.Edges)
                    | _ -> None
                | MergeSyntaxTrees.Variable(_)
                | MergeSyntaxTrees.Wildcard ->
                    Some(x, x.Edges)
                | MergeSyntaxTrees.Null
                | MergeSyntaxTrees.Constant(_)
                | MergeSyntaxTrees.Identifier(_) ->
                    if matchExpr expr = Some(x.Match) then Some(x, x.Edges)
                    else None
                | MergeSyntaxTrees.FunctionApplication(_) ->
                    matchFunctionApplication arguments x.Edges
                    |> Option.bind (fun edges -> Some(x, edges))
                | MergeSyntaxTrees.InfixOperator(op) ->
                    matchInfixOperation arguments op x.Edges
                    |> Option.bind (fun edges -> Some(x, edges))
                | MergeSyntaxTrees.PrefixOperator(op) ->
                    matchPrefixOperation arguments op x.Edges
                    |> Option.bind (fun edges -> Some(x, edges))
                | MergeSyntaxTrees.Parentheses -> 
                    arguments.SubHint(expr, x.Edges) 
                    |> matchHintExpr 
                    |> Option.bind (fun edges -> Some(x, edges))
                | MergeSyntaxTrees.Lambda(args) -> 
                    matchLambda arguments args x.Edges
                    |> Option.bind (fun edges -> Some(x, edges))
                | MergeSyntaxTrees.Tuple(_) ->
                    matchTuple arguments x.Edges
                    |> Option.bind (fun edges -> Some(x, edges))
                | MergeSyntaxTrees.List(_) ->
                    matchList arguments x.Edges
                    |> Option.bind (fun edges -> Some(x, edges))
                | MergeSyntaxTrees.Array(_) ->
                    matchArray arguments x.Edges
                    |> Option.bind (fun edges -> Some(x, edges))
                | MergeSyntaxTrees.If(_) ->
                    matchIf arguments x.Edges
                    |> Option.bind (fun edges -> Some(x, edges)))
                |> List.choose id

            for hint in matches |> Seq.map fst |> Seq.collect (fun x -> x.Suggestions) do
                let expr = arguments.OriginalExpression
                
                match hint.Match, hint.Suggestion with
                | Expression.Lambda(_), Suggestion.Expr(Expression.Identifier(_)) -> 
                    if lambdaCanBeReplacedWithFunction arguments.CheckFile arguments.Breadcrumbs expr then
                        hintError hint arguments.VisitorInfo expr.Range
                | _ ->
                    hintError hint arguments.VisitorInfo expr.Range
 
            Some(matches |> List.map snd |> List.concat)
 
        and private doExpressionsMatch expressions hintExpressions (arguments: Arguments) =
            expressions
            |> List.fold 
                (fun edges expr -> 
                    Option.bind (fun x -> arguments.SubHint(expr, x) |> matchHintExpr) edges) 
                (Some(hintExpressions))
 
        and private matchIf arguments edges =
            match arguments.Expression with
            | SynExpr.IfThenElse(cond, expr, None, _, _, _, _) -> 
                maybe {
                    let! edges = arguments.SubHint(cond, edges) |> matchHintExpr
                    return! arguments.SubHint(expr, edges) |> matchHintExpr }
            | SynExpr.IfThenElse(cond, expr, Some(elseExpr), _, _, _, _) -> 
                maybe {
                    let! edges = arguments.SubHint(cond, edges) |> matchHintExpr
                    let! edges = arguments.SubHint(expr, edges) |> matchHintExpr
                    return! arguments.SubHint(elseExpr, edges) |> matchHintExpr }
            | _ -> None
 
        and private matchFunctionApplication arguments edges =
            match arguments.Expression with
            | SynExpr.App(_) as application -> 
                let expressions = flattenFunctionApplication application
 
                doExpressionsMatch expressions edges arguments
            | _ -> None
 
        and private matchLambda arguments lambdaArgs edges =
            match arguments.Expression with
            | SynExpr.Lambda(_) as lambda -> 
                match matchLambdaArguments lambdaArgs lambda with
                | LambdaMatch.Match(bodyExpr, lambdaArguments, numberOfWildcards) -> 
                    match removeAutoGeneratedMatchesFromLambda numberOfWildcards bodyExpr with
                    | Some(bodyExpr) -> 
                        { arguments.SubHint(bodyExpr, edges) with LambdaArguments = lambdaArguments } 
                        |> matchHintExpr
                    | None -> None
                | LambdaMatch.NoMatch -> None
            | _ -> None
 
        and private matchTuple arguments edges =
            match arguments.Expression with
            | SynExpr.Tuple(expressions, _, _) ->
                doExpressionsMatch expressions edges arguments
            | _ -> None
 
        and private matchList arguments edges =
            match arguments.Expression with
            | SynExpr.ArrayOrList(false, expressions, _) ->
                doExpressionsMatch expressions edges arguments
            | SynExpr.ArrayOrListOfSeqExpr(false, SynExpr.CompExpr(true, _, expression, _), _) ->
                arguments.SubHint(expression, edges) |> matchHintExpr
            | _ -> None
 
        and private matchArray arguments edges =
            match arguments.Expression with
            | SynExpr.ArrayOrList(true, expressions, _) ->
                doExpressionsMatch expressions edges arguments
            | SynExpr.ArrayOrListOfSeqExpr(true, SynExpr.CompExpr(true, _, expression, _), _) ->
                arguments.SubHint(expression, edges) |> matchHintExpr
            | _ -> None
 
        and private matchInfixOperation arguments op edges =
            match arguments.Expression with
            | SynExpr.App(_, true, SynExpr.Ident(ident), SynExpr.Tuple([leftExpr; rightExpr], _, _), _) ->
                if identAsDecompiledOpName ident = op then
                    maybe {
                        let! edges = arguments.SubHint(rightExpr, edges) |> matchHintExpr
                        return! arguments.SubHint(leftExpr, edges) |> matchHintExpr }
                else None
            | SynExpr.App(_, _, infixExpr, rightExpr, _) -> 
                match removeParens infixExpr with
                | SynExpr.App(_, true, opExpr, leftExpr, _) ->
                    match removeParens opExpr with
                    | SynExpr.Ident(ident) ->
                        if identAsDecompiledOpName ident = op then
                            maybe {
                                let! edges = arguments.SubHint(leftExpr, edges) |> matchHintExpr
                                let! edges = 
                                    if notPropertyInitialisationOrNamedParameter arguments leftExpr opExpr then 
                                        Some(edges) 
                                    else None
                                return! arguments.SubHint(rightExpr, edges) |> matchHintExpr }
                        else None
                    | _ -> None
                | _ -> None
            | _ -> None
 
        and private matchPrefixOperation arguments op edges =
            match arguments.Expression with
            | SynExpr.App(_, _, opExpr, rightExpr, _) -> 
                match removeParens opExpr with
                | SynExpr.Ident(ident) ->
                    if identAsDecompiledOpName ident = "~" + op then
                        arguments.SubHint(rightExpr, edges) |> matchHintExpr
                    else None
                | _ -> None
            | SynExpr.AddressOf(_, addrExpr, _, _) when op = "&" || op = "&&" ->
                arguments.SubHint(addrExpr, edges) |> matchHintExpr
            | _ -> None
 
    module MatchPattern =
 
        type Arguments =
            { VisitorInfo: VisitorInfo
              OriginalPattern: SynPat
              Hints: MergeSyntaxTrees.Node list }
 
        let private matchPattern = function
            | SynPat.LongIdent(ident, _, _, _, _, _) ->
                let identifier = ident.Lid |> List.map (fun x -> x.idText)
                Some(MergeSyntaxTrees.Identifier(identifier))
            | SynPat.Const(constant, _) -> 
                matchConst constant |> Option.map MergeSyntaxTrees.Constant
            | SynPat.Null(_) ->
                Some(MergeSyntaxTrees.Null)
            | _ -> None
 
        /// Extracts a pattern from parentheses e.g. ((x)) -> x
        let rec private removeParens = function
            | SynPat.Paren(x, _) -> removeParens x
            | x -> x
    
        let rec matchHintPattern arguments (pattern, edges: MergeSyntaxTrees.Node list) =
            let pattern = removeParens pattern
 
            let matches =
                edges
                |> List.map (fun x ->
                match x.Match with
                | MergeSyntaxTrees.Variable(_)
                | MergeSyntaxTrees.Wildcard -> Some(x, x.Edges)
                | MergeSyntaxTrees.Null
                | MergeSyntaxTrees.Constant(_)
                | MergeSyntaxTrees.Identifier(_) ->
                    if matchPattern pattern = Some(x.Match) then Some(x, x.Edges)
                    else None
                | MergeSyntaxTrees.InfixOperator("::") -> 
                    matchConsPattern arguments (pattern, x.Edges)
                    |> Option.bind (fun edges -> Some(x, edges))
                | MergeSyntaxTrees.InfixOperator("|") -> 
                    matchOrPattern arguments (pattern, x.Edges)
                    |> Option.bind (fun edges -> Some(x, edges))
                | MergeSyntaxTrees.InfixOperator("&") -> 
                    matchAndPattern arguments (pattern, x.Edges)
                    |> Option.bind (fun edges -> Some(x, edges))
                | MergeSyntaxTrees.Parentheses -> 
                    matchHintPattern arguments (pattern, x.Edges)
                    |> Option.bind (fun edges -> Some(x, edges))
                | MergeSyntaxTrees.Tuple(_) -> 
                    matchTuple arguments (pattern, x.Edges)
                    |> Option.bind (fun edges -> Some(x, edges))
                | MergeSyntaxTrees.List(_) -> 
                    matchList arguments (pattern, x.Edges)
                    |> Option.bind (fun edges -> Some(x, edges))
                | MergeSyntaxTrees.Array(_) -> 
                    matchArray arguments (pattern, x.Edges)
                    |> Option.bind (fun edges -> Some(x, edges))
                | MergeSyntaxTrees.FunctionApplication(_)
                | MergeSyntaxTrees.Lambda(_)
                | MergeSyntaxTrees.If(_)
                | MergeSyntaxTrees.InfixOperator(_)
                | MergeSyntaxTrees.PrefixOperator(_) -> None)
                |> List.choose id

            for hint in matches |> Seq.map fst |> Seq.collect (fun x -> x.Suggestions) do
                hintError hint arguments.VisitorInfo arguments.OriginalPattern.Range
 
            Some(matches |> List.map snd |> List.concat)
 
        and private doPatternsMatch arguments patterns hintExpressions =
            patterns
            |> List.fold 
                (fun edges pattern -> 
                    Option.bind (fun x -> matchHintPattern arguments (pattern, x)) edges) 
                (Some(hintExpressions))
 
        and private matchList arguments (pattern, edges) =
            match pattern with
            | SynPat.ArrayOrList(false, patterns, _) ->
                doPatternsMatch arguments patterns edges
            | _ -> None
 
        and private matchArray arguments (pattern, edges) =
            match pattern with
            | SynPat.ArrayOrList(true, patterns, _) ->
                doPatternsMatch arguments patterns edges
            | _ -> None
 
        and private matchTuple arguments (pattern, edges) =
            match pattern with
            | SynPat.Tuple(patterns, _) ->
                doPatternsMatch arguments patterns edges
            | _ -> None
 
        and private matchConsPattern arguments (pattern, edges) =
            match pattern with
            | SynPat.LongIdent(
                                LongIdentWithDots([ident],_), 
                                _, 
                                _, 
                                Pats([SynPat.Tuple([leftPattern;rightPattern], _)]), 
                                _, 
                                _)
                    when ident.idText = "op_ColonColon" ->
                maybe {
                    let! edges = matchHintPattern arguments (leftPattern, edges)
                    return! matchHintPattern arguments (rightPattern, edges) }
            | _ -> None
 
        and private matchOrPattern arguments (pattern, edges) =
            match pattern with
            | SynPat.Or(leftPattern, rightPattern, _) ->
                maybe {
                    let! edges = matchHintPattern arguments (leftPattern, edges)
                    return! matchHintPattern arguments (rightPattern, edges) }
            | _ -> None
 
        and private matchAndPattern arguments (pattern, edges) =
            let rec matchAndPatterns (patterns, edges) : MergeSyntaxTrees.Node list option =
                edges
                |> List.map (fun (edge:MergeSyntaxTrees.Node) ->
                match patterns, edge.Match with
                | rightPattern::leftPattern::_ & _::patterns, MergeSyntaxTrees.InfixOperator("&") -> 
                    maybe {
                        let! edges = matchHintPattern arguments (leftPattern, edge.Edges)
                        let! edges = matchHintPattern arguments (rightPattern, edges)
 
                        return! matchAndPatterns (patterns, edges) }
                | _ -> None)
                |> List.choose id
                |> List.concat
                |> Some
 
            match pattern with
            | SynPat.Ands(patterns, _) -> 
                matchAndPatterns (List.rev patterns, edges)
            | _ -> None
 
    let visitor (hints:MergeSyntaxTrees.Node list) visitorInfo checkFile (astNode:CurrentNode) = 
        if astNode.IsSuppressed(AnalyserName) |> not then
            match astNode.Node with
            | AstNode.Expression(SynExpr.Paren(_))
            | AstNode.Pattern(SynPat.Paren(_)) -> Continue
            | AstNode.Expression(SynExpr.Const(_) as expr)
            | AstNode.Expression(SynExpr.Tuple(_) as expr)
            | AstNode.Expression(SynExpr.App(_) as expr)
            | AstNode.Expression(SynExpr.Lambda(_) as expr)
            | AstNode.Expression(SynExpr.ArrayOrList(_) as expr)
            | AstNode.Expression(SynExpr.ArrayOrListOfSeqExpr(_) as expr)
            | AstNode.Expression(SynExpr.IfThenElse(_) as expr)
            | AstNode.Expression(SynExpr.AddressOf(_) as expr) -> 
                let arguments =
                    { MatchExpression.OriginalExpression = expr
                      MatchExpression.VisitorInfo = visitorInfo
                      MatchExpression.LambdaArguments = Map.ofList []
                      MatchExpression.Expression = expr
                      MatchExpression.Hints = hints
                      MatchExpression.CheckFile = checkFile
                      MatchExpression.FSharpCheckFileResults = checkFile
                      MatchExpression.Breadcrumbs = astNode.Breadcrumbs }
 
                MatchExpression.matchHintExpr arguments |> ignore

                Continue
            | AstNode.Pattern(pattern) ->
                let arguments =
                    { MatchPattern.VisitorInfo = visitorInfo
                      MatchPattern.OriginalPattern = pattern
                      MatchPattern.Hints = hints }

                MatchPattern.matchHintPattern arguments (pattern, hints) |> ignore

                Continue
            | _ -> Continue
        else
            Stop
 
    let getHintsFromConfig config =
        let analyser = Map.find AnalyserName config.Analysers
 
        match Map.tryFind "Hints" analyser.Settings with
        | Some(Hints(hints)) -> List.map (fun x -> x.ParsedHint) hints
        | _ ->
            Debug.Assert(false, "Hints analyser was not in the configuration.")
            []
 
    type RegisterHintVisitor() = 
        interface IRegisterPlugin with
            member __.RegisterPlugin config = 
                let hints = 
                    getHintsFromConfig config
                    |> MergeSyntaxTrees.mergeHints
 
                { Name = AnalyserName
                  Visitor = Ast(visitor hints) }