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

namespace FSharpLint.Application

module FSharpLintWorker =

    open System
    open Microsoft.FSharp.Compiler
    open FSharpLint.Framework

    type Result =
        | Success
        | Failure of string

    let runLint projectFile errorReceived errorProgress =
        let failed resouce args = 
            let formatString = Resources.GetString resouce
            String.Format(formatString, args) |> Failure

        try
            let neverFinishEarly _ = false

            let parseInfo =
                { FinishEarly = Some neverFinishEarly
                  ReceivedWarning = Some errorReceived
                  Configuration = None }

            let getParseFailureReason = function
                | ParseFile.FailedToParseFile(failures) ->
                    let getFailureReason (x:FSharpErrorInfo) =
                        sprintf "failed to parse file %s, message: %s" x.FileName x.Message

                    String.Join(", ", failures |> Array.map getFailureReason)
                | ParseFile.AbortedTypeCheck -> "Aborted type check."

            match lintProject parseInfo projectFile (Some errorProgress) with
            | LintResult.Failure(ProjectFileCouldNotBeFound(projectPath)) -> 
                failed "ConsoleProjectFileCouldNotBeFound" [|projectPath|]
            | LintResult.Failure(MSBuildFailedToLoadProjectFile(projectPath, e)) -> 
                failed "ConsoleMSBuildFailedToLoadProjectFile" [|projectPath; e.Message|]
            | LintResult.Failure(FailedToLoadConfig(message)) -> 
                failed "ConsoleFailedToLoadConfig" [|message|]
            | LintResult.Failure(RunTimeConfigError) -> 
                failed "ConsoleRunTimeConfigError" [||]
            | LintResult.Failure(FailedToParseFile(failure)) -> 
                Failure(
                    "Lint failed while analysing " + 
                    projectFile + 
                    ".\nFailed with: " + 
                    getParseFailureReason failure)
            | LintResult.Failure(FailedToParseFilesInProject(failures)) -> 
                Failure(
                    "Lint failed while analysing " + 
                    projectFile + 
                    ".\nFailed with: " + 
                    System.String.Join("\n", failures |> List.map getParseFailureReason))
            | LintResult.Success(_) -> Success
        with
        | e -> 
            Failure(
                "Lint failed while analysing " + 
                projectFile + 
                ".\nFailed with: " + 
                e.Message + 
                "\nStack trace: " + 
                e.StackTrace)