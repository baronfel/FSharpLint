module FSharpLint.FAKE

open System
open Fake
open FSharpLint.Application
open FSharpLint.Application.FSharpLintWorker

let private printException (e:System.Exception) =
    "Exception Message:" + e.Message + "Exception Stack Trace:" + e.StackTrace
        |> Console.WriteLine

let private failedToParseFileError (file:string) parseException =
    printfn "%A" file
    printException parseException

/// the default only prints something if FSharpLint found a lint in a file
let private defaultProgress = function
    | Failed(file, e) ->
        failedToParseFileError file e
    | Starting(_) | ReachedEnd(_) -> ()

let private defaultErrorReceived (error:LintWarning.Warning) =
    let formattedError = LintWarning.getWarningWithLocation error.Range error.Input

    error.Info + System.Environment.NewLine + formattedError
        |> System.Console.WriteLine

type LintOptions =
    { Progress: ProjectProgress -> unit
      ErrorReceived: LintWarning.Warning -> unit
      FailBuildIfAnyWarnings: bool }

let defaultLintOptions =
    { Progress = defaultProgress
      ErrorReceived = defaultErrorReceived
      FailBuildIfAnyWarnings = false }

/// Runs FSharpLint on a project.
/// ## Parameters
/// 
///  - `setParams ` - Function used to manipulate the default FSharpLint value.
///  - `projectFile` - The project file of the project you want to lint
/// 
/// ## Sample usage
///
///     Target "Lint" (fun _ ->
///         FSharpLint (fun o -> { o with ErrorReceived = System.Action<LintWarning.Error>(customErrorFunction) }) projectFile
///     )
let FSharpLint (setParams: LintOptions->LintOptions) (projectFile: string) =
    let parameters = defaultLintOptions |> setParams

    traceStartTask "FSharpLint" projectFile

    let numberOfWarnings, numberOfFiles = ref 0, ref 0
    
    let errorReceived error = 
        incr numberOfWarnings
        parameters.ErrorReceived error

    let parserProgress progress =
        match progress with
        | ReachedEnd(_) ->
            incr numberOfFiles
        | _ -> ()

        parameters.Progress progress

    match runLint projectFile errorReceived parserProgress with
    | Success when parameters.FailBuildIfAnyWarnings && !numberOfWarnings > 0 ->
        failwithf "Linted %s and failed the build as warnings were found. Linted %d files and found %d warnings." projectFile !numberOfFiles !numberOfWarnings
    | Success -> 
        tracefn "Successfully linted %s. Linted %d files and found %d warnings." projectFile !numberOfFiles !numberOfWarnings
    | Failure(message) -> 
        sprintf "Failed to lint %s. Failed with: %s" projectFile message |> traceError

    traceEndTask "FSharpLint" projectFile