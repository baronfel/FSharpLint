#r "paket:
nuget Fake.DotNet.Cli
nuget Fake.DotNet.FSFormatting
nuget Fake.DotNet.Paket
nuget Fake.DotNet.MSBuild
nuget Fake.IO.FileSystem
nuget Fake.Tools.Git
nuget Fake.Core.ReleaseNotes
nuget Fake.Core.Target //"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.Tools

let project = "FSharpLint"

let release = ReleaseNotes.load "RELEASE_NOTES.md"
Environment.setEnvironVar "Version" release.NugetVersion

Target.create "Clean" (fun _ ->
    !! "src/*/bin"
    ++ "src/*/obj"
    ++ "tests/*/bin"
    ++ "tests/*/obj"
    |> Shell.cleanDirs)

Target.create "Restore" (fun _ -> DotNet.exec id "paket" "restore" |> ignore)

Target.create "Build" (fun _ -> DotNet.build id "FSharpLint.sln")

let filterPerformanceTests (p:DotNet.TestOptions) = { p with Filter = Some "\"TestCategory!=Performance\""; Configuration = DotNet.Release }

Target.create "RunTests" (fun _ -> 
  DotNet.test filterPerformanceTests "tests/FSharpLint.Core.Tests"
  DotNet.test filterPerformanceTests "tests/FSharpLint.Console.Tests")
Target.create "RunFunctionalTests" (fun _ ->
  DotNet.restore id "tests/FSharpLint.FunctionalTest.TestedProject/FSharpLint.FunctionalTest.TestedProject.sln"
  DotNet.test filterPerformanceTests "tests/FSharpLint.FunctionalTest")


module ProcessResult = 
  let ensureOk (p: ProcessResult) = 
    if p.OK then ()
    else
      failwithf "Process exited with code %d:\n%A" p.ExitCode (p.Errors |> String.concat "\r\n\t")

Target.create "Package" (fun _ ->
    let execConfig (c: DotNet.Options) =
        { c with
            CustomParams = Some(sprintf "--output \"../../packaging\" --configuration Release \"-p:NoWarn=NU5125\" -p:Version=%s" (string release.NugetVersion)) }
    DotNet.exec execConfig "pack" "src/FSharpLint.Core/FSharpLint.Core.fsproj"
    |> ProcessResult.ensureOk
    
    DotNet.exec execConfig "pack" "src/FSharpLint.Console/FSharpLint.Console.fsproj"
    |> ProcessResult.ensureOk
)

Target.create "PublishPackages" (fun _ -> Paket.push(fun p -> { p with WorkingDir = "packaging" }))

Target.create "Release" (fun _ ->
    Git.Staging.stageAll ""
    Git.Commit.exec "" (sprintf "Bump version to %s" release.NugetVersion)
    Git.Branches.push ""

    Git.Branches.tag "" release.NugetVersion
    Git.Branches.pushTag "" "origin" release.NugetVersion)

Target.create "GenerateDocs" (fun _ ->
    Shell.cleanDir "docs"

    let projInfo =
        [ "project-name", "FSharpLint"
          "project-author", "Matthew Mcveigh"
          "project-summary", "A lint tool for F#."
          "project-github", "http://fsprojects.github.io/FSharpLint/"
          "project-nuget", "http://nuget.org/packages/FSharpLint.Core" ]

    Shell.copyDir "docs/content" "docs-gen/files" FileFilter.allFiles
    FSFormatting.createDocs (fun s ->
        { s with
            Source = "docs-gen/markdown"
            OutputDirectory = "docs"
            Template = "docs-gen/templates/template.html"
            ProjectParameters = projInfo
            LayoutRoots = [] }))

Target.create "Default" ignore

open Fake.Core.TargetOperators

"Clean"
    ==> "Restore"
    ==> "Build"
    ==> "RunTests"
    ==> "RunFunctionalTests"
    ==> "Package"
    ==> "GenerateDocs"
    ==> "Default"
    ==> "PublishPackages"
    ==> "Release"

Target.runOrDefault "Default"
