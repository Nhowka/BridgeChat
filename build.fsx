#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"

#if !FAKE
#r "netstandard"
#r "Facades/netstandard" // https://github.com/ionide/ionide-vscode-fsharp/issues/839#issuecomment-396296095
#endif

open System

open Fake.Core
open Fake.DotNet
open Fake.IO

let serverPath = Path.getFullName "./src/Server"
let clientPath = Path.getFullName "./src/Client"
let deployDir = Path.getFullName "./deploy"

let platformTool tool winTool =
    let tool = if Environment.isUnix then tool else winTool
    match Process.tryFindFileOnPath tool with
    | Some t -> t
    | _ ->
        let errorMsg =
            tool + " was not found in path. " +
            "Please install it and make sure it's available from your path. " +
            "See https://safe-stack.github.io/docs/quickstart/#install-pre-requisites for more info"
        failwith errorMsg

let nodeTool = platformTool "node" "node.exe"
let yarnTool = platformTool "yarn" "yarn.cmd"

let install = lazy DotNet.install id

let inline withWorkDir wd =
    DotNet.Options.lift install.Value
    >> DotNet.Options.withWorkingDirectory wd

let runTool cmd args workingDir =
    let result =
        Process.execSimple (fun info ->
            { info with
                FileName = cmd
                WorkingDirectory = workingDir
                Arguments = args })
            TimeSpan.MaxValue
    if result <> 0 then failwithf "'%s %s' failed" cmd args

let runDotNet cmd workingDir =
    let result =
        DotNet.exec (withWorkDir workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

let openBrowser url =
    let result =
        //https://github.com/dotnet/corefx/issues/10361
        Process.execSimple (fun info ->
            { info with
                FileName = url
                UseShellExecute = true })
            TimeSpan.MaxValue
    if result <> 0 then failwithf "opening browser failed"

Target.create "Clean" (fun _ ->
    Shell.cleanDirs [deployDir]
)

Target.create "InstallClient" (fun _ ->
    printfn "Node version:"
    runTool nodeTool "--version" __SOURCE_DIRECTORY__
    printfn "Yarn version:"
    runTool yarnTool "--version" __SOURCE_DIRECTORY__
    runTool yarnTool "install --frozen-lockfile" __SOURCE_DIRECTORY__
    runDotNet "restore" clientPath
)

Target.create "RestoreServer" (fun _ ->
    runDotNet "restore" serverPath
)

Target.create "Build" (fun _ ->
    runDotNet "build" serverPath
    runTool yarnTool "run webpack -- -p" clientPath
)

Target.create "Run" (fun _ ->
    let server = async {
        runDotNet "watch run" serverPath
    }
    let client = async {
        runTool yarnTool "run webpack-dev-server" clientPath
    }
    let browser = async {
        do! Async.Sleep 5000
        openBrowser "http://localhost:8080"
    }

    [ server; client; browser ]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore
)

Target.create "Bundle" (fun _ ->
    let serverDir = Path.combine deployDir "Server"
    let clientDir = Path.combine deployDir "Client"
    let publicDir = Path.combine clientDir "public"

    let publishArgs = sprintf "publish -c Release -o \"%s\" --runtime linux-x64" serverDir
    runDotNet publishArgs serverPath

    Shell.copyDir publicDir "src/Client/public" FileFilter.allFiles

    let procFile = "web: cd ./deploy/Server/ && ./Server"
    File.writeNew "Procfile" [procFile]
)

open Fake.Core.TargetOperators

"Clean"
    ==> "InstallClient"
    ==> "Build"
    ==> "Bundle"

"Clean"
    ==> "InstallClient"
    ==> "RestoreServer"
    ==> "Run"

Target.runOrDefault "Bundle"
