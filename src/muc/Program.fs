open Flame
open Flame.Compiler
open Flame.Front.Cli
open Flame.Front.Projects
open libcontextfree
open libmicron

[<EntryPoint>]
let main argv =
    // TODO: register a micron project handler
    ProjectHandlers.RegisterHandler(muc.MicronProjectHandler())
    let compiler = ConsoleCompiler("muc", "the magnificent micron compiler", "https://github.com/jonathanvdc/micron/releases")
    compiler.Compile(argv)
    0