open Flame.Front.Cli

[<EntryPoint>]
let main argv = 
    // TODO: register a micron project handler
    // ProjectHandlers.RegisterHandler(MicronProjectHandler())
    let compiler = ConsoleCompiler("muc", "the magnificent micron compiler", "https://github.com/jonathanvdc/micron/releases")
    compiler.Compile(argv)
    0