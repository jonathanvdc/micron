open Flame
open Flame.Compiler
open Flame.Front.Cli
open libcontextfree
open libmicron

[<EntryPoint>]
let main argv =
    // Use this to test the lexer: 
    // printfn "%A" (Lexer.lex (SourceDocument("let _ = _f x in 2.0;", "A.mu")) |> TokenHelpers.foldTrivia)

    // TODO: register a micron project handler
    // ProjectHandlers.RegisterHandler(MicronProjectHandler())
    let compiler = ConsoleCompiler("muc", "the magnificent micron compiler", "https://github.com/jonathanvdc/micron/releases")
    compiler.Compile(argv)
    0