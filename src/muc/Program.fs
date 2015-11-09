open Flame
open Flame.Compiler
open Flame.Front.Cli
open libcontextfree
open libmicron

[<EntryPoint>]
let main argv =
    // Use this to test the lexer: 
    // printfn "%A" (Lexer.lex (SourceDocument("let _ = _f x in 2.0;", "A.mu")) |> TokenHelpers.foldTrivia)

    (*
    // Use this to test type inference.
    let t1 = UnknownType() // a
    let t2 = UnknownType() // b
    let t3 = UnknownType() // c
    let t4 = UnknownType() // d
    let t5 = UnknownType() // e
    let constraints = 
        [
            // a = a
            TypeInference.Variable t1, TypeInference.Variable t1
            // a = bool -> b
            TypeInference.Variable t1, TypeInference.Function(TypeInference.Constant PrimitiveTypes.Boolean, TypeInference.Variable t2)
            // int = b
            TypeInference.Constant PrimitiveTypes.Int32, TypeInference.Variable t2
            // c = d<a, b>
            TypeInference.Variable t3, TypeInference.Instance(TypeInference.Variable t4, [TypeInference.Variable t1; TypeInference.Variable t2])
            // e = c
            TypeInference.Variable t5, TypeInference.Variable t3
        ]

    let show = TypeInference.createShow()
    TypeInference.resolve constraints |> Result.map (List.map (fun (k, v) -> show (TypeInference.Variable k) + " = " + show v))
                                      |> Result.map (String.concat System.Environment.NewLine)
                                      |> Result.print
    *)

    // TODO: register a micron project handler
    // ProjectHandlers.RegisterHandler(MicronProjectHandler())
    let compiler = ConsoleCompiler("muc", "the magnificent micron compiler", "https://github.com/jonathanvdc/micron/releases")
    compiler.Compile(argv)
    0