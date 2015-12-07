
open System
open libcontextfree
open libmicron
open Flame
open Flame.Compiler
open Flame.Functional
open Flame.Front
open Flame.Front.Cli

/// A compiled version of the micron program grammar.
let parser = Parser.createParser Parser.programGrammar

let showErrors (log : ICompilerLog) (expr : IExpression) =
    let visitor = Flame.Compiler.Visitors.LoggingVisitor(log, true, true)
    visitor.Visit expr

/// Names the given type.
let nameType = Analysis.nameType
let memProvider (ty : IType) = ty.GetAllMembers()
let getParameters = Analysis.getParameters

let parseExpression (log : ICompilerLog) (code : string) =
    // Prefix the list of tokens with `let <identifier> =`.
    let prefixTokens =
        [{ contents = "let"
           tokenType = TokenType.LetKeyword
           sourceLocation = null
           preTrivia = [] }
         { contents = ""
           tokenType = TokenType.Identifier
           sourceLocation = null
           preTrivia = [] }
         { contents = "="
           tokenType = TokenType.Equals
           sourceLocation = null
           preTrivia = [] }]
    // Create a source document so we can have
    // accurate diagnostics.
    let doc = SourceDocument(code, "repl.mu")
    // Lex tokens
    let tokens = prefixTokens @ Lexer.lex doc |> TokenHelpers.foldTrivia
    // Parse
    match parser tokens with
    | Choice1Of2 tree ->
        match tree with
        | ProductionNode("program", [ProductionNode("let-definition", [_; _; _; _; result]); ProductionNode("program", [])]) ->
            let globalScope = GlobalScope(FunctionalBinder(null), StrictConversionRules(nameType), log, nameType, memProvider, getParameters)
            let scope = LocalScope(globalScope)
            Analysis.analyzeExpression Map.empty scope (Parser.stripGroups result)
        | _ ->
            raise (System.Exception("Parser error."))
    | Choice2Of2 _ -> ExpressionBuilder.VoidError (new LogEntry("Invalid syntax",  "Could not parse expression.", SourceLocation(doc, 0, doc.CharacterCount)))

/// Gets a map of special "mode" prefixes
/// to handlers.
let modePrefixes =
    Map.ofList
        [
            ":t", fun (expr : IExpression) -> printfn "%s" (nameType expr.Type)
            ":repr", printfn "%A"
        ]

/// Executes the given expression, and prints the result.
let exec (expr : IExpression) : unit =
    let descMethod = Flame.ExpressionTrees.ExpressionMethod("repl", null, expr.Type, true)
    let cg = descMethod.CodeGenerator
    descMethod.SetMethodBody(cg.EmitReturn(expr.Emit(cg)))
    let result = descMethod.Invoke(null, Seq.empty)
    printfn "%A" (result.GetObjectValue())

/// Extracts the current "mode" from the input string.
let getMode (source : string) : (string * (IExpression -> unit)) =
    let asMode modePrefix mode =
        if source.StartsWith(modePrefix) then
            Some (source.Substring(modePrefix.Length), mode)
        else
            None

    match Map.tryPick asMode modePrefixes with
    | Some(src, mode) -> src, mode
    | None -> source, exec

/// Evaluates the given source expression.
let eval log source =
    let source, mode = getMode source
    let expr = parseExpression log source |> showErrors log
    mode expr

/// Runs a REPL loop.
let rec repl log input =
    Console.Write("> ")
    let line = Console.ReadLine()
    if line = null then
        ()
    else
        let split = line.Split([| ";;" |], StringSplitOptions.None)
        // Add previous input to first entry of array.
        split.[0] <- input + split.[0]
        // Gets all strings of source code that have to be evaluated.
        Seq.take (split.Length - 1) split |> Seq.iter (eval log)
        // Continue repl loop.
        repl log (split.[split.Length - 1])

[<EntryPoint>]
let main argv =
    use log = new ConsoleLog()
    repl log ""
    printfn ""
    0
