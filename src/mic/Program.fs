﻿
open System
open libcontextfree
open libmicron
open Flame
open Flame.Compiler
open Flame.Functional
open Flame.Front
open Flame.Front.Cli

let parser = Parser.createParser Parser.programGrammar

let showErrors (log : ICompilerLog) (expr : IExpression) =
    let visitor = Flame.Compiler.Visitors.LoggingVisitor(log, true, true)
    visitor.Visit expr

let nameType (ty : IType) = ty.FullName
let memProvider (ty : IType) = ty.GetAllMembers()
let getParameters (func : IMethod option) =
    Map.empty

let parseExpression (log : ICompilerLog) (code : string) =
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
    let doc = SourceDocument(code, "repl.mu")
    let tokens = prefixTokens @ Lexer.lex doc |> TokenHelpers.foldTrivia
    match parser tokens with
    | Choice1Of2 tree -> 
        match tree with
        | ProductionNode("program", [ProductionNode("let-definition", [_; _; _; _; result]); ProductionNode("program", [])]) ->
            let globalScope = GlobalScope(FunctionalBinder(null), StrictConversionRules(), log, nameType, memProvider, getParameters)
            let scope = LocalScope(globalScope)
            Analysis.analyzeExpression scope (Parser.stripGroups result)
        | _ ->
            raise (System.Exception("Parser error."))
    | Choice2Of2 _ -> ExpressionBuilder.VoidError (new LogEntry("Invalid syntax",  "Could not parse expression.", SourceLocation(doc, 0, doc.CharacterCount)))

let eval log source =
    let expr = parseExpression log source |> showErrors log
    printfn "%A" expr

let rec repl log input =
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
    0