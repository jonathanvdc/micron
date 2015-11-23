namespace FsUnit.Test
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit.MsTest
open NHamcrest.Core
open libmicron
open libcontextfree
open Flame
open Flame.Compiler
open Flame.Compiler.Expressions
open Flame.Compiler.Statements
open Flame.Compiler.Variables
open Flame.Functional

[<TestClass>]
type ParserTests () =

    let mutable compileException : exn = null
    let totalGrammarParser = 
        try
            Parser.createParser Parser.programGrammar
        with
        | _ as e -> 
            compileException <- e
            fun tokens -> Choice2Of2 tokens

    let parseExpression (code : string) =
        let tokens = Lexer.lex (SourceDocument("let result = " + code, "test.mu")) |> TokenHelpers.foldTrivia
        match totalGrammarParser tokens with
        | Choice1Of2 tree -> 
            match tree with
            | ProductionNode("program", [ProductionNode("let-definition", [_; _; _; _; result]); ProductionNode("program", [])]) ->
                result
            | _ ->
                raise (System.Exception("Parser error."))
        | Choice2Of2 _ -> raise (System.Exception("Could not parse expression '" + code + "'."))

    let checkTreeType (code : string) (treeType : string) =
        let tree = parseExpression code
        let strippedTree = Parser.stripGroups tree
        match strippedTree with
        | ProductionNode(head, _) when treeType = head ->
            ()
        | _ ->
            raise (System.Exception(sprintf "Invalid tree type. Expected '%s', got '%A'." treeType (ParseTree.treeHead strippedTree)))

    [<TestMethod>] 
    member this.CompileExpressionGrammar () =
        Parser.createParser Parser.expressionGrammar |> ignore

    [<TestMethod>] 
    member this.CompileTotalGrammar () =
        if compileException <> null then
            raise compileException

    [<TestMethod>] 
    member this.ParseIfThenElse () =
        checkTreeType "if x then f else g" Parser.ifThenElseIdentifier
        checkTreeType "if x then y else z || x" Parser.ifThenElseIdentifier

    [<TestMethod>] 
    member this.ParseParen () =
        checkTreeType "(x)" Parser.parenIdentifier

    [<TestMethod>] 
    member this.ParseApply () =
        checkTreeType "f x" Parser.applyIdentifier

    [<TestMethod>] 
    member this.ParseLet () =
        checkTreeType "let x = f y in f x" Parser.letIdentifier
        checkTreeType "let f x = x * x in id y" Parser.letIdentifier

    [<TestMethod>]
    member this.ParseOps () = 
        checkTreeType "2 + 3" Parser.operatorIdentifier
        checkTreeType "x ^ y" Parser.operatorIdentifier
        checkTreeType "x * y" Parser.operatorIdentifier
        checkTreeType "x % y" Parser.operatorIdentifier
        checkTreeType "x / y" Parser.operatorIdentifier
        checkTreeType "x :: xs" Parser.operatorIdentifier
        checkTreeType "x != y" Parser.operatorIdentifier