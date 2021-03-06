﻿namespace FsUnit.Test
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
type LexerTests () =

    let checkTypes (types : TokenType list) (items : Token list) : unit =
        items |> List.map TokenHelpers.tokenType
              |> should equal types

    let lex (input : string) : Token list =
        Lexer.lex (SourceDocument(input, "test.mu"))

    let checkLexedTypes (types : TokenType list) (input : string) : unit =
        lex input |> checkTypes types

    [<TestMethod>] 
    member this.VariousTokens () =
        "let _ = _f x in 2.0;" |> 
            checkLexedTypes 
                [
                    TokenType.LetKeyword; TokenType.Whitespace; 
                    TokenType.Underscore; TokenType.Whitespace;
                    TokenType.Equals; TokenType.Whitespace; 
                    TokenType.Identifier; TokenType.Whitespace; 
                    TokenType.Identifier; TokenType.Whitespace;
                    TokenType.InKeyword; TokenType.Whitespace;
                    TokenType.Double; TokenType.Semicolon;
                ]

    [<TestMethod>]
    member this.FoldTrivia () =
        lex "if x then y else z" |> TokenHelpers.foldTrivia
                                 |> checkTypes [TokenType.IfKeyword; TokenType.Identifier; TokenType.ThenKeyword; 
                                                TokenType.Identifier; TokenType.ElseKeyword; TokenType.Identifier]

    [<TestMethod>]
    member this.OperatorTokens () =
        let ops = "! + ++ - -- * % / :: ~ ^ & |" +
                  "<> == <= > < >= != ~= " + 
                  "|> ||> |||> <| <|| <||| " +
                  ">> << >>= <<= $ ??"
        lex ops |> TokenHelpers.foldTrivia
                |> checkTypes (List.replicate (ops.Split().Length) TokenType.OperatorToken)

    [<TestMethod>]
    member this.ExpandTrivia () =
        let original = lex "if x then y else z"

        original |> TokenHelpers.foldTrivia
                 |> TokenHelpers.expandTrivia
                 |> should equal original

    [<TestMethod>]
    member this.Keywords () =
        // Ensures that keywords like `let` do not interfere
        // with identifier parsing.
        lex "let _ = letter" |> TokenHelpers.foldTrivia
                             |> checkTypes [TokenType.LetKeyword; TokenType.Underscore; TokenType.Equals; 
                                            TokenType.Identifier]

    [<TestMethod>]
    member this.Comments () =
        lex "let // This is a comment\n//This is another comment" 
            |> checkTypes [TokenType.LetKeyword; TokenType.Whitespace; TokenType.Comment; 
                           TokenType.Whitespace; TokenType.Comment]