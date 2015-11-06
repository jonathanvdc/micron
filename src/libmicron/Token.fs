﻿namespace libmicron

open Flame.Compiler

/// Defines a list of all possible token types.
[<RequireQualifiedAccess>]
type TokenType =
/// An unknown token.
| Unknown
/// An identifier.
| Identifier
/// An integer literal.
| Integer
/// A string literal.
| String
/// A character literal.
| Char
/// A whitespace symbol.
| Whitespace
/// A special token type that
/// signifies the end of the token stream.
| EndOfStream
/// A comma.
| Comma
/// A semicolon.
| Semicolon
/// A colon.
| Colon
/// An underscore.
| Underscore
/// An equals (`=`) sign.
| Equals
| LParen
| RParen
/// The "if" keyword.
| IfKeyword
/// The "else" keyword.
| ElseKeyword
/// The "data" keyword.
| DataKeyword
/// The "let" keyword.
| LetKeyword
/// The "in" keyword.
| InKeyword

/// A single token is a structure consisting of a source
/// string, a token type, and a location in the source document,
/// as well as a number of trivia nodes.
type Token = { 
    /// The token's source string.
    contents : string
    /// The token's type.
    tokenType : TokenType
    /// The token's location in the source document.
    /// This only applies to this token's direct contents,
    /// and does not include the trivia this token manages.
    /// Note: SourceLocation is really a Flame.Compiler.SourceLocation.
    ///       This makes it easier to provide pretty diagnostics.
    sourceLocation : SourceLocation
    /// All trivia tokens that directly precede this token.
    preTrivia : Token list 
}

module TokenHelpers =

    /// Gets the given token's total source string, including
    /// its associated trivia tokens.
    let rec getTotalContents (tok : Token) : string =
        tok.preTrivia |> List.map getTotalContents
                      |> List.append [tok.contents]
                      |> String.concat ""

    /// Extracts the given token's total source location, including 
    /// its associated trivia tokens.
    let rec getTotalSourceLocation (tok : Token) : SourceLocation = 
        tok.preTrivia |> List.map getTotalSourceLocation
                      |> List.fold (fun x y -> x.Concat(y)) tok.sourceLocation

    /// Finds out if the given token type is a trivia token type,
    /// i.e. it should be skipped when parsing.
    let isTrivia : TokenType -> bool = function
    | TokenType.Whitespace 
    | TokenType.EndOfStream -> true
    | _ -> false

    /// Transforms the given list of tokens into another list
    /// of non-trivia tokens, where the trivia tokens from the
    /// original list have been "folded" into the non-trivia tokens.
    /// Trailing trivia tokens are discarded.
    let rec foldTrivia (toks : Token list) : Token list =
        let folder (results : Token list, accTrivia : Token list) (item : Token) : Token list * Token list =
            if isTrivia item.tokenType then
                (results, item :: accTrivia)
            else
                let newToken = { item with preTrivia = List.rev accTrivia }
                (newToken :: results, [])

        toks |> List.fold folder ([], [])
             |> fst
             |> List.rev