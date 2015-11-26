namespace libmicron

open libcontextfree
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
/// A double-precision floating-point literal.
| Double
/// A whitespace symbol.
| Whitespace
/// A special token type that signifies the end of the token stream.
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
/// A left parenthesis.
| LParen
/// A right parenthesis.
| RParen
/// An operator token.
| OperatorToken
/// The "if" keyword.
| IfKeyword
/// The "then" keyword.
| ThenKeyword
/// The "else" keyword.
| ElseKeyword
/// The "data" keyword.
| DataKeyword
/// The "let" keyword.
| LetKeyword
/// The "infixl" keyword.
| InfixlKeyword
/// The "infixr" keyword.
| InfixrKeyword
/// The "in" keyword.
| InKeyword
/// The "module" keyword.
| ModuleKeyword

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
    let rec totalContents (tok : Token) : string =
        tok.preTrivia |> List.map totalContents
                      |> List.append [tok.contents]
                      |> String.concat ""

    /// Extracts the given token's total source location, including 
    /// its associated trivia tokens.
    let rec totalSourceLocation (tok : Token) : SourceLocation = 
        tok.preTrivia |> List.map totalSourceLocation
                      |> List.fold (fun x y -> x.Concat(y)) tok.sourceLocation

    /// Gets the total source location for an entire parse tree of tokens.
    let rec treeSourceLocation : ParseTree<'a, Token> -> SourceLocation = function
    | TerminalLeaf token -> 
        totalSourceLocation token
    | ProductionNode(_, children) -> 
        children |> List.map treeSourceLocation
                 |> List.fold (fun a b -> CompilerLogExtensions.Concat(a, b)) null  
        
    /// Gets the given token's type.
    let tokenType (token : Token) : TokenType =
        token.tokenType

    /// Gets the given token's source location.
    let sourceLocation (token : Token) : SourceLocation =
        token.sourceLocation

    /// Finds out if the given token type is a trivia token type,
    /// i.e. it should be skipped when parsing.
    let isTrivia : TokenType -> bool =
        function
        | TokenType.Whitespace | TokenType.EndOfStream -> true
        | _ -> false

    /// Finds out if the given token type is a non-trivia type.
    let isNonTrivia : TokenType -> bool =
        not << isTrivia

    /// Transforms the given list of tokens into another list of non-trivia tokens, where the
    /// trivia tokens from the original list have been "folded" into the non-trivia tokens.
    /// Trailing trivia tokens are discarded.
    let foldTrivia : Token list -> Token list =
        ListHelpers.cutAfter (tokenType >> isNonTrivia)
        >> List.map (fun (trivia, token) -> { token with preTrivia = trivia })

    /// Transforms the given list of tokens into another list of tokens, where all pre-trivia
    /// tokens in tokens of the original list have been prepended recursively to the token they
    /// belonged to. Pre-trivia are then stripped from all tokens in the resulting list. This is
    /// the opposite of `foldTrivia`, which tries to hide trivia in a tree-like structure.
    /// `expandTrivia` expands all trivia to a flat list instead.
    let rec expandTrivia (toks : Token list) : Token list =
        let folder (item : Token) (results : Token list) : Token list =
            let preTrivia = expandTrivia item.preTrivia
            let newToken = { item with preTrivia = [] }
            List.append preTrivia (newToken :: results)

        List.foldBack folder toks []