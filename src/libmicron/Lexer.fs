namespace libmicron

open Flame.Compiler

module Lexer =
    /// Defines an immutable source stream:
    /// a state when lexing source code.
    type SourceStream = { 
        /// The source code this source stream is parsing.
        source : string
        /// The source stream's source document.
        /// Note: ISourceDocument is really Flame.Compiler.ISourceDocument.
        ///       This makes it easier to provide pretty diagnostics.
        document : ISourceDocument
        /// The current position in the source stream.
        pos : int
    }

    /// Moves the given source stream's position forward
    /// by the given number of characters.
    let seek (offset : int) (stream : SourceStream) : SourceStream =
        { stream with pos = stream.pos + offset }

    /// Moves the given source stream's position forward
    /// by one character.
    let next (stream : SourceStream) : SourceStream =
        seek 1 stream

    /// Tells if the given source stream is "empty", i.e.
    /// its position has exceeded the length of its underlying
    /// source code string.
    let isEmpty (stream : SourceStream) : bool =
        stream.pos > stream.source.Length

    /// Gets the current character in the source stream.
    /// If the source stream is empty, nothing is returned.
    let current (stream : SourceStream) : char option =
        if isEmpty stream then
            None
        else
            Some stream.source.[stream.pos]

    /// "Slices" the given source stream from a starting
    /// position to its current position. A string is returned
    /// that represents the code inbetween these two positions.
    let slice (startPos : int) (stream : SourceStream) : string =
        let clamp i = 
            min (max 0 i) stream.source.Length

        let startPos = clamp startPos
        let endPos = clamp stream.pos

        if endPos < startPos then
            stream.source.Substring(endPos, startPos - endPos)
        else
            stream.source.Substring(startPos, endPos - startPos)

    /// "Slices" the given source stream from a starting
    /// position to its current position. A token is returned
    /// that represents the code inbetween these two positions.
    let sliceToken (startPos : int) (stream : SourceStream) (tokType : TokenType) : Token =
        { contents = slice startPos stream
          tokenType = tokType
          sourceLocation = SourceLocation(stream.document, startPos, stream.pos)
          preTrivia = [] }

    /// Tries to read a single character from the given input stream, provided
    /// it matched the given predicate function.
    let tryReadChar (pred : char -> bool) (stream : SourceStream) : SourceStream option =
        match current stream with
        | Some c when pred c -> Some (next stream)
        | _ -> None

    /// Reads a range of source code from the given source stream
    /// by consuming characters from the stream as long as 
    /// they satisfy the given predicate
    let rec readRange (pred : char -> bool) (stream : SourceStream) : SourceStream =
        match tryReadChar pred stream with
        | Some stream -> readRange pred (next stream)
        | _ -> stream

    /// A map of strings that have a 1:1 mapping
    /// to their associated token.
    let private staticTokens = 
        Map.ofList [
                       ",", TokenType.Comma
                       ";", TokenType.Semicolon
                       ":", TokenType.Colon
                       "(", TokenType.LParen
                       ")", TokenType.RParen
                       "if", TokenType.IfKeyword
                       "else", TokenType.ElseKeyword
                       "data", TokenType.DataKeyword
                       "let", TokenType.LetKeyword
                       "in", TokenType.InKeyword
                   ]

    /// Tries to read a static token from the source stream.
    let tryReadStaticToken (stream : SourceStream) : (Token * SourceStream) option =
        let sliceStream (KeyValue(k : string, v : TokenType)) : Token * SourceStream =
            let startPos = stream.pos
            let stream = seek k.Length stream
            sliceToken startPos stream v, stream

        staticTokens |> Seq.tryFind (fun (KeyValue(k, v)) -> stream.source.Substring(stream.pos, k.Length) = k)
                     |> Option.map sliceStream

    /// Reads a token of an unknown type from the source stream.
    let readUnknownToken (stream : SourceStream) : Token * SourceStream =
        // We have no idea what kind of token this is.
        // Just return an "Unknown" token and let the parser
        // choke on it.
        let startPos = stream.pos
        let stream = next stream
        sliceToken startPos stream TokenType.Unknown, stream

    /// Tries to read a token by consuming characters from the given
    /// source stream as long as they satisfy the given predicate
    /// function. This makes sense for whitespace and integers.
    let tryReadRangeToken (pred : char -> bool) (tokType : TokenType) 
                          (stream : SourceStream) : (Token * SourceStream) option =
        let startPos = stream.pos
        let stream = readRange pred stream
        if stream.pos = startPos then
            // This means that an empty range was parsed.
            None
        else
            Some (sliceToken startPos stream tokType, stream)

    /// Tries to read a token by consuming characters from the given
    /// source stream as long as they satisfy the given predicate
    /// function. A different predicate is applied to the first
    /// character of the token. This makes sense for identifiers.
    let tryReadInitRangeToken (initPred : char -> bool) (bodyPred : char -> bool) 
                              (tokType : TokenType) (stream : SourceStream) 
                              : (Token * SourceStream) option =
        let startPos = stream.pos
        match tryReadChar initPred stream with
        | Some stream ->
            let stream = readRange bodyPred stream
            Some (sliceToken startPos stream tokType, stream)
        | None ->
            None

    /// A list of functions that are succesively applied
    /// to a source stream whenever a token is to be read.
    let private lexerFunctions = 
        [
            tryReadRangeToken System.Char.IsWhiteSpace TokenType.Whitespace
            tryReadRangeToken System.Char.IsDigit TokenType.Integer
            tryReadInitRangeToken (fun c -> System.Char.IsLetter c || c = '_') 
                                  (fun c -> System.Char.IsLetter c || System.Char.IsDigit c || c = '_') 
                                  TokenType.Identifier
            tryReadStaticToken
        ]

    /// Reads a single token from the given source stream.
    let readToken (stream : SourceStream) : Token * SourceStream =
        if isEmpty stream then
            // The stream was already empty. Not much to do here.
            { contents = ""
              tokenType = TokenType.EndOfStream
              sourceLocation = SourceLocation(stream.document)
              preTrivia = [] }, stream
        else
            lexerFunctions |> List.tryPick ((|>) stream) 
                           |> OptionHelpers.coalesce (lazy readUnknownToken stream)


    /// Reads all remaining tokens from the given source stream.
    let rec readTokensToEnd (stream : SourceStream) : Token list =
        let tok, stream = readToken stream
        match tok.tokenType with
        | TokenType.EndOfStream -> []
        | _ -> tok :: readTokensToEnd stream

    /// Lexes the entirety of the given source document.
    let lex (doc : ISourceDocument) : Token list =
        let stream = { source = doc.Source
                       document = doc
                       pos = 0 }
        readTokensToEnd stream