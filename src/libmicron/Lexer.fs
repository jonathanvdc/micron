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
        stream.pos >= stream.source.Length

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
    let tryReadChar (pred : char -> bool) (stream : SourceStream) : (char * SourceStream) option =
        match current stream with
        | Some c when pred c -> Some (c, next stream)
        | _ -> None

    /// Reads a range of source code from the given source stream
    /// by consuming characters from the stream as long as 
    /// they satisfy the given predicate
    let rec readRange (pred : char -> bool) (stream : SourceStream) : SourceStream =
        match tryReadChar pred stream with
        | Some(_, stream) -> readRange pred stream
        | _ -> stream

    /// A map of strings that have a 1:1 mapping
    /// to their associated token.
    let private staticTokens = 
        Map.ofList [
                       // Don't insert `"_", TokenType.Underscore` here:
                       // the identifier lexer takes care of that 
                       // (it's a precedence thing)
                       ",", TokenType.Comma
                       ";", TokenType.Semicolon
                       ":", TokenType.Colon
                       "(", TokenType.LParen
                       ")", TokenType.RParen
                       "=", TokenType.Equals
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

        staticTokens |> Seq.tryFind (fun (KeyValue(k, v)) -> slice stream.pos (seek k.Length stream) = k)
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

    /// Tries to read an identifier token from the given source stream.
    let tryReadIdentifier (stream : SourceStream) : (Token * SourceStream) option =
        let initPred c = System.Char.IsLetter c || c = '_'
        let bodyPred c = System.Char.IsLetter c || System.Char.IsDigit c || c = '_'

        let startPos = stream.pos
        match tryReadChar initPred stream with
        | Some(c, stream) when c = '_' ->
            let nextPos = stream.pos
            let stream = readRange bodyPred stream
            if nextPos = stream.pos then
                // We read nothing but an underscore. This results in an 
                // underscore token.
                Some (sliceToken startPos stream TokenType.Underscore, stream)
            else
                // We read an underscore followed by a nonempty string.
                // That means we really just read an identifier.
                Some (sliceToken startPos stream TokenType.Identifier, stream)
        | Some(_, stream) ->
            // This is an identifier no matter what.
            let stream = readRange bodyPred stream
            Some (sliceToken startPos stream TokenType.Identifier, stream)
        | None ->
            // There's no way this thing is an identifier.
            None

    /// Tries to read a delimited range of code, such as a
    /// string or character literal. This function allows
    /// the delimiter character to be escaped in the token
    /// by placing a backslash in front of it.
    let tryReadDelimitedToken (delim : char) (tokType : TokenType) (stream : SourceStream) 
                              : (Token * SourceStream) option =
        let startPos = stream.pos
        match tryReadChar ((=) delim) stream with
        | Some(_, stream) ->
            // This function reads characters from the given source stream until a delimiter
            // is encountered and the escaped flag is set to false.
            let rec readToTrailingDelim (stream : SourceStream) (escaped : bool) : SourceStream =
                match current stream with
                | Some c when not escaped && c = delim -> stream
                | Some c when c = '\\' -> readToTrailingDelim (next stream) (not escaped)
                | Some c -> readToTrailingDelim (next stream) false
                | None -> stream

            let stream = readToTrailingDelim stream false

            match tryReadChar ((=) delim) stream with
            | Some (_, stream) ->
                // Found a leading and a trailing delimiter. Awesome!
                Some (sliceToken startPos stream tokType, stream)
            | None ->
                // Alas, couldn't find a trailing delimiter.
                None
        | None ->
            // No leading delimiter found.
            None

    /// A list of functions that are succesively applied
    /// to a source stream whenever a token is to be read.
    let private lexerFunctions = 
        [
            tryReadRangeToken System.Char.IsWhiteSpace TokenType.Whitespace
            tryReadRangeToken System.Char.IsDigit TokenType.Integer
            tryReadDelimitedToken '"' TokenType.String
            tryReadDelimitedToken '\'' TokenType.Char
            tryReadStaticToken
            tryReadIdentifier
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