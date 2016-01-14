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
          sourceLocation = SourceLocation(stream.document, startPos, stream.pos - startPos)
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
                   ]

    /// A map of strings that have a 1:1 mapping
    /// to their associated token. Furthermore,
    /// we need to take extra care not to
    /// confuse these with identifier
    /// tokens.
    let private staticIdentifierTokens =
            Map.ofList [
                       "_", TokenType.Underscore
                       "if", TokenType.IfKeyword
                       "then", TokenType.ThenKeyword
                       "else", TokenType.ElseKeyword
                       "data", TokenType.DataKeyword
                       "let", TokenType.LetKeyword
                       "infixl", TokenType.InfixlKeyword
                       "infixr", TokenType.InfixrKeyword
                       "in", TokenType.InKeyword
                       "module", TokenType.ModuleKeyword
                       "open", TokenType.OpenKeyword
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

        /// "Interprets" an identifier: the given identifier is
        /// compared with a number of predefined identifiers
        /// that have a specific token type.
        let interpretIdentifier (token : Token) =
            match Map.tryFind token.contents staticIdentifierTokens with
            | Some ty -> { token with tokenType = ty }
            | None -> token

        let startPos = stream.pos
        match tryReadChar initPred stream with
        | Some(_, stream) ->
            // Awesome! We found something that *looks like* an identifier. 
            // Now we only have to read and interpret it.
            let stream = readRange bodyPred stream
            let token = sliceToken startPos stream TokenType.Identifier
            Some (interpretIdentifier token, stream)
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

    /// Parses a floating point number from the given source stream.
    let tryReadFloatingPoint (stream : SourceStream) : SourceStream option =
        let startPos = stream.pos
        let stream = readRange System.Char.IsDigit stream
        // Parses an exponent from the given stream.
        let parseExponent stream =
            match current stream with
            | Some 'e'
            | Some 'E' ->
                let newStream = next stream
                match current newStream with
                | Some '+'
                | Some '-' ->
                    let newStream = next newStream
                    match current newStream with
                    | Some c when System.Char.IsDigit c ->
                        // 'e+', 'E+', 'e-' or 'E-' followed by some digit. 
                        // Excellent.
                        readRange System.Char.IsDigit (next newStream)
                    | _ ->
                        // 'e+', 'E+', 'e-' or 'E-' followed by something
                        // other than a digit. This is not an exponent.
                        stream
                | Some c when System.Char.IsDigit c ->
                    // 'e' or 'E' followed by some digit. 
                    // That'll work just fine.
                    readRange System.Char.IsDigit newStream
                | _ ->
                    // 'e' or 'E' followed by something
                    // other than a digit. We have to
                    // go back.
                    stream
            | _ ->
                stream

        // Parse a decimal period, optionally followed by an
        // exponent.
        match current stream with
        | Some '.' ->
            let stream = next stream
            let periodPos = stream.pos
            let stream = readRange System.Char.IsDigit stream
            if stream.pos = periodPos then
                // We don't allow stuff like 2.E3.
                // Seriously, who does that?
                None
            else
                // We have a winner!
                Some (parseExponent stream)
        | _ ->
            // Sure, this may be an integer, but it is 
            // not a floating point literal. Return
            // None to signal that.
            None

    /// Tries to read a double-precision floating-point literal token from
    /// the stream.
    let tryReadDoubleToken (stream : SourceStream) : (Token * SourceStream) option =
        let startPos = stream.pos
        match tryReadFloatingPoint stream with
        | Some stream ->
            Some (sliceToken startPos stream TokenType.Double, stream)
        | None ->
            None

    /// A set of operator characters.
    let operatorCharacters = set ['!'; '='; '&'; '|'; '*'; '+'; '-'; '/'; 
                                  '%'; '@'; '^'; '<'; '>'; ':'; '~'; '?'; 
                                  '.'; '$'; '\\']

    /// Tries to read an operator token
    let tryReadOperatorToken (stream : SourceStream) : (Token * SourceStream) option =
        match tryReadRangeToken (fun c -> Set.contains c operatorCharacters) TokenType.OperatorToken stream with
        | None -> None
        | Some(token, stream) -> 
            if Map.containsKey token.contents staticTokens 
                then None 
                else Some (token, stream)
        
    /// Tries to read a comment token
    let tryReadCommentToken (stream : SourceStream) : (Token * SourceStream) option =
        let startPos = stream.pos
        match tryReadChar ((=) '/') stream with
        | Some (_, stream) -> 
            match tryReadChar ((=) '/') stream with
            | Some(_, steam) ->
                let stream = readRange ((<>) '\n') stream
                Some (sliceToken startPos stream TokenType.Comment, stream)
            | None -> None
        | None -> None


    /// A list of functions that are succesively applied
    /// to a source stream whenever a token is to be read.
    let private lexerFunctions = 
        [
            tryReadCommentToken
            tryReadRangeToken System.Char.IsWhiteSpace TokenType.Whitespace
            tryReadDoubleToken
            tryReadRangeToken System.Char.IsDigit TokenType.Integer
            tryReadDelimitedToken '"' TokenType.String
            tryReadDelimitedToken '\'' TokenType.Char
            tryReadOperatorToken
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