namespace libmicron

open Flame.Compiler

module Lexer =
    /// Defines an immutable source stream:
    /// a state when lexing source code.
    type SourceStream = { 
        /// The source code this source stream is parsing.
        source : string
        /// The source stream's source document.
        document : SourceDocument
        /// The current position in the source stream.
        pos : int
    }

    /// Moves the given source stream's position forward
    /// by one character.
    let next (stream : SourceStream) : SourceStream =
        { stream with pos = stream.pos + 1 }

    /// Tells if the given source stream is "empty", i.e.
    /// its position has exceeded the length of its underlying
    /// source code string.
    let isEmpty (stream : SourceStream) : bool =
        stream.pos > stream.source.Length

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

    /// Reads a single token from the given source stream.
    let readToken (stream : SourceStream) : Token * SourceStream =
        if isEmpty stream then
            // The stream was already empty. Not much to do here.
            { contents = ""
              tokenType = TokenType.EndOfStream
              sourceLocation = SourceLocation(stream.document)
              preTrivia = [] }, stream
        else
            // We have no idea what kind of token this is.
            // Just return an "Unknown" token and let the parser
            // choke on it.
            let startPos = stream.pos
            let stream = next stream
            sliceToken startPos stream TokenType.Unknown, stream
