namespace libmicron

open Flame
open System
open System.Text
open libcontextfree

module NameHelpers =
    /// Gets a map of type names to built-in types.
    let builtinTypes =
        Map.ofList
            [
                "int8", PrimitiveTypes.Int8
                "int16", PrimitiveTypes.Int16
                "int32", PrimitiveTypes.Int32
                "int64", PrimitiveTypes.Int64

                "uint8", PrimitiveTypes.UInt8
                "uint16", PrimitiveTypes.UInt16
                "uint32", PrimitiveTypes.UInt32
                "uint64", PrimitiveTypes.UInt64

                "float32", PrimitiveTypes.Float32
                "float64", PrimitiveTypes.Float64

                "bool", PrimitiveTypes.Boolean
                "char", PrimitiveTypes.Char
                "void", PrimitiveTypes.Void
            ]

    /// Names the given type.
    let rec nameType (ty : IType) : string = 
        match Map.tryFindKey (fun _ item -> item = ty) builtinTypes with
        | Some name -> name
        | None ->
            match ty with
            | :? GenericType as ty ->
                GenericNameExtensions.ChangeTypeArguments(nameType (ty.Declaration), (ty.GenericArguments |> Seq.map nameType))
            | _ ->
                match MethodType.GetMethod ty with
                | null -> ty.FullName
                | signature -> nameFunction signature.ReturnType (signature.Parameters.GetTypes() |> List.ofSeq)
    /// Names the given function.
    and nameFunction (retTy : IType) = function
    | [] -> nameType retTy
    | argTy :: argTys -> 
        match MethodType.GetMethod argTy with
        | null -> nameType argTy + " " + nameFunction retTy argTys
        | _ -> "(" + nameType argTy + ") " + nameFunction retTy argTys

    /// A mapping of reserved symbols to their mangled versions.
    let mangleMapping =
        Map.ofList
            [
                '<', "lt"
                '>', "gt"
                '^', "caret"
                '*', "star"
                '/', "slash"
                '\\', "bslash"
                '+', "plus"
                '-', "minus"
                '|', "bar"
                '&', "and"
                '%', "percent"
                '@', "at"
                '#', "hash"
                '=', "eq"
                '!', "bang"
                ':', "colon"
                '.', "dot"
                '\'', "apo"
                '`', "btick"
                '\"', "quot"
                '_', "uscore"
                '$', "dollar"
            ]

    /// Mangles the given name.
    let mangleName (name : string) : string =
        let foldChar (tokens : string list, tokenBuilder : StringBuilder) (item : char) : string list * StringBuilder =
            match Map.tryFind item mangleMapping with
            | Some mangledString ->
                let sb = StringBuilder()
                sb.Append '$' |> ignore
                sb.Append mangledString |> ignore
                sb.ToString() :: tokenBuilder.ToString() :: tokens, new StringBuilder()
            | None ->
                tokenBuilder.Append item |> ignore
                tokens, tokenBuilder
        let tokens, lastTokenBuilder = Seq.fold foldChar ([], StringBuilder()) name
        lastTokenBuilder.ToString() :: tokens
            |> List.filter (String.IsNullOrEmpty >> not)
            |> List.rev
            |> String.concat "_"

    /// A mapping of mangled tokens to their
    /// character values.
    let demangleMapping = 
        mangleMapping |> Map.toSeq 
                      |> Seq.map (fun (k, v) -> v, k)
                      |> Map.ofSeq

    /// Demangles the given name.
    let demangleName (name : string) : string =
        let demangleToken (token : string) : string =
            if token.Length > 0 && token.Chars 0 = '$' then
                match Map.tryFind (token.Substring(1)) demangleMapping with
                | Some x -> x.ToString()
                | None -> token
            else
                token
        name.Split('_') |> Seq.map demangleToken
                        |> String.concat ""

    /// The standard operator name prefix for mangled
    /// operator names.
    let operatorNamePrefix = "$op_"

    /// Mangles the given binary operator fixity specification.
    let mangleOperatorFixity (spec : Parser.OpFixity) : string =
        match spec with
        | Parser.InfixLeft i -> "l" + string i
        | Parser.InfixRight i -> "r" + string i

    /// Tries to demangle the given binary operator
    /// fixity specification.
    let tryDemangleOperatorFixity (spec : string) : Result<Parser.OpFixity> =
        if spec.Length = 0 then
            Error "Empty fixity specification."
        else
            match System.Int32.TryParse (spec.Substring(1)) with
            | (true, prec) ->
                match spec.Chars 0 with
                | 'l' -> Success (Parser.InfixLeft prec)
                | 'r' -> Success (Parser.InfixRight prec)
                | _   -> 
                    Error (sprintf "Fixity specification '%s' does not start with 'l' or 'r'." spec)
            | (false, _) ->
                Error (sprintf "Could not parse precendence '%s' in fixity specification '%s'." (spec.Substring(1)) spec)

    /// Mangles the given operator name.
    let mangleOperatorName (name : string) (fixitySpec : Parser.OpFixity) : string = 
        operatorNamePrefix + (mangleOperatorFixity fixitySpec) + "_" + mangleName name

    /// Tests if the given name is a mangled operator name.
    let isOperatorName (name : string) : bool = 
        name.StartsWith(operatorNamePrefix)

    /// Demangles the given operator name.
    let tryDemangleOperatorName (name : string) : Result<string * Parser.OpFixity> =
        let payload = name.Substring(operatorNamePrefix.Length)
        let splitPayload = payload.Split([| '_' |], 2)
        match splitPayload with
        | [| fixitySpec; opName |] ->
            match tryDemangleOperatorFixity fixitySpec with
            | Success fixitySpec ->
                Success (demangleName opName, fixitySpec)
            | Error e ->
                Error e
        | _ ->
            Error (sprintf "Mangled operator name '%s' did not contain both a fixity specification and a name." name)