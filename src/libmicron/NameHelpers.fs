namespace libmicron

open Flame
open System

module NameHelpers =
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
                nameType (ty.Declaration) + "<" + (ty.GenericArguments |> Seq.map nameType |> String.concat ", ") + ">"
            | _ ->
                match MethodType.GetMethod ty with
                | null -> ty.FullName
                | signature -> nameFunction signature.ReturnType (signature.Parameters.GetTypes() |> List.ofSeq)
    and nameFunction (retTy : IType) = function
    | [] -> nameType retTy
    | argTy :: argTys -> 
        match MethodType.GetMethod argTy with
        | null -> nameType argTy + " " + nameFunction retTy argTys
        | _ -> "(" + nameType argTy + ") " + nameFunction retTy argTys