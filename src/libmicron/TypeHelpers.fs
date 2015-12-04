namespace libmicron

open Flame
open Flame.Build
open Flame.Compiler
open Flame.Functional

module TypeHelpers =
    /// Creates a delegate method signature from the given sequence of parameters,
    /// return type, and attribute sequence.
    let createDelegateSignature (attrs : seq<IAttribute>) (parameters : seq<IParameter>) 
                                (returnType : IType) : IMethod =
        let header = FunctionalMemberHeader("", attrs)
        FunctionalMethod(header, null, true).WithParameters(fun _ -> parameters)
                                            .WithReturnType(fun _ -> returnType)
                                            :> IMethod

    /// Filters a sequence of attributes based on the given
    /// purity flag. If said flag is set to true, then the 
    /// attribute sequence is returned unchanged. Otherwise,
    /// purity attributes are removed from the sequence.
    let filterPurity (isPure : bool) (attrs : seq<IAttribute>) : seq<IAttribute> =
        if isPure then
            attrs
        else
            Seq.filter (fun (attr : IAttribute) -> 
                attr.AttributeType <> PrimitiveAttributes.Instance.ConstantAttribute.AttributeType) attrs

    /// Uncurries the given method signature.
    let uncurry (signature : IMethod) : IMethod =
        let rec getInfo (signature : IMethod) : IParameter list * IType * bool =
            match MethodType.GetMethod(signature.ReturnType) with
            | null ->
                List.ofSeq signature.Parameters, signature.ReturnType, signature.get_IsConstant()
            | other ->
                let innerParams, innerRetType, innerPurity = getInfo other
                List.append (List.ofSeq signature.Parameters) innerParams, innerRetType, signature.get_IsConstant() && innerPurity

        let parameters, retType, isPure = getInfo signature
        let attrs = filterPurity isPure signature.Attributes
        createDelegateSignature attrs parameters retType