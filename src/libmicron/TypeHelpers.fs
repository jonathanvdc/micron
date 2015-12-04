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

    /// Uncurries the given method signature, along with a body.
    /// The body is also uncurried step-by-step by repeatedly calling
    /// the given body-uncurrying function parameter on the body,
    /// providing the index of the first parameter and a contiguous list
    /// of parameters that belong to the current step in the uncurrying 
    /// process. Methods and their bodies are uncurried from the 
    /// inside out.
    let uncurry (uncurryBody : int -> IParameter list -> 'a -> 'a) (body : 'a) (signature : IMethod) : IMethod * 'a =
        // Uncurries the given body, but only if it is
        // not top-level.
        let maybeUncurry (isTopLevel : bool) (index : int) (parameterList : IParameter list) (body : 'a) : 'a =
            if isTopLevel then
                body
            else
                uncurryBody index parameterList body

        let rec getInfo (signature : IMethod) (body : 'a) (paramCount : int) (isTopLevel : bool) : IParameter list * IType * bool * 'a =
            match MethodType.GetMethod(signature.ReturnType) with
            | null ->
                let paramList = List.ofSeq signature.Parameters
                paramList, signature.ReturnType, signature.get_IsConstant(), maybeUncurry isTopLevel paramCount paramList body
            | other ->
                // Create a list of outer parameters.
                let outerParams = List.ofSeq signature.Parameters
                // Compute the total number of outer parameters.
                let totalOuterParamCount = paramCount + List.length outerParams
                // Process the inner method body.
                let innerParams, innerRetType, innerPurity, body = getInfo other body totalOuterParamCount false
                // Uncurry the outer method's body.
                let body = maybeUncurry isTopLevel totalOuterParamCount outerParams body
                List.append outerParams innerParams, innerRetType, signature.get_IsConstant() && innerPurity, body

        let parameters, retType, isPure, body = getInfo signature body 0 true
        let attrs = filterPurity isPure signature.Attributes
        createDelegateSignature attrs parameters retType, body

    /// Uncurries the given method signature.
    let uncurrySignature (signature : IMethod) = 
        uncurry (fun _ _ x -> x) () signature