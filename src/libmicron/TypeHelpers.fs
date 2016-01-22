namespace libmicron

open Flame
open Flame.Build
open Flame.Compiler
open Flame.Compiler.Expressions
open Flame.Compiler.Statements
open Flame.Compiler.Variables
open Flame.Compiler.Visitors
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
    /// process. Methods and their bodies are uncurried in a top-down fashion.
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
                paramList, signature.ReturnType, signature.GetIsConstant(), maybeUncurry isTopLevel paramCount paramList body
            | other ->
                // Create a list of outer parameters.
                let outerParams = List.ofSeq signature.Parameters
                // Compute the total number of outer parameters.
                let totalOuterParamCount = paramCount + List.length outerParams
                // Uncurry the outer method's body first.
                let body = maybeUncurry isTopLevel paramCount outerParams body
                // Process the inner method and its body.
                let innerParams, innerRetType, innerPurity, body = getInfo other body totalOuterParamCount false
                // Concatenate the results
                List.append outerParams innerParams, innerRetType, signature.GetIsConstant() && innerPurity, body

        let parameters, retType, isPure, body = getInfo signature body 0 true
        let attrs = filterPurity isPure signature.Attributes
        createDelegateSignature attrs parameters retType, body

    /// Uncurries the given method signature.
    let uncurrySignature (signature : IMethod) : IMethod = 
        uncurry (fun _ _ x -> x) () signature |> fst

    /// Gets this (curried) function signature's parameters, as well
    /// as its eventual return type.
    let rec getCurriedParametersAndReturnType (signature : IMethod) : seq<IParameter> list * IType =
        let parameters = signature.Parameters
        let retType = signature.ReturnType
        match MethodType.GetMethod(retType) with
        | null -> [parameters], retType
        | retFunc -> 
            let innerParams, innerRetType = getCurriedParametersAndReturnType retFunc
            parameters :: innerParams, innerRetType

    /// Gets this (curried) function signature's parameters.
    let getCurriedParameters : IMethod -> seq<IParameter> list =
        getCurriedParametersAndReturnType >> fst

    /// Gets the number of arguments a function accepts at a time.
    let getCurriedParameterCounts : IMethod -> int list =
        getCurriedParameters >> List.map Seq.length