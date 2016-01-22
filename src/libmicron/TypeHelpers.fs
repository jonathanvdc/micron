namespace libmicron

open Flame
open Flame.Build
open Flame.Compiler
open Flame.Compiler.Expressions
open Flame.Compiler.Statements
open Flame.Compiler.Variables
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

    /// Uncurries and re-curries the given expression to have it match
    /// the given function signature.
    let recurry (signature : IMethod) (expr : IExpression) : IExpression = 
        // Let's check if we really have to do this first. This construction
        // is kind of expensive, so we should use it sparingly.
        if getCurriedParameterCounts signature = getCurriedParameterCounts (MethodType.GetMethod(expr.Type)) then
            expr
        else
            // Re-curries a fully uncurried expression to match the 
            // given function signature. We'll do this in a bottom-up
            // fashion.
            let rec recurryStep (signature : IMethod) (argList : seq<IExpression>) (expr : IExpression) : LambdaExpression =
                // The plan is to transform a function like f(a, b, c) into a differently
                // curried function, such as f(a)(b)(c). We will use lambdas to remember 
                // which parameters were passed.
                //
                // In the example above: 
                //
                //    f --> [f] pure (a) -> return_type1 { 
                //              return [f, a] pure (b) -> return_type2 {
                //                  return [f, a, b] pure (c) -> return_type3 { 
                //                      return f(a, b, c); 
                //                  };
                //              };
                //          };

                let retType = signature.ReturnType

                // We're going to construct a lambda either way, so we'd 
                // better get this over with already.
                let argArr = Array.ofSeq argList
                let header = LambdaHeader(signature, Array.append [| expr |] argArr)
                let boundHeader = LambdaBoundHeaderBlock()
                let captExpr = LambdaCapturedValueExpression(header, boundHeader, 0)
                let captArgList = Array.mapi (fun i _ -> LambdaCapturedValueExpression(header, boundHeader, i + 1) :> IExpression) argArr
                let lambdaArgList = Array.mapi (fun i x -> ArgumentVariable(x, i).CreateGetExpression()) (signature.GetParameters())
                let totalArgList = Array.append captArgList lambdaArgList

                match MethodType.GetMethod(retType) with
                | null ->
                    // We're constructing the bottom lambda now.
                    LambdaExpression(header, ReturnStatement(InvocationExpression(captExpr, totalArgList)), boundHeader)
                | retFunc ->
                    // Right now, we're tackling the top-level case.
                    let innerExpr = recurryStep retFunc totalArgList captExpr
                    LambdaExpression(header, ReturnStatement(innerExpr), boundHeader)

            // Use the fancy lambda construction to uncurry this.
            recurryStep signature Seq.empty expr :> IExpression

    /// Optionally re-curries the given expression to match the 
    /// given type.
    let recurryType (ty : IType) (expr : IExpression) : IExpression =
        match MethodType.GetMethod(ty), MethodType.GetMethod(expr.Type) with
        | null, _  
        | _, null -> expr
        | func, _ -> recurry func expr

    /// Fully curries the given expression.
    let curry (expr : IExpression) : IExpression =
        match MethodType.GetMethod(expr.Type) with
        | null -> expr
        | func ->
            let parameters, retType = getCurriedParametersAndReturnType func
            let addParam (retType : IType) (param : IParameter) : IType =
                createDelegateSignature func.Attributes [| param |] retType |> MethodType.Create
            let curriedTy = parameters |> Seq.concat
                                       |> Seq.fold addParam retType
            match MethodType.GetMethod(curriedTy) with
            | null -> AutoInvokeExpression(expr) :> IExpression
            | curriedFunc -> recurry curriedFunc expr