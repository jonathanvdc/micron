﻿namespace libmicron

open Flame
open Flame.Build
open Flame.Compiler
open Flame.Compiler.Expressions
open Flame.Compiler.Statements
open Flame.Compiler.Variables
open Flame.Compiler.Visitors

module ExpressionHelpers =
    /// A type of node visitor that rewrites
    /// return nodes.
    type ReturnNodeVisitor(visit : ReturnStatement -> IStatement) =
        inherit StatementVisitorBase()

        override this.Matches (stmt : IStatement) : bool =
            stmt :? ReturnStatement

        override this.Transform (stmt : IStatement) : IStatement =
            match stmt with
            | :? ReturnStatement as stmt ->
                (visit stmt).Accept(this)
            | _ ->
                stmt.Accept(this)

    /// Uncurries all return statements in the given body.
    let uncurryReturnStatements (firstParamIndex : int) (parameters : IParameter list) (stmt : IStatement) : IStatement =
        // Create a list of argument-expressions for the given parameter list.
        let argList = List.mapi (fun i x -> ArgumentVariable(x, firstParamIndex + i).CreateGetExpression()) parameters
        let visitReturn (stmt : ReturnStatement) : IStatement =
            // Rewrite return statements by inserting a call.
            // Use a partial application instead of a direct
            // invocation, because partial applications will try
            // to coalesce first, which may be desirable.
            ReturnStatement(PartialApplication.ApplyOneByOne stmt.Value argList) :> IStatement

        // Now rewrite all return statements.
        let visitor = ReturnNodeVisitor(visitReturn)
        visitor.Visit(stmt)

    /// Uncurries the given lambda expression.
    let uncurryLambda (expr : LambdaExpression) : IExpression =
        // Curry the lambda, and rewrite return statements.
        let signature, body = TypeHelpers.uncurry uncurryReturnStatements expr.Body expr.Signature
        // Create a new lambda header.
        let lambdaHeader = LambdaHeader(signature, expr.Header.CaptureList)
        // Create a new lambda expression from the curried body,
        // the new lambda header, and the old lambda's bound header block.
        // 'Copy' it to remove any references to the old lambda header.
        LambdaExpression(lambdaHeader, body, expr.BoundHeaderBlock).Copy()

    /// Uncurries any expression. This converts expressions that yield
    /// a delegate value that returns some other delegate value into
    /// expressions that yield a delegate value that does not return
    /// another delegate value. Note: this function is *not* recusive:
    /// only the top-level expression will be uncurried, not its children.
    let uncurryExpression : IExpression -> IExpression = function
    | :? LambdaExpression as expr -> 
        // We can uncurry lambda-expressions by padding their parameter
        // lists.
        uncurryLambda expr
    | expr ->
        match MethodType.GetMethod(expr.Type) with
        | signature when signature <> null && 
                         signature.ReturnType.GetIsDelegate() ->
            // Well, this is a somewhat sad situation.
            // The only real way to fix this is to create
            // a lambda that captures the expression, and
            // is itself in uncurried form.
            // Fortunately, this isn't overly hard to implement:
            // we can create a parameterless lambda that simply 
            // returns the original expression, which it captures, 
            // and then uncurry that lambda.
            
            // Create the following lambda:
            //
            // [expr] pure () -> expr_type { return capt_values[0]; }
            let lambdaSignature = TypeHelpers.createDelegateSignature [PrimitiveAttributes.Instance.ConstantAttribute] [] expr.Type
            let lambdaHeader = LambdaHeader(lambdaSignature, [| expr |])
            let lambdaBoundBlock = LambdaBoundHeaderBlock()
            let body = ReturnStatement(LambdaCapturedValueExpression(lambdaHeader, lambdaBoundBlock, 0))
            let lambda = LambdaExpression(lambdaHeader, body, lambdaBoundBlock)

            // Let the lambda-uncurrying function work its magic.
            uncurryLambda lambda
        | _ ->
            expr

        /// Uncurries and re-curries the given expression to have it match
    /// the given function signature.
    let recurry (signature : IMethod) (expr : IExpression) : IExpression = 
        // Let's check if we really have to do this first. This construction
        // is kind of expensive, so we should use it sparingly.
        let curriedParamCounts = TypeHelpers.getCurriedParameterCounts signature
        if curriedParamCounts = TypeHelpers.getCurriedParameterCounts (MethodType.GetMethod(expr.Type)) then
            expr
        else
            let uncurriedExpr = uncurryExpression expr
            if curriedParamCounts = TypeHelpers.getCurriedParameterCounts (MethodType.GetMethod(uncurriedExpr.Type)) then
                uncurriedExpr
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
            let parameters, retType = TypeHelpers.getCurriedParametersAndReturnType func
            let addParam (retType : IType) (param : IParameter) : IType =
                TypeHelpers.createDelegateSignature func.Attributes [| param |] retType |> MethodType.Create
            let curriedTy = parameters |> Seq.concat
                                       |> Seq.fold addParam retType
            match MethodType.GetMethod(curriedTy) with
            | null -> AutoInvokeExpression(expr) :> IExpression
            | curriedFunc -> recurry curriedFunc expr

    /// A type of node visitor that re-curries return node values,
    /// if necessary. None can be provided as a "return type", if 
    /// re-currying of top-level return nodes is not desirable.
    type ReturnValueRecurryingVisitor(retType : IType option) =
        inherit StatementVisitorBase()

        interface ILambdaVisitor with
            member this.VisitBody(value : IExpression, owningMember : IMethod) : IExpression =
                recurryType owningMember.ReturnType value

            member this.VisitBody(value : IStatement, owningMember : IMethod) : IStatement =
                let newVisitor = ReturnValueRecurryingVisitor(Some owningMember.ReturnType)
                newVisitor.Visit(value)

        override this.Matches (stmt : IStatement) : bool =
            stmt :? ReturnStatement && retType.IsSome

        override this.Transform (stmt : IStatement) : IStatement =
            match stmt, retType with
            | :? ReturnStatement as stmt, Some retType ->
                ReturnStatement(recurryType retType stmt.Value) :> IStatement
            | _, _ ->
                stmt.Accept(this)

    /// A visitor that recurries partial application expression
    /// arguments whenever necessary.
    type PartialApplicationRecurryingVisitor() =
        inherit ContextlessVisitorBase()

        override this.Matches (stmt : IStatement) =
            false

        override this.Matches (expr : IExpression) =
            expr :? PartialApplication

        override this.Transform (stmt : IStatement) =
            stmt.Accept this

        override this.Transform (expr : IExpression) =
            match expr with
            | :? PartialApplication as expr ->
                match expr.TargetSignature with
                | Some targetSig ->
                    let parameters = targetSig.Parameters.GetTypes() 
                                        |> Seq.take (List.length expr.Arguments)
                                        |> List.ofSeq
                    let recurriedArgs = List.zip parameters expr.Arguments
                                        |> List.map ((<||) recurryType)
                    PartialApplication(expr.Target, recurriedArgs) :> IExpression
                | None ->
                    (expr :> IExpression).Accept(this)
            | _ ->
                expr.Accept(this)

    /// A node visitor that uncurries expressions.
    /// Note: variables are not uncurried by
    /// this visitor. This must be done 
    /// in some step previous to this
    /// visitor's application.
    type UncurryingVisitor() =
        inherit ContextlessVisitorBase()

        override this.Matches (stmt : IStatement) =
            false

        override this.Matches (expr : IExpression) =
            true

        override this.Transform (stmt : IStatement) =
            stmt.Accept this

        override this.Transform (expr : IExpression) =
            // Perform uncurrying in a bottom-up order:
            // accept first, then uncurry.
            (expr.Accept this) |> uncurryExpression

    /// Rewrites all types in uncurried form.
    type UncurryingTypeVisitor() =
        inherit TypeTransformerBase()

        override this.ConvertDelegateType (ty : IType) : IType =
            let signature = MethodType.GetMethod(ty)
            let newSig = TypeHelpers.uncurrySignature signature
            MethodType.Create newSig

    /// Uncurries types in the given expression.
    let uncurryTypes (expr : IExpression) =
        MemberNodeVisitor.ConvertTypes(UncurryingTypeVisitor(), expr)

    /// Uncurries an entire expression, including
    /// its child expressions. Types are
    /// also uncurried.
    let uncurryRecursive (expr : IExpression) =
        // First uncurry expressions, then tackle variables.
        UncurryingVisitor().Visit(expr) |> uncurryTypes