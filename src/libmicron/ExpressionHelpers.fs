namespace libmicron

open Flame
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
            ReturnStatement(PartialApplication(stmt.Value, argList)) :> IStatement

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

    /// Uncurries an entire expression, including
    /// its child expressions.
    /// Note: variables are not uncurried by
    /// this function. This must be done 
    /// in some step previous to this
    /// visitor's application.
    let uncurryRecursive (expr : IExpression) =
        UncurryingVisitor().Visit(expr)