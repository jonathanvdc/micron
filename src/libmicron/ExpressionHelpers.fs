﻿namespace libmicron

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

    /// Uncurries the given lambda expression.
    let uncurryLambda (expr : LambdaExpression) : IExpression =
        /// Rewrites all return statements in the given body.
        let rewriteReturnStatements (firstParamIndex : int) (parameters : IParameter list) (stmt : IStatement) : IStatement =
            // Create a list of argument-expressions for the given parameter list.
            let argList = List.mapi (fun i x -> ArgumentVariable(x, firstParamIndex + i).CreateGetExpression()) parameters
            let visitReturn (stmt : ReturnStatement) : IStatement =
                // Rewrite return statements by inserting a call.
                ReturnStatement(InvocationExpression(stmt.Value, argList)) :> IStatement

            // Now rewrite all return statements.
            let visitor = ReturnNodeVisitor(visitReturn)
            visitor.Visit(stmt)
        // Curry the lambda, and rewrite return statements.
        let signature, body = TypeHelpers.uncurry rewriteReturnStatements expr.Body expr.Signature
        // Create a new lambda header.
        let lambdaHeader = LambdaHeader(signature, expr.Header.CaptureList)
        // Create a new lambda expression from the curried body,
        // the new lambda header, and the old lambda's bound header block.
        // 'Copy' it to remove any references to the old lambda header.
        LambdaExpression(lambdaHeader, body, expr.BoundHeaderBlock).Copy()