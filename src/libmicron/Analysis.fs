namespace libmicron

open libmicron.ConstantPattern
open libcontextfree
open Flame
open Flame.Compiler
open Flame.Compiler.Expressions
open Flame.Compiler.Statements
open Flame.Compiler.Variables
open Flame.Functional

/// A semantic analysis module for
/// micron parse trees.
module Analysis =
    module EB = ExpressionBuilder

    /// Analyzes the given expression parse tree.
    let rec analyzeExpression (scope : LocalScope) : ParseTree<string, Token> -> IExpression = function
    | ProductionNode(Constant Parser.ifThenElseIdentifier,
                     [TerminalLeaf ifKeyword; cond; _; ifExpr; _; elseExpr]) ->
        // A simple if-then-else expression
        EB.Select scope (analyzeExpression scope cond)
                        (analyzeExpression scope ifExpr)
                        (analyzeExpression scope elseExpr)
            |> EB.Source (TokenHelpers.sourceLocation ifKeyword)
    | ProductionNode(Constant Parser.literalIntIdentifier,
                     [TerminalLeaf token]) ->
        // Integer literal
        (match System.Int32.TryParse token.contents with
        | (true, i) -> EB.ConstantInt32 i
        | (false, _) -> EB.Error (LogEntry("Invalid integer literal",
                                           sprintf "'%s' could not be parsed as a valid integer literal." token.contents))
                                 (EB.ConstantInt32 0)
        ) |> EB.Source (TokenHelpers.sourceLocation token)
    | ProductionNode(Constant Parser.literalDoubleIdentifier,
                     [TerminalLeaf token]) ->
        // Double literal
        (match System.Double.TryParse token.contents with
        | (true, d) -> EB.ConstantFloat64 d
        | (false, _) -> EB.Error (LogEntry("Invalid double literal",
                                           sprintf "'%s' could not be parsed as a valid double literal." token.contents))
                                 (EB.ConstantFloat64 0.0)
        ) |> EB.Source (TokenHelpers.sourceLocation token)
    | ProductionNode(Constant Parser.parenIdentifier,
                     [TerminalLeaf lParen; expr; TerminalLeaf rParen]) ->
        // Parentheses
        analyzeExpression scope expr
            |> EB.Source (CompilerLogExtensions.Concat(TokenHelpers.sourceLocation lParen,
                                                       TokenHelpers.sourceLocation rParen))
    | ProductionNode(Constant Parser.letIdentifier,
                     [ProductionNode(
                          Constant Parser.letDefinitionIdentifier,
                          [TerminalLeaf letKeyword
                           TerminalLeaf name
                           ProductionNode(Constant Parser.identifierListIdentifier, args) as argsNode
                           TerminalLeaf eq
                           value])
                      _
                      expr]) ->
        let here = TokenHelpers.sourceLocation letKeyword
        match args with
        | [] ->
            // Local variable declaration:  `let name = value in expr`
            let childScope = scope.ChildScope

            // First, bind `value` to `name`.
            let localValue = analyzeExpression childScope value
            let defLocal, updatedScope = EB.Quickbind childScope localValue name.contents
            let defLocal = EB.Source (TokenHelpers.sourceLocation eq) defLocal
            // Take care of the `in expr` clause
            let innerExpr = analyzeExpression updatedScope expr
            let result = EB.Initialize defLocal innerExpr

            EB.Scope result updatedScope |> EB.Source here
        | _ ->
            // Local function declaration:  let name args = value in expr
            // This should also support recursion.
            let childScope = scope.ChildScope
            let createBody lambdaScope = analyzeExpression lambdaScope value

            let attributes = [PrimitiveAttributes.Instance.ConstantAttribute]
            let argumentNames = [for t in ParseTree.treeYield argsNode -> t.contents]

            // Create a lambda for the defined function.
            let header = FunctionalMemberHeader("", attributes, here)
            let makeParam argName = Flame.Build.DescribedParameter(argName, UnknownType()) :> IParameter
            let signature = FunctionalMethod(header, null, true)
                                .WithParameters(fun _ -> Seq.map makeParam argumentNames)
                                .WithReturnType(fun _ -> UnknownType() :> IType)

            // The lambda's identifier is equivalent to the name token's contents.
            let ident = name.contents
            // Create the lambda itself.
            let lambda = EB.RecursiveLambda createBody signature ident childScope

            // Bind this lambda to `name`.
            let defLocal, updatedScope = EB.Quickbind childScope lambda ident
            let defLocal = EB.Source (TokenHelpers.sourceLocation eq) defLocal

            // Take care of the `in expr` clause
            let innerExpr = analyzeExpression updatedScope expr
            let result = EB.Initialize defLocal innerExpr

            EB.Scope result updatedScope |> EB.Source here

    | ProductionNode(Constant Parser.applyIdentifier, [left; right]) as node ->
        // Function application

        // Left-hand side is the function to apply. Right-hand side is
        // the argument to apply the function to. Analyze both.
        let funcExpr = analyzeExpression scope left
        let argExpr = analyzeExpression scope right

        (match funcExpr.GetEssentialExpression() with
        | :? PartialApplication as appl ->
            // Try to combine partial applications as much as possible.
            // This may discard *some* debug information, but I think that's
            // a fair trade-off, even in a debug build ([-g] or [-Og]):
            // once lamba expressions are lowered, they create really
            // hard-to-read stack traces.
            (PartialApplication(appl.Target, List.append appl.Arguments [argExpr])) :> IExpression
        | _ ->
            // You can't win 'em all, I guess. However,
            // we do want to preserve the left-hand side's debug
            // info, if any - we don't have a lot to gain by discarding
            // source locations here.
            (PartialApplication(funcExpr, [argExpr])) :> IExpression
        ) |> EB.Source (TokenHelpers.treeSourceLocation node)
    | ProductionNode(Constant Parser.identifierIdentifier,
                     [TerminalLeaf ident]) ->
        // Identifier
        (match scope.GetVariable ident.contents with
        | Some local ->
            local.CreateGetExpression()
        | None ->
            EB.VoidError (LogEntry("Unresolved identifier", sprintf "Identifier '%s' could not be resolved." ident.contents))
        ) |> EB.Source (TokenHelpers.sourceLocation ident)
    | ProductionNode(nonterm, _) as node ->
        // Unimplemented node type.
        // This just means that a construct has been defined in the grammar,
        // and that the semantic analysis pass does not support it yet.
        EB.VoidError (LogEntry("Unimplemented node type", sprintf "'%s' nodes have not been implemented yet." nonterm))
            |> EB.Source (TokenHelpers.treeSourceLocation node)
    | TerminalLeaf(term) ->
        // Unexpected terminal leaf.
        // This points to an error in the grammar.
        EB.VoidError (LogEntry("Unexpected raw token", sprintf "Token '%s' was completely unexpected here." term.contents))
            |> EB.Source (TokenHelpers.sourceLocation term)
