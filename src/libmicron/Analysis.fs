namespace libmicron

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
    | ProductionNode("if-then-else", [TerminalLeaf ifKeyword; cond; _; ifExpr; _; elseExpr]) -> 
        // A simple if-then-else expression
        EB.Select scope (analyzeExpression scope cond) (analyzeExpression scope ifExpr) (analyzeExpression scope elseExpr)
            |> EB.Source (TokenHelpers.totalSourceLocation ifKeyword)
    | ProductionNode("literal-int", [TerminalLeaf token]) ->
        // Integer literal
        (match System.Int32.TryParse token.contents with
        | (true, i) -> EB.ConstantInt32 i
        | (false, _) -> EB.Error (LogEntry("Invalid integer literal", sprintf "'%s' could not be parsed as a valid integer literal." token.contents))
                                                (EB.ConstantInt32 0)
        ) |> EB.Source (TokenHelpers.totalSourceLocation token)
    | ProductionNode("literal-double", [TerminalLeaf token]) ->
        // Double literal
        (match System.Double.TryParse token.contents with
        | (true, d) -> EB.ConstantFloat64 d
        | (false, _) -> EB.Error (LogEntry("Invalid double literal", sprintf "'%s' could not be parsed as a valid double literal." token.contents))
                                                (EB.ConstantFloat64 0.0)
        ) |> EB.Source (TokenHelpers.totalSourceLocation token)
    | ProductionNode("paren", [TerminalLeaf lParen; expr; TerminalLeaf rParen]) ->
        // Parentheses
        analyzeExpression scope expr
            |> EB.Source (CompilerLogExtensions.Concat(TokenHelpers.totalSourceLocation lParen, TokenHelpers.totalSourceLocation rParen))
    | ProductionNode("let", [ProductionNode("let-definition", [TerminalLeaf letKeyword; TerminalLeaf name; ProductionNode("identifier...", args); TerminalLeaf eq; value]); _; expr]) ->
        match args with
        | [] -> 
            // Local variable declaration.
            let scope = scope.ChildScope
            // First, bind `value` in `let ident = value` to `ident`.
            let localValue = analyzeExpression scope value
            let defLocal, scope = EB.Quickbind scope localValue name.contents
            let defLocal = EB.Source (TokenHelpers.totalSourceLocation eq) defLocal
            // Take care of the `in expr` clause
            let innerExpr = analyzeExpression scope expr
            let result = EB.Initialize defLocal innerExpr
            EB.Scope result scope 
                |> EB.Source (TokenHelpers.totalSourceLocation letKeyword)
        | _ ->
            // Local function declaration.
            // TODO: implement this!
            EB.VoidError (LogEntry("Unimplemented feature", "Local functions have not been implemented yet."))
                |> EB.Source (TokenHelpers.totalSourceLocation letKeyword)
    | ProductionNode("identifier", [TerminalLeaf ident]) ->
        // Identifier
        (match scope.GetVariable ident.contents with
        | Some local ->
            local.CreateGetExpression()
        | None ->
            EB.VoidError (LogEntry("Unresolved identifier", sprintf "Identifier '%s' could not be resolved." ident.contents))
        ) |> EB.Source (TokenHelpers.totalSourceLocation ident)
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
            |> EB.Source (TokenHelpers.totalSourceLocation term)