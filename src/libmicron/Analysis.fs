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
    
    /// Analyzes the given expression parse tree.
    let rec analyzeExpression (scope : LocalScope) : ParseTree<string, Token> -> IExpression = function
    | ProductionNode("if-then-else", [TerminalLeaf ifKeyword; cond; _; ifExpr; _; elseExpr]) -> 
        // A simple if-then-else expression
        ExpressionBuilder.Select scope (analyzeExpression scope cond) (analyzeExpression scope ifExpr) (analyzeExpression scope elseExpr)
            |> ExpressionBuilder.Source (TokenHelpers.totalSourceLocation ifKeyword)
    | ProductionNode(nonterm, _) as node ->
        // Unimplemented node type.
        // This just means that a construct has been defined in the grammar, 
        // and that the semantic analysis pass does not support it yet.
        ExpressionBuilder.VoidError (LogEntry("Unimplemented node type", sprintf "'%s' nodes have not been implemented yet." nonterm))
            |> ExpressionBuilder.Source (TokenHelpers.treeSourceLocation node)
    | TerminalLeaf(term) ->
        // Unexpected terminal leaf.
        // This points to an error in the grammar.
        ExpressionBuilder.VoidError (LogEntry("Unexpected raw token", sprintf "Token '%s' was completely unexpected here." term.contents))
            |> ExpressionBuilder.Source (TokenHelpers.totalSourceLocation term)