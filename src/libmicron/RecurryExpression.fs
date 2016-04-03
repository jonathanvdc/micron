namespace libmicron

open Flame
open Flame.Compiler
open Flame.Compiler.Expressions
open System

/// A conversion expression that converts an expression to a 
/// differently curried function type, if necessary.
type RecurryExpression(expr : IExpression, targetType : IType) = 
    inherit ConversionExpressionBase(expr, targetType)

    // Lowers this expression.
    let lowered = lazy ExpressionHelpers.recurryType targetType expr

    /// Creates a recurrying expression from the given inner expression
    /// and type.
    override this.Create(newExpr : IExpression, newTargetType : IType) : ConversionExpressionBase =
        RecurryExpression(newExpr, newTargetType) :> ConversionExpressionBase

    override this.Evaluate() : IBoundObject = lowered.Value.Evaluate()
    override this.Optimize() : IExpression = lowered.Value.Optimize()
    override this.IsConstantNode : bool = lowered.Value.IsConstantNode
    override this.Emit(cg : ICodeGenerator) : ICodeBlock = lowered.Value.Emit(cg)

    override this.ToString() =
        sprintf "recurry(%s, %O)" (NameHelpers.nameType targetType) expr