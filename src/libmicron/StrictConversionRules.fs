namespace libmicron
namespace libmicron

open Flame
open Flame.Compiler
open Flame.Compiler.Expressions
open Flame.Functional

/// Defines a set of very strict conversion rules.
type StrictConversionRules(typeNamer : IType -> string) =
    interface IConversionRules with
        /// Implicit conversion between non-equivalent types are disallowed.
        member this.HasImplicitConversion tyFrom tyTo = tyFrom.IsEquivalent(tyTo)

        /// Tries to convert the given expression to the given type implicitly.
        /// Implicit conversion between non-equivalent types are disallowed. 
        member this.ConvertImplicit expr tyTo = 
            match expr.Type with
            | :? UnknownType -> expr
            | exprTy when exprTy.IsEquivalent(tyTo) -> expr
            | exprTy ->
                // Make sure to insert an error node.
                ExpressionBuilder.Error (LogEntry("Type mismatch", 
                                                  "Expected an expression of type '" + typeNamer tyTo + 
                                                  "'. Got one of type '" + typeNamer expr.Type + "'.")) 
                                        (ConversionExpression.Instance.Create(expr, tyTo))

        /// Converts the given expression to the given type in an explicit manner.
        member this.ConvertExplicit expr tyTo = ConversionExpression.Instance.Create(expr, tyTo)

