namespace libmicron
namespace libmicron

open Flame
open Flame.Compiler
open Flame.Compiler.Expressions
open Flame.Functional

type StrictConversionRules(typeNamer : IType -> string) =
    interface IConversionRules with
        member this.HasImplicitConversion tyFrom tyTo = false
        member this.ConvertImplicit expr tyTo = 
            let exprTy = expr.Type
            if exprTy.IsEquivalent(tyTo) then
                expr
            else
                ExpressionBuilder.Error (LogEntry("Type mismatch", 
                                                  "Expected an expression of type '" + typeNamer tyTo + 
                                                  "'. Got one of type '" + typeNamer expr.Type + "'.")) 
                                        (ConversionExpression.Instance.Create(expr, tyTo))
        member this.ConvertExplicit expr tyTo = ConversionExpression.Instance.Create(expr, tyTo)

