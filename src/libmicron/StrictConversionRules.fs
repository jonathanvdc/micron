namespace libmicron
namespace libmicron

open Flame
open Flame.Compiler
open Flame.Compiler.Expressions
open Flame.Functional

type StrictConversionRules() =
    interface IConversionRules with
        member this.HasImplicitConversion tyFrom tyTo = false
        member this.ConvertImplicit expr tyTo = ConversionExpression.Instance.Create(expr, tyTo)
        member this.ConvertExplicit expr tyTo = ConversionExpression.Instance.Create(expr, tyTo)

