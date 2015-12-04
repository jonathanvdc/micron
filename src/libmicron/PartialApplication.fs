namespace libmicron

open Flame
open Flame.Compiler
open Flame.Compiler.Expressions
open Flame.Compiler.Statements
open Flame.Compiler.Variables
open Flame.Functional

/// An expression representing a possibly partial application of `target` to some arguments `args`.
type PartialApplication private(target : IExpression, args : IExpression list,
                                resultType : UnknownType) =

    let targetSignature = MethodType.GetMethod target.Type

    /// If this PartialApplication still needs more arguments, make it a LambdaExpression.
    /// If it has all the arguments it needs, it can be a call.
    let lowered =
        lazy if targetSignature = null then
                 // The target signature can be null if we don't know for
                 // sure that the target is a function value. This can
                 // mean one of two things:
                 //   1. Type inference has yet to figure out the
                 //      type of the target, even though it is a function.
                 //   2. The target is no function.
                 // That does not mean, however, that lowering this
                 // expression is a meaningless gesture.
                 // All hope of lowering this expression into an *executable*
                 // expression may be lost, but we can still associate this
                 // partial application with an unknown result type. If
                 // we're lucky, then the type inference engine will use
                 // this information to infer that the target of this partial
                 // application is indeed a function, and then rewrite this
                 // expression such that it *can* be lowered into an
                 // executable expression (because we know the target's type).

                 UnknownExpression(resultType) :> IExpression
             else if List.length args < Seq.length targetSignature.Parameters then
                 // If we already have n arguments, skip the first n parameters.
                 let n = List.length args
                 let newParams = Array.ofSeq (Seq.skip n targetSignature.Parameters)

                 // Copy the attributes from the original function.
                 // Construct the new function's signature...
                 let newSignature = TypeHelpers.createDelegateSignature targetSignature.Attributes 
                                                                        (Seq.ofArray newParams)
                                                                        targetSignature.ReturnType
                 // ...and capture all the relevant values in it.
                 let lambdaHeader = LambdaHeader(newSignature, Array.ofList (target :: args))
                 let boundHeaderBlock = LambdaBoundHeaderBlock()

                 // We've captured the arguments in the following order:
                 //
                 //     * captures[0] is the original function.
                 //     * captures[1] ... captures[n] are the first few arguments.
                 //
                 // Let's index the capture list using:
                 let capture i = LambdaCapturedValueExpression(lambdaHeader, boundHeaderBlock, i) :> IExpression
                 let arguments = Array.mapi (fun i p -> ArgumentVariable(p, i).CreateGetExpression()) newParams

                 // Our new lambda's result is then the following call:
                 let invoc = InvocationExpression(
                                 capture 0,
                                 [for i in 1..n -> capture i] @ List.ofArray arguments
                             ) :> IExpression

                 // We have everything we need to build the partially-applied lambda!
                 LambdaExpression(lambdaHeader, ReturnStatement(invoc), boundHeaderBlock) :> IExpression
             else
                 // We have all the arguments we need for a call.
                 InvocationExpression(target, args) :> IExpression

    /// Creates a new partial application expression from the given
    /// target expression and list of argument expressions.
    new(target : IExpression, args : IExpression list) =
        PartialApplication(target, args, UnknownType())

    /// Gets this partial application expression's target.
    member this.Target = target

    /// Gets this partial application expression's argument list.
    member this.Arguments = args

    /// Gets this partial application expression's type.
    member this.Type = lowered.Value.Type

    override this.ToString() =
      let totalArgs = target.ToString() :: List.map (fun (x : IExpression) -> x.ToString()) args
      sprintf "apply(%s)" (String.concat ", " totalArgs)

    interface IExpression with
        member this.Optimize() = lowered.Value.Optimize()
        member this.Evaluate() = lowered.Value.Evaluate()
        member this.IsConstant = lowered.Value.IsConstant
        member this.Emit(cg) = lowered.Value.Emit(cg)

        member this.Type = lowered.Value.Type
        member this.Accept(visitor) =
            let visit : IExpression -> IExpression = visitor.Visit
            PartialApplication(visit target, List.map visit args, resultType) :> IExpression
