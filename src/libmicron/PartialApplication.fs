namespace libmicron

open Flame
open Flame.Compiler
open Flame.Compiler.Expressions
open Flame.Compiler.Statements
open Flame.Compiler.Variables
open Flame.Functional

/// An expression representing a possibly partial application of `target` to some arguments `args`.
type PartialApplication(target : IExpression, args : IExpression list) =
    let targetSignature = MethodType.GetMethod target.Type
    let isPartial = List.length args < Seq.length targetSignature.Parameters

    /// If this PartialApplication still needs more arguments, make it a LambdaExpression.
    /// If it has all the arguments it needs, it can be a call.
    let lowered =
        lazy if isPartial then
                 // Copy the attributes from the original function.
                 let methodHeader = FunctionalMemberHeader("", targetSignature.Attributes)
                 // If we already have n arguments, skip the first n parameters.
                 let n = List.length args
                 let newParams = Array.ofSeq (Seq.skip n targetSignature.Parameters)
                 // Construct the new function's signature...
                 let newSignature =
                     FunctionalMethod(methodHeader, null, true)
                         .WithParameters(fun _ -> Seq.ofArray newParams)
                         .WithReturnType(fun _ -> targetSignature.ReturnType)
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
    
    interface IExpression with
        member this.Optimize() = lowered.Value.Optimize()
        member this.Evaluate() = lowered.Value.Evaluate()
        member this.IsConstant = lowered.Value.IsConstant
        member this.Emit(cg) = lowered.Value.Emit(cg)

        member this.Type = lowered.Value.Type
        member this.Accept(visitor) =
            let visit : IExpression -> IExpression = visitor.Visit
            PartialApplication(visit target, List.map visit args) :> IExpression
