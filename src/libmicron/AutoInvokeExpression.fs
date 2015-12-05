namespace libmicron

open Flame
open Flame.Compiler
open Flame.Compiler.Expressions
open Flame.Compiler.Statements

/// Defines auto-invoke expressions, which
/// recursively invoke parameterless delegate
/// expressions, if such an expression is
/// indeed encountered.
/// We need an expression like this because
/// type inference treats parameterless functions
/// as values.
type AutoInvokeExpression(target : IExpression) =
    /// Recursively invokes parameterless delegate expressions.
    let rec autoInvoke (tgt : IExpression) : IExpression =
        match MethodType.GetMethod(tgt.Type) with
        | signature when signature <> null && 
                         Seq.isEmpty signature.Parameters ->
            // We found a parameterless delegate expression.
            // Invoke it!
            autoInvoke (InvocationExpression(tgt, Seq.empty))
        | _ -> 
            // The given expression is not a parameterless
            // delegate expression. Nothing to do here.
            tgt
            
    /// A lowered version of this expression.
    let lowered = lazy autoInvoke target

    /// Gets the target expression of this
    /// auto-invoke expression.
    member this.Target = target

    /// Checks if this auto-invoke expression is
    /// trivial, i.e. it doesn't do anything
    /// on its own.
    member this.IsTrivial = lowered.Value = target

    /// Gets this auto-invoke expression's
    /// type.
    member this.Type = lowered.Value.Type

    interface IExpression with
        member this.Type = lowered.Value.Type
        member this.IsConstant = lowered.Value.IsConstant
        member this.Optimize() = lowered.Value.Optimize()
        member this.Emit(cg) = lowered.Value.Emit(cg)
        member this.Evaluate() = lowered.Value.Evaluate()
        member this.Accept(visitor) =
            AutoInvokeExpression(visitor.Visit target) :> IExpression