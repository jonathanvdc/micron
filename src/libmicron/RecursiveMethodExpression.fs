namespace libmicron

open Flame
open Flame.Compiler
open Flame.Compiler.Expressions
open Flame.Compiler.Statements
open Flame.Compiler.Variables
open Flame.Functional

/// An expression that represents a delegate to its declaring method. 
/// A temporary return type and parameter list are also given, which 
/// are used to construct this expression's type.
/// This expression is useful when referring to a method whose types
/// have not been inferred yet, and thus does not have a generic 
/// parameter list yet, even though it may have one later on.
type RecursiveMethodExpression(declMethod : IMethod, returnType : IType, parameters : IParameter list) =
    let targetSignature = 
        let header = FunctionalMemberHeader(declMethod.Name, declMethod.Attributes)
        FunctionalMethod(header, null, true).WithReturnType(fun _ -> returnType)
                                            .WithParameters(fun _ -> Seq.ofList parameters)

    /// Gets the recursive method's declaring method.
    member this.DeclaringMethod = declMethod

    /// Lowers the method this recursive method will
    /// create a delegate for.
    member this.Target =
        if declMethod.get_IsGeneric() then
            declMethod.MakeGenericMethod(declMethod.GenericParameters |> Seq.cast)
        else
            declMethod

    /// Lowers this recursive method delegate 
    /// expression to a direct method delegate 
    /// expression.
    member this.Lowered =
        let callObj = 
            if declMethod.IsStatic then 
                null 
            else
                ThisVariable(declMethod.DeclaringType).CreateGetExpression()

        GetMethodExpression(this.Target, callObj)

    /// Gets the tentatively defined signature of the target method.
    member this.TargetSignature = 
        targetSignature

    /// Gets this recursive method expression's type.
    member this.Type =
        MethodType.Create this.TargetSignature

    override this.ToString() =
        sprintf "rec-method(%s)" declMethod.Name

    interface IExpression with
        member this.Optimize() = this.Lowered.Optimize()
        member this.Evaluate() = this.Lowered.Evaluate()
        member this.IsConstant = this.Lowered.IsConstant
        member this.Emit(cg) = this.Lowered.Emit(cg)

        member this.Type = this.Type
        member this.Accept(visitor) =
            this :> IExpression
