namespace libmicron

open libcontextfree
open System
open System.Collections.Generic
open Flame
open Flame.Functional
open Flame.Compiler
open Flame.Compiler.Expressions
open Flame.Compiler.Statements
open Flame.Compiler.Visitors

/// Defines an unknown type: a type that
/// has yet to be inferred.
type UnknownType() =
    inherit FunctionalType("?", null)

module TypeInference =
    /// Describes a type constraint.
    type TypeConstraint =
    /// Represents a type variable.
    | Variable of UnknownType
    /// Represents a "constant" type, which
    /// is really just a known type, such
    /// as a boolean, integer, or user-defined
    /// data structure.
    | Constant of IType
    /// Represents a generic instance type.
    | Instance of TypeConstraint * TypeConstraint list
    /// Represents a function type.
    | Function of TypeConstraint * TypeConstraint

    /// Substitutes an unknown type by a constraint
    /// in the given type constraint.
    let rec substitute (ty : UnknownType) (constr : TypeConstraint) : TypeConstraint -> TypeConstraint = function
    | Variable cTy when cTy = ty ->
        constr
    | Variable cTy ->
        Variable cTy
    | Constant cTy ->
        Constant cTy
    | Function(arg, ret) ->
        Function (substitute ty constr arg, substitute ty constr ret)
    | Instance(genType, args) ->
        Instance (substitute ty constr genType, args |> List.map (substitute ty constr)) 

    /// Checks if the given type is or contains
    /// an unknown type.
    let rec containsUnknown : IType -> bool = function
    | :? UnknownType -> true
    | :? GenericType as ty ->
        containsUnknown ty.Declaration || Seq.exists containsUnknown ty.GenericArguments
    | ty ->
        match MethodType.GetMethod ty with
        | null -> false
        | m -> containsUnknown m.ReturnType || Seq.exists containsUnknown (m.Parameters.GetTypes() |> List.ofSeq)

    /// Converts the given type to a type constraint.
    let rec toConstraint : IType -> TypeConstraint = function
    | :? GenericType as ty when containsUnknown ty ->
        Instance(toConstraint ty.Declaration, ty.GenericArguments |> Seq.map toConstraint |> List.ofSeq)
    | :? UnknownType as ty ->
        Variable ty
    | ty ->
        match MethodType.GetMethod ty with
        | null ->
            Constant ty
        | m ->
            let rec toFunctionConstraint retType = function
            | [] -> toConstraint retType
            | param :: parameters -> Function(toConstraint param, toFunctionConstraint retType parameters)

            toFunctionConstraint m.ReturnType (m.Parameters.GetTypes() |> List.ofSeq)

    /// Creates a function that converts type constraints to strings.
    /// Unknown types are assigned unique single-character names.
    let createShow () : TypeConstraint -> string =
        let dict = new System.Collections.Concurrent.ConcurrentDictionary<UnknownType, string>()
        let rec createName prefix offset k = 
            let range = int 'z' - int 'a'
            let index = (dict.Count - offset) % range
            let result = prefix + string ('a' + char index)
            if dict.Values.Contains result then
                createName result (offset + index) k
            else
                result
            
        let rec show : TypeConstraint -> string = function
        | Constant x -> x.FullName
        | Variable x -> dict.GetOrAdd(x, createName "" 0)
        | Function(x, y) -> 
            match x with
            | Function(_, _) -> "(" + show x + ")" + " -> " + show y
            | _ -> show x + " -> " + show y
        | Instance(x, ys) -> show x + "<" + (ys |> List.map show |> String.concat ", ") + ">"
        
        show

    /// Tells if the given unknown type occurs in the 
    /// given type constraint.
    let rec occursIn (left : UnknownType) : TypeConstraint -> bool = function
    | Variable x when x = left -> true
    | Variable _
    | Constant _ -> false
    | Function(x, y) -> occursIn left x || occursIn left y
    | Instance(x, ys) -> occursIn left x || List.exists (occursIn left) ys

    /// Tries to resolve the given set of type constraints.
    /// This corresponds to the "unification" step in most
    /// Hindley-Milner type systems.
    let rec resolve (relations : (TypeConstraint * TypeConstraint) list) 
                    : Result<LinearMap<UnknownType, TypeConstraint>> =
        let show = createShow()
        let rec step (results : Result<LinearMap<UnknownType, TypeConstraint>>) 
                     (left : TypeConstraint, right : TypeConstraint) 
                     : Result<LinearMap<UnknownType, TypeConstraint>> =
            match results with
            | Success substs ->
                let applySubst target tyFrom tyTo =
                    substitute tyFrom tyTo target

                match LinearMap.fold applySubst left substs, 
                      LinearMap.fold applySubst right substs with
                | Constant t1, Constant t2 when t1.IsEquivalent(t2) ->
                    // This is a pretty boring case, really.
                    results
                | Variable tVar1, Variable tVar2 when tVar1 = tVar2 ->
                    // Again, no magic here.
                    results
                | Variable tVar, other
                | other, Variable tVar when occursIn tVar other ->
                    // Unifying these types would result in an infinite type.
                    // I suppose that is somewhat undesirable.
                    Error("Could not unify '" + show (Variable tVar) + "' and '" + show other + 
                          "' because the resulting type would be infinite.")
                | Variable tVar, other
                | other, Variable tVar ->
                    // If either one of the input constraints are type variables,
                    // substitute them with the other constraint. Also make sure
                    // to apply this substitution rule to the results list itself.
                    let newSubsts = LinearMap.add tVar other (LinearMap.map (fun k v -> substitute tVar other v) substs)

                    Success newSubsts
                | Function(tArg1, tRet1), Function(tArg2, tRet2) ->
                    // Resolve generic declaration constraints both for the
                    // argument and return types.
                    step (step results (tArg1, tArg2)) (tRet1, tRet2)
                | Instance(tDecl1, tArgs1), Instance(tDecl2, tArgs2) ->
                    // First, resolve the generic declaration constraints.
                    let genDecls = step results (tDecl1, tDecl2)
                    // Next, resolve generic parameter constraints.
                    List.zip tArgs1 tArgs2 |> List.fold step genDecls
                | t1, t2 -> 
                    // Incompatible constant types mean trouble.
                    Error("Could not unify incompatible types '" + show t1 + "' and '" + show t2 + "'.")
            | Error _ -> 
                results

        List.fold step (Success LinearMap.empty) relations
        

    /// A node visitor that makes up type constraints
    /// for unknown types.
    type TypeConstraintVisitor(initialConstraints : (TypeConstraint * TypeConstraint) list) = 
        inherit ContextlessVisitorBase()

        let mutable constraints = initialConstraints

        /// Adds a constraint to this type constraint visitor's constraint list.
        let addConstraint (left : IType) (right : IType) : unit =
            constraints <- (toConstraint left, toConstraint right) :: constraints

        /// Adds a constraint to this type constraint visitor's constraint list.
        member this.AddConstraint (left : IType) (right : IType) : unit =
            addConstraint left right
            
        override this.Matches (stmt : IStatement) : bool = 
            true

        override this.Matches (expr : IExpression) : bool =
            true

        override this.Transform (stmt : IStatement) : IStatement =
            match stmt with
            | :? ISetVariableNode as varNode when varNode.Action = VariableNodeAction.Set ->
                addConstraint varNode.Value.Type (varNode.GetVariable().Type)
                stmt.Accept this
            | :? IfElseStatement as select ->
                addConstraint select.Condition.Type PrimitiveTypes.Boolean
                stmt.Accept this
            | _ ->
                stmt.Accept this

        override this.Transform (expr : IExpression) : IExpression = 
            match expr with
            | :? SelectExpression as select -> 
                addConstraint select.Condition.Type PrimitiveTypes.Boolean
                let trueTy = select.TrueValue.Type
                let falseTy = select.FalseValue.Type
                addConstraint trueTy falseTy
                addConstraint falseTy trueTy
                expr.Accept this
            | _ -> 
                expr.Accept this
