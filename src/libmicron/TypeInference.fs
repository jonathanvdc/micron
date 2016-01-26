namespace libmicron

open libcontextfree
open System
open System.Collections.Generic
open Flame
open Flame.Build
open Flame.Functional
open Flame.Compiler
open Flame.Compiler.Expressions
open Flame.Compiler.Statements
open Flame.Compiler.Visitors

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

    /// An active pattern for types. It tries to decompose
    /// the given type into either a method type, a generic
    /// instance type, an unknown (variable) type or a simple
    /// (constant) type.
    let (|MethodTy|GenericTy|UnknownTy|SimpleTy|) (ty : IType) =
        match MethodType.GetMethod ty with
        | null ->
            match ty with
            | :? UnknownType as ty -> UnknownTy ty
            | :? GenericType as ty -> GenericTy ty
            | _ -> SimpleTy ty
        | m ->
            MethodTy m

    /// Checks if the given type is or contains
    /// an unknown type.
    let rec containsUnknown : IType -> bool = function
    | UnknownTy _ -> true
    | GenericTy ty -> containsUnknown ty.Declaration || Seq.exists containsUnknown ty.GenericArguments
    | MethodTy m -> containsUnknown m.ReturnType || Seq.exists containsUnknown (m.Parameters.GetTypes() |> List.ofSeq)
    | SimpleTy _ -> false

    /// Converts the given type to a type constraint.
    let rec toConstraint : IType -> TypeConstraint = function
    | MethodTy m -> toFunctionConstraint m.ReturnType (m.Parameters.GetTypes() |> List.ofSeq)
    | UnknownTy ty -> Variable ty
    | GenericTy ty -> Instance(toConstraint ty.Declaration, ty.GenericArguments |> Seq.map toConstraint |> List.ofSeq)
    | SimpleTy ty -> Constant ty
    /// Creates a function constraint from the given return type
    /// and parameter type list.
    and toFunctionConstraint retType = function
    | [] -> toConstraint retType
    | param :: parameters -> Function(toConstraint param, toFunctionConstraint retType parameters)

    /// Flattens the given type constraint to a 
    /// flat list of parameter types, followed by 
    /// a return type.
    let rec uncurry : TypeConstraint -> TypeConstraint list * TypeConstraint = function
    | Function(argTy, retTy) -> 
        let extraArgTys, ty = uncurry retTy
        argTy :: extraArgTys, ty
    | ty -> [], ty
    
    /// Given a mapping function that converts unknown types to
    /// types, this function converts type constraints to types.
    let rec toType (mapping : UnknownType -> IType) : TypeConstraint -> IType = function
    | Variable vTy -> mapping vTy
    | Constant cTy -> cTy
    | Instance(declTy, argTys) -> (toType mapping declTy).MakeGenericType(List.map (toType mapping) argTys)
    | Function(_, _) as fConstraint -> 
        let argTys, retTy = uncurry fConstraint
        let parameters _ = argTys |> List.map (fun ty -> DescribedParameter("", toType mapping ty) :> IParameter)
                                  |> Seq.ofList
        let header = FunctionalMemberHeader("")
        let result = FunctionalMethod(header, null, true).WithReturnType(fun _ -> toType mapping retTy)
                                                         .WithParameters(parameters)
        MethodType.Create result
        
    /// Creates a function that converts type constraints to strings.
    /// Unknown types are assigned unique single-character names.
    let createShow () : TypeConstraint -> string =
        let dict = new System.Collections.Concurrent.ConcurrentDictionary<UnknownType, string>()
        let rec createName prefix offset k =
            let range = int 'z' + 1 - int 'a'
            let index = (dict.Count - offset) % range
            let result = prefix + string ('a' + char index)
            if dict.Values.Contains result then
                createName result (offset + index) k
            else
                result

        let rec show : TypeConstraint -> string = function
        | Constant x -> NameHelpers.nameType x
        | Variable x -> dict.GetOrAdd(x, createName "" 0)
        | Function(x, y) ->
            match x with
            | Function(_, _) -> "(" + show x + ")" + " -> " + show y
            | _ -> show x + " -> " + show y
        | Instance(x, ys) -> GenericNameExtensions.ChangeTypeArguments(show x, ys |> List.map show)

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
    let rec resolve (relations : (TypeConstraint * TypeConstraint * SourceLocation) list)
                    : Result<LinearMap<UnknownType, TypeConstraint>, LogEntry> =
        let show = createShow()
        let rec step (results : Result<LinearMap<UnknownType, TypeConstraint>, LogEntry>)
                     (left : TypeConstraint, right : TypeConstraint, srcLoc : SourceLocation)
                     : Result<LinearMap<UnknownType, TypeConstraint>, LogEntry> =
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
                    Error(LogEntry("Type error",
                                   MarkupHelpers.refer2 
                                       "Could not unify " (show (Variable tVar))
                                       " and " (show other)
                                       " because the resulting type would be infinite.",
                                   srcLoc))
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
                    step (step results (tArg1, tArg2, srcLoc)) (tRet1, tRet2, srcLoc)
                | Instance(tDecl1, tArgs1), Instance(tDecl2, tArgs2) ->
                    // First, resolve the generic declaration constraints.
                    let genDecls = step results (tDecl1, tDecl2, srcLoc)
                    // Create a list of source locations for
                    // debugging purposes.
                    let srcLocList = List.replicate (List.length tArgs1) srcLoc
                    // Next, resolve generic parameter constraints.
                    List.zip3 tArgs1 tArgs2 srcLocList |> List.fold step genDecls
                | t1, t2 ->
                    // Incompatible constant types mean trouble.
                    Error(LogEntry("Type error",
                                   MarkupHelpers.refer2
                                       "Could not unify incompatible types "
                                       (show t1) " and " (show t2) ".",
                                   srcLoc))
            | Error _ ->
                results

        List.fold step (Success LinearMap.empty) relations

    /// A type visitor that tries to find unknown types.
    type UnknownTypeVisitor(target : HashSet<UnknownType>) =
        inherit TypeTransformerBase()

        override this.ConvertTypeDefault (ty : IType) : IType =
            match ty with
            | :? UnknownType as ty -> target.Add ty |> ignore; ty :> IType
            | _ -> ty

    /// A node visitor that tries to find all unknown types.
    type UnknownTypeFinder() =
        inherit ContextlessVisitorBase()

        let mutable results = HashSet<UnknownType>()

        let tyConv = UnknownTypeVisitor(results)
        let memConv = MemberConverter(tyConv, new TypeMethodConverter(tyConv), 
                                      new TypeFieldConverter(tyConv))

        /// Gets the set of unknown types that this unknown type visitor has
        /// discovered so far.
        member this.UnknownTypes = results

        override this.Matches(stmt : IStatement) : bool =
            stmt :? IMemberNode
        override this.Matches(expr : IExpression) : bool =
            true
        
        override this.Transform(stmt : IStatement) : IStatement =
            match stmt with
            | :? IMemberNode as memberNode ->
                memberNode.ConvertMembers(memConv) |> ignore
            | _ ->
                ()
            stmt.Accept this
            
        override this.Transform(expr : IExpression) : IExpression =
            tyConv.Convert expr.Type |> ignore
            match expr with
            | :? IMemberNode as memberNode -> 
                memberNode.ConvertMembers(memConv) |> ignore
            | _ -> 
                ()
            expr.Accept this

    /// Extracts all unknown types from the given expression.
    let findUnknownTypes (expr : IExpression) : LinearSet<UnknownType> =
        let visitor = UnknownTypeFinder()

        visitor.Visit expr |> ignore
        LinearSet.ofSeq visitor.UnknownTypes

    /// A node visitor that makes up type constraints
    /// for unknown types.
    type TypeConstraintVisitor(initialConstraints : (TypeConstraint * TypeConstraint * SourceLocation) list,
                               topLevelRetType : IType) =
        inherit ContextlessVisitorBase()

        let mutable constraints = initialConstraints
        let mutable srcLoc : SourceLocation = null
        /// A variable that remembers the 
        /// current lambda's return type, if
        /// any.
        let mutable retType : IType = topLevelRetType

        /// Adds a constraint to this type constraint visitor's constraint list.
        let addConstraint (left : TypeConstraint) (right : TypeConstraint) : unit =
            constraints <- (left, right, srcLoc) :: constraints

        /// Adds a constraint to this type constraint visitor's constraint list.
        member this.AddConstraint (left : TypeConstraint) (right : TypeConstraint) : unit =
            addConstraint left right

        member this.Constraints = constraints

        override this.Matches (stmt : IStatement) : bool =
            true

        override this.Matches (expr : IExpression) : bool =
            true

        override this.Transform (stmt : IStatement) : IStatement =
            match stmt with
            | :? ISetVariableNode as varNode when varNode.Action = VariableNodeAction.Set ->
                addConstraint (toConstraint varNode.Value.Type) (toConstraint (varNode.GetVariable().Type))
                stmt.Accept this
            | :? IfElseStatement as select ->
                addConstraint (toConstraint select.Condition.Type) (toConstraint PrimitiveTypes.Boolean)
                stmt.Accept this
            | :? SourceStatement as srcStmt ->
                let newLoc = srcStmt.Location
                if newLoc <> null then
                    let oldLoc = srcLoc
                    srcLoc <- newLoc
                    let result = srcStmt.Accept this
                    srcLoc <- oldLoc
                    result
                else
                    srcStmt.Accept this
            | :? ReturnStatement as ret ->
                // Add lambda return types here.
                if retType <> null then
                    addConstraint (toConstraint retType) (toConstraint ret.Value.Type)
                stmt.Accept this
            | _ ->
                stmt.Accept this

        override this.Transform (expr : IExpression) : IExpression =
            match expr with
            | :? SelectExpression as select ->
                addConstraint (toConstraint select.Condition.Type) (toConstraint PrimitiveTypes.Boolean)
                let trueTy = toConstraint select.TrueValue.Type
                let falseTy = toConstraint select.FalseValue.Type
                addConstraint trueTy falseTy
                select.Accept this
            | :? PartialApplication as apply ->
                let targetTy = apply.Target.Type
                let argTys = List.map (fun (x : IExpression) -> x.Type) apply.Arguments
                let retType = apply.Type
                let funcConstraint = toFunctionConstraint retType argTys
                addConstraint (toConstraint targetTy) funcConstraint
                expr.Accept this
            | :? LambdaExpression as lambda ->
                // The expression analysis pass does not
                // equate a lambda's return type with
                // its expression's type, because it
                // simplifies the analysis implementation.
                // Thus, we must do so in the type inference
                // pass. We'll just remember the lambda
                // return type here, and unify that
                // with the actual return type of the
                // lambda when encountering a return 
                // statement.
                let oldRetType = retType
                retType <- lambda.Signature.ReturnType
                let result = expr.Accept this
                retType <- oldRetType
                result
            | :? SourceExpression as srcExpr ->
                let newLoc = srcExpr.Location
                if newLoc <> null then
                    let oldLoc = srcLoc
                    srcLoc <- newLoc
                    let result = srcExpr.Accept this
                    srcLoc <- oldLoc
                    result
                else
                    srcExpr.Accept this
            | _ ->
                expr.Accept this

    /// Finds all constraints in the given expression.
    /// The expression's result type is optionally associated 
    /// with an unknown type.
    let findConstraints (predefinedConstraints : (TypeConstraint * TypeConstraint * SourceLocation) list) 
                        (expr : IExpression) : (TypeConstraint * TypeConstraint * SourceLocation) list =
        let visitor = TypeConstraintVisitor(predefinedConstraints, null)
        visitor.Visit expr |> ignore
        visitor.Constraints

    /// Runs type inference on the given expression.
    /// All unknown types that can be resolved are 
    /// mapped to their known counterparts. 
    /// The remaining unkown types are stored
    /// in a set.
    let inferTypes (predefinedConstraints : (TypeConstraint * TypeConstraint * SourceLocation) list) 
                   (expr : IExpression) : Result<LinearMap<UnknownType, TypeConstraint> * LinearSet<UnknownType>, LogEntry> =
        let allUnknowns = findUnknownTypes expr
        let replaceResolved (resolved : LinearMap<UnknownType, TypeConstraint>) =
            resolved, LinearSet.difference allUnknowns resolved.Keys

        findConstraints predefinedConstraints expr |> resolve
                                                   |> Result.map replaceResolved

    /// Binds all items in the set of truly unknown types to
    /// generic parameters, and creates a mapping from
    /// unknown types to known types.
    let bindTypes (knownTypes : LinearMap<UnknownType, TypeConstraint>) 
                  (unknownTypes : LinearSet<UnknownType>)
                  (declMember : IGenericMember)
                  : IGenericParameter list * (UnknownType -> IType) =
        let show = createShow()
        // Maps a single unknown type to a generic parameter.
        let mapGenericParam ty =
            ty, DescribedGenericParameter(show (Variable ty), declMember) :> IGenericParameter

        // Now use the above function to map all truly unknown types 
        // to generic parameter types.
        let genParamMap = unknownTypes |> LinearSet.toList
                                       |> List.map mapGenericParam
                                       |> LinearMap.ofList

        // Resolves an unknown type by recursively trying to
        // match it with a generic parameter, or with
        // some known type constraint.
        let rec resolveUnknownType ty =
            match LinearMap.tryFind ty genParamMap with
            | Some result -> result :> IType
            | None -> 
                match LinearMap.tryFind ty knownTypes with
                | Some result -> toType resolveUnknownType result
                | None -> 
                    let show = createShow()
                    let unknown = sprintf "%A" (unknownTypes |> LinearSet.toList |> List.map (Variable >> show))
                    let known = sprintf "%A" (knownTypes |> LinearMap.toList |> List.map (fun (k, v) -> show (Variable k) + " := " + show v))
                    let bad = show (Variable ty)
                    raise (InvalidOperationException(sprintf "Found a free unknown type '%s' after type inference. Non-free unknown types: %s. Known types: %s" bad unknown known))

        // Compares two generic parameters based on their name.
        let compareLexico (left : IGenericParameter) (right : IGenericParameter) : int =
            compare left.Name right.Name

        // Sort generic parameters in lexicographical order, and
        // return.
        genParamMap.Values |> List.sortWith compareLexico, resolveUnknownType

    /// A type visitor that replaces unknown types by
    /// their known counterparts.
    type UnknownTypeResolvingVisitor(mapping : UnknownType -> IType) =
        inherit TypeTransformerBase()

        override this.ConvertTypeDefault (ty : IType) : IType =
            match ty with
            | :? UnknownType as ty -> this.Convert(mapping ty)
            | _ -> ty

    /// Substitutes all unknown types in the given expression
    /// according to the given mapping function. The expression
    /// is then rewritten in uncurried form.
    let resolveExpression (mapping : UnknownType -> IType) (expr : IExpression) : IExpression =
        let tyResolvedExpr = MemberNodeVisitor.ConvertTypes(UnknownTypeResolvingVisitor(mapping), expr)
        tyResolvedExpr |> ExpressionHelpers.uncurryExpression
                       |> ExpressionHelpers.PartialApplicationRecurryingVisitor().Visit
                       |> ExpressionHelpers.ReturnValueRecurryingVisitor(None).Visit