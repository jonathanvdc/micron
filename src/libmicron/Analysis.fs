﻿namespace libmicron

open libmicron.ConstantPattern
open libcontextfree
open Flame
open Flame.Build
open Flame.Compiler
open Flame.Compiler.Expressions
open Flame.Compiler.Statements
open Flame.Compiler.Variables
open Flame.Functional
open System
open System.Collections.Generic

/// A semantic analysis module for
/// micron parse trees.
module Analysis =
    module EB = ExpressionBuilder

    let builtinTypes =
        Map.ofList
            [
                "int8", PrimitiveTypes.Int8
                "int16", PrimitiveTypes.Int16
                "int32", PrimitiveTypes.Int32
                "int64", PrimitiveTypes.Int64

                "uint8", PrimitiveTypes.UInt8
                "uint16", PrimitiveTypes.UInt16
                "uint32", PrimitiveTypes.UInt32
                "uint64", PrimitiveTypes.UInt64

                "float32", PrimitiveTypes.Float32
                "float64", PrimitiveTypes.Float64

                "bool", PrimitiveTypes.Boolean
                "char", PrimitiveTypes.Char
            ]

    /// Names the given type.
    let rec nameType (ty : IType) : string = 
        match Map.tryFindKey (fun _ item -> item = ty) builtinTypes with
        | Some name -> name
        | None ->
            match ty with
            | :? GenericType as ty ->
                nameType (ty.Declaration) + "<" + (ty.GenericArguments |> Seq.map nameType |> String.concat ", ") + ">"
            | _ ->
                match MethodType.GetMethod ty with
                | null -> ty.FullName
                | signature -> nameFunction signature.ReturnType (signature.Parameters.GetTypes() |> List.ofSeq)
    and nameFunction (retTy : IType) = function
    | [] -> nameType retTy
    | argTy :: argTys -> 
        match MethodType.GetMethod argTy with
        | null -> nameType argTy + " " + nameFunction retTy argTys
        | _ -> "(" + nameType argTy + ") " + nameFunction retTy argTys

    /// Creates a mapping from names to variables
    /// that represents the given method option's
    /// parameter list.
    let getParameters = function
    | None -> Map.empty
    | Some (func : IMethod) ->
        func.Parameters |> Seq.mapi (fun i param -> param, i)
                        |> Seq.fold (fun result (param, i) -> Map.add param.Name (ArgumentVariable(param, i) :> IVariable) result) Map.empty

    type DefinitionMap = Map<string, IMethod>

    /// Analyzes the given expression parse tree.
    let rec analyzeExpression (previousDefinitions : DefinitionMap) (scope : LocalScope) : ParseTree<string, Token> -> IExpression = function
    | ProductionNode(Constant Parser.ifThenElseIdentifier,
                     [TerminalLeaf ifKeyword; cond; _; ifExpr; _; elseExpr]) ->
        // A simple if-then-else expression
        EB.Select scope (analyzeExpression previousDefinitions scope cond)
                        (analyzeExpression previousDefinitions scope ifExpr)
                        (analyzeExpression previousDefinitions scope elseExpr)
            |> EB.Source (TokenHelpers.sourceLocation ifKeyword)
    | ProductionNode(Constant Parser.literalIntIdentifier,
                     [TerminalLeaf token]) ->
        // Integer literal
        (match System.Int32.TryParse token.contents with
        | (true, i) -> EB.ConstantInt32 i
        | (false, _) -> EB.Error (LogEntry("Invalid integer literal",
                                           sprintf "'%s' could not be parsed as a valid integer literal." token.contents))
                                 (EB.ConstantInt32 0)
        ) |> EB.Source (TokenHelpers.sourceLocation token)
    | ProductionNode(Constant Parser.literalDoubleIdentifier,
                     [TerminalLeaf token]) ->
        // Double literal
        (match System.Double.TryParse token.contents with
        | (true, d) -> EB.ConstantFloat64 d
        | (false, _) -> EB.Error (LogEntry("Invalid double literal",
                                           sprintf "'%s' could not be parsed as a valid double literal." token.contents))
                                 (EB.ConstantFloat64 0.0)
        ) |> EB.Source (TokenHelpers.sourceLocation token)
    | ProductionNode(Constant Parser.parenIdentifier,
                     [TerminalLeaf lParen; expr; TerminalLeaf rParen]) ->
        // Parentheses
        analyzeExpression previousDefinitions scope expr
            |> EB.Source (CompilerLogExtensions.Concat(TokenHelpers.sourceLocation lParen,
                                                       TokenHelpers.sourceLocation rParen))
    | ProductionNode(Constant Parser.letIdentifier,
                     [ProductionNode(
                          Constant Parser.letDefinitionIdentifier,
                          [TerminalLeaf letKeyword
                           TerminalLeaf name
                           ProductionNode(Constant Parser.identifierListIdentifier, args) as argsNode
                           TerminalLeaf eq
                           value])
                      _
                      expr]) ->
        let here = TokenHelpers.sourceLocation letKeyword
        match args with
        | [] ->
            // Local variable declaration:  `let name = value in expr`
            let childScope = scope.ChildScope

            // First, bind `value` to `name`.
            let localValue = analyzeExpression previousDefinitions childScope value
            let defLocal, updatedScope = EB.Quickbind childScope localValue name.contents
            let defLocal = EB.Source (TokenHelpers.sourceLocation eq) defLocal
            // Take care of the `in expr` clause
            let innerExpr = analyzeExpression previousDefinitions updatedScope expr
            let result = EB.Initialize defLocal innerExpr

            EB.Scope result updatedScope |> EB.Source here
        | _ ->
            // Local function declaration:  let name args = value in expr
            // This should also support recursion.
            let childScope = scope.ChildScope
            let createBody lambdaScope = analyzeExpression previousDefinitions lambdaScope value
            // Add a source location for diagnostics purposes.
            let attributes = [
                              PrimitiveAttributes.Instance.ConstantAttribute
                              SourceLocationAttribute(here) :> IAttribute
                             ]
            let argumentNames = [for t in ParseTree.treeYield argsNode -> t.contents]

            // Create a lambda for the defined function.
            let makeParam argName = Flame.Build.DescribedParameter(argName, UnknownType()) :> IParameter
            let signature = TypeHelpers.createDelegateSignature attributes (Seq.map makeParam argumentNames) (UnknownType() :> IType)

            // The lambda's identifier is equivalent to the name token's contents.
            let ident = name.contents
            // Create the lambda itself.
            let lambda = EB.RecursiveLambda createBody signature ident childScope

            // Bind this lambda to `name`.
            let defLocal, updatedScope = EB.Quickbind childScope lambda ident
            let defLocal = EB.Source (TokenHelpers.sourceLocation eq) defLocal

            // Take care of the `in expr` clause
            let innerExpr = analyzeExpression previousDefinitions updatedScope expr
            let result = EB.Initialize defLocal innerExpr

            EB.Scope result updatedScope |> EB.Source here

    | ProductionNode(Constant Parser.applyIdentifier, [left; right]) as node ->
        // Function application

        // Left-hand side is the function to apply. Right-hand side is
        // the argument to apply the function to. Analyze both.
        let funcExpr = analyzeExpression previousDefinitions scope left
        let argExpr = analyzeExpression previousDefinitions scope right

        // Create a partial application expression, and wrap that
        // in an auto-invoke expression.
        AutoInvokeExpression(PartialApplication(funcExpr, [argExpr]))
            |> EB.Source (TokenHelpers.treeSourceLocation node)
    | ProductionNode(Constant Parser.identifierIdentifier,
                     [TerminalLeaf ident]) ->
        // Identifier
        let name = ident.contents
        (match scope.GetVariable name with
        | Some local ->
            local.CreateGetExpression()
        | None ->
            // If it's not a local, maybe we defined it earlier?
            match Map.tryFind name previousDefinitions with
            | Some func ->
                let func' =
                    if Seq.isEmpty func.GenericParameters
                        then func
                        else func.MakeGenericMethod(func.GenericParameters
                                                    |> Seq.map (fun _ -> UnknownType() :> IType))

                GetMethodExpression(func', null) :> IExpression
            | None ->
                EB.VoidError (LogEntry("Unresolved identifier", sprintf "Identifier '%s' could not be resolved." name))
        ) |> EB.Source (TokenHelpers.sourceLocation ident)
    | ProductionNode(nonterm, _) as node ->
        // Unimplemented node type.
        // This just means that a construct has been defined in the grammar,
        // and that the semantic analysis pass does not support it yet.
        EB.VoidError (LogEntry("Unimplemented node type", sprintf "'%s' nodes have not been implemented yet." nonterm))
            |> EB.Source (TokenHelpers.treeSourceLocation node)
    | TerminalLeaf(term) ->
        // Unexpected terminal leaf.
        // This points to an error in the grammar.
        EB.VoidError (LogEntry("Unexpected raw token", sprintf "Token '%s' was completely unexpected here." term.contents))
            |> EB.Source (TokenHelpers.sourceLocation term)

    /// Analyzes a let-definition.
    let rec analyzeLetDefinition (previousDefinitions : DefinitionMap) (scope : GlobalScope)
                                 (name : Token) (parameterNames : Token list) 
                                 (value : ParseTree<string, Token>) (srcLoc : SourceLocation) 
                                 (declModule : IType) : Result<IMember, LogEntry> =  
                                 
        /// Sets the given described method's body statement, uncurrying
        /// its signature in the process.
        let setBody (target : DescribedBodyMethod) (body : IStatement) : unit =
            // A single step in the uncurrying process.
            let uncurryStep (firstParamIndex : int) (parameters : IParameter list) (body : IStatement) : IStatement =
                // Step 1: insert those parameters!
                for item in parameters do
                    target.AddParameter item
                // Step 2: uncurry all return statements.
                ExpressionHelpers.uncurryReturnStatements firstParamIndex parameters body

            // Now uncurry the signature and body.
            let signature, body = TypeHelpers.uncurry uncurryStep body target
            // Set the return type.
            target.ReturnType <- signature.ReturnType
            // Set the body.
            target.Body <- body
                                       
        match parameterNames with
        | [] -> 
            // Zero parameters.

            // Analyze the field's value.
            let fieldVal = analyzeExpression previousDefinitions (LocalScope(scope)) value
            // Run type inference. We don't need
            // to invent a return type here, because
            // constants can't call themselves
            // recursively. Pass None to highlight that fact.
            let inferredTypes = TypeInference.inferTypes [] fieldVal

            let createConstant (knownTypes, unknownTypes) = 
                // We'll create a (parameterless) method for this let-binding.
                // We *could* also create a field here, but this has some disadvantages:
                //   * requires initialization --> requires static constructor
                //   * makes dead member elimination harder 
                //     (fields are initialized in the static constructor, which cannot be eliminated)
                let descMethod = DescribedBodyMethod(name.contents, declModule)
                // Mark the newly created method as static (it does not use a `this` pointer).
                descMethod.IsStatic <- true
                // Mark the newly created method as public.
                descMethod.AddAttribute(AccessAttribute(AccessModifier.Public))
                // Mark the newly created method as pure.
                descMethod.AddAttribute(PrimitiveAttributes.Instance.ConstantAttribute)
                // Add a source location for diagnostics purposes.
                descMethod.AddAttribute(SourceLocationAttribute(TokenHelpers.sourceLocation name))
                // Bind the free unknown types to generic parameters.
                let genParams, resolveType = TypeInference.bindTypes knownTypes unknownTypes descMethod
                // Add all generic parameters to the method.
                for item in genParams do
                    descMethod.AddGenericParameter item
                    
                // Now, substitute unknown types in the let-binding's body.
                let fieldVal = TypeInference.resolveExpression resolveType fieldVal

                // Set the newly created method's return type
                // to the value's type.
                descMethod.ReturnType <- fieldVal.Type

                // Return the expression's value.
                setBody descMethod (EB.ReturnUnchecked fieldVal |> EB.ToStatement)

                descMethod :> IMember

            Result.map createConstant inferredTypes
        | _ -> 
            // One or more parameters. We'll compile this
            // let-binding down to a method.

            // Let's start by creating a method.
            let descMethod = DescribedBodyMethod(name.contents, declModule)

            // Mark that method as static, public and pure.
            descMethod.IsStatic <- true
            descMethod.AddAttribute(AccessAttribute(AccessModifier.Public))
            descMethod.AddAttribute(PrimitiveAttributes.Instance.ConstantAttribute)
            // Add a source location for diagnostics purposes.
            descMethod.AddAttribute(SourceLocationAttribute(TokenHelpers.sourceLocation name))

            // We don't know what the above method's parameter
            // types are. So instead of creating a parameter list
            // right now, we'll just invent a bunch of fake parameters.
            // Later on, we'll run type inference, resolve those
            // fake parameters' types, and create an actual parameter list.
            let unknownParamDesc = parameterNames |> List.map (fun tok -> tok.contents, UnknownType())
            let unknownParams = unknownParamDesc |> List.map (fun (name, ty) -> DescribedParameter(name, ty) :> IParameter)

            // The same goes for the return type. We just
            // don't know what its type is.
            let unknownRetType = UnknownType()

            // Create a local scope from the declaring scope.
            let localScope = LocalScope(scope)
            // Register the parameter list as variables.
            let localScope = unknownParams |> List.mapi (fun i p -> i, p)
                                           |> List.fold (fun (localScope : LocalScope) (i, p) -> 
                                               localScope.WithVariable (ArgumentVariable(p, i)) p.Name) localScope
            // Create a delegate to the (recursive) method we're building here.
            let recDeleg = RecursiveMethodExpression(descMethod, unknownRetType, unknownParams)
            // Then add that delegate to the local scope.
            let localScope = localScope.WithVariable (ExpressionVariable(recDeleg)) descMethod.Name

            // Analyze the body expression's value.
            let bodyExpr = analyzeExpression previousDefinitions localScope value

            let createFunction (knownTypes, unknownTypes) = 
                // Add unknown parameter types if necessary
                let unknownTypes = unknownParamDesc |> List.fold (fun unknownTypes (_, ty) -> 
                    if LinearMap.containsKey ty knownTypes 
                        then unknownTypes 
                        else LinearSet.add ty unknownTypes) unknownTypes

                // Bind the free unknown types to generic parameters.
                let genParams, resolveType = TypeInference.bindTypes knownTypes unknownTypes descMethod
                // Add all generic parameters to the method.
                for item in genParams do
                    descMethod.AddGenericParameter item

                // Add all parameters to the method.
                for (name, ty) in unknownParamDesc do
                    descMethod.AddParameter(DescribedParameter(name, resolveType ty))
                    
                // Resolve and store the return type.
                descMethod.ReturnType <- resolveType unknownRetType

                // Now, substitute unknown types in the let-binding's body.
                let bodyExpr = TypeInference.resolveExpression resolveType bodyExpr

                // Return the expression's value.
                setBody descMethod (bodyExpr |> EB.ReturnUnchecked
                                             |> EB.Source (TokenHelpers.treeSourceLocation value) 
                                             |> EB.ToStatement)

                descMethod :> IMember

            /// Constrain the function's return type to the
            /// function body's result type.
            let initConstraints = [TypeInference.Variable unknownRetType,
                                   TypeInference.toConstraint bodyExpr.Type, 
                                   srcLoc]
            // Run type inference.
            let inferredTypes = TypeInference.inferTypes initConstraints bodyExpr

            Result.map createFunction inferredTypes

    /// Analyzes a module definition.
    let analyzeModule (scope : GlobalScope) (name : string) (definitions : ParseTree<string, Token> list) (declNs : INamespace) 
                      : IType =
        // Let's start by creating a type to hold the module's contents...
        let moduleType = DescribedType(name, declNs)
        // ... which we'll mark as static, and public.
        moduleType.AddAttribute(PrimitiveAttributes.Instance.StaticTypeAttribute)
        moduleType.AddAttribute(AccessAttribute(AccessModifier.Public))

        // Keep track of defined methods, so that definitions further
        // down the module may make use of them.
        let mutable defined : DefinitionMap =
            Map.empty

        // Add a method to the DefinitionMap `defined`, but raise a warning
        // if it already has a previous definition.
        let addMethod (func : IMethod) (defined : DefinitionMap) : DefinitionMap =
            if Map.containsKey (func.Name) defined then
                scope.Log.LogWarning(LogEntry("Shadowed definition", sprintf "This definition of '%s' shadows a previous one:" func.Name, func.GetSourceLocation()))
            Map.add func.Name func defined

        // Iterate over the list of definitions in the module.
        for def in definitions do
            match def with
            | ProductionNode(Constant Parser.letDefinitionIdentifier,
                             [TerminalLeaf letKeyword
                              TerminalLeaf name
                              ProductionNode(Constant Parser.identifierListIdentifier, _) as argsNode
                              TerminalLeaf eq
                              value]) ->
                // Let-binding. We'll analyze it, and add the resulting
                // member to the module type. If we need some
                // kind of initialization for this member, then
                // we'll add a statement to this initialization 
                // statement list.
                let parameterTokens = ParseTree.treeYield argsNode
                let srcLoc = TokenHelpers.sourceLocation letKeyword
                match analyzeLetDefinition defined scope name parameterTokens value srcLoc moduleType with
                | Success(:? IMethod as result) ->
                    moduleType.AddMethod result
                    defined <- addMethod result defined
                | Success(:? IField as result) ->
                    moduleType.AddField result
                | Success(:? IProperty as result) ->
                    moduleType.AddProperty result
                | Success(_) ->
                    scope.Log.LogError(LogEntry("Unknown member type", sprintf "Member type of let-binding '%s' was not unexpected." name.contents, srcLoc))
                | Error msg ->
                    scope.Log.LogError msg
            | ProductionNode(nonterm, _) as node ->
                // Unimplemented node type.
                // This just means that a construct has been defined in the grammar,
                // and that the semantic analysis pass does not support it yet.
                scope.Log.LogError(LogEntry("Unimplemented node type", sprintf "'%s' nodes have not been implemented yet." nonterm, TokenHelpers.treeSourceLocation node))
            | TerminalLeaf(term) ->
                // Unexpected terminal leaf.
                // This points to an error in the grammar.
                scope.Log.LogError(LogEntry("Unexpected raw token", sprintf "Token '%s' was completely unexpected here." term.contents, term.sourceLocation))

        moduleType :> IType

    /// Analyzes an entire program.
    let analyzeProgram (scope : GlobalScope) (contents : ParseTree<string, Token>) (declAsm : IAssembly) : INamespaceBranch =
        let flatDefs = Parser.flattenList Parser.programIdentifier contents

        // Checks if a parse tree is a module declaration.
        // If so, then relevant information is extracted.
        let isModule = function
        | ProductionNode(Constant Parser.moduleIdentifier, [TerminalLeaf moduleKeyword; TerminalLeaf ident; contents]) ->
            Some (ident.contents, Parser.flattenList Parser.letDefinitionListIdentifier contents)
        | _ ->
            None
        
        // Create a namespace, which will contain everything in
        // this program.
        let namespaceHeader = FunctionalMemberHeader("")
        let result = FunctionalNamespace(namespaceHeader, declAsm)

        // First, analyze all module declarations.
        let result = List.choose isModule flatDefs
                        |> List.fold (fun (result : FunctionalNamespace) (name, contents) -> 
                            result.WithType(analyzeModule scope name contents)) result
        
        
        // Then, analyze top-level declarations.
        let topLevelDefs = List.filter (fun x -> Option.isNone (isModule x)) flatDefs

        if List.isEmpty topLevelDefs then
            // If there are no top-level declarations,
            // then we're done here.
            result :> INamespaceBranch
        else
            // Otherwise, create a `<Program>` module and
            // put them in there.
            result.WithType(analyzeModule scope "<Program>" topLevelDefs) :> INamespaceBranch

    /// Analyzes an assembly. This does the same thing as analyzing a program,
    /// except that the result is wrapped in an assembly of the given name.
    let analyzeAssembly (scope : GlobalScope) (name : string) (contents : ParseTree<string, Token>) : IAssembly =
        let asm = DescribedAssembly(name, scope.Environment)
        asm.MainNamespace <- analyzeProgram scope contents asm
        asm :> IAssembly