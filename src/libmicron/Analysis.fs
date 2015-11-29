namespace libmicron

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

    /// Analyzes the given expression parse tree.
    let rec analyzeExpression (scope : LocalScope) : ParseTree<string, Token> -> IExpression = function
    | ProductionNode(Constant Parser.ifThenElseIdentifier,
                     [TerminalLeaf ifKeyword; cond; _; ifExpr; _; elseExpr]) ->
        // A simple if-then-else expression
        EB.Select scope (analyzeExpression scope cond)
                        (analyzeExpression scope ifExpr)
                        (analyzeExpression scope elseExpr)
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
        analyzeExpression scope expr
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
            let localValue = analyzeExpression childScope value
            let defLocal, updatedScope = EB.Quickbind childScope localValue name.contents
            let defLocal = EB.Source (TokenHelpers.sourceLocation eq) defLocal
            // Take care of the `in expr` clause
            let innerExpr = analyzeExpression updatedScope expr
            let result = EB.Initialize defLocal innerExpr

            EB.Scope result updatedScope |> EB.Source here
        | _ ->
            // Local function declaration:  let name args = value in expr
            // This should also support recursion.
            let childScope = scope.ChildScope
            let createBody lambdaScope = analyzeExpression lambdaScope value

            let attributes = [PrimitiveAttributes.Instance.ConstantAttribute]
            let argumentNames = [for t in ParseTree.treeYield argsNode -> t.contents]

            // Create a lambda for the defined function.
            let header = FunctionalMemberHeader("", attributes, here)
            let makeParam argName = Flame.Build.DescribedParameter(argName, UnknownType()) :> IParameter
            let signature = FunctionalMethod(header, null, true)
                                .WithParameters(fun _ -> Seq.map makeParam argumentNames)
                                .WithReturnType(fun _ -> UnknownType() :> IType)

            // The lambda's identifier is equivalent to the name token's contents.
            let ident = name.contents
            // Create the lambda itself.
            let lambda = EB.RecursiveLambda createBody signature ident childScope

            // Bind this lambda to `name`.
            let defLocal, updatedScope = EB.Quickbind childScope lambda ident
            let defLocal = EB.Source (TokenHelpers.sourceLocation eq) defLocal

            // Take care of the `in expr` clause
            let innerExpr = analyzeExpression updatedScope expr
            let result = EB.Initialize defLocal innerExpr

            EB.Scope result updatedScope |> EB.Source here

    | ProductionNode(Constant Parser.applyIdentifier, [left; right]) as node ->
        // Function application

        // Left-hand side is the function to apply. Right-hand side is
        // the argument to apply the function to. Analyze both.
        let funcExpr = analyzeExpression scope left
        let argExpr = analyzeExpression scope right

        (match funcExpr.GetEssentialExpression() with
        | :? PartialApplication as appl ->
            // Try to combine partial applications as much as possible.
            // This may discard *some* debug information, but I think that's
            // a fair trade-off, even in a debug build ([-g] or [-Og]):
            // once lamba expressions are lowered, they create really
            // hard-to-read stack traces.
            (PartialApplication(appl.Target, List.append appl.Arguments [argExpr])) :> IExpression
        | _ ->
            // You can't win 'em all, I guess. However,
            // we do want to preserve the left-hand side's debug
            // info, if any - we don't have a lot to gain by discarding
            // source locations here.
            (PartialApplication(funcExpr, [argExpr])) :> IExpression
        ) |> EB.Source (TokenHelpers.treeSourceLocation node)
    | ProductionNode(Constant Parser.identifierIdentifier,
                     [TerminalLeaf ident]) ->
        // Identifier
        (match scope.GetVariable ident.contents with
        | Some local ->
            local.CreateGetExpression()
        | None ->
            EB.VoidError (LogEntry("Unresolved identifier", sprintf "Identifier '%s' could not be resolved." ident.contents))
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
    let rec analyzeLetDefinition (scope : GlobalScope) (name : Token) (parameterNames : Token list) 
                                 (value : ParseTree<string, Token>) (srcLoc : SourceLocation) 
                                 (declModule : IType) : Result<IMember * IStatement, LogEntry> =        
        match parameterNames with
        | [] -> 
            // Zero parameters. If this let-binding does not
            // contain any free unknown types, then we
            // can compile this down to a field. Otherwise,
            // we'll have to turn it into a method.

            // Analyze the field's value.
            let fieldVal = analyzeExpression (LocalScope(scope)) value
            // Run type inference.
            let inferredTypes = TypeInference.inferTypes fieldVal

            let createConstant (knownTypes, unknownTypes) = 
                if LinearSet.isEmpty unknownTypes then
                    // There are no free unknown types. We can reduce
                    // this let-binding to a field definition.
                    let resolveType = 
                        raise (InvalidOperationException("Free unknown type in field definition. " + 
                                                         "Something went wrong during type inference."))
                    // Resolve unknown types.
                    let fieldVal = TypeInference.resolveExpression resolveType fieldVal
                    // Create a new described field to hold the field's value.
                    let descField = DescribedField(name.contents, declModule, fieldVal.Type, true)
                    // Mark the newly created field as public.
                    descField.AddAttribute(AccessAttribute(AccessModifier.Public))
                    // Create an initialization statement for the newly created field.
                    let fieldInitStmt = FieldVariable(descField, null).CreateSetStatement(fieldVal)
                    descField :> IMember, fieldInitStmt
                else
                    // We found free unknown types. We'll have to create
                    // a (parameterless) method for this let-binding.
                    let descMethod = DescribedBodyMethod(name.contents, declModule)
                    // Mark the newly created method as static (it does not use a `this` pointer).
                    descMethod.IsStatic <- true
                    // Mark the newly created method as public.
                    descMethod.AddAttribute(AccessAttribute(AccessModifier.Public))
                    // Mark the newly created method as pure.
                    descMethod.AddAttribute(PrimitiveAttributes.Instance.ConstantAttribute)
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
                    descMethod.Body <- EB.ReturnUnchecked fieldVal |> EB.ToStatement

                    descMethod :> IMember, EmptyStatement.Instance :> IStatement

            Result.map createConstant inferredTypes
        | _ -> 
            Error (LogEntry("Unimplemented feature", "Let-binding functions are not supported yet.", srcLoc))

    /// Analyzes a module definition.
    let analyzeModule (scope : GlobalScope) (name : string) (definitions : ParseTree<string, Token> list) (declNs : INamespace) 
                      : IType =
        // Let's start by creating a type to hold the module's contents...
        let moduleType = DescribedType(name, declNs)
        // ... which we'll mark as static, and public.
        moduleType.AddAttribute(PrimitiveAttributes.Instance.StaticTypeAttribute)
        moduleType.AddAttribute(AccessAttribute(AccessModifier.Public))

        let initStmts = List<IStatement>()

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
                match analyzeLetDefinition scope name parameterTokens value srcLoc moduleType with
                | Success(:? IMethod as result, init) ->
                    moduleType.AddMethod result
                    initStmts.Add init
                | Success(:? IField as result, init) ->
                    moduleType.AddField result
                    initStmts.Add init
                | Success(:? IProperty as result, init) ->
                    moduleType.AddProperty result
                    initStmts.Add init
                | Success(_, _) ->
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

        // We're going to need a static constructor to
        // initialize fields with. This thing will be executed
        // at run-time before we access any of the 
        // module type's members.
        let staticCtor = DescribedBodyMethod("cctor", moduleType, PrimitiveTypes.Void, true)
        // Make the list of initialization statements
        // the body of the static constructor.
        staticCtor.Body <- BlockStatement(initStmts)
        // Register the static constructor with the module type.
        moduleType.AddMethod staticCtor

        moduleType :> IType

    /// Analyzes an entire program.
    let analyzeProgram (scope : GlobalScope) (contents : ParseTree<string, Token>) (declAsm : IAssembly) : INamespace =
        let flatDefs = Parser.flattenList Parser.programIdentifier contents

        // Checks if a parse tree is a module declaration.
        // If so, then relevant information is extracted.
        let isModule = function
        | ProductionNode(Constant Parser.moduleIdentifier, [TerminalLeaf moduleKeyword; TerminalLeaf ident; contents]) ->
            Some (ident.contents, Parser.flattenList Parser.identifierListIdentifier contents)
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
            result :> INamespace
        else
            // Otherwise, create a `<Program>` module and
            // put them in there.
            result.WithType(analyzeModule scope "<Program>" topLevelDefs) :> INamespace