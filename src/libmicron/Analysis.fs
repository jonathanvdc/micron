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
open Pixie
open System
open System.Collections.Generic

/// A semantic analysis module for
/// micron parse trees.
module Analysis =
    module EB = ExpressionBuilder

    /// Creates a mapping from names to variables
    /// that represents the given method option's
    /// parameter list.
    let getParameters = function
    | None -> Map.empty
    | Some (func : IMethod) ->
        func.Parameters |> Seq.mapi (fun i param -> param, i)
                        |> Seq.fold (fun result (param, i) -> Map.add param.Name (ArgumentVariable(param, i) :> IVariable) result) Map.empty

    type DefinitionMap = { functions : Map<string, IMethod>; prec : Map<string, Parser.OpFixity> }

    /// Creates an error expression whose type should be
    /// fixed by type inference. This can be preferable to
    /// EB.VoidError: the type inference algorithm won't complain
    /// about type mismatches if this is used.
    let unknownError (entry : LogEntry) =
        EB.Error entry (UnknownExpression(UnknownType()))

    /// Tries to resolve a name.
    let resolveName (nameType : string) (previousDefinitions : DefinitionMap) (scope : LocalScope) (token : Token) =
        let name = token.contents
        match scope.GetVariable name with
        | Some local ->
            AutoInvokeExpression(local.CreateGetExpression()) :> IExpression
        | None ->
            // If it's not a local, maybe we defined it earlier?
            match Map.tryFind name previousDefinitions.functions with
            | Some func ->
                let func' =
                    if Seq.isEmpty func.GenericParameters
                        then func
                        else func.MakeGenericMethod(func.GenericParameters
                                                    |> Seq.map (fun _ -> UnknownType() :> IType))

                AutoInvokeExpression(GetMethodExpression(func', null)) :> IExpression
            | None ->
                unknownError (LogEntry("Unresolved " + nameType, sprintf "'%s' could not be resolved." name))

    /// Tries to resolve an identifier.
    let resolveIdentifier = resolveName "identifier"

    /// Tries to resolve an operator.
    let resolveOperator = resolveName "operator"

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
        resolveIdentifier previousDefinitions scope ident 
            |> EB.Source (TokenHelpers.sourceLocation ident)
    | ProductionNode(Constant Parser.operatorIdentifier, _) as node ->
        // Operator application
        let prec name = 
            match Map.tryFind name previousDefinitions.prec with
            | Some x -> x
            | None -> Parser.InfixLeft 9
        match Parser.reassociate prec node with
        | ProductionNode(Constant Parser.operatorIdentifier, [left; TerminalLeaf op; right])
          when op.tokenType = TokenType.OperatorToken ->
            let leftExpr = analyzeExpression previousDefinitions scope left
            let rightExpr = analyzeExpression previousDefinitions scope right
            let funcExpr = resolveOperator previousDefinitions scope op 
                            |> EB.Source (TokenHelpers.sourceLocation op)
            PartialApplication(funcExpr, [leftExpr; rightExpr])
                |> EB.Source (TokenHelpers.treeSourceLocation node)
        | _ ->
            EB.VoidError (LogEntry("Something went wrong", "Invalid operator reassociation.", TokenHelpers.treeSourceLocation node))
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
                                 (memberName : string) (name : Token) (parameterNames : Token list) 
                                 (value : ParseTree<string, Token>) (srcLoc : SourceLocation) 
                                 (declModule : IType) : Result<DescribedBodyMethod, LogEntry> =  
                                 
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

        // Let's start by creating a method.
        let descMethod = DescribedBodyMethod(memberName, declModule)

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

            descMethod

        /// Constrain the function's return type to the
        /// function body's result type.
        let initConstraints = [TypeInference.Variable unknownRetType,
                               TypeInference.toConstraint bodyExpr.Type, 
                               srcLoc]
        // Run type inference.
        let inferredTypes = TypeInference.inferTypes initConstraints bodyExpr

        Result.map createFunction inferredTypes

    
    // Add a method to a DefinitionMap, but raise a warning
    // if it already has a previous definition.
    let addMethod (log : ICompilerLog) (name : string) (func : IMethod) (defined : DefinitionMap) : DefinitionMap =
        match Map.tryFind name defined.functions with
        | Some predef when func.DeclaringType = predef.DeclaringType ->
            // Don't allow redefinitions in the same module, because
            // this could introduce more than one method with the same
            // signature -- other modules wouldn't be able to differentiate
            // between the newer and the older version. Plus, declaring a
            // method with the same signature more than once is invalid IR.
            // We'll log an error, which we'll format like so:
            //
            // <message>
            // <diagnostic>
            // <remark>

            let message = MarkupNode(NodeConstants.TextNodeType, sprintf "'%s' is defined more than once in the same module. " name) :> IMarkupNode
            let diagnostics = CompilerLogExtensions.CreateDiagnosticsNode(func.GetSourceLocation())
            let remark = CompilerLogExtensions.CreateRemarkDiagnosticsNode(predef.GetSourceLocation(), "Previous definition: ")
            log.LogError(LogEntry("Redefinition", seq [message; diagnostics; remark]))
        | Some predef when Warnings.Instance.Shadow.UseWarning(log.Options) ->
            // Log a warning if the new method shadows a previous method.
            //
            // We'll format that warning like so:
            //
            // <message> [-Wshadows]
            // <diagnostic>
            // <remark>

            let message = MarkupNode(NodeConstants.TextNodeType, sprintf "This definition of '%s' shadows a previous one. " name) :> IMarkupNode
            let cause = Warnings.Instance.Shadow.CauseNode
            let diagnostics = CompilerLogExtensions.CreateDiagnosticsNode(func.GetSourceLocation())
            let remark = CompilerLogExtensions.CreateRemarkDiagnosticsNode(predef.GetSourceLocation(), "Previous definition: ")
            log.LogWarning(LogEntry("Shadowed definition", seq [message; cause; diagnostics; remark]))
        | _ ->
            ()
        let newFunctions = Map.add name func defined.functions
        { defined with functions = newFunctions }

    /// A warning for badly formatted mangled names.
    let badNameWarning = WarningDescription("bad-name", Warnings.Instance.Build)

    /// Opens the module with the given name, and imports its contents.
    /// An error is reported if something goes wrong.
    let openModule (scope : GlobalScope) (moduleName : Token) (defined : DefinitionMap) : DefinitionMap =
        // Try to bind the module name to a type.
        let moduleTy = scope.Binder.Bind moduleName.contents
        match moduleTy with
        | null ->
            // We couldn't resolve the given module name. That's too bad.
            // Create a log entry to report this.
            scope.Log.LogError(LogEntry("Unresolved module name", 
                                        sprintf "Could not resolve module '%s'. Did you forget to link it or list it as a library dependency?" moduleName.contents, 
                                        TokenHelpers.sourceLocation moduleName))
            defined
        | _ ->
            // Import all static methods.
            let foldMethod defined (item : IMethod) =
                 if item.IsStatic then
                    if NameHelpers.isOperatorName item.Name then
                        match NameHelpers.tryDemangleOperatorName item.Name with
                        | Success (opName, opFixity) ->
                            // Import this as an operator.
                            { addMethod scope.Log opName item defined with prec = Map.add opName opFixity defined.prec }
                        | Error e ->
                            // This thing looked like an operator, but its name wasn't mangled correctly.
                            if badNameWarning.UseWarning(scope.Log.Options) then
                                let msg = badNameWarning.CreateMessage( 
                                             sprintf "Function '%s' in module '%s' looks like an operator, but its name was incorrectly formatted. Skipping it. " item.Name moduleName.contents)
                                let remark = MarkupNode(NodeConstants.RemarksNodeType, e) :> IMarkupNode
                                // Found an operator with badly mangled name. Can't do anything with this.
                                scope.Log.LogWarning(LogEntry("Bad operator name", MarkupNode("#group", [| msg; remark |]), TokenHelpers.sourceLocation moduleName))
                            defined
                    else
                        // Import this as a function.
                        addMethod scope.Log item.Name item defined
                 else
                    // Don't import instance methods.
                    defined
            Seq.fold foldMethod defined moduleTy.Methods

    /// Reads the fixity specification encoded in the given parse tree.
    let readFixitySpecification (scope : GlobalScope) (spec : ParseTree<string, Token>) : Parser.OpFixity =
        match spec with
        | ProductionNode(Constant Parser.infixSpecifierIdentifier, 
                         [ProductionNode(Constant Parser.infixKeywordIdentifier, [TerminalLeaf infixKeyword])
                          TerminalLeaf lParen
                          TerminalLeaf precDecl
                          TerminalLeaf rParen]) ->
            // Parse the fixity declaration's precedence:
            //     
            //     infix?(?)
            //            ^
            let precInt = 
                match System.Int32.TryParse precDecl.contents with
                | (true, result) -> result
                | (false, _) -> 
                    scope.Log.LogError(LogEntry("Invalid precedence specification",
                                                sprintf "'%s' could not be parsed as a valid integer literal." precDecl.contents,
                                                TokenHelpers.sourceLocation precDecl))
                    9
            // Parse the fixity:
            //
            //    infix?(?)
            //    ^~~~~~
            match infixKeyword.tokenType with
            | TokenType.InfixlKeyword ->
                // infixl
                Parser.InfixLeft precInt
            | TokenType.InfixrKeyword ->
                // infixr
                Parser.InfixRight precInt
            | _ ->
                // Pick infixl if something goes wrong.
                scope.Log.LogError(LogEntry("Invalid fixity specification",
                                            sprintf "'%s' was not a valid fixity keyword." infixKeyword.contents,
                                            TokenHelpers.sourceLocation infixKeyword))
                Parser.InfixLeft precInt
        | _ ->
            // How does that even happen?
            scope.Log.LogError(LogEntry("Invalid fixity specification",
                                        "This node was not formatted as a valid fixity declaration.",
                                        TokenHelpers.treeSourceLocation spec))
            Parser.InfixLeft 9
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
            { functions = Map.empty; prec = Map.empty }

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
                // member to the module type.
                let parameterTokens = ParseTree.treeYield argsNode
                let srcLoc = TokenHelpers.sourceLocation letKeyword
                match analyzeLetDefinition defined scope name.contents name parameterTokens value srcLoc moduleType with
                | Success result ->
                    moduleType.AddMethod result
                    defined <- addMethod scope.Log result.Name result defined
                | Error msg ->
                    scope.Log.LogError msg
            | ProductionNode(Constant Parser.letDefinitionIdentifier,
                             [TerminalLeaf letKeyword
                              ProductionNode(Constant Parser.infixSpecifierIdentifier, _) as spec
                              TerminalLeaf leftArg
                              TerminalLeaf name
                              TerminalLeaf rightArg
                              TerminalLeaf eq
                              value]) ->
                // Binary operator let-binding. We'll do the 
                // same analyze-add dance as above.
                let parameterTokens = [leftArg; rightArg]
                let srcLoc = TokenHelpers.sourceLocation letKeyword
                let fixitySpec = readFixitySpecification scope spec
                let mangledName = NameHelpers.mangleOperatorName name.contents fixitySpec
                match analyzeLetDefinition defined scope mangledName name parameterTokens value srcLoc moduleType with
                | Success result ->
                    moduleType.AddMethod result
                    defined <- addMethod scope.Log name.contents result defined
                    // Update precedence map as well
                    let newPrec = Map.add name.contents fixitySpec defined.prec
                    defined <- { defined with prec = newPrec }
                | Error msg ->
                    scope.Log.LogError msg
            | ProductionNode(Constant Parser.openModuleIdentifier, 
                             [TerminalLeaf openKeyword
                              TerminalLeaf name]) ->
                // Try to import the module's contents. If something
                // goes wrong, then we'll just throw an error message
                // in the users' face.
                defined <- openModule scope name defined
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
            Some (ident.contents, Parser.flattenList Parser.definitionListIdentifier contents)
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