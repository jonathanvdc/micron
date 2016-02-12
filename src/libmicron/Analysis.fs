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
                unknownError (LogEntry("Unresolved " + nameType, MarkupHelpers.referSuffix name " could not be resolved."))

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
                                           MarkupHelpers.referSuffix token.contents " could not be parsed as a valid integer literal." ))
                                 (EB.ConstantInt32 0)
        ) |> EB.Source (TokenHelpers.sourceLocation token)
    | ProductionNode(Constant Parser.literalDoubleIdentifier,
                     [TerminalLeaf token]) ->
        // Double literal
        (match System.Double.TryParse token.contents with
        | (true, d) -> EB.ConstantFloat64 d
        | (false, _) -> EB.Error (LogEntry("Invalid double literal",
                                           MarkupHelpers.referSuffix token.contents " could not be parsed as a valid double literal."))
                                 (EB.ConstantFloat64 0.0)
        ) |> EB.Source (TokenHelpers.sourceLocation token)
    | ProductionNode(Constant Parser.literalStringIdentifier,
                     [TerminalLeaf token]) ->
        // String literal
        // TODO: implement escape sequence analysis

        // Get rid of the quotes (first and last characters)
        let stringVal = token.contents.Substring(1, token.contents.Length - 2)
        EB.ConstantString stringVal |> EB.Source (TokenHelpers.sourceLocation token)
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
    | ProductionNode(Constant Parser.parenOperatorIdentifier, [_; TerminalLeaf op; _]) as node ->
        resolveOperator previousDefinitions scope op
         |> EB.Source (TokenHelpers.treeSourceLocation node)
    | ProductionNode(nonterm, _) as node ->
        // Unimplemented node type.
        // This just means that a construct has been defined in the grammar,
        // and that the semantic analysis pass does not support it yet.
        EB.VoidError (LogEntry("Unimplemented node type", MarkupHelpers.referSuffix nonterm " nodes have not been implemented yet."))
            |> EB.Source (TokenHelpers.treeSourceLocation node)
    | TerminalLeaf(term) ->
        // Unexpected terminal leaf.
        // This points to an error in the grammar.
        EB.VoidError (LogEntry("Unexpected raw token", MarkupHelpers.refer "Token " term.contents " was completely unexpected here."))
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

            let message = MarkupHelpers.referSuffix name " is defined more than once in the same module. "
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

            let message = MarkupHelpers.refer "This definition of " name " shadows a previous one. "
            let cause = Warnings.Instance.Shadow.CauseNode
            let diagnostics = CompilerLogExtensions.CreateDiagnosticsNode(func.GetSourceLocation())
            let remark = CompilerLogExtensions.CreateRemarkDiagnosticsNode(predef.GetSourceLocation(), "Previous definition: ")
            log.LogWarning(LogEntry("Shadowed definition", seq [message; cause; diagnostics; remark]))
        | _ ->
            ()
        let newFunctions = Map.add name func defined.functions

        if log.Options.GetOption<bool>("print-types", false) then
            let ty = NameHelpers.nameType (MethodType.Create(func))
            log.LogMessage(LogEntry(name, sprintf "%s" ty))

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
                                        MarkupHelpers.refer "Could not resolve module " moduleName.contents ". Did you forget to link it or list it as a library dependency?",
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
                                scope.Log.LogWarning(LogEntry("Bad operator name", MarkupHelpers.group [| msg; remark |], TokenHelpers.sourceLocation moduleName))
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
                                                MarkupHelpers.referSuffix precDecl.contents " could not be parsed as a valid integer literal.",
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
                                            MarkupHelpers.referSuffix infixKeyword.contents " was not a valid fixity keyword.",
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
                    // Log errors/warnings in the method body first.
                    result.Body <- LogPass.Apply result.Body scope.Log
                    // Then proceed to add the method to the module.
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
                    // Log errors/warnings in the method body first.
                    result.Body <- LogPass.Apply result.Body scope.Log
                    // Then proceed to add the method to the module.
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
                scope.Log.LogError(LogEntry("Unimplemented node type", MarkupHelpers.referSuffix nonterm " nodes have not been implemented yet.", TokenHelpers.treeSourceLocation node))
            | TerminalLeaf(term) ->
                // Unexpected terminal leaf.
                // This points to an error in the grammar.
                scope.Log.LogError(LogEntry("Unexpected raw token", MarkupHelpers.refer "Token " term.contents " was completely unexpected here.", term.sourceLocation))

        moduleType :> IType

    let topLevelModuleName = "__program"
    let entryPointFunctionName = "__entry_point"
    let IOMonadActionField = "Action"

    /// Tries to create an entry point.
    let tryCreateEntryPoint (scope : GlobalScope) (programModule : IType) : IMethod option =
        // We're looking for a parameterless static method called '__entry_point'.
        match programModule.GetMethod(true, [||]) with
        | null -> None
        | mainFunc ->
            // Construct the following function:
            //
            // public static void __entry_point(string[] Args)
            // {
            //     main().Action();
            //     return;
            // }

            // Construct the `public static void __entry_point(string[] Args)`
            // signature.
            let epFunc = DescribedBodyMethod(entryPointFunctionName, programModule, PrimitiveTypes.Void, true)

            // Construct the `main().Action()` call. Instantiate the main function
            // with all int32s if it is generic.
            let mainCall = InvocationExpression(mainFunc.MakeGenericMethod(mainFunc.GenericParameters |> Seq.map (fun _ -> PrimitiveTypes.Int32)), null, Seq.empty)
            let localScope = LocalScope(FunctionScope(scope, epFunc))
            let accessedField = EB.AccessNamedMembers localScope IOMonadActionField (EB.GetAccessedExpression mainCall)
            if EB.IsError accessedField then
                // main function was bad. Log an error message.
                scope.Log.LogError(LogEntry("Invalid 'main' value",
                                            MarkupHelpers.refer "'main' should have been an IO monad. Instead, its type was " (NameHelpers.nameType mainCall.Type) ".",
                                            mainFunc.GetSourceLocation()))
                None
            else
                // Make the call.
                let actionCall = EB.Invoke localScope accessedField Seq.empty |> EB.Pop
                // Then return void.
                epFunc.Body <- EB.Initialize actionCall EB.ReturnVoid |> EB.ToStatement
                Some (epFunc :> IMethod)

    /// Analyzes an entire program.
    let analyzeProgram (scope : GlobalScope) (contents : ParseTree<string, Token>) (declAsm : IAssembly) : INamespaceBranch * IMethod option =
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
            // then we're done here. Also, there is no entry point.
            result :> INamespaceBranch, None
        else
            // Otherwise, create a `__program` module and
            // put them in there.
            let resultNs = result.WithType(analyzeModule scope topLevelModuleName topLevelDefs)
            let program = resultNs.GetAllTypes() |> Seq.find (fun (x : IType) -> x.Name = topLevelModuleName)
            resultNs :> INamespaceBranch, tryCreateEntryPoint scope program

    /// Analyzes an assembly. This does the same thing as analyzing a program,
    /// except that the result is wrapped in an assembly of the given name.
    let analyzeAssembly (scope : GlobalScope) (name : string) (contents : ParseTree<string, Token>) : IAssembly =
        let asm = DescribedAssembly(name, scope.Environment)
        let mainNs, ep = analyzeProgram scope contents asm
        asm.MainNamespace <- mainNs
        match ep with
        | Some ep -> asm.EntryPoint <- ep
        | None -> ()
        asm :> IAssembly
