namespace muc

open Flame
open Flame.Compiler
open Flame.Compiler.Projects
open Flame.Functional
open Flame.Front
open Flame.Front.Cli
open Flame.Front.Options
open Flame.Front.Projects
open Flame.Front.Target
open Flame.Verification
open Flame.Optimization
open System
open System.Threading.Tasks
open libcontextfree
open libmicron

type MicronProjectHandler() =
    let parser = Parser.createParser Parser.programGrammar

    /// Tries to parse a micron file. If this cannot be done, None is returned.
    /// This function is chatty.
    let tryParseFile (file : IProjectSourceItem) (parameters : CompilationParameters) =
        match ProjectHandlerHelpers.GetSourceSafe(file, parameters) with
        | null -> 
            None
        | doc -> 
            let tokens = Lexer.lex doc |> TokenHelpers.foldTrivia
            match parser tokens with
            | Choice1Of2 tree -> Some (Parser.stripGroups tree)
            | Choice2Of2(token :: _) ->
                parameters.Log.LogError(LogEntry("Syntax error",  "Unexpected token type.", token.sourceLocation))
                None
            | Choice2Of2 [] -> 
                let charCount = doc.Source.Length
                parameters.Log.LogError(LogEntry("Syntax error",  "Unexpected end-of-file marker.", SourceLocation(doc, charCount - 1, charCount)))
                None

    interface IProjectHandler with
        member this.Extensions = seq [ "mu" ]
        member this.Partition(projects) = projects
        member this.Parse(path : ProjectPath, log : ICompilerLog) : IProject = 
            SingleFileProject(path, log.Options.GetTargetPlatform()) :> IProject

        member this.MakeProject (project : IProject, path : ProjectPath, log : ICompilerLog) : IProject =
            log.LogWarning(LogEntry("Ignored '-make-project'", 
                                    "The '-make-project' option was ignored " + 
                                    "because micron files are self-contained assemblies: " + 
                                    "they have no use for a project."));
            project

        member this.GetPassPreferences(log : ICompilerLog) : PassPreferences = 
            PassPreferences(
                seq [ 
                    // Run -fslim-lambda at -O1 or above
                    PassCondition(SlimLambdaPass.SlimLambdaPassName, fun optInfo -> optInfo.OptimizeMinimal)
                    // Run -fflatten-init at -O1 or above
                    PassCondition(FlattenInitializationPass.FlattenInitializationPassName, fun optInfo -> optInfo.OptimizeMinimal)
                    // -ftail-recursion is enabled by default at -O2 or above,
                    // but we'll enable it at -O1 or above, because this optimization
                    // is essential for functional programming languages.
                    PassCondition(TailRecursionPass.TailRecursionPassName, fun optInfo -> optInfo.OptimizeMinimal)
                    // Run -fdead-code-elimination at -O1 or above, or if -g is enabled.
                    PassCondition(PassExtensions.EliminateDeadCodePassName, fun optInfo -> optInfo.OptimizeMinimal || optInfo.OptimizeDebug)
                    // Run -finfinite-recursion if it is useful (i.e. the warning it is associated with is active) 
                    PassCondition(InfiniteRecursionPass.InfiniteRecursionPassName, fun optInfo -> InfiniteRecursionPass.IsUseful(optInfo.Log))
                    ],
                seq [
                    // A pass that removes dead code, and logs a warning
                    // when it finds unreachable source code.
                    PassInfo<IStatement * IMethod * ICompilerLog, IStatement>(
                        VerifyingDeadCodePass.Instance,
                        PassExtensions.EliminateDeadCodePassName)

                    // A pass that tries to find infinite recursion, and
                    // logs a warning when it does.
                    PassInfo<IStatement * IMethod * ICompilerLog, IStatement>(
                        InfiniteRecursionPass.Instance,
                        InfiniteRecursionPass.InfiniteRecursionPassName)
                ])

        member this.CompileAsync(project : IProject, parameters : CompilationParameters) : Task<IAssembly> = 
            async {
                // We know the project is a single-file project,
                // because we made it so in `this.Parse`
                let proj = project :?> SingleFileProject

                // A single file project has exactly one source item.
                let file = proj.GetSourceItems() |> Seq.exactlyOne
                // Parse that file.
                let parsed = tryParseFile file parameters
                // Then create a binder.
                let! binder = Async.AwaitTask parameters.BinderTask
                match parsed with
                | None ->
                    // Parsing went wrong.
                    // Creating an empty assembly is 
                    // just about the best we can do here.
                    return DescribedAssembly(project.Name, binder.Environment) :> IAssembly
                | Some tree ->
                    // This is a horribly inefficient implementation of a 
                    // member provider. This is okay right now, because
                    // we don't use the member provider at this time.
                    // This may change in the future, though.
                    let memProvider (ty : IType) = ty.GetAllMembers()
                    // Create a global scope
                    let scope = GlobalScope(FunctionalBinder(binder), StrictConversionRules(NameHelpers.nameType),
                                            parameters.Log, NameHelpers.nameType, memProvider, Analysis.getParameters)
                    // Analyze the assembly
                    return Analysis.analyzeAssembly scope project.Name tree
            } |> Async.StartAsTask