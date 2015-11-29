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
            | Choice1Of2 tree -> Some tree
            | Choice2Of2(token :: _) ->
                parameters.Log.LogError(LogEntry("Syntax error",  "Unexpected token type.", token.sourceLocation))
                None
            | Choice2Of2 [] -> 
                let charCount = doc.Source.Length
                parameters.Log.LogError(LogEntry("Syntax error",  "Unexpected end-of-file.", SourceLocation(doc, charCount - 1, charCount)))
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
            PassPreferences(seq [],
                seq [
                    PassInfo<IStatement * IMethod * ICompilerLog, IStatement>(
                        VerifyingDeadCodePass.Instance,
                        PassExtensions.EliminateDeadCodePassName, 
                        fun optInfo isPref -> optInfo.OptimizeMinimal || optInfo.OptimizeDebug)

                    PassInfo<IStatement * IMethod * ICompilerLog, IStatement>(
                        InfiniteRecursionPass.Instance,
                        InfiniteRecursionPass.InfiniteRecursionPassName,
                        fun optInfo isPref -> InfiniteRecursionPass.IsUseful(log))
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
                    // member provider.
                    let memProvider (ty : IType) = ty.GetAllMembers()
                    // Create a global scope
                    let scope = GlobalScope(FunctionalBinder(binder), StrictConversionRules(Analysis.nameType),
                                            parameters.Log, Analysis.nameType, memProvider, Analysis.getParameters)
                    // Analyze the assembly
                    return Analysis.analyzeAssembly scope project.Name tree
            } |> Async.StartAsTask