namespace libmicron

open Flame
open Flame.Compiler
open Flame.Compiler.Visitors

/// A pass that logs errors and warning nodes, and strips
/// them from the node tree afterward.
type LogPass() =
    static member Apply (body : IStatement) (log : ICompilerLog) : IStatement =
        let logVisitor = LoggingVisitor(log, true, false)
        logVisitor.Visit(body)

    interface IPass<IStatement * IMethod * ICompilerLog, IStatement> with
        member this.Apply(tuple : IStatement * IMethod * ICompilerLog) : IStatement =
            let body, _, log = tuple
            LogPass.Apply body log
