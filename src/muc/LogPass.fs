namespace muc

open Flame
open Flame.Compiler
open Flame.Compiler.Visitors

type LogPass() =
    interface IPass<IStatement * IMethod * ICompilerLog, IStatement> with
        member this.Apply(tuple : IStatement * IMethod * ICompilerLog) : IStatement =
            let body, _, log = tuple
            let logVisitor = new LoggingVisitor(log, true, false)
            logVisitor.Visit(body)
