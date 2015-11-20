namespace FsUnit.Test
open Microsoft.VisualStudio.TestTools.UnitTesting
open FsUnit.MsTest
open NHamcrest.Core
open libmicron
open libcontextfree
open Flame
open Flame.Compiler
open Flame.Compiler.Expressions
open Flame.Compiler.Statements
open Flame.Compiler.Variables
open Flame.Functional

[<TestClass>]
type ParserTests () =

    [<TestMethod>] 
    member this.CompileExpressionGrammar () =
        Parser.createParser Parser.expressionGrammar |> ignore

    [<TestMethod>] 
    member this.CompileTotalGrammar () =
        Parser.createParser Parser.programGrammar |> ignore