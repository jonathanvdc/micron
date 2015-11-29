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
type TypeInferenceTests () =
    let assertError : Result<'a, 'b> -> unit = function
    | Error _ -> ()
    | Success _ -> true |> should equal false

    let assertEqual (left : Result<'a, 'b>) (right : Result<'a, 'b>) : unit =   
        left = right |> should equal true

    [<TestMethod>] 
    member this.ValidInference () =
        let t1 = UnknownType() // a
        let t2 = UnknownType() // b
        let t3 = UnknownType() // c
        let t4 = UnknownType() // d
        let t5 = UnknownType() // e
        let constraints = 
            [
                // a = a
                TypeInference.Variable t1, TypeInference.Variable t1, null
                // a = bool -> b
                TypeInference.Variable t1, TypeInference.Function(TypeInference.Constant PrimitiveTypes.Boolean, TypeInference.Variable t2), null
                // int = b
                TypeInference.Constant PrimitiveTypes.Int32, TypeInference.Variable t2, null
                // c = d<a, b>
                TypeInference.Variable t3, TypeInference.Instance(TypeInference.Variable t4, [TypeInference.Variable t1; TypeInference.Variable t2]), null
                // e = c
                TypeInference.Variable t5, TypeInference.Variable t3, null
            ]

        let expected = 
            LinearSet.ofList
                [
                    // a = bool -> int
                    t1, TypeInference.Function(TypeInference.Constant PrimitiveTypes.Boolean, TypeInference.Constant PrimitiveTypes.Int32)
                    // b = int
                    t2, TypeInference.Constant PrimitiveTypes.Int32
                    // c = d<bool -> int, int>
                    t3, TypeInference.Instance(TypeInference.Variable t4, 
                        [
                            TypeInference.Function(TypeInference.Constant PrimitiveTypes.Boolean, TypeInference.Constant PrimitiveTypes.Int32)
                            TypeInference.Constant PrimitiveTypes.Int32
                        ])
                    // e = d<bool -> int, int>
                    t5, TypeInference.Instance(TypeInference.Variable t4, 
                        [
                            TypeInference.Function(TypeInference.Constant PrimitiveTypes.Boolean, TypeInference.Constant PrimitiveTypes.Int32)
                            TypeInference.Constant PrimitiveTypes.Int32
                        ])
                ]

        let results = TypeInference.resolve constraints
        assertEqual (results |> Result.map LinearMap.toSet) (Success expected)

    [<TestMethod>] 
    member this.InfiniteType () =
        let t1 = UnknownType() // a
        let t2 = UnknownType() // b

        let constraints = 
            [
                // b = a<b>
                TypeInference.Variable t2, TypeInference.Instance(TypeInference.Variable t1, [TypeInference.Variable t2]), null
            ]
            
        assertError (TypeInference.resolve constraints)

    [<TestMethod>] 
    member this.DefinedTwice () =
        let t1 = UnknownType() // a
        
        let constraints =
            [
                // a = bool
                TypeInference.Variable t1, TypeInference.Constant (PrimitiveTypes.Boolean), null
                // a = int
                TypeInference.Variable t1, TypeInference.Constant (PrimitiveTypes.Int32), null
            ]

        assertError (TypeInference.resolve constraints)

    [<TestMethod>]
    member this.SelfAssignment () =
        // Verifies that an expression
        //
        // let x = x
        //
        // is well-typed

        let idunnoty = UnknownType()
        let local = LateBoundVariable(idunnoty)
        let stmt = local.CreateSetStatement(local.CreateGetExpression())
        let expr = InitializedExpression(stmt, VoidExpression.Instance)
        let constraints = TypeInference.findConstraints expr
        let results = TypeInference.resolve constraints
        let expected = LinearSet.empty<UnknownType * TypeInference.TypeConstraint>

        assertEqual (results |> Result.map LinearMap.toSet) (Success expected)

    [<TestMethod>]
    member this.AssignTypeNames() =
        // Verify that type names are at least assigned as follows:
        //
        //     a, b, c, ... z, aa.
        //
        let show = TypeInference.createShow()
        
        Array.init 27 (fun _ -> show(TypeInference.Variable <| UnknownType()))
        |> should equal [| "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i";
                           "j"; "k"; "l"; "m"; "n"; "o"; "p"; "q"; "r";
                           "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z"; "aa" |]

    [<TestMethod>]
    member this.SubstLambda() =
        let unknownTy1 = UnknownType()
        let unknownTy2 = UnknownType()
        let signature = Flame.Build.DescribedMethod("", null, unknownTy2, true)
        signature.AddParameter(Flame.Build.DescribedParameter("x", unknownTy1))
        let header = LambdaHeader(signature, [||])
        // Creates an 'a -> 'b lambda
        let lambdaExpr = LambdaExpression(header, ReturnStatement(Int32Expression(0)))
        // Maps everything to int
        let mapping ty = PrimitiveTypes.Int32
        // Check that the result is an int -> int lambda
        let result = TypeInference.resolveExpression mapping lambdaExpr
        MethodType.GetMethod(result.Type).Parameters |> Seq.exactlyOne 
                                                     |> (fun parameter -> parameter.ParameterType)
                                                     |> should equal PrimitiveTypes.Int32
        MethodType.GetMethod(result.Type).ReturnType |> should equal PrimitiveTypes.Int32