namespace libmicron

open System
open Pixie

module MarkupHelpers =
    /// Creates a highlighted node from the given markup node.
    let highlight (target : IMarkupNode) : IMarkupNode = 
        MarkupNode(NodeConstants.BrightNodeType, [| target |]) :> IMarkupNode

    /// Creates a text node from the given contents.
    let text (contents : string) : IMarkupNode =
        MarkupNode(NodeConstants.TextNodeType, contents) :> IMarkupNode

    /// Creates a group node from the given sequence of markup nodes.
    let group (nodes : seq<IMarkupNode>) : IMarkupNode =
        MarkupNode("#group", nodes) :> IMarkupNode

    /// Creates a node that references something, which is quoted and highlighted.
    /// A suffix and prefix may be provided.
    let refer (prefix : string) (target : string) (suffix : string) : IMarkupNode =
        group [text (prefix + "'"); target |> text |> highlight; text ("'" + suffix)]

    /// Creates a node that references something, which is quoted and highlighted.
    /// A prefix may be provided.
    let referPrefix (prefix : string) (target : string) : IMarkupNode =
        refer prefix target ""

    /// Creates a node that references something, which is quoted and highlighted.
    /// A suffix may be provided.
    let referSuffix (target : string) (suffix : string) : IMarkupNode =
        refer "" target suffix

    /// Creates a node that references two things.
    let refer2 (prefix : string) (target1 : string) (infix : string) (target2 : string) (suffix : string) : IMarkupNode =
        group [text (prefix + "'"); 
               target1 |> text |> highlight; 
               text ("'" + infix + "'"); 
               target2 |> text |> highlight; 
               text ("'" + suffix)]
