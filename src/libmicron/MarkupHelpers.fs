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
    let reference (prefix : string) (target : string) (suffix : string) : IMarkupNode =
        let ref = sprintf "'%s'" target
        match String.IsNullOrEmpty(prefix), String.IsNullOrEmpty(suffix) with
        | true, true -> ref |> text |> highlight
        | true, false -> group [text prefix; ref |> text |> highlight]
        | false, true -> group [ref |> text |> highlight; text suffix]
        | false, false -> group [text prefix; ref |> text |> highlight; text suffix]

    /// Creates a node that references something, which is quoted and highlighted.
    /// A prefix may be provided.
    let referencePrefix (prefix : string) (target : string) : IMarkupNode =
        reference prefix target ""

    /// Creates a node that references something, which is quoted and highlighted.
    /// A suffix may be provided.
    let referenceSuffix (target : string) (suffix : string) : IMarkupNode =
        reference "" target suffix