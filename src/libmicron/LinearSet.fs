namespace libmicron

open System
open System.Collections.Generic

/// A set type that uses generic equality and hash operations.
type LinearSet<'a when 'a : equality> private(items : 'a list) =
    /// Gets a list of all items in this linear set.
    member this.Items = items

    /// Checks if this list contains the given item.
    member this.Contains (item : 'a) =
        items |> List.exists ((=) item)

    /// Adds the given element to the set.
    member this.Add (item : 'a) =
        if this.Contains item then
            this
        else
            LinearSet<'a>(item :: items)

    /// Removes the given element from the set.
    member this.Remove (item : 'a) =
        if this.Contains item then
            LinearSet<'a>(items |> List.filter ((<>) item))
        else
            this

    /// Checks if this list is a subset of the given list.
    member this.IsSubset (superset : LinearSet<'a>) =
        items |> List.fold (fun result item -> result && superset.Contains item) true

    /// Creates a linear set from the given sequence of elements.
    static member ofSeq (items : seq<'a>) =
        LinearSet<'a>(items |> Seq.distinct |> List.ofSeq)

    interface IEnumerable<'a> with
        member this.GetEnumerator () : IEnumerator<'a> = 
            (Seq.ofList items).GetEnumerator()

        member this.GetEnumerator () : System.Collections.IEnumerator = 
            (this :> IEnumerable<'a>).GetEnumerator() :> System.Collections.IEnumerator

    interface IEquatable<LinearSet<'a>> with
        member this.Equals (other : LinearSet<'a>) =
            this.IsSubset other && other.IsSubset this

    override this.Equals (other : obj) =
        match other with
        | :? LinearSet<'a> as other -> this.IsSubset other && other.IsSubset this
        | _ -> false

    override this.GetHashCode () =
        items |> List.map hash 
              |> List.fold (^^^) 0

/// Helpers for the linear set type.
module LinearSet =
    /// Creates a linear set from the given list of elements.
    let ofList items = LinearSet.ofSeq items

    /// Gets a list of all elements in the given
    /// linear set.
    let toList (set : LinearSet<'a>) = set.Items

    /// The empty set.
    let empty<'a when 'a : equality> : LinearSet<'a> = ofList []

    /// Creates a singleton set from the given item.
    let singleton item = ofList [item]

    /// Checks if a set contains the given item.
    let contains (item : 'a) (set : LinearSet<'a>) : bool = 
        set.Contains item

    /// Adds the given item to a linear set.
    let add (item : 'a) (set : LinearSet<'a>) : LinearSet<'a> = 
        set.Add item

    /// Removes the given item from a linear set.
    let remove (item : 'a) (set : LinearSet<'a>) : LinearSet<'a> = 
        set.Remove item

    /// Folds the given linear set with the given folding function
    /// and initial state.
    let fold (folder : 'b -> 'a -> 'b) (init : 'b) (set : LinearSet<'a>) : 'b =
        List.fold folder init set.Items

    /// Builds a new linear set whose elements are the results of applying
    /// the given function to each of the elements of the collection.
    let map (mapping : 'a -> 'b) (set : LinearSet<'a>) : LinearSet<'b> =
        ofList (List.map mapping set.Items)

    /// Creates a new set containing only the elements in the given set
    /// for which the predicate returns "true".
    let filter (predicate : 'a -> bool) (set : LinearSet<'a>) : LinearSet<'a> =
        ofList (List.filter predicate set.Items)

    /// Computes the union of two sets.
    let union (first : LinearSet<'a>) (second : LinearSet<'a>) : LinearSet<'a> =
        List.foldBack add second.Items first

    /// Removes all items in the second set from the first set.
    let except (first : LinearSet<'a>) (second : LinearSet<'a>) : LinearSet<'a> =
        List.foldBack remove second.Items first