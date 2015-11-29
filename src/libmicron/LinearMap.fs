namespace libmicron

open System
open System.Collections.Generic

/// A map type that uses generic equality and hash operations.
type LinearMap<'a, 'b when 'a : equality> private(items : ('a * 'b) list) =
    /// Gets this linear map's set of keys.
    member this.Keys = items |> List.map fst
                             |> LinearSet.ofList

    /// Gets this linear map's list of values, 
    /// which may contain duplicates.
    member this.Values = items |> List.map snd

    /// Gets this linear map's list of key-value
    /// pairs.
    member this.Items = items

    /// Removes the given key from the linear map.
    member this.RemoveKey key =
        LinearMap(List.filter (fst >> (<>) key) items)

    /// Checks if this linear map contains the given key.
    member this.ContainsKey key =
        List.exists (fst >> (=) key) items

    /// Adds the given key-value pair to the linear map.
    /// If the given key was already in the map, then
    /// it is replaced by this pair.
    member this.Add key value =
        LinearMap((key, value) :: (this.RemoveKey key).Items)

    /// Tries to find the value belonging to the given key.
    member this.TryFind key =
        List.tryFind (fst >> (=) key) items |> Option.map snd

    /// Gets the value beloning to the given key.
    member this.Find key =
        this.TryFind key |> Option.get

    interface IEnumerable<'a * 'b> with
        member this.GetEnumerator () : IEnumerator<'a * 'b> = 
            (Seq.ofList items).GetEnumerator()

        member this.GetEnumerator () : System.Collections.IEnumerator = 
            (this :> IEnumerable<'a * 'b>).GetEnumerator() :> System.Collections.IEnumerator

    /// The empty linear map.
    static member empty =
        LinearMap<'a, 'b>([])

/// Helpers for the linear map type.
module LinearMap =    
    /// Checks if a map contains the given item.
    let containsKey (key : 'a) (table : LinearMap<'a, 'b>) : bool = 
        table.ContainsKey key

    /// Adds the given key-value pair to a linear map.
    let add (key : 'a) (value : 'b) (table : LinearMap<'a, 'b> : LinearMap<'a, 'b>) = 
        table.Add key value

    /// Removes the given key from a linear map.
    let removeKey (key : 'a) (table : LinearMap<'a, 'b>) : LinearMap<'a, 'b> = 
        table.RemoveKey key

    /// Tries to find the value beloning to the given key
    /// in a linear map.
    let tryFind (key : 'a) (table : LinearMap<'a, 'b>) : 'b option =
        table.TryFind key

    /// Finds the value belonging to the given key
    /// in a linear map.
    let find (key : 'a) (table : LinearMap<'a, 'b>) : 'b =
        table.Find key

    /// Creates a linear map from the given sequence of
    /// key-value pairs.
    let ofSeq (items : seq<'a * 'b>) : LinearMap<'a, 'b> =
        Seq.fold (fun table (k, v) -> add k v table) LinearMap.empty items

    /// Creates a linear map from the given list of
    /// key-value pairs.
    let ofList (items : ('a * 'b) list) : LinearMap<'a, 'b> =
        ofSeq items

    /// Convers this linear map to a list of key-value pairs.
    let toList (table : LinearMap<'a, 'b>) : ('a * 'b) list =
        table.Items

    /// Converts the given linear map to a sequence of key-value pairs.
    let toSeq (table : LinearMap<'a, 'b>) : seq<'a * 'b> =
        List.toSeq (toList table)

    /// Converts this linear map to a linear set of key-value pairs.
    let toSet (table : LinearMap<'a, 'b>) : LinearSet<'a * 'b> =
        LinearSet.ofList (toList table)

    /// Folds a linear map with the given folding function and initial state.
    let fold (folder : 'c -> 'a -> 'b -> 'c) (init : 'c) (table : LinearMap<'a, 'b>) : 'c =
        List.fold (fun state (k, v) -> folder state k v) init table.Items

    /// Applies the given mapping function to all elements in the table.
    let map (mapping : 'a -> 'b -> 'c) (table : LinearMap<'a, 'b>) : LinearMap<'a, 'c> =
        List.map (fun (k, v) -> k, mapping k v) table.Items |> ofSeq

    /// Adds all items in the second linear map to the first linear map.
    let addAll (left : LinearMap<'a, 'b>) (right : LinearMap<'a, 'b>) : LinearMap<'a, 'b> =
        right |> fold (fun result key value -> add key value result) left