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
    let containsKey (key : 'a) (table : LinearMap<'a, 'b>) = 
        table.ContainsKey key

    /// Adds the given key-value pair to a linear map.
    let add (key : 'a) (value : 'b) (table : LinearMap<'a, 'b>) = 
        table.Add key value

    /// Removes the given key from a linear map.
    let removeKey (key : 'a) (table : LinearMap<'a, 'b>) = 
        table.RemoveKey key