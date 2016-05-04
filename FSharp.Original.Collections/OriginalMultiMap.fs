﻿namespace Original.Collections
open System.Collections.Generic

type OriginalMultiMap<'a, 'b  when 'a : comparison and 'b : comparison>(x : OriginalMap<'a, OriginalSet<'b>>) =
    member this.OriginalMap() = x
    
module OriginalMultiMap =
    let add (key : 'a) (value : 'b) (table : OriginalMultiMap<'a, 'b>) =
        let m = table.OriginalMap()
        let v =
            match OriginalMap.tryFind key m with
            | Some(m) -> OriginalSet.add value m
            | None -> OriginalSet.singleton value
        OriginalMultiMap(OriginalMap.add key v m)

    let empty<'a, 'b  when 'a : comparison and 'b : comparison> =
        let m : OriginalMap<'a, OriginalSet<'b>> = OriginalMap(Seq.empty, Map.empty)
        OriginalMultiMap<'a, 'b>(m)

    let iter (action : 'a -> 'b -> unit) (table : OriginalMultiMap<'a, 'b>) =
        let m = table.OriginalMap()
        OriginalMap.iter (fun k v ->
            OriginalSet.iter (fun i -> action k i) v) m

    let (+) (set1 : OriginalMultiMap<'a, 'b>) (set2 : OriginalMultiMap<'a, 'b>) =
        let mutable set = set1
        iter (fun k v ->
            set <- add k v set) set2
        set

    let containsKey (key : 'a) (table : OriginalMultiMap<'a, 'b>) =
        let map  = table.OriginalMap()
        OriginalMap.containsKey key map

    let exists (predicate : 'a -> 'b -> bool) (table : OriginalMultiMap<'a, 'b>) =
        let map  = table.OriginalMap()
        OriginalMap.exists (fun k v -> OriginalSet.exists (predicate k) v) map

    let filter (predicate : 'a -> 'b -> bool) (table : OriginalMultiMap<'a, 'b>) =
//        let map = table.OriginalMap()
        let mutable set : OriginalMultiMap<'a, 'b> = empty
        iter (fun k v ->
            if predicate k v then
                set <- add k v set
                ) table
        set

    let find (key : 'a) (table : OriginalMultiMap<'a, 'b>) =
        let map  = table.OriginalMap()
        if containsKey key table then
            raise (KeyNotFoundException())
        else
            OriginalMap.find key map
            
    let findkey (predicate : 'a -> 'b -> bool) (table : OriginalMultiMap<'a, 'b>) =
        let map  = table.OriginalMap()
        OriginalMap.findKey (fun k v -> OriginalSet.exists (predicate k) v) map

    let forall (predicate : 'a -> 'b -> bool) (table : OriginalMultiMap<'a, 'b>) =
        let map  = table.OriginalMap()
        OriginalMap.forall (fun k v -> OriginalSet.forall (predicate k) v) map

    let isEmpty (table : OriginalMultiMap<'a, 'b>) =
        let map  = table.OriginalMap()
        OriginalMap.isEmpty map
        
    let map (mapping : 'a -> 'b -> 'c) (table : OriginalMultiMap<'a, 'b>) =
        let mutable map : OriginalMultiMap<'a, 'c> = empty
        iter (fun k v ->
            map <- add k (mapping k v) map) table
        map

    let tryFind (key : 'a) (table : OriginalMultiMap<'a, 'b>) =
        let map = table.OriginalMap()
        OriginalMap.tryFind key map

    let ofSeq (elements : ('a * 'b) seq) =
        let mutable map : OriginalMultiMap<'a, 'b> = empty
        for (k, v) in elements do
            map <- add k v map
        map

    let ofArray (elements : ('a * 'b) []) =
        elements |> Array.toSeq |> ofSeq

    let ofList (elements : ('a * 'b) list) =
        elements |> List.toSeq |> ofSeq

    let partition (predicate : 'a -> 'b -> bool) (table : OriginalMultiMap<'a, 'b>) =
        let map = table.OriginalMap()
        let map1 = filter predicate table
        let map2 = filter (fun k v -> not (predicate k v)) table
        map1, map2

    let remove (key : 'a) (table : OriginalMultiMap<'a, 'b>) =
        match tryFind key table with
        | Some(v) ->
            let map = table.OriginalMap()
            OriginalMultiMap(OriginalMap.remove key map)
        | None -> table

    let toSeq (table : OriginalMultiMap<'a, 'b>) =
        let m = table.OriginalMap()
        let s = m |> OriginalMap.toSeq
        s |> Seq.map (fun x -> m |> OriginalMap.find x
                                 |> OriginalSet.toSeq
                                 |> Seq.map (fun y -> (x, y)))
          |> Seq.reduce Seq.append

    let pick (chooser : 'a -> 'b -> 'c option) (table : OriginalMultiMap<'a, 'b>) =
        let seq = toSeq table
        Seq.pick (fun (k, v) -> chooser k v) seq

    let fold (folder : 's -> 'a -> 'b -> 's) (state : 's) (table : OriginalMultiMap<'a, 'b>) =
        let seq = toSeq table
        Seq.fold (fun s (xk, xv) -> folder s xk xv) state seq
    
    let foldBack (folder : 'a -> 'b -> 's -> 's) (table : OriginalMultiMap<'a, 'b>) (state : 's) =
        let seq = toSeq table
        Seq.foldBack (fun (k, v) s -> folder k v s) seq state

    let toArray (table : OriginalMultiMap<'a, 'b>) =
        table |> toSeq |> Array.ofSeq
        
    let toList (table : OriginalMultiMap<'a, 'b>) =
        table |> toSeq |> List.ofSeq

    let tryFindKey (predicate : 'a -> 'b -> bool) (table : OriginalMultiMap<'a, 'b>) =
        match exists predicate table with
        | true -> Some(findkey predicate table)
        | false -> None

    let tryPick (chooser : 'a -> 'b -> 'c option) (table : OriginalMultiMap<'a, 'b>) =
        let seq = toSeq table
        Seq.pick (fun (k, v) -> chooser k v) seq
