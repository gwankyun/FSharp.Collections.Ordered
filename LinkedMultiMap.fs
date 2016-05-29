namespace FSharp.Collections

open System.Collections.Generic
open Extension
open FSharp.Collections

module LinkedMultiMap = 
    let add (key : 'a) (value : 'b) (table : LinkedMultiMap<'a, 'b>) = 
        let m = table.LinkedMap()
        
        let v = 
            match LinkedMap.tryFind key m with
            | Some(v) -> 
                m
                |> LinkedMap.remove key
                |> LinkedMap.add key (LinkedSet.add value v)
            | None -> LinkedMap.add key (LinkedSet.singleton value) m
        LinkedMultiMap(v)
    
    let empty<'a, 'b when 'a : comparison and 'b : comparison> = 
        let m : LinkedMap<'a, LinkedSet<'b>> = LinkedMap(LazyList.empty, Map.empty)
        LinkedMultiMap<'a, 'b>(m)
    
    let iter (action : 'a -> 'b -> unit) (table : LinkedMultiMap<'a, 'b>) = 
        let m = table.LinkedMap()
        LinkedMap.iter (fun k v -> LinkedSet.iter (fun i -> action k i) v) m
    
    let (+) (set1 : LinkedMultiMap<'a, 'b>) (set2 : LinkedMultiMap<'a, 'b>) = 
        let mutable set = set1
        iter (fun k v -> set <- add k v set) set2
        set
    
    let containsKey (key : 'a) (table : LinkedMultiMap<'a, 'b>) = 
        let map = table.LinkedMap()
        LinkedMap.containsKey key map
    
    let exists (predicate : 'a -> 'b -> bool) (table : LinkedMultiMap<'a, 'b>) = 
        let map = table.LinkedMap()
        LinkedMap.exists (fun k v -> LinkedSet.exists (predicate k) v) map
    
    let find (key : 'a) (table : LinkedMultiMap<'a, 'b>) = 
        let map = table.LinkedMap()
        if containsKey key table then raise (KeyNotFoundException())
        else LinkedMap.find key map
    
    let findkey (predicate : 'a -> 'b -> bool) (table : LinkedMultiMap<'a, 'b>) = 
        let map = table.LinkedMap()
        LinkedMap.findKey (fun k v -> LinkedSet.exists (predicate k) v) map
    
    let forall (predicate : 'a -> 'b -> bool) (table : LinkedMultiMap<'a, 'b>) = 
        let map = table.LinkedMap()
        LinkedMap.forall (fun k v -> LinkedSet.forall (predicate k) v) map
    
    let isEmpty (table : LinkedMultiMap<'a, 'b>) = 
        let map = table.LinkedMap()
        LinkedMap.isEmpty map
    
    let map (mapping : 'a -> 'b -> 'c) (table : LinkedMultiMap<'a, 'b>) = 
        let mutable map : LinkedMultiMap<'a, 'c> = empty
        iter (fun k v -> map <- add k (mapping k v) map) table
        map
    
    let tryFind (key : 'a) (table : LinkedMultiMap<'a, 'b>) = 
        let map = table.LinkedMap()
        LinkedMap.tryFind key map
    
    let ofSeq (elements : ('a * 'b) seq) = Seq.fold (fun s (k, v) -> add k v s) empty elements
    
    let ofArray (elements : ('a * 'b) []) = 
        elements
        |> Array.toSeq
        |> ofSeq
    
    let ofList (elements : ('a * 'b) list) = 
        elements
        |> List.toSeq
        |> ofSeq
    
    let remove (key : 'a) (table : LinkedMultiMap<'a, 'b>) = 
        match tryFind key table with
        | Some(v) -> 
            let map = table.LinkedMap()
            LinkedMultiMap(LinkedMap.remove key map)
        | None -> table
    
    let toSeq (table : LinkedMultiMap<'a, 'b>) = 
        let m = table.LinkedMap()
        let s = m.List()
        let mutable seq : ('a * 'b) list = List.empty
        iter (fun k v -> seq <- (k, v) :: seq) table
        seq
        |> Seq.ofList
        |> Seq.rev
    
    let filter (predicate : 'a -> 'b -> bool) (table : LinkedMultiMap<'a, 'b>) = 
        let r = 
            table.LinkedMap()
            |> LinkedMap.map (fun k v -> v |> LinkedSet.filter (predicate k))
            |> LinkedMap.filter (fun k v -> 
                   v
                   |> LinkedSet.isEmpty
                   |> not)
        LinkedMultiMap(r)
    
    let partition (predicate : 'a -> 'b -> bool) (table : LinkedMultiMap<'a, 'b>) = 
        let map = table.LinkedMap()
        let map1 = filter predicate table
        let map2 = filter (fun k v -> not (predicate k v)) table
        map1, map2
    
    let pick (chooser : 'a -> 'b -> 'c option) (table : LinkedMultiMap<'a, 'b>) = 
        let seq = toSeq table
        Seq.pick (fun (k, v) -> chooser k v) seq
    
    let fold (folder : 's -> 'a -> 'b -> 's) (state : 's) (table : LinkedMultiMap<'a, 'b>) = 
        let seq = toSeq table
        Seq.fold (fun s (xk, xv) -> folder s xk xv) state seq
    
    let foldBack (folder : 'a -> 'b -> 's -> 's) (table : LinkedMultiMap<'a, 'b>) (state : 's) = 
        let seq = toSeq table
        Seq.foldBack (fun (k, v) s -> folder k v s) seq state
    
    let toArray (table : LinkedMultiMap<'a, 'b>) = 
        table
        |> toSeq
        |> Array.ofSeq
    
    let toList (table : LinkedMultiMap<'a, 'b>) = 
        table
        |> toSeq
        |> List.ofSeq
    
    let tryFindKey (predicate : 'a -> 'b -> bool) (table : LinkedMultiMap<'a, 'b>) = 
        match exists predicate table with
        | true -> Some(findkey predicate table)
        | false -> None
    
    let tryPick (chooser : 'a -> 'b -> 'c option) (table : LinkedMultiMap<'a, 'b>) = 
        let seq = toSeq table
        Seq.pick (fun (k, v) -> chooser k v)
    
    let difference (table1 : LinkedMultiMap<'a, 'b>) (table2 : LinkedMultiMap<'a, 'b>) = 
        filter (fun k v -> 
            match tryFindKey (fun a b -> k = a || v = b) table2 with
            | Some(x) -> false
            | None -> true) table1
    
    let groupBy (projection : 'a -> 'b -> 'key) (table : LinkedMultiMap<'a, 'b>) = 
        table
        |> fold (fun s k v -> 
               let key = projection k v
               s |> add key (k, v)) empty
        |> (fun x -> x.LinkedMap())
    
    let sort (table : LinkedMultiMap<'a, 'b>) = 
        table
        |> toSeq
        |> Seq.sort
        |> ofSeq
    
    let sortBy (projection : 'a -> 'b -> 'key) (table : LinkedMultiMap<'a, 'b>) = 
        table
        |> toSeq
        |> Seq.sortBy (fun (k, v) -> projection k v)
        |> ofSeq
    
    let sortWith (comparer : 'a * 'b -> ('a * 'b -> int)) (table : LinkedMultiMap<'a, 'b>) = 
        table
        |> toSeq
        |> Seq.sortWith (fun k1 k2 -> comparer k1 k2)
        |> ofSeq
    
    let sortDescending (table : LinkedMultiMap<'a, 'b>) = 
        table
        |> toSeq
        |> Seq.sort
        |> ofSeq
    
    let sortByDescending (projection : 'a -> 'b -> 'key) (table : LinkedMultiMap<'a, 'b>) = 
        table
        |> toSeq
        |> Seq.sortBy (fun (k, v) -> projection k v)
        |> ofSeq

    let length (table : LinkedMultiMap<'a, 'b>) = table.LinkedMap() |> LinkedMap.length
