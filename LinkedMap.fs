namespace FSharp.Collections

open System.Collections.Generic
open FSharp.Extension
open FSharp.Collections
open FSharpx.Collections

module LinkedMap = 
    let add (key : 'a) (value : 'b) (set : LinkedMap<'a, 'b>) = 
        let a, b = set.List(), set.Map()
        let seq = LazyList.cons key a
        LinkedMap(seq, b.Add(key, value))
    
    let containsKey (key : 'a) (table : LinkedMap<'a, 'b>) = table.Map() |> Map.containsKey key
    let empty<'a, 'b when 'a : comparison and 'b : comparison> = LinkedMap<'a, 'b>(LazyList.empty, Map.empty)
    let exists (predicate : 'a -> 'b -> bool) (table : LinkedMap<'a, 'b>) = table.Map() |> Map.exists predicate
    
    let filter (predicate : 'a -> 'b -> bool) (table : LinkedMap<'a, 'b>) = 
        let seq, map = table.List(), table.Map()
        LinkedMap(LazyList.filter (fun x -> predicate x map.[x]) seq, Map.filter predicate map)
    
    let find (key : 'a) (table : LinkedMap<'a, 'b>) = table.Map() |> Map.find key
    let findKey (predicate : 'a -> 'b -> bool) (table : LinkedMap<'a, 'b>) = table.Map() |> Map.findKey predicate
    
    let fold (folder : 's -> 'a -> 'b -> 's) (state : 's) (table : LinkedMap<'a, 'b>) = 
        let list, map = table.List(), table.Map()
        list
        |> LazyList.filter (fun x -> map |> Map.containsKey x)
        |> LazyList.fold (fun s x -> folder s x map.[x]) state
    
//    let foldBack (folder : 'a -> 'b -> 's -> 's) (table : LinkedMap<'a, 'b>) (state : 's) = 
//        let list, map = table.List(), table.Map()
//        list
//        |> LazyList.filter (fun x -> map |> Map.containsKey x)
//        |> (fun i -> LazyList.f (fun x s -> folder x map.[x] state) i state)
    
    let forall (predicate : 'a -> 'b -> bool) (table : LinkedMap<'a, 'b>) = table.Map() |> Map.forall predicate
    let isEmpty (table : LinkedMap<'a, 'b>) = table.Map() |> Map.isEmpty
    
    let iter (action : 'a -> 'b -> unit) (table : LinkedMap<'a, 'b>) = 
        let list, map = table.List(), table.Map()
        list
        |> LazyList.filter (fun x -> map |> Map.containsKey x)
        |> LazyList.iter (fun x -> action x map.[x])
    
    let map (mapping : 'a -> 'b -> 'c) (table : LinkedMap<'a, 'b>) = 
        let list, map = table.List(), table.Map()
        let map = Map.map mapping map
        LinkedMap(list, map)
    
    let ofArray (elements : ('a * 'b) []) = 
        let seq = LazyList.ofArray elements
        let map = seq |> LazyList.toSeq |> Map.ofSeq
        LinkedMap(LazyList.map Tuple.first seq, map)
    
    let ofList (elements : ('a * 'b) list) = 
        let seq = LazyList.ofList elements
        let map = seq |> LazyList.toSeq |> Map.ofSeq
        LinkedMap(LazyList.map Tuple.first seq, map)
    
    let ofSeq (elements : ('a * 'b) seq) = elements |> Seq.fold (fun k (a, b) -> k |> add a b) empty
    
    //    let partition (predicate : 'a -> 'b -> bool) (table : LinkedMap<'a, 'b>) = 
    //        let seq, map = table.List(), table.Map()
    //        let seq1, seq2 = LazyList.partition (fun x -> predicate x map.[x]) seq
    //        let map1, map2 = Map.partition predicate map
    //        LinkedMap(seq1, map1), LinkedMap(seq2, map2)
    let remove (key : 'a) (table : LinkedMap<'a, 'b>) = 
        let seq, map = table.List() |> LazyList.filter ((<>) key), table.Map() |> Map.remove key
        LinkedMap(seq, map)
    
    let toSeq (table : LinkedMap<'a, 'b>) = 
        let seq, map = table.List(), table.Map()
        seq
        |> LazyList.filter (fun x -> map |> Map.containsKey x)
        |> LazyList.map (fun x -> x, find x table)
        |> LazyList.toSeq
    
    let toArray (table : LinkedMap<'a, 'b>) = 
        table
        |> toSeq
        |> Array.ofSeq
    
    let toList (table : LinkedMap<'a, 'b>) = 
        table
        |> toSeq
        |> List.ofSeq
    
    let tryFind (key : 'a) (table : LinkedMap<'a, 'b>) = table.Map() |> Map.tryFind key
    let tryFindKey (predicate : 'a -> 'b -> bool) (table : LinkedMap<'a, 'b>) = table.Map() |> Map.tryFindKey predicate
    
    let tryPick (chooser : 'a -> 'b -> 'c option) (table : LinkedMap<'a, 'b>) = 
        let seq, map = table.List(), table.Map()
        seq
        |> LazyList.filter (fun x -> map |> Map.containsKey x)
        |> LazyList.toSeq
        |> Seq.tryPick (fun x -> chooser x map.[x])
    
    let difference (table1 : LinkedMap<'a, 'b>) (table2 : LinkedMap<'a, 'b>) = 
        let map1, map2 = table1.Map(), table2.Map()
        let map = Map.difference map1 map2
        table1 |> filter (fun k v -> map |> Map.containsKey k)
    
    //        let s1 = table1.List()
    //        let m1 = table1.Map()
    //        let m2 = table2.Map()
    //        let map = Map.difference m1 m2
    //        let seq = LazyList.filter (fun x -> Map.containsKey x map) s1
    //        LinkedMap(seq, map)
    //    let sortBy (projection : 'a -> 'b -> 'key) (table : LinkedMap<'a, 'b>) =
    //        table
    //        |> Seq.sortBy (fun (k, v) -> projection k v)
    //        |> ofSeq
    //    let sortBy (projection : 'a -> 'b -> 'key) (table : LinkedMap<'a, 'b>) =
    //        table
    //        |> toSeq
    //        |> Seq.sortBy (fun (k, v) -> projection k v)
    //        |> ofSeq
    let sort (table : LinkedMap<'a, 'b>) = 
        table
        |> toSeq
        |> Seq.sort
        |> ofSeq
    
    let sortBy (projection : 'a -> 'b -> 'key) (table : LinkedMap<'a, 'b>) = 
        table
        |> toSeq
        |> Seq.sortBy (fun (k, v) -> projection k v)
        |> ofSeq
    
//    let sortWith (comparer : 'a * 'b -> ('a * 'b -> int)) (table : LinkedMap<'a, 'b>) = 
//        table
//        |> toSeq
//        |> Seq.sortWith (fun k1 k2 -> comparer k1 k2)
//        |> ofSeq
    
    let sortDescending (table : LinkedMap<'a, 'b>) = 
        table
        |> toSeq
        |> Seq.sort
        |> ofSeq
    
    let sortByDescending (projection : 'a -> 'b -> 'key) (table : LinkedMap<'a, 'b>) = 
        table
        |> toSeq
        |> Seq.sortBy (fun (k, v) -> projection k v)
        |> ofSeq
    
    let length (table : LinkedMap<'a, 'b>) = table.List() |> LazyList.length
    
    let keys (table : LinkedMap<'a, 'b>) = 
        table
        |> toSeq
        |> Seq.map (fun (a, _) -> a)
    
    let values (table : LinkedMap<'a, 'b>) = 
        table
        |> toSeq
        |> Seq.map (fun (_, b) -> b)
