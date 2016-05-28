namespace FSharp.Collections

open System.Collections.Generic
open Extension
open FSharp.Collections

type LinkedMap<'a, 'b when 'a : comparison>(x : LazyList<'a>, y : Map<'a, 'b>) = 
    member this.Seq() = x
    member this.Map() = y

module LinkedMap = 
    let add (key : 'a) (value : 'b) (set : LinkedMap<'a, 'b>) = 
        let (a, b) = (set.Seq(), set.Map())
        let seq = a.Cons(key)
        LinkedMap(seq, b.Add(key, value))
    
    let containsKey (key : 'a) (table : LinkedMap<'a, 'b>) = Map.containsKey key (table.Map())
    let empty<'a, 'b when 'a : comparison and 'b : comparison> = LinkedMap<'a, 'b>(LazyList.empty, Map.empty)
    let exists (predicate : 'a -> 'b -> bool) (table : LinkedMap<'a, 'b>) = Map.exists predicate (table.Map())
    
    let filter (predicate : 'a -> 'b -> bool) (table : LinkedMap<'a, 'b>) = 
        let seq, map = table.Seq(), table.Map()
        LinkedMap(LazyList.filter (fun x -> predicate x map.[x]) seq, Map.filter predicate map)
    
    let find (key : 'a) (table : LinkedMap<'a, 'b>) = Map.find key (table.Map())
    let findKey (predicate : 'a -> 'b -> bool) (table : LinkedMap<'a, 'b>) = Map.findKey predicate (table.Map())
    
    let fold (folder : 's -> 'a -> 'b -> 's) (state : 's) (table : LinkedMap<'a, 'b>) = 
        let seq, map = table.Seq(), table.Map()
        LazyList.fold (fun s x -> folder state x map.[x]) state seq
    
    let foldBack (folder : 'a -> 'b -> 's -> 's) (table : LinkedMap<'a, 'b>) (state : 's) = 
        let seq, map = table.Seq(), table.Map()
        LazyList.foldBack (fun x s -> folder x map.[x] state) seq state
    
    let forall (predicate : 'a -> 'b -> bool) (table : LinkedMap<'a, 'b>) = Map.forall predicate (table.Map())
    let isEmpty (table : LinkedMap<'a, 'b>) = Map.isEmpty (table.Map())
    let iter (action : 'a -> 'b -> unit) (table : LinkedMap<'a, 'b>) = Map.iter action (table.Map())
    
    let map (mapping : 'a -> 'b -> 'c) (table : LinkedMap<'a, 'b>) = 
        let seq, map = table.Seq(), table.Map()
        let map = Map.map mapping map
        LinkedMap(seq, map)
    
    let ofArray (elements : ('a * 'b) []) = 
        let seq = LazyList.ofArray elements
        let map = Map.ofSeq (seq.List())
        LinkedMap(LazyList.map (fun (k, _) -> k) seq, map)
    
    let ofList (elements : ('a * 'b) list) = 
        let seq = LazyList.ofList elements
        let map = Map.ofSeq (seq.List())
        LinkedMap(LazyList.map (fun (k, _) -> k) seq, map)
    
    let ofSeq (elements : ('a * 'b) seq) = 
        let seq = 
            elements
            |> Seq.map (fun (k, _) -> k)
            |> LazyList.ofSeq
        
        let map = Map.ofSeq elements
        LinkedMap(seq, map)
    
    let partition (predicate : 'a -> 'b -> bool) (table : LinkedMap<'a, 'b>) = 
        let seq, map = table.Seq(), table.Map()
        let seq1, seq2 = LazyList.partition (fun x -> predicate x map.[x]) seq
        let map1, map2 = Map.partition predicate map
        LinkedMap(seq1, map1), LinkedMap(seq2, map2)
    
    let pick (chooser : 'a -> 'b -> 'c option) (table : LinkedMap<'a, 'b>) = 
        let seq, map = table.Seq(), table.Map()
        Seq.pick (fun x -> chooser x map.[x]) (LazyList.toSeq seq).Value
    
    let remove (key : 'a) (table : LinkedMap<'a, 'b>) = 
        let seq, map = table.Seq() |> LazyList.filter ((=) key), table.Map() |> Map.remove key
        LinkedMap(seq, map)
    
    let toSeq (table : LinkedMap<'a, 'b>) = 
        let seq = table.Seq()
        seq
        |> LazyList.map (fun x -> (x, find x table))
        |> LazyList.toSeq
    
    let toArray (table : LinkedMap<'a, 'b>) = (table |> toSeq).Value |> Array.ofSeq
    let toList (table : LinkedMap<'a, 'b>) = (table |> toSeq).Value |> List.ofSeq
    let tryFind (key : 'a) (table : LinkedMap<'a, 'b>) = Map.tryFind key (table.Map())
    let tryFindKey (predicate : 'a -> 'b -> bool) (table : LinkedMap<'a, 'b>) = Map.tryFindKey predicate (table.Map())
    
    let tryPick (chooser : 'a -> 'b -> 'c option) (table : LinkedMap<'a, 'b>) = 
        let seq, map = toSeq table, table.Map()
        Seq.tryPick (fun x -> chooser x map.[x])
    
    let difference (table1 : LinkedMap<'a, 'b>) (table2 : LinkedMap<'a, 'b>) = 
        let s1 = table1.Seq()
        let m1 = table1.Map()
        let m2 = table2.Map()
        let map = Map.difference m1 m2
        let seq = LazyList.filter (fun x -> Map.containsKey x map) s1
        LinkedMap(seq, map)

//    let sortBy (projection : 'a -> 'b -> 'key) (table : LinkedMap<'a, 'b>) =
//        table
//        |> Seq.sortBy (fun (k, v) -> projection k v)
//        |> ofSeq
