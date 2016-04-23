namespace Original.Collections
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

//    let containsKey (key : 'a) (table : OriginalMultiMap<'a, 'b>) =
//        let map  = table.OriginalMap().Map()
//        Map.containsKey key map
//
//    let exists (predicate : 'a -> 'b -> bool) (table : OriginalMultiMap<'a, 'b>) =
//        let map  = table.OriginalMap().Map()
//        Map.exists (fun k t -> OriginalSet.exists (predicate k) t) map
//
//    let filter (predicate : 'a -> 'b -> bool) (table : OriginalMap<'a, 'b>) =
//        let seq, map = table.Seq(), table.Map()
//        OriginalMap(Seq.filter (fun x -> predicate x map.[x]) seq, Map.filter predicate map)
//
//    let find (key : 'a) (table : OriginalMultiMap<'a, 'b>) =
//        let map  = table.OriginalMap().Map()
//        Map.find key map
//
//    let findKey (predicate : 'a -> 'b -> bool) (table : OriginalMultiMap<'a, 'b>) =
//        let map  = table.OriginalMap()
//        Map.findKey predicate map
//
//    let fold (folder : 's -> 'a -> 'b -> 's) (state : 's) (table : OriginalMap<'a, 'b>) =
//        let seq, map = table.Seq(), table.Map()
//        Seq.fold (fun s x -> folder state x map.[x]) state seq
//
//    let foldBack (folder : 'a -> 'b -> 's -> 's) (table : OriginalMap<'a, 'b>) (state : 's) =
//        let seq, map = table.Seq(), table.Map()
//        Seq.foldBack (fun x s -> folder x map.[x] state) seq state
//
//    let forall (predicate : 'a -> 'b -> bool) (table : OriginalMap<'a, 'b>) =
//        Map.forall predicate (table.Map())
//
//    let isEmpty (table : OriginalMap<'a, 'b>) =
//        Map.isEmpty (table.Map())
//
//    let iter (action : 'a -> 'b -> unit) (table : OriginalMap<'a, 'b>) =
//        Map.iter action (table.Map())
//
//    let map (mapping : 'a -> 'b -> 'c) (table : OriginalMap<'a, 'b>) =
//        let seq, map = table.Seq(), table.Map()
//        let map = Map.map mapping map
//        OriginalMap(seq, map)
//
//    let ofArray (elements : ('a * 'b) []) =
//        let seq = Seq.ofArray elements
//        let map = Map.ofSeq seq
//        OriginalMap(Seq.map (fun (k, _) -> k) seq, map)
//
//    let ofList (elements : ('a * 'b) list) =
//        let seq = Seq.ofList elements
//        let map = Map.ofSeq seq
//        OriginalMap(Seq.map (fun (k, _) -> k) seq, map)
//        
//    let ofSeq (elements : ('a * 'b) seq) =
//        let seq = Seq.map (fun (k, _) -> k) elements
//        let map = Map.ofSeq elements
//        OriginalMap(seq, map)
//
//    let partition (predicate : 'a -> 'b -> bool) (table : OriginalMap<'a, 'b>) =
//        let seq, map = table.Seq(), table.Map()
//        let seq1, seq2 = Seq.partition (fun x -> predicate x map.[x]) seq
//        let map1, map2 = Map.partition predicate map
//        OriginalMap(seq1, map1), OriginalMap(seq2, map2)
//
//    let pick (chooser : 'a -> 'b -> 'c option) (table : OriginalMap<'a, 'b>) =
//        let seq, map = table.Seq(), table.Map()
//        Seq.pick (fun x -> chooser x map.[x]) seq
//
//    let remove (key : 'a) (table : OriginalMap<'a, 'b>) =
//        let seq, map = table.Seq() |> Seq.filter ((=) key), table.Map() |> Map.remove key
//        OriginalMap(seq, map)
//
//    let toSeq (table : OriginalMap<'a, 'b>) =
//        table.Seq()
//
//    let toArray (table : OriginalMap<'a, 'b>) =
//        table |> toSeq |> Array.ofSeq
//        
//    let toList (table : OriginalMap<'a, 'b>) =
//        table |> toSeq |> List.ofSeq
//
//    let tryFind (key : 'a) (table : OriginalMap<'a, 'b>) =
//        Map.tryFind key (table.Map())
//
//    let tryFindKey (predicate : 'a -> 'b -> bool) (table : OriginalMap<'a, 'b>) =
//        Map.tryFindKey predicate (table.Map())
//
//    let tryPick (chooser : 'a -> 'b -> 'c option) (table : OriginalMap<'a, 'b>) =
//        let seq, map = toSeq table, table.Map()
//        Seq.tryPick (fun x -> chooser x map.[x]) seq

