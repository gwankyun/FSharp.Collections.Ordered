namespace OrderedCollection
open FSharpx.Collections
open FSharpx.Functional
open FSharpx.Functional.Prelude

type OrderedMultiMap<'k, 'v  when 'k : comparison and 'v : comparison>(map : OrderedMap<'k, OrderedSet<'v>>) =
    class
        member this.First = map.First
        member this.Last = map.Last
        member this.Map = map
    end

module OrderedMultiMap =
    begin
        //let flip f a b = f b a
        //let empty<'k,'v when 'k : comparison and 'v : comparison> : OrderedMultiMap<'k, 'v> =
        //    OrderedMultiMap(OrderedMap.empty)

        let isEmpty (set : OrderedMultiMap<'k, 'v>) =
            set.Map |> OrderedMap.isEmpty

        let (|IsEmpty|_|) (set : OrderedMultiMap<'k, 'v>) =
            match set |> isEmpty with
            | true -> Some()
            | false -> None

        let ofOrderedMap(map : OrderedMap<'k, OrderedSet<'v>>) =
            OrderedMultiMap(map)

        let empty<'k, 'v  when 'k : comparison and 'v : comparison> : OrderedMultiMap<'k, 'v> =
            OrderedMap(None, Map.empty, None) |> ofOrderedMap

        let add (key : 'k) (value : 'v) (set : OrderedMultiMap<'k, 'v>) =
            match set with
            | IsEmpty -> empty
            | _ ->
                let first = set.First |> Option.get in
                let last = set.Last |> Option.get in
                let map = set.Map in
                match map |> OrderedMap.tryFind key with
                | Some (v) ->
                    map
                    |> OrderedMap.updateWith (fun v -> Some (v |> OrderedSet.add value)) key
                    |> ofOrderedMap
                | None ->
                    map
                    |> OrderedMap.add key (OrderedSet.singleton value)
                    |> ofOrderedMap

        let containsKey (key : 'k) (set : OrderedMultiMap<'k, 'v>) =
            set.Map |> OrderedMap.containsKey key

        let count (set : OrderedMultiMap<'k, 'v>) =
            set.Map
            |> OrderedMap.map (fun _ v -> v |> OrderedSet.count)
            |> OrderedMap.fold (fun s _ v -> s + v) 0

        let fold (folder : 's -> 'k -> 'v -> 's) (state : 's) (set : OrderedMultiMap<'k, 'v>) =
            set.Map
            |> OrderedMap.fold (fun s k v ->
                                    v |> OrderedSet.fold (fun ss i -> folder ss k i) s) state
        
        let remove (key : 'k) (set : OrderedMultiMap<'k, 'v>) =
            set.Map
            |> OrderedMap.remove key
            |> ofOrderedMap
            
        let exists (predicate : 'k -> 'v -> bool) (set : OrderedMultiMap<'k, 'v>) =
            let map = set.Map in
            map |> OrderedMap.exists (fun k v ->
                                        v |> OrderedSet.exists (fun value -> predicate k value))
            
        let filter (predicate : 'k -> 'v -> bool) (set : OrderedMultiMap<'k, 'v>) =
            fold (fun s k v ->
                match (predicate k v) with
                | true -> s |> add k v
                | false -> s)
                empty set
                
        let find (key : 'k) (set : OrderedMultiMap<'k, 'v>) =
            set.Map |> OrderedMap.find key
            
        let tryFind (key : 'k) (set : OrderedMultiMap<'k, 'v>) =
            set.Map
            |> OrderedMap.tryFind key
            //|> Option.map (fun (_, v, _) -> v)

        //let findKey (predicate : 'k -> 'v -> bool) (set : OrderedMultiMap<'k, 'v>) =
        //    set.Map |> OrderedMap.findKey (fun k (_, v, _) -> predicate k v)

        //let tryFindKey (predicate : 'k -> 'v -> bool) (set : OrderedMap<'k, 'v>) =
        //    set.Map |> Map.tryFindKey (fun k (_, v, _) -> predicate k v)
                
        let iter (action : 'k -> 'v -> unit) (set : OrderedMultiMap<'k, 'v>) =
            fold (fun s k v ->
                action k v
                s) empty set

        let forall (predicate : 'k -> 'v -> bool) (set : OrderedMultiMap<'k, 'v>) =
            let map = set.Map in
            map |> OrderedMap.forall (fun k v ->
                                   v |> OrderedSet.forall (fun value -> predicate k value))

        let map (mapping : 'k -> 'v -> 'u) (set : OrderedMultiMap<'k, 'v>) =
            fold (fun s k v -> s |> add k (mapping k v)) empty set

        let addTuple s (k, v) =
            s |> add k v
            
        let ofArray (array : ('k * 'v) []) =
            Array.fold addTuple empty array
            
        let ofList (elements : ('k * 'v) list) =
            List.fold addTuple empty elements

        let ofSeq (elements : ('k * 'v) seq) =
            Seq.fold addTuple empty elements

        let partition (predicate : 'k -> 'v -> bool) (set : OrderedMultiMap<'k, 'v>) =
            fold (fun (set1, set2) k v ->
                match predicate k v with
                | true -> (set1 |> add k v, set2)
                | false -> (set1, set2 |> add k v)
            ) (empty, empty) set
            
        let foldBack (folder : 'k -> 'v -> 's -> 's) (set : OrderedMultiMap<'k, 'v>) (state : 's) =
            OrderedMap.foldBack (fun k v s ->
                OrderedSet.foldBack (fun t ss -> folder k t ss) v s) set.Map state

        let toList (set : OrderedMultiMap<'k, 'v>) =
            foldBack (fun k v s -> (k, v) :: s) set []

        let toArray (set : OrderedMultiMap<'k, 'v>) =
            set
            |> toList
            |> List.toArray

        let toSeq (set : OrderedMultiMap<'k, 'v>) =
            set
            |> toList
            |> List.toSeq
    end