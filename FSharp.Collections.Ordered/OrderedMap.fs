namespace OrderedCollection
open FSharpx.Collections
open FSharpx.Functional
open FSharpx.Functional.Prelude

type OrderedMap<[<EqualityConditionalOn>] 'k, 'v  when 'k : comparison and 'v : comparison>(first : 'k option, table : Map<'k, ('k * 'v * 'k)>, last : 'k option) =
    class
        member this.First = first
        member this.Last = last
        member this.Map = table
        override x.ToString() =
            table.ToString()
        override x.GetHashCode() = hash x.First
        override x.Equals(y : obj) =
            match y with
            | :? OrderedMap<'k, 'v> as y -> (x.First = y.First && x.Last = y.Last && x.Map = y.Map) || (x.Map |> Map.isEmpty && y.Map |> Map.isEmpty)
            | _ -> false
        interface System.IComparable
            with
                member this.CompareTo(o : obj) = 0
            end
    end

module OrderedMap =
    begin
        //let flip f a b = f b a

        let isEmpty (set : OrderedMap<'k, 'v>) =
            set.Map |> Map.isEmpty

        let (|IsEmpty|_|) (set : OrderedMap<'k, 'v>) =
            match set |> isEmpty with
            | true -> Some()
            | false -> None

        let add (key : 'k) (value : 'v) (set : OrderedMap<'k, 'v>) =
            match set with
            | IsEmpty ->
                let first = Some key in
                let last = Some key in
                let map = Map.empty |> Map.add key (key, value, key) in
                OrderedMap(first, map, last)
            | _ ->
                let first = set.First |> Option.get in
                let last = set.Last |> Option.get in
                let map = set.Map in
                match map |> Map.tryFind key with
                | Some (_, _, _) ->
                    let map =
                        map
                        |> Map.updateWith (fun (prev, _, next) -> Some (prev, value, next)) key
                    in
                    OrderedMap(Some first, map, Some last)
                | None ->
                    let (prev, _, _) = map |> Map.find last in
                    let map =
                        map
                        |> Map.updateWith (fun (prev, v, _) -> Some (prev, v, key)) last
                        |> Map.add key (last, value, key)
                    in
                    OrderedMap(Some first, map, Some key)

        let containsKey (key : 'k) (set : OrderedMap<'k, 'v>) =
            set.Map |> Map.containsKey key

        let count (set : OrderedMap<'k, 'v>) =
            set.Map |> Map.count

        let empty<'k, 'v  when 'k : comparison and 'v : comparison> : OrderedMap<'k, 'v> =
            OrderedMap(None, Map.empty, None)

        let fold (folder : 's -> 'k -> 'v -> 's) (state : 's) (set : OrderedMap<'k, 'v>) =
            match set with
            | IsEmpty -> state
            | _ ->
                let first = set.First |> Option.get in
                let last = set.Last |> Option.get in
                let rec inner s k =
                    let map = set.Map in
                    let (_, v, next) = map.[k] in
                    let s = folder s k v in
                    match k = last with
                    | true -> s
                    | false ->
                        inner s next
                inner state first
        
        let remove (key : 'k) (set : OrderedMap<'k, 'v>) =
            match set with
            | IsEmpty -> empty
            | _ ->
                let first = set.First |> Option.get in
                let last = set.Last |> Option.get in
                let map = set.Map in
                match map |> Map.tryFind key with
                | Some (prev, _, next) ->
                    match (key = first, key = last) with
                    | (true, true) -> empty // 只有一個鍵
                    | (true, false) -> // 第一個
                        //let first = next in
                        let map =
                            map
                            |> Map.remove key
                            |> Map.updateWith (fun (_, v, n) -> Some (next, v, n)) next
                        in
                        OrderedMap(Some next, map, Some last)
                    | (false, true) ->
                        //let last = prev in
                        let map =
                            map
                            |> Map.remove key
                            |> Map.updateWith (fun (p, v, _) -> Some (p, v, prev)) prev
                        in
                        OrderedMap(Some first, map, Some prev)
                    | (false, false) ->
                        let map =
                            map
                            |> Map.remove key
                            |> Map.updateWith (fun (p, v, _) -> Some (p, v, next)) prev
                            |> Map.updateWith (fun (_, v, n) -> Some (prev, v, n)) next
                        in
                        OrderedMap(Some first, map, Some last)
                | None -> set
            
        let exists (predicate : 'k -> 'v -> bool) (set : OrderedMap<'k, 'v>) =
            let map = set.Map in
            map |> Map.exists (fun k v ->
                                begin
                                    let (_, v, _) = map.[k];
                                    predicate k v;
                                end
                                    )
            
        let filter (predicate : 'k -> 'v -> bool) (set : OrderedMap<'k, 'v>) =
            fold (fun s k v ->
                match (predicate k v) with
                | true -> s |> add k v
                | false -> s)
                empty set
                
        let find (key : 'k) (set : OrderedMap<'k, 'v>) =
            let (_, value, _) = set.Map.[key] in
            value
            
        let tryFind (key : 'k) (set : OrderedMap<'k, 'v>) =
            set.Map
            |> Map.tryFind key
            |> Option.map (fun (_, v, _) -> v)

        let findKey (predicate : 'k -> 'v -> bool) (set : OrderedMap<'k, 'v>) =
            set.Map |> Map.findKey (fun k (_, v, _) -> predicate k v)

        let tryFindKey (predicate : 'k -> 'v -> bool) (set : OrderedMap<'k, 'v>) =
            set.Map |> Map.tryFindKey (fun k (_, v, _) -> predicate k v)
                
        let iter (action : 'k -> 'v -> unit) (set : OrderedMap<'k, 'v>) =
            fold (fun s k v ->
                begin
                    action k v;
                    s;
                end) empty set

        let forall (predicate : 'k -> 'v -> bool) (set : OrderedMap<'k, 'v>) =
            let map = set.Map in
            map |> Map.forall (fun k v ->
                                   let (_, v, _) = map.[k] in
                                   predicate k v
                                   )
        let map (mapping : 'k -> 'v -> 'u) (set : OrderedMap<'k, 'v>) =
            fold (fun s k v -> s |> add k (mapping k v)) empty set

        let addTuple s (k, v) =
            s |> add k v
            
        let ofArray (array : ('k * 'v) []) =
            Array.fold addTuple empty array
            
        let ofList (elements : ('k * 'v) list) =
            List.fold addTuple empty elements

        let ofSeq (elements : ('k * 'v) seq) =
            Seq.fold addTuple empty elements

        let partition (predicate : 'k -> 'v -> bool) (set : OrderedMap<'k, 'v>) =
            fold (fun (set1, set2) k v ->
                match predicate k v with
                | true -> (set1 |> add k v, set2)
                | false -> (set1, set2 |> add k v)
            ) (empty, empty) set
            
        let foldBack (folder : 'k -> 'v -> 's -> 's) (set : OrderedMap<'k, 'v>) (state : 's) =
            match set with
            | IsEmpty -> state
            | _ ->
                let first = set.First |> Option.get in
                let last = set.Last |> Option.get in
                let rec inner s k =
                    let map = set.Map in
                    let (prev, v, _) = map.[k] in
                    let s = folder k v s in
                    match k = first with
                    | true ->
                        s
                    | false ->
                        inner s prev
                inner state last

        let toList (set : OrderedMap<'k, 'v>) =
            foldBack (fun k v s ->
                (k, v) :: s) set []

        let toArray (set : OrderedMap<'k, 'v>) =
            set
            |> toList
            |> List.toArray

        let toSeq (set : OrderedMap<'k, 'v>) =
            set
            |> toList
            |> List.toSeq

        let updateWith (f : 'v -> 'v option) (key : 'k) (set : OrderedMap<'k, 'v>) =
            match set |> tryFind key with
            | Some(v) ->
                match f v with
                | Some(value) ->
                    set
                    |> add key value
                | None -> set |> remove key
            | None -> set
    end
