namespace OrderedCollection
open FSharpx.Collections

type OrderedSet<'k when 'k : comparison>(first : 'k option, map : Map<'k, ('k * 'k)>, last : 'k option) =
    member this.First = first
    member this.Map = map
    member this.Last = last

module OrderedSet =
    let isEmpty (set : OrderedSet<'k>) =
        set.Map |> Map.isEmpty

    let add (value : 'k) (set : OrderedSet<'k>) =
        match set |> isEmpty with
        | true ->
            let first = Some value
            let last = Some value
            let map = Map.empty |> Map.add value (value, value)
            OrderedSet(first, map, last)
        | false ->
            let first = set.First |> Option.get
            let last = set.Last |> Option.get
            let map = set.Map
            match map |> Map.tryFind value with
            | Some (_, _) ->
                set
            | None ->
                let (prev, _) = map |> Map.find last
                let map =
                    map
                    |> Map.updateWith (fun (prev, _) -> Some (prev, value)) last
                    |> Map.add value (last, value)
                OrderedSet(Some first, map, Some value)

    let contains (value : 'k) (set : OrderedSet<'k>) =
        set.Map |> Map.containsKey value

    let count (set : OrderedSet<'k>) =
        set.Map |> Map.count

    let empty<'k when 'k : comparison> : OrderedSet<'k> =
        OrderedSet(None, Map.empty, None)
        
    let fold (folder : 's -> 't -> 's) (state : 's) (set : OrderedSet<'t>) =
        match set |> isEmpty with
        | true -> state
        | false ->
            let first = set.First |> Option.get
            let last = set.Last |> Option.get
            let rec inner s t =
                let s = folder s t
                match t = last with
                | true -> s
                | false ->
                    let map = set.Map
                    let (_, next) = map.[t]
                    inner s next
            inner state first

    let foldBack (folder : 't -> 's -> 's) (set : OrderedSet<'t>) (state : 's) =
        match set |> isEmpty with
        | true -> state
        | false ->
            let first = set.First |> Option.get
            let last = set.Last |> Option.get
            let rec inner s t =
                let s = folder t s
                match t = first with
                | true -> s
                | false ->
                    let map = set.Map
                    let (prev, _) = map.[t]
                    inner s prev
            inner state last

        
    let remove (value : 'k) (set : OrderedSet<'k>) =
        match set |> isEmpty with
        | true -> set
        | false ->
            let first = set.First |> Option.get
            let last = set.Last |> Option.get
            let map = set.Map
            match map |> Map.tryFind value with
            | Some (prev, next) ->
                let map = map |> Map.remove value
                match (prev = first, next = last) with
                | (true, true) ->
                    OrderedSet(Some first, map, Some last)
                | (true, _) ->
                    let first = next
                    let map =
                        map
                        |> Map.updateWith (fun (_, n) -> Some (next, n)) next
                    OrderedSet(Some first, map, Some last)
                | (_, true) ->
                    let last = next
                    let map =
                        map
                        |> Map.updateWith (fun (p, _) -> Some (p, prev)) prev
                    OrderedSet(Some first, map, Some last)
                | (_, _) ->
                    let map =
                        map
                        |> Map.updateWith (fun (p, _) -> Some (p, next)) prev
                        |> Map.updateWith (fun (_, n) -> Some (prev, n)) next
                    OrderedSet(Some first, map, Some last)
            | None -> set

    let toList (set : OrderedSet<'k>) =
        foldBack List.cons set []
            