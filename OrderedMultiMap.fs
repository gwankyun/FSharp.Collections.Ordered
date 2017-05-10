namespace OrderedCollection

type OrderedMultiMap<'k, 'v  when 'k : comparison and 'v : comparison>(lst : 'k list, table : Map<'k, (Set<'v> * 'v list)>) =
    member this.List = lst
    member this.Table = table

module OrderedMultiMap =
    let empty<'k, 'v when 'k : comparison and 'v : comparison> =
        let (lst : 'k list) = List.empty
        let (table : Map<'k, (Set<'v> * 'v list)>) = Map.empty
        OrderedMultiMap(lst, table)

    let add (key : 'k) (value : 'v) (map : OrderedMultiMap<'k, 'v>) =
        let lst = map.List
        let table = map.Table
        match table |> Map.tryFind key with
        | Some (s, l) ->
            match s |> Set.contains value with
            | true -> map
            | false ->
                let s = s |> Set.add value
                let l = value :: l
                let table = table |> Map.add key (s, l)
                OrderedMultiMap(lst, table)
        | None ->
            let lst = key :: lst
            let s = Set.empty |> Set.add value
            let l = List.singleton value
            let table = table |> Map.add key (s, l)
            OrderedMultiMap(lst, table)

    let toList (map : OrderedMultiMap<'k, 'v>) = 
        let table = map.Table
        map.List
        |> List.filter (fun x -> table |> Map.containsKey x)
        |> List.rev
        |> List.map (fun key ->
            let (s, l) = table |> Map.find key
            let l = 
                l
                |> List.filter (fun x -> s |> Set.contains x)
                |> List.rev
                |> List.map (fun x -> (key, x))
            l)
        |> List.concat

    let ofList (lst : ('k * 'v) list) =
        lst
        |> List.fold (fun s (k, v) ->
            s |> add k v) empty

    let ofSeq (lst : ('k * 'v) seq) =
        lst
        |> Seq.fold (fun s (k, v) ->
            s |> add k v) empty


    //let partition (predicate : 'k -> 'v -> bool) (table : OrderedMultiMap<'k, 'v>) =
    //    let first = table.