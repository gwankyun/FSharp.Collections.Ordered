//namespace Ordered
namespace FSharp.Collections.Ordered

type OrderedMap<'a, 'b when 'a : comparison> =
    { 
        Table: Map<'a, (int * 'b)>; 
        Index: int;
    }

module OrderedMap =
    let empty : OrderedMap<'a, 'b> =
        { Table = Map.empty; Index = 0; }

    let isEmpty (table: OrderedMap<'a, 'b>) : bool =
        table.Table |> Map.isEmpty

    let count (table: OrderedMap<'a, 'b>) : int =
        table.Table |> Map.count

    let containsKey (key: 'a) (table: OrderedMap<'a, 'b>) : bool =
        table.Table |> Map.containsKey key

    let exists (predicate: 'a -> 'b -> bool) (table: OrderedMap<'a, 'b>) : bool =
        table.Table |> Map.exists (fun k (_, v) -> predicate k v)

    let add (key: 'a) (value: 'b) (table: OrderedMap<'a, 'b>) : OrderedMap<'a, 'b> =
        let m = table.Table
        match m |> Map.tryFind key with
        | Some (i, v) -> { table with Table = m |> Map.add key (i, value) }
        | None ->
            let i = table.Index
            let t =  m |> Map.add key (i, value)
            { table with Table = t; Index = i + 1; }

    let remove (key: 'a) (table: OrderedMap<'a, 'b>) : OrderedMap<'a, 'b> =
        { table with Table = table.Table |> Map.remove key; }

    let ofList (elements: ('a * 'b) list) : OrderedMap<'a, 'b> =
        elements
        |> List.fold (fun s (k, v) -> s |> add k v) empty

    let ofArray (elements: ('a * 'b) []) : OrderedMap<'a, 'b> =
        elements
        |> Array.fold (fun s (k, v) -> s |> add k v) empty

    let ofSeq (elements: ('a * 'b) seq) : OrderedMap<'a, 'b> =
        elements
        |> Seq.fold (fun s (k, v) -> s |> add k v) empty

    let find (key: 'a) (table: OrderedMap<'a, 'b>) : 'b =
        table.Table
        |> Map.find key
        |> snd

    let findKey (predicate: 'a -> 'b -> bool) (table: OrderedMap<'a, 'b>) : 'a =
        table.Table
        |> Map.findKey (fun k (_, v) -> predicate k v)

    let tryFind (key: 'a) (table: OrderedMap<'a, 'b>) : 'b option =
        table.Table
        |> Map.tryFind key
        |> Option.map snd

    let tryFindKey (predicate: 'a -> 'b -> bool) (table: OrderedMap<'a, 'b>) : 'a option =
        table.Table
        |> Map.tryFindKey (fun k (_, v) -> predicate k v)

    let toSeq (table: OrderedMap<'a, 'b>) : ('a * 'b) seq =
        table.Table
        |> Map.toSeq
        |> Seq.sortBy fst
        |> Seq.map (fun (k, (_, v)) -> (k, v))

    let toList (table: OrderedMap<'a, 'b>) : ('a * 'b) list =
        table.Table
        |> Map.toList
        |> List.sortBy fst
        |> List.map (fun (k, (_, v)) -> (k, v))

    let fold (folder: 's -> 'a -> 'b -> 's) (state: 's) (table: OrderedMap<'a, 'b>) : 's =
        table
        |> toSeq
        |> Seq.fold (fun s (k, v) -> folder s k v) state

    let partition (predicate: 'a -> 'b -> bool) (table: OrderedMap<'a, 'b>) : (OrderedMap<'a, 'b> * OrderedMap<'a, 'b>) =
        table
        |> fold (fun (t1, t2) k v ->
            match predicate k v with
            | true -> (t1 |> add k v, t2)
            | false -> (t1, t2 |> add k v))
            (empty, empty)

    let pick (chooser: 'a -> 'b -> 'c option) (table: OrderedMap<'a, 'b>) : 'c =
        table
        |> toList
        |> List.find (fun (k, v) -> (chooser k v) |> Option.isSome)
        |> (fun (k, v) -> chooser k v)
        |> Option.get

    let tryPick (chooser: 'a -> 'b -> 'c option) (table: OrderedMap<'a, 'b>) : 'c option =
        table
        |> toList
        |> List.find (fun (k, v) -> (chooser k v) |> Option.isSome)
        |> (fun (k, v) -> chooser k v)

    let change (key: 'a) (f: 'b option -> 'b option) (table: OrderedMap<'a, 'b>) : OrderedMap<'a, 'b> =
        let v = table |> tryFind key
        let n = f v
        match n with
        | Some s -> table |> add key s
        | None -> table |> remove key

    let foldBack (folder: 'a -> 'b -> 's -> 's) (table: OrderedMap<'a, 'b>) (state: 's) : 's =
        Seq.foldBack (fun (k, v) s -> folder k v s) (table |> toSeq) state

    let forall (predicate: 'a -> 'b -> bool) (table: OrderedMap<'a, 'b>) : bool =
        table
        |> toSeq
        |> Seq.forall (fun (k, v) -> predicate k v)

    let iter (action: 'a -> 'b -> unit) (table: OrderedMap<'a, 'b>) : unit =
        table
        |> toSeq
        |> Seq.iter (fun (k, v) -> action k v)

    let toArray (table: OrderedMap<'a, 'b>) : ('a * 'b) [] =
        table.Table
        |> Map.toArray
        |> Array.sortBy fst
        |> Array.map (fun (k, (_, v)) -> (k, v))

    let filter (predicate: 'a -> 'b -> bool) (table: OrderedMap<'a, 'b>) : OrderedMap<'a, 'b> =
        table
        |> fold (fun s k v ->
            match predicate k v with
            | true -> s |> add k v
            | false -> s
            ) empty

    let map (mapping: 'a -> 'b -> 'c) (table: OrderedMap<'a, 'b>) : OrderedMap<'a, 'c> =
        table
        |> fold (fun s k v -> s |> add k (mapping k v)) empty

    let groupBy (projection: 'a -> 'b -> 'c) (table: OrderedMap<'a, 'b>) : OrderedMap<'c, OrderedMap<'a, 'b>> =
        table
        |> toList
        |> List.groupBy (fun (k, v) -> projection k v)
        |> List.fold (fun s (k, v) -> s |> add k (v |> ofList)) empty
