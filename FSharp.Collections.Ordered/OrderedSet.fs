//namespace Ordered
namespace FSharp.Collections.Ordered

type OrderedSet<'a when 'a : comparison> =
    { 
        Table: Map<'a, int>; 
        Index: int;
    }

module OrderedSet =
    let empty : OrderedSet<'a> =
        { Table = Map.empty; Index = 0; }

    let isEmpty (set: OrderedSet<'a>) : bool =
        set.Table |> Map.isEmpty

    let add (value: 'a) (set: OrderedSet<'a>) : OrderedSet<'a> =
        let table = set.Table
        match table |> Map.tryFind value with
        | Some _ -> set
        | None ->
            let index = set.Index
            { Table = table |> Map.add value index; Index = index + 1; }

    let remove (value: 'a) (set: OrderedSet<'a>) : OrderedSet<'a> =
        { set with Table = set.Table |> Map.remove value; }

    let singleton (value: 'a) : OrderedSet<'a> =
        empty |> add value

    let count (set: OrderedSet<'a>) : int =
        set.Table |> Map.count

    let contains (element: 'a) (set: OrderedSet<'a>) : bool =
        set.Table |> Map.containsKey element
        
    let exists (predicate: 'a -> bool) (set: OrderedSet<'a>) : bool =
        set.Table |> Map.exists (fun k _ -> predicate k)

    let filter (predicate: 'a -> bool) (set: OrderedSet<'a>) : OrderedSet<'a> =
        { set with Table = set.Table |> Map.filter (fun k _ -> predicate k); }

    let difference (set1: OrderedSet<'a>) (set2: OrderedSet<'a>) : OrderedSet<'a> =
        set1
        |> filter (fun x -> set2 |> contains x |> not)

    let toList (set: OrderedSet<'a>) : 'a list =
        set.Table
        |> Map.toList
        |> List.sortBy snd
        |> List.map fst

    let toArray (set: OrderedSet<'a>) : 'a [] =
        set.Table
        |> Map.toArray
        |> Array.sortBy snd
        |> Array.map fst

    let toSeq (set: OrderedSet<'a>) : 'a seq =
        set.Table
        |> Map.toSeq
        |> Seq.sortBy snd
        |> Seq.map fst

    let forall (predicate: 'a -> bool) (set: OrderedSet<'a>) : bool =
        set
        |> toList
        |> List.forall predicate

    let fold (folder: 's -> 't -> 's) (state: 's) (set: OrderedSet<'t>) : 's =
        set
        |> toList
        |> List.fold folder state

    let intersect (set1: OrderedSet<'a>) (set2: OrderedSet<'a>) : OrderedSet<'a> =
        set1
        |> filter (fun x -> set2 |> contains x)

    let union (set1: OrderedSet<'a>) (set2: OrderedSet<'a>) : OrderedSet<'a> =
        fold (fun s t -> s |> add t) set1 set2

    let intersectMany (sets: OrderedSet<'a> seq) : OrderedSet<'a> =
        let (h, t) = (sets |> Seq.head, sets |> Seq.tail)
        t
        |> Seq.fold (fun s i -> intersect s i) h 

    let unionMany (sets: OrderedSet<'a> seq) : OrderedSet<'a> =
        let (h, t) = (sets |> Seq.head, sets |> Seq.tail)
        t
        |> Seq.fold (fun s i -> union s i) h 

    let isSubset (set1: OrderedSet<'a>) (set2: OrderedSet<'a>) : bool =
        set1 |> forall (fun x -> set2 |> contains x)

    let isSuperset (set1: OrderedSet<'a>) (set2: OrderedSet<'a>) : bool =
        isSubset set2 set1

    let isProperSubset (set1: OrderedSet<'a>) (set2: OrderedSet<'a>) : bool =
        (isSubset set1 set2) && (isSubset set2 set1 |> not)

    let isProperSuperset (set1: OrderedSet<'a>) (set2: OrderedSet<'a>) : bool =
        (isSuperset set1 set2) && (isSuperset set2 set1 |> not)

    let iter (action: 'a -> unit) (set: OrderedSet<'a>) : unit =
        set
        |> toList
        |> List.iter action

    let maxElement (set: OrderedSet<'a>) : 'a =
        set
        |> toList
        |> List.max

    let minElement (set: OrderedSet<'a>) : 'a =
        set
        |> toList
        |> List.min

    let foldBack (folder: 't -> 's -> 's) (set: OrderedSet<'t>) (state: 's) : 's =
        List.foldBack folder (set |> toList) state

    let ofList (elements: 'a list) : OrderedSet<'a> =
        elements |> List.fold (fun s t -> s |> add t) empty

    let ofSeq (elements: 'a seq) : OrderedSet<'a> =
        elements |> Seq.fold (fun s t -> s |> add t) empty

    let ofArray (array: 'a []) : OrderedSet<'a> =
        array |> Array.fold (fun s t -> s |> add t) empty

    let map (mapping: 'a -> 'b) (set: OrderedSet<'a>) : OrderedSet<'b> =
        fold (fun s t -> s |> add (mapping t)) empty set

    let rev (set: OrderedSet<'a>) : OrderedSet<'a> =
        set
        |> toList
        |> List.rev
        |> ofList

    let partition (predicate: 'a -> bool) (set: OrderedSet<'a>) : (OrderedSet<'a> * OrderedSet<'a>) =
        set
        |> fold (fun (s1, s2) t ->
            match predicate t with
            | true -> (s1 |> add t, s2)
            | false -> (s1, s2 |> add t))
            (empty, empty)

    // 額外
    let head (set: OrderedSet<'a>) : 'a =
        set
        |> toSeq
        |> Seq.head

    // 額外
    let tail (set: OrderedSet<'a>) : OrderedSet<'a> =
        let h = set |> head
        set
        |> remove h
