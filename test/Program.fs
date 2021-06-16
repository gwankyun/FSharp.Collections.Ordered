open System
open FsCheck
open FSharp.Collections.Ordered

let flip (f: 'a -> 'b -> 'c) : ('b -> 'a -> 'c) =
    (fun a b -> f b a)

module List =
    let headOr (value: 'a) (list: 'a list) : 'a =
        match list |> List.tryHead with
        | Some h -> h
        | None -> value

    let tailOr (value: 'a list) (list: 'a list) : 'a list =
        match list |> List.isEmpty with
        | true -> value
        | false -> list |> List.tail

    let isSubset (set1: int list) (set2: int list) =
         set1 |> List.forall (set2 |> flip List.contains)

    let isSuperset (set1: int list) (set2: int list) =
         set2 |> List.forall (set1 |> flip List.contains)

    let isProperSubset (set1: int list) (set2: int list) =
        (isSubset set1 set2) && (isSubset set2 set1 |> not)

    let isProperSuperset (set1: int list) (set2: int list) =
        (isSuperset set1 set2) && (isSuperset set2 set1 |> not)

type CheckSet() =
    static member ``[ofList toList]`` (elements: int list) =
        let a = elements |> OrderedSet.ofList |> OrderedSet.toList
        let b = elements |> List.distinct
        a = b

    static member ``[ofArray toArray]`` (elements: int []) =
        let a = elements |> OrderedSet.ofArray |> OrderedSet.toArray
        let b = elements |> Array.distinct
        a = b

    //static member ``[ofSeq toSeq]`` (elements: int seq) =
    //    let a = elements |> OrderedSet.ofSeq |> OrderedSet.toList
    //    let b = elements |> Seq.distinct |> Seq.toList
    //    a = b

    static member isEmpty (elements: int list) =
        let a = elements |> OrderedSet.ofList |> OrderedSet.isEmpty
        let b = elements |> List.isEmpty
        a = b

    static member add (v: int) =
        OrderedSet.empty |> OrderedSet.add v |> OrderedSet.toList = [v]

    static member ofList (elements: int list) =
        let a = elements |> OrderedSet.ofList |> OrderedSet.toList
        let b = (List.fold (flip OrderedSet.add) OrderedSet.empty elements) |> OrderedSet.toList
        a = b

    static member remove (elements: int list) (v: int) =
        let a = elements |> OrderedSet.ofList |> OrderedSet.remove v |> OrderedSet.toList
        let b = elements |> List.distinct |> List.filter (fun x -> x <> v)
        a = b

    static member map (elements: int list) (mapping: int -> int) =
        let a = elements |> OrderedSet.ofList |> OrderedSet.map mapping |> OrderedSet.toList
        let b = elements |> List.distinct |> List.map mapping |> List.distinct
        a = b

    static member singleton (value: int) =
        let a = value |> OrderedSet.singleton |> OrderedSet.toList
        a = [value]

    static member count (elements: int list) =
        let a = elements |> List.distinct |> List.length
        let b = elements |> OrderedSet.ofList |> OrderedSet.count
        a = b

    static member contains (element: int) (elements: int list) =
        let a = elements |> OrderedSet.ofList |> OrderedSet.contains element
        let b = elements |> List.contains element
        a = b

    static member exists (predicate: int -> bool) (elements: int list) =
        let a = elements |> OrderedSet.ofList |> OrderedSet.exists predicate
        let b = elements |> List.exists predicate
        a = b

    static member filter (predicate: int -> bool) (elements: int list) =
        let a = elements |> OrderedSet.ofList |> OrderedSet.filter predicate |> OrderedSet.toList
        let b = elements |> List.filter predicate |> List.distinct
        a = b

    static member forall (predicate: int -> bool) (elements: int list) =
        let a = elements |> OrderedSet.ofList |> OrderedSet.forall predicate
        let b = elements |> List.forall predicate
        a = b

    static member difference (set1: int list) (set2: int list) =
        let a =
            let s1 = set1 |> OrderedSet.ofList
            let s2 = set2 |> OrderedSet.ofList
            (OrderedSet.difference s1 s2) |> OrderedSet.toList
        let b = (set1 |> List.distinct) |> List.filter (fun x -> set2 |> List.contains x |> not)
        a = b

    static member fold (folder: int -> int -> int) (state: int) (elements: int list) =
        let a =
            let s = elements |> OrderedSet.ofList
            OrderedSet.fold folder state s
        let b =
            let ls = elements |> List.distinct
            List.fold folder state ls
        a = b

    static member foldBack (folder: int -> int -> int) (elements: int list) (state: int)  =
        let a =
            let s = elements |> OrderedSet.ofList
            OrderedSet.foldBack folder s state
        let b =
            let ls = elements |> List.distinct
            List.foldBack folder ls state
        a = b

    static member intersect (set1: int list) (set2: int list) =
        let a =
            let s1 = set1 |> OrderedSet.ofList
            let s2 = set2 |> OrderedSet.ofList
            (OrderedSet.intersect s1 s2) |> OrderedSet.toList
        let b =
            set1 |> List.distinct |> List.filter (set2 |> flip List.contains)
        a = b

    static member union (set1: int list) (set2: int list) =
        let a =
            let s1 = set1 |> OrderedSet.ofList
            let s2 = set2 |> OrderedSet.ofList
            (OrderedSet.union s1 s2) |> OrderedSet.toList
        let b =
            (List.append set1 set2) |> List.distinct
        a = b

    static member unionMany (set: int list list) =
        let a =
            set |> List.map OrderedSet.ofList |> List.toSeq |> OrderedSet.unionMany |> OrderedSet.toList
        let b =
            let s = set |> List.map List.distinct
            let (h, t) = (set |> List.headOr [], set |> List.tailOr [])
            let ls = List.fold List.append h t
            ls |> List.distinct
        a = b

    static member intersectMany (set: int list list) =
        let a =
            set |> List.map OrderedSet.ofList |> List.toSeq |> OrderedSet.intersectMany |> OrderedSet.toList
        let b =
            let s = set |> List.map List.distinct
            let (h, t) = (set |> List.headOr [], set |> List.tailOr [])
            let ls = List.fold (fun s t ->
                let tContains = t |> flip List.contains
                s |> List.filter tContains) h t
            ls |> List.distinct
        a = b

    static member isSubset (set1: int list) (set2: int list) =
        let a = List.isSubset set1 set2
        let b =
            (OrderedSet.ofList set1, OrderedSet.ofList set2)
            ||> OrderedSet.isSubset
        a = b

    static member isSuperset (set1: int list) (set2: int list) =
        let a = List.isSuperset set1 set2
        let b =
            (OrderedSet.ofList set1, OrderedSet.ofList set2)
            ||> OrderedSet.isSuperset
        a = b

    static member isProperSubset (set1: int list) (set2: int list) =
        let a = List.isProperSubset set1 set2
        let b =
            (OrderedSet.ofList set1, OrderedSet.ofList set2)
            ||> OrderedSet.isProperSubset
        a = b

    static member isProperSuperset (set1: int list) (set2: int list) =
        let a = List.isProperSuperset set1 set2
        let b =
            (OrderedSet.ofList set1, OrderedSet.ofList set2)
            ||> OrderedSet.isProperSuperset
        a = b

    static member maxElement (set: int list) =
        match set |> List.isEmpty with
        | true -> true
        | false ->
            let a = List.max set
            let b = set |> OrderedSet.ofList |> OrderedSet.maxElement
            a = b

    static member minElement (set: int list) =
        match set |> List.isEmpty with
        | true -> true
        | false ->
            let a = List.min set
            let b = set |> OrderedSet.ofList |> OrderedSet.minElement
            a = b

//type CheckMap() =
//    static member ``[ofList toList]`` (elements: (int * int) list) =
//        let a = elements |> OrderedMap.ofList |> OrderedMap.toList
//        let b = elements |> List.distinct
//        a = b

[<EntryPoint>]
let main argv =
    Check.QuickAll(typeof<CheckSet>)
    0
