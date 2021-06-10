open System
open FsCheck
open FSharp.Collections.Ordered

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
    //    let a = elements |> Seq.init 100 |> Seq.toList |> OrderedSet.ofList |> OrderedSet.toList
    //    let b = elements |> Seq.distinct
    //    a = b

    static member isEmpty (elements: int list) =
        let a = elements |> OrderedSet.ofList |> OrderedSet.isEmpty
        let b = elements |> List.isEmpty
        a = b

    static member add (v: int) =
        OrderedSet.empty |> OrderedSet.add v |> OrderedSet.toList = [v]

    static member ofList (elements: int list) =
        let a = elements |> OrderedSet.ofList |> OrderedSet.toList
        let b = (List.fold (fun s t -> s |> OrderedSet.add t) OrderedSet.empty elements) |> OrderedSet.toList
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

    static member intersect (set1: int list) (set2: int list) =
        let a =
            let s1 = set1 |> OrderedSet.ofList
            let s2 = set2 |> OrderedSet.ofList
            (OrderedSet.intersect s1 s2) |> OrderedSet.toList
        let b =
            set1 |> List.distinct |> List.filter (fun x -> set2 |> List.contains x)
        a = b

    static member union (set1: int list) (set2: int list) =
        let a =
            let s1 = set1 |> OrderedSet.ofList
            let s2 = set2 |> OrderedSet.ofList
            (OrderedSet.union s1 s2) |> OrderedSet.toList
        let b =
            (List.append set1 set2) |> List.distinct
        a = b

[<EntryPoint>]
let main argv =
    Check.QuickAll(typeof<CheckSet>)
    0 // return an integer exit code