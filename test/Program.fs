// 在 http://fsharp.org 上了解有关 F# 的详细信息
// 请参阅“F# 教程”项目以获取更多帮助。
open FsCheck
open FSharpx.Collections
open OrderedCollection

type OrderedSetProperties =
    class
        static member ``toList ofList`` (xs : list<int>) =
            let list = xs |> OrderedSet.ofList in
            list = (list |> OrderedSet.toList |> OrderedSet.ofList)
        static member ``add`` (xs : list<int>, value : int) =
            xs @ (value |> List.singleton) |> OrderedSet.ofList = (xs |> OrderedSet.ofList |> OrderedSet.add value)
        static member ``equal`` (xs : list<int>) =
            OrderedSet.ofList xs = OrderedSet.ofList xs
        static member ``isEmpty`` (xs : list<int>) =
            xs |> OrderedSet.ofList |> OrderedSet.isEmpty = (xs |> List.isEmpty)
        static member ``contains`` (xs : list<int>, value : int) =
            xs |> OrderedSet.ofList |> OrderedSet.contains value = (xs |> List.contains value)
        static member ``count`` (xs : list<int>) =
            let xs = xs |> List.distinct
            xs |> OrderedSet.ofList |> OrderedSet.count = (xs |> List.length)
        static member ``fold`` (xs : list<int>) =
            let xs = xs |> List.distinct
            xs |> OrderedSet.ofList |> OrderedSet.fold (+) 0 = (xs |> List.fold (+) 0)
        static member ``remove`` (xs : list<int>, v : int) =
            let xs = xs |> List.distinct
            let a = xs |> OrderedSet.ofList |> OrderedSet.remove v
            let b = xs |> List.filter ((<>) v) |> OrderedSet.ofList
            a = b
        static member ``difference`` (xs : list<int>, ys : list<int>) =
            let xs = xs |> List.distinct
            let ys = ys |> List.distinct
            let x = xs |> OrderedSet.ofList
            let y = ys |> OrderedSet.ofList
            (OrderedSet.difference x y) |> OrderedSet.toList = (xs |> List.filter (fun x -> ys |> List.forall ((<>) x)))
        static member ``exists`` (xs : list<int>, predicate : int -> bool) =
            let xs = xs |> List.distinct
            let s = xs |> OrderedSet.ofList
            xs |> List.exists predicate = (s |> OrderedSet.exists predicate)
        static member ``filter`` (xs : list<int>, predicate : int -> bool) =
            let xs = xs |> List.distinct
            let s = xs |> OrderedSet.ofList
            xs |> List.filter predicate |> OrderedSet.ofList = (s |> OrderedSet.filter predicate)
        static member ``forall`` (xs : list<int>, predicate : int -> bool) =
            let xs = xs |> List.distinct
            let s = xs |> OrderedSet.ofList
            xs |> List.forall predicate = (s |> OrderedSet.forall predicate)
        static member ``isSubset`` (xs1 : list<int>, xs2 : list<int>) =
            let xs1 = xs1 |> List.distinct
            let s1 = xs1 |> OrderedSet.ofList
            let xs2 = xs2 |> List.distinct
            let s2 = xs2 |> OrderedSet.ofList
            (xs1 |> List.forall (fun x -> xs2 |> List.exists ((=) x))) = (OrderedSet.isSubset s1 s2)
        static member ``isProperSubset`` (xs1 : list<int>, xs2 : list<int>) =
            let xs1 = xs1 |> List.distinct
            let s1 = xs1 |> OrderedSet.ofList
            let xs2 = xs2 |> List.distinct
            let s2 = xs2 |> OrderedSet.ofList
            match xs1.Length < xs2.Length with
            | true -> (xs1 |> List.forall (fun x -> xs2 |> List.exists ((=) x))) = (OrderedSet.isProperSubset s1 s2)
            | false -> true
        static member ``isSuperset`` (xs1 : list<int>, xs2 : list<int>) =
            let xs1 = xs1 |> List.distinct
            let s1 = xs1 |> OrderedSet.ofList
            let xs2 = xs2 |> List.distinct
            let s2 = xs2 |> OrderedSet.ofList
            (xs2 |> List.forall (fun x -> xs1 |> List.exists ((=) x))) = (OrderedSet.isSuperset s1 s2)
        static member ``isProperSuperset`` (xs1 : list<int>, xs2 : list<int>) =
            let xs1 = xs1 |> List.distinct
            let s1 = xs1 |> OrderedSet.ofList
            let xs2 = xs2 |> List.distinct
            let s2 = xs2 |> OrderedSet.ofList
            match xs1.Length > xs2.Length with
            | true -> (xs2 |> List.forall (fun x -> xs1 |> List.exists ((=) x))) = (OrderedSet.isSuperset s1 s2)
            | false -> true
        static member ``map`` (xs : list<int>, predicate : int -> bool) =
            let xs = xs |> List.distinct |> List.map predicate
            let s = xs |> OrderedSet.ofList
            xs |> OrderedSet.ofList = s
        static member ``maxElement`` (xs : list<int>) =
            match List.isEmpty xs with
            | true -> true
            | false -> 
                xs |> List.max = (xs |> OrderedSet.ofList |> OrderedSet.maxElement)
        static member ``minElement`` (xs : list<int>) =
            match List.isEmpty xs with
            | true -> true
            | false -> 
                xs |> List.min = (xs |> OrderedSet.ofList |> OrderedSet.minElement)
        static member ``ofArray`` (xs : int []) =
            let xs = xs |> Array.distinct
            xs = (xs |> OrderedSet.ofArray |> OrderedSet.toArray)
        static member ``ofList`` (xs : int list) =
            let xs = xs |> List.distinct
            xs = (xs |> OrderedSet.ofList |> OrderedSet.toList)
        static member ``ofSeq`` (xs : int list) =
            let xs = xs |> List.distinct |> List.toSeq
            xs |> Seq.toList = (xs |> OrderedSet.ofSeq |> OrderedSet.toList)
        static member ``partition`` (xs : list<int>, predicate : int -> bool) =
            let xs = xs |> List.distinct
            let (x, y) = xs |> List.partition predicate
            (x |> OrderedSet.ofList, y |> OrderedSet.ofList) = (xs |> OrderedSet.ofList |> OrderedSet.partition predicate)
        static member ``singleton`` (v : int) =
            v |> OrderedSet.singleton = (OrderedSet.empty |> OrderedSet.add v)
        static member ``foldBack`` (xs : int list) =
            let xs = xs |> List.distinct |> List.map string
            OrderedSet.foldBack (+) (xs |> OrderedSet.ofList) "0" = (List.foldBack (+) xs "0")
        static member ``toList`` (xs : int list) =
            let xs = xs |> List.distinct
            xs |> OrderedSet.ofList |> OrderedSet.toList = xs
        static member ``toArray`` (xs : int []) =
            let xs = xs |> Array.distinct
            xs |> OrderedSet.ofArray |> OrderedSet.toArray = xs
        static member ``toSeq`` (xs : int list) =
            let xs = xs |> List.distinct |> List.toSeq
            xs |> OrderedSet.ofSeq |> OrderedSet.toList = (xs |> Seq.toList)
        static member ``union`` (xs : int list, ys : int list) =
            let ls = xs @ ys
            ls |> OrderedSet.ofList = OrderedSet.union (xs |> OrderedSet.ofList) (ys |> OrderedSet.ofList)
        static member ``unionMany`` (xs : int list list) =
            let ys = xs |> List.toSeq |> List.concat
            ys |> OrderedSet.ofList = (xs |> List.map OrderedSet.ofList |> List.toSeq |> OrderedSet.unionMany)
    end

let toAssocList (source : ('k * 'v) list) =
    let m = source |> Map.ofList
    m
    |> Map.keys 
    |> List.ofSeq 
    |> List.map (fun x -> (x, m.[x]))

let toOrderedMap (source : ('k * 'v) list) =
    source
    |> toAssocList
    |> OrderedMap.ofList

let assocListConj (k : 'k) (v : 'v) (source : ('k * 'v) list) =
    match source |> List.exists (fun (a, _) -> a = k) with
    | true ->
        source |> List.map (fun (a, b) ->
                                if a = k then
                                    (k, v)
                                else
                                    (a, b))
    | false ->
        source @ (List.singleton (k, v))

type OrderedMapProperties =
    class
        static member ofList (xs : Map<int, int>) =
            let keys = xs |> Map.keys |> List.ofSeq in
            (keys |> List.map (fun x -> x, xs.[x]) |> OrderedMap.ofList |> OrderedMap.toList) = (keys |> List.map (fun x -> x, xs.[x]))
        static member toList (xs : (int * int) list) =
            let k = toAssocList xs in 
            (k |> OrderedMap.ofList |> OrderedMap.toList) = k
        static member add (xs : (int * int) list, k : int, v : int) =
            let keys = toAssocList xs in 
            keys |> assocListConj k v |> OrderedMap.ofList = (keys |> OrderedMap.ofList |> OrderedMap.add k v)
        static member isEmpty (xs : (int * int) list) =
            let keys = toAssocList xs in 
            xs |> OrderedMap.ofList |> OrderedMap.isEmpty = (keys |> List.isEmpty)
        static member containsKey (xs : (int * int) list, key : int) =
            xs |> List.exists (fun (k, _) -> k = key) = (xs |> OrderedMap.ofList |> OrderedMap.containsKey key)
        static member count (xs : (int * int) list) =
            let keys = toAssocList xs in 
            keys |> OrderedMap.ofList |> OrderedMap.count = (keys |> List.length)
        static member fold (xs : (string * string) list) =
            let keys = toAssocList xs in
            (keys |> List.fold (fun s (k, v) -> s + k + v) "") = (keys |> OrderedMap.ofList |> OrderedMap.fold (fun s k v -> s + k + v) "")
        static member foldBack (xs : (string * string) list) =
            let keys = toAssocList xs in 
            (List.foldBack (fun (k, v) s -> k + v + s) keys "") = (OrderedMap.foldBack (fun k v s -> k + v + s) (keys |> OrderedMap.ofList) "")
        static member remove (xs : (int * int) list, key : int) =
            let keys = toAssocList xs in 
            (keys |> List.filter (fun (k, _) -> k <> key) |> OrderedMap.ofList) = (keys |> OrderedMap.ofList |> OrderedMap.remove key)
        static member exists (xs : (int * int) list, f : int -> int -> bool) =
            let keys = toAssocList xs in 
            keys |> List.exists (fun (k, v) -> f k v) = (keys |> OrderedMap.ofList |> OrderedMap.exists f)
        static member filter (xs : (int * int) list, f : int -> int -> bool) =
            let keys = toAssocList xs in 
            keys |> List.filter (fun (k, v) -> f k v) |> OrderedMap.ofList = (keys |> OrderedMap.ofList |> OrderedMap.filter f)
        static member find (xs : (int * int) list, key : int) =
            let keys = toAssocList xs
            match keys |> List.exists (fun (k, _) -> k = key) with
            | true ->
                let (_, value) = keys |> List.find (fun (k, _) -> k = key) in
                value = (keys |> OrderedMap.ofList |> OrderedMap.find key)
            | false ->
                true
        static member tryFind (xs : (int * int) list, key : int) =
            let keys = toAssocList xs
            let f (k, _) = k = key
            keys |> List.tryFind f |> Option.map (fun (_, v) -> v) = (keys |> OrderedMap.ofList |> OrderedMap.tryFind key)
        static member findKey (xs : (int * int) list, f : int -> int -> bool) =
            let keys = toAssocList xs in
            match keys |> List.exists (fun (k, v) -> f k v) with
            | true ->
                let (key, _) = keys |> List.find (fun (k, v) -> f k v) in 
                key = (keys |> OrderedMap.ofList |> OrderedMap.findKey f)
            | false ->
                true
        static member tryFindKey (xs : (int * int) list, f : int -> int -> bool) =
            let keys = toAssocList xs in
            keys |> List.tryFind (fun (k, v) -> f k v) |> Option.map (fun (k, _) -> k) = (keys |> OrderedMap.ofList |> OrderedMap.tryFindKey f)
        static member iter (xs : (string * string) list) =
            let keys = toAssocList xs in
            let mutable str = "" in
            keys 
            |> OrderedMap.ofList 
            |> OrderedMap.iter (fun k v ->
                str <- str + k + v)
            |> ignore
            (keys |> List.fold (fun s (k, v) -> s + k + v) "") = str
        static member forall (xs : (int * int) list, f : int -> int -> bool) =
            let keys = toAssocList xs in
            (keys |> List.forall (fun (k, v) -> f k v)) = (keys |> OrderedMap.ofList |> OrderedMap.forall f)
        static member map (xs : (int * int) list, f : int -> int -> bool) =
            let keys = toAssocList xs in 
            (keys |> List.map (fun (k, v) -> k, f k v)) = (keys |> OrderedMap.ofList |> OrderedMap.map f |> OrderedMap.toList)
        static member ofArray (xs : (int * int) list) =
            let keys = toAssocList xs in
            (keys |> OrderedMap.ofList) = (keys |> Array.ofList |> OrderedMap.ofArray)
        static member ofSeq (xs : (int * int) list) =
            let keys = toAssocList xs in
            (keys |> OrderedMap.ofList) = (keys |> Seq.ofList |> OrderedMap.ofSeq)
        static member partition (xs : (int * int) list, f : int -> int -> bool) =
            let keys = toAssocList xs in
            let (a, b) = (keys |> OrderedMap.ofList |> OrderedMap.partition f)
            (keys |> List.partition (fun (k, v) -> f k v)) = (OrderedMap.toList a, OrderedMap.toList b)
        static member toArray (xs : (int * int) list) =
            let keys = toAssocList xs in
            (keys |> List.toArray) = (keys |> OrderedMap.ofList |> OrderedMap.toArray)
        static member toSeq (xs : (int * int) list) =
            let keys = toAssocList xs in
            (keys |> List.toSeq) = (keys |> OrderedMap.ofList |> OrderedMap.toSeq)
        static member updateWith (xs : (int * int) list, f : int -> int option, key : int) =
            let m = xs |> OrderedMap.ofList in
            let ra =
                match m |> OrderedMap.tryFind key with
                | Some(value) ->
                    let m = m |> OrderedMap.remove key in
                    match f value with
                    | Some(v) ->
                        m
                        |> OrderedMap.add key v
                    | None -> m
                | None -> m
            in
            ra = (m |> OrderedMap.updateWith f key)
        //static member remove (list : (int * int) list) =
        //    let list = list |> toAssocList in 
        //    match list with 
        //    | [] -> true
        //    | [_] -> true
        //    | _ ->
        //        printfn "%A" list
        //        let (k, _) = list |> List.head in 
        //        let m = list |> OrderedMap.ofList in 
        //        (m |> OrderedMap.remove k |> OrderedMap.toList) = (list |> List.filter (fun (a, b) -> a = k)) 
    end

type OrderedMultiMapProperties =
    class 
        //static member add (xs : (int * int) list) (k : int) (v : int) =
            
        //static member ofList (xs : Map<int, int>) =
        //    let keys = xs |> Map.keys |> List.ofSeq in
        //    (keys |> List.map (fun x -> x, xs.[x]) |> OrderedMap.ofList |> OrderedMap.toList) = (keys |> List.map (fun x -> x, xs.[x]))
        static member isEmpty (xs : (int * int) list) =
            let keys = toAssocList xs in 
            xs 
            |> OrderedMultiMap.ofList 
            |> OrderedMultiMap.isEmpty = (keys |> List.isEmpty)
    end

let printApply f =
    (fun x ->
        printfn "%A" x
        f x)

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    Check.QuickAll<OrderedSetProperties>()
    Check.QuickAll<OrderedMapProperties>()
    Check.QuickAll<OrderedMultiMapProperties>();
    let ls = [(1, 2);(2, 3);(3, 4)] |> OrderedMap.ofList |> OrderedMap.add 1 1 |> OrderedMap.toList
    //let ls = [(1, 2);(2, 3);(3, 4)] |> OrderedMap.ofList |> OrderedMap.toList
    printfn "%A" ls
    //printfn "%A" ([(1, 1); (2, 2); (3, 3)]
    //              |> OrderedMap.ofList
    //              |> (fun x ->
    //                printfn "before remove %A %A %A" x x.First x.Last
    //                x |> OrderedMap.remove 2)
    //              |> (fun x ->
    //                printfn "after remove %A %A %A" x x.First x.Last
    //                //printfn "first last %A %A" x.First.Value x.Last.Value
    //                x |> OrderedMap.toList)
    //              )
    let rl = (OrderedMap.empty
                  |> OrderedMap.add 1 1 
                  |> OrderedMap.add 2 2
                  |> OrderedMap.add 3 3
                  |> OrderedMap.add 4 4
                  )
    printfn "%A" rl
    printfn "%A" (rl |> OrderedMap.remove 2)

    printfn "OrderedMultiMap"
    printfn "%A" (OrderedMultiMap.empty
                  |> OrderedMultiMap.add 1 1
                  |> OrderedMultiMap.add 2 1
                  |> OrderedMultiMap.add 3 1
                  |> OrderedMultiMap.toList)
    0 // 返回整数退出代码
