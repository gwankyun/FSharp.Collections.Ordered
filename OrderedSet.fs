namespace OrderedCollection
open FSharpx.Collections
//open FSharpx.Functional
//open FSharpx.Functional.Prelude

//[<CustomEquality; NoComparison>]
type OrderedSet<[<EqualityConditionalOn>] 'k when 'k : comparison>(first : 'k option, map : Map<'k, ('k * 'k)>, last : 'k option) =
//type OrderedSet<'k when 'k : comparison>(first : 'k option, map : Map<'k, ('k * 'k)>, last : 'k option) =
    member x.First = first
    member x.Map = map
    member x.Last = last
    //static member op_Equality (x : OrderedSet<'k>, y : OrderedSet<'k>) =
    //    x.First = y.First && x.Last = y.Last && x.Map = y.Map
    //static member op_Inequality (x : OrderedSet<'k>, y : OrderedSet<'k>) =
    //    x.First <> y.First || x.Last <> y.Last || x.Map = y.Map
    override x.GetHashCode() = hash x.Map
    override x.Equals(y : obj) =
        match y with
        | :? OrderedSet<'k> as y -> (x.First = y.First && x.Last = y.Last && x.Map = y.Map) || (x.Map |> Map.isEmpty && y.Map |> Map.isEmpty)
        | _ -> false
    interface System.IComparable with
        member this.CompareTo(o : obj) = 0
            

module OrderedSet =
    let flip f a b = f b a

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
                match (prev = value, next = value) with
                | (true, true) ->
                    //OrderedSet(Some first, map, Some last)
                    empty
                | (true, _) -> // 第一個元素
                    let first = next // 第二個
                    let map =
                        map
                        |> Map.updateWith (fun (_, n) -> Some (next, n)) next // 將前節點改爲自己
                    OrderedSet(Some first, map, Some last)
                | (_, true) -> // 最后一个元素
                    let last = prev // 
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

    let difference (set1 : OrderedSet<'k>) (set2 : OrderedSet<'k>) =
        fold (flip remove) set1 set2
        
    let exists (predicate : 'k -> bool) (set : OrderedSet<'k>) =
        set.Map |> Map.exists (fun k _ -> predicate k)
        
    let filter (predicate : 'k -> bool) (set : OrderedSet<'k>) =
        fold (fun s i ->
            match (predicate i) with
            | true -> s |> add i
            | false -> s)
            empty set
            
    let iter (action : 'k -> unit) (set : OrderedSet<'k>) =
        fold (fun a b ->
            action b 
            a) empty set

    let forall (predicate : 'k -> bool) (set : OrderedSet<'k>) =
        set.Map |> Map.forall (fun k _ -> predicate k)
        
    let intersect (set1 : OrderedSet<'k>) (set2 : OrderedSet<'k>) =
        set1
        |> filter (flip exists set2 << (=))
        
    let intersectMany (sets : OrderedSet<'k> seq) =
        sets
        |> Seq.reduce intersect
        
    let isSubset (set1 : OrderedSet<'k>) (set2 : OrderedSet<'k>) =
        set1 |> forall (flip exists set2 << (=))
        
    let isProperSubset (set1 : OrderedSet<'k>) (set2 : OrderedSet<'k>) =
        isSubset set1 set2 &&
        (set2 |> count) > (set1 |> count)
        
    let isSuperset (set1 : OrderedSet<'k>) (set2 : OrderedSet<'k>) =
        set2 |> forall (flip exists set1 << (=))
        
    let isProperSuperset (set1 : OrderedSet<'k>) (set2 : OrderedSet<'k>) =
        isSuperset set1 set2 &&
        (set2 |> count) < (set1 |> count)

    let map (mapping : 'a -> 'b) (set : OrderedSet<'a>) =
        fold (fun s i -> s |> add (mapping i)) empty set
        
    let maxElement (set : OrderedSet<'a>) =
        set.Map |> Map.keys |> Seq.max

    let minElement (set : OrderedSet<'a>) =
        set.Map |> Map.keys |> Seq.min
        
    let ofArray (array : 'k []) =
        Array.fold (fun s i -> s |> add i) empty array
        
    let ofList (elements : 'k list) =
        List.fold (fun s i -> s |> add i) empty elements

    let ofSeq (elements : 'k seq) =
        Seq.fold (fun s i -> s |> add i) empty elements

    let partition (predicate : 'k -> bool) (set : OrderedSet<'k>) =
        fold (fun (set1, set2) t ->
            match predicate t with
            | true -> (set1 |> add t, set2)
            | false -> (set1, set2 |> add t)
        ) (empty, empty) set
        
    let singleton (value : 'k) =
        empty |> add value

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

    let toList (set : OrderedSet<'k>) =
        foldBack List.cons set []

    let toArray (set : OrderedSet<'k>) =
        set
        |> toList
        |> List.toArray

    let toSeq (set : OrderedSet<'k>) =
        set
        |> toList
        |> List.toSeq
        
    let union (set1 : OrderedSet<'k>) (set2 : OrderedSet<'k>) =
        set2
        |> fold (fun s i -> s |> add i) set1
        
    let unionMany (sets : OrderedSet<'k> seq) =
        match sets |> Seq.isEmpty with
        | true -> empty
        | false -> sets |> Seq.reduce union
            