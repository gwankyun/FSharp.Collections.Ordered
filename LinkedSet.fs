namespace FSharp.Collections

open System.Collections.Generic
open System.Collections
open System
open Extension
open FSharp.Collections

type LinkedSet<'a when 'a : comparison>(x : LazyList<'a>, y : Set<'a>) = 
    
    override this.ToString() = 
        this.List()
        |> LazyList.filter (fun x -> this.Set() |> Set.contains x)
        |> Member.toString
    
    member this.Set() = y
    member this.List() = x

type LinkedMap<'a, 'b when 'a : comparison>(x : LazyList<'a>, y : Map<'a, 'b>) = 
    member this.List() = x
    member this.Map() = y
    static member Empty<'a, 'b when 'a : comparison and 'b : comparison>() = 
        LinkedMap<'a, 'b>(LazyList.empty, Map.empty)
    member this.Add(key : 'a, value : 'b) = 
        let a, b = this.List(), this.Map()
        let seq = a.Cons(key)
        LinkedMap(seq, b.Add(key, value))

type LinkedMultiMap<'a, 'b when 'a : comparison and 'b : comparison>(x : LinkedMap<'a, LinkedSet<'b>>) = 
    member this.LinkedMap() = x

module LinkedSet = 
    let add (value : 'a) (set : LinkedSet<'a>) = 
        let a, b = set.List(), set.Set()
        match Set.contains value b with
        | true -> LinkedSet(a, b)
        | false -> 
            let list = a.Cons(value)
            LinkedSet(list, b.Add(value))
    
    let contains (value : 'a) (set : LinkedSet<'a>) = Set.contains value (set.Set())
    
    let count (set : LinkedSet<'a>) = 
        let list = set.List()
        LazyList.length list
    
    let difference (set1 : LinkedSet<'a>) (set2 : LinkedSet<'a>) = 
        let set1 = set1.Set()
        let set2 = set2.Set()
        set1 - set2
    
    let empty<'a when 'a : comparison> = LinkedSet<'a>(LazyList.empty, Set.empty)
    
    let exists (predicate : 'a -> bool) (set : LinkedSet<'a>) = 
        let set = set.Set()
        Set.exists predicate set
    
    let toSeq (set : LinkedSet<'a>) = 
        let list = set.List()
        list |> LazyList.toSeq
    
    let filter (predicate : 'a -> bool) (set : LinkedSet<'a>) = 
        let list, set = set.List(), set.Set()
        LinkedSet(LazyList.filter predicate list, Set.filter predicate set)
    
    let fold (folder : 's -> 't -> 's) (state : 's) (set : LinkedSet<'t>) = 
        let list = set.List()
        list |> LazyList.fold folder state
    
    let foldBack (folder : 't -> 's -> 's) (set : LinkedSet<'t>) (state : 's) = 
        let list = set.List()
        LazyList.foldBack folder list state
    
    let forall (predicate : 'a -> bool) (set : LinkedSet<'a>) = 
        let set = set.Set()
        Set.forall predicate set
    
    let intersect (set1 : LinkedSet<'a>) (set2 : LinkedSet<'a>) = filter (fun x -> exists ((=) x) set2) set1
    
    let intersectMany (sets : LinkedSet<'a> seq) = 
        let head, tail = Seq.head sets, Seq.tail sets
        filter (fun x -> Seq.forall (exists ((=) x)) tail) head
    
    let isEmpty (set : LinkedSet<'a>) = 
        let set = set.Set()
        Set.isEmpty set
    
    let isProperSubset (set1 : LinkedSet<'a>) (set2 : LinkedSet<'a>) = 
        let set1, set2 = set1.Set(), set2.Set()
        Set.isProperSubset set1 set2
    
    let isProperSuperset (set1 : LinkedSet<'a>) (set2 : LinkedSet<'a>) = 
        let set1, set2 = set1.Set(), set2.Set()
        Set.isProperSuperset set1 set2
    
    let isSubset (set1 : LinkedSet<'a>) (set2 : LinkedSet<'a>) = 
        let set1, set2 = set1.Set(), set2.Set()
        Set.isSubset set1 set2
    
    let isSuperset (set1 : LinkedSet<'a>) (set2 : LinkedSet<'a>) = 
        let set1, set2 = set1.Set(), set2.Set()
        Set.isSuperset set1 set2
    
    let iter (action : 'a -> unit) (set : LinkedSet<'a>) = 
        let list = set.List()
        list |> LazyList.iter action
    
    let map (mapping : 'a -> 'b) (set : LinkedSet<'a>) = 
        let list, set = set.List(), set.Set()
        LinkedSet(LazyList.map mapping list, Set.map mapping set)
    
    let maxElement (set : LinkedSet<'a>) = 
        let set = set.Set()
        Set.maxElement set
    
    let minElement (set : LinkedSet<'a>) = 
        let set = set.Set()
        Set.minElement set
    
    let ofArray (array : 'a []) = 
        let list = LazyList.ofArray array
        let set = Set.ofList (list.List())
        LinkedSet(list, set)
    
    let ofList (elements : 'a list) = 
        let list = LazyList.ofList elements
        let set = Set.ofList (list.List())
        LinkedSet(list, set)
    
    let ofSeq (elements : 'a seq) = 
        let list = LazyList.ofSeq elements
        let set = Set.ofList (list.List())
        LinkedSet(list, set)
    
    let partition (predicate : 'a -> bool) (set : LinkedSet<'a>) = 
        let sq1, sq2 = 
            set
            |> toSeq
            |> Seq.partition predicate
        ofSeq sq1, ofSeq sq2
    
    let remove (value : 'a) (set : LinkedSet<'a>) = 
        let set = set.Set()
        Set.remove value set
    
    let singleton (value : 'a when 'a : comparison) = add value (LinkedSet<'a>(LazyList.empty, Set.empty))
    
    let toArray (set : LinkedSet<'a>) = 
        set
        |> toSeq
        |> Array.ofSeq
    
    let toList (set : LinkedSet<'a>) = 
        set
        |> toSeq
        |> List.ofSeq
    
    let union (set1 : LinkedSet<'a>) (set2 : LinkedSet<'a>) = 
        let mutable set : LinkedSet<'a> = empty
        for i in toSeq set1 do
            set <- add i set
        for i in toSeq set2 do
            set <- add i set
        set
    
    let unionMany (sets : LinkedSet<'a> seq) = Seq.fold union (Seq.head sets) (Seq.tail sets)
    let (+) (set1 : LinkedSet<'a>) (set2 : LinkedSet<'a>) = union set1 set2
    
    let (-) (set1 : LinkedSet<'a>) (set2 : LinkedSet<'a>) = 
        filter (fun x -> 
            set2
            |> exists ((=) x)
            |> not) set1
    
    let groupBy (projection : 'a -> 'key) (table : LinkedSet<'a>) = 
        table |> fold (fun (s : LinkedMap<'key, 'a>) t -> 
                     let key = projection t
                     s.Add(key, t)) (LinkedMap<'key, 'a>.Empty())
