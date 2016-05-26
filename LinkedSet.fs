namespace Original.Collections

open System.Collections.Generic
open System.Collections
open System
open Extension

type LinkedSet<'a when 'a : comparison>(x : LazyList<'a>, y : Set<'a>) = 
    member this.Set() = y
    member this.List() = x

module LinkedSet = 
    let add (value : 'a) (set : LinkedSet<'a>) = 
        let a, b = set.List(), set.Set()
        match Set.contains value b with
        | true -> LinkedSet(a, b)
        | false -> 
            let list = a.Cons(value)
            LinkedSet(list, b.Add(value))
    
    let contains (value : 'a) (set : LinkedSet<'a>) = Set.contains value (set.Set())
    let count (set : LinkedSet<'a>) = Set.count (set.Set())
    let difference (set1 : LinkedSet<'a>) (set2 : LinkedSet<'a>) = set1.Set() - set2.Set()
    let empty<'a when 'a : comparison> = LinkedSet<'a>(LazyList.empty, Set.empty)
    let exists (predicate : 'a -> bool) (set : LinkedSet<'a>) = Set.exists predicate (set.Set())
    let toSeq (set : LinkedSet<'a>) = set.List() |> LazyList.toSeq
    let filter (predicate : 'a -> bool) (set : LinkedSet<'a>) = 
        //        let a, b, c = toSeq set, set.Set(), set.List()
        LinkedSet(LazyList.filter predicate (set.List()), Set.filter predicate (set.Set()))
    let fold (folder : 's -> 't -> 's) (state : 's) (set : LinkedSet<'t>) = set.List() |> LazyList.fold folder state
    let foldBack (folder : 't -> 's -> 's) (set : LinkedSet<'t>) (state : 's) = 
        LazyList.foldBack folder (set.List()) state
    let forall (predicate : 'a -> bool) (set : LinkedSet<'a>) = Set.forall predicate (set.Set())
    let intersect (set1 : LinkedSet<'a>) (set2 : LinkedSet<'a>) = filter (fun x -> exists ((=) x) set2) set1
    
    let intersectMany (sets : LinkedSet<'a> seq) = 
        let head, tail = Seq.head sets, Seq.tail sets
        filter (fun x -> Seq.forall (exists ((=) x)) tail) head
    
    let isEmpty (set : LinkedSet<'a>) = Set.isEmpty (set.Set())
    let isProperSubset (set1 : LinkedSet<'a>) (set2 : LinkedSet<'a>) = Set.isProperSubset (set1.Set()) (set2.Set())
    let isProperSuperset (set1 : LinkedSet<'a>) (set2 : LinkedSet<'a>) = 
        Set.isProperSuperset (set1.Set()) (set2.Set())
    let isSubset (set1 : LinkedSet<'a>) (set2 : LinkedSet<'a>) = Set.isSubset (set1.Set()) (set2.Set())
    let isSuperset (set1 : LinkedSet<'a>) (set2 : LinkedSet<'a>) = Set.isSuperset (set1.Set()) (set2.Set())
    let iter (action : 'a -> unit) (set : LinkedSet<'a>) = set.List() |> LazyList.iter action
    let map (mapping : 'a -> 'b) (set : LinkedSet<'a>) = 
        LinkedSet(LazyList.map mapping (set.List()), Set.map mapping (set.Set()))
    let maxElement (set : LinkedSet<'a>) = Set.maxElement (set.Set())
    let minElement (set : LinkedSet<'a>) = Set.minElement (set.Set())
    
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
        let sq1, sq2 = (set |> toSeq).Value |> Seq.partition predicate
        ofSeq sq1, ofSeq sq2
    
    let remove (value : 'a) (set : LinkedSet<'a>) = Set.remove value (set.Set())
    let singleton (value : 'a when 'a : comparison) = add value (LinkedSet<'a>(LazyList.empty, Set.empty))
    let toArray (set : LinkedSet<'a>) = (set |> toSeq).Value |> Array.ofSeq
    let toList (set : LinkedSet<'a>) = (set |> toSeq).Value |> List.ofSeq
    
    let union (set1 : LinkedSet<'a>) (set2 : LinkedSet<'a>) = 
        let mutable set : LinkedSet<'a> = empty
        for i in (toSeq set1).Value do
            set <- add i set
        for i in (toSeq set2).Value do
            set <- add i set
        set
    
    let unionMany (sets : LinkedSet<'a> seq) = Seq.fold union (Seq.head sets) (Seq.tail sets)
    let (+) (set1 : LinkedSet<'a>) (set2 : LinkedSet<'a>) = union set1 set2
    
    let (-) (set1 : LinkedSet<'a>) (set2 : LinkedSet<'a>) = 
        filter (fun x -> 
            set2
            |> exists ((=) x)
            |> not) set1
