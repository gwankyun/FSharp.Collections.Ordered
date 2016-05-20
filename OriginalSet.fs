namespace Original.Collections

open System.Collections.Generic
open System.Collections
open System
open Extension

type OriginalSet<'a when 'a : comparison>(x : LazyList<'a>, y : Set<'a>) = 
    member this.Set() = y
    member this.List() = x

//    interface System.Collections.IEnumerable with
//        member this.GetEnumerator() =
//            (this.Seq()
//    interface System.Collections.IEnumerable with
//        member this.GetEnumerator() =
//            this.Seq().GetEnumerator()
//    interface System.IComparable with
//        member this.CompareTo(that : obj) =
//            let that = that :?> OriginalSet<'a>
//            compare (this.Seq()) (that.Seq())
//    override Object.Equals(that : obj) =
module OriginalSet = 
    let add (value : 'a) (set : OriginalSet<'a>) = 
        let a, b = set.List(), set.Set()
        match Set.contains value b with
        | true -> OriginalSet(a, b)
        | false -> 
            let list = a.Cons(value)
            OriginalSet(list, b.Add(value))
    
    let contains (value : 'a) (set : OriginalSet<'a>) = Set.contains value (set.Set())
    let count (set : OriginalSet<'a>) = Set.count (set.Set())
    let difference (set1 : OriginalSet<'a>) (set2 : OriginalSet<'a>) = set1.Set() - set2.Set()
    let empty<'a when 'a : comparison> = OriginalSet<'a>(LazyList.empty, Set.empty)
    let exists (predicate : 'a -> bool) (set : OriginalSet<'a>) = Set.exists predicate (set.Set())
    let toSeq (set : OriginalSet<'a>) = set.List() |> LazyList.toSeq
    let filter (predicate : 'a -> bool) (set : OriginalSet<'a>) = 
        //        let a, b, c = toSeq set, set.Set(), set.List()
        OriginalSet(LazyList.filter predicate (set.List()), Set.filter predicate (set.Set()))
    let fold (folder : 's -> 't -> 's) (state : 's) (set : OriginalSet<'t>) = set.List() |> LazyList.fold folder state
    //        |> List.ofSeq
    //        |> Seq.ofList
    //        |> Seq.fold folder state
    //        let a = set |> toSeq |> Seq.fold folder state
    //        let b = Set.ofSeq a
    //        OriginalSet(a, b)
    let foldBack (folder : 't -> 's -> 's) (set : OriginalSet<'t>) (state : 's) = 
        LazyList.foldBack folder (set.List()) state
    //        set.List()
    //        |> List.ofSeq
    //        |> Seq.ofList
    //        |> (fun x -> Seq.foldBack folder x state)
    //        let a = Seq.foldBack folder (toSeq set) state
    //        let b = Set.ofSeq a
    //        OriginalSet(a, b)
    let forall (predicate : 'a -> bool) (set : OriginalSet<'a>) = Set.forall predicate (set.Set())
    let intersect (set1 : OriginalSet<'a>) (set2 : OriginalSet<'a>) = filter (fun x -> exists ((=) x) set2) set1
    
    let intersectMany (sets : OriginalSet<'a> seq) = 
        let head, tail = Seq.head sets, Seq.tail sets
        filter (fun x -> Seq.forall (exists ((=) x)) tail) head
    
    let isEmpty (set : OriginalSet<'a>) = Set.isEmpty (set.Set())
    let isProperSubset (set1 : OriginalSet<'a>) (set2 : OriginalSet<'a>) = Set.isProperSubset (set1.Set()) (set2.Set())
    let isProperSuperset (set1 : OriginalSet<'a>) (set2 : OriginalSet<'a>) = 
        Set.isProperSuperset (set1.Set()) (set2.Set())
    let isSubset (set1 : OriginalSet<'a>) (set2 : OriginalSet<'a>) = Set.isSubset (set1.Set()) (set2.Set())
    let isSuperset (set1 : OriginalSet<'a>) (set2 : OriginalSet<'a>) = Set.isSuperset (set1.Set()) (set2.Set())
    
    let iter (action : 'a -> unit) (set : OriginalSet<'a>) = 
        set.List()
        |> LazyList.iter action
    
    let map (mapping : 'a -> 'b) (set : OriginalSet<'a>) = 
        OriginalSet(LazyList.map mapping (set.List()), Set.map mapping (set.Set()))
    let maxElement (set : OriginalSet<'a>) = Set.maxElement (set.Set())
    let minElement (set : OriginalSet<'a>) = Set.minElement (set.Set())
    
    let ofArray (array : 'a []) = 
        let list = LazyList.ofArray array
        let set = Set.ofList (list.List())
        OriginalSet(list, set)
    
    let ofList (elements : 'a list) = 
        let list = LazyList.ofList elements
        let set = Set.ofList (list.List())
        OriginalSet(list, set)
    
    let ofSeq (elements : 'a seq) = 
        let list = LazyList.ofSeq elements
        let set = Set.ofList (list.List())
        OriginalSet(list, set)
    
    let partition (predicate : 'a -> bool) (set : OriginalSet<'a>) = 
        let sq1, sq2 = 
            set
            |> toSeq
            |> Seq.partition predicate
        Set.ofSeq sq1, Set.ofSeq sq2
    
    let remove (value : 'a) (set : OriginalSet<'a>) = Set.remove value (set.Set())
    let singleton (value : 'a when 'a : comparison) = add value (OriginalSet<'a>(LazyList.empty, Set.empty))
    
    let toArray (set : OriginalSet<'a>) = 
        (set
        |> toSeq).Value
        |> Array.ofSeq
    
    let toList (set : OriginalSet<'a>) = 
        (set
        |> toSeq).Value
        |> List.ofSeq
    
    let union (set1 : OriginalSet<'a>) (set2 : OriginalSet<'a>) = 
        let mutable set : OriginalSet<'a> = empty
        for i in (Seq.append (toSeq set1) (toSeq set2)) do
            set <- add i set
        set
    
    let unionMany (sets : OriginalSet<'a> seq) = Seq.fold union (Seq.head sets) (Seq.tail sets)
    let (+) (set1 : OriginalSet<'a>) (set2 : OriginalSet<'a>) = union set1 set2
    
    let (-) (set1 : OriginalSet<'a>) (set2 : OriginalSet<'a>) = 
        filter (fun x -> 
            set2
            |> exists ((=) x)
            |> not) set1
