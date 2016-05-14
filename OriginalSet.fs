namespace Original.Collections

open System.Collections.Generic
open System.Collections
open System
open Extension

type OriginalSet<'a when 'a : comparison>(x : 'a seq, y : Set<'a>, z : 'a list) = 
    member this.Seq() = x
    member this.Set() = y
    member this.List() = z

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
        let a, b, c = set.Seq(), set.Set(), set.List()
        match Set.contains value b with
        | true -> OriginalSet(a, b, c)
        | false -> 
            let list = value :: c
            let seq = list |> Seq.ofList
            OriginalSet(seq, b.Add(value), list)
    
    let contains (value : 'a) (set : OriginalSet<'a>) = Set.contains value (set.Set())
    let count (set : OriginalSet<'a>) = Set.count (set.Set())
    let difference (set1 : OriginalSet<'a>) (set2 : OriginalSet<'a>) = set1.Set() - set2.Set()
    let empty<'a when 'a : comparison> = OriginalSet<'a>(Seq.empty, Set.empty, List.empty)
    let exists (predicate : 'a -> bool) (set : OriginalSet<'a>) = Set.exists predicate (set.Set())
    let toSeq (set : OriginalSet<'a>) = set.Seq() |> Seq.rev
    
    let filter (predicate : 'a -> bool) (set : OriginalSet<'a>) = 
        let a, b, c = toSeq set, set.Set(), set.List()
        OriginalSet(Seq.filter predicate a, Set.filter predicate b, c)
    
    let fold (folder : 's -> 't -> 's) (state : 's) (set : OriginalSet<'t>) = 
        set.List()
        |> List.ofSeq
        |> Seq.ofList
        |> Seq.fold folder state
    
    //        let a = set |> toSeq |> Seq.fold folder state
    //        let b = Set.ofSeq a
    //        OriginalSet(a, b)
    let foldBack (folder : 't -> 's -> 's) (set : OriginalSet<'t>) (state : 's) = 
        set.List()
        |> List.ofSeq
        |> Seq.ofList
        |> (fun x -> Seq.foldBack folder x state)
    
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
        set
        |> toSeq
        |> Seq.iter action
    
    let map (mapping : 'a -> 'b) (set : OriginalSet<'a>) = 
        OriginalSet(Seq.map mapping (toSeq set), Set.map mapping (set.Set()), List.map mapping (set.List()))
    let maxElement (set : OriginalSet<'a>) = Set.maxElement (set.Set())
    let minElement (set : OriginalSet<'a>) = Set.minElement (set.Set())
    
    let ofArray (array : 'a []) = 
        let list = array |> List.ofArray |> List.rev
        let seq = list |> Seq.ofList
        let set = Set.ofList (List.rev list)
        OriginalSet(seq, set, list)
    
    let ofList (elements : 'a list) = 
        let list = elements |> List.rev
        let seq = list |> Seq.ofList
        let set = Set.ofList (List.rev list)
        OriginalSet(seq, set, list)
    
    let ofSeq (elements : 'a seq) = 
        let seq = elements
        let list = seq |> Seq.rev |> List.ofSeq
        let set = Set.ofList (List.rev list)
        OriginalSet(seq, set, list)
    
    let partition (predicate : 'a -> bool) (set : OriginalSet<'a>) = 
        let sq1, sq2 = 
            set
            |> toSeq
            |> Seq.partition predicate
        Set.ofSeq sq1, Set.ofSeq sq2
    
    let remove (value : 'a) (set : OriginalSet<'a>) = Set.remove value (set.Set())
    let singleton (value : 'a when 'a : comparison) = add value (OriginalSet<'a>(Seq.empty, Set.empty, List.empty))
    
    let toArray (set : OriginalSet<'a>) = 
        set
        |> toSeq
        |> Array.ofSeq
    
    let toList (set : OriginalSet<'a>) = 
        set
        |> toSeq
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
