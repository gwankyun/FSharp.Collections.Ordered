namespace Original.Collections
open System.Collections.Generic

type OriginalSet<'T when 'T : comparison>(s: seq<'T>, m: Set<'T>) as self =
    [<DefaultValue>]
    val mutable _Seq: seq<'T>
    [<DefaultValue>]
    val mutable _Set: Set<'T>
    do
        self._Seq <- s
        self._Set <- m
    member this.Seq() =
        this._Seq
    member this.Set() =
        this._Set
    new (elements: seq<'T>) =
        new OriginalSet<'T>(elements, elements |> Set.ofSeq)
    override this.ToString() =
        (this._Seq.ToString()) + "\n" + (this._Set.ToString())

module OriginalSet =
    let add (value: 'T) (set: OriginalSet<'T>): OriginalSet<'T> when 'T : comparison =
        let s = Seq.append set._Seq ([value] |> Seq.ofList)
        let m = set._Set.Add value
        new OriginalSet<'T>(s, m)

    let empty<'T when 'T : comparison> : OriginalSet<'T> when 'T : comparison  = new OriginalSet<'T>(Seq.empty, Set.empty)

    let singleton (value: 'T): OriginalSet<'T> when 'T : comparison =
        let s = Seq.singleton value
        new OriginalSet<'T>(s)

    let contains (element: 'T) (set: OriginalSet<'T>): bool when 'T : comparison =
        set._Set.Contains element

    let isSubset (set1: OriginalSet<'T>) (set2: OriginalSet<'T>): bool when 'T : comparison =
        Set.isSubset set1._Set set2._Set

    let isProperSubset (set1:OriginalSet<'T>) (set2: OriginalSet<'T>): bool when 'T : comparison =
        Set.isProperSubset set1._Set set2._Set

    let isSuperset (set1: OriginalSet<'T>) (set2: OriginalSet<'T>): bool when 'T : comparison =
        Set.isSuperset set1._Set set2._Set

    let isProperSuperset (set1: OriginalSet<'T>) (set2: OriginalSet<'T>): bool when 'T : comparison =
        Set.isProperSuperset set1._Set set2._Set

    let count (set: OriginalSet<'T>): int when 'T : comparison  =
        set._Set.Count

    let exists (predicate: 'T -> bool) (set: OriginalSet<'T>): bool when 'T : comparison =
        Set.exists predicate set._Set

    let filter (predicate: 'T -> bool) (set: OriginalSet<'T>): OriginalSet<'T> when 'T : comparison =
        let s = Seq.filter predicate set._Seq
        let m = Set.filter predicate set._Set
        new OriginalSet<'T>(s, m)

    let map (mapping: 'T -> 'U) (set: OriginalSet<'T>): OriginalSet<'U> when 'T : comparison and 'U : comparison =
        let s = Seq.map mapping set._Seq
        let m = Set.map mapping set._Set
        new OriginalSet<'U>(s, m)

    let fold (folder: 'State -> 'T -> 'State) (state: 'State) (set: OriginalSet<'T>): 'State when 'T : comparison =
        Seq.fold folder state set._Seq

    let head (set: OriginalSet<'T>): 'T when 'T : comparison =
        Seq.head set._Seq

    let tail (set: OriginalSet<'T>): OriginalSet<'T> when 'T : comparison =
        let s = Seq.tail set._Seq
        let m = Set.ofSeq s
        new OriginalSet<'T>(s, m)

    let reduce (reduction: 'T -> 'T -> 'T) (set: OriginalSet<'T>): 'T when 'T : comparison =
        fold reduction (head set) (tail set)

    let foldBack (folder: 'T -> 'State -> 'State) (set: OriginalSet<'T>) (state: 'State): 'State when 'T : comparison =
        Seq.foldBack folder set._Seq state

    let forall (predicate: 'T -> bool) (set: OriginalSet<'T>): bool when 'T : comparison =
        Seq.forall predicate set._Seq

    let intersect (set1: OriginalSet<'T>) (set2: OriginalSet<'T>): OriginalSet<'T> when 'T : comparison =
        filter (fun x -> contains x set2) set1

    let intersectMany (sets: seq<OriginalSet<'T>>): OriginalSet<'T> when 'T : comparison =
        Seq.reduce intersect sets

    let union (set1: OriginalSet<'T>) (set2: OriginalSet<'T>): OriginalSet<'T> when 'T : comparison =
        let s = (Seq.append set1._Seq set2._Seq) |> Seq.distinct
        let m = Set.ofSeq s
        new OriginalSet<'T>(s, m)

    let unionMany (sets: seq<OriginalSet<'T>>): OriginalSet<'T> when 'T : comparison =
        Seq.reduce union sets

    let isEmpty (set: OriginalSet<'T>): bool when 'T : comparison =
        set._Set.IsEmpty

    let iter (action: 'T -> unit)  (set: OriginalSet<'T>): unit when 'T : comparison =
        Seq.iter action set._Seq

    let partition (predicate: 'T -> bool) (set: OriginalSet<'T>): OriginalSet<'T> * OriginalSet<'T> when 'T : comparison =
        (filter predicate set, filter (predicate >> not) set)

    let remove (value: 'T) (set: OriginalSet<'T>): OriginalSet<'T> when 'T : comparison =
        let s = Seq.filter ((=) value) set._Seq
        let m = Set.ofSeq s
        new OriginalSet<'T>(s, m)

    let minElement (set: OriginalSet<'T>): 'T when 'T : comparison =
        set._Set.MinimumElement

    let maxElement (set: OriginalSet<'T>): 'T when 'T : comparison =
        set._Set.MaximumElement

    let ofSeq (elements: seq<'T>): OriginalSet<'T> when 'T : comparison =
        new OriginalSet<'T>(elements)

    let toSeq (set: OriginalSet<'T>): seq<'T> when 'T : comparison =
        set._Seq

    let ofList (elements:'T list): OriginalSet<'T> when 'T : comparison =
        elements |> Seq.ofList |> ofSeq

    let toList (set: OriginalSet<'T>): 'T list when 'T : comparison =
        set |> toSeq |> List.ofSeq

    let ofArray (array:'T []): OriginalSet<'T> when 'T : comparison =
        array |> Seq.ofArray |> ofSeq

    let toArray (set: OriginalSet<'T>): 'T [] when 'T : comparison =
        set |> toSeq |> Array.ofSeq

    let difference (set1: OriginalSet<'T>) (set2: OriginalSet<'T>): OriginalSet<'T> when 'T : comparison =
        filter (fun x -> not(contains x set2)) set1
