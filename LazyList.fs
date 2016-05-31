namespace FSharp.Collections

open System.Collections.Generic
open System.Collections
open System
open Extension

type LazyList<'a>(x : 'a list) = 
    override this.ToString() = this.ToList() |> Member.toString
    member this.List() = x
    
    member this.Cons(value : 'a) = 
        let ls = value :: this.List()
        LazyList(ls)
    
    member this.ToList() = this.List() |> List.rev

module LazyList = 
    let ofSeq (elements : 'a seq) = 
        LazyList(elements
                 |> Seq.rev
                 |> List.ofSeq)
    
    let toSeq (list : LazyList<'a>) = 
        list.List()
        |> List.toSeq
        |> Seq.rev
    
    //    let toSeq (list : LazyList<'a>) = 
    //        list
    //        |> _toSeq
    //        |> (fun x -> x.Value)
    let map (mapping : 'a -> 'b) (list : LazyList<'a>) = 
        list.List()
        |> Seq.ofList
        |> Seq.map mapping
        |> Seq.toList
        |> (fun x -> LazyList(x))
    
    let filter (predicate : 'a -> bool) (list : LazyList<'a>) = 
        list.List()
        |> Seq.ofList
        |> Seq.filter predicate
        |> Seq.toList
        |> (fun x -> LazyList(x))
    
    let empty<'a> = LazyList<'a>(List.empty)
    let toList (list : LazyList<'a>) = list.ToList()
    
    let fold (folder : 's -> 't -> 's) (state : 's) (list : LazyList<'t>) = 
        list
        |> toList
        |> List.fold folder state
    
    let foldBack (folder : 't -> 's -> 's) (list : LazyList<'t>) (state : 's) = 
        let force = list |> toSeq
        Seq.foldBack folder force state
    
    let ofList (elements : 'a list) = LazyList(elements |> List.rev)
    
    let ofArray (array : 'a []) = 
        array
        |> Array.toList
        |> ofList
    
    //    let partition (predicate : 'a -> bool) (list : LazyList<'a>) = 
    //        let list1, list2 = (list |> _toSeq).Value |> Seq.partition predicate
    //        (ofSeq list1, ofSeq list2)
    let iter (action : 'a -> unit) (list : LazyList<'a>) = 
        list
        |> toList
        |> List.iter action
    
    let length (list : LazyList<'a>) = list.List() |> List.length
    
    let rev (list : LazyList<'a>) = 
        list
        |> toList
        |> (fun x -> LazyList(x))
