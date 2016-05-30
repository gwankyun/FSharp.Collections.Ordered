namespace FSharp.Collections

open System.Collections.Generic
open System.Collections
open System
open Extension

type LazyList<'a>(x : 'a list) = 
    
    override this.ToString() = 
        this.List()
        |> List.rev
        |> (fun x -> x.ToString())
    
    member this.List() = x
    member this.Cons(value : 'a) = 
        let ls = value :: this.List()
        LazyList(ls)

module LazyList = 
    let ofSeq (elements : 'a seq) = 
        LazyList(elements
                 |> Seq.rev
                 |> List.ofSeq)
    
    let _toSeq (list : LazyList<'a>) = 
        lazy (list.List()
              |> List.toSeq
              |> Seq.rev)
    
    let toSeq (list : LazyList<'a>) = 
        list
        |> _toSeq
        |> (fun x -> x.Value)
    
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
    
    let fold (folder : 's -> 't -> 's) (state : 's) (list : LazyList<'t>) = 
        let force = 
            list.List()
            |> Seq.ofList
            |> Seq.rev
        force |> Seq.fold folder state
    
    let foldBack (folder : 't -> 's -> 's) (list : LazyList<'t>) (state : 's) = 
        let force = 
            list.List()
            |> Seq.ofList
            |> Seq.rev
        Seq.foldBack folder force state
    
    let ofArray (array : 'a []) = 
        array
        |> Seq.ofArray
        |> Seq.rev
        |> ofSeq
    
    let ofList (elements : 'a list) = LazyList(elements |> List.rev)
    
    let partition (predicate : 'a -> bool) (list : LazyList<'a>) = 
        let list1, list2 = (list |> _toSeq).Value |> Seq.partition predicate
        (ofSeq list1, ofSeq list2)
    
    let iter (action : 'a -> unit) (list : LazyList<'a>) = (list.List() |> List.rev) |> Seq.iter action
    let length (list : LazyList<'a>) = list.List() |> List.length

    let rev (list : LazyList<'a>) =
        list.List()
        |> List.rev
        |> ofList
