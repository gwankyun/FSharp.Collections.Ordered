namespace Original.Collections

open System.Collections.Generic
open System.Collections
open System
open Extension

type LazyList<'a>(x : 'a list) = 
    //    member this.Seq() = x
    member this.List() = x
    member this.Cons(value : 'a) = 
        let ls = value :: this.List()
        //        let seq = ls |> Seq.ofList
        LazyList(ls)

module LazyList = 
    //    let makeLazyListFromSeq (seq : 'a seq) = 
    //        let ls = List.ofSeq seq
    //        LazyList(seq, ls)
    let ofSeq (elements : 'a seq) = 
        LazyList(elements
                 |> Seq.rev
                 |> List.ofSeq)
    
    let toSeq (list : LazyList<'a>) = 
        lazy (list.List()
              |> List.toSeq
              |> Seq.rev)
    
    let map (mapping : 'a -> 'b) (list : LazyList<'a>) = 
        let seq = (toSeq list).Value |> Seq.map mapping
        //        makeLazyListFromSeq seq
        seq |> ofSeq
    
    let filter (predicate : 'a -> bool) (list : LazyList<'a>) = 
        let seq = (toSeq list).Value |> Seq.filter predicate
        seq |> ofSeq
        //        makeLazyListFromSeq seq
//        LazyList(seq
//                 |> Seq.rev
//                 |> List.ofSeq)
    
    let empty<'a> = LazyList<'a>(List.empty)
    
    let fold (folder : 's -> 't -> 's) (state : 's) (list : LazyList<'t>) = 
        let force = (list |> toSeq).Value
        force |> Seq.fold folder state
    
    let foldBack (folder : 't -> 's -> 's) (list : LazyList<'t>) (state : 's) = 
        list |> fold (fun s t -> folder t s) state
    
    let ofArray (array : 'a []) = 
        array
        |> Seq.ofArray
        |> Seq.rev
        |> ofSeq
    
    let ofList (elements : 'a list) = LazyList(elements |> List.rev)
    
    //        |> Seq.ofList
    //        |> Seq.rev
    //        |> ofSeq
    let partition (predicate : 'a -> bool) (list : LazyList<'a>) = 
        //        let list1 = filter predicate list
        //        let list2 = filter (fun x -> not (predicate x)) list
        //        (list1, list2)
        let list1, list2 = (list |> toSeq).Value |> Seq.partition predicate
        (ofSeq list1, ofSeq list2)
