namespace Original.Collections

open System.Collections.Generic
open System.Collections
open System
open Extension

type LazyList<'a>(x : 'a seq, y : 'a list) =
    member this.Seq() = x
    member this.List() = y

module LazyList =
    let add (value : 'a) (list : LazyList<'a>) =
        let ls = value :: list.List()
        let seq = ls |> Seq.ofList
        LazyList(seq, ls)

    let makeLazyListFromSeq (seq : 'a seq) =
        let ls = List.ofSeq seq
        LazyList(seq, ls)

    let map (mapping : 'a -> 'b) (list : LazyList<'a>) =
        let seq = list.Seq() |> Seq.map mapping
        makeLazyListFromSeq seq

    let filter (predicate : 'a -> bool) (list : LazyList<'a>) =
        let seq = list.Seq() |> Seq.filter predicate
        makeLazyListFromSeq seq