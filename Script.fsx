// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
open FSharp.Original.Collections

// Define your library scripting code here

//let mutable seq1 : int seq = Seq.empty
//for i in [0..10000000] do
//    seq1 <- Seq.append (Seq.singleton i) seq1
//
//printfn "%i" (Seq.length seq1)

//let mutable seq : int seq = Seq.empty
//for i in [0..100000] do
//    seq <- Seq.concat ([seq; (Seq.singleton i)] |> Seq.ofList)
//
//printfn "%i" (Seq.length seq)

let mutable list : int list = List.empty

for i in [0..100000000] do
    list <- i :: list

printfn "%i" (List.length list)