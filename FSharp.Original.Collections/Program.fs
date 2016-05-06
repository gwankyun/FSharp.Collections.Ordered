// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System
open Original.Collections

[<EntryPoint>]
let main argv = 
    let os : OriginalSet<int> = OriginalSet.empty
    if ((OriginalSet.add 0 os).Set() = Set.singleton 0) then
        printfn "%s" "add pass"
    if ((OriginalSet.add 0 os).Set() = Set.singleton 0) then
        printfn "%s" "add pass"
//    let mutable m : OriginalMap<int, OriginalSet<int>> = OriginalMap.empty
//    for i in [0..9] do
//        let key = i % 3
//        if OriginalMap.containsKey key m then
//            let v = OriginalMap.find key m
//            let nv = OriginalSet.add i v
//            m <- OriginalMap.add key nv m
//        else
//            m <- OriginalMap.add key (OriginalSet.singleton i) m
//    for i in m.Seq() do
//        for j in OriginalSet.toSeq (OriginalMap.find i m) do
//            printf "%i" j  
//        printfn "%s" " "    
    let mutable m : OriginalMultiMap<int, int> = OriginalMultiMap.empty
    for i in [0..9] do
        let key = i % 3
        m <- OriginalMultiMap.add key i m
    m <- OriginalMultiMap.add 0 0 m
    let n = 1000000
    OriginalMultiMap.iter (fun k v ->
        printfn "%i %i" k v) m
    let sz = Seq.zip (seq {for i in [0..n] -> i}) (seq {for i in [0..n] -> i})
    printfn "%s" "Seq.zip OK"
    let s = OriginalMultiMap.ofSeq sz
    printfn "%s" "OriginalMultiMap.ofSeq OK"
    let sf = s |> OriginalMultiMap.filter (fun k v -> (v % 2) = 0)
    printfn "%s" "OriginalMultiMap.filter OK"
    printfn "%A" argv
    0 // return an integer exit code
