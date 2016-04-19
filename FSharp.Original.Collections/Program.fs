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
    let mutable m : OriginalMap<int, OriginalSet<int>> = OriginalMap.empty
    for i in [0..9] do
        let key = i % 3
        if OriginalMap.containsKey key m then
            let v = OriginalMap.find key m
            let nv = OriginalSet.add i v
            m <- OriginalMap.add key nv m
        else
            m <- OriginalMap.add key (OriginalSet.singleton i) m
    for i in m.Seq() do
        for j in OriginalSet.toSeq (OriginalMap.find i m) do
            printf "%i" j  
        printfn "%s" " "      
    printfn "%A" argv
    0 // return an integer exit code
