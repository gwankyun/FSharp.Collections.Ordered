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
    printfn "%A" argv
    0 // return an integer exit code
