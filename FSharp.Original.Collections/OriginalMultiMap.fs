namespace Original.Collections
open System.Collections.Generic

type OriginalMultiMap<'Key, 'Value when 'Key : comparison and 'Value : comparison>(m: OriginalMap<'Key, OriginalSet<'Value>>) as self =
    [<DefaultValue>]
    val mutable _Map: OriginalMap<'Key, OriginalSet<'Value>>
    do
        self._Map <- m
    member this.Map() =
        this._Map

module OriginalMultiMap =

    let add (key: 'Key) (value: 'T) (table: OriginalMultiMap<'Key,'T>): OriginalMultiMap<'Key,'T> when 'Key : comparison and 'T : comparison =
        let m = table.Map()
        match OriginalMap.tryFind key m with
        | Some(v) ->
            new OriginalMultiMap<'Key, 'T>(OriginalMap.add key (OriginalSet.add value v) m)
        | None ->
            new OriginalMultiMap<'Key, 'T>(OriginalMap.add key (OriginalSet.singleton value) m)

    let empty (table: OriginalMultiMap<'Key,'T>): OriginalMultiMap<'Key,'T> when 'Key : comparison and 'T : comparison =
        new OriginalMultiMap<'Key, 'T>(OriginalMap.empty)
