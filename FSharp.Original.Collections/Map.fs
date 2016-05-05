namespace Extension

module Map = 
    let keys (table : Map<'Key, 'T>) = 
        table
        |> Map.toSeq
        |> Seq.map (fun k v -> k)
    
    let values (table : Map<'Key, 'T>) = 
        table
        |> Map.toSeq
        |> Seq.map (fun k v -> v)
    
    let update (key : 'Key) (value : 'T) (table : Map<'Key, 'T>) : Map<'Key, 'T> when 'Key : comparison = 
        match table.ContainsKey(key) with
        | true -> table.Remove(key).Add(key, value)
        | false -> table.Add(key, value)
    
    let updateWith (key : 'Key) (updating : 'T -> 'T) (defult : 'T) (table : Map<'Key, 'T>) : Map<'Key, 'T> when 'Key : comparison = 
        match table.ContainsKey(key) with
        | true -> table.Remove(key).Add(key, updating table.[key])
        | false -> table.Add(key, defult)
    
    let updateAll (predicate : 'Key -> 'T -> bool) (mapping : 'Key -> 'T -> 'T) (table : Map<'Key, 'T>) : Map<'Key, 'T> when 'Key : comparison = 
        table |> Map.map (fun k v -> 
                     match predicate k v with
                     | true -> mapping k v
                     | false -> v)
    
    let merge (merging : 'T -> 'T -> 'T) (table1 : Map<'Key, 'T>) (table2 : Map<'Key, 'T>) : Map<'Key, 'T> when 'Key : comparison = 
        Map.fold (fun s k t -> 
            match s.ContainsKey(k) with
            | true -> 
                let t1 = s.[k]
                s.Remove(k).Add(k, merging t t1)
            | false -> s.Add(k, t)) table1 table2
//    let updateByKey (key : 'Key) (value : 'T) (table : Map<'Key, 'T>) : Map<'Key, 'T> when 'Key : comparison = 
//        match Map.containsKey key table with
//        | true ->
//            table
//            |> Map.remove key
//            |> Map.add key value
//        | false -> table |> Map.add key value

    let difference (table1 : Map<'a, 'b>) (table2 : Map<'a, 'b>) =
        Map.filter (fun k v ->
            match Map.containsKey k table2 with
            | false -> true
            | true ->
                let value = Map.find k table2
                value = v) table1
