namespace Extension

module Seq = 
    let partition (predicate : 'a -> bool) (source : 'a seq) = 
        (Seq.filter predicate source, Seq.filter (fun x -> not (predicate x)) source)
    let remove (value : 'a) (source : 'a seq) = Seq.filter (fun x -> x <> value) source
    
    //返回所有符合的索引
    let findAllIndex (predicate : 'a -> bool) (source : 'a seq) = 
        source
        |> Seq.indexed
        |> Seq.filter (fun (_, v) -> predicate v)
        |> Seq.map (fun (i, _) -> i)
    
    let splitWith (predicate : 'a -> bool) (source : 'a seq) = 
        let pushBack (value : 'a) (source : 'a seq) = Seq.append source (Seq.singleton value)
        let pushFront (value : 'b) (source : 'b seq) = Seq.append (Seq.singleton value) source
        
        let rec f (source1 : 'a seq) (source2 : 'a seq seq) (temp : 'a seq) = 
            match Seq.length source1 with
            | 0 -> source2
            | _ -> 
                let h = Seq.head source1
                let t = Seq.tail source1
                match predicate h with
                | true -> 
                    let temp = pushBack h temp
                    f t (pushFront temp source2) Seq.empty
                | false -> f t source2 (pushFront h temp)
        f source Seq.empty Seq.empty
    let groupByKey (source : ('a * 'b) seq) =

        source |> Seq.groupBy Tuple.first
               |> Seq.map (fun (k, v) -> (k, v |> Seq.map Tuple.second))

    let groupByValue (source : ('a * 'b) seq) =
        source |> Seq.groupBy Tuple.second
               |> Seq.map (fun (k, v) -> (k, v |> Seq.map Tuple.first))

//    let groupByOrigin