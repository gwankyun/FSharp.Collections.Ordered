namespace Collections.Immutable
open System.Collections.Immutable
open System.Linq

module ImmutableList =
    begin
        let add (value : 'a) (list : ImmutableList<'a>) =
            list.Add(value)

        let count (list : ImmutableList<'a>) =
            list.Count

        let contains (value : 'a) (list : ImmutableList<'a>) =
            list.Contains(value)

        let exists (f : 'a -> bool) (list : ImmutableList<'a>) =
            list.Exists(System.Predicate(f))

        let map (f : 'a -> bool) (list : ImmutableList<'a>) =
            list.Select(f).ToImmutableList()

        let insert (index : int) (item : 'a) (list : ImmutableList<'a>) =
            list.Insert(index, item)

        let setItem (index : int) (item : 'a) (list : ImmutableList<'a>) =
            list.SetItem(index, item)

        let filter (f : 'a -> bool) (list : ImmutableList<'a>) =
            list.Where(f).ToImmutableList()

        let tryFind (f : 'a -> bool) (list : ImmutableList<'a>) =
            let rec inner index =
                if index < (list |> count) then
                    let value = list.[index] in 
                    if f value then 
                        Some(value)
                    else
                        inner (index + 1)
                else
                    None
            in 
            inner 0

        let fold (f : 's -> 't -> 's) (state : 's) (list : ImmutableList<'t>) =
            list.Aggregate(state, System.Func<'s, 't, 's>(fun s t -> f s t))
        
        let tryFindIndex (f : 'a -> bool) (list : ImmutableList<'a>) =
            let rec inner index =
                if index < (list |> count) then
                    let value = list.[index] in 
                    if f value then 
                        Some(index)
                    else
                        inner (index + 1)
                else
                    None
            inner 0

        let tryItem (index : int) (list : ImmutableList<'a>) =
            let count = list |> count in 
            if index < count then
                Some(list.[index])
            else
                None

        let empty<'a> : ImmutableList<'a> = ImmutableList<'a>.Empty

        let isEmpty (list : ImmutableList<'a>) = list.IsEmpty

        let singleton (value : 'a) =
            empty
            |> add value
    end
