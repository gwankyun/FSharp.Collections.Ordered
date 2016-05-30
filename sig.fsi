



namespace FSharp.Original.Collections
  type Class1 =
    class
      new : unit -> Class1
      member X : string
    end

namespace FSharp.Collections
  type LazyList<'a> =
    class
      new : x:'a list -> LazyList<'a>
      member Cons : value:'a -> LazyList<'a>
      member List : unit -> 'a list
    end
  module LazyList = begin
    val ofSeq : elements:seq<'a> -> LazyList<'a>
    val _toSeq : list:LazyList<'a> -> Lazy<seq<'a>>
    val toSeq : list:LazyList<'a> -> seq<'a>
    val map : mapping:('a -> 'b) -> list:LazyList<'a> -> LazyList<'b>
    val filter : predicate:('a -> bool) -> list:LazyList<'a> -> LazyList<'a>
    val empty<'a> : LazyList<'a>
    val fold : folder:('s -> 't -> 's) -> state:'s -> list:LazyList<'t> -> 's
    val foldBack :
      folder:('t -> 's -> 's) -> list:LazyList<'t> -> state:'s -> 's
    val ofArray : array:'a [] -> LazyList<'a>
    val ofList : elements:'a list -> LazyList<'a>
    val partition :
      predicate:('a -> bool) -> list:LazyList<'a> -> LazyList<'a> * LazyList<'a>
    val iter : action:('a -> unit) -> list:LazyList<'a> -> unit
    val length : list:LazyList<'a> -> int
  end

namespace FSharp.Collections
  type LinkedSet<'a when 'a : comparison> =
    class
      new : x:LazyList<'a> * y:Set<'a> -> LinkedSet<'a>
      member List : unit -> LazyList<'a>
      member Set : unit -> Set<'a>
    end
  type LinkedMap<'a,'b when 'a : comparison> =
    class
      new : x:LazyList<'a> * y:Map<'a,'b> -> LinkedMap<'a,'b>
      member Add : key:'a * value:'b -> LinkedMap<'a,'b>
      member List : unit -> LazyList<'a>
      member Map : unit -> Map<'a,'b>
      static member Empty : unit -> LinkedMap<'a,'b>
    end
  type LinkedMultiMap<'a,'b when 'a : comparison and 'b : comparison> =
    class
      new : x:LinkedMap<'a,LinkedSet<'b>> -> LinkedMultiMap<'a,'b>
      member LinkedMap : unit -> LinkedMap<'a,LinkedSet<'b>>
    end
  module LinkedSet = begin
    val add :
      value:'a -> set:LinkedSet<'a> -> LinkedSet<'a> when 'a : comparison
    val contains : value:'a -> set:LinkedSet<'a> -> bool when 'a : comparison
    val count : set:LinkedSet<'a> -> int when 'a : comparison
    val difference :
      set1:LinkedSet<'a> -> set2:LinkedSet<'a> -> Set<'a> when 'a : comparison
    val empty<'a when 'a : comparison> : LinkedSet<'a> when 'a : comparison
    val exists :
      predicate:('a -> bool) -> set:LinkedSet<'a> -> bool when 'a : comparison
    val toSeq : set:LinkedSet<'a> -> seq<'a> when 'a : comparison
    val filter :
      predicate:('a -> bool) -> set:LinkedSet<'a> -> LinkedSet<'a>
        when 'a : comparison
    val fold :
      folder:('s -> 't -> 's) -> state:'s -> set:LinkedSet<'t> -> 's
        when 't : comparison
    val foldBack :
      folder:('t -> 's -> 's) -> set:LinkedSet<'t> -> state:'s -> 's
        when 't : comparison
    val forall :
      predicate:('a -> bool) -> set:LinkedSet<'a> -> bool when 'a : comparison
    val intersect :
      set1:LinkedSet<'a> -> set2:LinkedSet<'a> -> LinkedSet<'a>
        when 'a : comparison
    val intersectMany :
      sets:seq<LinkedSet<'a>> -> LinkedSet<'a> when 'a : comparison
    val isEmpty : set:LinkedSet<'a> -> bool when 'a : comparison
    val isProperSubset :
      set1:LinkedSet<'a> -> set2:LinkedSet<'a> -> bool when 'a : comparison
    val isProperSuperset :
      set1:LinkedSet<'a> -> set2:LinkedSet<'a> -> bool when 'a : comparison
    val isSubset :
      set1:LinkedSet<'a> -> set2:LinkedSet<'a> -> bool when 'a : comparison
    val isSuperset :
      set1:LinkedSet<'a> -> set2:LinkedSet<'a> -> bool when 'a : comparison
    val iter :
      action:('a -> unit) -> set:LinkedSet<'a> -> unit when 'a : comparison
    val map :
      mapping:('a -> 'b) -> set:LinkedSet<'a> -> LinkedSet<'b>
        when 'a : comparison and 'b : comparison
    val maxElement : set:LinkedSet<'a> -> 'a when 'a : comparison
    val minElement : set:LinkedSet<'a> -> 'a when 'a : comparison
    val ofArray : array:'a [] -> LinkedSet<'a> when 'a : comparison
    val ofList : elements:'a list -> LinkedSet<'a> when 'a : comparison
    val ofSeq : elements:seq<'a> -> LinkedSet<'a> when 'a : comparison
    val partition :
      predicate:('a -> bool) ->
        set:LinkedSet<'a> -> LinkedSet<'a> * LinkedSet<'a> when 'a : comparison
    val remove : value:'a -> set:LinkedSet<'a> -> Set<'a> when 'a : comparison
    val singleton : value:'a -> LinkedSet<'a> when 'a : comparison
    val toArray : set:LinkedSet<'a> -> 'a [] when 'a : comparison
    val toList : set:LinkedSet<'a> -> 'a list when 'a : comparison
    val union :
      set1:LinkedSet<'a> -> set2:LinkedSet<'a> -> LinkedSet<'a>
        when 'a : comparison
    val unionMany :
      sets:seq<LinkedSet<'a>> -> LinkedSet<'a> when 'a : comparison
    val ( + ) :
      set1:LinkedSet<'a> -> set2:LinkedSet<'a> -> LinkedSet<'a>
        when 'a : comparison
    val ( - ) :
      set1:LinkedSet<'a> -> set2:LinkedSet<'a> -> LinkedSet<'a>
        when 'a : comparison
    val groupBy :
      projection:('a -> 'key) -> table:LinkedSet<'a> -> LinkedMap<'key,'a>
        when 'a : comparison and 'key : comparison
  end

namespace FSharp.Collections
  module LinkedMap = begin
    val add :
      key:'a -> value:'b -> set:LinkedMap<'a,'b> -> LinkedMap<'a,'b>
        when 'a : comparison
    val containsKey :
      key:'a -> table:LinkedMap<'a,'b> -> bool when 'a : comparison
    val empty<'a,'b when 'a : comparison and 'b : comparison> :
      LinkedMap<'a,'b> when 'a : comparison and 'b : comparison
    val exists :
      predicate:('a -> 'b -> bool) -> table:LinkedMap<'a,'b> -> bool
        when 'a : comparison
    val filter :
      predicate:('a -> 'b -> bool) -> table:LinkedMap<'a,'b> -> LinkedMap<'a,'b>
        when 'a : comparison
    val find : key:'a -> table:LinkedMap<'a,'b> -> 'b when 'a : comparison
    val findKey :
      predicate:('a -> 'b -> bool) -> table:LinkedMap<'a,'b> -> 'a
        when 'a : comparison
    val fold :
      folder:('s -> 'a -> 'b -> 's) -> state:'s -> table:LinkedMap<'a,'b> -> 's
        when 'a : comparison
    val foldBack :
      folder:('a -> 'b -> 's -> 's) -> table:LinkedMap<'a,'b> -> state:'s -> 's
        when 'a : comparison
    val forall :
      predicate:('a -> 'b -> bool) -> table:LinkedMap<'a,'b> -> bool
        when 'a : comparison
    val isEmpty : table:LinkedMap<'a,'b> -> bool when 'a : comparison
    val iter :
      action:('a -> 'b -> unit) -> table:LinkedMap<'a,'b> -> unit
        when 'a : comparison
    val map :
      mapping:('a -> 'b -> 'c) -> table:LinkedMap<'a,'b> -> LinkedMap<'a,'c>
        when 'a : comparison
    val ofArray : elements:('a * 'b) [] -> LinkedMap<'a,'b> when 'a : comparison
    val ofList :
      elements:('a * 'b) list -> LinkedMap<'a,'b> when 'a : comparison
    val ofSeq : elements:seq<'a * 'b> -> LinkedMap<'a,'b> when 'a : comparison
    val partition :
      predicate:('a -> 'b -> bool) ->
        table:LinkedMap<'a,'b> -> LinkedMap<'a,'b> * LinkedMap<'a,'b>
        when 'a : comparison
    val remove :
      key:'a -> table:LinkedMap<'a,'b> -> LinkedMap<'a,'b> when 'a : comparison
    val toSeq : table:LinkedMap<'a,'b> -> seq<'a * 'b> when 'a : comparison
    val toArray : table:LinkedMap<'a,'b> -> ('a * 'b) [] when 'a : comparison
    val toList : table:LinkedMap<'a,'b> -> ('a * 'b) list when 'a : comparison
    val tryFind :
      key:'a -> table:LinkedMap<'a,'b> -> 'b option when 'a : comparison
    val tryFindKey :
      predicate:('a -> 'b -> bool) -> table:LinkedMap<'a,'b> -> 'a option
        when 'a : comparison
    val tryPick :
      chooser:('a -> 'b -> 'c option) -> table:LinkedMap<'a,'b> -> 'c option
        when 'a : comparison
    val difference :
      table1:LinkedMap<'a,'b> -> table2:LinkedMap<'a,'b> -> LinkedMap<'a,'b>
        when 'a : comparison and 'b : equality
    val sort :
      table:LinkedMap<'a,'b> -> LinkedMap<'a,'b>
        when 'a : comparison and 'b : comparison
    val sortBy :
      projection:('a -> 'b -> 'key) ->
        table:LinkedMap<'a,'b> -> LinkedMap<'a,'b>
        when 'a : comparison and 'key : comparison
    val sortWith :
      comparer:('a * 'b -> 'a * 'b -> int) ->
        table:LinkedMap<'a,'b> -> LinkedMap<'a,'b> when 'a : comparison
    val sortDescending :
      table:LinkedMap<'a,'b> -> LinkedMap<'a,'b>
        when 'a : comparison and 'b : comparison
    val sortByDescending :
      projection:('a -> 'b -> 'key) ->
        table:LinkedMap<'a,'b> -> LinkedMap<'a,'b>
        when 'a : comparison and 'key : comparison
    val length : table:LinkedMap<'a,'b> -> int when 'a : comparison
  end

namespace FSharp.Collections
  module LinkedMultiMap = begin
    val add :
      key:'a -> value:'b -> table:LinkedMultiMap<'a,'b> -> LinkedMultiMap<'a,'b>
        when 'a : comparison and 'b : comparison
    val empty<'a,'b when 'a : comparison and 'b : comparison> :
      LinkedMultiMap<'a,'b> when 'a : comparison and 'b : comparison
    val iter :
      action:('a -> 'b -> unit) -> table:LinkedMultiMap<'a,'b> -> unit
        when 'a : comparison and 'b : comparison
    val ( + ) :
      set1:LinkedMultiMap<'a,'b> ->
        set2:LinkedMultiMap<'a,'b> -> LinkedMultiMap<'a,'b>
        when 'a : comparison and 'b : comparison
    val containsKey :
      key:'a -> table:LinkedMultiMap<'a,'b> -> bool
        when 'a : comparison and 'b : comparison
    val exists :
      predicate:('a -> 'b -> bool) -> table:LinkedMultiMap<'a,'b> -> bool
        when 'a : comparison and 'b : comparison
    val find :
      key:'a -> table:LinkedMultiMap<'a,'b> -> LinkedSet<'b>
        when 'a : comparison and 'b : comparison
    val findkey :
      predicate:('a -> 'b -> bool) -> table:LinkedMultiMap<'a,'b> -> 'a
        when 'a : comparison and 'b : comparison
    val forall :
      predicate:('a -> 'b -> bool) -> table:LinkedMultiMap<'a,'b> -> bool
        when 'a : comparison and 'b : comparison
    val isEmpty :
      table:LinkedMultiMap<'a,'b> -> bool
        when 'a : comparison and 'b : comparison
    val map :
      mapping:('a -> 'b -> 'c) ->
        table:LinkedMultiMap<'a,'b> -> LinkedMultiMap<'a,'c>
        when 'a : comparison and 'b : comparison and 'c : comparison
    val tryFind :
      key:'a -> table:LinkedMultiMap<'a,'b> -> LinkedSet<'b> option
        when 'a : comparison and 'b : comparison
    val ofSeq :
      elements:seq<'a * 'b> -> LinkedMultiMap<'a,'b>
        when 'a : comparison and 'b : comparison
    val ofArray :
      elements:('a * 'b) [] -> LinkedMultiMap<'a,'b>
        when 'a : comparison and 'b : comparison
    val ofList :
      elements:('a * 'b) list -> LinkedMultiMap<'a,'b>
        when 'a : comparison and 'b : comparison
    val remove :
      key:'a -> table:LinkedMultiMap<'a,'b> -> LinkedMultiMap<'a,'b>
        when 'a : comparison and 'b : comparison
    val toSeq :
      table:LinkedMultiMap<'a,'b> -> seq<'a * 'b>
        when 'a : comparison and 'b : comparison
    val filter :
      predicate:('a -> 'b -> bool) ->
        table:LinkedMultiMap<'a,'b> -> LinkedMultiMap<'a,'b>
        when 'a : comparison and 'b : comparison
    val partition :
      predicate:('a -> 'b -> bool) ->
        table:LinkedMultiMap<'a,'b> ->
          LinkedMultiMap<'a,'b> * LinkedMultiMap<'a,'b>
        when 'a : comparison and 'b : comparison
    val pick :
      chooser:('a -> 'b -> 'c option) -> table:LinkedMultiMap<'a,'b> -> 'c
        when 'a : comparison and 'b : comparison
    val fold :
      folder:('s -> 'a -> 'b -> 's) ->
        state:'s -> table:LinkedMultiMap<'a,'b> -> 's
        when 'a : comparison and 'b : comparison
    val foldBack :
      folder:('a -> 'b -> 's -> 's) ->
        table:LinkedMultiMap<'a,'b> -> state:'s -> 's
        when 'a : comparison and 'b : comparison
    val toArray :
      table:LinkedMultiMap<'a,'b> -> ('a * 'b) []
        when 'a : comparison and 'b : comparison
    val toList :
      table:LinkedMultiMap<'a,'b> -> ('a * 'b) list
        when 'a : comparison and 'b : comparison
    val tryFindKey :
      predicate:('a -> 'b -> bool) -> table:LinkedMultiMap<'a,'b> -> 'a option
        when 'a : comparison and 'b : comparison
    val tryPick :
      chooser:('a -> 'b -> 'c option) ->
        table:LinkedMultiMap<'a,'b> -> (seq<'a * 'b> -> 'c)
        when 'a : comparison and 'b : comparison
    val difference :
      table1:LinkedMultiMap<'a,'b> ->
        table2:LinkedMultiMap<'a,'b> -> LinkedMultiMap<'a,'b>
        when 'a : comparison and 'b : comparison
    val groupBy :
      projection:('a -> 'b -> 'key) ->
        table:LinkedMultiMap<'a,'b> -> LinkedMap<'key,LinkedSet<'a * 'b>>
        when 'a : comparison and 'b : comparison and 'key : comparison
    val sort :
      table:LinkedMultiMap<'a,'b> -> LinkedMultiMap<'a,'b>
        when 'a : comparison and 'b : comparison
    val sortBy :
      projection:('a -> 'b -> 'key) ->
        table:LinkedMultiMap<'a,'b> -> LinkedMultiMap<'a,'b>
        when 'a : comparison and 'b : comparison and 'key : comparison
    val sortWith :
      comparer:('a * 'b -> 'a * 'b -> int) ->
        table:LinkedMultiMap<'a,'b> -> LinkedMultiMap<'a,'b>
        when 'a : comparison and 'b : comparison
    val sortDescending :
      table:LinkedMultiMap<'a,'b> -> LinkedMultiMap<'a,'b>
        when 'a : comparison and 'b : comparison
    val sortByDescending :
      projection:('a -> 'b -> 'key) ->
        table:LinkedMultiMap<'a,'b> -> LinkedMultiMap<'a,'b>
        when 'a : comparison and 'b : comparison and 'key : comparison
    val length :
      table:LinkedMultiMap<'a,'b> -> int
        when 'a : comparison and 'b : comparison
  end

