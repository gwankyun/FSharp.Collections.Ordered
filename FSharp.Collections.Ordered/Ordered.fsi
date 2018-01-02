

namespace FSharp.Collections.Ordered
  type Class1 =
    class
      new : unit -> Class1
      member X : string
    end

namespace OrderedCollection
  type SkipList<'a> =
    class
      new : data:System.Collections.Immutable.ImmutableList<System.Collections.Immutable.ImmutableList<'a>> *
            comparer:('a -> 'a -> int) -> SkipList<'a>
      member Comparer : ('a -> 'a -> int)
      member
        Data : System.Collections.Immutable.ImmutableList<System.Collections.Immutable.ImmutableList<'a>>
    end
  module SkipList = begin
    val randomLevel : unit -> int
    val emptyWith : comparer:('a -> 'a -> int) -> SkipList<'a>
    val isEmpty : list:SkipList<'a> -> bool
    val add : value:'a -> list:SkipList<'a> -> SkipList<'a>
  end
  module Seq = begin
    val ( |IsEmpty|_| ) : set:seq<'a> -> unit option
  end
  type OrderedSet<[<EqualityConditionalOnAttribute ()>] 'k
                    when [<EqualityConditionalOnAttribute ()>] 'k : comparison> =
    class
      interface System.IComparable
      new : first:'k option * map:Map<'k,('k * 'k)> * last:'k option ->
              OrderedSet<'k>
      override Equals : y:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member First : 'k option
      member Last : 'k option
      member Map : Map<'k,('k * 'k)>
    end
  module OrderedSet = begin
    val isEmpty : set:OrderedSet<'k> -> bool when 'k : comparison
    val ( |IsEmpty|_| ) : set:OrderedSet<'k> -> unit option when 'k : comparison
    val add :
      value:'k -> set:OrderedSet<'k> -> OrderedSet<'k> when 'k : comparison
    val contains : value:'k -> set:OrderedSet<'k> -> bool when 'k : comparison
    val count : set:OrderedSet<'k> -> int when 'k : comparison
    val empty<'k when 'k : comparison> : OrderedSet<'k> when 'k : comparison
    val fold :
      folder:('s -> 't -> 's) -> state:'s -> set:OrderedSet<'t> -> 's
        when 't : comparison
    val remove :
      value:'k -> set:OrderedSet<'k> -> OrderedSet<'k> when 'k : comparison
    val difference :
      set1:OrderedSet<'k> -> set2:OrderedSet<'k> -> OrderedSet<'k>
        when 'k : comparison
    val exists :
      predicate:('k -> bool) -> set:OrderedSet<'k> -> bool when 'k : comparison
    val filter :
      predicate:('k -> bool) -> set:OrderedSet<'k> -> OrderedSet<'k>
        when 'k : comparison
    val iter :
      action:('k -> unit) -> set:OrderedSet<'k> -> OrderedSet<'a>
        when 'k : comparison and 'a : comparison
    val forall :
      predicate:('k -> bool) -> set:OrderedSet<'k> -> bool when 'k : comparison
    val intersect :
      set1:OrderedSet<'k> -> set2:OrderedSet<'k> -> OrderedSet<'k>
        when 'k : comparison
    val intersectMany :
      sets:seq<OrderedSet<'k>> -> OrderedSet<'k> when 'k : comparison
    val isSubset :
      set1:OrderedSet<'k> -> set2:OrderedSet<'k> -> bool when 'k : comparison
    val isProperSubset :
      set1:OrderedSet<'k> -> set2:OrderedSet<'k> -> bool when 'k : comparison
    val isSuperset :
      set1:OrderedSet<'k> -> set2:OrderedSet<'k> -> bool when 'k : comparison
    val isProperSuperset :
      set1:OrderedSet<'k> -> set2:OrderedSet<'k> -> bool when 'k : comparison
    val map :
      mapping:('a -> 'b) -> set:OrderedSet<'a> -> OrderedSet<'b>
        when 'a : comparison and 'b : comparison
    val maxElement : set:OrderedSet<'a> -> 'a when 'a : comparison
    val minElement : set:OrderedSet<'a> -> 'a when 'a : comparison
    val ofArray : array:'k [] -> OrderedSet<'k> when 'k : comparison
    val ofList : elements:'k list -> OrderedSet<'k> when 'k : comparison
    val ofSeq : elements:seq<'k> -> OrderedSet<'k> when 'k : comparison
    val partition :
      predicate:('k -> bool) ->
        set:OrderedSet<'k> -> OrderedSet<'k> * OrderedSet<'k>
        when 'k : comparison
    val singleton : value:'k -> OrderedSet<'k> when 'k : comparison
    val foldBack :
      folder:('t -> 's -> 's) -> set:OrderedSet<'t> -> state:'s -> 's
        when 't : comparison
    val toList : set:OrderedSet<'k> -> 'k list when 'k : comparison
    val toArray : set:OrderedSet<'k> -> 'k [] when 'k : comparison
    val toSeq : set:OrderedSet<'k> -> seq<'k> when 'k : comparison
    val union :
      set1:OrderedSet<'k> -> set2:OrderedSet<'k> -> OrderedSet<'k>
        when 'k : comparison
    val unionMany :
      sets:seq<OrderedSet<'k>> -> OrderedSet<'k> when 'k : comparison
  end

namespace OrderedCollection
  type OrderedMap<[<EqualityConditionalOnAttribute ()>] 'k,'v
                    when [<EqualityConditionalOnAttribute ()>] 'k : comparison and
                         'v : comparison> =
    class
      interface System.IComparable
      new : first:'k option * table:Map<'k,('k * 'v * 'k)> * last:'k option ->
              OrderedMap<'k,'v>
      override Equals : y:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member First : 'k option
      member Last : 'k option
      member Map : Map<'k,('k * 'v * 'k)>
    end
  module OrderedMap = begin
    val isEmpty :
      set:OrderedMap<'k,'v> -> bool when 'k : comparison and 'v : comparison
    val ( |IsEmpty|_| ) :
      set:OrderedMap<'k,'v> -> unit option
        when 'k : comparison and 'v : comparison
    val add :
      key:'k -> value:'v -> set:OrderedMap<'k,'v> -> OrderedMap<'k,'v>
        when 'k : comparison and 'v : comparison
    val containsKey :
      key:'k -> set:OrderedMap<'k,'v> -> bool
        when 'k : comparison and 'v : comparison
    val count :
      set:OrderedMap<'k,'v> -> int when 'k : comparison and 'v : comparison
    val empty<'k,'v when 'k : comparison and 'v : comparison> :
      OrderedMap<'k,'v> when 'k : comparison and 'v : comparison
    val fold :
      folder:('s -> 'k -> 'v -> 's) -> state:'s -> set:OrderedMap<'k,'v> -> 's
        when 'k : comparison and 'v : comparison
    val remove :
      key:'k -> set:OrderedMap<'k,'v> -> OrderedMap<'k,'v>
        when 'k : comparison and 'v : comparison
    val exists :
      predicate:('k -> 'v -> bool) -> set:OrderedMap<'k,'v> -> bool
        when 'k : comparison and 'v : comparison
    val filter :
      predicate:('k -> 'v -> bool) -> set:OrderedMap<'k,'v> -> OrderedMap<'k,'v>
        when 'k : comparison and 'v : comparison
    val find :
      key:'k -> set:OrderedMap<'k,'v> -> 'v
        when 'k : comparison and 'v : comparison
    val tryFind :
      key:'k -> set:OrderedMap<'k,'v> -> 'v option
        when 'k : comparison and 'v : comparison
    val findKey :
      predicate:('k -> 'v -> bool) -> set:OrderedMap<'k,'v> -> 'k
        when 'k : comparison and 'v : comparison
    val tryFindKey :
      predicate:('k -> 'v -> bool) -> set:OrderedMap<'k,'v> -> 'k option
        when 'k : comparison and 'v : comparison
    val iter :
      action:('k -> 'v -> unit) -> set:OrderedMap<'k,'v> -> OrderedMap<'a,'b>
        when 'k : comparison and 'v : comparison and 'a : comparison and
             'b : comparison
    val forall :
      predicate:('k -> 'v -> bool) -> set:OrderedMap<'k,'v> -> bool
        when 'k : comparison and 'v : comparison
    val map :
      mapping:('k -> 'v -> 'u) -> set:OrderedMap<'k,'v> -> OrderedMap<'k,'u>
        when 'k : comparison and 'v : comparison and 'u : comparison
    val addTuple :
      s:OrderedMap<'a,'b> -> k:'a * v:'b -> OrderedMap<'a,'b>
        when 'a : comparison and 'b : comparison
    val ofArray :
      array:('k * 'v) [] -> OrderedMap<'k,'v>
        when 'k : comparison and 'v : comparison
    val ofList :
      elements:('k * 'v) list -> OrderedMap<'k,'v>
        when 'k : comparison and 'v : comparison
    val ofSeq :
      elements:seq<'k * 'v> -> OrderedMap<'k,'v>
        when 'k : comparison and 'v : comparison
    val partition :
      predicate:('k -> 'v -> bool) ->
        set:OrderedMap<'k,'v> -> OrderedMap<'k,'v> * OrderedMap<'k,'v>
        when 'k : comparison and 'v : comparison
    val foldBack :
      folder:('k -> 'v -> 's -> 's) -> set:OrderedMap<'k,'v> -> state:'s -> 's
        when 'k : comparison and 'v : comparison
    val toList :
      set:OrderedMap<'k,'v> -> ('k * 'v) list
        when 'k : comparison and 'v : comparison
    val toArray :
      set:OrderedMap<'k,'v> -> ('k * 'v) []
        when 'k : comparison and 'v : comparison
    val toSeq :
      set:OrderedMap<'k,'v> -> seq<'k * 'v>
        when 'k : comparison and 'v : comparison
    val update :
      key:'k -> value:'v -> set:OrderedMap<'k,'v> -> OrderedMap<'k,'v>
        when 'k : comparison and 'v : comparison
    val updateWith :
      f:('v -> 'v option) ->
        key:'k -> set:OrderedMap<'k,'v> -> OrderedMap<'k,'v>
        when 'v : comparison and 'k : comparison
  end

namespace OrderedCollection
  type OrderedMultiMap<'k,'v when 'k : comparison and 'v : comparison> =
    class
      new : map:OrderedMap<'k,OrderedSet<'v>> -> OrderedMultiMap<'k,'v>
      override ToString : unit -> string
      member First : 'k option
      member Last : 'k option
      member Map : OrderedMap<'k,OrderedSet<'v>>
    end
  module OrderedMultiMap = begin
    val isEmpty :
      set:OrderedMultiMap<'k,'v> -> bool
        when 'k : comparison and 'v : comparison
    val ( |IsEmpty|_| ) :
      set:OrderedMultiMap<'k,'v> -> unit option
        when 'k : comparison and 'v : comparison
    val ofOrderedMap :
      map:OrderedMap<'k,OrderedSet<'v>> -> OrderedMultiMap<'k,'v>
        when 'k : comparison and 'v : comparison
    val empty<'k,'v when 'k : comparison and 'v : comparison> :
      OrderedMultiMap<'k,'v> when 'k : comparison and 'v : comparison
    val add :
      key:'k -> value:'v -> set:OrderedMultiMap<'k,'v> -> OrderedMultiMap<'k,'v>
        when 'k : comparison and 'v : comparison
    val containsKey :
      key:'k -> set:OrderedMultiMap<'k,'v> -> bool
        when 'k : comparison and 'v : comparison
    val count :
      set:OrderedMultiMap<'k,'v> -> int when 'k : comparison and 'v : comparison
    val fold :
      folder:('s -> 'k -> 'v -> 's) ->
        state:'s -> set:OrderedMultiMap<'k,'v> -> 's
        when 'k : comparison and 'v : comparison
    val remove :
      key:'k -> set:OrderedMultiMap<'k,'v> -> OrderedMultiMap<'k,'v>
        when 'k : comparison and 'v : comparison
    val exists :
      predicate:('k -> 'v -> bool) -> set:OrderedMultiMap<'k,'v> -> bool
        when 'k : comparison and 'v : comparison
    val filter :
      predicate:('k -> 'v -> bool) ->
        set:OrderedMultiMap<'k,'v> -> OrderedMultiMap<'k,'v>
        when 'k : comparison and 'v : comparison
    val find :
      key:'k -> set:OrderedMultiMap<'k,'v> -> OrderedSet<'v>
        when 'k : comparison and 'v : comparison
    val tryFind :
      key:'k -> set:OrderedMultiMap<'k,'v> -> OrderedSet<'v> option
        when 'k : comparison and 'v : comparison
    val iter :
      action:('k -> 'v -> unit) ->
        set:OrderedMultiMap<'k,'v> -> OrderedMultiMap<'a,'b>
        when 'k : comparison and 'v : comparison and 'a : comparison and
             'b : comparison
    val forall :
      predicate:('k -> 'v -> bool) -> set:OrderedMultiMap<'k,'v> -> bool
        when 'k : comparison and 'v : comparison
    val map :
      mapping:('k -> 'v -> 'u) ->
        set:OrderedMultiMap<'k,'v> -> OrderedMultiMap<'k,'u>
        when 'k : comparison and 'v : comparison and 'u : comparison
    val addTuple :
      s:OrderedMultiMap<'a,'b> -> k:'a * v:'b -> OrderedMultiMap<'a,'b>
        when 'a : comparison and 'b : comparison
    val ofArray :
      array:('k * 'v) [] -> OrderedMultiMap<'k,'v>
        when 'k : comparison and 'v : comparison
    val ofList :
      elements:('k * 'v) list -> OrderedMultiMap<'k,'v>
        when 'k : comparison and 'v : comparison
    val ofSeq :
      elements:seq<'k * 'v> -> OrderedMultiMap<'k,'v>
        when 'k : comparison and 'v : comparison
    val partition :
      predicate:('k -> 'v -> bool) ->
        set:OrderedMultiMap<'k,'v> ->
          OrderedMultiMap<'k,'v> * OrderedMultiMap<'k,'v>
        when 'k : comparison and 'v : comparison
    val foldBack :
      folder:('k -> 'v -> 's -> 's) ->
        set:OrderedMultiMap<'k,'v> -> state:'s -> 's
        when 'k : comparison and 'v : comparison
    val toList :
      set:OrderedMultiMap<'k,'v> -> ('k * 'v) list
        when 'k : comparison and 'v : comparison
    val toArray :
      set:OrderedMultiMap<'k,'v> -> ('k * 'v) []
        when 'k : comparison and 'v : comparison
    val toSeq :
      set:OrderedMultiMap<'k,'v> -> seq<'k * 'v>
        when 'k : comparison and 'v : comparison
  end



