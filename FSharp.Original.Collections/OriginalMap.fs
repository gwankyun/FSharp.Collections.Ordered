namespace Original.Collections
open System.Collections.Generic

type OriginalMap<'Key, 'Value when 'Key : comparison>(s: seq<'Key>, m: Map<'Key, 'Value>) as self =
    [<DefaultValue>]
    val mutable _Seq: seq<'Key>
    [<DefaultValue>]
    val mutable _Map: Map<'Key, 'Value>
    do
        self._Seq <- s
        self._Map <- m
    member this.Seq() =
        this._Seq
    member this.Map() =
        this._Map
    override this.ToString() =
        (this._Seq.ToString()) + "\n" + (this._Map.ToString())
//    new (elements: seq<'T>) =
//        new OriginalSet<'T>(elements, elements |> Set.ofSeq)



module OriginalMap =
    /// 返回其绑定已添加到给定映射的新映射。
    /// key: 输入键。
    /// value: 输入值。
    /// table: 输入映射。
    let add (key: 'Key) (value: 'T) (table: OriginalMap<'Key,'T>): OriginalMap<'Key,'T> when 'Key : comparison =
        let (s, m) =
            let (s, m) = (table.Seq(), table.Map())
            match m.ContainsKey(key) with
            | true -> (s, m.Add(key, value))
            | false -> (Seq.append s (Seq.singleton key), m.Add(key, value))
        new OriginalMap<'Key, 'T>(s, m)
    /// 返回从给定绑定中进行的新映射。
    /// elements: 键/值对的输入序列。
    let ofSeq (elements: seq<'Key * 'T>): OriginalMap<'Key,'T> when 'Key : comparison =
        let s = elements |> Seq.map (fun (k, _) -> k)
        let m = elements |> Map.ofSeq
        new OriginalMap<'Key, 'T>(s, m)
    /// 以键值对的可枚举序列形式查看集合。序列将按映射的键进行排序。
    /// table: 输入映射。
    let toSeq (table: OriginalMap<'Key,'T>): seq<'Key * 'T> when 'Key : comparison =
        table._Map |> Map.toSeq
    /// 返回从给定绑定中进行的新映射。
    /// elements: 键/值对的输入列表。
    let ofList (elements: ('Key * 'T) list): OriginalMap<'Key,'T> when 'Key : comparison =
        let s = elements |> List.map (fun (k, _) -> k)
        let m = elements |> Map.ofList
        new OriginalMap<'Key, 'T>(s, m)
    /// 返回从给定绑定中进行的新映射。
    /// elements: 键/值对的输入数组。
    let ofArray (elements: ('Key * 'T) []): OriginalMap<'Key,'T> when 'Key : comparison =
        let s = elements |> Array.map (fun (k, _) -> k)
        let m = elements |> Map.ofArray
        new OriginalMap<'Key, 'T>(s, m)
    /// 返回由映射中所有键/值对构成的列表。列表将按映射的键进行排序。
    /// table: 输入映射。
    let toList (table: OriginalMap<'Key,'T>): ('Key * 'T) list when 'Key : comparison =
        table._Map |> Map.toList
    /// 返回由映射中所有键/值对构成的数组。数组将按映射的键进行排序。
    /// table: 输入映射。
    let toArray (table: OriginalMap<'Key,'T>): ('Key * 'T) [] when 'Key : comparison =
        table._Map |> Map.toArray
    /// 映射是否为空？
    /// table: 输入映射。
    let isEmpty (table: OriginalMap<'Key,'T>): bool when 'Key : comparison =
        table._Map.IsEmpty
    /// 空映射。
    let empty<'Key, 'T  when 'Key : comparison> : OriginalMap<'Key,'T> when 'Key : comparison =
        let s = Seq.empty
        let m = Map.empty
        new OriginalMap<'Key, 'T>(s, m)
    /// 在映射中查找元素，如果映射中不存在任何绑定，则引发 KeyNotFoundException。
    /// key: 输入键。
    /// table: 输入映射。
    let find (key: 'Key) (table: OriginalMap<'Key,'T>): 'T when 'Key : comparison =
        Map.find key table._Map
    /// 搜索映射，寻找给定函数为其返回 Some 值的第一个元素。
    /// chooser: 用于从键/值对生成选项的函数。
    /// table: 输入映射。
    let tryPick (chooser: 'Key -> 'T -> 'U option) (table: OriginalMap<'Key,'T>): 'U option when 'Key : comparison =
        Seq.tryPick (fun x -> chooser x (Map.find x table._Map)) table._Seq
    /// 搜索映射，寻找给定函数为其返回 Some 值的第一个元素
    /// chooser: 用于从键/值对生成选项的函数。
    /// table: 输入映射。
    let pick (chooser: 'Key -> 'T -> 'U option) (table: OriginalMap<'Key,'T>): 'U when 'Key : comparison =
        Seq.pick (fun x -> chooser x (Map.find x table._Map)) table._Seq
    /// 对映射中的绑定进行折叠。
    /// folder: 用于更新给定输入键/值对的状态的函数。
    /// table: 输入映射。
    /// state: 初始状态。
    let foldBack (folder: 'Key -> 'T -> 'State -> 'State) (table: OriginalMap<'Key,'T>) (state: 'State): 'State when 'Key : comparison =
        let m = table._Map
        let s = table._Seq
        Seq.foldBack (fun t s -> folder t (Map.find t m) s) s state
    /// 对映射中的绑定进行聚合。
    /// folder: 用于更新给定输入键/值对的状态的函数。
    /// state: 初始状态。
    /// table: 输入映射。
    let fold (folder: 'State -> 'Key -> 'T -> 'State) (state: 'State) (table: OriginalMap<'Key,'T>): 'State when 'Key : comparison =
        let m = table._Map
        let s = table._Seq
        Seq.fold (fun s k -> folder s k (Map.find k m)) state s
    /// 将给定函数应用于字典中的每个绑定。
    /// action: 要应用于每个键/值对的函数。
    /// table: 输入映射。
    let iter (action: 'Key -> 'T -> unit) (table: OriginalMap<'Key,'T>): unit when 'Key : comparison =
        let m = table._Map
        let s = table._Seq
        Seq.iter (fun k -> action k (Map.find k m)) s
    /// 如果给定谓词为映射中的某个绑定返回 true，则返回 true。
    /// predicate: 要测试输入元素的函数。
    /// table: 输入映射。
    let exists (predicate: 'Key -> 'T -> bool) (table: OriginalMap<'Key,'T>): bool when 'Key : comparison =
        Map.exists predicate table._Map
    /// 生成一个新映射，其中仅包含给定谓词为其返回“true”的绑定。
    /// predicate: 用于测试键/值对的函数。
    /// table: 输入映射。
    let filter (predicate: 'Key -> 'T -> bool) (table: OriginalMap<'Key,'T>): OriginalMap<'Key,'T> when 'Key : comparison =
        let m = Map.filter predicate table._Map
        let s = Seq.filter (fun x -> Map.containsKey x m) table._Seq
        new OriginalMap<'Key, 'T>(s, m)
    /// 如果给定谓词为映射中的所有绑定都返回 true，则返回 true。
    /// predicate: 要测试输入元素的函数。
    /// table: 输入映射。
    let forall (predicate: 'Key -> 'T -> bool) (table: OriginalMap<'Key,'T>): bool when 'Key : comparison =
        let m = table._Map
        let s = table._Seq
        Seq.forall (fun k -> predicate k (Map.find k m)) s
    /// 生成一个新集合，其元素是将给定函数应用于集合的每个元素的结果。传递给函数的键指示所转换元素的键。
    /// mapping: 用于转换键/值对的函数。
    /// table: 输入映射。
    let map (mapping: 'Key -> 'T -> 'U) (table: OriginalMap<'Key,'T>): OriginalMap<'Key,'U> when 'Key : comparison =
        let s = table._Seq
        let m = Map.map mapping table._Map
        new OriginalMap<'Key, 'U>(s, m)
    /// 测试元素是否位于映射的域中。
    /// key: 输入键。
    /// table: 输入映射。
    let containsKey (key: 'Key) (table: OriginalMap<'Key,'T>): bool when 'Key : comparison =
        Map.containsKey key table._Map
    /// 生成两个新映射，一个包含给定谓词为其返回“true”的绑定，另一个包含其余绑定。
    /// predicate: 要测试输入元素的函数。
    /// table: 输入映射。
    let partition (predicate: 'Key -> 'T -> bool) (table: OriginalMap<'Key,'T>): OriginalMap<'Key,'T> * OriginalMap<'Key,'T> when 'Key : comparison =
        let (mt, mf) = Map.partition predicate table._Map
        let st = Seq.filter (fun x -> Map.containsKey x mt) table._Seq
        let sf = Seq.filter (fun x -> Map.containsKey x mf) table._Seq
        (new OriginalMap<'Key, 'T>(st, mt), new OriginalMap<'Key, 'T>(sf, mf))
    /// 从映射的域中移除元素。如果元素不存在，则不引发异常。
    /// key: 输入键。
    /// table: 输入映射。
    let remove (key: 'Key) (table: OriginalMap<'Key,'T>): OriginalMap<'Key,'T> when 'Key : comparison =
        let s = Seq.filter ((<>) key) table._Seq
        let m = Map.remove key table._Map
        new OriginalMap<'Key, 'T>(s, m)
    /// 在映射中查找元素，如果元素在映射的域中，则返回 Some 值，否则返回 None。
    /// key: 输入键。
    /// table: 输入映射。
    let tryFind (key: 'Key) (table: OriginalMap<'Key,'T>): 'T option when 'Key : comparison =
        Map.tryFind key table._Map
    /// 针对集合中的每个映射计算该函数。返回该函数为其返回“true”的第一个映射的键。如果不存在这样的元素，则引发 KeyNotFoundException。
    /// predicate: 要测试输入元素的函数。
    /// table: 输入映射。
    let findKey (predicate: 'Key -> 'T -> bool) (table: OriginalMap<'Key,'T>): 'Key when 'Key : comparison =
        Map.findKey predicate table._Map
    /// 返回满足给定谓词的集合中第一个映射的键。如果不存在此类元素，则返回“None”。
    /// predicate: 要测试输入元素的函数。
    /// table: 输入映射。
    let tryFindKey (predicate: 'Key -> 'T -> bool) (table: OriginalMap<'Key,'T>): 'Key option when 'Key : comparison =
        Map.tryFindKey predicate table._Map

