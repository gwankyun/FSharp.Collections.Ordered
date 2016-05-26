# FSharp.Original.Collections
F#的可以記住插入順序的集合

依賴FSharp.Extension庫

目前有：

1. LinkedSet
	
	實現Set模塊函數，LinkedSet<'a>相當於'a seq * Set<'a>

2. LinkedMap

	實現Map模塊函數,LinkedMap<'a, 'b>相當於'a seq * Map<'a, 'b>

3. LinkedMultiMap

	LinkedMultiMap<'a, 'b>相當於LinkedMap<'a, LinkedSet<'b>>