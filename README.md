# FSharp.Original.Collections
F#的可以記住插入順序的集合

依賴FSharp.Extension庫

目前有：

1. OriginalSet
	
	實現Set模塊函數，OriginalSet<'a>相當於'a seq * Set<'a>

2. OriginalMap

	實現Map模塊函數,OriginalMap<'a, 'b>相當於'a seq * Map<'a, 'b>

3. OriginalMultiMap

	OriginalMultiMap<'a, 'b>相當於OriginalMap<'a, OriginalSet<'b>>