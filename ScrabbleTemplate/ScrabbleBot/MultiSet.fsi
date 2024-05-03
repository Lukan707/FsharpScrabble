// Insert your MultiSet.fsi file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison>

    val empty : MultiSet<'a>
    val add   : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>
    val addSingle : 'a -> MultiSet<'a> -> MultiSet<'a>
    val fold  : ('b -> 'a -> uint32 -> 'b) -> 'b -> MultiSet<'a> -> 'b
    val remove : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>
    val removeSingle : 'a -> MultiSet<'a> -> MultiSet<'a>
    val isEmpty : MultiSet<'a> -> bool
    val keysToList : MultiSet<'a> -> List<'a>
