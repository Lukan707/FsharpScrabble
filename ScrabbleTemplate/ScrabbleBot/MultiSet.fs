// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison> = Temp of Map<'a,uint>

    let empty = Temp Map.empty;;

    let add (a : 'a) (n : uint32) (Temp s : MultiSet<'a>) : MultiSet<'a> = 
        Temp (s |> Map.add a  (s.TryFind a |> Option.defaultValue 0u |>  (+) n ))   

    let addSingle (element : 'a) (Temp map : MultiSet<'a>) : MultiSet<'a> = add element 1u (Temp (map))

    let fold (f : 'b -> 'a -> uint32 -> 'b) (acc : 'b) (Temp s : MultiSet<'a>) = 
        s |> Map.fold f acc

    let remove (a : 'a) (n : uint32) (Temp s  : MultiSet<'a>) : MultiSet<'a> = 
        if s.TryFind a |> Option.defaultValue 0u > n then 
            Temp (s |> Map.add a  ((s.TryFind a |> Option.defaultValue 0u )-n)) else Temp (s.Remove a)

    let removeSingle (a : 'a) (s : MultiSet<'a>) : MultiSet<'a> = 
        remove a 1u s

    let isEmpty (Temp s : MultiSet<'a>) : bool = s.IsEmpty

    // Returns list of piece id's
    let keysToList (Temp s : MultiSet<'a>) : List<'a> = Map.keys s |> Seq.cast |> List.ofSeq