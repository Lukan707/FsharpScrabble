// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison> = Temp of Map<'a,uint>

    let empty = Temp Map.empty;;

    let add (a : 'a) (n : uint32) (Temp s : MultiSet<'a>) : MultiSet<'a> = 
        Temp (s |> Map.add a  (s.TryFind a |> Option.defaultValue 0u |>  (+) n ))   

    let fold (f : 'b -> 'a -> uint32 -> 'b) (acc : 'b) (Temp s : MultiSet<'a>) = 
        s |> Map.fold f acc
