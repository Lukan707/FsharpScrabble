module Dict

    type Dict = Node of Map<char,Dict> * bool
    let empty ()   = Node (Map.empty,false) 

    let insert (s: string) (dict : Dict) = 
        let rec aux s (Node (dict,bool)) = 
            match s with
                | [] -> Node (dict, false)
                | x::xs when List.length xs = 1 -> 
                    match Map.containsKey x dict with
                    | true -> aux xs (Node (dict, true))
                    | false -> aux xs (Node ((dict.Add (x,empty) ) , true))
                | x::xs ->
                    match Map.containsKey x dict with
                    | true -> aux xs (Node (dict, bool))
                    | false -> aux xs (Node ((dict.Add (x,empty)), false))

        aux (Seq.toList s) dict

    let lookup (s:string) (dict: Dict) = 
        let rec aux s (Node (dict, bool)) =
            match s with
            | [] -> bool
            | x::xs when List.length xs = 1 ->
                match Map.containsKey x dict with
                | true -> bool
                | false -> false
            | x::xs ->
                match Map.containsKey x dict with
                | true -> aux xs (Node (dict, bool))
                | false -> false
        aux (Seq.toList s) dict

    let step = failwith "Not implemented"