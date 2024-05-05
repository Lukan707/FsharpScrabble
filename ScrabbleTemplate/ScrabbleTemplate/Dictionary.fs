module Dictionary

    type Dict = Node of Map<char,Dict> * bool
    let empty () = Node (Map.empty,false) 


    let insert (s: string) (dict: Dict) = 
        let rec aux s (Node (dict, bool)) = 
            match s with 
            | [] -> Node (dict, bool)
            | x::xs when List.length xs = 0 ->
                match Map.containsKey x dict with
                | true -> Node (dict, true)
                | false -> (Node ((Map.add x (empty ()) dict), true))
            | x::xs -> 
                match Map.tryFind x dict with
                | Some (dict') -> Node( (Map.add x (aux xs dict')  dict), bool)
                | None -> Node(Map.add x (aux xs (empty ())) dict, bool)

        aux (Seq.toList s) dict

    let lookup (s:string) (dict: Dict) = 
        let rec aux s (Node (dict: Map<char,Dict>, bool)) =
            match s with
            | [] -> bool
            | x::xs ->
                match Map.tryFind x dict with
                | Some (dict') -> aux xs dict'
                | None -> false
        aux (Seq.toList s) dict

    let step c (Node(dict, bool)) = 
        try 
            match Map.find c dict with
            | Node (dict', bool') -> Some (bool', Node (dict', bool'))
        with 
        | _ -> None

