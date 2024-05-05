module Dictionary

    type Dict = Node of bool * Map<char,Dict> 
    let empty () = Node (false,Map.empty) 


    let insert (s: string) (dict: Dict) = 
        let rec aux s (Node ( bool, dict)) = 
            match s with 
            | [] -> Node (bool,dict)
            | x::xs when List.length xs = 0 ->
                match Map.tryFind x dict with
                | Some (Node (bool',dict'))-> Node (bool, (Map.add x (Node (true,dict')) dict))
                | None -> (Node (bool, (Map.add x (Node (true, Map.empty)) dict)))
            | x::xs -> 
                match Map.tryFind x dict with
                | Some (Node (bool',dict')) -> Node (bool,(Map.add x (aux xs (Node (bool',dict')))) dict)
                | None -> Node(bool, Map.add x (aux xs (empty ())) dict)

        aux (Seq.toList s) dict

    let lookup (s:string) (dict: Dict) : bool = 
        let rec aux s' (Node (bool, dict: Map<char,Dict>)) =
            match s' with
            | [] -> bool
            | x::xs ->
                match Map.tryFind x dict with
                | Some dict' -> aux xs dict'
                | None -> false
                
        aux (Seq.toList s) dict

    let step c (Node(_, dict)) = 
        try 
            match Map.find c dict with
            | Node (bool' , dict') -> Some (bool', Node ( bool',dict'))
        with 
        | _ -> None

