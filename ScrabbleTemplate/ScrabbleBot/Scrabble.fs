namespace Smooth_Operator

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System
open Dictionary

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        playedMoves   : Map<coord, (char * int)>
    }

    let mkState b d pn h m = {board = b; dict = d;  playerNumber = pn; hand = h; playedMoves = m}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand

module Scrabble =
    open System.Threading

    let playGame cstream (pieces : Map<uint32, tile>) (st : State.state) =

        let charToID = 
            Map ['A', 1u; 'B', 2u; 'C', 3u; 'D', 4u; 'E', 5u; 'F', 6u; 'G', 7u; 'H', 8u; 'I', 9u; 'J', 10u; 
            'K', 11u; 'L', 12u; 'M', 13u; 'N', 14u; 'O', 15u; 'P', 16u; 'Q', 17u; 'R', 18u; 'S', 19u; 'T', 20u;
            'U', 21u; 'V', 22u; 'W', 23u; 'X', 24u; 'Y', 25u; 'Z', 26u]

        let rec aux (st : State.state) =

            Print.printHand pieces (State.hand st)

            let findMaxLength coord direction =

            
                debugPrint("running findMaxLength " + direction +  coord.ToString() + "\n")
                let rec findSameLine coord''' count =
                      match count with
                      | x when x = 7 -> 7 
                      | _ ->       
                            match direction with
                                | "r" ->
                                    let newCoord = (fst coord''' + 1, snd coord''')
                                    match st.playedMoves |> Map.tryFind newCoord with
                                    | Some _ -> count - 2
                                    | None -> findSameLine newCoord (count+1)
                                | "d" -> 
                                    let newCoord = (fst coord''', snd coord''' + 1)
                                    debugPrint(newCoord.ToString()+ "new Coord \n")
                                    match st.playedMoves |> Map.tryFind newCoord with
                                    | Some _ -> count - 2
                                    | None -> findSameLine newCoord (count+1)   
                              
                debugPrint("FindSame\n")
                let globalCount = findSameLine coord 0
                debugPrint(globalCount.ToString() + "\n")

                let rec findLineAbove coord'' count =
                      match count with
                      | x when globalCount < 0 -> 0
                      | x when globalCount = count -> count 
                      | _ -> 
                        match st.playedMoves |> Map.tryFind coord'' with
                        | Some _ -> count - 2
                        | None -> 
                            match direction with
                                | "r" ->
                                    let newCoord = (fst coord'' + 1, snd coord'')
                                    match st.playedMoves |> Map.tryFind newCoord with
                                    | Some _ -> count - 2
                                    | None -> findLineAbove newCoord (count+1)
                                | "d" -> 
                                    let newCoord = (fst coord'', snd coord'' + 1)
                                    debugPrint(newCoord.ToString()+ "new Coord \n")
                                    match st.playedMoves |> Map.tryFind newCoord with
                                    | Some _ -> count - 2
                                    | None -> findLineAbove newCoord (count+1) 
                                    
                debugPrint("FindAbove\n")
                let globalCount = globalCount - findLineAbove (fst coord, snd coord - 1) 0
                debugPrint(globalCount.ToString() + "\n")

                let rec findLineBelow coord' count =
                      match count with
                      | x when globalCount = count -> count 
                      | _ -> 
                         match direction with
                                | "r" ->
                                    // update coord
                                    let newCoord = (fst coord' + 1, snd coord')
                                    match st.playedMoves |> Map.tryFind newCoord with
                                    | x when globalCount < 0 -> 0
                                    | Some _ -> count - 2 
                                    | None -> findLineBelow newCoord (count+1)
                                | "d" -> 
                                    let newCoord = (fst coord', snd coord' + 1)
                                    match st.playedMoves |> Map.tryFind newCoord with
                                    | x when globalCount < 0 -> 0
                                    | Some _ -> count - 2 
                                    | None -> findLineBelow newCoord (count+1)
                           
                
                
                debugPrint("FindBelow\n")
                let globalCount = findLineBelow (fst coord, snd coord + 1) 0
                debugPrint(globalCount.ToString() + "\n")

                let checkCoordBefore count = 
                    match direction with
                    | "r" -> 
                        match st.playedMoves |> Map.tryFind (fst coord - 1, snd coord) with
                            | None -> count
                            | Some _ -> 0
                    | "d" -> 
                        match st.playedMoves |> Map.tryFind (fst coord, snd coord - 1) with
                            | None -> count
                            | Some _ -> 0

                debugPrint("FindBefore\n")
                let globalCount = checkCoordBefore globalCount
                debugPrint(globalCount.ToString() + "\n")
                globalCount
            
            let chooseRandomCoord coordinates = 
                debugPrint("Random Coord\n")
                match Seq.length coordinates with
                | 0 -> (0,0),0
                | x -> 
                    let index = System.Random().Next(0,x)
                    coordinates |> Seq.item index , index


            (* Values we now are right

            Finding the current tile and char.
            let tileID =  List.item index (MultiSet.keysToList hand')
            let currentTile = Map.find tileID pieces
            let currentChar =  fst( List.head (Seq.toList currentTile))


            Adding to the accumulator
            let coord'' = (fst coord' + 1), (snd coord')
            let stringCoord = (fst coord').ToString() + " " + (snd coord').ToString()
            let tilePoint = snd (List.head (Seq.toList currentTile))
            let stringTile = tileID.ToString() + currentChar.ToString() + tilePoint.ToString()
            let stringCoordTile = stringCoord + " " + stringTile
            let acc = acc + " " + stringCoordTile                                 *)


            let combinate list size =
                let rec aux acc size set = seq {
                    match size, set with
                    | n, x::xs ->
                        if n > 0 then yield! aux (x::acc) (n - 1) xs 
                        if n >= 0 then yield! aux acc n xs
                    | 0, [] -> yield acc
                    | _, [] -> ()
                }

                Seq.toList (aux [] size list)
                

            let permutate list  =
                debugPrint("running permutate\n")
                let rec aux e = function
                    | [] -> [[e]]
                    | x::xs as list -> (e::list)::(aux e xs |> List.map (fun xs' -> x::xs'))

                List.fold (fun accum x -> List.collect (aux x) accum) [[]] list


            let rec createListOfHand hand = 
                debugPrint("running createListOfHand\n")
                let list = MultiSet.toList hand
                let rec aux (list : List<uint32 * uint>) (acc : List<Char>) = 
                    match list with
                    | [] -> acc
                    | x::xs ->       
                        let currentTile = Map.find (fst x) pieces
                        let currentChar =  fst( List.head (Seq.toList currentTile))
                        let rec aux' n' acc' =
                            match n' with
                            | 1u -> currentChar :: acc'
                            | x -> aux' (n' - 1u) (currentChar :: acc')
                        aux xs (aux' (snd x) acc)

                aux list []

            
            let charListToString list =
                list |> List.toArray |> (fun s -> System.String s)


            let parseWordToMoveString string direction startCoord =
                debugPrint("running parseWordToMoveString\n")
                let rec aux listOfChars coord acc =
                    match listOfChars with
                    | [] -> RegEx.parseMove(acc)
                    | x::xs ->
                        // add too accumulator
                        let stringCoord = (fst coord).ToString() + " " + (snd coord).ToString()
                        let tileID = Map.find x charToID
                        let currentTile = Map.find tileID pieces
                        let tilePoint = snd (List.head (Seq.toList currentTile))
                        let stringTile = tileID.ToString() + x.ToString() + tilePoint.ToString()
                        let acc = acc + " " + stringCoord + " " + stringTile
                        //debugPrint ("Acc: " + acc + "\n")
                        match direction with
                        | "r" ->
                            // update coord
                            aux xs ((fst coord) + 1, snd coord) acc
                        | "d" -> 
                            aux xs (fst coord, (snd coord) + 1) acc

                aux (List.ofSeq string) startCoord ""

            let findValidPermutation list direction coord dict char =
                debugPrint("runing findValidPermutation\n")
                //debugPrint(list.ToString())
                //debugPrint(" " + (List.length list).ToString())
                //debugPrint("\n")
                let rec aux list' = 
                    match list' with
                    | [] -> SMPass
                    | x::xs ->
                        match char with 
                        | y when y = '%' -> 
                            match Dictionary.lookup (charListToString x) dict with
                            | true -> 
                                debugPrint("match on word: " + charListToString x + "\n")
                                SMPlay (parseWordToMoveString x direction coord)
                            | false -> 
                                //debugPrint(charListToString x + "\n")
                                aux xs
                        | _ -> 
                                let x' = char :: x
                                debugPrint(char.ToString())
                                match Dictionary.lookup (charListToString x') dict with
                                | true -> 
                                    debugPrint("match on word: " + charListToString x + "\n")
                                    SMPlay (parseWordToMoveString x direction coord)
                                | false -> 
                                    //debugPrint(charListToString x + "\n")
                                    aux xs

                aux list


            // let goTroughTrie (hand : MultiSet.MultiSet<uint32>) coord dict maxLength : ServerMessage =
            //     debugPrint("running goTroughTrie\n")
            //     let rec auxTrie (hand' : MultiSet.MultiSet<uint32>) (coord' : int * int) dict' acc length index = 
            //         debugPrint("auxTrie køres: " + index.ToString() + " " + hand'.ToString() + " " + length.ToString() + "\n")
            //         // Check if there are any more letters to start a word with
            //         match MultiSet.isEmpty hand' with
            //         | true -> 
            //             debugPrint("hand is empty \n")
            //             auxTrie hand coord' dict' acc 0 (index + 1)
            //         | false -> 
            //             debugPrint("hand is not empty \n")
            //             let index' = if (index + length) >= 7 then 0 else index
            //             debugPrint("ahahahah Index set to " + index.ToString() + "\n")
            //             let tileID =  List.item index' (MultiSet.keysToList hand')
            //             debugPrint("Index set to 0, tileID: " + tileID.ToString() + "\n")
            //             let currentTile = Map.find tileID pieces
            //             // TODO: Convert currentId (fst currentTile) to char in call of Dictionary.step
            //             debugPrint("emil har ikke ret" + currentTile.ToString() + "\n")
            //             let currentChar =  fst( List.head (Seq.toList currentTile))
            //             match Dictionary.step currentChar dict' with
            //             | Some (bool', dict'') ->
            //                 debugPrint("\n\n bool: " + bool'.ToString() + "\n\n")
            //                 debugPrint("\n\n acc " + acc + "\n\n")
            //                 match length > maxLength with
            //                 | false ->
            //                     // Note: when implemeting different directions, match on direction and add to coord accordingly
            //                     let coord'' = (fst coord' + 1), (snd coord')
            //                     let stringCoord = (fst coord').ToString() + " " + (snd coord').ToString()
            //                     let tilePoint = snd (List.head (Seq.toList currentTile))
            //                     let stringTile = tileID.ToString() + currentChar.ToString() + tilePoint.ToString()
            //                     let stringCoordTile = stringCoord + " " + stringTile
            //                     let acc = acc + " " + stringCoordTile
            //                     match bool' with
            //                     | true -> SMPlay (RegEx.parseMove acc)
            //                     | false -> 
            //                         debugPrint("nope \n")
            //                         auxTrie (MultiSet.removeSingle tileID hand') coord'' dict'' acc (length + 1) 0 
            //                 | true -> auxTrie hand coord' dict' acc 0 (index + 1)
            //             | None -> 
            //                 debugPrint("there is no dict to current char: " + currentChar.ToString() + " " + index.ToString() + "\n")
            //                 match auxTrie (MultiSet.removeSingle tileID hand') coord' dict' acc (length + 1) index with
            //                     | SMPass -> 
            //                         let tileID =  List.item (index+1) (MultiSet.keysToList hand')
            //                         auxTrie (MultiSet.removeSingle tileID hand') coord' dict' acc (length) (index + 1)
            //                     | SMPlay x -> SMPlay x

            //     auxTrie hand coord dict "" 1 0    


            // let findMove (hand : MultiSet.MultiSet<uint32>) maxLength coord : ServerMessage  = 
            //     debugPrint("running findMove\n")
            //     let rec auxFindMove coord'  hand' =
            //         // Checking if there already is a letter at coord (if we are plyaing first move or not)
            //         match Map.tryFind coord st.playedMoves with
            //             | None -> 
            //                 // See if the starting letter matching a valid move, if not, try starting with the next letter in the hand  
            //                 match goTroughTrie hand coord st.dict maxLength with
            //                 | SMPlay move -> SMPlay move
            //                 | SMPass -> SMPass
            //             | Some char ->
            //                 //  Test if current char has children in trie
            //                 match Dictionary.step (fst char) st.dict with
            //                     | None -> SMPass
            //                     | Some (bool,dict) -> goTroughTrie hand coord dict maxLength
            //     // Check if we have found a length that can result in a valid word
            //     match maxLength <= 1 with
            //         | true -> SMPass
            //         | _ -> auxFindMove coord hand

            let findMove (hand : MultiSet.MultiSet<uint32>) maxLength coord direction : ServerMessage  = 
                debugPrint("running findMove"+ coord.ToString() +  "\n ")
                let rec auxFindMove length =
                    // Checking if there already is a letter at coord (if we are plyaing first move or not)
                    match Map.tryFind coord st.playedMoves with
                        | None -> 
                            debugPrint("None case\n")
                            let charListFromHand = createListOfHand hand
                            let combinationList = combinate charListFromHand length
                            match findValidPermutation combinationList direction (0,0) st.dict '%'with
                            | SMPlay move -> SMPlay move
                            | SMPass when length < 2 -> SMPass
                            | SMPass -> auxFindMove (length - 1)
                        | Some char ->
                            let charListFromHand = createListOfHand hand
                            let combinationList = combinate charListFromHand length
                            //  Test if current char has children in trie
                            match Dictionary.step (fst char) st.dict with
                                | None -> SMPass
                                | Some (bool,dict) -> 
                                    match findValidPermutation combinationList direction coord dict (fst char) with
                                    | SMPlay move -> SMPlay move
                                    | SMPass when length < 2 -> SMPass
                                    | SMPass -> auxFindMove (length - 1)
                //Check if we have found a length that can result in a valid word
                match maxLength <= 1 with
                    | true -> SMPass
                    | _ -> auxFindMove 7
            

            let mkMove () : ServerMessage =
                debugPrint("running make move\n")
                match Map.count(st.playedMoves) with
                    | 0 -> findMove st.hand 7 (0,0) "r"
                    | _ -> 
                        let rec auxNotBaseCase coordinates =
                            let coord , index = chooseRandomCoord coordinates
                            match findMove st.hand (findMaxLength coord "r") (fst coord , snd coord) "r" with
                            | SMPlay move -> SMPlay move
                            | SMPass -> 
                                match findMove st.hand (findMaxLength coord "d") (fst coord, snd coord ) "d" with
                                    | SMPlay move -> SMPlay move
                                    | SMPass ->
                                        match Seq.length coordinates with
                                        | 0 -> SMPass
                                        | _ -> auxNotBaseCase (coordinates |> Seq.removeAt index) 
                                
                        auxNotBaseCase (Map.keys st.playedMoves)
            
                // check if playedMoves is 0
                    // true : find word in hand, and place on (0,0)

                // check if coordinates are empty 

                    // true : pass turn

                // choose random coordinate in playedMoves

                    // process A

                    // check free length of row to the right as well as the row above and below it
                        // false : check free length of column downards as the well as the columne to left and right of it
                            // false : start over
                    
                    // call step with letter of the random coordinate

                    // process B

                        // try a match (child)  to the letter with any of the letters in the hand

                        // try match that letter with the remaining letters of the hand and cuntinue until a match is found, within the found length
                            // if found play word

                    // if not found try match the initial with any of the remaining letters on the hand, and try process B again
                    
                        // if not found any word, choose a random coordinate fron the remaining coordinates and repeat process A

            let rec updateState aux_st (ms : list<coord * (uint32 * (char * int))>) handState : State.state = 
                match ms with
                | [] -> aux_st
                | x :: xs -> updateState (State.mkState aux_st.board aux_st.dict aux_st.playerNumber handState ((Map.add  (fst x) (snd (snd x)) aux_st.playedMoves))) xs handState

            let updateHand (ms : list<coord * (uint32 * (char * int))>) (newPeices : list<uint32 * uint32>) = 

                let rec auxRemove usedPieces oldHand =
                    match usedPieces with                
                    | [] -> oldHand
                    | x::xs -> auxRemove xs (MultiSet.removeSingle (fst (snd x)) oldHand)


                let rec auxAppend newPieces' oldHand = 
                    match newPieces' with
                    | [] -> oldHand
                    | x::xs -> auxAppend xs (MultiSet.add (fst  x) (snd x) oldHand)
                    
                auxAppend newPeices (auxRemove ms st.hand)
                
            //Print.printHand pieces (State.hand st)
            
            // let input =  System.Console.ReadLine()
            // let move = (RegEx.parseMove input)
            let move = mkMove ()

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                debugPrint(ms.ToString() + " State \n")
                debugPrint(newPieces.ToString() + " New Pieces \n")
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let handState = updateHand ms newPieces // This state needs to be updated
                let st' = updateState st ms handState
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet Map.empty)
        