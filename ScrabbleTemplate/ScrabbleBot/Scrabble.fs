﻿namespace Smooth_Operator

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
            Map ['A', 0u; 'A', 1u; 'B', 2u; 'C', 3u; 'D', 4u; 'E', 5u; 'F', 6u; 'G', 7u; 'H', 8u; 'I', 9u; 'J', 10u; 
            'K', 11u; 'L', 12u; 'M', 13u; 'N', 14u; 'O', 15u; 'P', 16u; 'Q', 17u; 'R', 18u; 'S', 19u; 'T', 20u;
            'U', 21u; 'V', 22u; 'W', 23u; 'X', 24u; 'Y', 25u; 'Z', 26u]

        let rec aux (st : State.state) =

            Print.printHand pieces (State.hand st)

            let findMaxLength coord direction =
                let rec aux count coord'  =
                    match count with
                    | x when x = 7 -> 7
                    | _ -> 
                        match direction with
                        | "r" ->
                            // Check one to the left of the coord
                            match st.playedMoves |> Map.tryFind (fst coord - 1, snd coord) with
                            | Some _ -> 0
                            | None -> 
                                // Check on the same line, one to the right
                                let newCoord = (fst coord' + 1, snd coord')
                                match st.playedMoves |> Map.tryFind newCoord with
                                    | Some _  -> count - 2
                                    | None -> 
                                        // Check on the line under, one to the right
                                        match st.playedMoves |> Map.tryFind (fst newCoord, snd newCoord + 1) with
                                        | Some _ -> count - 2
                                        | None -> 
                                            // Check on the line above, one to the right
                                            match st.playedMoves |> Map.tryFind (fst newCoord, snd newCoord - 1) with
                                            | Some _ -> count - 2
                                            | None -> aux (count + 1) newCoord
                        | "d" ->
                            // Check one on top of the coord
                            match st.playedMoves |> Map.tryFind (fst coord, snd coord - 1) with
                            | Some _ -> 0
                            | None ->
                                // Check on the same line, one below
                                let newCoord = (fst coord', snd coord' + 1)
                                match st.playedMoves |> Map.tryFind newCoord with
                                    | Some _  -> count - 2
                                    | None -> 
                                        // Check on the line to the right, one below
                                        match st.playedMoves |> Map.tryFind (fst newCoord + 1, snd newCoord) with
                                        | Some _ -> count - 2
                                        | None -> 
                                            // Check on the line to the left, one below
                                            match st.playedMoves |> Map.tryFind (fst newCoord - 1, snd newCoord) with
                                            | Some _ -> count - 2
                                            | None -> aux (count + 1) newCoord
                let length = aux 0 coord
                //debugPrint("Længden er" + length.ToString() + "\n")
                length

            let chooseRandomCoord coordinates = 
                debugPrint("Random Coord\n")
                match Seq.length coordinates with
                | 0 -> (0,0),0
                | x -> 
                    let index = System.Random().Next(0,x)
                    coordinates |> Seq.item index , index                      

            let permutate list  =
                //debugPrint("running permutate\n")
                let rec aux e = function
                    | [] -> [[e]]
                    | x::xs as list -> (e::list)::(aux e xs |> List.map (fun xs' -> x::xs'))

                List.fold (fun accum x -> List.collect (aux x) accum) [[]] list

            let rec comb n l : List<List<char>> = 
                match n, l with
                | 0, _ -> [[]]
                | _, [] -> []
                | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs
                

            let combine list : List<List<char>> =
                let rec aux l acc : List<List<char>>  =
                    match l with
                    | [] -> acc
                    | x::xs -> aux xs (List.append acc (permutate x))

                aux list []


            let rec createListOfHand hand = 
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
                debugPrint("Start Coord er: " + startCoord.ToString() + "\n")
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
                let rec aux list' = 
                    match list' with
                    | [] -> SMPass
                    | x::xs ->
                        match char with 
                        | (y: char) when y = '%' -> 
                            match Dictionary.lookup (charListToString x) dict with
                            | true -> 
                                SMPlay (parseWordToMoveString x direction coord)
                            | false -> 
                                aux xs
                        | _ -> 
                                let x' = char :: x
                                let output = charListToString x'
                                match Dictionary.lookup output dict with
                                | true -> 
                                    debugPrint(output)
                                    debugPrint("x' :" + x'.ToString())
                                    debugPrint("char: " + char.ToString() + "\n")
                                    debugPrint("x: " + x.ToString() + "\n")
                                    match direction with 
                                        | "r" -> SMPlay (parseWordToMoveString x direction (fst coord + 1, snd coord))
                                        | "d" -> SMPlay (parseWordToMoveString x direction (fst coord, snd coord + 1))
                                | false -> 
                                    aux xs

                aux list


            let findMove (hand : MultiSet.MultiSet<uint32>) maxLength coord direction : ServerMessage  = 
                debugPrint("running findMove"+ coord.ToString() +  "\n ")
                let rec auxFindMove length =
                    // Checking if there already is a letter at coord (if we are plyaing first move or not)
                    match Map.tryFind coord st.playedMoves with
                        | None -> 
                            debugPrint("None case\n")
                            let charListFromHand = createListOfHand hand
                            let combinationList = comb length charListFromHand
                            let permutationList = combine combinationList
                            match findValidPermutation permutationList direction (0,0) st.dict '%' with
                            | SMPlay move -> SMPlay move
                            | SMPass when length < 2 -> SMPass
                            | SMPass -> auxFindMove (length - 1)
                        | Some char ->
                            let charListFromHand = createListOfHand hand
                            let combinationList = comb length charListFromHand
                            let permutationList = combine combinationList
                            //  Test if current char has children in trie
                            match Dictionary.step (fst char) st.dict with
                                | None -> SMPass
                                | Some (bool,dict) -> 
                                    match findValidPermutation permutationList direction coord st.dict (fst char) with
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
        