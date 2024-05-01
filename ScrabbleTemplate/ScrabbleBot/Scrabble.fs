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

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =

            let passTurn = SMPass

            let findMaxLength coord =
                let rec findSameLine coord count =
                      match count with
                      | 7 -> 7 
                      | _ -> 
                        match st.playedMoves |> Map.tryFind coord with
                        | Some _ -> count - 2 
                        | None -> 
                            let newCoord = (fst coord + 1, snd coord)
                            findSameLine newCoord (count+1)

                let globalCount = findSameLine coord 0

                let rec findLineAbove coord count =
                      match count with
                      | x when globalCount = count -> count 
                      | _ -> 
                        match st.playedMoves |> Map.tryFind coord with
                        | Some _ -> count - 2 
                        | None -> 
                            let newCoord = (fst coord + 1, snd coord)
                            findSameLine newCoord (count+1)

                let globalCount = globalCount - findLineAbove (fst coord, snd coord - 1) 0

                let rec findLineBelow coord count =
                      match count with
                      | x when globalCount = count -> count 
                      | _ -> 
                        match st.playedMoves |> Map.tryFind coord with
                        | Some _ -> count - 2 
                        | None -> 
                            let newCoord = (fst coord + 1, snd coord)
                            findSameLine newCoord (count+1)
                
                globalCount - findLineBelow (fst coord, snd coord + 1) 0


            let chooseRandomCoord coordinates = 
                match Seq.length coordinates with
                | 0 -> (0,0),0
                | x -> 
                    let index = System.Random().Next(1,x)
                    coordinates |> Seq.item index , index

            let findMove hand maxLength coord  = 
                match maxLength with
                    | 0 -> SMPass
                    // | _ -> SMPlay ....


                // return : SMPlay (Regex.parseMove move)
            
            let mkMove =
                match Map.count(st.playedMoves) with
                    | 0 -> findMove st.hand 7 (0,0)
                    | _ -> 
                        let rec auxNotBaseCase coordinates =
                            let coord , index = chooseRandomCoord coordinates
                            match findMove st.hand (findMaxLength coord) coord with
                            |  SMPlay move -> SMPlay move
                            |  SMPass -> 
                                match Seq.length coordinates with
                                | 0 -> SMPass
                                | _ -> auxNotBaseCase (coordinates |> Seq.removeAt index) // - 1
                                
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
                
            Print.printHand pieces (State.hand st)

            //let input =  System.Console.ReadLine()
            // let move = (RegEx.parseMove move)
            let move = mkMove

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
        