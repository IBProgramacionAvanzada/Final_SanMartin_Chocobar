namespace TetrisLibrary

module TetrisMod = 
    ///// TYPES
    
    type Orientation =
    //Type describing the orientations of a Tetrominoe
        |Up
        |Right
        |Down
        |Left
    
    type Movement =
    //Type describing the movements of a Tetrominoe
        |MoveLeft
        |MoveRight
        |MoveDown
        |Rotate
        |None

    let getMovement (int : int) : Movement =
    //Return a Movement type from an int
        match int with
        | 0 -> MoveLeft
        | 1 -> MoveRight
        | 2 -> MoveDown
        | 3 -> Rotate
        | 4 -> None
        | _ -> failwith "Invalid movement"
    
    type TetrominoeType =
    //Type describing the types of Tetrominoes
        |Straight
        |Square
        |T
        |L
        |L_inverted
        |Skew
        |Skew_inverted
    
    let getTetrominoeType (int : int) : TetrominoeType =
    //Return a TetrominoeType from an int
        match int with
        | 0 -> Straight
        | 1 -> Square
        | 2 -> T
        | 3 -> L
        | 4 -> L_inverted
        | 5 -> Skew
        | 6 -> Skew_inverted
        | _ -> failwith "Invalid tetrominoe type"
    
    type Tetrominoe =
    //Type describing a Tetrominoe
        {
            x : int //x coordinate of the Tetrominoe origin in the board
            y : int //y coordinate of the Tetrominoe origin in the board
            orientation : Orientation //Orientation of the Tetrominoe
            tetrominoeType : TetrominoeType //Type of the Tetrominoe
        }
    
    type TetrixState =
    //Type describing the state of the tetris game
        {
            board : int list list  //Board of the game with 0 for empty squares and 1 for filled squares without active Tetrominoe
            score : int //Score of the game
            tetrominoe : Tetrominoe //Active Tetrominoe
        }

    /// TETROMINOE FUNCTIONS
    let getStraightTetrominoeRelPositions (orientation : Orientation) =
        // Return [(x1,y1),...] positions with filled squares for a Straight Tetrominoe
        // relative to the Tetrominoe origin
        match orientation with
        | Up    -> [(2,0);(2,1);(2,2);(2,3)]
        | Right -> [(0,2);(1,2);(2,2);(3,2)]
        | Down  -> [(2,0);(2,1);(2,2);(2,3)]
        | Left  -> [(0,2);(1,2);(2,2);(3,2)]

    let  getSquareTetrominoeRelPositions =
    // Return [(x1,y1),...] positions with filled squares for a Square Tetrominoe
    // relative to the Tetrominoe origin
        [(1, +1); (1, 2); (2, 1); (2, 2)]

    let getTTetrominoeRelPositions (orientation : Orientation) =
    // Return [(x1,y1),...] positions with filled squares for a T Tetrominoe
    // relative to the Tetrominoe origin
        match orientation with
        | Up    -> [(0,2);(1,2);(2,2);(1,1)]
        | Right -> [(1,1);(1,2);(1,3);(2,2)]
        | Down  -> [(0,1);(1,1);(2,1);(1,2)]
        | Left  -> [(1,1);(1,2);(1,3);(0,2)]

    let getLTetrominoeRelPositions (orientation : Orientation) =
    // Return [(x1,y1),...] positions with filled squares for a L Tetrominoe
    // relative to the Tetrominoe origin
        match orientation with
        | Up    -> [(0,1);(0,2);(1,2);(2,2)]
        | Right -> [(1,1);(2,1);(1,2);(1,3)]
        | Down  -> [(0,1);(1,1);(2,1);(2,2)]
        | Left  -> [(1,1);(1,2);(1,3);(0,3)]

    let getL_invertedTetrominoeRelPositions (orientation : Orientation) =
    // Return [(x1,y1),...] positions with filled squares for a L_inverted Tetrominoe
    // relative to the Tetrominoe origin
        match orientation with
        | Up    -> [(0,2);(1,2);(2,2);(2,1)]
        | Right -> [(1,1);(1,2);(1,3);(2,3)]
        | Down  -> [(0,2);(0,1);(1,1);(2,1)]
        | Left  -> [(0,1);(1,1);(1,2);(1,3)]

    let getSkewTetrominoeRelPositions (orientation : Orientation) =
    // Return [(x1,y1),...] positions with filled squares for a Skew Tetrominoe
    // relative to the Tetrominoe origin
        match orientation with
        | Up    -> [(0,1);(1,1);(1,2);(2,2)]
        | Right -> [(1,1);(1,2);(0,2);(0,3)]
        | Down  -> [(0,1);(1,1);(1,2);(2,2)]
        | Left  -> [(1,1);(1,2);(0,2);(0,3)]

    let getSkew_invertedTetrominoeRelPositions (orientation : Orientation) =
    // Return [(x1,y1),...] positions with filled squares for a Skew_inverted Tetrominoe
    // relative to the Tetrominoe origin
        match orientation with
        | Up    -> [(0,2);(1,2);(1,1);(2,1)]
        | Right -> [(1,1);(1,2);(2,2);(2,3)]
        | Down  -> [(0,2);(1,2);(1,1);(2,1)]
        | Left  -> [(1,1);(1,2);(2,2);(2,3)]

    let getTetrominoeRelPositions (tetrominoe : Tetrominoe) =
        // Return [(x1,y1),...] positions with filled squares in a relative board for any Tetrominoe
        match tetrominoe.tetrominoeType with
        | Straight -> getStraightTetrominoeRelPositions tetrominoe.orientation
        | Square -> getSquareTetrominoeRelPositions
        | T -> getTTetrominoeRelPositions tetrominoe.orientation
        | L -> getLTetrominoeRelPositions tetrominoe.orientation
        | L_inverted -> getL_invertedTetrominoeRelPositions tetrominoe.orientation
        | Skew -> getSkewTetrominoeRelPositions tetrominoe.orientation
        | Skew_inverted -> getSkew_invertedTetrominoeRelPositions tetrominoe.orientation
    
    let getTetrominoePositions (tetrominoe : Tetrominoe) =
        // Return [(x1,y1),...] positions with filled squares in the real board for any Tetrominoe
        getTetrominoeRelPositions tetrominoe 
        |> List.map (fun (x,y) -> (x + tetrominoe.x, y + tetrominoe.y))

    let moveTetrominoe (movement : Movement) (tetrominoe : Tetrominoe) =
    // Return a new Tetrominoe after a movement (possible or not)
        match movement with
        |MoveLeft  -> {tetrominoe with x = tetrominoe.x - 1}
        |MoveRight -> {tetrominoe with x = tetrominoe.x + 1}
        |MoveDown  -> {tetrominoe with y = tetrominoe.y - 1}
        |Rotate    ->
            match tetrominoe.orientation with
            |Up    -> {tetrominoe with orientation = Right}
            |Right -> {tetrominoe with orientation = Down }
            |Down  -> {tetrominoe with orientation = Left }
            |Left  -> {tetrominoe with orientation = Up   }
        |None -> tetrominoe

    /// STATE FUNCTIONS 

    let createEmptyBoard (rows: int, columns: int) =
        // Return a board with all cells empty (0s) with the given dimensions
        List.replicate rows (List.replicate columns 0)

    let getRelBoardWithTetrominoe (tetrominoe : Tetrominoe) =
        // Return a 4x4 board with the relative position of the tetrominoe
        let miniboard = createEmptyBoard (4,4)
        let relPositions = getTetrominoeRelPositions tetrominoe
        miniboard
        |> List.mapi (fun yBoard row ->
            row
            |> List.mapi (fun xBoard cell ->
                if (relPositions |> List.exists (fun (xTetro,yTetro) -> xTetro = xBoard && yTetro = yBoard)) 
                then 1 else 0
                )
            )

    let getBoardWithTetrominoe (tetrixState : TetrixState) =
        // Return the board with the active tetrominoe in it
        tetrixState.board 
        |> List.mapi (fun y row -> 
            row 
            |> List.mapi (fun x cell -> 
                if (getTetrominoePositions tetrixState.tetrominoe |> List.exists (fun (x_,y_) -> x_ = x && y_ = y)) 
                then 1 else cell
                )
            )

    let printPrettyRow (row: int list) =
        printf "|"
        let printPrettyCell (num: int) =
            match num with
            | 1 -> printf "[X]"
            | 0 -> printf "   "
            | _ -> printf "???"
        row
        |> List.iter (printPrettyCell)
        printf "|\n"

    let printPrettyTetrixState (tetrixState : TetrixState) =
        printfn "Score: %d" tetrixState.score
        printf " ______________________________\n"
        tetrixState
        |> getBoardWithTetrominoe
        |> List.rev 
        |> List.iter (printPrettyRow)
        printf " ------------------------------\n"

    let printTetrixState (tetrixState : TetrixState) =
        // Print the current state of the game
        printfn "Score: %d" tetrixState.score
        tetrixState
        |> getBoardWithTetrominoe
        |> List.rev 
        |> List.iter (fun row -> printfn "%A" row)

    /// COLLISION FUNCTIONS
    let isTetrominoeContainedInBoard (tetrixState : TetrixState) =
    // Return true if the tetrominoe is contained in the board (i.e. all its squares are inside the board) 
        getTetromionePositions tetrixState.tetrominoe
        |> List.forall (fun (x,y) -> x >= 0 && x < (tetrixState.board.[0] |> List.length) && y >= 0 && y < tetrixState.board.Length)
    
    let doesntTetrominoeCollideWithBoard (tetrixState : TetrixState) =
    // Return true if the tetrominoe doesn't collide with the board (i.e. all its squares are in empty cells)
        getTetromionePositions tetrixState.tetrominoe
        |> List.forall (fun (x,y) -> tetrixState.board.[y].[x] = 0)

    /// GAME FUNCTIONS
    let updateTetrixBoard (tetrixState: TetrixState) =
        // Return a new TetrixState with the active tetrominoe in the board,
        // completed rows deleted
        // score updated
        // and a new tetrominoe in the top of the board (type random, orientation up)
        let newBoard = tetrixState |> getBoardWithTetrominoe |> List.filter (fun row -> row |> List.exists (fun x -> x = 0))
        let deletedRows = tetrixState.board.Length - newBoard.Length
        let newScore = tetrixState.score + deletedRows * 100
        let rnd = new System.Random()

        {
            board = (List.init deletedRows (fun _ -> List.init 10 (fun _ -> 0)))  |>  List.append newBoard
            score = newScore
            tetrominoe = {x = 3 ; y = 17 ; orientation = Up; tetrominoeType = rnd.Next(0,7) |> getTetrominoeType}
        }

    let moveTetrominoeInBoard (movement : Movement) (tetrixState : TetrixState) =
        // Return a new TetrixState with the tetrominoe moved in the given direction if possible
        // If not possible and the movement is MoveDown, update the board (before the movement) 
        // and return the new TetrixState.
        // If not possible and the movement is not MoveDown, return the same TetrixState
        match movement with
        |None -> tetrixState
        |MoveDown -> 
            let newTetrominoe = moveTetrominoe MoveDown tetrixState.tetrominoe
            let newTetrixState = {tetrixState with tetrominoe = newTetrominoe}
            if (isTetrominoeContainedInBoard newTetrixState && doesntTetrominoeCollideWithBoard newTetrixState) then
                newTetrixState
            else
                updateTetrixBoard tetrixState
        |_ -> 
            let newTetrominoe = moveTetrominoe movement tetrixState.tetrominoe
            let newTetrixState = {tetrixState with tetrominoe = newTetrominoe}
            if (isTetrominoeContainedInBoard newTetrixState && doesntTetrominoeCollideWithBoard newTetrixState) then
                newTetrixState
            else
                tetrixState

    let moveTetrominoeInBoard_downTurn (movement : Movement) (tetrixState : TetrixState) =
        // For a turn where tetrominoe is forced to move down:
        // Return a new TetrixState with the tetrominoe moved in the given direction if possible.
        // 2 simultaneous movements are allowed (e.g. MoveLeft and MoveDown), but not 2 MoveDowns.
        // If MoveDown not possible (with and without the other movemente)
        // return the updated board (before the movement) and the same TetrixState.
        match movement with
         // If movement is MoveDown return new TetrixState with the tetrominoe moved down only 1 cell
        |MoveDown -> tetrixState |> moveTetrominoeInBoard MoveDown 
        // If movement is not MoveDown, move the tetrominoe down and then move it in the given direction
        |_ -> 
            // newTetrixState is the TetrixState with the tetrominoe moved down 1 cell (regardless of collision)
            // and moved in the given direction (if possible)
            let newTetrixState = {tetrixState with tetrominoe = moveTetrominoe MoveDown tetrixState.tetrominoe} 
                                    |> moveTetrominoeInBoard movement
            // If the tetrominoe doesnt collide with the board, return the new TetrixState
            if (isTetrominoeContainedInBoard newTetrixState && doesntTetrominoeCollideWithBoard newTetrixState) then
                newTetrixState
            //else return the updated tetrixState (before the movement) 
            else
                tetrixState|> updateTetrixBoard

    let tetrixSimulator (numIt : int) (tetrixState : TetrixState) =
        // Simulates the Tetris game for numIt iterations or until the tetrominoe collides with the board
        // doing random movements
        // Returns the final state of the game
        let rnd = new System.Random()
        let rec loop (turn : int) (tetrixState : TetrixState) =
            // printTetrixState tetrixState
            if turn > numIt then
                tetrixState
            elif not (doesntTetrominoeCollideWithBoard tetrixState) then
                printfn "Game over. Turn %d" turn
                tetrixState
            else
                //random movement
                let movement = rnd.Next(0,5) |> getMovement
                if turn % 4 = 0 then
                    tetrixState |> moveTetrominoeInBoard_downTurn movement |> loop (turn + 1)
                else
                    tetrixState |> moveTetrominoeInBoard movement |> loop (turn + 1)
            
        loop 1 tetrixState