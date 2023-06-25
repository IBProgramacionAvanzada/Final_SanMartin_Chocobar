namespace TetrisLibrary

module TetrisMod = 
    ///// TYPES
    
    type Orientation =
        |Up
        |Right
        |Down
        |Left
    
    type Movement =
        |MoveLeft
        |MoveRight
        |MoveDown
        |Rotate
        |None
    
    type TetrominoeType =
        |Straight
        |Square
        |T
        |L
        |L_inverted
        |Skew
        |Skew_inverted
    
    type Tetrominoe =
        {
            x : int
            y : int
            orientation : Orientation
            tetrominoeType : TetrominoeType
        }
    
    type TetrixState =
        {
            board : int list list
            score : int
            tetrominoe : Tetrominoe
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

    let getTetrominoeRelPositions (tetrominoe : Tetrominoe) =
        // Return [(x1,y1),...] positions with filled squares in a relative board for any Tetrominoe
        match tetrominoe.tetrominoeType with
        | Straight -> getStraightTetrominoeRelPositions tetrominoe.orientation
        |_ -> [(0,0)] // TO DO

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
        List.replicate rows (List.replicate columns 0)

    let getRelBoardWithTetrominoe (tetrominoe : Tetrominoe) =
        // Return a 4x4 board with the relative position of the tetronimoe
        let miniboard = createEmptyBoard (4,4)
        let relPositions = getTetrominoeRelPositions tetrominoe
        miniboard
        |> List.mapi (fun yBoard row ->
            row
            |> List.mapi (fun xBoard cell ->
                if (relPositions |> List.exists (fun (xTetro,yTetro) -> xTetro = xBoard && yTetro = yBoard)) 
                then 1 
                else 0
                )
            )