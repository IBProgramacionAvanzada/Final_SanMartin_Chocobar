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

    let createEmptyBoard (rows, columns) =
        List.replicate rows (List.replicate columns 0)
