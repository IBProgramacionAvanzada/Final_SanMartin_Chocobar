namespace TetrisLibrary

module Types = 
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

module State = 
    //let a = Types.Up
    let createEmptyBoard (rows, columns) =
        List.replicate rows (List.replicate columns 0)
