open TetrisLibrary
open TetrisMod

[<EntryPoint>]
let main args =
    printfn $"Bienvenido al clasico Tetris"
    //let tetrix = createEmptyBoard (20, 10)
    let tetrixState = {
        board = createEmptyBoard (20, 10) ; 
        score = 0; 
        tetrominoe = {x = 4; y = 10; orientation = Up; tetrominoeType = Straight}
    }
    tetrixState |> printTetrixState
    0
