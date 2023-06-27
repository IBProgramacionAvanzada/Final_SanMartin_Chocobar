open TetrisLibrary
open TetrisMod

[<EntryPoint>]
let main args =
    printfn $"Bienvenido al clasico Tetris"
    //let tetrix = createEmptyBoard (20, 10)
    let tetrixState = initialTetrixState 20 10
    tetrixState  |> tetrixSimulator 1000000 |> printPrettyTetrixState
    0
