module TetrisTests

open NUnit.Framework
open TetrisLibrary
open TetrisMod

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``Create empty board 20x10`` () = 
    let actual = createEmptyBoard (20, 10)
    let expected =
        [
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Rotate Straight from Up to Rigth`` () = 
    let initial  = {x = 5; y = 5; orientation = Up; tetrominoeType = Straight}
    let expected = {initial with orientation = Right}
    let actual   = moveTetrominoe Rotate initial
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Rotate Straight from Rigth to Down`` () = 
    let initial  = {x = 5; y = 5; orientation = Right; tetrominoeType = Straight}
    let expected = {initial with orientation = Down}
    let actual   = moveTetrominoe Rotate initial
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Rotate Straight from Down to Left`` () = 
    let initial  = {x = 5; y = 5; orientation = Down; tetrominoeType = Straight}
    let expected = {initial with orientation = Left}
    let actual   = moveTetrominoe Rotate initial
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Rotate Straight from Up twice`` () = 
    let initial  = {x = 5; y = 5; orientation = Up; tetrominoeType = Straight}
    let expected = {initial with orientation = Down}
    let actual   = (moveTetrominoe Rotate initial) |> moveTetrominoe Rotate
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``MoveLeft Straight from x=5 to x=4`` () = 
    let initial  = {x = 5; y = 5; orientation = Up; tetrominoeType = Straight}
    let expected = {initial with x = 4}
    let actual   = moveTetrominoe MoveLeft initial
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``MoveLeft Straight from x=0 to x=-1`` () = 
    let initial  = {x = 0; y = 5; orientation = Up; tetrominoeType = Straight}
    let expected = {initial with x = -1}
    let actual   = moveTetrominoe MoveLeft initial
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``MoveRight Straight from x=5 to x=6`` () = 
    let initial  = {x = 5; y = 5; orientation = Up; tetrominoeType = Straight}
    let expected = {initial with x = 6}
    let actual   = moveTetrominoe MoveRight initial
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``MoveDown Straight from y=5 to y=4`` () = 
    let initial  = {x = 5; y = 5; orientation = Up; tetrominoeType = Straight}
    let expected = {initial with y = 4}
    let actual   = moveTetrominoe MoveDown initial
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of Straight with orientation Up`` () = 
    let initial = {x = 5; y = 5; orientation = Up; tetrominoeType = Straight}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 1; 0];
            [0; 0; 1; 0];
            [0; 0; 1; 0];
            [0; 0; 1; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of Straight with orientation Right`` () = 
    let initial = {x = 5; y = 5; orientation = Right; tetrominoeType = Straight}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [0; 0; 0; 0];
            [1; 1; 1; 1];
            [0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of Straight with orientation Down`` () = 
    let initial = {x = 5; y = 5; orientation = Down; tetrominoeType = Straight}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 1; 0];
            [0; 0; 1; 0];
            [0; 0; 1; 0];
            [0; 0; 1; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of Straight with orientation Left`` () = 
    let initial = {x = 5; y = 5; orientation = Left; tetrominoeType = Straight}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [0; 0; 0; 0];
            [1; 1; 1; 1];
            [0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Rotate Square from Up to Rigth`` () = 
    let initial  = {x = 5; y = 5; orientation = Up; tetrominoeType = Square}
    let expected = {initial with orientation = Right}
    let actual   = moveTetrominoe Rotate initial
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Rotate Square from Rigth to Down`` () = 
    let initial  = {x = 5; y = 5; orientation = Right; tetrominoeType = Square}
    let expected = {initial with orientation = Down}
    let actual   = moveTetrominoe Rotate initial
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Rotate Square from Down to Left`` () = 
    let initial  = {x = 5; y = 5; orientation = Down; tetrominoeType = Square}
    let expected = {initial with orientation = Left}
    let actual   = moveTetrominoe Rotate initial
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Rotate Square from Left to Up`` () = 
    let initial  = {x = 5; y = 5; orientation = Left; tetrominoeType = Square}
    let expected = {initial with orientation = Up}
    let actual   = moveTetrominoe Rotate initial
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of Square with orientation Up`` () = 
    let initial = {x = 5; y = 5; orientation = Up; tetrominoeType = Square}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [0; 1; 1; 0];
            [0; 1; 1; 0];
            [0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of T with orientation Up`` () = 
    let initial = {x = 5; y = 5; orientation = Up; tetrominoeType = T}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [0; 1; 0; 0];
            [1; 1; 1; 0];
            [0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of T with orientation Right`` () = 
    let initial = {x = 5; y = 5; orientation = Right; tetrominoeType = T}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [0; 1; 0; 0];
            [0; 1; 1; 0];
            [0; 1; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of T with orientation Down`` () = 
    let initial = {x = 5; y = 5; orientation = Down; tetrominoeType = T}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [1; 1; 1; 0];
            [0; 1; 0; 0];
            [0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of T with orientation Left`` () = 
    let initial = {x = 5; y = 5; orientation = Left; tetrominoeType = T}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [0; 1; 0; 0];
            [1; 1; 0; 0];
            [0; 1; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of L with orientation Up`` () = 
    let initial = {x = 5; y = 5; orientation = Up; tetrominoeType = L}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [1; 0; 0; 0];
            [1; 1; 1; 0];
            [0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of L with orientation Right`` () = 
    let initial = {x = 5; y = 5; orientation = Right; tetrominoeType = L}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [0; 1; 1; 0];
            [0; 1; 0; 0];
            [0; 1; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of L with orientation Down`` () = 
    let initial = {x = 5; y = 5; orientation = Down; tetrominoeType = L}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [1; 1; 1; 0];
            [0; 0; 1; 0];
            [0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of L with orientation Left`` () = 
    let initial = {x = 5; y = 5; orientation = Left; tetrominoeType = L}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [0; 1; 0; 0];
            [0; 1; 0; 0];
            [1; 1; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of L_inverted with orientation Up`` () = 
    let initial = {x = 5; y = 5; orientation = Up; tetrominoeType = L_inverted}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [0; 0; 1; 0];
            [1; 1; 1; 0];
            [0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of L_inverted with orientation Right`` () = 
    let initial = {x = 5; y = 5; orientation = Right; tetrominoeType = L_inverted}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [0; 1; 0; 0];
            [0; 1; 0; 0];
            [0; 1; 1; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of L_inverted with orientation Down`` () = 
    let initial = {x = 5; y = 5; orientation = Down; tetrominoeType = L_inverted}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [1; 1; 1; 0];
            [1; 0; 0; 0];
            [0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of L_inverted with orientation Left`` () = 
    let initial = {x = 5; y = 5; orientation = Left; tetrominoeType = L_inverted}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [1; 1; 0; 0];
            [0; 1; 0; 0];
            [0; 1; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of Skew with orientation Up`` () = 
    let initial = {x = 5; y = 5; orientation = Up; tetrominoeType = Skew}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [1; 1; 0; 0];
            [0; 1; 1; 0];
            [0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of Skew with orientation Right`` () = 
    let initial = {x = 5; y = 5; orientation = Right; tetrominoeType = Skew}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [0; 1; 0; 0];
            [1; 1; 0; 0];
            [1; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of Skew with orientation Down`` () = 
    let initial = {x = 5; y = 5; orientation = Down; tetrominoeType = Skew}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [1; 1; 0; 0];
            [0; 1; 1; 0];
            [0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of Skew with orientation Left`` () = 
    let initial = {x = 5; y = 5; orientation = Left; tetrominoeType = Skew}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [0; 1; 0; 0];
            [1; 1; 0; 0];
            [1; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of Skew_inverted with orientation Up`` () = 
    let initial = {x = 5; y = 5; orientation = Up; tetrominoeType = Skew_inverted}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [0; 1; 1; 0];
            [1; 1; 0; 0];
            [0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of Skew_inverted with orientation Right`` () = 
    let initial = {x = 5; y = 5; orientation = Right; tetrominoeType = Skew_inverted}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [0; 1; 0; 0];
            [0; 1; 1; 0];
            [0; 0; 1; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of Skew_inverted with orientation Down`` () = 
    let initial = {x = 5; y = 5; orientation = Down; tetrominoeType = Skew_inverted}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [0; 1; 1; 0];
            [1; 1; 0; 0];
            [0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of Skew_inverted with orientation Left`` () = 
    let initial = {x = 5; y = 5; orientation = Left; tetrominoeType = Skew_inverted}
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [0; 1; 0; 0];
            [0; 1; 1; 0];
            [0; 0; 1; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Straight-Up tetrominoe at start position in empty Board`` () = 
    let tetrixState = {
        board = createEmptyBoard (20, 10) ; 
        score = 0; 
        tetrominoe = {x = 3; y = 16; orientation = Up; tetrominoeType = Straight}
    }
    let actual = getBoardWithTetrominoe tetrixState
    let expected =
        [
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 1; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 1; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 1; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 1; 0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Square tetrominoe at start position in empty Board`` () = 
    let tetrixState = {
        board = createEmptyBoard (20, 10) ; 
        score = 0; 
        tetrominoe = {x = 3; y = 16; orientation = Up; tetrominoeType = Square}
    }
    let actual = getBoardWithTetrominoe tetrixState
    let expected =
        [
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 1; 1; 0; 0; 0; 0];
            [0; 0; 0; 0; 1; 1; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``T-Right tetrominoe at start position in empty Board`` () = 
    let tetrixState = {
        board = createEmptyBoard (20, 10) ; 
        score = 0; 
        tetrominoe = {x = 3; y = 16; orientation = Right; tetrominoeType = T}
    }
    let actual = getBoardWithTetrominoe tetrixState
    let expected =
        [
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 1; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 1; 1; 0; 0; 0; 0];
            [0; 0; 0; 0; 1; 0; 0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``L-Down tetrominoe at start position in empty Board`` () = 
    let tetrixState = {
        board = createEmptyBoard (20, 10) ; 
        score = 0; 
        tetrominoe = {x = 3; y = 16; orientation = Down; tetrominoeType = L}
    }
    let actual = getBoardWithTetrominoe tetrixState
    let expected =
        [
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 1; 1; 1; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 1; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``L_inverted-Left tetrominoe at start position in empty Board`` () = 
    let tetrixState = {
        board = createEmptyBoard (20, 10) ; 
        score = 0; 
        tetrominoe = {x = 3; y = 16; orientation = Left; tetrominoeType = L_inverted}
    }
    let actual = getBoardWithTetrominoe tetrixState
    let expected =
        [
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 1; 1; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 1; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 1; 0; 0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Skew-Up tetrominoe at start position in empty Board`` () = 
    let tetrixState = {
        board = createEmptyBoard (20, 10) ; 
        score = 0; 
        tetrominoe = {x = 3; y = 16; orientation = Up; tetrominoeType = Skew}
    }
    let actual = getBoardWithTetrominoe tetrixState
    let expected =
        [
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 1; 1; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 1; 1; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Skew_inverted-Right tetrominoe at start position in empty Board`` () = 
    let tetrixState = {
        board = createEmptyBoard (20, 10) ; 
        score = 0; 
        tetrominoe = {x = 3; y = 16; orientation = Right; tetrominoeType = Skew_inverted}
    }
    let actual = getBoardWithTetrominoe tetrixState
    let expected =
        [
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 1; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 1; 1; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 1; 0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``MoveLeft Straight-Up tetrominoe in empty Board`` () = 
    let tetrixState = {
        board = createEmptyBoard (20, 10) ; 
        score = 0; 
        tetrominoe = {x = 3; y = 10; orientation = Up; tetrominoeType = Straight}
    }
    let actual = moveTetrominoeInBoard MoveLeft tetrixState |> getBoardWithTetrominoe
    let expected =
        [
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 1; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 1; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 1; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 1; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``MoveRight T-Up tetrominoe in empty Board`` () = 
    let tetrixState = {
        board = createEmptyBoard (20, 10) ; 
        score = 0; 
        tetrominoe = {x = 3; y = 10; orientation = Up; tetrominoeType = T}
    }
    let actual = moveTetrominoeInBoard MoveRight tetrixState |> getBoardWithTetrominoe
    let expected =
        [
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 1; 0; 0; 0; 0];
            [0; 0; 0; 0; 1; 1; 1; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Rotate L-Up tetrominoe in empty Board`` () = 
    let tetrixState = {
        board = createEmptyBoard (20, 10) ; 
        score = 0; 
        tetrominoe = {x = 3; y = 10; orientation = Up; tetrominoeType = L}
    }
    let actual = moveTetrominoeInBoard Rotate tetrixState |> getBoardWithTetrominoe
    let expected =
        [
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 1; 1; 0; 0; 0; 0];
            [0; 0; 0; 0; 1; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 1; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``MoveDown Skew-Up tetrominoe in empty Board`` () = 
    let tetrixState = {
        board = createEmptyBoard (20, 10) ; 
        score = 0; 
        tetrominoe = {x = 3; y = 10; orientation = Up; tetrominoeType = Skew}
    }
    let actual = moveTetrominoeInBoard MoveDown tetrixState |> getBoardWithTetrominoe
    let expected =
        [
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 1; 1; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 1; 1; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Try MoveRight Straight-Up tetrominoe outside an empty Board`` () = 
    let tetrixState = {
        board = createEmptyBoard (20, 10) ; 
        score = 0; 
        tetrominoe = {x = 7; y = 10; orientation = Up; tetrominoeType = Straight}
    }
    let actual = moveTetrominoeInBoard MoveRight tetrixState |> getBoardWithTetrominoe
    let expected =
        [
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 1];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 1];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 1];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 1];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Try MoveLeft T-Right tetrominoe outside an empty Board`` () = 
    let tetrixState = {
        board = createEmptyBoard (20, 10) ; 
        score = 0; 
        tetrominoe = {x = -1; y = 10; orientation = Right; tetrominoeType = T}
    }
    let actual = moveTetrominoeInBoard MoveLeft tetrixState |> getBoardWithTetrominoe
    let expected =
        [
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [1; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [1; 1; 0; 0; 0; 0; 0; 0; 0; 0];
            [1; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Try Rotate Straight-Up tetrominoe outside an empty Board`` () = 
    let tetrixState = {
        board = createEmptyBoard (20, 10) ; 
        score = 0; 
        tetrominoe = {x = 7; y = 10; orientation = Up; tetrominoeType = Straight}
    }
    let actual = moveTetrominoeInBoard Rotate tetrixState |> getBoardWithTetrominoe
    let expected =
        [
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 1];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 1];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 1];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 1];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``updateTetrixBoard after forming one complete line - Test newBoard`` () = 
    let initialBoard = 
        [
            [1; 1; 1; 1; 1; 1; 1; 1; 1; 0];
            [1; 1; 1; 1; 1; 1; 1; 1; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    let tetrixState = {
        board = initialBoard; 
        score = 0; 
        tetrominoe = {x = 7; y = 0; orientation = Up; tetrominoeType = Straight}
    }
    let actual = (updateTetrixBoard tetrixState).board
    let expected =
        [
            [1; 1; 1; 1; 1; 1; 1; 1; 0; 1];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 1];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 1];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``updateTetrixBoard after forming one complete line - Test score`` () = 
    let initialBoard = 
        [
            [1; 1; 1; 1; 1; 1; 1; 1; 1; 0];
            [1; 1; 1; 1; 1; 1; 1; 1; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    let tetrixState = {
        board = initialBoard; 
        score = 0; 
        tetrominoe = {x = 7; y = 0; orientation = Up; tetrominoeType = Straight}
    }
    let actual = (updateTetrixBoard tetrixState).score
    let expected = 100
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``updateTetrixBoard after forming two complete lines - Test newBoard`` () = 
    let initialBoard = 
        [
            [1; 1; 1; 1; 1; 1; 1; 1; 1; 0];
            [1; 1; 1; 1; 1; 1; 1; 1; 1; 0];
            [1; 1; 1; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    let tetrixState = {
        board = initialBoard; 
        score = 0; 
        tetrominoe = {x = 7; y = 0; orientation = Up; tetrominoeType = Straight}
    }
    let actual = (updateTetrixBoard tetrixState).board
    let expected =
        [
            [1; 1; 1; 0; 0; 0; 0; 0; 0; 1];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 1];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``updateTetrixBoard after forming two complete lines - Test score`` () = 
    let initialBoard = 
        [
            [1; 1; 1; 1; 1; 1; 1; 1; 1; 0];
            [1; 1; 1; 1; 1; 1; 1; 1; 1; 0];
            [1; 1; 1; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    let tetrixState = {
        board = initialBoard; 
        score = 0; 
        tetrominoe = {x = 7; y = 0; orientation = Up; tetrominoeType = Straight}
    }
    let actual = (updateTetrixBoard tetrixState).score
    let expected = 200
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``moveTetrominoeInBoard with MoveDown and T-Up - collision forming one line`` () = 
    let initialBoard = 
        [
            [1; 1; 1; 1; 0; 1; 1; 1; 1; 1];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    let tetrixState = {
        board = initialBoard; 
        score = 0; 
        tetrominoe = {x = 3; y = -1; orientation = Up; tetrominoeType = T}
    }
    let actual = (moveTetrominoeInBoard MoveDown tetrixState).board
    let expected =
        [
            [0; 0; 0; 1; 1; 1; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``moveTetrominoeInBoard with MoveDown and L-Left - collision forming two lines`` () = 
    let initialBoard = 
        [
            [1; 1; 1; 1; 1; 1; 1; 1; 1; 0];
            [1; 1; 1; 1; 1; 1; 1; 1; 1; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    let tetrixState = {
        board = initialBoard; 
        score = 0; 
        tetrominoe = {x = 8; y = -1; orientation = Left; tetrominoeType = L}
    }
    let actual = (moveTetrominoeInBoard MoveDown tetrixState).board
    let expected =
        [
            [0; 0; 0; 0; 0; 0; 0; 0; 1; 1];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``moveTetrominoeInBoard with MoveDown and Skew-Up - collision at bottom not forming line `` () = 
    let initialBoard = createEmptyBoard (20,10)
    let tetrixState = {
        board = initialBoard; 
        score = 0; 
        tetrominoe = {x = 0; y = -1; orientation = Up; tetrominoeType = Skew}
    }
    let actual = (moveTetrominoeInBoard MoveDown tetrixState).board
    let expected =
        [
            [1; 1; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 1; 1; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``moveTetrominoeInBoard with MoveDown and Square-Up - collision with board not forming line `` () = 
    let initialBoard = 
        [
            [1; 1; 1; 1; 1; 1; 1; 1; 1; 0];
            [1; 1; 1; 1; 1; 1; 1; 1; 1; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    let tetrixState = {
        board = initialBoard; 
        score = 0; 
        tetrominoe = {x = -1; y = 1; orientation = Up; tetrominoeType = Square}
    }
    let actual = (moveTetrominoeInBoard MoveDown tetrixState).board
    let expected =
        [
            [1; 1; 1; 1; 1; 1; 1; 1; 1; 0];
            [1; 1; 1; 1; 1; 1; 1; 1; 1; 0];
            [1; 1; 0; 0; 0; 0; 0; 0; 0; 0];
            [1; 1; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0];
            [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))
