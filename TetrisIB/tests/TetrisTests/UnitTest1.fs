module TetrisTests

open NUnit.Framework
open TetrisLibrary
open TetrisMod

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
    Assert.Pass()

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
    let initial =  {
        x = 5
        y = 5
        orientation = Up
        tetrominoeType = Straight
    }
    let actual = moveTetrominoe Rotate initial
    let expected =  {initial with orientation = Right}
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Rotate Straight from Rigth to Down`` () = 
    let initial =  {
        x = 5
        y = 5
        orientation = Right
        tetrominoeType = Straight
    }
    let actual = moveTetrominoe Rotate initial
    let expected =  {initial with orientation = Down}
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Rotate Straight from Down to Left`` () = 
    let initial =  {
        x = 5
        y = 5
        orientation = Down
        tetrominoeType = Straight
    }
    let actual = moveTetrominoe Rotate initial
    let expected =  {initial with orientation = Left}
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Rotate Straight from Up twice`` () = 
    let initial =  {
        x = 5
        y = 5
        orientation = Up
        tetrominoeType = Straight
    }
    let actual = (moveTetrominoe Rotate initial) |> moveTetrominoe Rotate
    let expected =  {initial with orientation = Down}
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``MoveLeft Straight from x=5 to x=4`` () = 
    let initial =  {
        x = 5
        y = 5
        orientation = Up
        tetrominoeType = Straight
    }
    let actual = moveTetrominoe MoveLeft initial
    let expected =  {initial with x = 4}
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``MoveLeft Straight from x=0 to x=-1`` () = 
    let initial =  {
        x = 0
        y = 5
        orientation = Up
        tetrominoeType = Straight
    }
    let actual = moveTetrominoe MoveLeft initial
    let expected =  {initial with x = -1}
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``MoveRight Straight from x=5 to x=6`` () = 
    let initial =  {
        x = 5
        y = 5
        orientation = Up
        tetrominoeType = Straight
    }
    let actual = moveTetrominoe MoveRight initial
    let expected =  {initial with x = 6}
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``MoveDown Straight from y=5 to y=4`` () = 
    let initial =  {
        x = 5
        y = 5
        orientation = Up
        tetrominoeType = Straight
    }
    let actual = moveTetrominoe MoveDown initial
    let expected =  {initial with y = 4}
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of Straight with orientation Up`` () = 
    let initial =  {
        x = 5
        y = 5
        orientation = Up
        tetrominoeType = Straight
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Right
        tetrominoeType = Straight
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Down
        tetrominoeType = Straight
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Left
        tetrominoeType = Straight
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Up
        tetrominoeType = Square
    }
    let actual = moveTetrominoe Rotate initial
    let expected =  {initial with orientation = Right}
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Rotate Square from Rigth to Down`` () = 
    let initial =  {
        x = 5
        y = 5
        orientation = Right
        tetrominoeType = Square
    }
    let actual = moveTetrominoe Rotate initial
    let expected =  {initial with orientation = Down}
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Rotate Square from Down to Left`` () = 
    let initial =  {
        x = 5
        y = 5
        orientation = Down
        tetrominoeType = Square
    }
    let actual = moveTetrominoe Rotate initial
    let expected =  {initial with orientation = Left}
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Rotate Square from Left to Up`` () = 
    let initial =  {
        x = 5
        y = 5
        orientation = Left
        tetrominoeType = Square
    }
    let actual = moveTetrominoe Rotate initial
    let expected =  {initial with orientation = Up}
    Assert.That(expected, Is.EqualTo(actual))

[<Test>]
let ``Relative position of Square with orientation Up`` () = 
    let initial =  {
        x = 5
        y = 5
        orientation = Up
        tetrominoeType = Square
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Up
        tetrominoeType = T
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Right
        tetrominoeType = T
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Down
        tetrominoeType = T
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Left
        tetrominoeType = T
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Up
        tetrominoeType = L
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Right
        tetrominoeType = L
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Down
        tetrominoeType = L
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Left
        tetrominoeType = L
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Up
        tetrominoeType = L_inverted
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Right
        tetrominoeType = L_inverted
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Down
        tetrominoeType = L_inverted
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Left
        tetrominoeType = L_inverted
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Up
        tetrominoeType = Skew
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Right
        tetrominoeType = Skew
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Down
        tetrominoeType = Skew
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Left
        tetrominoeType = Skew
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Up
        tetrominoeType = Skew_inverted
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Right
        tetrominoeType = Skew_inverted
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Down
        tetrominoeType = Skew_inverted
    }
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
    let initial =  {
        x = 5
        y = 5
        orientation = Left
        tetrominoeType = Skew_inverted
    }
    let actual = getRelBoardWithTetrominoe initial
    let expected =
        [
            [0; 0; 0; 0];
            [0; 1; 0; 0];
            [0; 1; 1; 0];
            [0; 0; 1; 0]
        ]
    Assert.That(expected, Is.EqualTo(actual))


// Test MoveLeft  with other tetrominoe 
// Test MoveRight with other tetrominoe 
// Test MoveDown  with other tetrominoe 

