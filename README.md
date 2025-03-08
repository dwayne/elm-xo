# xo

A [Tic-tac-toe](https://en.wikipedia.org/wiki/Tic-tac-toe) library for Elm.

## An example

It shows how to use the library to play Tic-tac-toe.

```elm
import XO exposing (Player(..))

game0 = XO.start X
-- Playing { board = [], first = X, turn = X }

game1 = XO.play (1,1) game0 |> Result.withDefault game0
-- Playing { board = [((1,1),X)], first = X, turn = O }

game2 = XO.play (0,0) game1 |> Result.withDefault game1
-- Playing { board = [((0,0),O),((1,1),X)], first = X, turn = X }

XO.play (3,3) game2
-- Err (OutOfBounds (3,3))

XO.play (1,1) game2
-- Err (Occupied (1,1))

game3 = XO.play (2,2) game2 |> Result.withDefault game2
-- Playing { board = [((2,2),X),((0,0),O),((1,1),X)], first = X, turn = O }

game4 = XO.play (2,1) game3 |> Result.withDefault game3
-- Playing { board = [((2,1),O),((2,2),X),((0,0),O),((1,1),X)], first = X, turn = X }

XO.toString game4
-- "o...x..ox"
--
-- Which represents the following board:
--
--    0   1   2
-- 0  o |   |
--   ---+---+---
-- 1    | x |
--   ---+---+---
-- 2    | o | x
--

-- Where should I play?
XO.findGoodPositions game4
-- Ok { optimum = W, possibilities = [((0,2),3),((1,2),3)] }
--
-- i.e. X can win in 3 moves if X is played at (0,2) or (1,2)

game5 = XO.play (1,2) game4 |> Result.withDefault game4
-- Playing { board = [((1,2),X),((2,1),O),((2,2),X),((0,0),O),((1,1),X)], first = X, turn = O }

-- It doesn't matter where you play now, you're going to lose
game6 = XO.play (0,2) game5 |> Result.withDefault game5
-- Playing { board = [((0,2),O),((1,2),X),((2,1),O),((2,2),X),((0,0),O),((1,1),X)], first = X, turn = X }

-- Make the winning play
game7 = XO.play (1,0) game6 |> Result.withDefault game6
-- GameOver { board = [((1,0),X),((0,2),O),((1,2),X),((2,1),O),((2,2),X),((0,0),O),((1,1),X)], first = X, outcome = Win X [R2], turn = X }

XO.toState game7
-- { first = X, outcome = Win X [((1,0),(1,1),(1,2))], turn = X }
```

## Public API

A high-level overview of the types and functions that make up the library.

```txt
Player, nextPlayer

Game, start

Position, PlayError, play

Rules, defaultRules, playAgain

State, Outcome, Line, toState

openPositions

Tile, map

toString

Report, Optimum, AnalysisError, findGoodPositions
```
