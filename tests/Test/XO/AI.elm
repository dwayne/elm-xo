module Test.XO.AI exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Test.XO.Helpers as Helpers
import XO.AI as AI
import XO.Board as Board exposing (Position)
import XO.Mark exposing (Mark(..))


suite : Test
suite =
    describe "XO.Board"
        [ findGoodPositionsSuite
        ]


findGoodPositionsSuite : Test
findGoodPositionsSuite =
    describe "findGoodPositions" <|
        List.map
            testFindGoodPositions
            --
            --    0   1   2
            -- 0  x |   | o
            --   ---+---+---
            -- 1    | x |
            --   ---+---+---
            -- 2    |   |
            --
            -- It finds the blocking move to avoid losing.
            --
            [ { start = X
              , positions = [ ( 0, 0 ), ( 0, 2 ), ( 1, 1 ) ]
              , turn = O
              , goodPositions = [ ( 2, 2 ) ]
              }

            --
            --    0   1   2
            -- 0  x | o |
            --   ---+---+---
            -- 1    | x |
            --   ---+---+---
            -- 2    |   |
            --
            -- It has no good moves since every position is losing.
            --
            , { start = X
              , positions = [ ( 0, 0 ), ( 0, 1 ), ( 1, 1 ) ]
              , turn = O
              , goodPositions = [ ( 0, 2 ), ( 1, 0 ), ( 1, 2 ), ( 2, 0 ), ( 2, 1 ), ( 2, 2 ) ]
              }

            --
            --    0   1   2
            -- 0  x |   | o
            --   ---+---+---
            -- 1  x |   |
            --   ---+---+---
            -- 2    | o |
            --
            -- It finds the winning moves.
            --
            , { start = X
              , positions = [ ( 0, 0 ), ( 0, 2 ), ( 1, 0 ), ( 2, 1 ) ]
              , turn = X
              , goodPositions = [ ( 1, 1 ), ( 1, 2 ), ( 2, 0 ), ( 2, 2 ) ]
              }

            --
            --    0   1   2
            -- 0  x |   | o
            --   ---+---+---
            -- 1    |   |
            --   ---+---+---
            -- 2  x |   | o
            --
            -- It favors winning over blocking.
            --
            , { start = X
              , positions = [ ( 2, 0 ), ( 0, 2 ), ( 0, 0 ), ( 2, 2 ) ]
              , turn = X
              , goodPositions = [ ( 1, 0 ) ]
              }
            ]


testFindGoodPositions :
    { start : Mark
    , positions : List Position
    , turn : Mark
    , goodPositions : List Position
    }
    -> Test
testFindGoodPositions { start, positions, turn, goodPositions } =
    test (Debug.toString { start = start, positions = positions, turn = turn }) <|
        \_ ->
            start
                |> Helpers.putMany positions
                |> AI.findGoodPositions turn
                |> Expect.equal goodPositions
