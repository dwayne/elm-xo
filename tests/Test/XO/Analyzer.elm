module Test.XO.Analyzer exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Test.XO.Helpers as Helpers
import XO.Analyzer as Analyzer exposing (Error, Report)
import XO.Board as Board exposing (Position)
import XO.Mark exposing (Mark(..))


suite : Test
suite =
    describe "XO.Board"
        [ findGoodPositionsSuite
        ]


findGoodPositionsSuite : Test
findGoodPositionsSuite =
    --
    -- N.B. All analysis assumes perfect play by both players.
    --
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
            -- It finds the blocking move to avoid losing. The best you can do is draw.
            --
            [ { start = X
              , positions = [ ( 0, 0 ), ( 0, 2 ), ( 1, 1 ) ]
              , turn = O
              , result =
                    Ok
                        { optimum = Analyzer.D
                        , possibilities = [ ( ( 2, 2 ), 6 ) ]
                        }
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
              , result =
                    Ok
                        { optimum = Analyzer.L
                        , possibilities =
                            [ ( ( 0, 2 ), 2 )
                            , ( ( 1, 0 ), 2 )
                            , ( ( 1, 2 ), 2 )
                            , ( ( 2, 0 ), 2 )
                            , ( ( 2, 1 ), 2 )
                            , ( ( 2, 2 ), 4 ) -- Block and you can extend the game a little more.
                            ]
                        }
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
              , result =
                    Ok
                        { optimum = Analyzer.W
                        , possibilities =
                            [ ( ( 1, 1 ), 3 ) -- Have some fun, play around a little and win in 3 moves.
                            , ( ( 1, 2 ), 3 )
                            , ( ( 2, 0 ), 1 ) -- Win immediately.
                            , ( ( 2, 2 ), 3 )
                            ]
                        }
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
              , result =
                    Ok
                        { optimum = Analyzer.W
                        , possibilities = [ ( ( 1, 0 ), 1 ) ]
                        }
              }
            ]


testFindGoodPositions :
    { start : Mark
    , positions : List Position
    , turn : Mark
    , result : Result Error Report
    }
    -> Test
testFindGoodPositions { start, positions, turn, result } =
    test (Debug.toString { start = start, positions = positions, turn = turn }) <|
        \_ ->
            start
                |> Helpers.putMany positions
                |> Analyzer.findGoodPositions turn
                |> Expect.equal result
