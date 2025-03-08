module Test.XO exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import XO exposing (Game, Outcome(..), PlayError(..), Player(..), Position, Rules, defaultRules)


suite : Test
suite =
    describe "XO"
        [ playSuite
        , playAgainSuite
        , openPositionsSuite
        , findGoodPositionsSuite
        ]


playSuite : Test
playSuite =
    describe "play" <|
        List.map testPlay
            [ { description = "after 3 plays"
              , game =
                    X
                        |> playMany [ ( 1, 1 ), ( 0, 2 ), ( 2, 0 ) ]
                        |> Ok
              , state =
                    Ok
                        { first = X
                        , turn = O
                        , layout = "..o.x.x.."
                        , outcome = Undecided
                        }
              }
            , { description = "when X wins"
              , game =
                    X
                        |> playMany
                            [ ( 1, 1 )
                            , ( 0, 2 )
                            , ( 2, 0 )
                            , ( 1, 2 )
                            , ( 2, 2 )
                            , ( 2, 1 )
                            , ( 0, 0 )
                            ]
                        |> Ok
              , state =
                    Ok
                        { first = X
                        , turn = X
                        , layout = "x.o.xoxox"
                        , outcome = Win X [ ( ( 0, 0 ), ( 1, 1 ), ( 2, 2 ) ) ]
                        }
              }
            , { description = "when O draws"
              , game =
                    O
                        |> playMany
                            [ ( 1, 1 )
                            , ( 0, 0 )
                            , ( 2, 2 )
                            , ( 0, 2 )
                            , ( 0, 1 )
                            , ( 2, 1 )
                            , ( 1, 2 )
                            , ( 1, 0 )
                            , ( 2, 0 )
                            ]
                        |> Ok
              , state =
                    Ok
                        { first = O
                        , turn = O
                        , layout = "xoxxoooxo"
                        , outcome = Draw O
                        }
              }
            , { description = "when the position is already taken"
              , game =
                    X
                        |> playMany [ ( 1, 1 ) ]
                        |> XO.play ( 1, 1 )
              , state =
                    Err <| Occupied ( 1, 1 )
              }
            , { description = "when the position is not on the board"
              , game =
                    X
                        |> XO.start
                        |> XO.play ( 0, 4 )
              , state =
                    Err <| OutOfBounds ( 0, 4 )
              }
            , { description = "when the game has already ended"
              , game =
                    O
                        |> playMany
                            [ ( 0, 0 )
                            , ( 1, 0 )
                            , ( 0, 1 )
                            , ( 1, 1 )
                            , ( 0, 2 )
                            ]
                        |> XO.play ( 1, 2 )
              , state =
                    Err GameAlreadyEnded
              }
            ]


testPlay :
    { description : String
    , game : Result PlayError Game
    , state : Result PlayError State
    }
    -> Test
testPlay { description, game, state } =
    test description <|
        \_ ->
            game
                |> Result.map toState
                |> Expect.equal state


type alias State =
    { first : Player
    , turn : Player
    , layout : String
    , outcome : Outcome
    }


toState : Game -> State
toState game =
    let
        { first, turn, outcome } =
            XO.toState game
    in
    { first = first
    , turn = turn
    , layout = XO.toString game
    , outcome = outcome
    }


playAgainSuite : Test
playAgainSuite =
    let
        undecidedPositions =
            [ ( 1, 1 ) ]

        winPositions =
            [ ( 1, 1 )
            , ( 0, 2 )
            , ( 2, 0 )
            , ( 1, 2 )
            , ( 2, 2 )
            , ( 2, 1 )
            , ( 0, 0 )
            ]

        drawPositions =
            [ ( 1, 1 )
            , ( 0, 0 )
            , ( 2, 2 )
            , ( 0, 2 )
            , ( 0, 1 )
            , ( 2, 1 )
            , ( 1, 2 )
            , ( 1, 0 )
            , ( 2, 0 )
            ]
    in
    describe "playAgain" <|
        List.map testPlayAgain
            --
            -- winnerPlaysFirst = True, takeTurnsOnDraw = True
            --
            [ { rules = defaultRules
              , game = playMany undecidedPositions X
              , first = X
              }
            , { rules = defaultRules
              , game = playMany winPositions X
              , first = X
              }
            , { rules = defaultRules
              , game = playMany drawPositions X
              , first = O
              }

            --
            -- winnerPlaysFirst = False, takeTurnsOnDraw = True
            --
            , { rules = { defaultRules | winnerPlaysFirst = False }
              , game = playMany undecidedPositions X
              , first = X
              }
            , { rules = { defaultRules | winnerPlaysFirst = False }
              , game = playMany winPositions X
              , first = O
              }
            , { rules = { defaultRules | winnerPlaysFirst = False }
              , game = playMany drawPositions X
              , first = O
              }

            --
            -- winnerPlaysFirst = True, takeTurnsOnDraw = False
            --
            , { rules = { defaultRules | takeTurnsOnDraw = False }
              , game = playMany undecidedPositions X
              , first = X
              }
            , { rules = { defaultRules | takeTurnsOnDraw = False }
              , game = playMany winPositions X
              , first = X
              }
            , { rules = { defaultRules | takeTurnsOnDraw = False }
              , game = playMany drawPositions X
              , first = X
              }

            --
            -- winnerPlaysFirst = False, takeTurnsOnDraw = False
            --
            , { rules = { winnerPlaysFirst = False, takeTurnsOnDraw = False }
              , game = playMany undecidedPositions X
              , first = X
              }
            , { rules = { winnerPlaysFirst = False, takeTurnsOnDraw = False }
              , game = playMany winPositions X
              , first = O
              }
            , { rules = { winnerPlaysFirst = False, takeTurnsOnDraw = False }
              , game = playMany drawPositions X
              , first = X
              }
            ]


testPlayAgain :
    { rules : Rules
    , game : Game
    , first : Player
    }
    -> Test
testPlayAgain { rules, game, first } =
    test (Debug.toString { rules = rules, layout = XO.toString game }) <|
        \_ ->
            game
                |> XO.playAgain rules
                |> XO.toState
                |> Expect.equal
                    { first = first
                    , turn = first
                    , outcome = Undecided
                    }


openPositionsSuite : Test
openPositionsSuite =
    describe "openPositions"
        [ test "example 1" <|
            \_ ->
                X
                    |> playMany
                        [ ( 1, 1 )
                        , ( 0, 0 )
                        , ( 2, 2 )
                        , ( 0, 2 )
                        , ( 0, 1 )
                        , ( 2, 1 )
                        , ( 1, 2 )
                        ]
                    |> XO.openPositions
                    |> Expect.equal
                        [ ( 1, 0 )
                        , ( 2, 0 )
                        ]
        ]


findGoodPositionsSuite : Test
findGoodPositionsSuite =
    describe "findGoodPositions"
        [ test "it finds the blocking move to avoid losing" <|
            \_ ->
                X
                    |> playMany
                        [ ( 0, 0 )
                        , ( 0, 2 )
                        , ( 1, 1 )
                        ]
                    |> XO.findGoodPositions
                    |> Expect.equal
                        (Ok
                            { optimum = XO.D
                            , possibilities = [ ( ( 2, 2 ), 6 ) ]
                            }
                        )
        ]



-- HELPERS


playMany : List Position -> Player -> Game
playMany positions =
    playManyHelper positions << XO.start


playManyHelper : List Position -> Game -> Game
playManyHelper positions game =
    case positions of
        [] ->
            game

        pos :: restPositions ->
            case XO.play pos game of
                Ok nextGame ->
                    playManyHelper restPositions nextGame

                Err _ ->
                    playManyHelper restPositions game
