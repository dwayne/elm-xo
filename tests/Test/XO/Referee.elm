module Test.XO.Referee exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Test.XO.Helpers as Helpers
import XO.Board as Board exposing (Position)
import XO.Mark exposing (Mark(..))
import XO.Referee as Referee exposing (Location(..), Outcome(..))


suite : Test
suite =
    describe "XO.Referee"
        [ decideSuite
        ]


decideSuite : Test
decideSuite =
    describe "decide" <|
        List.map
            testDecide
            [ { description = "when X wins"
              , start = X
              , positions = [ ( 0, 0 ), ( 1, 0 ), ( 0, 1 ), ( 1, 1 ), ( 0, 2 ) ]
              , mark = X
              , maybeOutcome = Just (Win X [ R1 ])
              }
            , { description = "when O wins"
              , start = O
              , positions = [ ( 2, 0 ), ( 1, 0 ), ( 2, 1 ), ( 1, 1 ), ( 2, 2 ) ]
              , mark = O
              , maybeOutcome = Just (Win O [ R3 ])
              }
            , { description = "when the rare multi-win occurs"
              , start = X
              , positions =
                    [ ( 0, 0 )
                    , ( 1, 0 )
                    , ( 2, 2 )
                    , ( 2, 0 )
                    , ( 0, 1 )
                    , ( 2, 1 )
                    , ( 1, 2 )
                    , ( 1, 1 )
                    , ( 0, 2 )
                    ]
              , mark = X
              , maybeOutcome = Just (Win X [ R1, C3 ])
              }
            , { description = "when it's a draw"
              , start = X
              , positions =
                    [ ( 0, 0 )
                    , ( 1, 1 )
                    , ( 2, 2 )
                    , ( 0, 1 )
                    , ( 2, 1 )
                    , ( 2, 0 )
                    , ( 0, 2 )
                    , ( 1, 2 )
                    , ( 1, 0 )
                    ]
              , mark = X
              , maybeOutcome = Just (Draw X)
              }
            , { description = "after 2 moves it's still undecided"
              , start = X
              , positions = [ ( 0, 0 ), ( 1, 1 ) ]
              , mark = O
              , maybeOutcome = Nothing
              }
            ]


testDecide :
    { description : String
    , start : Mark
    , positions : List Position
    , mark : Mark
    , maybeOutcome : Maybe Outcome
    }
    -> Test
testDecide { description, start, positions, mark, maybeOutcome } =
    test description <|
        \_ ->
            start
                |> Helpers.putMany positions
                |> Referee.decide mark
                |> Expect.equal maybeOutcome
