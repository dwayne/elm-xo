module Test.XO.Board exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Test.XO.Helpers as Helpers
import XO.Board as Board exposing (Position)
import XO.Mark exposing (Mark(..))


suite : Test
suite =
    describe "XO.Board"
        [ isOpenSuite
        , openPositionsSuite
        , toStringSuite
        ]


isOpenSuite : Test
isOpenSuite =
    describe "isOpen"
        [ test "( 0, 1 ) is open" <|
            \_ ->
                []
                    |> Board.isOpen ( 0, 1 )
                    |> Expect.equal True
        , test "( 0, 1 ) is occupied" <|
            \_ ->
                X
                    |> Helpers.putMany [ ( 0, 1 ) ]
                    |> Board.isOpen ( 0, 1 )
                    |> Expect.equal False
        ]


openPositionsSuite : Test
openPositionsSuite =
    describe "openPositions"
        [ test "all open" <|
            \_ ->
                []
                    |> Board.openPositions
                    |> Expect.equal Board.allPositions
        , test "( 0, 1 ), ( 1, 0 ), ( 1, 2 ), ( 2, 1 ) is open" <|
            \_ ->
                X
                    |> Helpers.putMany [ ( 0, 0 ), ( 0, 2 ), ( 1, 1 ), ( 2, 0 ), ( 2, 2 ) ]
                    |> Board.openPositions
                    |> Expect.equal [ ( 0, 1 ), ( 1, 0 ), ( 1, 2 ), ( 2, 1 ) ]
        ]


toStringSuite : Test
toStringSuite =
    describe "toString" <|
        List.map
            testToString
            [ { start = X
              , positions = []
              , layout = "........."
              }
            , { start = X
              , positions = [ ( 0, 0 ), ( 1, 1 ) ]
              , layout = "x...o...."
              }
            , { start = O
              , positions = [ ( 0, 0 ), ( 1, 1 ) ]
              , layout = "o...x...."
              }
            ]


testToString : { start : Mark, positions : List Position, layout : String } -> Test
testToString { start, positions, layout } =
    test (Debug.toString { start = start, positions = positions }) <|
        \_ ->
            start
                |> Helpers.putMany positions
                |> Board.toString
                |> Expect.equal layout
