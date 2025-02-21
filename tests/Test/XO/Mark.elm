module Test.XO.Mark exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import XO.Mark as Mark exposing (Mark(..))


suite : Test
suite =
    describe "XO.Mark"
        [ swapSuite
        , toStringSuite
        ]


swapSuite : Test
swapSuite =
    describe "swap"
        [ test "X -> O" <|
            \_ ->
                Mark.swap X
                    |> Expect.equal O
        , test "O -> X" <|
            \_ ->
                Mark.swap O
                    |> Expect.equal X
        ]


toStringSuite : Test
toStringSuite =
    describe "toString"
        [ test "x" <|
            \_ ->
                Mark.toString X
                    |> Expect.equal "x"
        , test "o" <|
            \_ ->
                Mark.toString O
                    |> Expect.equal "o"
        ]
