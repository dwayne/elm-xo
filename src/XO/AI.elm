module XO.AI exposing (findGoodPositions)

import XO.Board as Board exposing (Board, Position)
import XO.Mark as Mark exposing (Mark)
import XO.Referee as Referee exposing (Outcome(..))


findGoodPositions : Mark -> Board -> List Position
findGoodPositions turn board =
    let
        openPositions =
            Board.openPositions board

        n =
            List.length openPositions
    in
    if n == 0 || n == 1 || n == 9 then
        openPositions

    else
        search openPositions (Mark.swap turn) turn board


type alias State =
    { value : Int
    , positions : List Position
    }


initState : State
initState =
    { value = minValue
    , positions = []
    }


minValue : Int
minValue =
    -2


search : List Position -> Mark -> Mark -> Board -> List Position
search openPositions prevTurn turn board =
    if Referee.decide prevTurn board == Nothing then
        openPositions
            |> List.foldl
                (\pos state ->
                    let
                        nextBoard =
                            Board.put pos turn board

                        nextValue =
                            negamax -1 turn prevTurn nextBoard
                    in
                    if nextValue > state.value then
                        State nextValue [ pos ]

                    else if nextValue == state.value then
                        { state | positions = pos :: state.positions }

                    else
                        state
                )
                initState
            |> .positions
            |> List.reverse

    else
        []


negamax : Int -> Mark -> Mark -> Board -> Int
negamax color prevTurn turn board =
    case Referee.decide prevTurn board of
        Nothing ->
            board
                |> Board.openPositions
                |> List.foldl
                    (\pos value ->
                        let
                            nextBoard =
                                Board.put pos turn board
                        in
                        max value (color * negamax -color turn prevTurn nextBoard)
                    )
                    minValue
                |> (*) color

        Just outcome ->
            -(color * score outcome)


score : Outcome -> Int
score outcome =
    case outcome of
        Win _ _ ->
            1

        Draw _ ->
            0
