module XO.Analyzer exposing (Error(..), Optimum(..), Report, findGoodPositions)

import XO.Board as Board exposing (Board, Position)
import XO.Mark as Mark exposing (Mark)
import XO.Referee as Referee exposing (Outcome(..))


type alias Report =
    { optimum : Optimum
    , possibilities : List ( Position, Int )
    }


type Optimum
    = W
    | D
    | L


type Error
    = NoOpenPositions
    | NoGameInProgress


findGoodPositions : Mark -> Board -> Result Error Report
findGoodPositions turn board =
    let
        openPositions =
            Board.openPositions board

        n =
            List.length openPositions
    in
    if n == 0 then
        Err NoOpenPositions

    else if n == 9 then
        Ok
            { optimum = D
            , possibilities =
                [ ( ( 0, 0 ), 9 )
                , ( ( 0, 1 ), 9 )
                , ( ( 0, 2 ), 9 )
                , ( ( 1, 0 ), 9 )
                , ( ( 1, 1 ), 9 )
                , ( ( 1, 2 ), 9 )
                , ( ( 2, 0 ), 9 )
                , ( ( 2, 1 ), 9 )
                , ( ( 2, 2 ), 9 )
                ]
            }

    else
        search openPositions (Mark.swap turn) turn board
            |> Maybe.map
                (\{ value, possibilities } ->
                    Ok
                        { optimum =
                            if value == 1 then
                                W

                            else if value == 0 then
                                D

                            else
                                -- value == -1
                                L
                        , possibilities = List.reverse possibilities
                        }
                )
            |> Maybe.withDefault (Err NoGameInProgress)


type alias State =
    { value : Int
    , possibilities : List ( Position, Int )
    }


search : List Position -> Mark -> Mark -> Board -> Maybe State
search openPositions prevTurn turn board =
    if Referee.decide prevTurn board == Nothing then
        Just <|
            List.foldl
                (\pos state ->
                    let
                        nextBoard =
                            Board.put pos turn board

                        ( value, numMoves ) =
                            minimize 1 turn prevTurn nextBoard
                    in
                    if value > state.value then
                        State value [ ( pos, numMoves ) ]

                    else if value == state.value then
                        { state | possibilities = ( pos, numMoves ) :: state.possibilities }

                    else
                        state
                )
                (State minValue [])
                openPositions

    else
        Nothing


maximize : Int -> Mark -> Mark -> Board -> ( Int, Int )
maximize numMoves prevTurn turn board =
    case Referee.decide prevTurn board of
        Nothing ->
            board
                |> Board.openPositions
                |> List.foldl
                    (\pos ( value, depth ) ->
                        let
                            nextBoard =
                                Board.put pos turn board

                            ( nextValue, nextNumMoves ) =
                                minimize (numMoves + 1) turn prevTurn nextBoard
                        in
                        if nextValue > value || (nextValue == value && nextNumMoves < depth) then
                            ( nextValue, nextNumMoves )

                        else
                            ( value, depth )
                    )
                    ( minValue, -1 )

        Just outcome ->
            ( minScore outcome
            , numMoves
            )


minimize : Int -> Mark -> Mark -> Board -> ( Int, Int )
minimize numMoves prevTurn turn board =
    case Referee.decide prevTurn board of
        Nothing ->
            board
                |> Board.openPositions
                |> List.foldl
                    (\pos ( value, depth ) ->
                        let
                            nextBoard =
                                Board.put pos turn board

                            ( nextValue, nextNumMoves ) =
                                maximize (numMoves + 1) turn prevTurn nextBoard
                        in
                        if nextValue < value || (nextValue == value && nextNumMoves < depth) then
                            ( nextValue, nextNumMoves )

                        else
                            ( value, depth )
                    )
                    ( maxValue, -1 )

        Just outcome ->
            ( maxScore outcome
            , numMoves
            )


maxScore : Outcome -> Int
maxScore outcome =
    case outcome of
        Win _ _ ->
            1

        Draw _ ->
            0


minScore : Outcome -> Int
minScore outcome =
    case outcome of
        Win _ _ ->
            -1

        Draw _ ->
            0


minValue : Int
minValue =
    -2


maxValue : Int
maxValue =
    2
