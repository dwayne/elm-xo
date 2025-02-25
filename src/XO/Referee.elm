module XO.Referee exposing (Location(..), Outcome(..), decide)

import XO.Board as Board exposing (Board, Tile)
import XO.Mark exposing (Mark)


type Outcome
    = Win Mark (List Location)
    | Draw Mark


type Location
    = R1
    | R2
    | R3
    | C1
    | C2
    | C3
    | D1
    | D2


decide : Mark -> Board -> Maybe Outcome
decide mark board =
    let
        tiles =
            Board.toTiles board
    in
    case findWin mark tiles of
        [] ->
            if isDraw tiles then
                Just <| Draw mark

            else
                Nothing

        locations ->
            Just <| Win mark locations


findWin : Mark -> List Tile -> List Location
findWin mark =
    let
        t =
            Just mark
    in
    toWinningArrangements >> keyFilter ( t, t, t )


toWinningArrangements : List Tile -> List ( ( Tile, Tile, Tile ), Location )
toWinningArrangements tiles =
    case tiles of
        [ a, b, c, d, e, f, g, h, i ] ->
            [ ( ( a, b, c ), R1 )
            , ( ( d, e, f ), R2 )
            , ( ( g, h, i ), R3 )
            , ( ( a, d, g ), C1 )
            , ( ( b, e, h ), C2 )
            , ( ( c, f, i ), C3 )
            , ( ( a, e, i ), D1 )
            , ( ( c, e, g ), D2 )
            ]

        _ ->
            []


keyFilter : k -> List ( k, v ) -> List v
keyFilter key =
    List.filterMap
        (\( k, v ) ->
            if key == k then
                Just v

            else
                Nothing
        )


isDraw : List Tile -> Bool
isDraw =
    List.all ((/=) Nothing)
