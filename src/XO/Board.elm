module XO.Board exposing
    ( Board
    , Move
    , Position
    , Tile
    , allPositions
    , inBounds
    , isOpen
    , map
    , openPositions
    , put
    , toString
    , toTiles
    )

import XO.Mark as Mark exposing (Mark)



-- CONSTANTS


allPositions : List Position
allPositions =
    [ ( 0, 0 )
    , ( 0, 1 )
    , ( 0, 2 )
    , ( 1, 0 )
    , ( 1, 1 )
    , ( 1, 2 )
    , ( 2, 0 )
    , ( 2, 1 )
    , ( 2, 2 )
    ]



-- BOARD


type alias Board =
    List Move


type alias Move =
    ( Position, Mark )


type alias Position =
    ( Int, Int )



-- MODIFY


put : Position -> Mark -> Board -> Board
put pos mark =
    (::) ( pos, mark )



-- QUERY


isOpen : Position -> Board -> Bool
isOpen pos =
    find pos >> (==) Nothing


inBounds : Position -> Bool
inBounds ( r, c ) =
    r >= 0 && r < 3 && c >= 0 && c < 3


openPositions : Board -> List Position
openPositions board =
    List.filter (flip isOpen board) allPositions



-- CONVERT


type alias Tile =
    Maybe Mark


map : (Position -> Tile -> a) -> Board -> List a
map f board =
    List.map (\pos -> f pos (find pos board)) allPositions


toTiles : Board -> List Tile
toTiles =
    map (\_ tile -> tile)


toString : Board -> String
toString =
    String.concat
        << map
            (\_ tile ->
                case tile of
                    Just mark ->
                        Mark.toString mark

                    Nothing ->
                        "."
            )



-- HELPERS


find : a -> List ( a, b ) -> Maybe b
find needle haystack =
    case haystack of
        [] ->
            Nothing

        ( key, value ) :: restOfHaystack ->
            if key == needle then
                Just value

            else
                find needle restOfHaystack


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b
