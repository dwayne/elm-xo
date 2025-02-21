module Test.XO.Helpers exposing (putMany)

import XO.Board as Board exposing (Board, Position)
import XO.Mark as Mark exposing (Mark)


putMany : List Position -> Mark -> Board
putMany =
    putManyHelper []


putManyHelper : Board -> List Position -> Mark -> Board
putManyHelper board positions turn =
    case positions of
        [] ->
            board

        pos :: restPositions ->
            let
                nextBoard =
                    Board.put pos turn board

                nextTurn =
                    Mark.swap turn
            in
            putManyHelper nextBoard restPositions nextTurn
