module XO exposing
    ( Player(..), nextPlayer
    , Game, start
    , Position, PlayError(..), play
    , Rules, defaultRules, playAgain
    , State, Outcome(..), Line, toState
    , Tile, map
    , toString
    , openPositions, goodPositions
    )

{-| `XO` is a reference to **Xs and Os** which is an alternative name for [Tic-tac-toe](https://en.wikipedia.org/wiki/Tic-tac-toe).


# Player

@docs Player, nextPlayer


# Game

@docs Game, start


# Play

@docs Position, PlayError, play
@docs Rules, defaultRules, playAgain


# State

@docs State, Outcome, Line, toState


# Transform

@docs Tile, map
@docs toString


# AI

@docs openPositions, goodPositions

-}

import XO.AI as AI
import XO.Board as Board exposing (Board)
import XO.Mark as Mark exposing (Mark)
import XO.Referee as Referee exposing (Location(..))



-- PLAYER


{-| A player is either an `X` or an `O`.
-}
type Player
    = X
    | O


{-| Get the next player.

    nextPlayer X == O

    nextPlayer O == X

-}
nextPlayer : Player -> Player
nextPlayer player =
    case player of
        X ->
            O

        O ->
            X


fromMark : Mark -> Player
fromMark mark =
    case mark of
        Mark.X ->
            X

        Mark.O ->
            O


toMark : Player -> Mark
toMark player =
    case player of
        X ->
            Mark.X

        O ->
            Mark.O



-- GAME


{-| Keeps track of the internal state of a game.

This state includes: who played first, who's the current player, and the configuration of the board.

-}
type Game
    = Playing
        { first : Mark
        , turn : Mark
        , board : Board
        }
    | GameOver
        { first : Mark
        , turn : Mark
        , board : Board
        , outcome : Referee.Outcome
        }


{-| Initialize a game with the given player having the first move.
-}
start : Player -> Game
start =
    startHelper << toMark


startHelper : Mark -> Game
startHelper first =
    Playing
        { first = first
        , turn = first
        , board = []
        }



-- PLAY


{-| A `(row, column)` position. Both coordinates are 0-based and should range from 0 to 2.
-}
type alias Position =
    ( Int, Int )


{-| The possible errors that could occur while playing a game.
-}
type PlayError
    = OutOfBounds Position
    | Occupied Position
    | GameAlreadyEnded


{-| Make a move at the given position with the current player of a game.
-}
play : Position -> Game -> Result PlayError Game
play pos game =
    case game of
        Playing { first, turn, board } ->
            if Board.inBounds pos then
                if Board.isOpen pos board then
                    let
                        nextBoard =
                            Board.put pos turn board
                    in
                    case Referee.decide turn nextBoard of
                        Nothing ->
                            Ok <|
                                Playing
                                    { first = first
                                    , turn = Mark.swap turn
                                    , board = nextBoard
                                    }

                        Just outcome ->
                            Ok <|
                                GameOver
                                    { first = first
                                    , turn = turn
                                    , board = nextBoard
                                    , outcome = outcome
                                    }

                else
                    Err <| Occupied pos

            else
                Err <| OutOfBounds pos

        GameOver _ ->
            Err GameAlreadyEnded


{-| Various rules that help determine how subsequent games are played.
-}
type alias Rules =
    { winnerPlaysFirst : Bool
    , takeTurnsOnDraw : Bool
    }


{-| By default, the winner plays first and when a game ends in a draw the other player who didn't play first,
gets to play first in the next game.
-}
defaultRules : Rules
defaultRules =
    { winnerPlaysFirst = True
    , takeTurnsOnDraw = True
    }


{-| Play another game and initialize it using the given rules.
-}
playAgain : Rules -> Game -> Game
playAgain rules game =
    case game of
        Playing { first } ->
            startHelper first

        GameOver { first, turn, outcome } ->
            case outcome of
                Referee.Win _ _ ->
                    if rules.winnerPlaysFirst then
                        startHelper turn

                    else
                        startHelper <| Mark.swap turn

                Referee.Draw _ ->
                    if rules.takeTurnsOnDraw then
                        startHelper <| Mark.swap turn

                    else
                        startHelper first



-- STATE


{-| The external state of a game.

  - Who played first?
  - Whose turn is it?
  - What's the outcome?

-}
type alias State =
    { first : Player
    , turn : Player
    , outcome : Outcome
    }


{-| Either there is a winner, the game ended in a draw, or it is undecided because the game is still in progress.
-}
type Outcome
    = Win Player (List Line)
    | Draw Player
    | Undecided


{-| The three positions on the board that represent the winning line.
-}
type alias Line =
    ( Position, Position, Position )


{-| Get the state of a game.
-}
toState : Game -> State
toState game =
    case game of
        Playing { first, turn } ->
            State (fromMark first) (fromMark turn) Undecided

        GameOver { first, turn, outcome } ->
            State (fromMark first) (fromMark turn) <|
                case outcome of
                    Referee.Win mark locations ->
                        Win (fromMark mark) (List.map toLine locations)

                    Referee.Draw mark ->
                        Draw (fromMark mark)


toLine : Location -> Line
toLine location =
    case location of
        R1 ->
            ( ( 0, 0 ), ( 0, 1 ), ( 0, 2 ) )

        R2 ->
            ( ( 1, 0 ), ( 1, 1 ), ( 1, 2 ) )

        R3 ->
            ( ( 2, 0 ), ( 2, 1 ), ( 2, 2 ) )

        C1 ->
            ( ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) )

        C2 ->
            ( ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) )

        C3 ->
            ( ( 0, 2 ), ( 1, 2 ), ( 2, 2 ) )

        D1 ->
            ( ( 0, 0 ), ( 1, 1 ), ( 2, 2 ) )

        D2 ->
            ( ( 0, 2 ), ( 1, 1 ), ( 2, 0 ) )



-- TRANSFORM


{-| Represents which player, if any, is at a position on a Tic-tac-toe board.
-}
type alias Tile =
    Maybe Player


{-| There are 9 positions/tiles on a Tic-tac-toe board. Apply a function over those
positions/tiles in [row-major order](https://en.wikipedia.org/wiki/Row-_and_column-major_order).

    map Tuple.pair (start X)
        == [ ( ( 0, 0 ), Nothing )
           , ( ( 0, 1 ), Nothing )
           , ( ( 0, 2 ), Nothing )
           , ( ( 1, 0 ), Nothing )
           , ( ( 1, 1 ), Nothing )
           , ( ( 1, 2 ), Nothing )
           , ( ( 2, 0 ), Nothing )
           , ( ( 2, 1 ), Nothing )
           , ( ( 2, 2 ), Nothing )
           ]

-}
map : (Position -> Tile -> a) -> Game -> List a
map f =
    toBoard >> Board.map (\pos tile -> f pos (Maybe.map fromMark tile))


toBoard : Game -> Board
toBoard game =
    case game of
        Playing { board } ->
            board

        GameOver { board } ->
            board


{-| A representation of the layout of the board for a game in [row-major order](https://en.wikipedia.org/wiki/Row-_and_column-major_order).

    toString (start X) == "........."

    let
        game0 =
            start X

        game1 =
            play ( 0, 0 ) game0 |> Result.withDefault game0

        game2 =
            play ( 1, 1 ) game1 |> Result.withDefault game1
    in
    toString game2 == "x...o...."

-}
toString : Game -> String
toString =
    String.fromList << map (always tileToChar)


tileToChar : Tile -> Char
tileToChar tile =
    case tile of
        Just X ->
            'x'

        Just O ->
            'o'

        Nothing ->
            '.'



-- AI


{-| Get the unoccupied positions on the board of a game.
-}
openPositions : Game -> List Position
openPositions game =
    case game of
        Playing { board } ->
            Board.openPositions board

        GameOver _ ->
            []


{-| Get the good unoccupied positions on the board of a game for the current player.

**N.B.** _If a configuration of the board for a game is ultimately losing for the current player, assuming the other player plays perfectly, then it really doesn't matter where they play. In that case, all the open positions are returned. For e.g. suppose they were playing and they ended up with this configuration of the board, `xo..x....`, then there is no position `O` can play to avoid losing, assuming `X` plays perfectly. In particular, blocking `X` at `(2, 2)` is not a good move since it only delays the inevitable. On the other hand, it can be argued that blocking `X` at `(2, 2)` is a good move assuming their opponent can make mistakes because it gives them a slight chance of not losing. This function assumes an opponent plays perfectly._

-}
goodPositions : Game -> List Position
goodPositions game =
    case game of
        Playing { turn, board } ->
            AI.findGoodPositions turn board

        GameOver _ ->
            []
