module XO exposing
    ( Player(..), nextPlayer
    , Game, start
    , Position, PlayError(..), play
    , Rules, defaultRules, playAgain
    , State, Outcome(..), Line, toState
    , openPositions
    , Tile, map
    , toString
    , Report, Optimum(..), AnalysisError(..), findGoodPositions
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
@docs openPositions


# Transform

@docs Tile, map
@docs toString


# Analyze

@docs Report, Optimum, AnalysisError, findGoodPositions

-}

import XO.Analyzer as Analyzer
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


{-| Get the unoccupied positions on the board of a game.
-}
openPositions : Game -> List Position
openPositions game =
    case game of
        Playing { board } ->
            Board.openPositions board

        GameOver _ ->
            []



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



-- Analyze


{-| The report tells you the best outcome that can be achieved, assuming both players play perfectly.

`possibilities` is a list of pairs of position and number of moves. Each pair represents the
position to play, to achieve the best outcome, and the number of moves it takes to achieve it.

-}
type alias Report =
    { optimum : Optimum
    , possibilities : List ( Position, Int )
    }


{-| The best outcome that can be achieved.

  - `W` means you can win
  - `D` means you can draw at best
  - `L` means no matter what position is played you will lose

-}
type Optimum
    = W
    | D
    | L


{-| Not all games can be analyzed. For e.g. if all positions are occupied or
the game is over then there's nothing to analyze.
-}
type AnalysisError
    = NoOpenPositions
    | NoGameInProgress


{-| Find the good unoccupied positions on the board of a game for the current player.


## Example 1

Suppose `O` has the next turn in a game, `g1`, with a board layout of `x.o.x....`, then

    --
    --    0   1   2
    -- 0  x |   | o
    --   ---+---+---
    -- 1    | x |
    --   ---+---+---
    -- 2    |   |
    --
    findGoodPositions g1
        == { optimum = D
           , possibilities = [ ( ( 2, 2 ), 6 ) ]
           }

It means that `O` can draw at best, in 6 moves, if `O` is played at `(2, 2)`.


## Example 2

Suppose `O` has the next turn in a game, `g2`, with a board layout of `xo..x....`, then

    --
    --    0   1   2
    -- 0  x | o |
    --   ---+---+---
    -- 1    | x |
    --   ---+---+---
    -- 2    |   |
    --
    findGoodPositions g2
        == { optimum = L
           , possibilities =
                [ ( ( 0, 2 ), 2 )
                , ( ( 1, 0 ), 2 )
                , ( ( 1, 2 ), 2 )
                , ( ( 2, 0 ), 2 )
                , ( ( 2, 1 ), 2 )
                , ( ( 2, 2 ), 4 )
                ]
           }

It means that no matter the position `O` plays, `O` will lose. However, if
`O` is played at `(2, 2)`, in order to block `X` from the immediate win, then
`O` can extend the game a little more.


## Example 3

Suppose `X` has the next turn in a game, `g3`, with a board layout of `x.ox...o.`, then

    --
    --    0   1   2
    -- 0  x |   | o
    --   ---+---+---
    -- 1  x |   |
    --   ---+---+---
    -- 2    | o |
    --
    findGoodPositions g3
        == { optimum = W
           , possibilities =
                [ ( ( 1, 1 ), 3 )
                , ( ( 1, 2 ), 3 )
                , ( ( 2, 0 ), 1 )
                , ( ( 2, 2 ), 3 )
                ]
           }

It means that `X` can win. `X` can either win immediately if `X` is played at `(2, 0)`, or
`X` can have some fun and win in 3 moves if `X` is played at `(1, 1)`, `(1, 2)`, or `(2, 2)`.


## Example 4

Suppose `X` has the next turn in a game, `g4`, with a board layout of `x.o...x.o`, then

    --
    --    0   1   2
    -- 0  x |   | o
    --   ---+---+---
    -- 1    |   |
    --   ---+---+---
    -- 2  x |   | o
    --
    findGoodPositions g4
        == { optimum = W
           , possibilities = [ ( ( 1, 0 ), 1 ) ]
           }

It means that `X` can win immediately if `X` is played at `(1, 0)`. Notice that
winning is favoured over blocking `O` at `(1, 2)`.

**N.B.** _All the analysis above assumes perfect play by both players._

-}
findGoodPositions : Game -> Result AnalysisError Report
findGoodPositions game =
    let
        state =
            case game of
                Playing { turn, board } ->
                    { turn = turn, board = board }

                GameOver { turn, board } ->
                    { turn = turn, board = board }
    in
    case Analyzer.findGoodPositions state.turn state.board of
        Ok { optimum, possibilities } ->
            Ok
                { optimum =
                    case optimum of
                        Analyzer.W ->
                            W

                        Analyzer.D ->
                            D

                        Analyzer.L ->
                            L
                , possibilities = possibilities
                }

        Err Analyzer.NoOpenPositions ->
            Err NoOpenPositions

        Err Analyzer.NoGameInProgress ->
            Err NoGameInProgress
