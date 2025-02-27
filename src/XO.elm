module XO exposing
    ( Game
    , Line
    , Outcome(..)
    , PlayError(..)
    , Player(..)
    , Position
    , Rules
    , State
    , defaultRules
    , goodPositions
    , map
    , nextPlayer
    , openPositions
    , play
    , playAgain
    , start
    , toState
    , toString
    )

import XO.AI as AI
import XO.Board as Board exposing (Board)
import XO.Mark as Mark exposing (Mark)
import XO.Referee as Referee exposing (Location(..))



-- PLAYER


type Player
    = X
    | O


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


type alias Position =
    Board.Position


type PlayError
    = OutOfBounds Position
    | Occupied Position
    | GameAlreadyEnded


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


type alias Rules =
    { winnerPlaysFirst : Bool
    , takeTurnsOnDraw : Bool
    }


defaultRules : Rules
defaultRules =
    { winnerPlaysFirst = True
    , takeTurnsOnDraw = True
    }


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


type alias State =
    { first : Player
    , turn : Player
    , outcome : Outcome
    }


type Outcome
    = Win Player (List Line)
    | Draw Player
    | Undecided


type alias Line =
    ( Position, Position, Position )


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


type alias Tile =
    Maybe Player


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


openPositions : Game -> List Position
openPositions game =
    case game of
        Playing { board } ->
            Board.openPositions board

        GameOver _ ->
            []


goodPositions : Game -> List Position
goodPositions game =
    case game of
        Playing { turn, board } ->
            AI.findGoodPositions turn board

        GameOver _ ->
            []
