module XO.Mark exposing (Mark(..), swap, toString)


type Mark
    = X
    | O


swap : Mark -> Mark
swap mark =
    case mark of
        X ->
            O

        O ->
            X


toString : Mark -> String
toString mark =
    case mark of
        X ->
            "x"

        O ->
            "o"
