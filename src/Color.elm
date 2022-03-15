module Color exposing (Color, red, toString)


type Color
    = Color String


red : Color
red =
    Color "red"


toString : Color -> String
toString (Color str) =
    str
