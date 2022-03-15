module Grid exposing (Grid, build, dimensions, empty, toList)

import Color exposing (Color)


type Grid
    = Grid
        { width : Int
        , height : Int
        , cells : List Cell
        }


type alias Cell =
    ( Color, ( Int, Int ) )


empty : Int -> Int -> Grid
empty w h =
    Grid
        { width = w
        , height = h
        , cells = []
        }


dimensions : Grid -> ( Int, Int )
dimensions (Grid { width, height }) =
    ( width, height )


build : Int -> Int -> List Cell -> Grid
build w h cells =
    let
        validCell ( color, ( x, y ) ) =
            x >= 0 && x < w && y >= 0 && y < h
    in
    Grid
        { width = w
        , height = h
        , cells = List.filter validCell cells
        }


toList : Grid -> List Cell
toList (Grid { cells }) =
    cells
