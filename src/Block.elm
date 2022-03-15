module Block exposing (Block, center, rotate, sample, toGrid)

import Color exposing (Color)
import Grid exposing (Grid)


{-| A block is an individual piece of Tetris. It has a color and a
non-empty list of coordinates describing its shape.
-}
type Block
    = Block Color ( Int, Int ) (List ( Int, Int ))


sample : Block
sample =
    Block Color.red
        ( 0, 0 )
        [ ( 0, 1 )
        , ( 1, 1 )
        , ( 0, 2 )
        ]


center : Block -> ( Int, Int )
center (Block _ ( x, y ) positions) =
    let
        length =
            toFloat (List.length positions + 1)

        ( xs, ys ) =
            List.unzip positions
    in
    ( round (toFloat (List.sum (x :: xs)) / length)
    , round (toFloat (List.sum (y :: ys)) / length)
    )


rotate : Block -> Block
rotate (Block color ( firstX, firstY ) positions) =
    let
        ys =
            firstY :: List.map Tuple.second positions

        maxY =
            Maybe.withDefault firstY (List.maximum ys)

        rotatePos ( x, y ) =
            ( maxY - y, x )
    in
    Block color (rotatePos ( firstX, firstY )) (List.map rotatePos positions)


toGrid : Block -> Grid
toGrid (Block color ( firstX, firstY ) positions) =
    let
        ( posXs, posYs ) =
            List.unzip positions

        ( xs, ys ) =
            ( firstX :: posXs
            , firstY :: posYs
            )

        width =
            Maybe.withDefault firstX (List.maximum xs)
                - Maybe.withDefault firstX (List.minimum xs)
                + 1

        height =
            Maybe.withDefault firstY (List.maximum ys)
                - Maybe.withDefault firstY (List.minimum ys)
                + 1
    in
    Grid.build
        width
        height
        (( color, ( firstX, firstY ) ) :: List.map (Tuple.pair color) positions)
