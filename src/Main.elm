module Main exposing (main)

import Block exposing (Block)
import Browser
import Browser.Events
import Color exposing (Color)
import Grid exposing (Grid)
import Html exposing (Html)
import Html.Events as Events
import Json.Decode as Decode
import Svg exposing (Svg)
import Svg.Attributes as SvgAttrs


type ActiveBlock
    = ActiveBlock Block ( Int, Float )


type GameState
    = Stopped
    | Playing ActiveBlock
    | Paused ActiveBlock


type alias Model =
    { gameState : GameState
    , grid : Grid
    }


type Msg
    = NoOp
    | Tick Float
    | Start
    | Pause
    | Resume
    | Rotate
    | MoveRight
    | MoveLeft


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


initialModel : Model
initialModel =
    { gameState = Stopped
    , grid = Grid.empty 10 20
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick elapsed ->
            ( animate elapsed model, Cmd.none )

        Start ->
            let
                activeBlock =
                    ActiveBlock Block.sample ( 0, 0.0 )
            in
            ( { model | gameState = Playing activeBlock }, Cmd.none )

        Pause ->
            let
                newModel =
                    case model.gameState of
                        Playing activeBlock ->
                            { model | gameState = Paused activeBlock }

                        _ ->
                            model
            in
            ( newModel, Cmd.none )

        Resume ->
            let
                newModel =
                    case model.gameState of
                        Paused activeBlock ->
                            { model | gameState = Playing activeBlock }

                        _ ->
                            model
            in
            ( newModel, Cmd.none )

        Rotate ->
            let
                newModel =
                    case model.gameState of
                        Playing (ActiveBlock block pos) ->
                            let
                                pos_ =
                                    Debug.log "pos" pos

                                _ =
                                    Debug.log "center" (Block.center (Block.rotate block))
                            in
                            { model | gameState = Playing (ActiveBlock (Block.rotate block) pos) }

                        _ ->
                            model
            in
            ( newModel, Cmd.none )

        MoveLeft ->
            let
                newModel =
                    case model.gameState of
                        Playing (ActiveBlock block ( xPos, yPos )) ->
                            { model | gameState = Playing (ActiveBlock block ( xPos - 1, yPos )) }

                        _ ->
                            model
            in
            ( newModel, Cmd.none )

        MoveRight ->
            let
                newModel =
                    case model.gameState of
                        Playing (ActiveBlock block ( xPos, yPos )) ->
                            { model | gameState = Playing (ActiveBlock block ( xPos + 1, yPos )) }

                        _ ->
                            model
            in
            ( newModel, Cmd.none )


animate : Float -> Model -> Model
animate elapsed model =
    case model.gameState of
        Playing (ActiveBlock block ( x, y )) ->
            let
                activeBlock =
                    ActiveBlock block ( x, y + (elapsed / 800) )
            in
            { model | gameState = Playing activeBlock }

        _ ->
            model


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text "Pentix" ]
        , Html.p [] [ Html.text (Debug.toString model.gameState) ]
        , Html.p [] <|
            case model.gameState of
                Stopped ->
                    [ Html.button [ Events.onClick Start ] [ Html.text "New Game" ] ]

                Playing _ ->
                    [ Html.button [ Events.onClick Pause ] [ Html.text "Pause" ]
                    , Html.button [ Events.onClick Start ] [ Html.text "Reset" ]
                    ]

                Paused _ ->
                    [ Html.button [ Events.onClick Resume ] [ Html.text "Resume" ]
                    , Html.button [ Events.onClick Start ] [ Html.text "Reset" ]
                    ]
        , viewGame model
        ]


viewGame : Model -> Html Msg
viewGame { grid, gameState } =
    let
        ( width, height ) =
            Grid.dimensions grid

        ( w, h ) =
            ( String.fromInt (width * 30)
            , String.fromInt (height * 30)
            )

        renderActiveBlock block ( x, y ) =
            let
                ( centerX, centerY ) =
                    Block.center block
            in
            block
                |> Block.toGrid
                |> Grid.toList
                |> List.map
                    (\( color, ( cellX, cellY ) ) ->
                        renderCell color
                            ( cellX + x - centerX
                            , cellY + floor y - centerY
                            )
                    )

        activeBlock =
            case gameState of
                Playing (ActiveBlock block ( x, y )) ->
                    renderActiveBlock block ( x, y )

                Paused (ActiveBlock block ( x, y )) ->
                    renderActiveBlock block ( x, y )

                Stopped ->
                    []
    in
    Svg.svg
        [ SvgAttrs.width w
        , SvgAttrs.height h
        ]
        ([ Svg.rect
            [ SvgAttrs.width w
            , SvgAttrs.height h
            , SvgAttrs.fill "rgb(236, 240, 241)"
            ]
            []
         ]
            ++ activeBlock
        )


renderCell : Color -> ( Int, Int ) -> Html Msg
renderCell color ( x, y ) =
    Svg.rect
        [ SvgAttrs.width (String.fromInt 30)
        , SvgAttrs.height (String.fromInt 30)
        , SvgAttrs.fill (Color.toString color)
        , SvgAttrs.stroke (Color.toString color)
        , SvgAttrs.strokeWidth "0.5"
        , SvgAttrs.x (String.fromInt (x * 30))
        , SvgAttrs.y (String.fromInt (y * 30))
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.gameState of
            Playing _ ->
                Browser.Events.onAnimationFrameDelta Tick

            _ ->
                Sub.none
        , Browser.Events.onKeyDown (Decode.map key Events.keyCode)
        ]


key : Int -> Msg
key keycode =
    case keycode of
        38 ->
            Rotate

        37 ->
            MoveLeft

        39 ->
            MoveRight

        _ ->
            NoOp
