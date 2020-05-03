module Moonlander exposing (main)

import Browser
import Color
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Length exposing (inMeters, meters)
import Palette exposing (shipColor, spaceColor)
import Pixels exposing (inPixels, pixels)
import Quantity exposing (at, divideBy, minus, multiplyBy, per, plus)
import TypedSvg exposing (circle, g, polyline, svg)
import TypedSvg.Attributes exposing (cx, cy, fill, noFill, points, r, stroke, strokeWidth, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), px)


screenWidth =
    pixels 800


screenHeight =
    pixels 400


worldWidth =
    meters 200


worldHeight =
    meters 100


pixelsPerMeter =
    screenWidth |> per worldWidth


type alias Model =
    { count : Int }


initialModel : Model
initialModel =
    { count = 0 }


type Msg
    = Increment
    | Decrement


update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }


view model =
    div
        [ style "background-color"
            (Color.toCssString spaceColor)
        , style "width" "75vw"
        , style "margin" "auto"
        , style "border-style" "dashed"
        , style "border-width" "5"
        ]
        [ svg [ viewBox 0 0 (inPixels screenWidth) (inPixels screenHeight) ]
            [ topLeft
            , topRight
            , bottomRight
            , bottomLeft
            , ship (meters 0) (meters 10)
            ]
        ]


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


ship wx wy =
    let
        bottom2anchor =
            meters 2

        top2anchor =
            meters 8

        shipHalfWidth =
            meters 5

        ( x0, y0 ) =
            worldToScreen wx (wy |> plus top2anchor)

        ( x1, y1 ) =
            worldToScreen (wx |> plus shipHalfWidth) (wy |> minus bottom2anchor)

        ( x2, y2 ) =
            worldToScreen (wx |> minus shipHalfWidth) (wy |> minus bottom2anchor)
    in
    g []
        [ polyline
            [ noFill
            , stroke <| Paint shipColor
            , points [ ( x0, y0 ), ( x1, y1 ), ( x2, y2 ), ( x0, y0 ) ]
            ]
            []
        , dot wx wy
        ]


dot wx wy =
    let
        ( x, y ) =
            worldToScreen wx wy
    in
    circle
        [ cx (px x)
        , cy (px y)
        , r (px 5)
        , fill <| Paint shipColor
        ]
        []


worldToScreen wx wy =
    ( wx |> plus (worldWidth |> divideBy 2) |> at pixelsPerMeter |> inPixels
    , worldHeight |> minus wy |> at pixelsPerMeter |> inPixels
    )


left =
    meters 0 |> minus worldWidth |> divideBy 2


right =
    meters 0 |> plus worldWidth |> divideBy 2


top =
    worldHeight


bottom =
    meters 0


topLeft =
    dot left top


topRight =
    dot right top


bottomRight =
    dot right bottom


bottomLeft =
    dot left bottom
