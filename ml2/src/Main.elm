module Main exposing (main)

import Browser
import Color
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Length exposing (meters)
import Pixels exposing (inPixels, pixels)
import Quantity exposing (at, divideBy, minus, multiplyBy, per, plus)
import TypedSvg exposing (circle, polyline, svg)
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


worldToScreen wx wy =
    ( wx |> plus (worldWidth |> divideBy 2) |> at pixelsPerMeter |> inPixels
    , worldHeight |> minus wy |> at pixelsPerMeter |> inPixels
    )


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
    div []
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
        ( x0, y0 ) =
            worldToScreen wx (wy |> plus (meters 5))

        ( x1, y1 ) =
            worldToScreen (wx |> plus (meters 5)) (wy |> minus (meters 5))

        ( x2, y2 ) =
            worldToScreen (wx |> minus (meters 5)) (wy |> minus (meters 5))
    in
    polyline
        [ noFill
        , stroke <| Paint Color.black
        , points [ ( x0, y0 ), ( x1, y1 ), ( x2, y2 ), ( x0, y0 ) ]
        ]
        []



--polyline [ fill FillNone, stroke Color.black, points [ ( 20, 100 ), ( 40, 60 ), ( 70, 80 ), ( 100, 20 ) ] ] []


dot x y =
    circle
        [ cx (px x)
        , cy (px y)
        , r (px 5)
        , fill <| Paint Color.black
        ]
        []


topLeft =
    dot 0 0


topRight =
    dot 800 0


bottomRight =
    dot 800 400


bottomLeft =
    dot 0 400
