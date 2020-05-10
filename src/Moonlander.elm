module Moonlander exposing (main)

import Browser
import Color
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Length exposing (inMeters, meters)
import LineSegment2d
import List
import Palette exposing (..)
import Pixels exposing (inPixels, pixels)
import Point2d
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


type YUpCoordinates
    = YUpCoordinates


type Surface
    = Surface (List (Point2d.Point2d Length.Meters YUpCoordinates))


type alias Model =
    { count : Int
    , surface : Surface
    }


initialModel : Model
initialModel =
    { count = 0
    , surface =
        Surface
            [ Point2d.meters -100 0
            , Point2d.meters -50 10
            , Point2d.meters 0 0
            , Point2d.meters 10 10
            , Point2d.meters 200 0
            ]
    }


update msg model =
    model


view model =
    let
        line1 =
            LineSegment2d.from (Point2d.meters 0 20) (Point2d.meters 10 100)

        line2 =
            LineSegment2d.from (Point2d.meters -50 20) (Point2d.meters 100 100)

        maybeCollPoint =
            case LineSegment2d.intersectionPoint line1 line2 of
                Just point ->
                    [ dot point ]

                Nothing ->
                    []
    in
    div
        [ style "background-color"
            (Color.toCssString spaceColor)
        , style "width" "75vw"
        , style "margin" "auto"
        , style "border-style" "dashed"
        , style "border-width" "5"
        ]
        [ svg [ viewBox 0 0 (inPixels screenWidth) (inPixels screenHeight) ]
            ([ topLeft
             , topRight
             , bottomRight
             , bottomLeft
             , ship (Point2d.meters 0 10)
             , mountain model.surface
             , line line1
             , line line2
             ]
                ++ maybeCollPoint
            )
        ]


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


line l =
    let
        ( p1, p2 ) =
            LineSegment2d.endpoints l

        s =
            Surface [ p1, p2 ]
    in
    mountain s


ship pos =
    let
        ( wx, wy ) =
            Point2d.coordinates pos

        bottom2anchor =
            meters 2

        top2anchor =
            meters 8

        shipHalfWidth =
            meters 5

        ( x0, y0 ) =
            worldToScreen (Point2d.xy wx (wy |> plus top2anchor))

        ( x1, y1 ) =
            worldToScreen (Point2d.xy (wx |> plus shipHalfWidth) (wy |> minus bottom2anchor))

        ( x2, y2 ) =
            worldToScreen (Point2d.xy (wx |> minus shipHalfWidth) (wy |> minus bottom2anchor))
    in
    g []
        [ polyline
            [ noFill
            , stroke <| Paint shipColor
            , points [ ( x0, y0 ), ( x1, y1 ), ( x2, y2 ), ( x0, y0 ) ]
            ]
            []
        , dot pos
        ]


mountain (Surface worldCoords) =
    let
        screenCoords : List ( Float, Float )
        screenCoords =
            List.map worldToScreen worldCoords
    in
    polyline
        [ noFill
        , stroke <| Paint mountainColor
        , points screenCoords
        ]
        []


dot pos =
    let
        ( x, y ) =
            worldToScreen pos
    in
    circle
        [ cx (px x)
        , cy (px y)
        , r (px 5)
        , fill <| Paint shipColor
        ]
        []


worldToScreen : Point2d.Point2d Length.Meters YUpCoordinates -> ( Float, Float )
worldToScreen point =
    let
        ( wx, wy ) =
            Point2d.coordinates point
    in
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
    dot <| Point2d.xy left top


topRight =
    dot <| Point2d.xy right top


bottomRight =
    dot <| Point2d.xy right bottom


bottomLeft =
    dot <| Point2d.xy left bottom
