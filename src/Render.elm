module Render exposing (view)

import Angle
import Color exposing (Color)
import Figure exposing (..)
import Figures exposing (..)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Length exposing (inMeters, meters)
import LineSegment2d
import Palette exposing (..)
import Pixels exposing (inPixels, pixels)
import Point2d
import Quantity exposing (at, divideBy, minus, multiplyBy, per, plus)
import TypedSvg exposing (circle, g, polygon, polyline, svg)
import TypedSvg.Attributes exposing (cx, cy, fill, noFill, points, r, stroke, strokeWidth, transform, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..), px)
import Vector2d



-- @remind remove Surface in favor of Figure?


type Surface
    = Surface (List Coord)


view model =
    let
        line1 =
            LineSegment2d.from (Point2d.meters -40 -20) (Point2d.meters 10 100)

        line2 =
            LineSegment2d.from (Point2d.meters -50 20) (Point2d.meters 100 100)

        maybeCollPoint =
            case LineSegment2d.intersectionPoint line1 line2 of
                Just point ->
                    [ viewPoint point ]

                Nothing ->
                    []

        ship =
            model.shipState
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
            ([ viewTopLeft
             , viewTopRight
             , viewBottomRight
             , viewBottomLeft
             , viewStar (Vector2d.meters -20 50)
             , viewFigure model.moon ship.centre ship.rotation
             , viewFigure shipFigure ship.centre ship.rotation
             , viewLine line1
             , viewLine line2
             ]
                ++ maybeCollPoint
            )
        ]


viewLine line =
    let
        ( p1, p2 ) =
            LineSegment2d.endpoints line

        fig =
            Figure (Point2d.meters 0 0) [ p1, p2 ] mountainColor
    in
    viewFigure fig (Vector2d.meters 0 0) (Angle.degrees 0)


viewStar : Offset -> Svg msg
viewStar offset =
    let
        triangle1 =
            viewFigure triangleFigure offset (Angle.degrees 0)

        triangle2 =
            viewFigure triangleFigure offset (Angle.degrees 180)
    in
    g []
        [ triangle1, triangle2 ]


viewFigure : Figure -> Offset -> Angle.Angle -> Svg msg
viewFigure fig offset rot =
    let
        (Figure anchor pts color) =
            transformFigure fig offset rot

        screenCoords : List ( Float, Float )
        screenCoords =
            List.map pointToScreen pts
    in
    polygon
        [ noFill
        , stroke <| Paint color
        , points screenCoords
        ]
        []


viewPoint : Coord -> Svg msg
viewPoint pos =
    let
        ( x, y ) =
            pointToScreen pos
    in
    circle
        [ cx (px x)
        , cy (px y)
        , r (px 5)
        , fill <| Paint starColor
        ]
        []


viewOffset : Offset -> Svg msg
viewOffset offset =
    let
        ( x, y ) =
            offsetToScreen offset
    in
    circle
        [ cx (px x)
        , cy (px y)
        , r (px 5)
        , fill <| Paint starColor
        ]
        []



-- @remind Screen should probably be 'SvgCoordinate' or smth


pointToScreen : Coord -> ( Float, Float )
pointToScreen point =
    let
        ( wx, wy ) =
            Point2d.coordinates point
    in
    ( wx |> plus (worldWidth |> divideBy 2) |> at pixelsPerMeter |> inPixels
    , worldHeight |> minus wy |> at pixelsPerMeter |> inPixels
    )


offsetToScreen : Offset -> ( Float, Float )
offsetToScreen offset =
    let
        ( wx, wy ) =
            Vector2d.components offset
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


viewTopLeft =
    viewPoint <| Point2d.xy left top


viewTopRight =
    viewPoint <| Point2d.xy right top


viewBottomRight =
    viewPoint <| Point2d.xy right bottom


viewBottomLeft =
    viewPoint <| Point2d.xy left bottom


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
