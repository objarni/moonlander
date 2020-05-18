module Moonlander exposing (main)

import Angle
import Browser
import Color exposing (Color)
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
import TypedSvg exposing (circle, g, polygon, polyline, svg)
import TypedSvg.Attributes exposing (cx, cy, fill, noFill, points, r, stroke, strokeWidth, transform, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..), px)
import Vector2d


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
    = YUpCoordinates Never


type Surface
    = Surface (List Pt)


type alias Model =
    { surface : Surface
    , shipState : ShipState
    }


type alias ShipState =
    { centre : Offset
    , rotation : Angle.Angle
    , leftBooster : Bool
    , rightBooster : Bool
    }


initialModel : Model
initialModel =
    { surface =
        Surface
            [ Point2d.meters -100 0
            , Point2d.meters -50 10
            , Point2d.meters 0 0
            , Point2d.meters 10 10
            , Point2d.meters 200 0
            ]
    , shipState =
        { centre = position 0 0
        , rotation = Angle.degrees 0
        , leftBooster = True
        , rightBooster = False
        }
    }


update msg model =
    model



-- @remind fix zoom problem


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
            ([ topLeft
             , topRight
             , bottomRight
             , bottomLeft
             , viewStar (Point2d.meters 0 50)
             , viewSurface model.surface
             , line line1
             , line line2
             , viewFigure shipFigure ship.centre ship.rotation
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
    viewSurface s


viewFigure : Figure -> Offset -> Angle.Angle -> Svg msg
viewFigure (Figure anchor pts color) offset rot =
    let
        transformPt pt =
            Point2d.rotateAround anchor rot pt
                |> Point2d.translateBy offset
                |> pointToScreen

        screenCoords : List ( Float, Float )
        screenCoords =
            List.map transformPt pts
    in
    polygon
        [ noFill
        , stroke <| Paint color
        , points screenCoords
        ]
        []


viewSurface (Surface worldCoords) =
    let
        screenCoords : List ( Float, Float )
        screenCoords =
            List.map pointToScreen worldCoords
    in
    polyline
        [ noFill
        , stroke <| Paint mountainColor
        , points screenCoords
        ]
        []


viewStar : Point2d.Point2d Length.Meters YUpCoordinates -> Svg msg
viewStar pos =
    let
        ( px1, py1 ) =
            pointToScreen pos

        ( ox, oy ) =
            pointToScreen Point2d.origin

        ( px, py ) =
            ( px1 - ox, py1 - oy )

        translateAmount =
            Vector2d.from Point2d.origin pos

        translate =
            Point2d.translateBy translateAmount

        screenCoords : List ( Float, Float )
        screenCoords =
            List.map pointToScreen
                [ Point2d.meters 0 2.5
                , Point2d.meters 2.5 -2.5
                , Point2d.meters -2.5 -2.5
                ]

        upSideDown =
            transform [ Translate px py, Rotate 180 ox oy ]

        position1 =
            transform [ Translate px py ]
    in
    g []
        [ polygon
            [ noFill
            , stroke <| Paint starColor
            , points screenCoords
            , position1
            ]
            []
        , polygon
            [ noFill
            , stroke <| Paint starColor
            , points screenCoords
            , upSideDown
            ]
            []
        ]


viewPoint : Pt -> Svg msg
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


pointToScreen : Pt -> ( Float, Float )
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


topLeft =
    viewPoint <| Point2d.xy left top


topRight =
    viewPoint <| Point2d.xy right top


bottomRight =
    viewPoint <| Point2d.xy right bottom


bottomLeft =
    viewPoint <| Point2d.xy left bottom



-- Polygon Figures


position : Float -> Float -> Offset
position x y =
    Vector2d.meters x y


type alias Offset =
    Vector2d.Vector2d Length.Meters YUpCoordinates


type alias Pt =
    Point2d.Point2d Length.Meters YUpCoordinates


type Figure
    = Figure Pt (List Pt) Color


shipFigure : Figure
shipFigure =
    let
        p1 =
            Point2d.meters 0 10

        p2 =
            Point2d.meters 5 0

        p3 =
            Point2d.meters -5 0

        points =
            [ p1, p2, p3 ]

        anchor =
            Point2d.meters 0 2
    in
    Figure anchor points shipColor
