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
        { centre = position 0 10
        , rotation = Angle.degrees 10
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
                    [ dot point ]

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
             , viewShip model.shipState
             , viewStar (Point2d.meters 0 50)
             , mountain model.surface
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
    mountain s


viewFigure : Figure -> Offset -> Angle.Angle -> Svg msg
viewFigure (Figure anchor pts color) offset rot =
    let
        transformPt pt =
            worldToScreen <| Point2d.rotateAround anchor rot pt

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


viewShip ship =
    let
        ( wx, wy ) =
            Point2d.coordinates ship.centre

        bottom2anchor =
            meters 2

        top2anchor =
            meters 8

        shipHalfWidth =
            meters 5

        rotate pt =
            Point2d.rotateAround ship.centre ship.rotation pt

        rotation =
            ship.rotation

        ( x0, y0 ) =
            worldToScreen <| rotate (Point2d.xy wx (wy |> plus top2anchor))

        ( x1, y1 ) =
            worldToScreen <| rotate (Point2d.xy (wx |> plus shipHalfWidth) (wy |> minus bottom2anchor))

        ( x2, y2 ) =
            worldToScreen <| rotate (Point2d.xy (wx |> minus shipHalfWidth) (wy |> minus bottom2anchor))

        rotate pt =
            Point2d.rotateAround ship.centerOfGravity rotation pt

        coords =
            [ ( x0, y0 ), ( x1, y1 ), ( x2, y2 ) ]

        color =
            shipColor
    in
    g []
        [ polygon
            [ noFill
            , stroke <| Paint color
            , points coords
            ]
            []
        , dot ship.centre
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



-- @remind rename view functions to viewX


viewStar : Point2d.Point2d Length.Meters YUpCoordinates -> Svg msg
viewStar pos =
    let
        ( px1, py1 ) =
            worldToScreen pos

        ( ox, oy ) =
            worldToScreen Point2d.origin

        ( px, py ) =
            ( px1 - ox, py1 - oy )

        translateAmount =
            Vector2d.from Point2d.origin pos

        translate =
            Point2d.translateBy translateAmount

        screenCoords : List ( Float, Float )
        screenCoords =
            List.map worldToScreen
                [ Point2d.meters 0 2.5
                , Point2d.meters 2.5 -2.5
                , Point2d.meters -2.5 -2.5
                ]

        upSideDown =
            transform [ Translate px py, Rotate 180 ox oy ]

        position =
            transform [ Translate px py ]
    in
    g []
        [ polygon
            [ noFill
            , stroke <| Paint starColor
            , points screenCoords
            , position
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


dot pos =
    let
        ( x, y ) =
            worldToScreen pos
    in
    circle
        [ cx (px x)
        , cy (px y)
        , r (px 5)
        , fill <| Paint starColor
        ]
        []


worldToScreen : Pt -> ( Float, Float )
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



-- Polygon Figures


position x y =
    Vector2d.meters x y


type alias Offset =
    Vector2d.Vector2d Length.Meters YUpCoordinates


type alias Pt =
    Point2d.Point2d Length.Meters YUpCoordinates


type Figure
    = Figure Pt (List Pt) Color


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
