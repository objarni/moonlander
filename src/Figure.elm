module Figure exposing (..)

import Angle
import Color exposing (Color)
import Length
import Point2d
import Vector2d


type YUpCoordinates
    = YUpCoordinates Never


type alias Coord =
    Point2d.Point2d Length.Meters YUpCoordinates


type alias Offset =
    Vector2d.Vector2d Length.Meters YUpCoordinates


type Figure
    = Figure Coord (List Coord) Color


transformFigure : Figure -> Offset -> Angle.Angle -> Figure
transformFigure (Figure anchor coords color) offset angle =
    let
        transformPt pt =
            Point2d.rotateAround anchor angle pt
                |> Point2d.translateBy offset

        newCoords : List Coord
        newCoords =
            List.map transformPt coords
    in
    Figure anchor newCoords color


rotateFigure : Figure -> Angle.Angle -> Figure
rotateFigure fig angle =
    transformFigure fig (Vector2d.meters 0 0) angle
