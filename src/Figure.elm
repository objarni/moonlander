module Figure exposing (..)

import Color exposing (Color)
import Length
import Point2d


type YUpCoordinates
    = YUpCoordinates Never


type alias Coord =
    Point2d.Point2d Length.Meters YUpCoordinates


type Figure
    = Figure Coord (List Coord) Color
