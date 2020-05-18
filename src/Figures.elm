module Figures exposing (..)

import Color exposing (Color)
import Figure exposing (..)
import Length exposing (inMeters, meters)
import Palette exposing (shipColor)
import Point2d
import Vector2d


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
