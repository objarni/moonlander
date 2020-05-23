module Figures exposing (..)

import Color exposing (Color)
import Figure exposing (..)
import Length exposing (inMeters, meters)
import Palette exposing (..)
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


triangleFigure : Figure
triangleFigure =
    let
        radius =
            1.5

        thirdOfCircle =
            2 * pi / 3

        nintyDegreesInRadians =
            pi / 2

        p1angle =
            nintyDegreesInRadians

        p2angle =
            nintyDegreesInRadians + thirdOfCircle

        p3angle =
            nintyDegreesInRadians + thirdOfCircle * 2

        p1 =
            Point2d.meters
                (radius * cos p1angle)
                (radius * sin p1angle)

        p2 =
            Point2d.meters
                (radius * cos p2angle)
                (radius * sin p2angle)

        p3 =
            Point2d.meters
                (radius * cos p3angle)
                (radius * sin p3angle)

        anchor =
            Point2d.meters 0 0
    in
    Figure anchor [ p1, p2, p3 ] starColor
