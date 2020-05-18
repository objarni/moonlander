module Moonlander exposing (main)

import Angle
import Browser
import Color exposing (Color)
import Figure exposing (..)
import Figures exposing (..)
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
import Render exposing (Offset, Surface(..), view)
import TypedSvg exposing (circle, g, polygon, polyline, svg)
import TypedSvg.Attributes exposing (cx, cy, fill, noFill, points, r, stroke, strokeWidth, transform, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..), px)
import Vector2d


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
        { centre = Vector2d.meters 0 0
        , rotation = Angle.degrees 0
        , leftBooster = True
        , rightBooster = False
        }
    }


update msg model =
    model



-- @remind fix zoom problem


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
