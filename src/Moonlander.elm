module Moonlander exposing (main)

import Angle
import Browser
import Figure exposing (Figure(..))
import Palette exposing (mountainColor)
import Point2d
import Render exposing (Offset, Surface(..), view)
import Vector2d


type alias Model =
    { moon : Figure
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
    { moon =
        Figure (Point2d.meters 0 0)
            [ Point2d.meters -100 0
            , Point2d.meters -50 10
            , Point2d.meters 0 0
            , Point2d.meters 10 10
            , Point2d.meters 200 0
            ]
            mountainColor
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
