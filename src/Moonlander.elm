module Moonlander exposing (main)

import Angle
import Browser
import Browser.Events as Events
import Duration
import Figure exposing (Figure(..), Offset)
import Length
import Palette exposing (mountainColor)
import Point2d
import Quantity
import Render exposing (view)
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


type Msg
    = Tick Duration.Duration


initialModel : () -> ( Model, Cmd a )
initialModel flags =
    ( { moon =
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
    , Cmd.none
    )


update msg model =
    case msg of
        Tick duration ->
            let
                oldShipState =
                    model.shipState

                ( x, y ) =
                    Vector2d.components oldShipState.centre

                newY =
                    Length.meters 1 |> Quantity.plus y

                newShipState =
                    { oldShipState
                        | centre = Vector2d.xy x newY
                    }
            in
            ( { model | shipState = newShipState }, Cmd.none )



-- @remind fix zoom problem


subscriptions model =
    Events.onAnimationFrameDelta
        (\deltaMs -> Tick (Duration.milliseconds deltaMs))


main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
