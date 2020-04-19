module Moonlander exposing (..)

import Browser
import Browser.Events
import Color
import Html exposing (Html)
import Json.Decode as Json
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import TypedSvg exposing (polygon, svg)
import TypedSvg.Attributes exposing (fill, points, stroke, strokeWidth, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), px)


appTitle =
    "Moonlander"


gravity =
    9.81


thrust =
    1000


rotationalThrust =
    3


friction =
    0


type alias Model =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    }


type Msg
    = Tick Float
    | HandleKeyboardEvent KeyboardEvent


initialModel () =
    ( { x = 100
      , y = -100
      , vx = 0
      , vy = 0
      , ax = 0
      , ay = 0
      , o = 0
      , ov = 0
      }
    , Cmd.none
    )


update msg model =
    case msg of
        Tick ms ->
            let
                dt =
                    ms / 1000
            in
            ( { model
                | x = model.x + model.vx * dt
                , y = model.y + model.vy * dt
                , vx = model.vx + model.ax * dt - friction * model.vx * dt
                , vy = model.vy + model.ay * dt - friction * model.vy * dt - gravity * dt
                , ax = 0
                , ay = 0
                , o = model.o + model.ov * dt
              }
            , Cmd.none
            )

        HandleKeyboardEvent event ->
            ( handleKeyDown event model, Cmd.none )


handleKeyDown event model =
    case event.key of
        Just " " ->
            { model
                | ax = thrust * cos model.o
                , ay = -thrust * sin model.o
            }

        Just "," ->
            { model
                | ov = -rotationalThrust
            }

        Just "." ->
            { model
                | ov = rotationalThrust
            }

        _ ->
            model


view model =
    { title = appTitle
    , body =
        [ svg [ viewBox 0 0 800 600 ] [ myCircle model.x model.y model.o 20 ]
        ]
    }


myCircle x y o r =
    let
        x0 =
            x + r * cos o

        y0 =
            -y + r * sin o

        x1 =
            x + r * cos (o + 3)

        y1 =
            -y + r * sin (o + 3)

        x2 =
            x + r * cos (o - 3)

        y2 =
            -y + r * sin (o - 3)
    in
    polygon
        [ points [ ( x0, y0 ), ( x1, y1 ), ( x2, y2 ) ]
        , fill <| Paint Color.blue
        , strokeWidth (px 2)
        , stroke <| Paint <| Color.rgba 0.8 0 0 0.5
        ]
        []



-- circle
--     [ cx (px (400 + 10 * x))
--     , cy (px (200 - 10 * y))
--     , r (px 30)
--     , fill <| Paint Color.blue
--     , strokeWidth (px 2)
--     , stroke <| Paint <| Color.rgba 0.8 0 0 0.5
--     ]
--     []


subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown (Json.map HandleKeyboardEvent decodeKeyboardEvent)
        ]


main =
    Browser.document
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
