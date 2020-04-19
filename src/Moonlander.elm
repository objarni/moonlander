module Moonlander exposing (Model, Msg(..), appTitle, friction, gravity, handleKeyDown, initialModel, landerSvg, landerWidth, main, rotationalThrust, subscriptions, thrust, update, view)

import Browser
import Browser.Events
import Color
import Html exposing (Html)
import Json.Decode as Json
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Length
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


landerWidth =
    Length.meters 10


landerAnchorHeight =
    Length.meters 2


landerAnchorToTopLength =
    Length.meters 10


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
        [ Html.text (String.fromFloat model.x)
        , Html.text (String.fromFloat model.y)
        , svg [ viewBox 0 0 800 600 ] [ landerSvg model.x model.y model.o 20 ]
        ]
    }


toPixels : Length.Length -> Float
toPixels l =
    2 * Length.inMeters l


landerSvg x y o r =
    let
        halfLanderWidthPixels =
            toPixels landerWidth / 2

        minX =
            x - halfLanderWidthPixels

        maxX =
            x + halfLanderWidthPixels

        minY =
            y - toPixels landerAnchorHeight

        maxY =
            y + toPixels landerAnchorToTopLength
    in
    polygon
        [ points [ ( minX, minY ), ( maxX, minY ), ( x, maxY ) ]
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
