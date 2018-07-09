module Main exposing (..)

import Time.DateTime as DateTime exposing (DateTime)
import Circle2d
import Html exposing (Html)
import Point3d
import Svg exposing (Svg)
import Svg.Attributes as SA
import Geometry.Svg as Svg
import Time.DateTime as DateTime exposing (DateTime)
import Time exposing (millisecond)


--

import Defs exposing (..)
import Viewport exposing (Viewport)
import Orbit.Render as Render exposing (RenderElement(..), RenderParams)


type alias State =
    { time : DateTime
    , delta : Int
    }


type Msg
    = Tick


initDateTime : DateTime
initDateTime =
    DateTime.dateTime
        { year = 2000
        , month = 7
        , day = 9
        , hour = 0
        , minute = 14
        , second = 0
        , millisecond = 0
        }


main : Program Never State Msg
main =
    Html.program
        { init = { time = initDateTime, delta = 0 } ! []
        , view = view
        , update = update
        , subscriptions = always subTime
        }


subTime : Sub Msg
subTime =
    Time.every (100 * millisecond) (always Tick)


update : Msg -> State -> ( State, Cmd Msg )
update msg st =
    let
        newTime =
            st.time |> DateTime.addDays 1
    in
        { st | time = newTime, delta = (DateTime.delta newTime initDateTime).seconds } ! []


viewport : Viewport
viewport =
    Viewport 800 800 200.0 0 (pi / 2)


renderParams : State -> CelestialBody -> RenderParams
renderParams state body =
    { mainColor = body.color
    , outlineColor = "white"
    , viewport = viewport
    , elements =
        [ Splines
        , PositionAt state.time
        ]
    }


view : State -> Html msg
view st =
    let
        bodies =
            [ mercury, venus, earth, mars ]

        renderBody body =
            Render.renderOrbit (renderParams st body) body.orbit

        bodiesSvg =
            bodies |> List.concatMap renderBody

        vw =
            toString viewport.width

        vh =
            toString viewport.height

        vb =
            "0 0 " ++ vw ++ " " ++ vh
    in
        Svg.svg
            [ SA.width vw, SA.height vh, SA.viewBox vb ]
        <|
            Svg.rect [ SA.x "0", SA.y "0", SA.width vw, SA.height vh, SA.fill "black" ] []
                :: renderSun viewport
                :: bodiesSvg


renderSun : Viewport -> Svg msg
renderSun viewport =
    let
        c =
            Point3d.origin
                |> Viewport.viewPoint viewport
                |> Circle2d.withRadius 8
    in
        Svg.circle2d
            [ SA.stroke "white"
            , SA.strokeWidth "0"
            , SA.fill "yellow"
            ]
            c
