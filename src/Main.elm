module Main exposing (..)

import AnimationFrame
import Circle2d
import Html exposing (Html)
import Point3d
import Svg exposing (Svg)
import Svg.Attributes as SA
import Geometry.Svg as Svg
import Time exposing (Time)
import Time.DateTime as DateTime exposing (DateTime)
import Time.Iso8601 as TI


--

import Defs exposing (..)
import Viewport exposing (Viewport)
import Orbit.Render as Render exposing (RenderElement(..), RenderParams)


type alias State =
    { time : DateTime
    , timeFactor : Float
    }


type Msg
    = Tick Time


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
        { init = { time = initDateTime, timeFactor = 864 } ! []
        , view = view
        , update = update
        , subscriptions = always subTime
        }


subTime : Sub Msg
subTime =
    AnimationFrame.diffs Tick


update : Msg -> State -> ( State, Cmd Msg )
update (Tick dt) st =
    let
        scaledDeltaTime =
            st.timeFactor * (Time.inMilliseconds dt)
    in
        { st | time = st.time |> DateTime.addSeconds (floor scaledDeltaTime) } ! []


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
    Html.div []
        [ Html.div []
            [ Html.text <| TI.fromDate <| DateTime.date st.time ]
        , Html.div []
            [ svg st ]
        ]


svg : State -> Html msg
svg st =
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
