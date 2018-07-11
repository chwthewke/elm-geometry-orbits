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

import Controls exposing (Controls, Action(..), animationSpeed, currentTime)
import Controls.View as Controls
import Defs exposing (..)
import Viewport exposing (Viewport)
import Orbit.Render as Render exposing (RenderElement(..), RenderParams)


type alias State =
    Controls


type alias Msg =
    Controls.Action



--


subscriptions : State -> Sub Msg
subscriptions controls =
    let
        updateTime : State -> Time -> Msg
        updateTime st time =
            SetDateTime
                (Controls.UpdateField
                    currentTime
                 <|
                    Just <|
                        (DateTime.addSeconds
                            (floor (Time.inMilliseconds time * animationSpeed.get st * 86.4))
                            (currentTime.get st)
                        )
                )
    in
        if Controls.isAnimating controls then
            AnimationFrame.diffs (updateTime controls)
        else
            Sub.none


main : Program Never State Msg
main =
    Html.program
        { init = Controls.init ! []
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> State -> ( State, Cmd Msg )
update msg st =
    Controls.update msg st ! []


renderParams : State -> CelestialBody -> RenderParams
renderParams state body =
    { mainColor = body.color
    , outlineColor = "white"
    , viewport = Controls.viewport state
    , elements =
        [ Splines
        , PositionAt <| currentTime.get state
        ]
    }


view : State -> Html Msg
view st =
    Html.div []
        [ Controls.view st
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
            toString (Controls.viewport st).width

        vh =
            toString (Controls.viewport st).height

        vb =
            "0 0 " ++ vw ++ " " ++ vh
    in
        Svg.svg
            [ SA.width vw, SA.height vh, SA.viewBox vb ]
        <|
            Svg.rect [ SA.x "0", SA.y "0", SA.width vw, SA.height vh, SA.fill "black" ] []
                :: renderSun (Controls.viewport st)
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
