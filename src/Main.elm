module Main exposing (..)

import Direction3d
import Html exposing (Html)
import Svg
import Svg.Attributes as SA
--
import Defs exposing (..)
import Render

type alias Model = ()
type alias Msg = ()

main : Program Never Model Msg
main = Html.beginnerProgram { model = (), view = (\_ -> view), update = (\_ -> identity) }

view : Html msg
view =
    let
        viewport = Render.Viewport 800 600 200.0 Direction3d.z 
        bodies = [ mercury, venus, earth, mars ]
        renderBody body = Render.renderOrbit viewport body.orbit body.color
        bodiesSvg = bodies |> List.concatMap renderBody
        vw = toString viewport.width
        vh = toString viewport.height
        vb = "0 0 " ++ vw ++ " " ++ vh
    in
        Svg.svg
            [ SA.width vw, SA.height vh, SA.viewBox vb ] 
            <|  Svg.rect [ SA.x "0", SA.y "0", SA.width vw, SA.height vh, SA.fill "black" ] [] :: bodiesSvg
