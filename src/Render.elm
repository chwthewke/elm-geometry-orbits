module Render exposing (Viewport, renderOrbit)

import Axis2d exposing (Axis2d)
import CubicSpline2d exposing (CubicSpline2d)
import CubicSpline3d exposing (CubicSpline3d)
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import SketchPlane3d exposing (SketchPlane3d)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Geometry.Svg as Svg


--

import Orbit exposing (Orbit)


type alias Viewport =
    { width : Int
    , height : Int
    , scale :
        Float
        -- pixels per AU
    , lookDir : Direction3d
    }


viewportFrame : Int -> Int -> Direction3d -> Frame3d
viewportFrame width height lookDir =
    Frame3d.withZDirection lookDir Point3d.origin
        |> Frame3d.translateAlongOwn Frame3d.xAxis (toFloat (-width) / 2)
        |> Frame3d.translateAlongOwn Frame3d.yAxis (toFloat (-height) / 2)


viewSpline : Viewport -> CubicSpline3d -> CubicSpline2d
viewSpline viewport spline =
    spline
        |> CubicSpline3d.scaleAbout Point3d.origin viewport.scale
        |> CubicSpline3d.relativeTo (viewportFrame viewport.width viewport.height viewport.lookDir)
        |> CubicSpline3d.projectInto SketchPlane3d.xy


renderOrbit : Viewport -> Orbit -> String -> List (Svg msg)
renderOrbit viewport orbit color =
    let
        mirrorAxis = Axis2d.through (Point2d.fromCoordinates (0, toFloat viewport.height / 2)) Direction2d.x
        renderSpline =
            viewSpline viewport
                >> CubicSpline2d.mirrorAcross mirrorAxis
                >> Svg.cubicSpline2d
                    [ SA.stroke color
                    , SA.fill "none"
                    , SA.strokeWidth "2"
                    ]
    in
        Orbit.orbitSplines orbit
            |> List.map renderSpline
