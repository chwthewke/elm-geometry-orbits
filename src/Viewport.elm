module Viewport
    exposing
        ( Viewport
        , viewSpline
        , viewPoint
        , viewLineSegment
        )

import Axis2d exposing (Axis2d)
import Axis3d
import CubicSpline2d exposing (CubicSpline2d)
import CubicSpline3d exposing (CubicSpline3d)
import Direction2d
import Frame3d exposing (Frame3d)
import LineSegment2d exposing (LineSegment2d)
import LineSegment3d exposing (LineSegment3d)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import SketchPlane3d exposing (SketchPlane3d)


{-|

    Viewport coordinates
    - width : screen-width in pixels
    - height: screen-height in pixels
    - scale : pixels per AU
    - theta, phi : spherical coordinates of eye direction

-}
type alias Viewport =
    { width : Int
    , height : Int
    , scale : Float
    , theta : Float
    , phi : Float
    }


viewportFrame : Viewport -> Frame3d
viewportFrame viewport =
    Frame3d.xyz
        |> Frame3d.rotateAround Axis3d.x (viewport.phi - pi / 2)
        |> Frame3d.rotateAround Axis3d.z viewport.theta
        |> Frame3d.translateAlongOwn Frame3d.xAxis (toFloat (-viewport.width) / 2)
        |> Frame3d.translateAlongOwn Frame3d.yAxis (toFloat (-viewport.height) / 2)


mirrorAxis : Viewport -> Axis2d
mirrorAxis viewport =
    Axis2d.through (Point2d.fromCoordinates ( 0, toFloat viewport.height / 2 )) Direction2d.x


type alias Project a3 a2 =
    { scaleAbout : Point3d -> Float -> a3 -> a3
    , relativeTo : Frame3d -> a3 -> a3
    , projectInto : SketchPlane3d -> a3 -> a2
    , mirrorAcross : Axis2d -> a2 -> a2
    }


project : Project a3 a2 -> Viewport -> a3 -> a2
project funcs viewport geom3d =
    geom3d
        |> funcs.scaleAbout Point3d.origin viewport.scale
        |> funcs.relativeTo (viewportFrame viewport)
        |> funcs.projectInto SketchPlane3d.xy
        |> funcs.mirrorAcross (mirrorAxis viewport)


viewSpline : Viewport -> CubicSpline3d -> CubicSpline2d
viewSpline =
    project
        (Project
            CubicSpline3d.scaleAbout
            CubicSpline3d.relativeTo
            CubicSpline3d.projectInto
            CubicSpline2d.mirrorAcross
        )


viewPoint : Viewport -> Point3d -> Point2d
viewPoint =
    project
        (Project
            Point3d.scaleAbout
            Point3d.relativeTo
            Point3d.projectInto
            Point2d.mirrorAcross
        )


viewLineSegment : Viewport -> LineSegment3d -> LineSegment2d
viewLineSegment =
    project
        (Project
            LineSegment3d.scaleAbout
            LineSegment3d.relativeTo
            LineSegment3d.projectInto
            LineSegment2d.mirrorAcross
        )
