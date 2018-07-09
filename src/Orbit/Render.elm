module Orbit.Render exposing (RenderElement(..), RenderParams, renderOrbit)

import Circle2d
import CubicSpline3d exposing (CubicSpline3d)
import LineSegment3d exposing (LineSegment3d)
import Point3d exposing (Point3d)
import Time.DateTime exposing (DateTime)


--

import Svg exposing (Svg)
import Svg.Attributes as SA
import Geometry.Svg as Svg


--

import Orbit exposing (Orbit)
import Orbit.Geometry as Orbit
import Viewport exposing (Viewport)


type RenderElement
    = Splines
    | PositionAt DateTime
    | Axes
    | Apsides
    | Center
    | NearFocus
    | FarFocus


type alias ViewParams a =
    { a
        | mainColor : String
        , outlineColor : String
        , viewport : Viewport
    }


type alias RenderParams =
    ViewParams { elements : List RenderElement }


renderOrbit : RenderParams -> Orbit -> List (Svg msg)
renderOrbit params orbit =
    params.elements |> List.concatMap (renderElement params orbit)


renderElement : ViewParams a -> Orbit -> RenderElement -> List (Svg msg)
renderElement params orbit element =
    case element of
        Splines ->
            renderSplines params (Orbit.splines orbit)

        PositionAt dateTime ->
            renderPosition params (Orbit.positionAt orbit dateTime)

        Axes ->
            renderAxis params "12 4" (Orbit.semiMajorAxisSegment orbit)
                ++ renderAxis params "8 8" (Orbit.semiMinorAxisSegment orbit)

        _ ->
            []


renderSplines : ViewParams a -> List CubicSpline3d -> List (Svg msg)
renderSplines viewParams =
    let
        renderSpline =
            Svg.cubicSpline2d
                [ SA.stroke viewParams.mainColor
                , SA.strokeWidth "2"
                , SA.fill "none"
                ]
                << Viewport.viewSpline viewParams.viewport
    in
        List.map renderSpline


renderPosition : ViewParams a -> Point3d -> List (Svg msg)
renderPosition viewParams =
    List.singleton
        << Svg.circle2d
            [ SA.stroke viewParams.outlineColor
            , SA.strokeWidth "2"
            , SA.fill viewParams.mainColor
            ]
        << Circle2d.withRadius 5
        << Viewport.viewPoint viewParams.viewport


renderAxis : ViewParams a -> String -> LineSegment3d -> List (Svg msg)
renderAxis viewParams dasharray =
    List.singleton
        << Svg.lineSegment2d
            [ SA.stroke viewParams.mainColor
            , SA.strokeWidth "2"
            , SA.strokeLinecap "round"
            , SA.strokeDasharray dasharray
            ]
        << Viewport.viewLineSegment viewParams.viewport
