module Orbit.Local
    exposing
        ( localPositionAt
        , localCenter
        , localPeriapsis
        , localApoapsis
        , localSemiMajorAxisSegment
        , localSemiMinorAxisSegment
        , localSplines
        )

{-|

    Geometric properties in ther orbit-local frame, which:
    - has its origin at the focus closer to periapsis
    - has the x direction pointing from center to focus to periapsis
    - has the z direction pointing along the angular momentum vector

-}

import Axis3d
import CubicSpline3d exposing (CubicSpline3d)
import LineSegment3d exposing (LineSegment3d)
import Point3d exposing (Point3d)
import Vector3d exposing (Vector3d)
import Time.DateTime as DateTime exposing (DateTime)


--

import Orbit exposing (Orbit)


trueAnomalyToPositionVector : Orbit -> Float -> Vector3d
trueAnomalyToPositionVector orbit ta =
    let
        a =
            Orbit.semiMajorAxis orbit

        e =
            Orbit.eccentricity orbit

        r =
            a * (1 - e ^ 2) / (1 + e * cos ta)
    in
        Vector3d.fromComponents ( r * cos ta, r * sin ta, 0 )


localPositionAt : Orbit -> DateTime -> Point3d
localPositionAt orbit =
    Orbit.trueAnomalyAt orbit
        >> trueAnomalyToPositionVector orbit
        >> flip Point3d.translateBy Point3d.origin


localCenter : Orbit -> Point3d
localCenter orbit =
    Point3d.fromCoordinates ( -(Orbit.semiMajorAxis orbit * Orbit.eccentricity orbit), 0, 0 )


localPeriapsis : Orbit -> Point3d
localPeriapsis orbit =
    Point3d.fromCoordinates ( Orbit.semiMajorAxis orbit * (1 - Orbit.eccentricity orbit), 0, 0 )


localApoapsis : Orbit -> Point3d
localApoapsis orbit =
    Point3d.fromCoordinates ( Orbit.semiMajorAxis orbit * (-1 - Orbit.eccentricity orbit), 0, 0 )


localSemiMajorAxisSegment : Orbit -> LineSegment3d
localSemiMajorAxisSegment orbit =
    LineSegment3d.from
        (localApoapsis orbit)
        (localPeriapsis orbit)


localSemiMinorAxisSegment : Orbit -> LineSegment3d
localSemiMinorAxisSegment orbit =
    let
        b =
            Orbit.semiMinorAxis orbit
    in
        LineSegment3d.along Axis3d.y (-b) b
            |> LineSegment3d.translateBy (Vector3d.from Point3d.origin (localCenter orbit))


localSplines : Orbit -> List CubicSpline3d
localSplines orbit =
    let
        magic : Float
        magic =
            0.551784

        a =
            Orbit.semiMajorAxis orbit

        b =
            Orbit.semiMinorAxis orbit

        pointFromCenter ( x, y ) =
            Point3d.fromCoordinates ( x, y, 0 )
                |> Point3d.translateBy (Vector3d.from Point3d.origin (localCenter orbit))

        spline sx sy =
            CubicSpline3d.with
                { startPoint = pointFromCenter ( sx * a, 0 )
                , startControlPoint = pointFromCenter ( sx * a, sy * magic * b )
                , endControlPoint = pointFromCenter ( sx * magic * a, sy * b )
                , endPoint = pointFromCenter ( 0, sy * b )
                }
    in
        [ spline 1 1, spline (-1) 1, spline (-1) (-1), spline 1 (-1) ]
