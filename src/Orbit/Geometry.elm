module Orbit.Geometry exposing (positionAt, center, periapsis, apoapsis, semiMajorAxisSegment, semiMinorAxisSegment, splines)

import Axis3d
import CubicSpline3d exposing (CubicSpline3d)
import Frame3d exposing (Frame3d)
import LineSegment3d exposing (LineSegment3d)
import Point3d exposing (Point3d)
import Time.DateTime as DateTime exposing (DateTime)


--

import Orbit exposing (Orbit)
import Orbit.Local exposing (..)


orbitFrame : Orbit -> Frame3d
orbitFrame orbit =
    Frame3d.xyz
        |> Frame3d.rotateAround Axis3d.z (Orbit.argumentOfPeriapsis orbit)
        |> Frame3d.rotateAround Axis3d.x (Orbit.inclination orbit)
        |> Frame3d.rotateAround Axis3d.z (Orbit.longitudeOfAscendingNode orbit)


positionAt : Orbit -> DateTime -> Point3d
positionAt orbit time =
    localPositionAt orbit time
        |> Point3d.placeIn (orbitFrame orbit)


center : Orbit -> Point3d
center orbit =
    localCenter orbit
        |> Point3d.placeIn (orbitFrame orbit)


periapsis : Orbit -> Point3d
periapsis orbit =
    localPeriapsis orbit
        |> Point3d.placeIn (orbitFrame orbit)


apoapsis : Orbit -> Point3d
apoapsis orbit =
    localApoapsis orbit
        |> Point3d.placeIn (orbitFrame orbit)


semiMajorAxisSegment : Orbit -> LineSegment3d
semiMajorAxisSegment orbit =
    localSemiMajorAxisSegment orbit
        |> LineSegment3d.placeIn (orbitFrame orbit)


semiMinorAxisSegment : Orbit -> LineSegment3d
semiMinorAxisSegment orbit =
    localSemiMinorAxisSegment orbit
        |> LineSegment3d.placeIn (orbitFrame orbit)


splines : Orbit -> List CubicSpline3d
splines orbit =
    localSplines orbit
        |> List.map (CubicSpline3d.placeIn (orbitFrame orbit))
