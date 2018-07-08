module Orbit
    exposing
        ( Orbit
        , semiMajorAxis
        , eccentricity
        , inclination
        , longitudeOfAscendingNode
        , argumentOfPeriapsis
        , meanAnomalyAtEpoch
        , orbitSplines
        )

import Axis3d exposing (Axis3d)
import CubicSpline3d exposing (CubicSpline3d)
import Frame3d exposing (Frame3d)
import Point3d exposing (Point3d)
import Vector3d exposing (Vector3d)


type alias Orbit =
    { sma : Float
    , ecc : Float
    , inc : Float
    , lan : Float
    , argp : Float
    , ma0 : Float
    }


semiMajorAxis : Orbit -> Float
semiMajorAxis =
    .sma


semiMinorAxis : Orbit -> Float
semiMinorAxis orbit =
    orbit.sma * orbit.sma * (1 - orbit.ecc * orbit.ecc) |> sqrt


eccentricity : Orbit -> Float
eccentricity =
    .ecc


inclination : Orbit -> Float
inclination =
    .inc


longitudeOfAscendingNode : Orbit -> Float
longitudeOfAscendingNode =
    .lan


argumentOfPeriapsis : Orbit -> Float
argumentOfPeriapsis =
    .argp


meanAnomalyAtEpoch : Orbit -> Float
meanAnomalyAtEpoch =
    .ma0


orbitFrame : Orbit -> Frame3d
orbitFrame orbit =
    Frame3d.xyz
        |> Frame3d.translateBy (Vector3d.fromComponents ( orbit.sma * orbit.ecc, 0, 0 ))
        |> Frame3d.rotateAround Axis3d.z orbit.argp
        |> Frame3d.rotateAround Axis3d.x orbit.inc
        |> Frame3d.rotateAround Axis3d.z orbit.lan


{-|
    Vector from center to periapsis
-}
semiMajorAxisVector : Orbit -> Vector3d
semiMajorAxisVector orbit =
    Vector3d.fromComponents ( orbit.sma, 0, 0 )
        |> Vector3d.placeIn (orbitFrame orbit)


semiMinorAxisVector : Orbit -> Vector3d
semiMinorAxisVector orbit =
    Vector3d.fromComponents ( 0, semiMinorAxis orbit, 0 )
        |> Vector3d.placeIn (orbitFrame orbit)


{-|
    Vector from center to focus closer to periapsis
-}
focusVector : Orbit -> Vector3d
focusVector orbit =
    Vector3d.fromComponents ( orbit.sma * orbit.ecc, 0, 0 )
        |> Vector3d.placeIn (orbitFrame orbit)


orbitSplines : Orbit -> List CubicSpline3d
orbitSplines orbit =
    let
        magic : Float
        magic =
            0.551784

        a =
            orbit.sma

        b =
            semiMinorAxis orbit

        pointInOrbitFrame ( x, y ) =
            Point3d.fromCoordinatesIn (orbitFrame orbit) ( x, y, 0 )

        spline sx sy =
            CubicSpline3d.with
                { startPoint = pointInOrbitFrame ( sx * a, 0 )
                , startControlPoint = pointInOrbitFrame ( sx * a, sy * magic * b )
                , endControlPoint = pointInOrbitFrame ( sx * magic * a, sy * b )
                , endPoint = pointInOrbitFrame ( 0, sy * b )
                }
    in
        [ spline 1 1, spline (-1) 1, spline (-1) (-1), spline 1 (-1) ]
