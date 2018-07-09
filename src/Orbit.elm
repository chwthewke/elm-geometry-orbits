module Orbit
    exposing
        ( OrbitData
        , Orbit
        , fromData
        , semiMajorAxis
        , semiMinorAxis
        , eccentricity
        , inclination
        , longitudeOfAscendingNode
        , argumentOfPeriapsis
        , meanAnomalyAtEpoch
        , orbitPeriod
        , trueAnomalyAt
        )

import Time.DateTime as DateTime exposing (DateTime)


type alias OrbitData =
    { sma : Float
    , ecc : Float
    , inc : Float
    , lan : Float
    , argp : Float
    , ma0 : Float
    }


type Orbit
    = Orbit OrbitData


orbitData : Orbit -> OrbitData
orbitData (Orbit data) =
    data


fromData : OrbitData -> Orbit
fromData =
    Orbit


semiMajorAxis : Orbit -> Float
semiMajorAxis =
    .sma << orbitData


semiMinorAxis : Orbit -> Float
semiMinorAxis orbit =
    let
        a =
            semiMajorAxis orbit

        e =
            eccentricity orbit
    in
        sqrt (a ^ 2 * (1 - e ^ 2))


eccentricity : Orbit -> Float
eccentricity =
    .ecc << orbitData


inclination : Orbit -> Float
inclination =
    .inc << orbitData


longitudeOfAscendingNode : Orbit -> Float
longitudeOfAscendingNode =
    .lan << orbitData


argumentOfPeriapsis : Orbit -> Float
argumentOfPeriapsis =
    .argp << orbitData


meanAnomalyAtEpoch : Orbit -> Float
meanAnomalyAtEpoch =
    .ma0 << orbitData


epochJ2000 : DateTime
epochJ2000 =
    DateTime.dateTime
        { year = 2000
        , month = 1
        , day = 1
        , hour = 11
        , minute = 58
        , second = 55
        , millisecond = 816
        }


{-|
    Solar mass parameter, in AU^3/s^2
-}
mu : Float
mu =
    3.964016055e-14


{-|
    Orbit period in seconds
-}
orbitPeriod : Orbit -> Float
orbitPeriod orbit =
    let
        a =
            semiMajorAxis orbit
    in
        2 * pi * sqrt (a ^ 3 / mu)


meanAnomalyAt : Orbit -> DateTime -> Float
meanAnomalyAt orbit t =
    let
        fmod : Float -> Float -> Float
        fmod x m =
            x - m * toFloat (floor (x / m))

        tau =
            2 * pi

        delta =
            (DateTime.delta t epochJ2000).seconds

        ma =
            meanAnomalyAtEpoch orbit + (toFloat delta / orbitPeriod orbit * tau)
    in
        fmod (fmod ma tau + tau) tau


meanToEccentricAnomaly : Orbit -> Float -> Float
meanToEccentricAnomaly orbit ma =
    let
        e =
            eccentricity orbit

        f ea =
            ea - e * sin ea - ma

        df ea =
            1 - e * cos ea

        dea ea =
            -(f ea) / df ea

        next ean =
            ean + dea ean

        loop ean =
            let
                ean1 =
                    next ean
            in
                if abs (ean1 - ean) < 0.0001 then
                    ean1
                else
                    loop ean1

        ea0 =
            if e > 0.8 then
                pi
            else
                ma
    in
        loop ea0


eccentricToTrueAnomaly : Orbit -> Float -> Float
eccentricToTrueAnomaly orbit ea =
    let
        e =
            eccentricity orbit
    in
        2 * atan2 (sqrt (1 + e) * sin (ea / 2)) (sqrt (1 - e) * cos (ea / 2))


trueAnomalyAt : Orbit -> DateTime -> Float
trueAnomalyAt orbit =
    meanAnomalyAt orbit
        >> meanToEccentricAnomaly orbit
        >> eccentricToTrueAnomaly orbit
