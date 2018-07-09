module Defs exposing (CelestialBody, mercury, venus, earth, mars)

import Orbit exposing (Orbit)


type alias CelestialBody =
    { name : String
    , orbit : Orbit
    , color : String
    }


toRadians : Float -> Float
toRadians degrees =
    degrees / 180.0 * pi


mercury : CelestialBody
mercury =
    { name = "Mercury"
    , orbit =
        Orbit.fromData
            { sma = 0.387098
            , ecc = 0.20563
            , inc = toRadians 7.005
            , lan = toRadians 48.331
            , argp = toRadians 29.124
            , ma0 = toRadians 174.796
            }
    , color = "lightslategray"
    }


venus : CelestialBody
venus =
    { name = "Venus"
    , orbit =
        Orbit.fromData
            { sma = 0.723332
            , ecc = 0.006772
            , inc = toRadians 3.39458
            , lan = toRadians 76.68
            , argp = toRadians 54.884
            , ma0 = toRadians 50.115
            }
    , color = "peru"
    }


earth : CelestialBody
earth =
    { name = "Earth"
    , orbit =
        Orbit.fromData
            { sma = 1
            , ecc = 0.0167086
            , inc = toRadians 0
            , lan = toRadians -11.26064
            , argp = toRadians 114.20783
            , ma0 = toRadians 358.617
            }
    , color = "royalblue"
    }


mars : CelestialBody
mars =
    { name = "Mars"
    , orbit =
        Orbit.fromData
            { sma = 1.523679
            , ecc = 0.0934
            , inc = toRadians 1.85
            , lan = toRadians 49.558
            , argp = toRadians 286.502
            , ma0 = toRadians 320.45776
            }
    , color = "indianred"
    }
