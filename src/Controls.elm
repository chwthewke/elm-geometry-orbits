module Controls
    exposing
        ( Action(..)
        , UpdateField
        , Controls
        , init
        , update
        , scale
        , theta
        , phi
        , currentTime
        , animationSpeed
        , isAnimating
        , viewport
        )

import Maybe.Extra as Maybe
import Monocle.Lens exposing (Lens)
import Time.DateTime as DateTime exposing (DateTime, zero)


--

import Defs exposing (PredefLocation, toRadians)
import Viewport exposing (Viewport)


type alias ControlsData =
    { scale : Float
    , theta : Float
    , phi : Float
    , currentTime : DateTime
    , animationSpeed : Float
    , animatingFrom : Maybe DateTime
    }


type Controls
    = Controls ControlsData


controlsLens : (ControlsData -> a) -> (a -> ControlsData -> ControlsData) -> Lens Controls a
controlsLens get set =
    Lens
        (\(Controls d) -> get d)
        (\v (Controls d) -> Controls (set v d))


scale : Lens Controls Float
scale =
    controlsLens .scale (\v d -> { d | scale = v })


theta : Lens Controls Float
theta =
    controlsLens .theta (\v d -> { d | theta = v })


phi : Lens Controls Float
phi =
    controlsLens .phi (\v d -> { d | phi = v })


currentTime : Lens Controls DateTime
currentTime =
    controlsLens .currentTime (\v d -> { d | currentTime = v })


animationSpeed : Lens Controls Float
animationSpeed =
    controlsLens .animationSpeed (\v d -> { d | animationSpeed = v })


isAnimating : Controls -> Bool
isAnimating (Controls { animatingFrom }) =
    Maybe.isJust animatingFrom


animatingFrom : Lens Controls (Maybe DateTime)
animatingFrom =
    controlsLens .animatingFrom (\v d -> { d | animatingFrom = v })


init : Controls
init =
    Controls
        { scale = 200.0
        , theta = 0.0
        , phi = 90.0
        , currentTime = DateTime.dateTime { zero | year = 2000 }
        , animationSpeed = 10.0
        , animatingFrom = Nothing
        }


viewport : Controls -> Viewport
viewport (Controls c) =
    Viewport 800 800 c.scale (toRadians c.theta) (toRadians c.phi)


type alias UpdateField a =
    { lens: Lens Controls a
    , value : Maybe a
    }

type Action
    = Play
    | Pause
    | Stop
    | Goto PredefLocation
    | SetFloat (UpdateField Float)
    | SetDateTime (UpdateField DateTime)


update : Action -> Controls -> Controls
update action =
    case action of
        SetFloat up ->
            updateField up

        SetDateTime up ->
            updateField up

        Goto loc ->
            goto loc

        Play ->
            play

        Pause ->
            pause

        Stop ->
            stop

updateField : UpdateField a -> Controls -> Controls
updateField {lens, value} =
    Maybe.unwrap identity lens.set value

goto : PredefLocation -> Controls -> Controls
goto loc =
    scale.set loc.scale
        >> theta.set 0
        >> phi.set (pi / 2)


play : Controls -> Controls
play controls =
    if isAnimating controls then
        controls
    else
        controls
            |> (animatingFrom.set <| Just <| currentTime.get controls)


pause : Controls -> Controls
pause =
    animatingFrom.set Nothing


stop : Controls -> Controls
stop controls =
    Maybe.unwrap
        controls
        (\st -> controls |> animatingFrom.set Nothing |> currentTime.set st)
        (animatingFrom.get controls)
