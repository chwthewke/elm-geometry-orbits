module Inputs exposing (..)

import Dict
import Html as HH exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as J
import Json.Encode as J exposing (Value)


type M
    = M (List Event)


initM =
    M []


type Event
    = Event String String


opts : HE.Options
opts =
    HE.Options True True

keys : J.Decoder String
keys = J.dict (J.succeed ()) |> J.map Dict.keys |> J.map showKeys

targetKeys =
    J.field "target" keys |> J.map (\s -> "target " ++ s)

targetValueV =
    J.at ["target", "value"] J.value |> J.map (J.encode 0)

targetValueAsNumberV =
    J.at ["target", "valueAsNumber"] J.value |> J.map (J.encode 0)

showKeys : List String -> String
showKeys = String.join ", "

onEvent name eventType d =
    HE.onWithOptions eventType opts (d)
        |> HA.map (Event (name ++ " " ++ eventType))


view events =
    HH.div [] [ textInput, numberInput, eventLog events ]


textInput =
    HH.div []
        [ HH.input [ HA.type_ "text", onEvent "text" "input" targetValueV ] []
        ]


numberInput =
    HH.div []
        [ HH.input [ HA.type_ "number", onEvent "number" "input" targetValueAsNumberV ] []
        ]


eventLog (M events) =
    let
        eventItem (Event eventType value) =
            HH.li [] [ HH.text <| eventType ++ " -> " ++ value ]
    in
        HH.ul [] (List.map eventItem events)


update : Event -> M -> ( M, Cmd Event )
update ev (M evs) =
    M (ev :: evs) ! []


main : Program Never M Event
main =
    HH.program
        { init = initM ! []
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
