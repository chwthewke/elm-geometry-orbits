module Controls.View exposing (view)

import Html as HH exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Maybe.Extra as Maybe
import Monocle.Lens exposing (Lens)
import Result exposing (Result(..))


--

import Defs exposing (toDegrees, toRadians)
import Controls exposing (Controls)


type Validation a
    = Valid a
    | Invalid String
    | Coerce String a


mapValidation : (a -> b) -> Validation a -> Validation b
mapValidation f v =
    case v of
        Valid a ->
            Valid (f a)

        Invalid err ->
            Invalid err

        Coerce err a ->
            Coerce err (f a)


update : Validation a -> Maybe a
update v =
    case v of
        Valid a ->
            Just a

        Invalid _ ->
            Nothing

        Coerce _ a ->
            Just a


toValidation : (a -> Maybe ( String, a )) -> Result String a -> Validation a
toValidation coerce res =
    case res of
        Ok a ->
            Maybe.unwrap (Valid a) (uncurry Coerce) (coerce a)

        Err e ->
            Invalid e


type alias Field a =
    { name : String
    , inputType : String
    , validate : String -> Validation a
    , show : a -> String
    , lens : Lens Controls a
    }


field : Field a -> (Controls.UpdateField a -> Controls.Action) -> Controls -> Html Controls.Action
field { name, inputType, validate, show, lens } cb controls =
    let
        fieldId : String
        fieldId =
            "input_" ++ (String.toLower name)

        onInput : String -> Controls.UpdateField a
        onInput value =
            Controls.UpdateField lens <|
                update <|
                    validate <|
                        Debug.log ("value@" ++ fieldId ++ " ") value
    in
        HH.div []
            [ HH.label
                [ HA.for fieldId ]
                [ HH.text <| name ++ ":" ]
            , HH.input
                [ HA.type_ inputType
                , HA.value <| show <| lens.get controls
                , HE.onInput onInput
                ]
                []
            ]
            |> HH.map cb


clamp : ( Float, Float ) -> Float -> Maybe ( String, Float )
clamp ( minV, maxV ) v =
    if v < minV || v > maxV then
        Just
            ( "must be between " ++ toString minV ++ " and " ++ toString maxV
            , min maxV (max v minV)
            )
    else
        Nothing


validateDegrees : ( Float, Float ) -> String -> Validation Float
validateDegrees bounds =
    String.toFloat
        >> toValidation (clamp bounds)
        >> mapValidation toRadians


fieldScale : Field Float
fieldScale =
    let
        validateScale : String -> Validation Float
        validateScale =
            String.toFloat
                >> toValidation (clamp ( 10, 1500 ))
    in
        Field "Scale" "number" validateScale toString Controls.scale


fieldTheta : Field Float
fieldTheta =
    Field "Theta" "number" (validateDegrees ( -180, 180 )) (toString << toDegrees) Controls.theta


fieldPhi : Field Float
fieldPhi =
    Field "Phi" "number" (validateDegrees ( -90, 90 )) (toString << toDegrees) Controls.phi


view : Controls -> Html Controls.Action
view controls =
    HH.div
        []
        [ field fieldScale Controls.SetFloat controls
        , field fieldTheta Controls.SetFloat controls
        , field fieldPhi Controls.SetFloat controls
        ]



-- field : String -> Lens Controls a -> Html Controls.Action
-- field name lens =
--     HH.input
--         [ HA.type_ ]
