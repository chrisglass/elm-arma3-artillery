module Main exposing (main)

import Basics exposing (atan, atan2, isNaN, pi, sqrt)
import Batteries exposing (BatteryProfile, batteries_map, firstValidRangeName, firstValidRangeVelocity, mk6_mortar)
import Dict exposing (get, keys)
import Html exposing (Html, button, div, fieldset, input, li, table, text, th, tr, ul, node, section, h1, h2, body, label, article, p, a)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import List exposing (map)
import Maybe exposing (withDefault)
import String exposing (toInt)
import Round exposing (round)


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


{-| Gravity in the arma3 game is not the real gravity constant (9.80665) but
instead is using the one displayed here.
-}
gravity : Float
gravity =
    9.81


{-| The smallest grid unit we use in the ballistics computation is a 10x10m
square.
-}
smallest_grid : Float
smallest_grid =
    10


type alias MapCoord =
    { x : Int
    , y : Int
    , z : Int
    }


type alias ArtilleryModel =
    { battery : MapCoord
    , target : MapCoord
    , selected_profile : BatteryProfile
    }


model : ArtilleryModel
model =
    { battery = MapCoord 0 0 0
    , target = MapCoord 0 0 0
    , selected_profile = mk6_mortar
    }



-- UPDATE


toDegrees : Float -> Float
toDegrees radians =
    radians * 180 / pi


{-| A helper that returns a tuple of the difference between two MapCoords.
-}
vectorDifference : MapCoord -> MapCoord -> ( Float, Float )
vectorDifference coord_battery coord_target =
    let
        delta_x =
            toFloat coord_target.x - toFloat coord_battery.x

        delta_y =
            toFloat coord_target.y - toFloat coord_battery.y
    in
        ( delta_x, delta_y )


bearing : ArtilleryModel -> Float
bearing model =
    let
        ( delta_x, delta_y ) =
            vectorDifference model.battery model.target
    in
        if delta_x == 0 then
            0
        else if delta_x <= 0 && delta_y >= 0 then
            450 - toDegrees (atan2 delta_y delta_x)
        else
            90 - toDegrees (atan2 delta_y delta_x)


distance : MapCoord -> MapCoord -> Float
distance battery_coord target_coord =
    let
        ( delta_x, delta_y ) =
            vectorDifference battery_coord target_coord
    in
        smallest_grid * sqrt ((delta_x ^ 2) + (delta_y ^ 2))


phi : (Float -> Float -> Float) -> ArtilleryModel -> Float
phi op model =
    let
        velocity =
            firstValidRangeVelocity model.selected_profile range

        range =
            distance model.battery model.target

        height_diff =
            toFloat model.target.z - toFloat model.battery.z

        top_part =
            -- Here we use "op". It should either be the "+" or "-" function.
            op (velocity ^ 2) (sqrt ((velocity ^ 4) - gravity * ((gravity * range ^ 2) + (2 * height_diff * velocity ^ 2))))
    in
        atan (top_part / (gravity * range))


{-| Compute the elevation inclination in degrees.

This function takes an "op" operator that is passed down to the phi function.
There can be two different valid values for phi depending on the operator
passed here (addition or substraction).

-}
elevation : (Float -> Float -> Float) -> ArtilleryModel -> Float
elevation op model =
    toDegrees (phi op model)


{-| Compute the flight time (ETA) to target.

This function takes an "op" operator that is passed down to the phi function.
There can be two different valid values for phi depending on the operator
passed here (addition or substraction).

-}
timeToTarget : (Float -> Float -> Float) -> ArtilleryModel -> Float
timeToTarget op model =
    let
        range =
            distance model.battery model.target

        velocity =
            firstValidRangeVelocity model.selected_profile range
    in
        range / (velocity * cos (phi op model))


fireMode : ArtilleryModel -> String
fireMode model =
    let
        range =
            distance model.battery model.target
    in
        firstValidRangeName model.selected_profile range


type Msg
    = BatteryXChange String
    | BatteryYChange String
    | BatteryZChange String
    | TargetXChange String
    | TargetYChange String
    | TargetZChange String
    | SwitchTo BatteryProfile


update : Msg -> ArtilleryModel -> ArtilleryModel
update msg model =
    case msg of
        BatteryXChange changed ->
            let
                changedInt =
                    Result.withDefault 0 (String.toInt changed)

                battery =
                    model.battery
            in
                { model | battery = { battery | x = changedInt } }

        BatteryYChange changed ->
            let
                changedInt =
                    Result.withDefault 0 (String.toInt changed)

                battery =
                    model.battery
            in
                { model | battery = { battery | y = changedInt } }

        BatteryZChange changed ->
            let
                changedInt =
                    Result.withDefault 0 (String.toInt changed)

                battery =
                    model.battery
            in
                { model | battery = { battery | z = changedInt } }

        TargetXChange changed ->
            let
                changedInt =
                    Result.withDefault 0 (String.toInt changed)

                target =
                    model.target
            in
                { model | target = { target | x = changedInt } }

        TargetYChange changed ->
            let
                changedInt =
                    Result.withDefault 0 (String.toInt changed)

                target =
                    model.target
            in
                { model | target = { target | y = changedInt } }

        TargetZChange changed ->
            let
                changedInt =
                    Result.withDefault 0 (String.toInt changed)

                target =
                    model.target
            in
                { model | target = { target | z = changedInt } }

        SwitchTo profile ->
            { model | selected_profile = profile }



-- VIEW


toStringOrImpossible : Float -> String
toStringOrImpossible float =
    if isNaN float then
        "impossible"
    else
        Round.round 2 float


renderBatteries : String -> Html Msg
renderBatteries one_battery =
    let
        battery_profile =
            withDefault mk6_mortar (get one_battery batteries_map)
    in
        div []
            [ input
                [ Attr.type_ "radio", Attr.name "battery", Attr.checked (battery_profile == model.selected_profile), onClick (SwitchTo battery_profile) ]
                []
            , text battery_profile.name
            ]


renderCoordInput : String -> (String -> Msg) -> (String -> Msg) -> (String -> Msg) -> Html Msg
renderCoordInput text_message x_message y_message z_message =
    div [ Attr.class "field" ]
        [ label [ Attr.class "label" ] [ text text_message ]
        , div [ Attr.class "control columns" ]
            [ input
                [ Attr.type_ "number"
                , Attr.class "input is-medium column"
                , onInput x_message
                ]
                []
            , input
                [ Attr.type_ "number"
                , Attr.class "input is-medium column"
                , onInput y_message
                ]
                []
            , input
                [ Attr.type_ "number"
                , Attr.class "input is-medium column"
                , onInput z_message
                ]
                []
            ]
        ]


renderOutput : ArtilleryModel -> Html Msg
renderOutput model =
    div [ Attr.class "tile is-ancestor" ]
        [ div [ Attr.class "tile is-vertical is-8" ]
            [ div [ Attr.class "tile" ]
                [ div [ Attr.class "tile is-parent is-vertical" ]
                    [ article [ Attr.class "tile is-child notification is-primary" ]
                        [ p [ Attr.class "title" ] [ text "Fire mode: " ]
                        , p [ Attr.class "subtitle" ] [ text (fireMode model) ]
                        ]
                    , article [ Attr.class "title is-child notification is-success" ]
                        [ p [ Attr.class "title" ] [ text "Bearing" ]
                        , p [ Attr.class "subtitle" ]
                            [ text (toStringOrImpossible (bearing model))
                            ]
                        ]
                    ]
                , div
                    [ Attr.class "tile is-parent" ]
                    [ article [ Attr.class "tile is-child notification is-info" ]
                        [ p [ Attr.class "title" ] [ text "Solution 1" ]
                        , p [ Attr.class "subtitle" ]
                            [ text "Elevation: "
                            , text (toStringOrImpossible (elevation (+) model))
                            ]
                        , p [ Attr.class "subtitle" ]
                            [ text "Flight time (seconds): "
                            , text (toStringOrImpossible (timeToTarget (+) model))
                            ]
                        ]
                    ]
                ]
            ]
        , div [ Attr.class "tile is-parent" ]
            [ article [ Attr.class "tile is-child notification is-info" ]
                [ p [ Attr.class "title" ] [ text "Solution 2" ]
                , p [ Attr.class "subtitle" ]
                    [ text "Elevation: "
                    , text (toStringOrImpossible (elevation (-) model))
                    ]
                , p [ Attr.class "subtitle" ]
                    [ text "Flight time (seconds): "
                    , text (toStringOrImpossible (timeToTarget (-) model))
                    ]
                ]
            ]
        ]


view : ArtilleryModel -> Html Msg
view model =
    div []
        [ section [ Attr.class "hero is-primary" ]
            [ div [ Attr.class "hero-body" ]
                [ div [ Attr.class "container" ]
                    [ h1 [ Attr.class "title" ] [ text "Tribaal's Arma3 artillery computer." ]
                    , h2 [ Attr.class "subtitle" ] [ text "Rewritten in Elm for your exploding pleasure." ]
                    ]
                ]
            ]
        , section [ Attr.class "section" ]
            [ div [ Attr.class "container" ]
                [ h2 [ Attr.class "subtitle" ] [ text "Instructions" ]
                , p []
                    [ text "Fill in the input coordinates for both the battery and your target using 4-digit grid"
                    , text " coordinates, then select the battery type you are shooting with."
                    ]
                , p [] [ text "The program will compute firing solutions in the 'output section.'" ]
                , p [] [ text "The code for this project can be found ", a [ Attr.href "https://github.com/chrisglass/elm-arma3-artillery" ] [ text "on Github" ] ]
                , p [] [ text "Is this page useful? Is it not? ", a [ Attr.href "https://twitter.com/3baal" ] [ text "Don't hesitate to let me know!" ] ]
                ]
            ]
        , section [ Attr.class "section" ]
            [ div [ Attr.class "container" ]
                [ h1 [ Attr.class "title" ] [ text "Input" ]
                , p [] [ text "Type in 4-digit coordinates here - the normal grid and an estimation of the 4th digit." ]
                , renderCoordInput "Battery coordinates (X, Y, Z):" BatteryXChange BatteryYChange BatteryZChange
                , renderCoordInput "Target coordinates (X, Y, Z):" TargetXChange TargetYChange TargetZChange
                ]
            ]
        , section [ Attr.class "section" ]
            [ div [ Attr.class "container" ]
                [ h1 [ Attr.class "title" ] [ text "Battery" ]
                , p [] [ text "The battery you are shooting with." ]
                , p []
                    [ text "Selected: "
                    , text model.selected_profile.name
                    ]
                ]
            , div [ Attr.class "container" ]
                [ fieldset []
                    (List.map renderBatteries (keys batteries_map))
                ]
            ]
        , section [ Attr.class "section" ]
            [ div [ Attr.class "container" ]
                [ h1 [ Attr.class "title" ] [ text "Output" ]
                , p [] [ text "Set those into the game, then press fire." ]
                , renderOutput model
                ]
            ]
        ]
