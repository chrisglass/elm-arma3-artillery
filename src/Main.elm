module Main exposing (main)

import Basics exposing (atan, atan2, pi, sqrt)
import Batteries exposing (BatteryProfile, batteries_map, firstValidRangeName, firstValidRangeVelocity, m4_scorcher)
import Dict exposing (get, keys)
import Html exposing (Html, button, div, fieldset, input, li, text, ul)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import List exposing (map)
import Maybe exposing (withDefault)
import String exposing (toInt)


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
    , selected_profile = m4_scorcher
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


distance : ArtilleryModel -> Float
distance model =
    let
        ( delta_x, delta_y ) =
            vectorDifference model.battery model.target
    in
    smallest_grid * sqrt ((delta_x ^ 2) + (delta_y ^ 2))


phi : ArtilleryModel -> Float
phi model =
    let
        velocity =
            firstValidRangeVelocity model.selected_profile range

        range =
            distance model

        height_diff =
            toFloat model.target.z - toFloat model.battery.z

        top_part =
            (velocity ^ 2) + sqrt ((velocity ^ 4) - gravity * ((gravity * range ^ 2) + (2 * height_diff * velocity ^ 2)))
    in
    atan (top_part / (gravity * range))


{-| Compute the elevation inclination in degrees.
-}
elevation : ArtilleryModel -> Float
elevation model =
    toDegrees (phi model)


timeToTarget : ArtilleryModel -> Float
timeToTarget model =
    let
        range =
            distance model

        velocity =
            firstValidRangeVelocity model.selected_profile range
    in
    range / (velocity * cos (phi model))


fireMode : ArtilleryModel -> String
fireMode model =
    let
        range =
            distance model
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


renderBatteries : String -> Html Msg
renderBatteries one_battery =
    let
        battery_profile =
            withDefault m4_scorcher (get one_battery batteries_map)
    in
    div []
        [ input
            [ Attr.type_ "radio", Attr.name "battery", onClick (SwitchTo battery_profile) ]
            []
        , text battery_profile.name
        ]


view : ArtilleryModel -> Html Msg
view model =
    div []
        [ div []
            [ text "Battery coordinates, X Y Z"
            , input
                [ Attr.type_ "number"
                , onInput BatteryXChange
                ]
                []
            , input
                [ Attr.type_ "number"
                , onInput BatteryYChange
                ]
                []
            , input
                [ Attr.type_ "number"
                , onInput BatteryZChange
                ]
                []
            ]
        , div []
            [ text "Target coordinates, X Y Z"
            , input
                [ Attr.type_ "number"
                , onInput TargetXChange
                ]
                []
            , input
                [ Attr.type_ "number"
                , onInput TargetYChange
                ]
                []
            , input
                [ Attr.type_ "number"
                , onInput TargetZChange
                ]
                []
            ]
        , div []
            [ text "Selected:"
            , text model.selected_profile.name
            ]
        , fieldset []
            (List.map renderBatteries (keys batteries_map))
        , div []
            [ text "Distance: "
            , text (toString (distance model))
            ]
        , div []
            [ text "Fire mode: "
            , text (fireMode model)
            ]
        , div []
            [ text "Velocity: "
            , text (toString model.selected_profile.medium.velocity)
            ]
        , div []
            [ text "Bearing: "
            , text (toString (bearing model))
            ]
        , div []
            [ text "Elevation: "
            , text (toString (elevation model))
            ]
        , div []
            [ text "Flight time: "
            , text (toString (timeToTarget model))
            ]
        ]
