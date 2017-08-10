module Main exposing (main)

import Basics exposing (atan, atan2, pi, sqrt)
import Batteries exposing (BatteryProfile, mk6_mortar)
import Html exposing (Html, button, div, input, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
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


type alias ArtilleryModel =
    { battery_x : Int
    , battery_y : Int
    , battery_z : Int
    , target_x : Int
    , target_y : Int
    , target_z : Int
    }


model : ArtilleryModel
model =
    { battery_x = 0
    , battery_y = 0
    , battery_z = 0
    , target_x = 0
    , target_y = 0
    , target_z = 0
    }



-- UPDATE


toDegrees : Float -> Float
toDegrees radians =
    radians * 180 / pi


vectorDelta : ArtilleryModel -> ( Float, Float )
vectorDelta model =
    let
        delta_x =
            toFloat model.target_x - toFloat model.battery_x

        delta_y =
            toFloat model.target_y - toFloat model.battery_y
    in
    ( delta_x, delta_y )


bearing : ArtilleryModel -> Float
bearing model =
    let
        ( delta_x, delta_y ) =
            vectorDelta model
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
            vectorDelta model
    in
    smallest_grid * sqrt ((delta_x ^ 2) + (delta_y ^ 2))


{-| Compute the elevation inclination in degrees.
-}
elevation : ArtilleryModel -> Float
elevation model =
    let
        velocity =
            mk6_mortar.medium.velocity

        range =
            distance model

        height_diff =
            toFloat model.target_z - toFloat model.battery_z

        top_part =
            (velocity ^ 2) + sqrt ((velocity ^ 4) - gravity * ((gravity * range ^ 2) + (2 * height_diff * velocity ^ 2)))
    in
    toDegrees (atan (top_part / (gravity * range)))


type Msg
    = BatteryXChange String
    | BatteryYChange String
    | BatteryZChange String
    | TargetXChange String
    | TargetYChange String
    | TargetZChange String


update : Msg -> ArtilleryModel -> ArtilleryModel
update msg model =
    case msg of
        BatteryXChange changed ->
            let
                newInt =
                    Result.withDefault 0 (String.toInt changed)
            in
            { model | battery_x = newInt }

        BatteryYChange changed ->
            let
                newInt =
                    Result.withDefault 0 (String.toInt changed)
            in
            { model | battery_y = newInt }

        BatteryZChange changed ->
            let
                newInt =
                    Result.withDefault 0 (String.toInt changed)
            in
            { model | battery_z = newInt }

        TargetXChange changed ->
            let
                newInt =
                    Result.withDefault 0 (String.toInt changed)
            in
            { model | target_x = newInt }

        TargetYChange changed ->
            let
                newInt =
                    Result.withDefault 0 (String.toInt changed)
            in
            { model | target_y = newInt }

        TargetZChange changed ->
            let
                newInt =
                    Result.withDefault 0 (String.toInt changed)
            in
            { model | target_z = newInt }



-- VIEW


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
            [ text "Distance: "
            , text (toString (distance model))
            ]
        , div []
            [ text "Bearing: "
            , text (toString (bearing model))
            ]
        , div []
            [ text "Elevation: "
            , text (toString (elevation model))
            ]
        ]



{--|
view model =
    div []
        [ button [ onClick ComputeSolution ] [ text "Compute solution" ]
        , div [] [ text (toString model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]
--}
