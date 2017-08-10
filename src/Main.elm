module Main exposing (main)

import Basics exposing (atan2, pi, sqrt, toFloat)
import Batteries exposing (BatteryProfile, mk6_mortar)
import Html exposing (Html, button, div, input, text)
import Html.Events exposing (onClick, onInput)


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
    { origin : MapCoord
    , target : MapCoord
    , active_battery : BatteryProfile
    }


model : ArtilleryModel
model =
    { origin = MapCoord 0 0 0
    , target = MapCoord 0 0 0
    , active_battery = mk6_mortar
    }



-- UPDATE


toDegrees : Float -> Float
toDegrees radians =
    radians * 180 / pi


{-| A helper that returns a tuple of the difference between two MapCoords.
-}
vectorDifference : MapCoord -> MapCoord -> ( Float, Float )
vectorDifference coord1 coord2 =
    let
        delta_x =
            toFloat coord1.x - toFloat coord2.x

        delta_y =
            toFloat coord1.y - toFloat coord2.y
    in
    ( delta_x, delta_y )


{-| Get the bearing between two MapCoord. It is assumed that the first
parameter is the origin of the bearing and the second its destination.
-}
bearing : MapCoord -> MapCoord -> Float
bearing coord1 coord2 =
    let
        ( delta_x, delta_y ) =
            vectorDifference coord1 coord2
    in
    if delta_x == 0 then
        0
    else if delta_x <= 0 && delta_y >= 0 then
        450 - toDegrees (atan2 delta_y delta_x)
    else
        90 - toDegrees (atan2 delta_y delta_x)


{-| Compute the distance betweein two map coordinates (MapCoord types), ignoring
elevation differences (2 dimentonal distance).
-}
distance : MapCoord -> MapCoord -> Float
distance coord1 coord2 =
    let
        ( delta_x, delta_y ) =
            vectorDifference coord1 coord2
    in
    smallest_grid * sqrt (delta_x ^ 2) + (delta_y ^ 2)


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
            { model | { origin | x = changed } }

        BatteryYChange changed ->
            { model | origin = changed }



-- VIEW


view : ArtilleryModel -> Html Msg
view model =
    div []
        [ text "Battery coordinates, X Y Z"
        , input [ onInput BatteryXChange ] []
        , input [ onInput BatteryYChange ] []
        , input [ onInput BatteryZChange ]
        , text "Target coordinates, X Y Z"
        , input [ onInput BatteryXChange ] []
        , input [ onInput BatteryYChange ] []
        , input [ onInput BatteryZChange ]
        ]



{--|
view model =
    div []
        [ button [ onClick ComputeSolution ] [ text "Compute solution" ]
        , div [] [ text (toString model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]
--}
