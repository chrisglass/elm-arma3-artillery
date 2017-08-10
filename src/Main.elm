module Main exposing (main)

import Basics exposing (atan2, pi, sqrt)
import Batteries exposing (m4_scorcher)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


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
smallest_grid : Int
smallest_grid =
    10


type alias MapCoord =
    { x : Int
    , y : Int
    , z : Int
    }


type alias Model =
    { origin : MapCoord
    , target : MapCoord
    }


model : Model
model =
    { origin = MapCoord 0 0 0
    , target = MapCoord 0 0 0
    }



-- UPDATE


toDegrees : Float -> Float
toDegrees radians =
    radians * 180 / pi


bearing : Float -> Float -> Float
bearing x y =
    if x == 0 then
        0
    else if x <= 0 && y >= 0 then
        450 - toDegrees (atan2 y x)
    else
        90 - toDegrees (atan2 y x)


{-| Compute the distance between two map coordinates (MapCoord types).
-}
distance : MapCoord -> MapCoord -> Int
distance coord1 coord2 =
    let
        delta_x =
            coord1.x - coord2.x

        delta_y =
            coord1.y - coord2.y
    in
    smallest_grid * sqrt (delta_x ^ 2) + (delta_y ^ 2)


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model

        Decrement ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (toString model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]
