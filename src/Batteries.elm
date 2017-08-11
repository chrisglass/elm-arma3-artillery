module Batteries exposing (BatteryProfile, batteries_map, m4_scorcher)

{-| This module contains the definitions for the various artillery pieces,
specifically their ranges and velocities equivalents for the various in-game
fire modes.
-}

import Dict exposing (fromList)


{-| The range information for a specific fire mode, for a specific battery.
-}
type alias RangeProfile =
    { min_range : Int
    , max_range : Int
    , velocity : Float
    }


{-| A particular in-game battery model (the thing that shoots)
-}
type alias BatteryProfile =
    { short : RangeProfile
    , medium : RangeProfile
    , far : RangeProfile
    , further : RangeProfile
    , extreme : RangeProfile
    , name : String
    }


m4_scorcher : BatteryProfile
m4_scorcher =
    { short = RangeProfile 826 2415 153.9
    , medium = RangeProfile 2059 6021 243.0
    , far = RangeProfile 2059 6021 243.0
    , further = RangeProfile 2059 6021 243.0
    , extreme = RangeProfile 2059 6021 243.0
    , name = "M4 Scorcher"
    }


mk6_mortar : BatteryProfile
mk6_mortar =
    { short = RangeProfile 34 499 70
    , medium = RangeProfile 139 1998 140
    , far = RangeProfile 284 4078 200
    , further = RangeProfile -1 -1 0.0
    , extreme = RangeProfile -1 -1 0.0
    , name = "Mk6 Mortar"
    }


batteries_list : List String
batteries_list =
    [ "mk6_mortar", "m4_scorcher" ]


batteries_map : Dict.Dict String BatteryProfile
batteries_map =
    Dict.fromList [ ( "mk6_mortar", mk6_mortar ), ( "m4_scorcher", m4_scorcher ) ]
