module Crypto.SHA.Types exposing (Alg(..), MessageSchedule, RoundConstants, WorkingVars)

import Array exposing (Array)


type Alg
    = SHA224
    | SHA256
    | SHA384
    | SHA512


type alias MessageSchedule =
    Array Int


type alias RoundConstants =
    Array Int


type alias WorkingVars =
    { a : Int
    , b : Int
    , c : Int
    , d : Int
    , e : Int
    , f : Int
    , g : Int
    , h : Int
    }
