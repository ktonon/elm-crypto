module Crypto.SHA exposing (preprocess)

import Byte exposing (Byte)
import Crypto.SHA.Preprocess
import Crypto.SHA.Types exposing (Alg(..), MessageSchedule, RoundConstants, WorkingVars)


preprocess : Alg -> List Byte -> List Byte
preprocess =
    Crypto.SHA.Preprocess.preprocess
