module Crypto.SHA.Process exposing (chunks)

import Array exposing (Array)
import Crypto.SHA.Types exposing (Alg(..), MessageSchedule, RoundConstants, WorkingVars)


chunks : Alg -> Array Int -> MessageSchedule
chunks alg message =
    message
