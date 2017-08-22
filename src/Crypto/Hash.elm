module Crypto.Hash exposing (sha256)

{-| Secure Hash Algorithms.

@docs sha256

-}

import Crypto.Bytes as Bytes
import Crypto.SHA as SHA
import Crypto.SHA.Types exposing (Alg(..))


{-| Secure Hash Algorithm using 32-bit words and 64 rounds.
-}
sha256 : String -> String
sha256 message =
    message
        |> Bytes.fromUTF8
        |> SHA.digest SHA256
        |> Bytes.toHex
