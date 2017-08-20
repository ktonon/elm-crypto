module Crypto.SHA.Preprocess exposing (calculateK, preprocess)

{-| SHA-2 preprocess.

    import Crypto.SHA.Types exposing (Alg(..))

-}

import Byte exposing (Byte)
import Crypto.SHA.Constants exposing (initialHashValues, roundConstants)
import Crypto.SHA.Types exposing (Alg(..), MessageSchedule, RoundConstants, WorkingVars)


preprocess : Alg -> List Byte -> List Byte
preprocess alg message =
    let
        messageBitCount =
            (*) 8 <| List.length message
    in
    message


{-| Calculate the amount of 0 bit padding.

    calculateK SHA256 (512 - 64 - 1)
    --> 0

    calculateK SHA512 (1024 - 128 - 1)
    --> 0

    calculateK SHA256 (512 - 64 - 1 + 8)
    --> (512 - 8)

    calculateK SHA256 (512 - 64 - 1 - 8)
    --> 8

    calculateK SHA384 (1024 - 128 - 1 + 16)
    --> (1024 - 16)

    calculateK SHA384 (1024 - 128 - 1 - 16)
    --> 16

-}
calculateK : Alg -> Int -> Int
calculateK alg l =
    let
        c =
            chunkSizeBits alg
    in
    (c
        - 1
        - messageSizeBits alg
        - (l % c)
    )
        % c


messageSizeBits : Alg -> Int
messageSizeBits alg =
    case alg of
        SHA224 ->
            messageSizeBits SHA256

        SHA256 ->
            64

        SHA384 ->
            messageSizeBits SHA512

        SHA512 ->
            128


chunkSizeBits : Alg -> Int
chunkSizeBits alg =
    case alg of
        SHA224 ->
            chunkSizeBits SHA256

        SHA256 ->
            512

        SHA384 ->
            chunkSizeBits SHA512

        SHA512 ->
            1024
