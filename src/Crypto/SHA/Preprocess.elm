module Crypto.SHA.Preprocess exposing (calculateK, preprocess)

{-| SHA-2 preprocess.

    import Byte
    import Crypto.Bytes exposing (fromUTF8)
    import Crypto.SHA.Types exposing (Alg(..))

-}

import Byte exposing (Byte)
import Crypto.Bytes as Bytes
import Crypto.SHA.Types exposing (Alg(..), MessageSchedule, RoundConstants, WorkingVars)


{-| Append 1 + K zeros + size of message.

    preprocess SHA256 []
    --> 0x80 :: (List.repeat 63 0x00) |> List.map Byte.fromInt

    preprocess SHA256 (fromUTF8 "hello") |> List.length
    --> 64

-}
preprocess : Alg -> List Byte -> List Byte
preprocess alg message =
    List.append message <| postfix alg (8 * List.length message)


postfix : Alg -> Int -> List Byte
postfix alg messageSize =
    List.concat
        [ Bytes.fromInt 0x80
        , List.repeat ((calculateK alg messageSize - 7) // 8) (Byte.fromInt 0x00)
        , Bytes.fromInt messageSize |> fixLength (messageSizeBits alg // 8) (Byte.fromInt 0x00)
        ]


fixLength : Int -> a -> List a -> List a
fixLength w val list =
    case compare (List.length list) w of
        EQ ->
            list

        LT ->
            List.append (List.repeat (w - List.length list) val) list

        GT ->
            List.take w list


{-| Calculate the amount of 0 bit padding.

    calculateK SHA256 0
    --> (512 - 64 - 1)

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
