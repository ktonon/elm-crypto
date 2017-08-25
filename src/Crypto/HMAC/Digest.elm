module Crypto.HMAC.Digest exposing (digestBytes)

import Byte exposing (Byte)
import Word.Bytes as Bytes


digestBytes : (List Byte -> List Byte) -> Int -> List Byte -> List Byte -> String
digestBytes hash blockSize key message =
    key
        |> normalizeKey hash blockSize
        |> hmac_ hash message
        |> Bytes.toHex


hmac_ : (List Byte -> List Byte) -> List Byte -> List Byte -> List Byte
hmac_ hash message key =
    let
        oKeyPad =
            key
                |> List.map (Byte.xor <| Byte.fromInt 0x5C)

        iKeyPad =
            key
                |> List.map (Byte.xor <| Byte.fromInt 0x36)
    in
    List.append iKeyPad message
        |> hash
        |> List.append oKeyPad
        |> hash


normalizeKey : (List Byte -> List Byte) -> Int -> List Byte -> List Byte
normalizeKey hash blockSize key =
    case compare blockSize <| List.length key of
        EQ ->
            key

        GT ->
            key
                |> padEnd blockSize

        LT ->
            key
                |> hash
                |> padEnd blockSize


padEnd : Int -> List Byte -> List Byte
padEnd blockSize bytes =
    List.append bytes <|
        List.repeat
            (blockSize - List.length bytes)
            (Byte.fromInt 0)
