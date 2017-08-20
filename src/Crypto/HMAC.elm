module Crypto.HMAC exposing (Hash, Key, Message, digest, sha224, sha256)

{-| Compute HMAC SHA message digests.

    import Crypto.Bytes as Bytes

@docs digest, sha224, sha256


## Aliases

@docs Key, Message, Hash

-}

import Byte exposing (Byte)
import Crypto.Bytes as Bytes
import Crypto.Hash as Hash


-- EXPOSED API


{-| Secret key
-}
type alias Key =
    List Byte


{-| Message to be hashed
-}
type alias Message =
    List Byte


{-| Type of hash algorithm.
-}
type Hash
    = SHA224
    | SHA256


{-| Use SHA224 as the hash algorithm.
-}
sha224 : Hash
sha224 =
    SHA224


{-| Use SHA256 as the hash algorithm.
-}
sha256 : Hash
sha256 =
    SHA256



-- Crypto.HMAC.digest sha256
--     (Bytes.fromUTF8 "")
--     (Bytes.fromUTF8 "")
-- --> "b613679a0814d9ec772f95d778c35fc5ff1697c493715653c6c712144292c5ad"


{-| HMAC SHA256 digest.

    0
    --> 0

-}
digest : Hash -> Key -> Message -> String
digest type_ key message =
    normalizeKey (hash type_) (blockSize type_) key
        |> hmac_ (hash type_) message
        |> Bytes.toHex


hmac_ : (List Byte -> List Byte) -> Message -> Key -> List Byte
hmac_ hash message key =
    let
        oKeyPad =
            key
                |> List.map (Byte.xor <| Byte.fromInt 0x5C)
                |> i2 "okey"

        iKeyPad =
            key
                |> List.map (Byte.xor <| Byte.fromInt 0x36)
                |> i2 "ikey"
    in
    List.append iKeyPad message
        |> hash
        -- |> everyOther
        |> i2 "inner"
        |> inspectBytes "inner"
        |> List.append oKeyPad
        |> i2 "outer"
        |> hash
        |> i2 "final"


inspectBytes : String -> List Byte -> List Byte
inspectBytes label =
    \b ->
        let
            _ =
                Debug.log label <| Bytes.toHex b
        in
        b


i2 : String -> List Byte -> List Byte
i2 label =
    \b ->
        let
            _ =
                b |> List.map Byte.toInt |> Debug.log label
        in
        b



-- HELPERS


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
                |> Debug.log "Hashing key"
                |> hash
                |> Debug.log "Hashed key"
                |> padEnd blockSize


padEnd : Int -> List Byte -> List Byte
padEnd blockSize bytes =
    List.append bytes <|
        List.repeat
            (blockSize - List.length bytes)
            (Byte.fromInt 0)


blockSize : Hash -> Int
blockSize _ =
    64


everyOther : List a -> List a
everyOther aList =
    aList
        |> List.indexedMap (,)
        |> List.foldl
            (\( i, val ) acc ->
                if i % 2 == 0 then
                    acc
                else
                    val :: acc
            )
            []
        |> List.reverse


hash : Hash -> List Byte -> List Byte
hash hash =
    case hash of
        SHA224 ->
            Hash.sha224

        SHA256 ->
            Hash.sha256
