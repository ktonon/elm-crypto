module Crypto.HMAC
    exposing
        ( Hash
        , Key
        , Message
        , digest
        , sha224
        , sha256
        , sha384
        , sha512
        )

{-| Compute HMAC SHA message digests.

    import Word.Bytes as Bytes

@docs digest, sha224, sha256, sha384, sha512


## Aliases

@docs Key, Message, Hash

-}

import Byte exposing (Byte)
import Crypto.SHA
import Crypto.SHA.Alg exposing (Alg(..))
import Word.Bytes as Bytes


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
    = SHA Crypto.SHA.Alg.Alg


{-| Use SHA224 as the hash algorithm.
-}
sha224 : Hash
sha224 =
    SHA Crypto.SHA.Alg.SHA224


{-| Use SHA256 as the hash algorithm.
-}
sha256 : Hash
sha256 =
    SHA Crypto.SHA.Alg.SHA256


{-| Use SHA224 as the hash algorithm.
-}
sha384 : Hash
sha384 =
    SHA Crypto.SHA.Alg.SHA384


{-| Use SHA256 as the hash algorithm.
-}
sha512 : Hash
sha512 =
    SHA Crypto.SHA.Alg.SHA512


{-| HMAC SHA256 digest.

    Crypto.HMAC.digest sha256
    (Bytes.fromUTF8 "")
    (Bytes.fromUTF8 "")
    --> "b613679a0814d9ec772f95d778c35fc5ff1697c493715653c6c712144292c5ad"

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

        iKeyPad =
            key
                |> List.map (Byte.xor <| Byte.fromInt 0x36)
    in
    List.append iKeyPad message
        |> hash
        |> List.append oKeyPad
        |> hash



-- inspectBytes : String -> List Byte -> List Byte
-- inspectBytes label =
--     \b ->
--         let
--             _ =
--                 Debug.log label <| Bytes.toHex b
--         in
--         b
--
--
-- i2 : String -> List Byte -> List Byte
-- i2 label =
--     \b ->
--         let
--             _ =
--                 b |> List.map Byte.toInt |> Debug.log label
--         in
--         b
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
                |> hash
                |> padEnd blockSize


padEnd : Int -> List Byte -> List Byte
padEnd blockSize bytes =
    List.append bytes <|
        List.repeat
            (blockSize - List.length bytes)
            (Byte.fromInt 0)


blockSize : Hash -> Int
blockSize (SHA alg) =
    case alg of
        SHA224 ->
            blockSize (SHA SHA256)

        SHA256 ->
            64

        SHA384 ->
            blockSize (SHA SHA512)

        SHA512 ->
            128



-- everyOther : List a -> List a
-- everyOther aList =
--     aList
--         |> List.indexedMap (,)
--         |> List.foldl
--             (\( i, val ) acc ->
--                 if i % 2 == 0 then
--                     acc
--                 else
--                     val :: acc
--             )
--             []
--         |> List.reverse


hash : Hash -> List Byte -> List Byte
hash (SHA alg) =
    Crypto.SHA.digest alg
