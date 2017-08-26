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

@docs digest, Key, Message


## Hash Algorithms

@docs Hash, sha224, sha256, sha384, sha512

-}

import Array exposing (Array)
import Crypto.HMAC.Digest exposing (digestBytes)
import Crypto.SHA
import Crypto.SHA.Alg exposing (Alg(..))
import Word exposing (Word)
import Word.Bytes as Bytes


-- EXPOSED API


{-| Secret key
-}
type alias Key =
    String


{-| Message to be hashed
-}
type alias Message =
    String


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

    Crypto.HMAC.digest sha256 "key" "The quick brown fox jumps over the lazy dog"
    --> "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8"

    Crypto.HMAC.digest sha512 "key" "I ❤ cheese"
    --> "a885c96140f95cb0b326306edfba49afbb5d38d3a7ed6ccfd67153429cbd3c56d0c514fcaa53b710bb7ba6cc0dfedfdb4d53795acbeb48eb23aa93e5ce9760dd"

-}
digest : Hash -> Key -> Message -> String
digest type_ key message =
    digestBytes
        (hash type_)
        (blockSize type_)
        (Bytes.fromUTF8 key)
        (Bytes.fromUTF8 message)


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


hash : Hash -> List Int -> Array Word
hash (SHA alg) =
    Crypto.SHA.digest alg
