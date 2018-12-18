module Crypto.Hash exposing (sha224, sha256, sha384, sha512, sha512_224, sha512_256)

{-| Secure Hash Algorithms.

@docs sha224, sha256, sha384, sha512, sha512_224, sha512_256

-}

import Crypto.SHA as SHA
import Crypto.SHA.Alg exposing (Alg(..))
import Word.Bytes as Bytes
import Word.Hex as Hex


{-| Secure Hash Algorithm using 32-bit words and 64 rounds (truncated).

    Crypto.Hash.sha224 ""
    --> "d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f"

    Crypto.Hash.sha224 "The quick brown fox jumps over the lazy dog"
    --> "730e109bd7a8a32b1cb9d9a09aa2325d2430587ddbc0c38bad911525"

-}
sha224 : String -> String
sha224 message =
    message
        |> Bytes.fromUTF8
        |> SHA.digest SHA224
        |> Hex.fromWordArray


{-| Secure Hash Algorithm using 32-bit words and 64 rounds.

    Crypto.Hash.sha256 ""
    --> "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

-}
sha256 : String -> String
sha256 message =
    message
        |> Bytes.fromUTF8
        |> SHA.digest SHA256
        |> Hex.fromWordArray


{-| Secure Hash Algorithm using 64-bit words and 80 rounds (truncated).

    Crypto.Hash.sha384 ""
    --> "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b"

-}
sha384 : String -> String
sha384 message =
    message
        |> Bytes.fromUTF8
        |> SHA.digest SHA384
        |> Hex.fromWordArray


{-| Secure Hash Algorithm using 64-bit words and 80 rounds.

    Crypto.Hash.sha512 ""
    --> "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"

-}
sha512 : String -> String
sha512 message =
    message
        |> Bytes.fromUTF8
        |> SHA.digest SHA512
        |> Hex.fromWordArray


{-| Secure Hash Algorithm using 64-bit words and 80 rounds (truncated to 224).

    Crypto.Hash.sha512_224 ""
    --> "6ed0dd02806fa89e25de060c19d3ac86cabb87d6a0ddd05c333b84f4"

-}
sha512_224 : String -> String
sha512_224 message =
    message
        |> Bytes.fromUTF8
        |> SHA.digest SHA512_224
        |> Hex.fromWordArray


{-| Secure Hash Algorithm using 64-bit words and 80 rounds (truncated to 256).

    Crypto.Hash.sha512_256 ""
    --> "c672b8d1ef56ed28ab87c3622c5114069bdd3ad7b8f9737498d0c01ecef0967a"

-}
sha512_256 : String -> String
sha512_256 message =
    message
        |> Bytes.fromUTF8
        |> SHA.digest SHA512_256
        |> Hex.fromWordArray
