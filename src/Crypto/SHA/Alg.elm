module Crypto.SHA.Alg exposing (Alg(..), wordSize)

import Word exposing (Size(..))


type Alg
    = SHA224
    | SHA256
    | SHA384
    | SHA512


wordSize : Alg -> Size
wordSize alg =
    case alg of
        SHA224 ->
            wordSize SHA256

        SHA256 ->
            Bit32

        SHA384 ->
            wordSize SHA512

        SHA512 ->
            Bit64
