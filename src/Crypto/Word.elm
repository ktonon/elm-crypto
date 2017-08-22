module Crypto.Word exposing (Size, bits32, bits64, sizeInBytes)

{-| Functions and types for working with words (groups of bytes).

@docs Size, sizeInBytes, bits32, bits64

-}


{-| Size of a word.
-}
type Size
    = Bits32
    | Bits64


{-| Word size converted to number of bytes.
-}
sizeInBytes : Size -> Int
sizeInBytes size =
    case size of
        Bits32 ->
            4

        Bits64 ->
            8


{-| 32-bit words size.
-}
bits32 : Size
bits32 =
    Bits32


{-| 64-bit words size.
-}
bits64 : Size
bits64 =
    Bits64
