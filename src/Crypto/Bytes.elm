module Crypto.Bytes exposing (fromInt, fromUTF8, toHex)

{-| Working with lists of bytes.

    import Byte

-}

import Bitwise
import Byte exposing (Byte)
import Char


{-| Split an integer value into bytes.

    fromInt 0
    --> [ 0 ] |> List.map Byte.fromInt

    fromInt 1
    --> [ 1 ] |> List.map Byte.fromInt

    fromInt 2
    --> [ 2 ] |> List.map Byte.fromInt

    fromInt 255
    --> [ 255 ] |> List.map Byte.fromInt

    fromInt 256
    --> [ 1, 0 ] |> List.map Byte.fromInt

    fromInt 65537
    --> [ 1, 0, 1 ] |> List.map Byte.fromInt

-}
fromInt : Int -> List Byte
fromInt val =
    let
        n =
            if val > 1 then
                logBase 2 (toFloat val)
                    / 8
                    |> floor
            else
                0
    in
    List.range 0 n
        |> List.map
            (\b ->
                val
                    |> Bitwise.shiftRightZfBy (b * 8)
                    |> Byte.fromInt
            )
        |> List.reverse


{-| Convert a character into a list of bytes

    fromUTF8 "a"
    --> [ Byte.fromInt 97 ]

    fromUTF8 "I ❤ cheese"
    --> [ 73, 32,
    -->   226, 157, 164,
    -->   32, 99, 104, 101, 101, 115, 101 ]
    --> |> List.map Byte.fromInt

    fromUTF8 "dѐf"
    --> [ 100, 209, 144, 102 ] |> List.map Byte.fromInt

-}
fromUTF8 : String -> List Byte
fromUTF8 =
    String.toList
        >> List.foldl
            (\char acc ->
                List.append acc (char |> Char.toCode |> splitUtf8)
            )
            []
        >> List.map Byte.fromInt


splitUtf8 : Int -> List Int
splitUtf8 x =
    if x < 128 then
        [ x ]
    else if x < 2048 then
        [ x |> Bitwise.and 0x07C0 |> Bitwise.shiftRightZfBy 6 |> Bitwise.or 0xC0
        , x |> Bitwise.and 0x3F |> Bitwise.or 0x80
        ]
    else
        [ x |> Bitwise.and 0xF000 |> Bitwise.shiftRightZfBy 12 |> Bitwise.or 0xE0
        , x |> Bitwise.and 0x0FC0 |> Bitwise.shiftRightZfBy 6 |> Bitwise.or 0x80
        , x |> Bitwise.and 0x3F |> Bitwise.or 0x80
        ]


{-| Convert a list of bytes to a string of hexadecimal characters.
-}
toHex : List Byte -> String
toHex =
    List.foldl
        (\byte acc ->
            List.append acc
                [ byte |> Byte.toInt |> Bitwise.shiftRightZfBy 4 |> Byte.fromInt |> charToHex
                , byte |> charToHex
                ]
        )
        []
        >> String.fromList


charToHex : Byte -> Char
charToHex byte =
    let
        x2 =
            byte |> Byte.and (Byte.fromInt 0x0F) |> Byte.toInt
    in
    (x2
        + (if x2 < 10 then
            Char.toCode '0'
           else
            -10 + Char.toCode 'a'
          )
    )
        |> Char.fromCode
