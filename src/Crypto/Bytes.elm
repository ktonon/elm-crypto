module Crypto.Bytes exposing (fixLength, fromInt, fromUTF8, fromWords, toHex, toWords)

{-| Working with lists of bytes.

    import Array
    import Byte
    import Crypto.Word exposing (bits32, bits64)

-}

import Array exposing (Array)
import Bitwise
import Byte exposing (Byte)
import Char
import Crypto.Word as Word


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

    fromInt 16777216
    --> [ 1, 0, 0, 0 ] |> List.map Byte.fromInt

    fromInt 281474976710656
    --> [ 1, 0, 0, 0, 0, 0, 0 ] |> List.map Byte.fromInt

    fromInt 72058693549555970
    --> [ 1, 0, 1, 0, 0, 0, 1, 0 ] |> List.map Byte.fromInt

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
                    // (2 ^ (b * 8))
                    |> Byte.fromInt
            )
        |> List.reverse


{-| Convert an array of words to a list of bytes.

    fromWords bits32 Array.empty
    --> []

    [ 0 ] |> Array.fromList |> fromWords bits32
    --> [ 0, 0, 0, 0 ] |> List.map Byte.fromInt

    [ 0 ] |> Array.fromList |> fromWords bits64
    --> [ 0, 0, 0, 0, 0, 0, 0, 0 ] |> List.map Byte.fromInt

    [ 16843010 ] |> Array.fromList |> fromWords bits32
    --> [ 1, 1, 1, 2 ] |> List.map Byte.fromInt

    [ 16843010, 16843010, 72058693549555970 ] |> Array.fromList |> fromWords bits64
    --> [ 0, 0, 0, 0, 1, 1, 1, 2
    --> , 0, 0, 0, 0, 1, 1, 1, 2
    --> , 1, 0, 1, 0, 0, 0, 1, 0
    --> ] |> List.map Byte.fromInt

    [ 1231491741, 32472341, 5308308302 ]
        |> Array.fromList
        |> fromWords bits64
        |> toWords bits64
        |> Array.toList
    --> [ 1231491741, 32472341, 5308308302 ]

-}
fromWords : Word.Size -> Array Int -> List Byte
fromWords wordSize =
    Array.toList
        >> List.concatMap (fromInt >> fixLength (Word.sizeInBytes wordSize) 0)


{-| Convert a list of bytes to a list of words of the given size.

    toWords bits32 []
    --> [] |> Array.fromList

    toWords bits64 []
    --> [] |> Array.fromList

    toWords bits32 [ Byte.fromInt 0x01 ]
    --> [] |> Array.fromList

    toWords bits32 ([ 0x01, 0x01, 0x01, 0x02 ] |> List.map Byte.fromInt)
    --> [ 16843010 ] |> Array.fromList

    toWords bits64 ([ 0x01, 0x01, 0x01, 0x02 ] |> List.map Byte.fromInt)
    --> [] |> Array.fromList

    toWords bits32 ([ 0x01, 0x01, 0x01, 0x02, 0xFF ] |> List.map Byte.fromInt)
    --> [ 16843010 ] |> Array.fromList

    toWords bits32 (
        [ 0x01, 0x01, 0x01, 0x02
        , 0x00, 0x00, 0x00, 0xFF
        , 0x01, 0x00, 0x00, 0x00
        ] |> List.map Byte.fromInt)
    --> [ 16843010, 255, 16777216 ] |> Array.fromList

    toWords bits64 (
        [ 0x01, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00
        , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF
        , 0x00, 0x00, 0x00, 0x00, 0x01, 0x01, 0x01, 0x01
        ] |> List.map Byte.fromInt)
    --> [ 72058693549555970, 255, 16843009 ] |> Array.fromList

    toWords bits32 (List.repeat 400000 (Byte.fromInt 0x01))
    --> Array.repeat 100000 16843009

    toWords bits64 (List.repeat 800000 (Byte.fromInt 0x01))
    --> Array.repeat 100000 72340172838076670

-}
toWords : Word.Size -> List Byte -> Array Int
toWords wordSize bytes =
    toWordsHelper wordSize bytes Array.empty


toWordsHelper : Word.Size -> List Byte -> Array Int -> Array Int
toWordsHelper wordSize bytes acc =
    case
        nextWordPart
            (Word.sizeInBytes wordSize - 1)
            0
            bytes
    of
        ( Nothing, _ ) ->
            acc

        ( Just word, [] ) ->
            Array.push word acc

        ( Just word, rest ) ->
            toWordsHelper
                wordSize
                rest
                (Array.push word acc)


nextWordPart : Int -> Int -> List Byte -> ( Maybe Int, List Byte )
nextWordPart i acc bytes =
    if i >= 0 then
        case bytes of
            byte :: rest ->
                nextWordPart
                    (i - 1)
                    (byte
                        |> Byte.toInt
                        |> (*) (2 ^ (i * 8))
                        |> (+) acc
                    )
                    rest

            _ ->
                ( Nothing, [] )
    else
        ( Just acc, bytes )


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


{-| Fix a list
-}
fixLength : Int -> Int -> List Byte -> List Byte
fixLength byteCount val list =
    case compare (List.length list) byteCount of
        EQ ->
            list

        LT ->
            List.append
                (List.repeat (byteCount - List.length list) (Byte.fromInt val))
                list

        GT ->
            List.take byteCount list
