module SHA2Tests exposing (all)

import Crypto.SHA
import Crypto.SHA.Alg exposing (Alg(..))
import Expect
import SHA.SHA224LongMsg
import SHA.SHA224ShortMsg
import SHA.SHA256LongMsg
import SHA.SHA256ShortMsg
import SHA.SHA384LongMsg
import SHA.SHA384ShortMsg
import SHA.SHA512LongMsg
import SHA.SHA512ShortMsg
import Test exposing (Test, describe, test)
import Word.Bytes as Bytes


all : Test
all =
    describe "SHA-2"
        (List.map runSuite suites)


runSuite : Suite -> Test
runSuite ( name, hash, vectors ) =
    describe name
        (List.indexedMap (testVector name hash) vectors)


testVector : String -> Alg -> Int -> ( String, String ) -> Test
testVector name alg num ( md, msg ) =
    test (name ++ ": " ++ toString num) <|
        \_ ->
            Expect.equal
                md
                (Bytes.fromHex msg
                    |> (Crypto.SHA.digest alg >> Bytes.toHex)
                )


type alias Suite =
    ( String, Alg, List ( String, String ) )


suites : List Suite
suites =
    [ ( "SHA224 Short", SHA224, SHA.SHA224ShortMsg.vectors )
    , ( "SHA256 Short", SHA256, SHA.SHA256ShortMsg.vectors )
    , ( "SHA384 Short", SHA384, SHA.SHA384ShortMsg.vectors )
    , ( "SHA512 Short", SHA512, SHA.SHA512ShortMsg.vectors )
    , ( "SHA224 Long", SHA224, SHA.SHA224LongMsg.vectors )
    , ( "SHA256 Long", SHA256, SHA.SHA256LongMsg.vectors )
    , ( "SHA384 Long", SHA384, SHA.SHA384LongMsg.vectors )
    , ( "SHA512 Long", SHA512, SHA.SHA512LongMsg.vectors )
    ]
