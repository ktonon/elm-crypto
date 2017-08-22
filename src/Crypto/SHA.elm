module Crypto.SHA exposing (digest)

import Byte exposing (Byte)
import Crypto.Bytes as Bytes
import Crypto.SHA.Preprocess
import Crypto.SHA.Process
import Crypto.SHA.Types exposing (Alg(..), MessageSchedule, RoundConstants, WorkingVars)
import Crypto.Word as Word


digest : Alg -> List Byte -> List Byte
digest alg =
    Crypto.SHA.Preprocess.preprocess alg
        >> Bytes.toWords (wordSize alg)
        >> Crypto.SHA.Process.chunks alg
        >> Bytes.fromWords (wordSize alg)


wordSize : Alg -> Word.Size
wordSize alg =
    case alg of
        SHA224 ->
            wordSize SHA256

        SHA256 ->
            Word.bits32

        SHA384 ->
            wordSize SHA512

        SHA512 ->
            Word.bits64
