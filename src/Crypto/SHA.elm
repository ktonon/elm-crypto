module Crypto.SHA exposing (digest)

import Byte exposing (Byte)
import Crypto.SHA.Alg as Alg exposing (Alg(..))
import Crypto.SHA.Preprocess
import Crypto.SHA.Process
import Word


digest : Alg -> List Byte -> List Byte
digest alg =
    Crypto.SHA.Preprocess.preprocess alg
        >> Word.fromBytes (Alg.wordSize alg)
        >> Crypto.SHA.Process.chunks alg
        >> Word.toBytes
