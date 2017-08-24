module Crypto.Debug exposing (..)

import Array exposing (Array)
import Crypto.SHA.Alg as Alg exposing (Alg(..))
import Crypto.SHA.Types exposing (WorkingVars, workingVarsToWords)
import Word exposing (Word)


log : Alg -> String -> Array Word -> Array Word
log alg label words =
    case alg of
        SHA512 ->
            let
                _ =
                    Debug.crash label (prettyWords words)
            in
            words

        _ ->
            words


prettyWords : Array Word -> String
prettyWords =
    Array.toList
        >> List.map (\word -> Array.fromList [ word ] |> Word.toHex)
        >> String.join ","


logW : Alg -> String -> Word -> Word
logW alg label word =
    case alg of
        SHA512 ->
            let
                _ =
                    Debug.log label (prettyWords (Array.fromList [ word ]))
            in
            word

        _ ->
            word


logWV : Alg -> String -> WorkingVars -> WorkingVars
logWV alg label vars =
    case alg of
        SHA512 ->
            let
                _ =
                    Debug.crash
                        (label
                            ++ (vars
                                    |> workingVarsToWords alg
                                    |> prettyWords
                                    |> toString
                               )
                        )
            in
            vars

        _ ->
            vars
