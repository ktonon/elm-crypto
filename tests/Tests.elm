module Tests exposing (all)

import Expect
import Test exposing (..)


all : Test
all =
    describe "empty" [ test "nothing" <| \_ -> Expect.equal 0 0 ]
