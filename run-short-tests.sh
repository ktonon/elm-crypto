#!/usr/bin/env bash

set -e

elm-verify-examples
elm-test tests/Doc/**/*.elm tests/HMACTests.elm tests/SHA2ShortTests.elm
