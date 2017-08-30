#!/usr/bin/env bash

set -e

elm-doc-test
elm-test tests/Doc/**/*.elm tests/HMACTests.elm tests/SHA2ShortTests.elm
