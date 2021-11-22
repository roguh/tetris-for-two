#!/bin/sh
JS_BUILD=./main.js
set -x
mkdir -p "$(dirname "$JS_BUILD")"
elm-make --warn Main.elm --output=$JS_BUILD
