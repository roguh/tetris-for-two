JS_BUILD=main.js
elm-make --warn Main.elm --output=$JS_BUILD
closure-compiler --js $JS_BUILD --compilation_level ADVANCED_OPTIMIZATIONS --js_output_file compiled.$JS_BUILD 
closure-compiler --js $JS_BUILD --compilation_level ADVANCED_OPTIMIZATIONS --js_output_file compiled5s.$JS_BUILD --language_in=ECMASCRIPT5_STRICT
closure-compiler --js $JS_BUILD --compilation_level ADVANCED_OPTIMIZATIONS --js_output_file compiled5.$JS_BUILD --language_in=ECMASCRIPT5
