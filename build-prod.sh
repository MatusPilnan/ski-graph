#!/bin/sh

set -e

js="public/elm.js"
min="public/elm.min.js"

elm make --optimize --output=$js "$@"

./node_modules/uglifyjs/bin/uglify-js $js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | ./node_modules/uglify-js/bin/uglifyjs --mangle --output $min

echo "Compiled size: $(wc $js -c) bytes  ($js)"
echo "Minified size: $(wc $min -c) bytes  ($min)"
echo "Gzipped size:  $(gzip $min -c | wc -c) bytes"

./node_modules/tailwindcss/lib/cli --minify -o public/styles.min.css
