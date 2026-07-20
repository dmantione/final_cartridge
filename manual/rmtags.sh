sed -E -e 's/style:font-name-complex=".*" ?//' \
    -E -e 's/style:font-size-complex=".*" ?//' \
    -E -e 's/style:font-weight-complex=".*" ?//' \
    -E -e 's/style:language-complex=".*" ?//' \
    -E -e 's/style:country-complex=".*" ?//' \
    -E -e 's/style:language-comples=".*" ?//' \
    -E -e 's/style:font-name-asian=".*" ?//' \
    -E -e 's/style:font-size-asian=".*" ?//' \
    -E -e 's/style:font-weight-asian=".*" ?//' \
    -E -e 's/style:language-asian=".*" ?//' \
    -E -e 's/style:country-asian=".*" ?//' \
    -E -e 's/style:language-asian=".*" ?//' \
    $1
