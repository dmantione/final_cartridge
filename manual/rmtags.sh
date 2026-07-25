#!/bin/bash

set -eu

sed -z \
    -E -e 's/style:font-name-complex="[^"]*" ?//g' \
    -E -e 's/style:font-size-complex="[^"]*" ?//g' \
    -E -e 's/style:font-weight-complex="[^"]*" ?//g' \
    -E -e 's/style:language-complex="[^"]*" ?//g' \
    -E -e 's/style:country-complex="[^"]*" ?//g' \
    -E -e 's/style:font-name-asian="[^"]*" ?//g' \
    -E -e 's/style:font-size-asian="[^"]*" ?//g' \
    -E -e 's/style:font-weight-asian="[^"]*" ?//g' \
    -E -e 's/style:language-asian="[^"]*" ?//g' \
    -E -e 's/style:country-asian="[^"]*" ?//g' \
    -E -e 's|<text:soft-page-break/>||g' \
    -E -e 's|<config:config-item config:name="Rsid" config:type="int">[^<]*</config:config-item>||g' \
    -E -e 's|<config:config-item config:name="RsidRoot" config:type="int">[^<]*</config:config-item>||g' \
    -E -e 's|<office:scripts>.*</office:scripts>||' \
    "${1:-/dev/stdin}"

