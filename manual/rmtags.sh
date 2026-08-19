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
    -E -e 's|<text:soft-page-break */>||g' \
    -E -e 's|text:use-soft-page-breaks="true"|text:use-soft-page-breaks="false"|g' \
    -E -e 's|<config:config-item config:name="Rsid" config:type="int">[^<]*</config:config-item>||g' \
    -E -e 's|<config:config-item config:name="RsidRoot" config:type="int">[^<]*</config:config-item>||g' \
    -E -e 's|<config:config-item config:name="PrinterName" config:type="string">[^<]*</config:config-item>||g' \
    -E -e 's|<config:config-item config:name="PrinterName" config:type="string"/>||g' \
    -E -e 's|<config:config-item config:name="PrinterSetup" config:type="base64Binary">[^<]*</config:config-item>||g' \
    -E -e 's|<config:config-item-map-indexed config:name="Views">.*</config:config-item-map-indexed>||g' \
    -E -e 's|<config:config-item config:name="PrinterSetup" config:type="base64Binary"/>||g' \
    -E -e 's|<meta:document-statistic[^>]*/>||g' \
    -E -e 's|<meta:editing-duration>.*</meta:editing-duration>||g' \
    -E -e 's|<meta:editing-cycles>.*</meta:editing-cycles>||g' \
    -E -e 's|<office:scripts>.*</office:scripts>||' \
    "${1:-/dev/stdin}"

