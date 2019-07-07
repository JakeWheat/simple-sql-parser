#! /bin/sh

set -ex

# todo: check this is run from the project root and not the website/
# dir

mkdir -p build
cp website/main.css build
cp website/ocean.css build

# index
asciidoctor website/index.asciidoc -o - | runhaskell website/AddLinks.lhs > build/index.html

asciidoctor website/supported_sql.asciidoc -o - | runhaskell website/AddLinks.lhs > build/supported_sql.html

# tpch sql file
# pandoc src/tpch.sql -s --highlight-style kate -o tpch.sql.html
# rendered test cases
# build the parserexe target first to fix the package database
runhaskell -itools website/RenderTestCases.lhs > build/test_cases.asciidoc

asciidoctor build/test_cases.asciidoc -o - | \
    sed -e "s/max-width:62\.5em//g" \
        > build/test_cases.html
# TODO: reduce the text size on the test cases page
# TODO: use scrollbars inside the tables
# TODO: make the tables autowidth
#        -e "s/(code.*)font-size:1em/\1font-size:0.8em/g"

rm build/test_cases.asciidoc
# haddock
cabal v2-haddock
rm -Rf build/haddock
mkdir build/haddock/
cp -R dist/doc/html/simple-sql-parser/* build/haddock/
