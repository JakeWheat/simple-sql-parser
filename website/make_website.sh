#! /bin/sh

set -e

# todo: check this is run from the project root and not the website/
# dir

mkdir -p build
cp website/main.css build
cp website/ocean.css build

# index
asciidoctor website/index.asciidoc -o - | runhaskell website/AddLinks.lhs > build/index.html
#pandoc --from=markdown --to=html website/index.txt -o build/index.html -c main.css --title=simple-sql-parser --toc
asciidoctor website/supported_sql.asciidoc -o - | runhaskell website/AddLinks.lhs > build/supported_sql.html


#pandoc --from=markdown --to=html website/supported_sql.txt -o build/supported_sql.html -c main.css '--title=simple-sql-parser supported SQL' --toc



# tpch sql file
# pandoc src/tpch.sql -s --highlight-style kate -o tpch.sql.html
# rendered test cases
runhaskell -package-db=.cabal-sandbox/x86_64-linux-ghc-7.10.2-packages.conf.d -i:tools website/RenderTestCases.lhs > build/test_cases.asciidoc

#pandoc --from=markdown --to=html build/test_cases.asciidoc -o build/test_cases.html -c main.css '--title=simple-sql-parser examples/test cases' --toc
asciidoctor build/test_cases.asciidoc -o - | \
    sed -e "s/max-width:62\.5em//g" \
        > build/test_cases.html
# TODO: reduce the text size on the test cases page
# TODO: use scrollbars inside the tables
# TODO: make the tables autowidth
#        -e "s/(code.*)font-size:1em/\1font-size:0.8em/g" 

rm build/test_cases.asciidoc
# haddock
cabal haddock
rm -Rf build/haddock
mkdir build/haddock/
cp -R dist/doc/html/simple-sql-parser/* build/haddock/
