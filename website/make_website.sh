#! /bin/sh

set -x
set -e

# todo: check this is run from the project root and not the website/
# dir

mkdir -p build
cp website/main.css build
cp website/ocean.css build

# index
pandoc --from=markdown --to=html website/index.txt -o build/index.html -c main.css --title=simple-sql-parser --toc
pandoc --from=markdown --to=html website/supported_sql.txt -o build/supported_sql.html -c main.css '--title=simple-sql-parser supported SQL' --toc
# tpch sql file
# pandoc src/tpch.sql -s --highlight-style kate -o tpch.sql.html
# rendered test cases
runhaskell -package-db=.cabal-sandbox/x86_64-linux-ghc-7.10.2-packages.conf.d -i:tools website/RenderTestCases.lhs > build/test_cases.txt
pandoc --from=markdown --to=html build/test_cases.txt -o build/test_cases.html -c main.css '--title=simple-sql-parser examples/test cases' --toc
rm build/test_cases.txt
# haddock
cabal haddock
rm -Rf build/haddock
mkdir build/haddock/
cp -R dist/doc/html/simple-sql-parser/* build/haddock/
