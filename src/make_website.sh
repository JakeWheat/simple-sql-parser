#! /bin/sh

set -x
cd ..
# index
pandoc --from=markdown --to=html src/index.txt -o index.html -c main.css --title=simple-sql-parser --toc
pandoc --from=markdown --to=html src/supported_sql.txt -o supported_sql.html -c main.css '--title=simple-sql-parser supported SQL' --toc
# tpch sql file
# pandoc src/tpch.sql -s --highlight-style kate -o tpch.sql.html
# rendered test cases
runhaskell -i../trunk:../trunk/tools src/RenderTestCases > src/test_cases.txt
pandoc --from=markdown --to=html src/test_cases.txt -o test_cases.html -c main.css '--title=simple-sql-parser examples/test cases' --toc
# haddock
cd ../trunk
cabal configure && cabal haddock
rm -Rf ../gh-pages/haddock/
mkdir ../gh-pages/haddock/
cp -R dist/doc/html/simple-sql-parser/* ../gh-pages/haddock/
