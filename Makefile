
# quick makefile to document how to do the various tasks

# there is no real reason to actually use the makefile except for a
# very small amount of convenience, apart from the website build
# and website consistency checks

.PHONY : build
build :
	cabal build

.PHONY : test
test :
	cabal run test:Tests -- --hide-successes --ansi-tricks=false

.PHONY : test-coverage
test-coverage :
	cabal test --enable-coverage

.PHONY : clean
clean :
	cabal clean
	cd website && cabal clean
	rm -Rf build/

.PHONY : parserexe
parserexe :
	cabal build -fparserexe SimpleSQLParserTool

.PHONY : all
all : build test parserexe website

###############################################

# website

# it's a bit crap, run cabal test or make test or something at least once
# to get the website build to work

.PHONY : website
website : website-non-haddock build-haddock

.PHONY : website-non-haddock
website-non-haddock : build/main.css build/main1.css build/index.html \
          build/supported_sql.html build/test_cases.html

build/main.css : website/main.css
	mkdir -p build
	cp website/main.css build

# todo: combine main and main1, change the one bit they can't share with sed
# to create the additional main1 as part of the build
build/main1.css : website/main1.css
	mkdir -p build
	cp website/main1.css build

build/index.html : website/index.md website/template.pandoc
	mkdir -p build
	pandoc -s --template website/template.pandoc -V toc-title:"Table of contents" -c main.css -f markdown  -t html --toc=true --metadata title="Simple SQL Parser" website/index.md > build/index.html

build/supported_sql.html : website/supported_sql.md website/template.pandoc
	mkdir -p build
	pandoc -s --template website/template.pandoc -V toc-title:"Table of contents" -c main.css -f markdown  -t html --toc=true --metadata title="Simple SQL Parser supported SQL" website/supported_sql.md > build/supported_sql.html

build/test_cases.html : website/RenderTestCases.hs website/template1.pandoc
	mkdir -p build
	# no idea why not using --disable-optimisation on cabal build, but putting -O0
	# in the cabal file (and then cabal appears to say it's still using -O1
	# is faster
	cd website/ && cabal build RenderTestCases && cabal run RenderTestCases | pandoc -s -N --template template1.pandoc -V toc-title:"Simple SQL Parser test case examples" -c main1.css -f markdown  -t html --toc=true --metadata title="Simple SQL Parse test case examples" > ../build/test_cases.html

# works here, but not in a recipe. amazing
# GHC_VER="$(shell ghc --numeric-version)"

.PHONY : build-haddock
build-haddock :
	cabal haddock --haddock-option="--hyperlinked-source"
	# todo: handle the deps properly
	rm -Rf build/haddock
	mkdir build/haddock/
	$(eval GHC_VER="$(shell ghc --numeric-version)")
	$(eval SSP_VER="$(shell cat simple-sql-parser.cabal |grep -P '^version:' | awk '{print $$2}')")
	cp -R dist-newstyle/build/x86_64-linux/ghc-${GHC_VER}/simple-sql-parser-${SSP_VER}/doc/html/simple-sql-parser/* build/haddock/

# check the website pages code snippets
.PHONY : doctool
doctool :
	cabal build -fparserexe SimpleSQLParserTool
	silverbane website/index.md

.PHONY : really-all
really-all : build test parserexe website doctool

