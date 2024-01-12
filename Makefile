
# quick makefile to document how to do the various tasks

# there is no real reason to actually use the makefile except for a
# very small amount of convenience, apart from the website build

.PHONY : build
build :
	cabal build --enable-tests -fparserexe

.PHONY : test
test :
	cabal run test:Tests -- --hide-successes --ansi-tricks=false


.PHONY : test-coverage
test-coverage :
	cabal test --enable-coverage

.PHONY : clean
clean :
	cabal clean
	rm -Rf build/

.PHONY : parserexe
parserexe :
	cabal build -fparserexe SimpleSqlParserTool


###############################################

# website

# it's a bit crap, run cabal test or make test or something at least once
# to get the website build to work

.PHONY : website
website : website-non-haddock build-haddock

.PHONY : website-non-haddock
website-non-haddock : build/main.css build/ocean.css build/index.html build/supported_sql.html \
          build/test_cases.html build/contributing.html


build/main.css : website/main.css
	mkdir -p build
	cp website/main.css build

build/ocean.css : website/ocean.css
	mkdir -p build
	cp website/ocean.css build

build/index.html : website/index.asciidoc website/AddLinks.hs
	asciidoctor website/index.asciidoc -o - | cabal -v0 exec runhaskell website/AddLinks.hs > build/index.html

build/supported_sql.html : website/supported_sql.asciidoc website/AddLinks.hs
	asciidoctor website/supported_sql.asciidoc -o - | cabal -v0 exec runhaskell website/AddLinks.hs > build/supported_sql.html

build/contributing.html : website/contributing.asciidoc website/AddLinks.hs
	asciidoctor website/contributing.asciidoc -o - | cabal -v0 exec runhaskell website/AddLinks.hs > build/contributing.html

build/test_cases.html : website/RenderTestCases.hs
	cabal -v0 exec runhaskell -- --ghc-arg=-package=pretty-show -itools website/RenderTestCases.hs > build/test_cases.asciidoc
	asciidoctor build/test_cases.asciidoc -o - | \
	    sed -e "s/max-width:62\.5em//g" > build/test_cases.html
	# TODO: reduce the text size on the test cases page
	# TODO: use scrollbars inside the tables
	# TODO: make the tables autowidth
	#        -e "s/(code.*)font-size:1em/\1font-size:0.8em/g"
	rm build/test_cases.asciidoc
        # the tests don't render right if the TestCases aren't all at the same level
        # of group nesting, which should be fixed - if this isn't the case, it
        # will silently not render some of the tests

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
