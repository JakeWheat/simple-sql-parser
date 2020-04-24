
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
          build/test_cases.html


build/main.css : website/main.css
	mkdir -p build
	cp website/main.css build

build/ocean.css : website/ocean.css
	mkdir -p build
	cp website/ocean.css build

build/index.html : website/index.asciidoc website/AddLinks.lhs
	asciidoctor website/index.asciidoc -o - | cabal -v0 exec runhaskell website/AddLinks.lhs > build/index.html

build/supported_sql.html : website/supported_sql.asciidoc website/AddLinks.lhs
	asciidoctor website/supported_sql.asciidoc -o - | cabal -v0 exec runhaskell website/AddLinks.lhs > build/supported_sql.html

build/test_cases.html : website/RenderTestCases.lhs
	cabal -v0 exec runhaskell -- --ghc-arg=-package=pretty-show -itools website/RenderTestCases.lhs > build/test_cases.asciidoc
	asciidoctor build/test_cases.asciidoc -o - | \
	    sed -e "s/max-width:62\.5em//g" > build/test_cases.html
	# TODO: reduce the text size on the test cases page
	# TODO: use scrollbars inside the tables
	# TODO: make the tables autowidth
	#        -e "s/(code.*)font-size:1em/\1font-size:0.8em/g"
	rm build/test_cases.asciidoc

# works here, but not in a recipe. amazing
# GHC_VER="$(shell ghc --numeric-version)"

.PHONY : build-haddock
build-haddock :
	cabal haddock --haddock-option="--hyperlinked-source"
	# todo: handle the deps properly
	rm -Rf build/haddock
	mkdir build/haddock/

	#GHC_VER="$(shell ghc --numeric-version)"
	# wtf
	$(eval GHC_VER="$(shell ghc --numeric-version)")
	cp -R dist-newstyle/build/x86_64-linux/ghc-${GHC_VER}/simple-sql-parser-0.6.0/doc/html/simple-sql-parser/* build/haddock/
