
# quick makefile to document how to do the various tasks

# there is no real reason to actually use the makefile except for a
# very small amount of convenience

.PHONY : build
build :
	cabal v2-build --enable-tests -fparserexe

.PHONY : test
test :
	cabal v2-run test:Tests -- --hide-successes --ansi-tricks=false

.PHONY : website
website :
	website/make_website.sh

.PHONY : clean
clean :
	cabal v2-clean
	rm -Rf build/

.PHONY: parserexe
parserexe :
	cabal v2-build -fparserexe SimpleSqlParserTool
