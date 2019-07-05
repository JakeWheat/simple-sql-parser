
# quick makefile to document how to do the various tasks

# there is no real reason to actually use the makefile except for a
# very small amount of convenience

.PHONY : init
init :
	cabal v1-sandbox init
	cabal v1-install happy
	cabal v1-install --only-dependencies --enable-tests
	cabal v1-configure --enable-tests

.PHONY : build
build :
	cabal v1-build

.PHONY : test
test : build
	dist/build/Tests/Tests --hide-successes

.PHONY : website
website :
	website/make_website.sh

.PHONY : clean
clean :
	cabal v1-clean
	cabal v1-sandbox delete
	rm -Rf build/

