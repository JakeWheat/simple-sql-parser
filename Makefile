
# quick makefile to document how to do the various tasks

# there is no real reason to actually use the makefile except for a
# very small amount of convenience

.PHONY : init
init :
	cabal sandbox init
	cabal install happy
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests

.PHONY : build
build :
	cabal build

.PHONY : test
test : build
	dist/build/Tests/Tests --hide-successes

.PHONY : website
website :
	website/make_website.sh

.PHONY : clean
clean :
	cabal clean
	cabal sandbox delete
	rm -Rf build/

