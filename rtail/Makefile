.PHONY: all cabal cabal-dev clean deps

all : deps cabal-dev/bin/*

clean :
	rm -r cabal-dev
	rm -r dist

deps : cabal-dev

cabal-dev/bin/* : *.hs
	cabal-dev install

cabal-dev: cabal
	@echo "Checking for cabal-dev..."
	@(cabal-dev --numeric-version > /dev/null || cabal install cabal-dev)

cabal:
	@echo "Checking for cabal..."
	@(cabal --numeric-version > /dev/null || \
		(echo "`cabal` not found. do you have the haskell platform installed?"; \
		 exit 1))
