all: build

build:
	cabal sandbox init
	cabal install
	cabal build

dist: clean build

clean:
	rm -rf ./dist/ ./.cabal-sandbox/ ./cabal.sandbox.config
