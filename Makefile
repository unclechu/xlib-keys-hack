dist: clean
	cabal sandbox init
	cabal install
	cabal build

clean:
	rm -rf ./dist/ ./.cabal-sandbox/ ./cabal.sandbox.config

all: dist
