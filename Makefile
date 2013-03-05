name = gui

run: build
	dist/build/$(name)/$(name)

build: dist/setup-config
	cabal build --ghc-options="-O2 -Wall"

profile: dist/setup-config
	cabal build --ghc-options="-prof -auto-all -O2 -Wall"

clean:
	cabal clean

dist/setup-config:
	cabal configure

doc: dist/setup-config
	cabal haddock --executables
