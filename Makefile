.PHONY: all clean-sandbox clean deps compile compile-quick freeze lint lint-quick

all: compile lint

.cabal-sandbox:
	@cabal sandbox init

clean-sandbox:
	@rm -rf .cabal-sandbox

clean: clean-sandbox
	@cabal clean

deps: .cabal-sandbox
	@cabal install --only-dependencies -j4

compile: deps
	@cabal build

# like compile but doesn't check whether dependencies are up to date or not
compile-quick:
	@cabal build

freeze: compile
	@cabal freeze

lint: compile
	@hlint balsa.hs

# like lint but doesn't compile or check dependencies first
lint-quick:
	@hlint balsa.hs
