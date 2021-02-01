.PHONY: build repl run

build: 
	stack build

repl:
	stack ghci --ghc-options -XOverloadedStrings

run:
	stack run
