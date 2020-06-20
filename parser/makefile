.PHONY: test fast exe

all: fast exe
asm: fast exe-asm

test:
	stack test --fast

build: fast

fast:
	stack build --fast

exe:
	stack exec frisbee-exe

exe-asm:
	stack exec frisbee-exe asm

alex:
	rm -f src/Tokens.hs
	alex src/Tokens.x -o src/Tokens.hs