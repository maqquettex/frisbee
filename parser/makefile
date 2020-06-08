all: build exe

build: fast

fast:
	stack build --fast

exe:
	stack exec frisbee-exe

alex:
	rm -f src/Tokens.hs
	alex src/Tokens.x -o src/Tokens.hs