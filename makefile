all:
	stack build --fast
	stack exec frisbee-exe

tests:
	stack test --fast