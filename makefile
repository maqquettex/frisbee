all:
	stack build --fast
	stack exec frisbee-exe

tests:
	timeout 5 stack test --fast