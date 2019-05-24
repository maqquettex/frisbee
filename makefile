all:
	stack build --fast
	stack exec frisbee-exe

tests:
	timeout 15 stack test --fast