.DEFAULT_GOAL: default
.PHONY: default clean

default: solution

solution: solution.hs
	ghc $^

clean:
	-$(RM) solution.hi
	-$(RM) solution.o

clean-all: clean
	-$(RM) solution
