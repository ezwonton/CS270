halt: halt.cpp halt_algorithm.cpp
	g++ -o halt halt.cpp
cond: contradiction.cpp halt_algorithm.cpp
	g++ -o cond contradiction.cpp
all: halt cond
run: all
	./halt cond cond
	./cond

