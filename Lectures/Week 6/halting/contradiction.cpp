#include <iostream>
#include <fstream>
#include "halt_algorithm.cpp"
//Mark Boady
//CS 270 - Drexel University
//The Halting Problem

//This program provides a contradiction.
//If halt works, then it does the opposite of what halt predicts.
//That means halt can't be right.

int main()
{
	//We know out own name and take no inputs
	std::ifstream* self = new std::ifstream();
	self->open("cond");
	std::ifstream* input = new std::ifstream();
	input->open("cond");
	
	if(will_halt(self,input))
	{
		while(true){std::cout << "Loop!" << std::endl;}
	}else
	{
		std::cout << "I am Halting!" << std::endl;
		return 0;
	}
	
	return 0;
}