#include <iostream>
#include <fstream>
#include "halt_algorithm.cpp"

//Mark Boady
//CS 270 - Drexel University
//The Halting Problem

//This program tells you if the program halts.
//Assuming our algorithm works!
int main(int argc, char* argv[])
{
	if(argc!=3)
	{
		std::cout << "Usage: halt program_code input_file" << std::endl;
		return 0;
	}
	std::ifstream * prog = new std::ifstream(argv[1]);
	std::ifstream * input = new std::ifstream(argv[2]);
	if(will_halt(prog,input))
	{
		std::cout << "The Program will halt." << std::endl;
	}else
	{
		std::cout << "The Program will not halt." << std::endl;
	}
	return 0;
}