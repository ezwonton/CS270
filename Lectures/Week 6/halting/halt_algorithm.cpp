#include <iostream>
#include <fstream>

//Mark Boady
//CS 270 - Drexel University
//The Halting Problem

//This is a proof by contradiction.
//Assume there exists a program that can determine if for a given input another program halts.

//We will sacrifice some generality in this proof to make the point clearer.
//We are given a program and input file.
//Both are binary files.
bool will_halt(std::ifstream* program, std::ifstream* input);
//Just for fun, let us print out the binary
char* char_to_binary(char c);

bool will_halt(std::ifstream* program, std::ifstream* input)
{
	while(!program->eof())
	{
		char byte;
		*(program) >> byte;
		//std::cout << char_to_binary(byte);
		if(byte%2==1)
			return false;
		else
			return true;
	}
	return false;
}

char* char_to_binary(char c)
{
	char* x=new char[8];
	int pos=7;
	for(int i=0; i < 8; i++)
	{
		x[i]='0';
	}
	
	while(c>0)
	{
		if(c%2==0)
		{
			x[pos]='0';
		}else
		{
			x[pos]='1';
		}
		pos--;
		c=c/2;
	}
	return x;
}
