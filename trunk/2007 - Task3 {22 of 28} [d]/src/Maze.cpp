#include <fstream>
#include <iostream>
#include "Maze.h"

Maze::Maze(int x, int y) 
{
	this->matrix = new Matrix<int>(x + 2, y + 2);
	this->matrix->fill(1);
}

Maze::Maze(std::string fileName) 
{
	std::ifstream f;
	f.open(fileName.c_str());
	if (!f.is_open()) 
	{
		std::cout << "File \"" << fileName << "\" not found" << std::endl;
		throw std::runtime_error("");
	}
	Point2d size;
	f >> size.x >> size.y;
	this->matrix = new Matrix<int>(size.x + 2, size.y + 2);
	this->matrix->fill(1);

	for (int y = 0; y < size.y; y++)
		for (int x = 0; x < size.x; x++)
			f >> (*this->matrix)[x + 1][y + 1];

	f.close();
}

Maze::~Maze() 
{
	delete matrix;
}

