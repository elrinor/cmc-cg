#ifndef __MAZE_H__
#define __MAZE_H__
#include <string>
#include "common.h"

class Maze 
{
private:
	Matrix<int>* matrix;

	Maze& operator= (const Maze&);
	Maze(const Maze&);

public:
	int getX() 
	{
		return this->matrix->getX() - 2;
	}

	int getY() 
	{
		return this->matrix->getY() - 2;
	}

	int* operator[] (int index) 
	{
		return (*this->matrix)[index + 1] + 1;
	}

	int at(int x, int y) 
	{
		return (*this->matrix)[x + 1][y + 1];
	}

	int at(Point2d p) 
	{
		return at(p.x, p.y);
	}

	Maze(int x, int y);

	Maze(std::string fileName);

	~Maze();
};

#endif