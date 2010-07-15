#include <gl/glut.h>
#include <iostream>
#include <fstream>
#include <vector>
#include "common.h"
#include "Maze.h"
#include "Wave.h"
#include "Visual.h"

using namespace std;

int main(int argc, char** argv) 
{
	if (argc != 2) 
	{
		cout << "USAGE: asgn3 MAZEFILE" << endl;
		return 1;
	} 
	else try 
	{
		vector<Point2d> way;
		Maze maze(argv[1]);
		FindWay(&way, &maze);

		glutInit(&argc, argv);
		Visualize(&maze, &way);

	} 
	catch(...) 
	{
		return 1;
	}
	return 0;
}