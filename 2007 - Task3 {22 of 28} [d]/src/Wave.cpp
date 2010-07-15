#include <vector>
#include <limits>
#include <algorithm>
#include "wave.h"

#ifdef max
#  undef max
#endif

class WayFinder 
{
private:
	Maze* maze;
	Matrix<int> dist;
	Matrix<Point2d> last;
	//std::vector<Point2d> *way;
	std::vector<Point2d> *wave0, *wave1, *wavex;
public:

	WayFinder(Maze* maze): maze(maze), dist(maze->getX(), maze->getY()), last(maze->getX(), maze->getY()) 
	{
		dist.fill(std::numeric_limits<int>::max());
	}

	void step(Point2d from, Point2d to) 
	{
		if (maze->at(to) == 0 && dist[to.x][to.y] > dist[from.x][from.y] + 1) 
		{
			this->wave1->push_back(to);
			this->last[to.x][to.y] = from;
			this->dist[to.x][to.y] = this->dist[from.x][from.y] + 1;
		}
	}

	void findWay(std::vector<Point2d>* way) 
	{
		//this->way = way;
		this->wave0 = new std::vector<Point2d>;
		this->wave1 = new std::vector<Point2d>;

		wave0->push_back(Point2d(0, 0));
		last[0][0] = Point2d(0, 0);
		dist[0][0] = 0;
		while (wave0->size() != 0) 
		{
			for (std::vector<Point2d>::iterator i = wave0->begin(); i != wave0->end(); i++) 
			{
				step(*i, Point2d(i->x + 1, i->y));
				step(*i, Point2d(i->x - 1, i->y));
				step(*i, Point2d(i->x, i->y + 1));
				step(*i, Point2d(i->x, i->y - 1));
			}

			wavex = wave1;
			wave1 = wave0;
			wave0 = wavex;
			wave1->clear();
		}

		if (dist[maze->getX() - 1][maze->getY() - 1] < std::numeric_limits<int>::max()) 
		{
			Point2d pos(maze->getX() - 1, maze->getY() - 1);
			way->push_back(pos);
			while (!(pos.x == 0 && pos.y == 0)) 
			{
				pos = last[pos.x][pos.y];
				way->push_back(pos);
			}
			std::reverse(way->begin(), way->end());
		}

		delete this->wave0;
		delete this->wave1;
	}
};


void FindWay(std::vector<Point2d>* way, Maze* maze) 
{
	WayFinder finder(maze);
	finder.findWay(way);
}
