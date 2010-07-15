#include <Windows.h>
#include <gl/glut.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <cmath>
#include "loadbmp.h"

#define MAX_MAZE_SIZE 128

#define SENSITIVITY 0.004

#define MOVE_SPEED 2.5f

#define PI 3.14159268f

using namespace std;

int mazeX, mazeY;

int maze[MAX_MAZE_SIZE][MAX_MAZE_SIZE];

bool wayExists;

bool movingFwd, movingBack, movingLeft, movingRight;

class Point 
{
public:
	int x;
	int y;
	Point(int x, int y): x(x), y(y) {}
	Point() {}
};

float time;
float frameTime;

vector<Point> way;

int windowX, windowY;

float mouseX, mouseY;

float px, py, pz;

float wx, wy, wz;

float lx, ly, lz;

int camIndex;

bool walking;

GLuint texWood, texFloor, texWall;

void moveToCam(int index) 
{
	pz = 2 + (mazeX + mazeY) / 5;
	px = mazeX / 2.0f + sin(2 * PI * index / 10) * mazeX / 2.0f;
	py = mazeY / 2.0f + cos(2 * PI * index / 10) * mazeY / 2.0f;

	mouseY = - PI / 4;
	mouseX = 2 * PI * index / 10 - PI;

	camIndex = index;
}

void nextCam() 
{
	camIndex = (camIndex + 1) % 10;
	moveToCam(camIndex);
}


void buildWay() 
{
	int steps[MAX_MAZE_SIZE][MAX_MAZE_SIZE];
	Point from[MAX_MAZE_SIZE][MAX_MAZE_SIZE];
	vector<pair<Point, Point> > *a, *b, *c;
	
	for(int y = 0; y < mazeY; y++)
		for(int x = 0; x < mazeX; x++)
			steps[x][y] = 2000000000;

	a = new vector<pair<Point, Point> >();
	b = new vector<pair<Point, Point> >();

	a->resize(mazeX * mazeY * 16);
	b->resize(mazeX * mazeY * 16);

	(*a)[0] = make_pair(Point(mazeX, mazeY), Point(0, 0));
	steps[mazeX][mazeY] = 0;

	int asize = 1;
	int bsize = 0;

	while(asize > 0) 
	{
		for(int i = 0; i < asize; i++) 
		{
			if((*a)[i].second.x < 0 || (*a)[i].second.x >= mazeX || (*a)[i].second.y < 0 || (*a)[i].second.y >= mazeY)
				continue;
			if(maze[(*a)[i].second.x][(*a)[i].second.y] == 1)
				continue;
			if(steps[(*a)[i].second.x][(*a)[i].second.y] < 2000000000)
				continue;
			if(steps[(*a)[i].second.x][(*a)[i].second.y] < steps[(*a)[i].first.x][(*a)[i].first.y] + 1)
				continue;

			from[(*a)[i].second.x][(*a)[i].second.y] = (*a)[i].first;
			steps[(*a)[i].second.x][(*a)[i].second.y] = steps[(*a)[i].first.x][(*a)[i].first.y] + 1;

			(*b)[bsize++] = make_pair((*a)[i].second, Point((*a)[i].second.x + 1, (*a)[i].second.y));
			(*b)[bsize++] = make_pair((*a)[i].second, Point((*a)[i].second.x - 1, (*a)[i].second.y));
			(*b)[bsize++] = make_pair((*a)[i].second, Point((*a)[i].second.x, (*a)[i].second.y + 1));
			(*b)[bsize++] = make_pair((*a)[i].second, Point((*a)[i].second.x, (*a)[i].second.y - 1));
		}

		c = a;
		a = b;
		b = c;

		asize = bsize;
		bsize = 0;
	}

	if(steps[mazeX - 1][mazeY - 1] < 2000000000) 
	{
		wayExists = true;
		Point p(mazeX - 1, mazeY - 1);
		do 
		{
			way.push_back(p);
			p = from[p.x][p.y];
		} 
		while(!(p.x == 0 && p.y == 0));
		way.push_back(Point(0, 0));
		reverse(way.begin(), way.end());
	} 
	else
		wayExists = false;
}

void glInit() 
{
	glClearColor(0, 0, 0, 0);
	glEnable(GL_DEPTH_TEST);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);
	glShadeModel(GL_SMOOTH);
	glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);
	glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
	glEnable(GL_TEXTURE_2D);
	glEnable(GL_NORMALIZE);
	glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

	glLightModelf(GL_LIGHT_MODEL_LOCAL_VIEWER, 1);

	GLfloat light1p[4] = {0, 0, 0, 1};
	GLfloat light1d[4] = {1.0, 1.0, 1.0, 1.0};
	GLfloat light1s[4] = {0.5, 0.5, 0.5, 1.0};
	GLfloat light1a[4] = {0, 0, 0, 1};
	glLightfv(GL_LIGHT0, GL_POSITION, light1p);
	glLightfv(GL_LIGHT0, GL_DIFFUSE, light1d);
	glLightfv(GL_LIGHT0, GL_SPECULAR, light1s);
	glLightfv(GL_LIGHT0, GL_AMBIENT, light1a);
	glLightf(GL_LIGHT0, GL_SPOT_EXPONENT, 1);
	glLightf(GL_LIGHT0, GL_QUADRATIC_ATTENUATION, 0.2);

	GLfloat material1e[4] = { 0.6f, 0.6f, 0.6f, 0.0f };
	GLfloat material1d[4] = { 1.0f, 1.0f, 1.0f, 0.0f };
	GLfloat material1s[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
	GLfloat material1a[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
	glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, material1e);
	glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, material1d);
	glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, material1s);
	glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, material1a);
	glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 1);
}

void drawMaze() 
{
	for(int x = 0; x < mazeX; x++) 
	{
		for(int y = 0; y < mazeY; y++) 
		{
			if(maze[x][y] == 0) 
			{
				glBindTexture(GL_TEXTURE_2D, texFloor);
				glBegin(GL_QUADS);
				glNormal3f(0, 0, 1);
				glTexCoord2f(0, 0); glVertex3f(x,     y,     0);
				glTexCoord2f(1, 0); glVertex3f(x + 1, y,     0);
				glTexCoord2f(1, 1); glVertex3f(x + 1, y + 1, 0);
				glTexCoord2f(0, 1); glVertex3f(x,     y + 1, 0);
				glEnd();
			} 
			else 
			{
				glBindTexture(GL_TEXTURE_2D, texWall);
				glBegin(GL_QUADS);
				glNormal3f(0, 0, 1);
				glTexCoord2f(0, 0); glVertex3f(x,     y,     1);
				glTexCoord2f(1, 0); glVertex3f(x + 1, y,     1);
				glTexCoord2f(1, 1); glVertex3f(x + 1, y + 1, 1);
				glTexCoord2f(0, 1); glVertex3f(x,     y + 1, 1);

				glNormal3f(1, 0, 0);
				glTexCoord2f(0, 0); glVertex3f(x + 1, y,     0);
				glTexCoord2f(0, 1); glVertex3f(x + 1, y,     1);
				glTexCoord2f(1, 1); glVertex3f(x + 1, y + 1, 1);
				glTexCoord2f(1, 0); glVertex3f(x + 1, y + 1, 0);

				glNormal3f(-1, 0, 0);
				glTexCoord2f(0, 0); glVertex3f(x, y,     0);
				glTexCoord2f(0, 1); glVertex3f(x, y,     1);
				glTexCoord2f(1, 1); glVertex3f(x, y + 1, 1);
				glTexCoord2f(1, 0); glVertex3f(x, y + 1, 0);

				glNormal3f(0, 1, 0);
				glTexCoord2f(1, 0); glVertex3f(x,     y + 1, 0);
				glTexCoord2f(0, 0); glVertex3f(x + 1, y + 1, 0);
				glTexCoord2f(0, 1); glVertex3f(x + 1, y + 1, 1);
				glTexCoord2f(1, 1); glVertex3f(x,     y + 1, 1);

				glNormal3f(0, -1, 0);
				glTexCoord2f(1, 0); glVertex3f(x,     y, 0);
				glTexCoord2f(0, 0); glVertex3f(x + 1, y, 0);
				glTexCoord2f(0, 1); glVertex3f(x + 1, y, 1);
				glTexCoord2f(1, 1); glVertex3f(x,     y, 1);
				glEnd();
			}
		}
	}
}

void drawArrow() 
{
	glBegin(GL_TRIANGLES);
	glNormal3f(0, 0, 1);
	glTexCoord2d(0, 0.5); glVertex3f(-0.25, 0,   0.3);
	glTexCoord2d(0.5, 1); glVertex3f(0,    0.25, 0.3);
	glTexCoord2d(1, 0.5); glVertex3f(0.25,  0,   0.3);
	glEnd();
	glBegin(GL_QUADS);
	glNormal3f(0, 0, 1);
	glTexCoord2d(0.25, 0.5); glVertex3f(-0.125, 0, 0.3);
	glTexCoord2d(0.75, 0.5); glVertex3f(0.125, 0, 0.3);
	glTexCoord2d(0.75, 0.0); glVertex3f(0.125, -0.25, 0.3);
	glTexCoord2d(0.25, 0.0); glVertex3f(-0.125, -0.25, 0.3);

	glNormal3f(0, -1, 0);
	glTexCoord2d(0.25, 0.0); glVertex3f(-0.125, -0.25, 0.3);
	glTexCoord2d(0.75, 0.0); glVertex3f(0.125, -0.25, 0.3);
	glTexCoord2d(0.75, 0.0); glVertex3f(0.125, -0.25, 0.2);
	glTexCoord2d(0.25, 0.0); glVertex3f(-0.125, -0.25, 0.2);

	glNormal3f(1, 0, 0);
	glTexCoord2d(0.75, 0.5); glVertex3f(0.125, 0, 0.3);
	glTexCoord2d(0.75, 0.0); glVertex3f(0.125, -0.25, 0.3);
	glTexCoord2d(0.75, 0.0); glVertex3f(0.125, -0.25, 0.2);
	glTexCoord2d(0.75, 0.5); glVertex3f(0.125, 0, 0.2);

	glNormal3f(-1, 0, 0);
	glTexCoord2d(0.25, 0.0); glVertex3f(-0.125, -0.25, 0.3);
	glTexCoord2d(0.25, 0.5); glVertex3f(-0.125, 0, 0.3);
	glTexCoord2d(0.25, 0.5); glVertex3f(-0.125, 0, 0.2);
	glTexCoord2d(0.25, 0.0); glVertex3f(-0.125, -0.25, 0.2);

	glNormal3f(-1, 1, 0);
	glTexCoord2d(0, 0.5); glVertex3f(-0.25, 0,   0.3);
	glTexCoord2d(0.5, 1); glVertex3f(0,    0.25, 0.3);
	glTexCoord2d(0.5, 1); glVertex3f(0,    0.25, 0.2);
	glTexCoord2d(0, 0.5); glVertex3f(-0.25, 0,   0.2);

	glNormal3f(1, 1, 0);
	glTexCoord2d(0.5, 1); glVertex3f(0,    0.25, 0.3);
	glTexCoord2d(1, 0.5); glVertex3f(0.25,  0,   0.3);
	glTexCoord2d(1, 0.5); glVertex3f(0.25,  0,   0.2);
	glTexCoord2d(0.5, 1); glVertex3f(0,    0.25, 0.2);

	glNormal3f(0, -1, 0);
	glTexCoord2d(1, 0.5); glVertex3f(0.25,  0,   0.3);
	glTexCoord2d(0, 0.5); glVertex3f(-0.25, 0,   0.3);
	glTexCoord2d(0, 0.5); glVertex3f(-0.25, 0,   0.2);
	glTexCoord2d(1, 0.5); glVertex3f(0.25,  0,   0.2);

	glEnd(); 
}

void drawWay() 
{
	glBindTexture(GL_TEXTURE_2D, texWood);
	for(int i = 0; i < way.size() - 1; i++) 
	{
		glPushMatrix();
		glTranslatef(way[i].x + 0.5, way[i].y + 0.5, 0);
		glRotatef(- (180.0f / PI) * atan2((float) way[i + 1].x - way[i].x, (float) way[i + 1].y - way[i].y), 0, 0, 1);
		drawArrow();
		glPopMatrix();
	}
}

void drawLight() 
{
	float r = 0.03;
	glDisable(GL_LIGHTING);
	glDisable(GL_TEXTURE_2D);
	glColor4f(1, 1, 1, 1);
	glBegin(GL_TRIANGLES);
	glVertex3f(lx - r, ly - r, lz + r);
	glVertex3f(lx + r, ly - r, lz + r);
	glVertex3f(lx, ly + r, lz + r);

	glVertex3f(lx - r, ly - r, lz + r);
	glVertex3f(lx + r, ly - r, lz + r);
	glVertex3f(lx, ly, lz - r);

	glVertex3f(lx + r, ly - r, lz + r);
	glVertex3f(lx, ly + r, lz + r);
	glVertex3f(lx, ly, lz - r);

	glVertex3f(lx, ly + r, lz + r);
	glVertex3f(lx - r, ly - r, lz + r);
	glVertex3f(lx, ly, lz - r);
	glEnd();
	glEnable(GL_LIGHTING);
	glEnable(GL_TEXTURE_2D);
}

void displayFunc() 
{
	static unsigned long prevTickCount = GetTickCount() - 1;
	unsigned long newTickCount = GetTickCount();
	frameTime = (newTickCount - prevTickCount) / 1000.0f;
	time += frameTime;
	prevTickCount = newTickCount;

	if(movingFwd) 
	{
		px += MOVE_SPEED * sin(mouseX) * cos(mouseY) * frameTime;
		py += MOVE_SPEED * cos(mouseX) * cos(mouseY) * frameTime;
		pz += MOVE_SPEED * sin(mouseY) * frameTime;
	}
	if(movingBack) 
	{
		px -= MOVE_SPEED * sin(mouseX) * cos(mouseY) * frameTime;
		py -= MOVE_SPEED * cos(mouseX) * cos(mouseY) * frameTime;
		pz -= MOVE_SPEED * sin(mouseY) * frameTime;
	}
	if(movingLeft) 
	{
		px += - MOVE_SPEED * cos(mouseX) * frameTime;
		py +=   MOVE_SPEED * sin(mouseX) * frameTime;
	}
	if(movingRight) 
	{
		px +=   MOVE_SPEED * cos(mouseX) * frameTime;
		py += - MOVE_SPEED * sin(mouseX) * frameTime;
	}

	if(pz < 1.2)
		pz = 1.2;

	if(wayExists) 
	{
		int tt = (int) time;
		float d = time - tt;
		tt = tt % (way.size() - 1);
		wx = way[tt].x + (way[tt + 1].x - way[tt].x) * d + 0.5;
		wy = way[tt].y + (way[tt + 1].y - way[tt].y) * d + 0.5;
		wz = 0.8;
		if(walking) 
		{
			px = wx;
			py = wy;
			pz = wz;
		}
	}

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(70.0, ((float) windowX) / windowY, 0.1, 200);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();

	gluLookAt(px, py, pz, px + sin(mouseX) * cos(mouseY), py + cos(mouseX) * cos(mouseY), pz + sin(mouseY), 0, 0, 1);

	// place light
	lx = mazeX / 2.0f + sin(10 * time / (mazeX + mazeY)) * mazeX / 2.0f;
	ly = mazeY / 2.0f + cos(10 * time / (mazeX + mazeY)) * mazeY / 2.0f;
	lz = 2;
	GLfloat lp[4] = {lx, ly, lz, 1};
	glLightfv(GL_LIGHT0, GL_POSITION, lp);
	drawLight();

	drawMaze();
	if(wayExists)
		drawWay();

	glutSwapBuffers();
}

void reshapeFunc(int width, int height) 
{
	glViewport(0, 0, width, height);
	windowX = width;
	windowY = height;
}

void idleFunc() 
{
	glutPostRedisplay();
}

void keyDownFunc(unsigned char key, int x, int y) 
{
	if(key == 27)
		exit(0);
	if(key == 'w')
		movingFwd = true;
	if(key == 's')
		movingBack = true;
	if(key == 'a')
		movingLeft = true;
	if(key == 'd')
		movingRight = true;
	if(key >= '0' && key <= '9')
		moveToCam(key - '0');
	if(key == 32)
		nextCam();
	if(key == '\t')
		walking = !walking;
}

void keyUpFunc(unsigned char key, int x, int y) 
{
	if(key == 'w')
		movingFwd = false;
	if(key == 's')
		movingBack = false;
	if(key == 'a')
		movingLeft = false;
	if(key == 'd')
		movingRight = false;
}

void mouseFunc(int x, int y) 
{
	if(!(x == windowX / 2 && y == windowY / 2)) 
	{
		mouseX += (x - windowX / 2) * SENSITIVITY;
		mouseY -= (y - windowY / 2) * SENSITIVITY;
		if(mouseY > PI * 0.47)
			mouseY = PI * 0.47;
		if(mouseY < PI * -0.47)
			mouseY = PI * -0.47;
		glutWarpPointer(windowX / 2, windowY / 2);
	}
}

int main(int argc, char** argv) 
{
	if(argc != 2)
		cout << "задайте имя файла лабиринта в командной строке" << endl;
	else 
	{
		ifstream f;
		f.open(argv[1]);
		if(!f.is_open()) 
		{
			cout << "не могу открыть файл \"" << argv[1] << "\"" << endl;
			return 1;
		}
		f >> mazeX >> mazeY;
		for(int y = 0; y < mazeY; y++)
			for(int x = 0; x < mazeX; x++)
				f >> maze[x][y];
		f.close();

		buildWay();
		
		glutInit(&argc, argv);
		glutInitDisplayMode(GLUT_DOUBLE | GLUT_DEPTH | GLUT_RGB | GLUT_ALPHA);
		glutInitWindowSize(640, 480);
		glutCreateWindow("task3");
		glutDisplayFunc(displayFunc);
		glutReshapeFunc(reshapeFunc);
		glutIdleFunc(idleFunc);
		glutIgnoreKeyRepeat(1);
		glutKeyboardFunc(keyDownFunc);
		glutKeyboardUpFunc(keyUpFunc);
		glutPassiveMotionFunc(mouseFunc); 
		glutSetCursor(GLUT_CURSOR_NONE);

		glInit();


		IMAGE img;
		glPixelStorei(GL_UNPACK_ALIGNMENT,1);
	
		LoadBMP("floor.bmp", &img);
		glGenTextures(1, &texFloor);
		glBindTexture(GL_TEXTURE_2D, texFloor);
		gluBuild2DMipmaps(GL_TEXTURE_2D, 3, img.width, img.height, GL_RGB, GL_UNSIGNED_BYTE, img.data);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);

		LoadBMP("wood.bmp", &img);
		glGenTextures(1, &texWood);
		glBindTexture(GL_TEXTURE_2D, texWood);
		gluBuild2DMipmaps(GL_TEXTURE_2D, 3, img.width, img.height, GL_RGB, GL_UNSIGNED_BYTE, img.data);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
		
		LoadBMP("up.bmp", &img);
		glGenTextures(1, &texWall);
		glBindTexture(GL_TEXTURE_2D, texWall);
		gluBuild2DMipmaps(GL_TEXTURE_2D, 3, img.width, img.height, GL_RGB, GL_UNSIGNED_BYTE, img.data);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);

		glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

		time = 0;

		px = 0; 
		py = 0;
		pz = 5;

		lx = mazeX / 2;
		ly = mazeY / 2;
		lz = 5;

		walking = false;

		camIndex = 0;

		glutMainLoop();
	}
	return 0;
}