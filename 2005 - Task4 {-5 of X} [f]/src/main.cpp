#include <Windows.h>
#include <gl/glut.h>
#include <iostream>
#include <limits>
#include <fstream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <cstdlib>
#include "loadbmp.h"
#include "Vector3D.h"

#undef max
#undef min

#define SENSITIVITY 0.004

#define MOVE_SPEED 2.5f

#define PI 3.14159268f

#define BUILDING_LIST 1
#define LAND_LIST 2
#define CAR_LIST 3

using namespace std;

bool movingFwd, movingBack, movingLeft, movingRight;

float time = 0;
float frameTime;

int windowX, windowY;

float mouseX, mouseY;

Vector3D p(0, 0, 5); // player position

Vector3D l(0, 0, 0); // light position

int camIndex = 0;

GLuint texGrass, texConcrete, texFace0, texRoof0, texFace1, texRoof1, texFace2, texRoof2, texBlue, texRoof3;

struct building {
	int type;  // 0 - round, 1 - square, 2 - tetris
	Vector3D pos;
	float r, dx, dy, h, a;
	GLuint texRoof, texFace;
};

vector<building> buildings, cottages;

Vector3D vec(float x, float y, float z) {
	return Vector3D(x, y, z);
}

Vector3D vec(float x, float y) {
	return vec(x, y, 0);
}

void glNormal(Vector3D v) {
	glNormal3f(v.x, v.y, v.z);
}

void glVertex(Vector3D v) {
	glVertex3f(v.x, v.y, v.z);
}

void glTexCoord(Vector3D v) {
	glTexCoord2f(v.x, v.y);
}

void moveToCam(int index) 
{
	p.z = 7;
	p.x = sin(2 * PI * index / 10) * 10;
	p.y = cos(2 * PI * index / 10) * 10;

	mouseY = - PI / 4;
	mouseX = 2 * PI * index / 10 - PI;

	camIndex = index;
}

void nextCam() 
{
	camIndex = (camIndex + 1) % 10;
	moveToCam(camIndex);
}

void glInit() 
{
	glClearColor(0, 0, 0, 0);
	glEnable(GL_DEPTH_TEST);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);
	glShadeModel(GL_SMOOTH);
	glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
	glEnable(GL_TEXTURE_2D);
	glEnable(GL_NORMALIZE);

	GLfloat light1p[4] = {0, 0, 0, 1};
	GLfloat light1d[4] = {1.0, 1.0, 1.0, 1.0};
	GLfloat light1s[4] = {0, 0, 0, 1.0};
	GLfloat light1a[4] = {0, 0, 0, 1};
	glLightfv(GL_LIGHT0, GL_POSITION, light1p);
	glLightfv(GL_LIGHT0, GL_DIFFUSE, light1d);
	glLightfv(GL_LIGHT0, GL_SPECULAR, light1s);
	glLightfv(GL_LIGHT0, GL_AMBIENT, light1a);
	glLightf(GL_LIGHT0, GL_SPOT_EXPONENT, 1);

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

void placeRoundRoad(float x, float y, float R, float r, int N) 
{
	float K = 1;
	glBindTexture(GL_TEXTURE_2D, texConcrete);
	glEnable(GL_POLYGON_OFFSET_FILL);
	glPolygonOffset(-1, -1);
	glBegin(GL_QUAD_STRIP);
	for(int i = 0; i <= N; i++) {
		float a = 2 * PI * i / N;
		float x0 = x + R * sin(a);
		float y0 = y + R * cos(a);
		float x1 = x + (R + r) * sin(a);
		float y1 = y + (R + r) * cos(a);
		glTexCoord2f(x0 * K, y0 * K); glVertex3f(x0, y0, 0);
		glTexCoord2f(x1 * K, y1 * K); glVertex3f(x1, y1, 0);
	}
	glEnd();
	glDisable(GL_POLYGON_OFFSET_FILL);
}

void placeRoundBuilding(float x, float y, float r, float h, int N, GLuint texFace, GLuint texRoof) 
{
	float K = 1;
	glBindTexture(GL_TEXTURE_2D, texFace);
	glBegin(GL_QUAD_STRIP);
	for(int i = 0; i <= N; i++) 
	{
		float a = 2 * PI * i / N;
		glNormal3f(sin(a), cos(a), 0);
		glTexCoord2f(i / 15.0f, 0); glVertex3f(x + r * sin(a), y + r * cos(a), 0);
		glTexCoord2f(i / 15.0f, h); glVertex3f(x + r * sin(a), y + r * cos(a), h);
	}
	glEnd();
	
	float r0 = r - 0.1;
	float h0 = h - 0.1;

	glBindTexture(GL_TEXTURE_2D, texRoof);
	glBegin(GL_TRIANGLE_FAN);
	glNormal3f(0, 0, 1);
	for(int i = 0; i < N; i++) 
	{
		float a = 2 * PI * i / N;
		float dx = r0 * sin(a);
		float dy = r0 * cos(a);
		glTexCoord2f(dx * K, dy * K); glVertex3f(x + dx, y + dy, h0);
	}
	glEnd();

	glBegin(GL_QUAD_STRIP);
	for(int i = 0; i <= N; i++) 
	{
		float a = 2 * PI * i / N;
		float dx = r0 * sin(a);
		float dy = r0 * cos(a);
		glNormal3f(-sin(a), -cos(a), 0);
		glTexCoord2f(dx * K, dy * K);
		glVertex3f(x + dx, y + dy, h0);
		glVertex3f(x + dx, y + dy, h);
	}
	glEnd();

	glBegin(GL_QUAD_STRIP);
	glNormal3f(0, 0, 1);
	for(int i = 0; i <= N; i++) 
	{
		float a = 2 * PI * i / N;
		float dx0 = r0 * sin(a);
		float dy0 = r0 * cos(a);
		float dx1 = r * sin(a);
		float dy1 = r * cos(a);
		glTexCoord2f(dx0 * K, dy0 * K); glVertex3f(x + dx0, y + dy0, h);
		glTexCoord2f(dx1 * K, dy1 * K); glVertex3f(x + dx1, y + dy1, h);
	}
	glEnd();

}

void quad(Vector3D v0, Vector3D dvx, Vector3D dvy, Vector3D dt) 
{
	glBegin(GL_QUADS);
	glNormal(dvx ^ dvy);
	glTexCoord2f(0,    0   ); glVertex(v0);
	glTexCoord2f(dt.x, 0   ); glVertex(v0 + dvx);
	glTexCoord2f(dt.x, dt.y); glVertex(v0 + dvx + dvy);
	glTexCoord2f(0,    dt.y); glVertex(v0 + dvy);
	glEnd();
}

void quad(Vector3D v0, Vector3D dvx, Vector3D dvy) 
{
	glBegin(GL_QUADS);
	glNormal(dvx ^ dvy);
	Vector3D v00 = v0;
	Vector3D v01 = v0 + dvx;
	Vector3D v11 = v0 + dvx + dvy;
	Vector3D v10 = v0 + dvy;
	glTexCoord(v00); glVertex(v00);
	glTexCoord(v01); glVertex(v01);
	glTexCoord(v11); glVertex(v11);
	glTexCoord(v10); glVertex(v10);
	glEnd();
}

void placeSquareBuilding(float x, float y, float h, float dx, float dy, float a, GLuint texFace, GLuint texRoof) 
{
	glPushMatrix();
	glTranslatef(x, y, 0);
	glRotatef(a, 0, 0, 1);
	glPushMatrix();
	glScalef(dx, dy, h);
	
	glBindTexture(GL_TEXTURE_2D, texFace);
	quad(vec( 1, -1, 0), vec(0,  2, 0), vec(0, 0, 1), vec(dy, h));
	quad(vec(-1, -1, 0), vec(2,  0, 0), vec(0, 0, 1), vec(dx, h));
  quad(vec(-1,  1, 0), vec(0, -2, 0), vec(0, 0, 1), vec(dy, h));
  quad(vec( 1,  1, 0), vec(-2, 0, 0), vec(0, 0, 1), vec(dx, h));

	glPopMatrix();

	glBindTexture(GL_TEXTURE_2D, texRoof);
	quad(vec(-dx + 0.1, -dy + 0.1, h - 0.1), vec(2 * dx - 0.2, 0, 0), vec(0, 2 * dy - 0.2, 0));
	quad(vec(-dx, -dy, h), vec(2 * dx, 0, 0), vec(0, 0.1, 0));
	quad(vec(-dx,  dy, h), vec(0, -0.1, 0), vec(2 * dx, 0, 0));
	quad(vec(-dx, -dy + 0.1, h), vec(0.1, 0, 0), vec(0, 2 * dy - 0.2, 0));
	quad(vec( dx, -dy + 0.1, h), vec(0, 2 * dy - 0.2, 0), vec(-0.1, 0, 0));

	quad(vec(-dx + 0.1, -dy + 0.1, h), vec(2 * dx - 0.2, 0, 0), vec(0, 0, -0.1));
	quad(vec(dx - 0.1, -dy + 0.1, h), vec(0, 2 * dy - 0.2, 0), vec(0, 0, -0.1));
	quad(vec(dx - 0.1, dy - 0.1, h), vec(-2 * dx + 0.2, 0, 0), vec(0, 0, -0.1));
	quad(vec(-dx + 0.1, dy - 0.1, h), vec(0, -2 * dy + 0.2, 0), vec(0, 0, -0.1));

	glPopMatrix();
}

void placeCottage(float x, float y, float dx, float dy, float h, float a) {
	glPushMatrix();
	glTranslatef(x, y, 0);
	glRotatef(a, 0, 0, 1);
	glPushMatrix();
	glScalef(dx, dy, h);

	glBindTexture(GL_TEXTURE_2D, texFace2);
	quad(vec( 1, -1, 0), vec(0,  2, 0), vec(0, 0, 1), vec(dy, h));
	quad(vec(-1, -1, 0), vec(2,  0, 0), vec(0, 0, 1), vec(dx, h));
	quad(vec(-1,  1, 0), vec(0, -2, 0), vec(0, 0, 1), vec(dy, h));
	quad(vec( 1,  1, 0), vec(-2, 0, 0), vec(0, 0, 1), vec(dx, h));

	glPopMatrix();

	glBindTexture(GL_TEXTURE_2D, texRoof3);
	quad(vec(-dx, -dy, h), vec(0, 2 * dy, 0), vec(dx, 0, dx));
	quad(vec( dx,  dy, h), vec(0, -2 * dy, 0), vec(-dx, 0, dx));

	glBegin(GL_TRIANGLES);
	glNormal3f(0, 1, 0);
	glTexCoord2f(h,      -dx); glVertex3f(-dx,  dy, h);
	glTexCoord2f(h + dx, 0  ); glVertex3f(0,    dy, h + dx);
	glTexCoord2f(h,      dx ); glVertex3f(dx,   dy, h);

	glNormal3f(0, -1, 0);
	glTexCoord2f(h,      -dx); glVertex3f(-dx, -dy, h);
	glTexCoord2f(h + dx, 0  ); glVertex3f(0,   -dy, h + dx);
	glTexCoord2f(h,      dx ); glVertex3f(dx,  -dy, h);
	glEnd();

	glPopMatrix();
}


void placeTetrisBuilding(float x, float y, float dx, float dy, float h, float a, GLuint texFace, GLuint texRoof) {
	glPushMatrix();
	glTranslatef(x, y, 0);
	glRotatef(a, 0, 0, 1);
	glPushMatrix();
	glScalef(dx, dy, h);

	glBindTexture(GL_TEXTURE_2D, texFace);
	quad(vec( 1,  1, 1), vec(0, -2, 0), vec(0, 0, -1), vec(2 * dy, h));
	quad(vec( 1, -1, 1), vec(-2, 0, 0), vec(0, 0, -1), vec(2 * dx, h));
	quad(vec(-1, -1, 1), vec(0,  1, 0), vec(0, 0, -1), vec(dy, h));
	quad(vec(-1,  0, 1), vec(1,  0, 0), vec(0, 0, -1), vec(dx, h));
	quad(vec( 0,  0, 1), vec(0,  1, 0), vec(0, 0, -1), vec(dy, h));
	quad(vec( 0,  1, 1), vec(1,  0, 0), vec(0, 0, -1), vec(dx, h));
	glPopMatrix();

	glBindTexture(GL_TEXTURE_2D, texRoof);
	quad(vec(-dx + 0.1, -dy + 0.1, h - 0.1), vec(2 * dx - 0.2, 0, 0), vec(0, dy - 0.2, 0));
	quad(vec(0.1, -0.1, h - 0.1), vec(dx - 0.2, 0, 0), vec(0, dy, 0));

	quad(vec(-dx, -dy, h), vec(2 * dx, 0, 0), vec(0, 0.1, 0));
	quad(vec(-dx, -0.1, h), vec(dx + 0.1, 0, 0), vec(0, 0.1, 0));
	quad(vec(0, dy - 0.1, h), vec(dx, 0, 0), vec(0, 0.1, 0));
	quad(vec(-dx + 0.1, -dy + 0.1, h), vec(0, dy - 0.2, 0), vec(-0.1, 0, 0));
	quad(vec(0.1, 0, h), vec(0, dy - 0.1, 0), vec(-0.1, 0, 0));
	quad(vec(dx, -dy + 0.1, h), vec(0, 2 * dy - 0.2, 0), vec(-0.1, 0, 0));

	quad(vec(-dx + 0.1, -dy + 0.1, h - 0.1), vec(0, dy - 0.2, 0), vec(0, 0, 0.1));
	quad(vec(-dx + 0.1, -0.1, h - 0.1), vec(dx, 0, 0), vec(0, 0, 0.1));
	quad(vec(0.1, -0.1, h - 0.1), vec(0, dy, 0), vec(0, 0, 0.1));
	quad(vec(0.1, dy - 0.1, h - 0.1), vec(dx - 0.2, 0, 0), vec(0, 0, 0.1));
	quad(vec(dx - 0.1, dy - 0.1, h - 0.1), vec(0, -2 * dy + 0.2, 0), vec(0, 0, 0.1));
	quad(vec(dx - 0.1, -dy + 0.1, h - 0.1), vec(-2 * dx + 0.2, 0, 0), vec(0, 0, 0.1));

	glPopMatrix();
}

void placeCone(float x, float y, float r, float h, int N) {
	glBindTexture(GL_TEXTURE_2D, texBlue);
	glPushMatrix();
	glTranslatef(x, y, 0);
	glBegin(GL_QUAD_STRIP);
	for(int i = 0; i <= N; i++) {
		float a = 2 * PI * i / N;
		Vector3D v0(r * sin(a), r * cos(a), 0);
		Vector3D v1(0.6 * r * sin(a), 0.6 * r * cos(a), 0.8 * h);
		glNormal(((v1 - v0) ^ vec(0, 0, 1)) ^ (v1 - v0));
		glTexCoord2f(v0.x, v0.y); glVertex(v0);
		glTexCoord2f(v1.x, v1.y); glVertex(v1);
	}
	glEnd();

	glBegin(GL_TRIANGLE_FAN);
	glNormal3f(0, 0, 1);
	glVertex3f(0, 0, h);
	for(int i = 0; i <= N; i++) {
		float a = 2 * PI * i / N;
		Vector3D v(0.6 * r * sin(a), 0.6 * r * cos(a), 0.8 * h);
		glNormal(((v - vec(0, 0, h)) ^ vec(0, 0, 1)) ^ (v - vec(0, 0, h)));
		glTexCoord2f(v.x, v.y); glVertex(v);
	}
	glEnd();

	glPopMatrix();
}

void placeSquareRoad(float x, float y, float dx, float dy, float r, float a) 
{
	glPushMatrix();
	glTranslatef(x, y, 0);
	glRotatef(a, 0, 0, 1);

	glBindTexture(GL_TEXTURE_2D, texConcrete);
	glEnable(GL_POLYGON_OFFSET_FILL);
	glPolygonOffset(-1, -1);
	quad(vec(-dx - r, -dy - r, 0), vec(2 * (dx + r), 0, 0), vec(0, r, 0));
	quad(vec(-dx - r,  dy + r, 0), vec(0, -r, 0), vec(2 * (dx + r), 0, 0));
	quad(vec(-dx - r, -dy, 0), vec(r, 0, 0), vec(0, 2 * dy, 0));
	quad(vec( dx + r, -dy, 0), vec(0, 2 * dy, 0), vec(-r, 0, 0));
	glDisable(GL_POLYGON_OFFSET_FILL);

	glPopMatrix();
}

void placeRoad(float x0, float y0, float x1, float y1, float w) 
{
	Vector3D v0(x0, y0, 0);
	Vector3D d = vec(x1, y1, 0) - v0;
	Vector3D s = (d ^ vec(0, 0, 1)).normalize() * w / 2;
	glBindTexture(GL_TEXTURE_2D, texConcrete);
	glEnable(GL_POLYGON_OFFSET_FILL);
	glPolygonOffset(-1, -1);
	quad(v0 - s, 2 * s, d);
	glDisable(GL_POLYGON_OFFSET_FILL);
}

void drawCar(float dx, float dy, float h) {
	glPushMatrix();
	glScalef(dx, dy, h);
	glBindTexture(GL_TEXTURE_2D, texRoof0);
	quad(vec( 1, -1, 0), vec(0,  2, 0), vec(0, 0, 1), vec(dy, h));
	quad(vec(-1, -1, 0), vec(2,  0, 0), vec(0, 0, 1), vec(dx, h));
	quad(vec(-1,  1, 0), vec(0, -2, 0), vec(0, 0, 1), vec(dy, h));
	quad(vec( 1,  1, 0), vec(-2, 0, 0), vec(0, 0, 1), vec(dx, h));
	quad(vec(-1, -1, 1), vec(2, 0, 0), vec(0, 2, 0));
	glPopMatrix();
}

void drawCars() {
	for(int i = 0; i < 5; i++) {
		glPushMatrix();
		glRotatef(time * 20 + i * 5, 0, 0, 1);
		glTranslatef(6.75, 0, 0);
		glCallList(CAR_LIST);
		glPopMatrix();

		glPushMatrix();
		glRotatef(-time * 20 - i * 5, 0, 0, 1);
		glTranslatef(6.25, 0, 0);
		glCallList(CAR_LIST);
		glPopMatrix();
	}
}

void displayFunc() 
{
	static unsigned long prevTickCount = GetTickCount() - 1;
	unsigned long newTickCount = GetTickCount();
	frameTime = (newTickCount - prevTickCount) / 1000.0f;
	time += frameTime;
	prevTickCount = newTickCount;

	if(movingFwd) 
		p += MOVE_SPEED * frameTime * vec(sin(mouseX) * cos(mouseY), cos(mouseX) * cos(mouseY), sin(mouseY));
	if(movingBack) 
		p -= MOVE_SPEED * frameTime * vec(sin(mouseX) * cos(mouseY), cos(mouseX) * cos(mouseY), sin(mouseY));
	if(movingLeft) 
		p += MOVE_SPEED * frameTime * vec(-cos(mouseX), sin(mouseX), 0);
	if(movingRight) 
		p -= MOVE_SPEED * frameTime * vec(-cos(mouseX), sin(mouseX), 0);

	p.x = min(20.0f, max(-20.0f, p.x));
	p.y = min(20.0f, max(-20.0f, p.y));
	if(p.z < 1.2)
		p.z = 1.2;

	if(p.length() < 6)
		p *= 6 / p.length();

	if(p.length() > 30)
		p *= 30 / p.length();

	float daytime = fmod(time / 10, 1);
	float light = (daytime > 0.5) ? 0 : (1 - 4 * abs(daytime - 0.25));

	glClearColor(light / 1.5, light / 1.2, light, 0);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(70.0, ((float) windowX) / windowY, 0.1, 70);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();

	gluLookAt(p.x, p.y, p.z, p.x + sin(mouseX) * cos(mouseY), p.y + cos(mouseX) * cos(mouseY), p.z + sin(mouseY), 0, 0, 1);

	// place light
	l = vec(100 * cos(daytime * 2 * PI), 50, 100 * sin(daytime * 2 * PI));
	
	GLfloat lp[4] = {l.x, l.y, l.z, 1};
	glLightfv(GL_LIGHT0, GL_POSITION, lp);

	GLfloat light1d[4] = {light, light, light, 1.0};
	glLightfv(GL_LIGHT0, GL_DIFFUSE, light1d);
	

	// objects
	glCallList(LAND_LIST);
	glCallList(BUILDING_LIST);
	drawCars();

	// shadows
	if(daytime > 0.05 && daytime < 0.45) {
		float alpha = 0.4 * (1 - 5 * abs(daytime - 0.25));
		glEnable(GL_POLYGON_OFFSET_FILL);
		glPolygonOffset(-2,-2);
		glEnable(GL_STENCIL_TEST);
		glStencilFunc(GL_EQUAL, 0, 1);
		glStencilOp(GL_KEEP, GL_INCR, GL_INCR);
		glDisable(GL_LIGHTING);
		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
		glColor4f(0, 0, 0, alpha);
		glPushMatrix();
		GLfloat Projection[16]= {l.z, 0, 0, 0,   0, l.z, 0, 0,   -l.x, -l.y, 0, -1,   0, 0, 0, l.z};
		glMultMatrixf(Projection);

		glCallList(BUILDING_LIST);
		drawCars();

		glPopMatrix();
		glDisable(GL_BLEND);
		glEnable(GL_LIGHTING); 
		glDisable(GL_STENCIL_TEST);
		glDisable(GL_POLYGON_OFFSET_FILL);
	}

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
	if(key == 'w' || key == 'W')
		movingFwd = true;
	if(key == 's' || key == 'S')
		movingBack = true;
	if(key == 'a' || key == 'A')
		movingLeft = true;
	if(key == 'd' || key == 'D')
		movingRight = true;
	if(key >= '0' && key <= '9')
		moveToCam(key - '0');
	if(key == 32)
		nextCam();
}

void keyUpFunc(unsigned char key, int x, int y) 
{
	if(key == 'w' || key == 'W')
		movingFwd = false;
	if(key == 's' || key == 'S')
		movingBack = false;
	if(key == 'a' || key == 'A')
		movingLeft = false;
	if(key == 'd' || key == 'D')
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

GLuint loadTexture(const char* fileName) {
  GLuint texId;
	IMAGE img;
	LoadBMP(fileName, &img);
	glGenTextures(1, &texId);
	glBindTexture(GL_TEXTURE_2D, texId);
	gluBuild2DMipmaps(GL_TEXTURE_2D, 3, img.width, img.height, GL_RGB, GL_UNSIGNED_BYTE, img.data);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
	return texId;
}

int main(int argc, char** argv) 
{
	srand(GetTickCount());

	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_DEPTH | GLUT_RGB | GLUT_ALPHA | GLUT_STENCIL);
	glutInitWindowSize(640, 480);
	glutCreateWindow("task4");
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

	texGrass    = loadTexture("grass.bmp");
	texConcrete = loadTexture("concrete.bmp");
	texFace0    = loadTexture("face0.bmp");
	texRoof0    = loadTexture("roof0.bmp");
	texFace1    = loadTexture("face1.bmp");
	texRoof1    = loadTexture("roof1.bmp");
	texFace2    = loadTexture("face2.bmp");
	texRoof2    = loadTexture("roof2.bmp");
	texRoof3    = loadTexture("roof3.bmp");
	texBlue     = loadTexture("blue.bmp");


	for(int i = 0; i < 200; i++) {
		building b;
		b.type = 3 * rand() / RAND_MAX;
		b.pos = vec(5 - 10.0 * rand() / RAND_MAX, 5 - 10.0 * rand() / RAND_MAX, 0);
		if(b.pos.length() < 2)
			continue;
		if(b.pos.length() > 5)
			continue;
		bool breaking = false;
		for(int j = 0; j < buildings.size(); j++)
			if((buildings[j].pos - b.pos).length() < 2.0)
				breaking = true;
		if(breaking)
			continue;
		b.dx = 0.2 + 0.5 * rand() / RAND_MAX;
		b.dy = 0.2 + 0.5 * rand() / RAND_MAX;
		b.r = 0.2 + 0.5 * rand() / RAND_MAX;
		b.a = 360 * rand() / RAND_MAX;
		b.h = 0.8 + 1.5 * rand() / RAND_MAX;
		if(rand() < RAND_MAX / 3) {
			b.texFace = texFace0;
			b.texRoof = texRoof0;
		} else if(rand() < RAND_MAX / 2) {
			b.texFace = texFace1;
			b.texRoof = texRoof1;
		} else {
			b.texFace = texFace2;
			b.texRoof = texRoof2;
		}
		buildings.push_back(b);
	}

	vector<Vector3D> smallTowns;
	for(int i = 0; i < 15; i++) {
		Vector3D pos = vec(15 - 30.0 * rand() / RAND_MAX, 15 - 30.0 * rand() / RAND_MAX, 0);
		if(pos.length() < 9)
			continue;
		bool breaking = false;
		for(int j = 0; j < smallTowns.size(); j++)
			if((smallTowns[j] - pos).length() < 6)
				breaking = true;
		if(breaking)
			continue;
		smallTowns.push_back(pos);

		for(int j = 0; j < 100; j++) {
			building b;
			b.pos = pos + vec(1.5 - 3.0 * rand() / RAND_MAX, 1.5 - 3.0 * rand() / RAND_MAX, 0);
			if(pos.length() < 8)
				continue;
			bool breaking = false;
			for(int k = 0; k < cottages.size(); k++)
				if((cottages[k].pos - b.pos).length() < 0.7)
					breaking = true;
			if(breaking)
				continue;
			b.dx = 0.1 + 0.2 * rand() / RAND_MAX;
			b.dy = 0.1 + 0.2 * rand() / RAND_MAX;
			b.a = 360 * rand() / RAND_MAX;
			b.h = 0.2 + 0.5 * rand() / RAND_MAX;
			cottages.push_back(b);
		}
	}


	// Buildings
	glNewList(BUILDING_LIST, GL_COMPILE);
	placeRoundBuilding(0, 0, 0.7, 2.5, 30, texFace0, texRoof0);
	placeSquareBuilding(0.7 * cos(2 * PI / 3), 0.7 * sin(2 * PI / 3), 1.5, 0.4, 0.4, 120, texFace0, texRoof0);
	placeSquareBuilding(0.7 * cos(4 * PI / 3), 0.7 * sin(4 * PI / 3), 1.5, 0.4, 0.4, 240, texFace0, texRoof0);
	placeSquareBuilding(0.7 * cos(6 * PI / 3), 0.7 * sin(6 * PI / 3), 1.5, 0.4, 0.4, 360, texFace0, texRoof0);
	placeRoundBuilding(0, 0, 0.4, 3.5, 30, texFace0, texRoof0);
	placeCone(0, 0, 0.2, 4.5, 20);
	for(int i = 0; i < buildings.size(); i++) {
		building b = buildings[i];
		switch(b.type) {
		case 0:
			placeRoundBuilding(b.pos.x, b.pos.y, b.r, b.h, 30, texFace0, texRoof0);
			break;
		case 1:
			placeSquareBuilding(b.pos.x, b.pos.y, b.h, b.dx, b.dy, b.a, b.texFace, b.texRoof);
			break;
		case 2:
			placeTetrisBuilding(b.pos.x, b.pos.y, b.dx, b.dy, b.h, b.a, b.texFace, b.texRoof);
		default:
			break;
		}
	}
	for(int i = 0; i < cottages.size(); i++) {
		building b = cottages[i];
		placeCottage(b.pos.x, b.pos.y, b.dx, b.dy, b.h, b.a);
	}
	glEndList();

	glNewList(LAND_LIST, GL_COMPILE);
	// Grass
	glBindTexture(GL_TEXTURE_2D, texGrass);
	glBegin(GL_QUADS);
	glNormal3f(0, 0, 1);
	for(int x = -100; x < 100; x += 5) 
	{
		for(int y = -100; y < 100; y += 5) 
		{
			glTexCoord2f(x,   y  ); glVertex3f(x,   y,   0);
			glTexCoord2f(x+5, y  ); glVertex3f(x+5, y,   0);
			glTexCoord2f(x+5, y+5); glVertex3f(x+5, y+5, 0);
			glTexCoord2f(x,   y+5); glVertex3f(x,   y+5, 0);
		}
	}
	glEnd();

	// Roads
	placeRoundRoad(0, 0, 6, 1.0, 50);
	placeRoundRoad(0, 0, 1.3, 0.2, 20);
	for(int i = 0; i < buildings.size(); i++) {
		building b = buildings[i];
		switch(b.type) {
		case 0:
			placeRoundRoad(b.pos.x, b.pos.y, b.r, 0.2, 30);
			break;
		case 1:
		case 2:
			placeSquareRoad(b.pos.x, b.pos.y, b.dx, b.dy, 0.2, b.a);
			break;
		default:
			break;
		}
		float minDist = numeric_limits<float>::max();
		int n = -1;
		for(int j = i + 1; j < buildings.size(); j++) {
			float d = (b.pos - buildings[j].pos).length();
			if(d < minDist) {
				minDist = d;
				n = j;
			}
		}
		if(n != -1 && minDist < 5)
			placeRoad(b.pos.x, b.pos.y, buildings[n].pos.x, buildings[n].pos.y, 0.2);
	}
	for(int i = 0; i < buildings.size(); i++) {
		building b = buildings[i];
		if(b.pos.length() > 3) {
			Vector3D roadpos = 6 * b.pos / b.pos.length();
			placeRoad(b.pos.x, b.pos.y, roadpos.x, roadpos.y, 0.2);
		}
	}
	for(int i = 0; i < smallTowns.size(); i++) {
		float l = smallTowns[i].length();
		Vector3D dir = smallTowns[i] / l;
		placeRoundRoad(smallTowns[i].x, smallTowns[i].y, 2, 0.2, 30);
		Vector3D v0 = dir * 6;
		Vector3D v1 = dir * (l - 2);
		placeRoad(v0.x, v0.y, v1.x, v1.y, 0.2);
	}
	glEndList();


	glNewList(CAR_LIST, GL_COMPILE);
	drawCar(0.05, 0.1, 0.05);
	glEndList();

	glutMainLoop();
}

