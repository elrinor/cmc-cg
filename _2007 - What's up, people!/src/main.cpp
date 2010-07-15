#define _CRT_SECURE_NO_DEPRECATE
#define _USE_MATH_DEFINES
#include <gl/glut.h>
#include <gl/GL.h>
#include <gl/GLU.h>
#include <gl/GLAux.h>
#include <cmath>
#include <algorithm>
#include <vector>
#include "libTexture.h"
#include "Bass.h"
#pragma comment(lib, "Bass.lib")

#define MUSIC_FILE "data/What's up, people!.mp3"

#define KNOT_SIZEPHI 256
#define KNOT_SIZEPSI 16
#define KNOT_R0 1.5f
#define KNOT_R1 0.2f
#define KNOT_A 0.5f
#define KNOT_N 1.25f
#define KNOT_MAX_PHI (8 * M_PI)
#define KNOT_MAX_PSI (2 * M_PI)

#define TRIANGLES_N 10
#define TRIANGLES_M 10

#define EXCL_N 1000

// Lame
#define SCN1_END 10.0f
// Flash
#define SCN2_END (SCN1_END + 6.75f)
// Text
#define SCN3_END (SCN2_END + 6.0f)
// Final
#define SCN4_END (SCN1_END + 80.3f)

using namespace std;

class Vector {
public:
	float x, y, z;
	Vector(float x, float y, float z) {this->x = x; this->y = y; this->z = z;}
	Vector() {}
	Vector operator+ (const Vector& a) {
		return Vector(x + a.x, y + a.y, z + a.z);
	}
	Vector operator- (const Vector& a) {
		return Vector(x - a.x, y - a.y, z - a.z);
	}
	float operator* (const Vector& a) {
		return x * a.x + y * a.y + z * a.z;
	}
	Vector operator* (float a) {
		return Vector(x * a, y * a, z * a);
	}
	Vector operator/ (float a) {
		return Vector(x / a, y / a, z / a);
	}
	Vector operator| (const Vector& a) {
		return Vector(y * a.z - z * a.y, - x * a.z + z * a.x, x * a.y - y * a.x);
	}
	float absSq() {
		return *this * *this;
	}
	float abs() {
		return sqrt(absSq());
	}
	Vector norm() {
		return *this / abs();
	}
};

void glVertex(const Vector& v) {
	glVertex3f(v.x, v.y, v.z);
}

void glTexCoord(const Vector& v) {
	glTexCoord3f(v.x, v.y, v.z);
}

void glNormal(const Vector& v) {
	glNormal3f(v.x, v.y, v.z);
}

class Polygon4 {
public:
	Vector vec[4], tex[4], nor[4];
	float d;
	void draw() {
		for(int i = 0; i < 4; i++) {
			glTexCoord(tex[i]);
			glNormal(nor[i]);
			glVertex(vec[i]);
		}
	}
};

class TextTriangle {
public:
	Vector vec[3];
	Vector dir;
	Vector pos;
	Vector axis;
	void draw() {
		for(int i = 0; i < 3; i++)
			glVertex(vec[i]);
	}
};

class Cmp {
public:
	bool operator() (const Polygon4* a, const Polygon4* b) {
		return a->d > b->d;
	}
};

class Particle {
public:
	Vector pos, dir;
	float color[4];
	float life, dlife;
	bool alive() {
		return life > 0.0f;
	}
	void move(float dt) {
		pos = pos + dir * dt;
		life -= dlife * dt;
		if(life < 0)
			life = 0.0f;
	}
};

// ************************************************************************** //
// Globals
// ************************************************************************** //
int wx, wy;

float dt, t;

HSTREAM music;
bool playing;
float bassK;

GLuint lameFloorTex, lameTex, hellNoTex, exclTex, altarTex, cubeTex, evilFloorTex, fireTex, particleTex, okTex, rustTex, designTex;

Vector camera, light;

Polygon4 knot[KNOT_SIZEPHI][KNOT_SIZEPSI];
vector<Polygon4*> sortedKnot;

vector<TextTriangle> triangles;

vector<Vector> excls;
vector<Vector> exclsNewPos;
vector<float> exclChangeTimes;

GLuint boxList, holeList;

GLuint cubeList, sphereList;

float altarPsi, altarPhi;

vector<Particle> altarFire, altarFireWork, sideFire;

// ************************************************************************** //
// Logic
// ************************************************************************** //
Vector randVector() {
	return Vector(1 - 2.0 * rand() / RAND_MAX, 1 - 2.0 * rand() / RAND_MAX, 1 - 2.0 * rand() / RAND_MAX);
}

Vector getKnotV(float u, float v) {
	Vector a;
	a.x = KNOT_R1 * cos(v) * cos(u) + KNOT_R0 * cos(u) * (1 + KNOT_A * cos(KNOT_N * u));
	a.z = (KNOT_R1 * sin(v) + KNOT_A * sin(KNOT_N * u) + KNOT_A + KNOT_R1 + 0.5);
	a.y = KNOT_R1 * cos(v) * sin(u) + KNOT_R0 * sin(u) * (1 + KNOT_A * cos(KNOT_N * u));
	return a;
}

void knotSetPolygonV(Polygon4* p, int i, float phi, float psi) {
	p->vec[i] = getKnotV(phi, psi);
	p->nor[i] = (getKnotV(phi + 0.001, psi) - p->vec[i]) | (getKnotV(phi, psi + 0.001) - p->vec[i]);
	p->tex[i] = Vector(64 * phi / KNOT_MAX_PHI, psi / KNOT_MAX_PSI, 0);
}

void preGenKnot() {
	for(int i = 0; i < KNOT_SIZEPHI; i++) {
		float phi0 = KNOT_MAX_PHI * i / KNOT_SIZEPHI;
		float phi1 = KNOT_MAX_PHI * (i + 1) / KNOT_SIZEPHI;
		for (int j = 0; j < KNOT_SIZEPSI; j++) {
			float psi0 = KNOT_MAX_PSI * j / KNOT_SIZEPSI;
			float psi1 = KNOT_MAX_PSI * (j + 1) / KNOT_SIZEPSI;
			knotSetPolygonV(&knot[i][j], 0, phi0, psi0);
			knotSetPolygonV(&knot[i][j], 1, phi1, psi0);
			knotSetPolygonV(&knot[i][j], 2, phi1, psi1);
			knotSetPolygonV(&knot[i][j], 3, phi0, psi1);
			sortedKnot.push_back(&knot[i][j]);
		}
	}
}

void knotDraw() {
	glBegin(GL_QUADS);
	for(unsigned int i = 0; i < sortedKnot.size(); i++)
		sortedKnot[i]->draw();
	glEnd();
}

void knotSortAndDraw() {
	GLfloat mDiffuse[4]  = {1.0, 1.0, 1.0, 0.5};
	glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, mDiffuse);
	glBindTexture(GL_TEXTURE_2D, lameTex);
	glEnable(GL_BLEND);
	for(unsigned int i = 0; i < sortedKnot.size(); i++)
		sortedKnot[i]->d = (sortedKnot[i]->vec[0] - camera).absSq();
	sort(sortedKnot.begin(), sortedKnot.end(), Cmp());
	knotDraw();
	glDisable(GL_BLEND);
}

void drawLight() {
	static GLUquadric* sphere = gluNewQuadric();
	glDisable(GL_TEXTURE_2D);
	glDisable(GL_LIGHTING);
	glColor3f(0.3, 0.6, 0.3);
	glPushMatrix();
	glTranslatef(light.x, light.y, light.z);
	gluSphere(sphere, 0.05f, 5, 5);
	glPopMatrix();
	glEnable(GL_LIGHTING);
	glEnable(GL_TEXTURE_2D);
}

void drawLameScene(float t) {
	camera = Vector(5 * cos(t * 2), 5 * sin(t * 2), 5);
	light = Vector(3 * sin(t * 2), 3 * cos(t * 1), 3);
	GLfloat lPosition[4]  = {light.x, light.y, light.z, 1};

	GLfloat lDiffuse[4]  = {0.4, 0.8, 0.4, 1};
	GLfloat lSpecular[4] = {0.5, 0.8, 0.5, 1};
	glLightfv(GL_LIGHT0, GL_DIFFUSE, lDiffuse);
	glLightfv(GL_LIGHT0, GL_SPECULAR, lSpecular);
	GLfloat mDiffuse[4]  = {1.0, 1.0, 1.0, 0.6};

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(80, (float) wx / wy, 0.01, 100);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	gluLookAt(camera.x, camera.y, camera.z, 0, 0, 0, 0, 0, 1);

	GLfloat fColor[4]  = {0.0, 0.1, 0.0, 1.0};

	glClearColor(fColor[0], fColor[1], fColor[2], 0.0f);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

	glEnable(GL_FOG);
	glFogfv(GL_FOG_COLOR, fColor);
	glFogf(GL_FOG_START, 7.0f);
	glFogf(GL_FOG_END, 15.0f);
	glFogi(GL_FOG_MODE, GL_LINEAR);

	// Down
	glPushMatrix();
	glScalef(1, 1, -1);
	camera.z *= -1;
	glLightfv(GL_LIGHT0, GL_POSITION, lPosition);
	drawLight();
	knotSortAndDraw();
	camera.z *= -1;
	glPopMatrix();
	glLightfv(GL_LIGHT0, GL_POSITION, lPosition);

	// Floor
	glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, mDiffuse);
	glBindTexture(GL_TEXTURE_2D, lameFloorTex);
	glEnable(GL_BLEND);
	glBegin(GL_QUADS);
	glNormal3f(0, 0, 1);
	for(int i = -100; i < 100; i++) {
		for(int j = -100; j < 100; j++) {
			glTexCoord2f(0.3 * (i    ), 0.3 * (j    )); glVertex3f(i    , j    , 0);
			glTexCoord2f(0.3 * (i + 1), 0.3 * (j    )); glVertex3f(i + 1, j    , 0);
			glTexCoord2f(0.3 * (i + 1), 0.3 * (j + 1)); glVertex3f(i + 1, j + 1, 0);
			glTexCoord2f(0.3 * (i    ), 0.3 * (j + 1)); glVertex3f(i    , j + 1, 0);
		}
	}
	glEnd();
	glDisable(GL_BLEND);

	// Shadow
	glBindTexture(GL_TEXTURE_2D, lameTex);
	glDisable(GL_DEPTH_TEST);
	glDisable(GL_LIGHTING);
	glEnable(GL_BLEND);
	glColor4f(0, 0, 0, 0.3);
	glPushMatrix();
	GLfloat proj[16] = 
		{light.z, 0, 0, 0, 
		 0, light.z, 0, 0, 
		 -light.x, -light.y, 0, -1, 
		 0, 0, 0, light.z};
	glMultMatrixf(proj);
	knotDraw();
	glPopMatrix();
	glDisable(GL_BLEND);
	glEnable(GL_LIGHTING);

	// Up
	knotSortAndDraw();
	drawLight();

	glDisable(GL_FOG);
}

float getBassK() {
	const int bassN = 48;
	static float maxBass = 1;
	static float data[1024];
	BASS_ChannelGetData(music, data, BASS_DATA_FFT1024S); 
	float bass = 0;
	for(int i = 0; i < bassN; i++)
	//for(int i = 1024 - bassN; i < 1024; i++)
		bass += sqrt(data[i] * sqrt((float)(i + 1)));
	maxBass = max(bass, maxBass);
	return bass / maxBass;
}

void drawWhiteToBlack(float progress, float transitionTime) {
	float color;
	float r = 1.0;
	float g = progress;
	float b = (progress * progress);

	static float prevBoomT = 0;
	if(bassK > 0.8 && (t - prevBoomT) > 0.03)
		prevBoomT = t;
	
	color = max(1 - (t - prevBoomT) / transitionTime, 0);

	if(progress > 0.9)
		color = (color * ((progress - 1) / 0.1));
	glClearColor(color * r, color * g, color * b, 0);
	glClear(GL_COLOR_BUFFER_BIT);
	glClearColor(0, 0, 0, 0);

	static float changePosT = t;
	static Vector pos(0, 0, 0);
	if(t - changePosT > 0.03) {
		changePosT = t;
		pos = randVector() * 0.03;
	}
	
	float a = 5 * max(0.0f, 0.2 - abs(progress - 0.5));
	if(a > 0.0f) {
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		gluPerspective(80, (float) wx / wy, 0.01, 100);
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity();
		glTranslatef(pos.x, pos.y, 0);
		glBindTexture(GL_TEXTURE_2D, okTex);
		glDisable(GL_LIGHTING);
		glEnable(GL_TEXTURE_2D);
		glEnable(GL_BLEND);
		glBegin(GL_QUADS);
		glColor4f(1.0, 1.0 - 2 * (progress - 0.3), 0.0, a);
		glTexCoord2f(0, 1); glVertex3f(-1, -1, -1);
		glTexCoord2f(1, 1); glVertex3f( 1, -1, -1);
		glTexCoord2f(1, 0); glVertex3f( 1,  1, -1);
		glTexCoord2f(0, 0); glVertex3f(-1,  1, -1);
		glEnd();
		glDisable(GL_BLEND);
		glEnable(GL_LIGHTING);
	}
}

Vector getTriangleVertex(int i, int j) {
	float a = j * 2 * M_PI / TRIANGLES_M;
	return Vector(i * cos(a), i * sin(a), 0);
}

TextTriangle getTriangle(int i0, int j0, int i1, int j1, int i2, int j2) {
	TextTriangle t;
	t.vec[0] = getTriangleVertex(i0, j0);
	t.vec[1] = getTriangleVertex(i1, j1);
	t.vec[2] = getTriangleVertex(i2, j2);
	t.pos = (t.vec[0] + t.vec[1] + t.vec[2]) / 3;
	t.vec[0] = t.vec[0] - t.pos;
	t.vec[1] = t.vec[1] - t.pos;
	t.vec[2] = t.vec[2] - t.pos;
	t.dir = (t.pos.norm() + randVector() * 0.5).norm();
	t.axis = t.dir | Vector(0, 0, 1);
	return t;
}

void initTriangles() {
	for(int i = 0; i < TRIANGLES_N; i++) {
		for(int j = 0; j < TRIANGLES_M; j++) {
			triangles.push_back(getTriangle(i, j, i + 1, j, i + 1, j + 1));
			if(i != 0)
				triangles.push_back(getTriangle(i, j, i, j + 1, i + 1, j + 1));
		}
	}
}

void drawTriangles(float t) {
	glColor3f(0, 0, 0);
	glPushMatrix();
	glScalef(0.2, 0.2, 0.2);
	for(unsigned int i = 0; i < triangles.size(); i++) {
		glPushMatrix();
		glTranslatef(5 * triangles[i].dir.x * t, 5 * triangles[i].dir.y * t, 5 * triangles[i].dir.z * t);
		glTranslatef(triangles[i].pos.x, triangles[i].pos.y, triangles[i].pos.z);
		glRotatef(- t * (320 + 10 * (i * 5 % 13)), triangles[i].axis.x, triangles[i].axis.y, triangles[i].axis.z);
		glBegin(GL_TRIANGLES);
		triangles[i].draw();
		glEnd();
		glPopMatrix();
	}
	glPopMatrix();
}

void initExcls() {
	for(int i = 0; i < EXCL_N; i++) {
		excls.push_back(randVector() * 2.0f);
		exclChangeTimes.push_back(0.2 * rand() / RAND_MAX);
	}
	exclsNewPos = excls;
}

void drawExcls(float t) {
	glEnable(GL_BLEND);
	glBindTexture(GL_TEXTURE_2D, exclTex);
	glBegin(GL_QUADS);
	for(unsigned int i = 0; i < excls.size(); i++) {
		glPushMatrix();
		if(t - exclChangeTimes[i] > 0.05) {
			exclChangeTimes[i] = t;
			exclsNewPos[i] = excls[i] + randVector() * 0.05;
		}
		float r = (1.0) * bassK;
		float g = ((0.5 + (i % 6) / 10.0f)) * bassK;
		glColor3f(r, g, 0);
		glTexCoord2f(0, 0); glVertex(exclsNewPos[i] + Vector( 0.1,  0.1, 0));
		glTexCoord2f(1, 0); glVertex(exclsNewPos[i] + Vector(-0.1,  0.1, 0));
		glTexCoord2f(1, 1); glVertex(exclsNewPos[i] + Vector(-0.1, -0.1, 0));
		glTexCoord2f(0, 1); glVertex(exclsNewPos[i] + Vector( 0.1, -0.1, 0));
		glPopMatrix();
	}
	glEnd();
	glDisable(GL_BLEND);
}

void drawText(float t) {
	glClearColor(1.0, 0.5 + bassK / 2, 0, 0);
	glClear(GL_COLOR_BUFFER_BIT);

	glDisable(GL_DEPTH_TEST);
	glDisable(GL_LIGHTING);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(80, (float) wx / wy, 0.01, 100);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	gluLookAt(0, 0, 2, 0, 0, 0, 0, 1, 0);

	static float changePosT = t;
	static Vector pos(0, 0, 0);
	if(t - changePosT > 0.05) {
		changePosT = t;
		pos = randVector() * 0.1;
	}

	drawExcls(t);

	glEnable(GL_BLEND);
	glBindTexture(GL_TEXTURE_2D, hellNoTex);
	glPushMatrix();
	glTranslatef(pos.x, pos.y, pos.z);
	glScalef(1.5, 1.5, 1.5);
	glBegin(GL_QUADS);
	glColor3f(1, 0, 0);
	glTexCoord2f(0, 0); glVertex3f(-1,  1, 0);
	glTexCoord2f(1, 0); glVertex3f( 1,  1, 0);
	glTexCoord2f(1, 1); glVertex3f( 1, -1, 0);
	glTexCoord2f(0, 1); glVertex3f(-1, -1, 0);
	glEnd();
	glPopMatrix();
	glDisable(GL_BLEND);

	drawTriangles(t);

	glEnable(GL_DEPTH_TEST);
	glEnable(GL_LIGHTING);
}

void mySolidCube(float side) {
	float r = side / 2;
	glBegin(GL_QUADS);
	glNormal3f(0, -1, 0);
	glTexCoord2f(1, 0); glVertex3f(-r, -r,  r);
	glTexCoord2f(0, 0); glVertex3f(-r, -r, -r);
	glTexCoord2f(0, 1); glVertex3f( r, -r, -r);
	glTexCoord2f(1, 1); glVertex3f( r, -r,  r);
	glNormal3f(1, 0, 0);
	glTexCoord2f(1, 1); glVertex3f( r, -r,  r);
	glTexCoord2f(0, 1); glVertex3f( r, -r, -r);
	glTexCoord2f(0, 0); glVertex3f( r,  r, -r);
	glTexCoord2f(1, 0); glVertex3f( r,  r,  r);
	glNormal3f(0, 1, 0);
	glTexCoord2f(1, 0); glVertex3f( r,  r,  r);
	glTexCoord2f(0, 0); glVertex3f( r,  r, -r);
	glTexCoord2f(0, 1); glVertex3f(-r,  r, -r);
	glTexCoord2f(1, 1); glVertex3f(-r,  r,  r);
	glNormal3f(-1, 0, 0);
	glTexCoord2f(1, 1); glVertex3f(-r,  r,  r);
	glTexCoord2f(0, 1); glVertex3f(-r,  r, -r);
	glTexCoord2f(0, 0); glVertex3f(-r, -r, -r);
	glTexCoord2f(1, 0); glVertex3f(-r, -r,  r);
	glNormal3f(0, 0, 1);
	glTexCoord2f(1, 0); glVertex3f(-r, -r,  r);
	glTexCoord2f(1, 1); glVertex3f( r, -r,  r);
	glTexCoord2f(0, 1); glVertex3f( r,  r,  r);
	glTexCoord2f(0, 0); glVertex3f(-r,  r,  r);
	glNormal3f(0, 0, -1);
	glTexCoord2f(1, 0); glVertex3f(-r,  r, -r);
	glTexCoord2f(1, 1); glVertex3f( r,  r, -r);
	glTexCoord2f(0, 1); glVertex3f( r, -r, -r);
	glTexCoord2f(0, 0); glVertex3f(-r, -r, -r);
	glEnd();
}

void genAltar() {
	altarPsi = 0;
	altarPhi = 0;

	boxList = 1;
	holeList = 2;
	GLUquadric* q = gluNewQuadric();

	glNewList(boxList, GL_COMPILE);
	mySolidCube(1);
	glEndList();

	glNewList(holeList, GL_COMPILE);
	gluQuadricTexture(q, GL_TRUE);
	gluSphere(q, 0.65, 16, 16);
	glEndList();

	gluDeleteQuadric(q);
}

void drawParticles(vector<Particle>& v, float width, float height) {
	static GLfloat matrix[16];
	glGetFloatv(GL_MODELVIEW_MATRIX, matrix);
	Vector up   = Vector(matrix[1], matrix[5], matrix[9]).norm() * width / 2;
	Vector left = Vector(matrix[0], matrix[4], matrix[8]).norm() * height / 2;

	glEnable(GL_BLEND);
	glDepthMask(GL_FALSE);
	glDisable(GL_LIGHTING);
	glBegin(GL_QUADS);
	for(unsigned int i = 0; i < v.size(); i++) if(v[i].alive()) {
		glColor4fv(v[i].color);
		glTexCoord2f(0, 0); glVertex(v[i].pos + up + left);
		glTexCoord2f(1, 0); glVertex(v[i].pos - up + left);
		glTexCoord2f(1, 1); glVertex(v[i].pos - up - left);
		glTexCoord2f(0, 1); glVertex(v[i].pos + up - left);
	}
	glEnd();
	glEnable(GL_LIGHTING);
	glDepthMask(GL_TRUE);
	glDisable(GL_BLEND);
}

void drawBillBoards(vector<Particle>& v, float width, float height) {
	static GLfloat matrix[16];
	glGetFloatv(GL_MODELVIEW_MATRIX, matrix);
	Vector up   = Vector(0, 0, 1) * width / 2;
	Vector left = Vector(matrix[0], matrix[4], matrix[8]).norm() * height / 2;

	glEnable(GL_BLEND);
	glDepthMask(GL_FALSE);
	glDisable(GL_LIGHTING);
	glBegin(GL_QUADS);
	for(unsigned int i = 0; i < v.size(); i++) if(v[i].alive()) {
		glColor4fv(v[i].color);
		glTexCoord2f(1, 0); glVertex(v[i].pos + up + left);
		glTexCoord2f(1, 1); glVertex(v[i].pos - up + left);
		glTexCoord2f(0, 1); glVertex(v[i].pos - up - left);
		glTexCoord2f(0, 0); glVertex(v[i].pos + up - left);
	}
	glEnd();
	glEnable(GL_LIGHTING);
	glDepthMask(GL_TRUE);
	glDisable(GL_BLEND);
}

void drawAinB(GLuint a, GLuint b, GLenum cullFace, GLenum stencilTest) { 
	glEnable(GL_DEPTH_TEST);
	glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
	glCullFace(cullFace);
	glCallList(a);

	glDepthMask(GL_FALSE);
	glEnable(GL_STENCIL_TEST);
	glStencilFunc(GL_ALWAYS, 0, 0);
	glStencilOp(GL_KEEP, GL_KEEP, GL_INCR);
	glCullFace(GL_BACK);
	glCallList(b);

	glStencilOp(GL_KEEP, GL_KEEP, GL_DECR);
	glCullFace(GL_FRONT);
	glCallList(b);

	glDepthMask(GL_TRUE);
	glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);

	glStencilFunc(stencilTest, 0, 1);
	glDisable(GL_DEPTH_TEST);

	glCullFace(cullFace);
	glCallList(a);
	glEnable(GL_DEPTH_TEST);
}

void drawDepth(GLuint a) {
	glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
	glEnable(GL_DEPTH_TEST);
	glDisable(GL_STENCIL_TEST);
	glDepthFunc(GL_ALWAYS);
	glCallList(a);
	glDepthFunc(GL_LESS);
}

void drawAltar() {
	glBindTexture(GL_TEXTURE_2D, particleTex);
	drawParticles(altarFire, 0.5, 0.5);

	glBindTexture(GL_TEXTURE_2D, altarTex);
	glEnable(GL_CULL_FACE);

	drawAinB(holeList, boxList, GL_FRONT, GL_NOTEQUAL);
	drawDepth(boxList);
	drawAinB(boxList, holeList, GL_BACK, GL_EQUAL);
	
	glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
	glStencilFunc(GL_NOTEQUAL, 0, 1);
	glCullFace(GL_FRONT);
	glEnable(GL_DEPTH_TEST);
	glDepthFunc(GL_ALWAYS);
	glCallList(boxList);
	glDepthFunc(GL_LESS);
	glCullFace(GL_BACK);
	glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);

	glDisable(GL_CULL_FACE);

	glStencilFunc(GL_EQUAL, 0, 1);
	glBindTexture(GL_TEXTURE_2D, particleTex);
	drawParticles(altarFire, 0.5, 0.5);

	glDisable(GL_STENCIL_TEST);
}

void genLists() {
	cubeList = 5;
	sphereList = 6;
	GLUquadric* q = gluNewQuadric();
	glNewList(cubeList, GL_COMPILE);
	mySolidCube(1);
	glEndList();
	glNewList(sphereList, GL_COMPILE);
	gluQuadricTexture(q, GL_TRUE);
	gluSphere(q, 1, 16, 16);
	glEndList();
	gluDeleteQuadric(q);
}

void drawEvilFloor(float t) {
	const int R = 15;
	glBindTexture(GL_TEXTURE_2D, evilFloorTex);
	glPushMatrix();
	glRotatef(t * 60, 0, 0, 1);
	glTranslatef(0, 0, 0.5);
	for(int i = -R; i < R; i++) {
		for(int j = -R; j < R; j++) {
			glPushMatrix();
			float r = sqrt((float) i * i + (float) j * j);
			glTranslatef(i, j, 0.5 * r * sin(2 * M_PI * r / 5 + t) + r / 20);
			glScalef(1, 1, 1 + r / 5);
			glCallList(cubeList);
			glPopMatrix();
		}
	}
	glPopMatrix();
}

void setAltarFireParticleColor(Particle& p) {
	if(p.life > 0.9) {
		p.color[0] = 1.0;
		p.color[1] = 0.0;
		p.color[2] = 0.0;
		p.color[3] = 0.5;
	} else if(p.life > 0.4) {
		p.color[0] = 1.0;
		p.color[1] = (0.9 - p.life) / 0.5;
		p.color[2] = 0.0;
		p.color[3] = 0.5;
	} else {
		p.color[0] = p.life / 0.4;
		p.color[1] = p.life / 0.4;
		p.color[2] = 0.0;
		p.color[3] = p.life;
	}
}

void setSideFireParticleColor(Particle& p) {
	if(p.life > 0.9) {
		p.color[0] = 1.0;
		p.color[1] = 1.0;
		p.color[2] = 0.5;
		p.color[3] = 5 * (1.0 - p.life);
	} else if(p.life < 0.5) {
		p.color[0] = 1.0;
		p.color[1] = 1.0;
		p.color[2] = 0.5;
		p.color[3] = p.life;
	}
}

Particle genAltarParticle() {
	Particle p;
	p.pos = randVector() * 0.2;
	p.dir = Vector(0, 0, -1) * (3.0 + 3.0 * rand() / RAND_MAX) + randVector() * 0.5;
	p.life = 1;
	p.dlife = 1.0 + 1.0 * rand() / RAND_MAX;
	setAltarFireParticleColor(p);
	return p;
}

Particle genAltarFireWorkParticle() {
	Particle p;
	p.pos = randVector().norm();
	p.dir = p.pos * (12.0 + 1.0 * rand() / RAND_MAX);
	p.pos = p.pos * 1.5;
	p.life = 1;
	p.dlife = 2.0 + 1.0 * rand() / RAND_MAX;
	setAltarFireParticleColor(p);
	return p;
}

Particle genSideFire() {
	const float r = 5;
	const float z = -6;
	static int n = 0;
	n = (n + 1) % 4;
	Particle p;
	switch(n) {
		case 0: p.pos = Vector( r,  r, z); break;
		case 1: p.pos = Vector(-r,  r, z); break;
		case 2: p.pos = Vector(-r, -r, z); break;
		case 3: p.pos = Vector( r, -r, z); break;
		default: break;
	}
	p.pos = p.pos + randVector() * 0.2;
	p.dir = Vector(0, 0, 8) + randVector() * 0.3;
	p.life = 1;
	p.dlife = 2.0 + 1.0 * rand() / RAND_MAX;
	p.color[0] = 1;
	p.color[1] = 1;
	p.color[2] = 1;
	p.color[3] = 1;
	setSideFireParticleColor(p);
	return p;
}

void moveParticles(float dt) {
	for(unsigned int i = 0; i < altarFire.size(); i++) {
		if(altarFire[i].alive()) {
			altarFire[i].move(dt);
			setAltarFireParticleColor(altarFire[i]);
		} else
			altarFire[i] = genAltarParticle();
	}
	for(unsigned int i = 0; i < altarFireWork.size(); i++) {
		if(altarFireWork[i].alive()) {
			altarFireWork[i].move(dt);
			setAltarFireParticleColor(altarFireWork[i]);
		}
	}
	for(unsigned int i = 0; i < sideFire.size(); i++) {
		if(sideFire[i].alive()) {
			sideFire[i].move(dt);
			setSideFireParticleColor(sideFire[i]);
		}
		else
			sideFire[i] = genSideFire();
	}
}

void genParticles() {
	for(int i = 0; i < 250; i++)
		altarFire.push_back(genAltarParticle());
	for(int i = 0; i < 1000; i++) {
		Particle p;
		p.life = -1;
		altarFireWork.push_back(p);
	}
	for(int i = 0; i < 200; i++)
		sideFire.push_back(genSideFire());
	for(int i = 0 ; i < 50; i++)
		moveParticles(0.1);
}

void performFireWork() {
	const int maxN = 500;
	int n = 0;
	for(unsigned int i = 0; i < altarFireWork.size(); i++) {
		if(!altarFireWork[i].alive()) {
			n++;
			altarFireWork[i] = genAltarFireWorkParticle();
		}
		if(n > maxN)
			break;
	}
}

void blink(float r, float g, float b, float a) {
	glPushMatrix();
	glLoadIdentity();
	glDisable(GL_LIGHTING);
	glDisable(GL_TEXTURE_2D);
	glEnable(GL_BLEND);
	glDisable(GL_DEPTH_TEST);
	glBegin(GL_QUADS);
	glColor4f(r, g, b, a);
	glVertex3f(-5, -5, -1);
	glVertex3f( 5, -5, -1);
	glVertex3f( 5,  5, -1);
	glVertex3f(-5,  5, -1);
	glEnd();
	glEnable(GL_DEPTH_TEST);
	glDisable(GL_BLEND);
	glEnable(GL_TEXTURE_2D);
	glEnable(GL_LIGHTING);
	glPopMatrix();
}

void sphereAt(float x, float y, float z, float r, float phi, float psi) {
	glPushMatrix();
	glTranslatef(x, y, z);
	glRotatef(phi, 0, 0, 1);
	glRotatef(psi, 0, 1, 0);
	glScalef(r, r, r);
	glCallList(sphereList);
	glPopMatrix();
}

void cubeAt(float x, float y, float z, float r, float phi, float psi) {
	glPushMatrix();
	glTranslatef(x, y, z);
	glRotatef(phi, 0, 0, 1);
	glRotatef(psi, 0, 1, 0);
	glScalef(r, r, r);
	glCallList(cubeList);
	glPopMatrix();
}

void drawFinal(float t) {
	camera = Vector(4 * cos(0.2 * t), 4 * sin(0.2 * t), -3);
	light = Vector(0.1 * sin(t * 2), 0.1 * cos(t * 1), -2);
	GLfloat lPosition[4]  = {light.x, light.y, light.z, 1};

	GLfloat lDiffuse[4]  = {1.0, 0.0, 0.0, 1};
	GLfloat lSpecular[4] = {1.0, 1.0, 0.0, 1};
	glLightfv(GL_LIGHT0, GL_DIFFUSE, lDiffuse);
	glLightfv(GL_LIGHT0, GL_SPECULAR, lSpecular);
	GLfloat mDiffuse[4]  = {1.0, 1.0, 1.0, 0.6};

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(80, (float) wx / wy, 0.01, 100);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	gluLookAt(camera.x, camera.y, camera.z, 0, 0, -2, 0, 0, 1);

	GLfloat fColor[4]  = {0.1, 0.0, 0.0, 1.0};

	glClearColor(fColor[0], fColor[1], fColor[2], 0.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

	glEnable(GL_FOG);
	glFogfv(GL_FOG_COLOR, fColor);
	glFogf(GL_FOG_START, 7.0f);
	glFogf(GL_FOG_END, 15.0f);
	glFogi(GL_FOG_MODE, GL_LINEAR);

	glLightfv(GL_LIGHT0, GL_POSITION, lPosition);

	// Move particles
	moveParticles(dt);

	// Altar size & orientation
	altarPhi += dt * 180;
	altarPsi += -dt * 60;
	static Vector dAltar(0, 0, 0);
	static float altarSize = 1.0;
	static float lastAltarReSizeT = 0;
	static float lastAltarRePosT = 0;
	if(t - lastAltarRePosT > 0.1) {
		dAltar = randVector() * 0.2;
		lastAltarRePosT = t;
	}
	if(t - lastAltarReSizeT > 0.2) {
		altarSize = 0.8 + 0.4 * rand() / RAND_MAX;
		lastAltarReSizeT = t;
		altarPhi += 20.0 - 40.0 * rand() / RAND_MAX;
		altarPsi += 20.0 - 40.0 * rand() / RAND_MAX;
	}
	altarSize = altarSize * (1 - 3.0 * dt) + 3.0 * dt;

	// Boom!
	static float prevBoomT = 0;
	static float prevBoomTModified = 0;
	static float tBoomModifier = 0;
	if(bassK > 0.85 && t - prevBoomT > 0.15) {
		performFireWork();
		prevBoomT = t;
		altarPsi += 120.0 - 240.0 * rand() / RAND_MAX;
		altarPhi += 120.0 - 240.0 * rand() / RAND_MAX;
		altarSize = 1.0 + 0.6 * rand() / RAND_MAX;
	}
	if(bassK > 0.85 && t - prevBoomTModified > 0.3) {
		tBoomModifier = 10.0 * rand() / RAND_MAX;
		prevBoomTModified = t;
	}

	// Floor
	drawEvilFloor(t + tBoomModifier);

	// Fire Spheres
	glBindTexture(GL_TEXTURE_2D, rustTex);
	sphereAt( 5.3,  5.3, -6, 0.25, 0, 0);
	sphereAt(-5.3,  5.3, -6, 0.25, 0, 0);
	sphereAt(-5.3, -5.3, -6, 0.25, 0, 0);
	sphereAt( 5.3, -5.3, -6, 0.25, 0, 0);

	// Change Positions of Flying Spheres & Cubes
	static Vector dCubes[5];
	static float prevCubesChangePosT = 0;
	if(t - prevCubesChangePosT > 0.13) {
		prevCubesChangePosT = t;
		for(int i = 0; i < 5; i++)
			dCubes[i] = randVector() * 0.2;
	}

	// Flying Spheres & Cubes
	glBindTexture(GL_TEXTURE_2D, designTex);
	glPushMatrix(); glTranslatef(dCubes[0].x, dCubes[0].y, dCubes[0].z);
	for(int i = 0; i < 30; i++) { float a = fmod(M_PI * i / 30 + (t + tBoomModifier) * 0.5, M_PI); float r; r = 7; sphereAt(0, r * cos(a), -r * sin(a) + 1, 0.25, 20 * i + 90 * t, 10 * i - 30 * (t + tBoomModifier)); }
	glPopMatrix();
	glPushMatrix(); glTranslatef(dCubes[1].x, dCubes[1].y, dCubes[1].z);
	for(int i = 0; i < 30; i++) { float a = fmod(M_PI * i / 30 + (t + tBoomModifier) * 0.9, M_PI); float r; r = 7; cubeAt(r * cos(a), 0, -r * sin(a) + 1, 0.5,    20 * i - 40 * t, 15 * i + 80 * (t + tBoomModifier)); }
	glPopMatrix();
	glPushMatrix(); glTranslatef(dCubes[2].x, dCubes[2].y, dCubes[2].z);
	for(int i = 0; i < 30; i++) { float a = fmod(M_PI * i / 30 + (t + tBoomModifier) * 0.7, M_PI); float r; r = 6; cubeAt(r * cos(a * 2), r * sin(a * 2), -4, 0.7, 50 * i - 70 * t, 25 * i + 120 * (t + tBoomModifier)); }
	glPopMatrix();
	glPushMatrix(); glTranslatef(dCubes[3].x, dCubes[3].y, dCubes[3].z);
	for(int i = 0; i < 30; i++) { float a = fmod(M_PI * i / 30 + (t + tBoomModifier) * -0.6, M_PI); float r; r = 8; cubeAt(r * cos(a * 2), r * sin(a * 2), -6, 0.8, 25 * i - 30 * t, 35 * i + 90 * (t + tBoomModifier)); }
	glPopMatrix();
	glPushMatrix(); glTranslatef(dCubes[4].x, dCubes[4].y, dCubes[4].z);
	for(int i = 0; i < 30; i++) { float a = fmod(M_PI * i / 30 + (t + tBoomModifier) * 0.3, M_PI); float r; r = 10; cubeAt(r * cos(a * 2), r * sin(a * 2), -8, 1.0, 70 * i - 99 * t, 67 * i + 45 * (t + tBoomModifier)); }
	glPopMatrix();

	// BillBoard flame
	glBindTexture(GL_TEXTURE_2D, fireTex);
	drawBillBoards(sideFire, 1.0, 1.5);

	// Altar
	glPushMatrix();
	glTranslatef(dAltar.x, dAltar.y, dAltar.z - 2.0);
	glRotatef(altarPhi, 0, 0, 1);
	glRotatef(altarPsi, 0, 1, 0);
	glScalef(altarSize, altarSize, altarSize);
	drawAltar();
	glPopMatrix();

	// Fire!!!
	glPushMatrix();
	glTranslatef(0, 0, -2.0);
	glBindTexture(GL_TEXTURE_2D, particleTex);
	drawParticles(altarFireWork, 1.0, 1.0);
	glPopMatrix();

	if(t - prevBoomT < 0.1)
		blink(1, 1, 1, 0.5 * (t - prevBoomT) / 0.1);

	glDisable(GL_FOG);
}

void onDraw() {
	// Calculate t / dt
	static int lastTime = timeGetTime(); 
	dt = (float) (timeGetTime() - lastTime) / 1000.0f; 
	t += dt; 
	lastTime = timeGetTime();

	//float t = ::t + 24;

	// Play music if needed
	if(t > SCN1_END) {
		if(!playing) {
			playing = true;
			BASS_StreamPlay(music, 0, BASS_SAMPLE_LOOP);
		}
		BASS_Update();
		bassK = getBassK();
	}

	//drawLameScene((t) / SCN1_END);

	if(t < SCN1_END) {
		drawLameScene(t);
	} else if(t < SCN2_END) {
		drawWhiteToBlack((t - SCN1_END) / (SCN2_END - SCN1_END), 0.1);
	} else if(t < SCN3_END) {
		drawText(t - SCN2_END);
	} else if(t < SCN4_END) {
		drawFinal(t - SCN3_END);
		if(SCN4_END - t < 1.0)
			blink(0, 0, 0, 1 - (SCN4_END - t));
	} else {
		exit(0);
	}

	glFlush();
	glutSwapBuffers();
}


void onIdle() {
	glutPostRedisplay(); 
}

void onKeyPressed(unsigned char key, int x, int y) {
	if(key==27)
		exit(0);
}

void onResize(int w, int h) {
	glViewport(0, 0, w, h);
	wx = w;
	wy = h;
}

int main(int argc, char** argv) {
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH | GLUT_STENCIL | GLUT_MULTISAMPLE);
	glutCreateWindow("");
	glutReshapeFunc(onResize);
	glutIdleFunc(onIdle);
	glutDisplayFunc(onDraw);
	glutKeyboardFunc(onKeyPressed);

	// rand
	srand(GetTickCount());

	// load Textures
	lameFloorTex = createTexture2D(true, "data/lameFloor.bmp");
	lameTex      = createTexture2D(true, "data/lameTex.bmp");
	hellNoTex    = createTexture2D(true, "data/no.bmp");
	exclTex      = createTexture2D(true, "data/!.bmp");
	altarTex     = createTexture2D(true, "data/altar.bmp");
	evilFloorTex = createTexture2D(true, "data/evilFloor.bmp");
	fireTex      = createTexture2D(true, "data/fire.bmp");
	particleTex  = createTexture2D(true, "data/particle.bmp");
	okTex        = createTexture2D(true, "data/ok.bmp");
	rustTex      = createTexture2D(true, "data/rust.bmp");
	designTex    = createTexture2D(true, "data/design.bmp");

	// PreCalc
	preGenKnot();
	initTriangles();
	initExcls();
	genAltar();
	genLists();
	genParticles();

	// Music
	playing = false;
	BASS_Init(-1, 44100, BASS_DEVICE_NOTHREAD, 0);
	music = BASS_StreamCreateFile(false, MUSIC_FILE, 0, 0, 0); 
	BASS_Start();
	BASS_ChannelSetPosition(music, (QWORD) MAKELONG(0, 0));
	BASS_SetVolume(100);

	if(MessageBox(NULL, "Running this program in a fullscreen mode may inflict a serious damage on your brain. Do you want to run it in a fullscreen mode?", "WARNING!", MB_YESNO | MB_ICONQUESTION) == IDYES) {
		DEVMODE dmScreenSettings;        
		memset(&dmScreenSettings, 0, sizeof(dmScreenSettings));
		dmScreenSettings.dmSize = sizeof(dmScreenSettings);  
		dmScreenSettings.dmPelsWidth = 800;    
		dmScreenSettings.dmPelsHeight = 600;    
		dmScreenSettings.dmBitsPerPel = 32;      
		dmScreenSettings.dmFields = DM_BITSPERPEL | DM_PELSWIDTH | DM_PELSHEIGHT;
		ChangeDisplaySettings(&dmScreenSettings, CDS_FULLSCREEN);
		glutFullScreen();  
	}

	// GL Init
	glClearColor(0, 0, 0, 0);
	glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glEnable(GL_POINT_SMOOTH);
	glEnable(GL_TEXTURE_2D);
	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_NORMALIZE);
	glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);

	// Material
	GLfloat mEmissive[4] = {0.0, 0.0, 0.0, 0.0};
	GLfloat mDiffuse[4]  = {1.0, 1.0, 1.0, 0.7};
	GLfloat mSpecular[4] = {1.0, 1.0, 1.0, 0.0};
	GLfloat mAmbient[4]  = {1.0, 1.0, 1.0, 0.0};
	glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, mEmissive);
	glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, mDiffuse);
	glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, mSpecular);
	glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, mAmbient);
	glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 5);

	// Light
	GLfloat lPosition[4] = {0, 0, 0, 1};
	GLfloat lDiffuse[4]  = {0.4, 0.4, 0.4, 1};
	GLfloat lSpecular[4] = {0.5, 0.5, 0.5, 1};
	GLfloat lAmbient[4]  = {0.1, 0.1, 0.1, 1};
	glLightfv(GL_LIGHT0, GL_POSITION, lPosition);
	glLightfv(GL_LIGHT0, GL_DIFFUSE, lDiffuse);
	glLightfv(GL_LIGHT0, GL_SPECULAR, lSpecular);
	glLightfv(GL_LIGHT0, GL_AMBIENT, lAmbient);
	glLightf(GL_LIGHT0, GL_QUADRATIC_ATTENUATION, 0.04);
	glLightf(GL_LIGHT0, GL_SPOT_EXPONENT, 1);


	glutMainLoop();
	return 0;
}