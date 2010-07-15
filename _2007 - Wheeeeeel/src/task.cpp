#include <Windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <GL/glut.h>
#include <GL/GLAux.h>

const int WHEELLIST = 1, CHAIRLIST = 2, FLOORLIST = 3, LIGHTLIST = 4, BOXESLIST = 5;

const float PI = 3.141592653589793238462643383279f;

GLuint tWheel, tRed, tFloor;

float camAlpha = 0.0f;

float t = 0;
float t0 = timeGetTime() / 1000.0f;

float lightpos[4];

void DisplayScene(bool drawLight) 
{
	if(drawLight) 
	{
		glLightfv(GL_LIGHT0, GL_POSITION, lightpos);
		glPushMatrix();
		glTranslatef(lightpos[0], lightpos[1], lightpos[2]);
		glCallList(LIGHTLIST);
		glPopMatrix();
	}

	glCallList(BOXESLIST);
	glCallList(CHAIRLIST);

	glPushMatrix();
	glRotatef(60.0f * t, 0, 0, 1);
	glTranslatef(20, 0, 5.5);
	glRotatef(-150.0f * t, 1, 0, 0);
	glRotatef(90, 0, 1, 0);
	glCallList(WHEELLIST);
	glPopMatrix();
}

void DisplayFunc(void) 
{
	t = timeGetTime() / 1000.0f - t0;

	lightpos[0] = 10 * sin(1.0f * t);
	lightpos[1] = 10 * cos(1.5f * t);
	lightpos[2] = 30;
	lightpos[3] = 1;

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

	glLoadIdentity();
	gluLookAt(40 * cos(camAlpha), 40 * sin(camAlpha), 35, 0, 0, 0, 0, 0, 1);

	glPushMatrix();
	glScalef(1, 1, -1);
	DisplayScene(true);
	glPopMatrix();
	DisplayScene(true);

	glEnable(GL_STENCIL_TEST);
	glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
	glDepthMask(GL_FALSE);
	glStencilFunc(GL_ALWAYS, 1, 1);
	glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);
	GLfloat mat[] = {lightpos[2],0,0,0, 0,lightpos[2],0,0, -lightpos[0],-lightpos[1],0,-1, 0,0,0,lightpos[2]};
	glPushMatrix();
	glMultMatrixf(mat);
	DisplayScene(false);
	glPopMatrix();
	glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
	glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
	glDepthMask(GL_TRUE);

	glEnable(GL_BLEND);
	
	glStencilFunc(GL_EQUAL, 1, 1);
	glDisable(GL_LIGHT0);
	glCallList(FLOORLIST);
	glEnable(GL_LIGHT0);

	glStencilFunc(GL_NOTEQUAL, 1, 1);
	glCallList(FLOORLIST);
	
	glDisable(GL_BLEND);
	glDisable(GL_STENCIL_TEST);


	glutSwapBuffers();
	glutPostRedisplay();
}

void KeyboardFunc(unsigned char c, int x, int y) 
{
	switch (c) 
	{
	case 27:
		exit(0);
		break;
	case 'z':
	case 'Z':
		camAlpha -= 0.1;
		break;
	case 'x':
	case 'X':
		camAlpha += 0.1;
		break;
	default:
		break;
	}
}

void ReshapeFunc(int w, int h) 
{
	glViewport(0, 0, w, h);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(70, (float) w / h, 0.1, 100);
	glMatrixMode(GL_MODELVIEW);
}

void DrawWheelXY(float r, float dr, float dz) 
{
	const int n = 64;
	glBegin(GL_QUAD_STRIP);
	for(int i = 0; i <= n; i++) 
	{
		float a = 2 * PI * i / n;
		glNormal3f(cos(a), sin(a), 0);
		glTexCoord2f((float) i / n, 0);
		glVertex3f((r + dr) * cos(a), (r + dr) * sin(a),  dz / 2);
		glTexCoord2f((float) i / n, 0.25);
		glVertex3f((r + dr) * cos(a), (r + dr) * sin(a), -dz / 2);
	}
	glEnd();
	glBegin(GL_QUAD_STRIP);
	for(int i = 0; i <= n; i++) 
	{
		float a = 2 * PI * i / n;
		glNormal3f(-cos(a), -sin(a), 0);
		glTexCoord2f((float) i / n, 0.75);
		glVertex3f((r) * cos(a), (r) * sin(a),  dz / 2);
		glTexCoord2f((float) i / n, 0.5);
		glVertex3f((r) * cos(a), (r) * sin(a), -dz / 2);
	}
	glEnd();
	glBegin(GL_QUAD_STRIP);
	for(int i = 0; i <= n; i++) 
	{
		float a = 2 * PI * i / n;
		glNormal3f(0, 0, 1);
		glTexCoord2f((float) i / n, 0);
		glVertex3f((r + dr) * cos(a), (r + dr) * sin(a),  dz / 2);
		glTexCoord2f((float) i / n, 0.75);
		glVertex3f((r) * cos(a), (r) * sin(a),  dz / 2);
	}
	glEnd();
	glBegin(GL_QUAD_STRIP);
	for(int i = 0; i <= n; i++) 
	{
		float a = 2 * PI * i / n;
		glNormal3f(0, 0, -1);
		glTexCoord2f((float) i / n, 0.5);
		glVertex3f((r) * cos(a), (r) * sin(a), -dz / 2);
		glTexCoord2f((float) i / n, 0.25);
		glVertex3f((r + dr) * cos(a), (r + dr) * sin(a), -dz / 2);
	}
	glEnd();
}

void DrawBox(float cx, float cy, float cz, float dx, float dy, float dz) 
{
	glBegin(GL_QUADS);
	glNormal3f(0, 0, 1);
	glTexCoord2f(0, 0); 
	glVertex3f(cx + dx, cy + dy, cz + dz);
	glTexCoord2f(1, 0); 
	glVertex3f(cx - dx, cy + dy, cz + dz);
	glTexCoord2f(1, 1); 
	glVertex3f(cx - dx, cy - dy, cz + dz);
	glTexCoord2f(0, 1); 
	glVertex3f(cx + dx, cy - dy, cz + dz);

	glNormal3f(0, 0, -1);
	glTexCoord2f(0, 0); 
	glVertex3f(cx + dx, cy + dy, cz - dz);
	glTexCoord2f(1, 0); 
	glVertex3f(cx - dx, cy + dy, cz - dz);
	glTexCoord2f(1, 1); 
	glVertex3f(cx - dx, cy - dy, cz - dz);
	glTexCoord2f(0, 1); 
	glVertex3f(cx + dx, cy - dy, cz - dz);

	glNormal3f(0, 1, 0);
	glTexCoord2f(0, 0); 
	glVertex3f(cx + dx, cy + dy, cz + dz);
	glTexCoord2f(1, 0); 
	glVertex3f(cx - dx, cy + dy, cz + dz);
	glTexCoord2f(1, 1); 
	glVertex3f(cx - dx, cy + dy, cz - dz);
	glTexCoord2f(0, 1); 
	glVertex3f(cx + dx, cy + dy, cz - dz);

	glNormal3f(0, -1, 0);
	glTexCoord2f(0, 0); 
	glVertex3f(cx + dx, cy - dy, cz + dz);
	glTexCoord2f(1, 0); 
	glVertex3f(cx - dx, cy - dy, cz + dz);
	glTexCoord2f(1, 1); 
	glVertex3f(cx - dx, cy - dy, cz - dz);
	glTexCoord2f(0, 1); 
	glVertex3f(cx + dx, cy - dy, cz - dz);

	glNormal3f(1, 0, 0);
	glTexCoord2f(0, 0); 
	glVertex3f(cx + dx, cy + dy, cz + dz);
	glTexCoord2f(1, 0); 
	glVertex3f(cx + dx, cy - dy, cz + dz);
	glTexCoord2f(1, 1); 
	glVertex3f(cx + dx, cy - dy, cz - dz);
	glTexCoord2f(0, 1); 
	glVertex3f(cx + dx, cy + dy, cz - dz);

	glNormal3f(-1, 0, 0);
	glTexCoord2f(0, 0); 
	glVertex3f(cx - dx, cy + dy, cz + dz);
	glTexCoord2f(1, 0); 
	glVertex3f(cx - dx, cy - dy, cz + dz);
	glTexCoord2f(1, 1); 
	glVertex3f(cx - dx, cy - dy, cz - dz);
	glTexCoord2f(0, 1); 
	glVertex3f(cx - dx, cy + dy, cz - dz);
	glEnd();
}


void Init(void) 
{
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_TEXTURE_2D);
	glEnable(GL_NORMALIZE);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);

	GLfloat lightdif[] = {0.3, 0.3, 0.3, 1.0};
	GLfloat lightspec[] = {0.0, 0.0, 0.0, 1.0};
	GLfloat lightamb[] = {0.0, 0.0, 0.0, 1.0};
	glLightfv(GL_LIGHT0, GL_SPECULAR, lightspec);
	glLightfv(GL_LIGHT0, GL_DIFFUSE, lightdif);
	glLightfv(GL_LIGHT0, GL_AMBIENT, lightamb);

	GLfloat mat[] = {1, 1, 1, 0.7};
	glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, mat);
	glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, mat);
	glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, mat);

	GLfloat fogcolor[] = {0, 0, 0, 0};
	glEnable(GL_FOG);
	glFogfv(GL_FOG_COLOR, fogcolor);
	glFogf(GL_FOG_START, 40.0f);
	glFogf(GL_FOG_END, 95.0f);
	glFogi(GL_FOG_MODE, GL_LINEAR);

	AUX_RGBImageRec* image;

	image = auxDIBImageLoad("wood.bmp");
	glGenTextures(1, &tWheel);
	glBindTexture(GL_TEXTURE_2D, tWheel);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
	gluBuild2DMipmaps(GL_TEXTURE_2D, 3, image->sizeX, image->sizeY, GL_RGB, GL_UNSIGNED_BYTE, image->data);

	image = auxDIBImageLoad("cloth.bmp");
	glGenTextures(1, &tRed);
	glBindTexture(GL_TEXTURE_2D, tRed);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
	gluBuild2DMipmaps(GL_TEXTURE_2D, 3, image->sizeX, image->sizeY, GL_RGB, GL_UNSIGNED_BYTE, image->data);

	image = auxDIBImageLoad("floor.bmp");
	glGenTextures(1, &tFloor);
	glBindTexture(GL_TEXTURE_2D, tFloor);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
	gluBuild2DMipmaps(GL_TEXTURE_2D, 3, image->sizeX, image->sizeY, GL_RGB, GL_UNSIGNED_BYTE, image->data);

	glNewList(WHEELLIST, GL_COMPILE);
	glBindTexture(GL_TEXTURE_2D, tWheel);
	DrawWheelXY(5, 0.5, 0.5);
	DrawWheelXY(0.5, 0.5, 0.5);
	for(int i = 0; i < 10; i++) 
	{
		glPushMatrix();
		glRotatef(360.0f * i / 10, 0, 0, 1);
		DrawBox( 3.0, 0, 0, 2.25, 0.2, 0.2);
		glPopMatrix();
	}
	glEndList();

	glNewList(CHAIRLIST, GL_COMPILE);
	glBindTexture(GL_TEXTURE_2D, tWheel);
	DrawBox( 3,  3, 3, 0.3, 0.3, 3);
	DrawBox(-3,  3, 3, 0.3, 0.3, 3);
	DrawBox(-3, -3, 6.5, 0.3, 0.3, 6.5);
	DrawBox( 3, -3, 6.5, 0.3, 0.3, 6.5);
	DrawBox( 0,  0, 6, 3.27, 3.27, 0.3);
	DrawBox( 0, -3, 12.7, 2.7, 0.3, 0.3);
	DrawBox( 0, -3, 09.7, 2.7, 0.3, 0.3);
	glBindTexture(GL_TEXTURE_2D, tRed);
	DrawBox( 0,  0, 6.3, 2.7, 2.7, 0.1);
	DrawBox( 0, -2.9, 11.2, 2.7, 0.3, 1.2);
	glEndList();

	glNewList(FLOORLIST, GL_COMPILE);
	glBindTexture(GL_TEXTURE_2D, tFloor);
	glPushMatrix();
	glScalef(5, 5, 1);
	glBegin(GL_QUADS);
	glNormal3f(0, 0, 1);
	for(int x = -20; x < 20; x++) for(int y = -20; y < 20; y++) 
	{
		glTexCoord2f(x    , y    ); 
		glVertex3f(x    , y    , 0);
		glTexCoord2f(x + 1, y    ); 
		glVertex3f(x + 1, y    , 0);
		glTexCoord2f(x + 1, y + 1); 
		glVertex3f(x + 1, y + 1, 0);
		glTexCoord2f(x    , y + 1); 
		glVertex3f(x    , y + 1, 0);
	}
	glEnd();
	glPopMatrix();
	glEndList();

	glNewList(BOXESLIST, GL_COMPILE);
	glBindTexture(GL_TEXTURE_2D, tFloor);
	glBegin(GL_QUADS);
	glNormal3f(0, 0, 1);
	for(int i = 0; i < 100; i++)
	{
		int x = -15 + 30 * rand() / RAND_MAX;
		int y = -15 + 30 * rand() / RAND_MAX;
		if(x * x + y * y < 45)
			continue;
		DrawBox(6 * x, 6 * y, 3, 3, 3, 3);
	}
	glEnd();
	glEndList();

	glNewList(LIGHTLIST, GL_COMPILE);
	glDisable(GL_TEXTURE_2D);
	glDisable(GL_LIGHTING);
	glColor3f(1, 1, 1);
	GLUquadricObj* quadric = gluNewQuadric();
	gluSphere(quadric, 0.5, 10, 10);
	gluDeleteQuadric(quadric);
	glEnable(GL_LIGHTING);
	glEnable(GL_TEXTURE_2D);
	glEndList();
}

int main(int argc, char **argv) 
{
	srand(GetTickCount());

	glutInitWindowSize(640, 480);
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_DEPTH | GLUT_RGBA | GLUT_STENCIL);
	glutCreateWindow("wheeeeeeee");

	glutReshapeFunc(ReshapeFunc);
	glutDisplayFunc(DisplayFunc);
	glutKeyboardFunc(KeyboardFunc);

	Init();	

	glutMainLoop();
	return 0;
}