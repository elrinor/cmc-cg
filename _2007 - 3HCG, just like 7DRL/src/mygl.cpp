#include <Windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <gl/glut.h>
#include <gl/glaux.h>
#include <gl/glu.h>

#define M_PI 3.1415

GLuint wallpaper, woodpanel, plaster, chess, wood;

GLuint RoomList = 1, MirrorRoomList = 2;

float px = 5, py = 0, pz = 2.5;
float cx = 0, cy = 0, cz = 2;
float ca = M_PI;

float time;

int window_h = 1;

void Render4Quads(float rx, float ry, float x0, float y0, float z0, float h) 
{
	glBegin(GL_QUADS);
	glNormal3f(1,0,0);
	glTexCoord2f(1,0); glVertex3f(x0+rx,y0+ry,z0+h);
	glTexCoord2f(1,1); glVertex3f(x0+rx,y0+ry,z0+0);
	glTexCoord2f(0,1); glVertex3f(x0+rx,y0-ry,z0+0);
	glTexCoord2f(0,0); glVertex3f(x0+rx,y0-ry,z0+h);
	glNormal3f(-1,0,0);
	glTexCoord2f(1,0); glVertex3f(x0-rx,y0+ry,z0+h);
	glTexCoord2f(1,1); glVertex3f(x0-rx,y0+ry,z0+0);
	glTexCoord2f(0,1); glVertex3f(x0-rx,y0-ry,z0+0);
	glTexCoord2f(0,0); glVertex3f(x0-rx,y0-ry,z0+h);
	glNormal3f(0,1,0);
	glTexCoord2f(1,0); glVertex3f(x0+rx,y0+ry,z0+h);
	glTexCoord2f(1,1); glVertex3f(x0+rx,y0+ry,z0+0);
	glTexCoord2f(0,1); glVertex3f(x0-rx,y0+ry,z0+0);
	glTexCoord2f(0,0); glVertex3f(x0-rx,y0+ry,z0+h);
	glNormal3f(0,-1,0);
	glTexCoord2f(1,0); glVertex3f(x0+rx,y0-ry,z0+h);
	glTexCoord2f(1,1); glVertex3f(x0+rx,y0-ry,z0+0);
	glTexCoord2f(0,1); glVertex3f(x0-rx,y0-ry,z0+0);
	glTexCoord2f(0,0); glVertex3f(x0-rx,y0-ry,z0+h);
	glEnd();
}

void RenderBox(float rx, float ry, float x0, float y0, float z0, float h) 
{
	Render4Quads(rx, ry, x0, y0, z0, h);
	glBegin(GL_QUADS);
	glNormal3f(0,0,1);
	glTexCoord2f(1,0); glVertex3f(x0+rx,y0+ry,z0+h);
	glTexCoord2f(1,1); glVertex3f(x0-rx,y0+ry,z0+h);
	glTexCoord2f(0,1); glVertex3f(x0-rx,y0-ry,z0+h);
	glTexCoord2f(0,0); glVertex3f(x0+rx,y0-ry,z0+h);
	glNormal3f(0,0,-1);
	glTexCoord2f(1,0); glVertex3f(x0+rx,y0+ry,z0+0);
	glTexCoord2f(1,1); glVertex3f(x0-rx,y0+ry,z0+0);
	glTexCoord2f(0,1); glVertex3f(x0-rx,y0-ry,z0+0);
	glTexCoord2f(0,0); glVertex3f(x0+rx,y0-ry,z0+0);
	glEnd();
}

void PreRenderRoom(bool IsReflection) 
{
	int x, y;

	GLfloat light_position[] = {0, 0, 4, 1};
	glLightfv(GL_LIGHT0, GL_POSITION, light_position);

	// mirror
	if(!IsReflection) 
	{
		glEnable(GL_BLEND);
		glDisable(GL_TEXTURE_2D);
		glDisable(GL_LIGHTING);
		glBegin(GL_QUADS);
		glColor4f(0.8,0.8,1,0.3);
		glVertex3f(6.5, -3, 1);
		glVertex3f(6.5,  3, 1);
		glVertex3f(6.5,  3, 4);
		glVertex3f(6.5, -3, 4);
		glEnd();
		glEnable(GL_LIGHTING);
		glEnable(GL_TEXTURE_2D);
		glDisable(GL_BLEND);
	}

	// mirrorbox
	glBindTexture(GL_TEXTURE_2D, wood);
	RenderBox(0.05, 0.05, 6.5, -3.05, 0, 4);
	RenderBox(0.05, 0.05, 6.5,  3.05, 0, 4);
	RenderBox(0.5, 0.05, 6.5, -3.05, 0, 0.1);
	RenderBox(0.5, 0.05, 6.5,  3.05, 0, 0.1);
	RenderBox(0.05, 3.0, 6.5,  0, 0.9, 0.1);
	RenderBox(0.05, 3.1, 6.5,  0, 4, 0.1);

	// walls
	glBindTexture(GL_TEXTURE_2D, wallpaper);
	glBegin(GL_QUADS);

	if(!IsReflection) 
	{
		glNormal3f(-1, 0, 0);
		glTexCoord2f(0, 0.0); glVertex3f(7, -7, 0);
		glTexCoord2f(7, 0.0); glVertex3f(7,  7, 0);
		glTexCoord2f(7, 2.5); glVertex3f(7,  7, 5);
		glTexCoord2f(0, 2.5); glVertex3f(7, -7, 5);
	}

	glNormal3f(1, 0, 0);
	glTexCoord2f(0, 0.0); glVertex3f(-7, -7, 0);
	glTexCoord2f(7, 0.0); glVertex3f(-7,  7, 0);
	glTexCoord2f(7, 2.5); glVertex3f(-7,  7, 5);
	glTexCoord2f(0, 2.5); glVertex3f(-7, -7, 5);
	glNormal3f(0, 1, 0);
	glTexCoord2f(0, 0.0); glVertex3f(-7, -7, 0);
	glTexCoord2f(7, 0.0); glVertex3f( 7, -7, 0);
	glTexCoord2f(7, 2.5); glVertex3f( 7, -7, 5);
	glTexCoord2f(0, 2.5); glVertex3f(-7, -7, 5);
	glNormal3f(0, -1, 0);
	glTexCoord2f(0, 0.0); glVertex3f(-7, 7, 0);
	glTexCoord2f(7, 0.0); glVertex3f( 7, 7, 0);
	glTexCoord2f(7, 2.5); glVertex3f( 7, 7, 5);
	glTexCoord2f(0, 2.5); glVertex3f(-7, 7, 5);
	glEnd();

	// floor
	glBindTexture(GL_TEXTURE_2D, woodpanel);
	glBegin(GL_QUADS);
	glNormal3f(0, 0, 1);
	for(x = -7; x < 7; x++) for(y = -7; y < 7; y++) 
	{
		glTexCoord2f(x+1,y  ); glVertex3f(x+1,y  , 0);
		glTexCoord2f(x+1,y+1); glVertex3f(x+1,y+1, 0);
		glTexCoord2f(x  ,y+1); glVertex3f(x  ,y+1, 0);
		glTexCoord2f(x  ,y  ); glVertex3f(x  ,y  , 0);
	}
	glEnd();

	// ceiling
	glBindTexture(GL_TEXTURE_2D, plaster);
	glBegin(GL_QUADS);
	glNormal3f(0, 0, -1);
	for(x = -7; x < 7; x++) for(y = -7; y < 7; y++) 
	{
		glTexCoord2f(x+1,y  ); glVertex3f(x+1,y  , 5);
		glTexCoord2f(x+1,y+1); glVertex3f(x+1,y+1, 5);
		glTexCoord2f(x  ,y+1); glVertex3f(x  ,y+1, 5);
		glTexCoord2f(x  ,y  ); glVertex3f(x  ,y  , 5);
	}
	glEnd();

	// light
	glDisable(GL_TEXTURE_2D);
	glDisable(GL_LIGHTING);
	glColor3f(0.1,0.1,0);
	Render4Quads(0.02, 0.02, 0, 0, 4, 1);

	glPushMatrix();
	glTranslatef(0, 0, 4);
	glColor3f(1,1,0.8);
	glutSolidSphere(0.1, 8, 8);
	glPopMatrix();
	glEnable(GL_TEXTURE_2D);
	glEnable(GL_LIGHTING);

	// table
	glBindTexture(GL_TEXTURE_2D, wood);
	Render4Quads(0.05, 0.05, 0.5, 0.5, 0, 1);
	Render4Quads(0.05, 0.05,-0.5, 0.5, 0, 1);
	Render4Quads(0.05, 0.05,-0.5,-0.5, 0, 1);
	Render4Quads(0.05, 0.05, 0.5,-0.5, 0, 1);
	RenderBox(0.6, 0.6, 0, 0, 1, 0.1);
	Render4Quads(0.5, 0.5, 0, 0, 1.0, 0.12);

	glBindTexture(GL_TEXTURE_2D, chess);
	glBegin(GL_QUADS);
	glNormal3f(0,0,1);
	glTexCoord2f(0,0); glVertex3f(-0.5,-0.5,1.12);
	glTexCoord2f(1,0); glVertex3f(+0.5,-0.5,1.12);
	glTexCoord2f(1,1); glVertex3f(+0.5,+0.5,1.12);
	glTexCoord2f(0,1); glVertex3f(-0.5,+0.5,1.12);
	glEnd();
}

void PrepareLists() 
{
	glNewList(RoomList, GL_COMPILE);
	PreRenderRoom(false);
	glEndList();
	glNewList(MirrorRoomList, GL_COMPILE);
	PreRenderRoom(true);
	glEndList();
}

void RenderRoom(bool IsReflection) 
{
	if(IsReflection)
		glCallList(MirrorRoomList);
	else
		glCallList(RoomList);

	glBindTexture(GL_TEXTURE_2D, woodpanel);
	glPushMatrix();
	glRotatef(time * 90, 0, 0, 1);
	glTranslatef(0.4, 0, 1.22);
	glRotatef(-time * 180, 1, 0, 0);
	GLUquadricObj* q = gluNewQuadric();
	gluQuadricTexture(q, GL_TRUE);
	gluSphere(q, 0.1, 16, 16);
	gluDeleteQuadric(q);
	glPopMatrix();
}

void DoRenderPass() 
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	gluLookAt(px, py, pz, cx, cy, cz, 0, 0, 1);

	// Draw mirror in stencil buffer
	glEnable(GL_STENCIL_TEST);
	glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);
	glStencilFunc(GL_ALWAYS, 1, 1);
	glDepthMask(GL_FALSE);
	glBegin(GL_QUADS);
	glVertex3f(6.5, -3, 1);
	glVertex3f(6.5,  3, 1);
	glVertex3f(6.5,  3, 4);
	glVertex3f(6.5, -3, 4);
	glEnd();
	glDepthMask(GL_TRUE);

	// draw reflection
	glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
	glStencilFunc(GL_EQUAL, 1, 1);
	glPushMatrix();
	glScalef(-1,1,1);
	glTranslatef(-13,0,0);
	RenderRoom(true);
	glPopMatrix();
	glDisable(GL_STENCIL_TEST);
	
	// draw room
	RenderRoom(false);
}

void RenderToAccum(float dcx, float dcy, float dcz) 
{
	cx += dcx;
	cy += dcy;
	cz += dcz;
	DoRenderPass();
	glAccum(GL_ACCUM,1.0/5);
	cx -= dcx;
	cy -= dcy;
	cz -= dcz;
}

void Render() 
{
	time = GetTickCount() / 1000.0f;

	px = cos(ca) * 6;
	py = sin(ca) * 6;

	glClear(GL_ACCUM_BUFFER_BIT);
	float dc = 2.0 / window_h;
	float dx =  sin(ca) * dc;
	float dy = -cos(ca) * dc;

	RenderToAccum(0,0,0);
	RenderToAccum(+2*dx,+2*dy,+1*dc);
	RenderToAccum(-2*dx,-2*dy,-1*dc);
	RenderToAccum(-1*dx,-1*dy,+2*dc);
	RenderToAccum(+1*dx,+1*dy,-2*dc);

	glAccum(GL_RETURN,1.0f);
	glutSwapBuffers();
}

void ChangeSize(int w, int h) 
{
	if(h == 0) h = 1;
	float ratio = 1.0 * w / h;
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glViewport(0, 0, w, h);
	gluPerspective(60, ratio, 0.1, 50);
	glMatrixMode(GL_MODELVIEW);
	window_h = h;
}

void ProcessKeys(unsigned char key, int x, int y) 
{
	if(key == 27)
		exit(0);
	if(key == 'q' || key == 'Q')
		ca -= 0.03;
	if(key == 'w' || key == 'W')
		ca += 0.03;
}

AUX_RGBImageRec* generateChessTexture() 
{
	AUX_RGBImageRec* result = new AUX_RGBImageRec();
	result->sizeX = 128;
	result->sizeY = 128;
	result->data = new unsigned char[result->sizeY * result->sizeX * 3];
	for(int iy = 0; iy < result->sizeY; iy++) for(int ix = 0; ix < result->sizeX; ix++) 
	{
		unsigned char *p = result->data + (result->sizeX * iy + ix) * 3;
		unsigned char c = ((sin(M_PI * iy / 16) * sin(M_PI * ix / 16)) > 0) ? 16 : 240;
		p[0] = c;
		p[1] = c;
		p[2] = c;
	}
	return result;
}

int main(int argc, char** argv) 
{
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA | GLUT_STENCIL | GLUT_MULTISAMPLE);
	glutInitWindowPosition(100,100);
	glutInitWindowSize(320,320);
	glutCreateWindow("");

	glutDisplayFunc(Render);
	glutIdleFunc(Render);
	glutReshapeFunc(ChangeSize);
	glutKeyboardFunc(ProcessKeys);

	AUX_RGBImageRec* ImageRec;
	
	ImageRec = auxDIBImageLoad("wallpaper.bmp");
	glGenTextures(1, &wallpaper);
	glBindTexture(GL_TEXTURE_2D, wallpaper);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
	gluBuild2DMipmaps(GL_TEXTURE_2D, 3, ImageRec->sizeX, ImageRec->sizeY, GL_RGB, GL_UNSIGNED_BYTE, ImageRec->data);

	ImageRec = auxDIBImageLoad("woodpanel.bmp");
	glGenTextures(1, &woodpanel);
	glBindTexture(GL_TEXTURE_2D, woodpanel);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
	gluBuild2DMipmaps(GL_TEXTURE_2D, 3, ImageRec->sizeX, ImageRec->sizeY, GL_RGB, GL_UNSIGNED_BYTE, ImageRec->data);

	ImageRec = auxDIBImageLoad("plaster.bmp");
	glGenTextures(1, &plaster);
	glBindTexture(GL_TEXTURE_2D, plaster);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
	gluBuild2DMipmaps(GL_TEXTURE_2D, 3, ImageRec->sizeX, ImageRec->sizeY, GL_RGB, GL_UNSIGNED_BYTE, ImageRec->data);

	ImageRec = auxDIBImageLoad("wood.bmp");
	glGenTextures(1, &wood);
	glBindTexture(GL_TEXTURE_2D, wood);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
	gluBuild2DMipmaps(GL_TEXTURE_2D, 3, ImageRec->sizeX, ImageRec->sizeY, GL_RGB, GL_UNSIGNED_BYTE, ImageRec->data);

	ImageRec = generateChessTexture();
	glGenTextures(1, &chess);
	glBindTexture(GL_TEXTURE_2D, chess);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
	gluBuild2DMipmaps(GL_TEXTURE_2D, 3, ImageRec->sizeX, ImageRec->sizeY, GL_RGB, GL_UNSIGNED_BYTE, ImageRec->data);

	glEnable(GL_NORMALIZE);
	glEnable(GL_TEXTURE_2D);
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glClearColor(1.0,1.0,1.0,1.0);

	GLfloat light_diffuse[] = {1,1,0.91,1};
	GLfloat light_specular[] = {1,1,0.91,1};
	GLfloat light_ambient[] = {0.5,0.5,0.5,1};
	glLightfv(GL_LIGHT0, GL_AMBIENT, light_ambient);
	glLightfv(GL_LIGHT0, GL_DIFFUSE, light_diffuse);
	glLightfv(GL_LIGHT0, GL_SPECULAR, light_specular);
	glLightf(GL_LIGHT0, GL_LINEAR_ATTENUATION, 0.01);

	GLfloat material_diffuse[] = {0.6,0.6,0.6,1};
	GLfloat material_specular[] = {0.0,0.0,0.0,1};
	GLfloat material_ambient[] = {0.6,0.6,0.6,1};
	GLfloat material_emission[] = {0,0,0,1};
	glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, material_ambient);
	glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, material_diffuse);
	glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, material_specular);
	glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, material_emission);
	glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 20);

	PrepareLists();

	glutMainLoop();

	return 0;
}