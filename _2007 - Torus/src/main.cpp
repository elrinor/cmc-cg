#include <Windows.h>
#include <gl/glut.h>
#include <gl/glu.h>
#include <gl/glaux.h>
#include <stdlib.h>
#include <math.h>

#define PI 3.141592

#define TORUS 1

GLfloat lightpos[] = {-4.0f, 0.0f, 2.0f, 1.0f};
GLfloat lightamb[] = {0.05f, 0.05f, 0.05f, 1.0f};
GLfloat lightdif[] = {0.7f, 0.7f, 0.3f, 1.0f};
GLfloat lightspc[] = {0.9f, 0.3f, 0.3f, 1.0f};

float torusAngle0 = 0;
float torusAngle1 = 0;
float sceneAng0 = 0;

bool animating = true;

unsigned lastFrameTime = timeGetTime();

GLuint texture;

void
redraw(void)
{
	if(animating) 
	{
		float t = (timeGetTime() - lastFrameTime) / 1000.0;
		sceneAng0 += t * 90;
		torusAngle0 += t * 170;
		torusAngle1 -= t * 130;
	}
	lastFrameTime = timeGetTime();

	glClear(GL_DEPTH_BUFFER_BIT | GL_COLOR_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

	static GLfloat torus_mat[] = {1.0f, 1.0f, 1.0f, 1.0f};
	static GLfloat torus_ems[] = {0,0,0,1};
	glMaterialfv(GL_FRONT, GL_AMBIENT, torus_mat);
	glMaterialfv(GL_FRONT, GL_DIFFUSE, torus_mat);
	glMaterialfv(GL_FRONT, GL_SPECULAR, torus_mat);
	glMaterialfv(GL_FRONT, GL_EMISSION, torus_ems);
	glMaterialf(GL_FRONT, GL_SHININESS, 16);

	glPushMatrix();
	glRotatef(sceneAng0, 0, 0, 1);

	// light
	glLightfv(GL_LIGHT0, GL_POSITION, lightpos);
	glDisable(GL_LIGHTING);
	glDisable(GL_TEXTURE_2D);
	glBegin(GL_POINTS);
	glColor3f(1, 0.9, 0);
	glVertex3f(lightpos[0], lightpos[1], lightpos[2]);
	glEnd();
	glEnable(GL_LIGHTING);
	glEnable(GL_TEXTURE_2D);

	// torus
	glPushMatrix();
	glRotatef(torusAngle0, 1, 0, 0);
	glRotatef(torusAngle1, 0, 1, 0);
	glColor3f(1, 1, 1);
	glCallList(TORUS);
	glPopMatrix();

	// walls & floor
	float k = 0.1;
	glBegin(GL_QUADS);
	glNormal3f(0,0,1);
	for(int i = -10; i < 10; i++) for(int j = -10; j < 10; j++) 
	{
		glTexCoord2f(k*(i  ),k*(j  ));glVertex3f(i  ,j  ,-10);
		glTexCoord2f(k*(i+1),k*(j  ));glVertex3f(i+1,j  ,-10);
		glTexCoord2f(k*(i+1),k*(j+1));glVertex3f(i+1,j+1,-10);
		glTexCoord2f(k*(i  ),k*(j+1));glVertex3f(i  ,j+1,-10);
	}
	glNormal3f(1,0,0);
	for(int i = -10; i < 10; i++) for(int j = -10; j < 10; j++) 
	{
		glTexCoord2f(k*(i  ),k*(j  ));glVertex3f(-10,j  ,i  );
		glTexCoord2f(k*(i+1),k*(j  ));glVertex3f(-10,j  ,i+1);
		glTexCoord2f(k*(i+1),k*(j+1));glVertex3f(-10,j+1,i+1);
		glTexCoord2f(k*(i  ),k*(j+1));glVertex3f(-10,j+1,i  );
	}
	glNormal3f(-1,0,0);
	for(int i = -10; i < 10; i++) for(int j = -10; j < 10; j++) 
	{
		glTexCoord2f(k*(i  ),k*(j  ));glVertex3f(10,j  ,i  );
		glTexCoord2f(k*(i+1),k*(j  ));glVertex3f(10,j  ,i+1);
		glTexCoord2f(k*(i+1),k*(j+1));glVertex3f(10,j+1,i+1);
		glTexCoord2f(k*(i  ),k*(j+1));glVertex3f(10,j+1,i  );
	}
	glNormal3f(0,1,0);
	for(int i = -10; i < 10; i++) for(int j = -10; j < 10; j++) 
	{
		glTexCoord2f(k*(i  ),k*(j  ));glVertex3f(j  ,-10,i  );
		glTexCoord2f(k*(i+1),k*(j  ));glVertex3f(j  ,-10,i+1);
		glTexCoord2f(k*(i+1),k*(j+1));glVertex3f(j+1,-10,i+1);
		glTexCoord2f(k*(i  ),k*(j+1));glVertex3f(j+1,-10,i  );
	}
	glNormal3f(0,-1,0);
	for(int i = -10; i < 10; i++) for(int j = -10; j < 10; j++) 
	{
		glTexCoord2f(k*(i  ),k*(j  ));glVertex3f(j  ,10,i  );
		glTexCoord2f(k*(i+1),k*(j  ));glVertex3f(j  ,10,i+1);
		glTexCoord2f(k*(i+1),k*(j+1));glVertex3f(j+1,10,i+1);
		glTexCoord2f(k*(i  ),k*(j+1));glVertex3f(j+1,10,i  );
	}
	glEnd();

	glPopMatrix();

	glutSwapBuffers();
}

void 
key(unsigned char key, int x, int y)
{
	if(key == '\033')
		exit(0);
	if(key == 'a' || key == 'A')
		torusAngle0 += 10;
	if(key == 'd' || key == 'D')
		torusAngle0 -= 10;
	if(key == 'w' || key == 'W')
		torusAngle1 += 10;
	if(key == 's' || key == 'S')
		torusAngle1 -= 10;
	if(key == 'z' || key == 'Z')
		sceneAng0 += 10;
	if(key == 'x' || key == 'X')
		sceneAng0 -= 10;
	if(key == ' ')
		animating = !animating;
}

void 
idle(void) 
{
	glutPostRedisplay();
}

int
main(int argc, char *argv[])
{
	glutInit(&argc, argv);
	glutInitWindowSize(600, 600);
	glutInitDisplayMode(GLUT_RGBA | GLUT_DEPTH | GLUT_STENCIL | GLUT_DOUBLE);
	glutCreateWindow("");
	glutDisplayFunc(redraw);
	glutIdleFunc(idle);
	glutKeyboardFunc(key);

	glMatrixMode(GL_PROJECTION);
	gluPerspective(90, 1, 0.1, 100);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	gluLookAt(6, 0, 6, 0, 0, 0, 0, 0, 1);

	glEnable(GL_DEPTH_TEST);
	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);

	glPointSize(10);
	glEnable(GL_POINT_SMOOTH);
	glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);

	glClearColor(0,0,0,0);

	glShadeModel(GL_SMOOTH);

	glLightfv(GL_LIGHT0, GL_AMBIENT, lightamb);
	glLightfv(GL_LIGHT0, GL_DIFFUSE, lightdif);
	glLightfv(GL_LIGHT0, GL_SPECULAR, lightspc);
	glLightf(GL_LIGHT0, GL_LINEAR_ATTENUATION, 0.05);
	glLightModelfv(GL_LIGHT_MODEL_AMBIENT, lightamb);

	glEnable(GL_TEXTURE_2D);

	AUX_RGBImageRec* ptex = auxDIBImageLoad("texture.bmp");
	glGenTextures(1, &texture);
	glBindTexture(GL_TEXTURE_2D, texture);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	gluBuild2DMipmaps(GL_TEXTURE_2D, 3, ptex->sizeX, ptex->sizeY, GL_RGB, GL_UNSIGNED_BYTE, ptex->data);

	glNewList(TORUS, GL_COMPILE);
	const int TORUS_FACES = 128;
	const float _R = 2;
	const float _r = 1.0;
	float torusVertex[TORUS_FACES + 1][TORUS_FACES + 1][3], torusNormal[TORUS_FACES + 1][TORUS_FACES + 1][3], torusTexCoord[TORUS_FACES + 1][TORUS_FACES + 1][2];
	for(int i = 0; i <= TORUS_FACES; i++) 
	{
		for(int j = 0; j <= TORUS_FACES; j++) 
		{
			float phi = 2 * PI * i / TORUS_FACES;
			float psi = 2 * PI * j / TORUS_FACES;
			
			torusVertex[i][j][0] = (_R + _r * sin(psi)) * cos(phi);
			torusVertex[i][j][1] = (_R + _r * sin(psi)) * sin(phi);
			torusVertex[i][j][2] = _r * cos(psi);

			torusNormal[i][j][0] = sin(psi) * cos(phi);
			torusNormal[i][j][1] = sin(psi) * sin(phi);
			torusNormal[i][j][2] = cos(psi);

			torusTexCoord[i][j][0] = 2.0 * i / TORUS_FACES;
			torusTexCoord[i][j][1] = 1.0 * j / TORUS_FACES;
		}
	}
	glBegin(GL_QUADS);
	for(int i = 0; i < TORUS_FACES; i++) 
	{
		for(int j = 0; j < TORUS_FACES; j++) 
		{
			glTexCoord2f(torusTexCoord[i  ][j  ][0], torusTexCoord[i  ][j  ][1]);
			glNormal3f(torusNormal[i  ][j  ][0], torusNormal[i  ][j  ][1], torusNormal[i  ][j  ][2]);
			glVertex3f(torusVertex[i  ][j  ][0], torusVertex[i  ][j  ][1], torusVertex[i  ][j  ][2]);

			glTexCoord2f(torusTexCoord[i+1][j  ][0], torusTexCoord[i+1][j  ][1]);
			glNormal3f(torusNormal[i+1][j  ][0], torusNormal[i+1][j  ][1], torusNormal[i+1][j  ][2]);
			glVertex3f(torusVertex[i+1][j  ][0], torusVertex[i+1][j  ][1], torusVertex[i+1][j  ][2]);

			glTexCoord2f(torusTexCoord[i+1][j+1][0], torusTexCoord[i+1][j+1][1]);
			glNormal3f(torusNormal[i+1][j+1][0], torusNormal[i+1][j+1][1], torusNormal[i+1][j+1][2]);
			glVertex3f(torusVertex[i+1][j+1][0], torusVertex[i+1][j+1][1], torusVertex[i+1][j+1][2]);

			glTexCoord2f(torusTexCoord[i  ][j+1][0], torusTexCoord[i  ][j+1][1]);
			glNormal3f(torusNormal[i  ][j+1][0], torusNormal[i  ][j+1][1], torusNormal[i  ][j+1][2]);
			glVertex3f(torusVertex[i  ][j+1][0], torusVertex[i  ][j+1][1], torusVertex[i  ][j+1][2]);
		}
	}
	glEnd();
	glEndList();

	glutMainLoop();
	return 0;          
}