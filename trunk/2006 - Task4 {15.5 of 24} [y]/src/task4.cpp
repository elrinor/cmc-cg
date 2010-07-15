#include <windows.h>
#include <gl\glut.h>
#include <stdio.h>
#include <math.h>
#include "BmpLoad.h"

#define PI 3.1415926535897932384626433832795

float right = 0, forward = 0, t, dt, t1 = GetTickCount() / 1000.0;
int wsizex, wsizey;
float phi, psi;
float r = 3;
int type = 0;
bool usetexture = true;
bool showcelled = false;
bool mirrored = true;

GLuint Lists[3] = {0, 0, 0};
GLuint Textures[3];

GLUquadricObj *Sky;

void display()
{
  t = GetTickCount() / 1000.0;
  dt = t - t1;
  t1 = t;
  
  r -= forward * dt;
  phi -= right * dt;
	if (r < 3)
    r = 3;

  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(r * sin(phi) * cos(psi), r * cos(phi) * cos(psi), r * sin(psi), 0, 0, 0, 0, 0, 1);

	GLfloat lp[4] = { 1.5 * sin(t), 1.5 * cos(t), sin(3 * t), 1.0f };
	glLightfv(GL_LIGHT0, GL_POSITION, lp);

  glPointSize(10);
  glDisable(GL_LIGHTING);
  glDisable(GL_TEXTURE_2D);
  glBegin(GL_POINTS);
    glColor3f(1,1,1);
    glVertex3f(lp[0],lp[1],lp[2]);
  glEnd();
  glEnable(GL_LIGHTING);
  glEnable(GL_TEXTURE_2D);

  if(mirrored)
  {
    glBindTexture(GL_TEXTURE_2D, Textures[1]);
    glDisable(GL_TEXTURE_GEN_S);
  	glDisable(GL_TEXTURE_GEN_T);
    gluSphere(Sky, 10, 20, 20);
    glEnable(GL_TEXTURE_GEN_S);
  	glEnable(GL_TEXTURE_GEN_T);
	  glTexGeni(GL_S,GL_TEXTURE_GEN_MODE,GL_SPHERE_MAP);
  	glTexGeni(GL_T,GL_TEXTURE_GEN_MODE,GL_SPHERE_MAP);
    glBindTexture(GL_TEXTURE_2D, Textures[1]);
  }
  else
  {
    glBindTexture(GL_TEXTURE_2D, Textures[0]);
    glDisable(GL_TEXTURE_GEN_S);
  	glDisable(GL_TEXTURE_GEN_T);
  }


  if (!showcelled)
  {
    if (!usetexture)
      glDisable(GL_TEXTURE_2D);
    glCallList(Lists[type]);
    if (!usetexture)
      glEnable(GL_TEXTURE_2D);
  }
  else
  {
    glEnable(GL_POLYGON_OFFSET_FILL);
    glPolygonOffset(1,4);
    glDisable(GL_TEXTURE_2D);
    glCallList(Lists[type]);
    glDisable(GL_POLYGON_OFFSET_FILL);
    glDisable(GL_LIGHTING);
    glColor3f(0,0,0);
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    glCallList(Lists[type]);
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_LIGHTING);
  }

  glutSwapBuffers();
}

void reshape(int x, int y)
{
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(65.0, (float)x / y, 0.1, 30);
  glViewport(0, 0, x, y);
  wsizex = x;
  wsizey = y;
}

void idle()
{
  glutPostRedisplay();
}

void keydown(unsigned char Key, int x, int y)
{
  if (Key == 27)
    exit(0);
	if (Key =='w')
    forward += 1;
  if (Key =='s')
    forward += -1;
  if (Key == 'a')
    right += -1;
  if (Key == 'd')
    right += 1;
  if (Key == ' ')
    type = (type + 1) % 3;
  if (Key == 't')
    usetexture = !usetexture;
  if (Key == 'c')
    showcelled = !showcelled;
  if (Key == 'm')
    mirrored = !mirrored;
}

void keyup(unsigned char Key, int x, int y)
{
	if (Key =='w')
    forward -= 1;
  if (Key =='s')
    forward -= -1;
  if (Key == 'a')
    right -= -1;
  if (Key == 'd')
    right -= 1;
}

void mousemove(int x, int y)
{
  if (!(x == wsizex / 2 && y == wsizey / 2))
  {
    phi += (x - wsizex / 2) * 0.01;
    psi += (wsizey / 2 - y) * 0.01;

    if(psi >  PI * 0.47) psi =  PI * 0.47f; 
    if(psi < -PI * 0.47) psi = -PI * 0.47f; 

    glutWarpPointer(wsizex / 2, wsizey / 2);
  }
}



int main(int argc, char** argv)
{
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE|GLUT_DEPTH|GLUT_RGB|GLUT_ALPHA);

  glutInitWindowSize(1024,768);
  glutCreateWindow("");

  glutIdleFunc(idle);
  glutPassiveMotionFunc(mousemove); 
  glutDisplayFunc(display);
  glutKeyboardFunc(keydown);
  glutKeyboardUpFunc(keyup);
  glutReshapeFunc(reshape);

  glutIgnoreKeyRepeat(GL_TRUE);

  glutSetCursor(GLUT_CURSOR_NONE);

  float vertex[25][103][3];
  float normal[25][103][3];

  Lists[0] = glGenLists(1);
	glNewList(Lists[0], GL_COMPILE);
  glBegin(GL_QUADS);
	for (int i = 0; i <= 20; i++) for (int j = 0; j <= 100; j++)
  {
    float u = 2 * PI * j / 100.0;
    float v = 2 * i / 20.0 - 1;
    vertex[i][j][0] = (1 + v / 2 * cos(u / 2)) * cos(u);
    vertex[i][j][1] = (1 + v / 2 * cos(u / 2)) * sin(u);
    vertex[i][j][2] = v / 2 * sin(u / 2);
  }
  for (int i = 0; i <= 20; i++) for (int j = 0; j <= 100; j++)
  {
    float xu = vertex[(i + 1)  % 21][j][0] - vertex[(i + 20) % 21][j][0];
    float yu = vertex[(i + 1)  % 21][j][1] - vertex[(i + 20) % 21][j][1];
    float zu = vertex[(i + 1)  % 21][j][2] - vertex[(i + 20) % 21][j][2];
    float xv = vertex[i][(j + 1)  % 101][0] - vertex[i][(j + 100) % 101][0];
    float yv = vertex[i][(j + 1)  % 101][1] - vertex[i][(j + 100) % 101][1];
    float zv = vertex[i][(j + 1)  % 101][2] - vertex[i][(j + 100) % 101][2];
    float nx = yu * zv - zu * yv;
    float ny = zu * xv - xu * zv;
    float nz = xu * yv - yu * xv;
    float length = sqrt(nx * nx + ny * ny + nz * nz);
    nx /= length;
    ny /= length;
    nz /= length;
    normal[i][j][0] = nx;
    normal[i][j][1] = ny;
    normal[i][j][2] = nz;
  }
  for (int i = 1; i < 19; i++) for (int j = 0; j < 100; j++)
  {
    glTexCoord2f(3.75 * (j    ) / 100.0, (i    ) / 20.0);
    glNormal3fv(normal[(i    ) % 21][(j    ) % 101]);
    glVertex3fv(vertex[(i    ) % 21][(j    ) % 101]);
    glTexCoord2f(3.75 * (j    ) / 100.0, (i + 1) / 20.0);
    glNormal3fv(normal[(i + 1) % 21][(j    ) % 101]);
    glVertex3fv(vertex[(i + 1) % 21][(j    ) % 101]);
    glTexCoord2f(3.75 * (j + 1) / 100.0, (i + 1) / 20.0);
    glNormal3fv(normal[(i + 1) % 21][(j + 1) % 101]);
    glVertex3fv(vertex[(i + 1) % 21][(j + 1) % 101]);
    glTexCoord2f(3.75 * (j + 1) / 100.0, (i    ) / 20.0);
    glNormal3fv(normal[(i    ) % 21][(j + 1) % 101]);
    glVertex3fv(vertex[(i    ) % 21][(j + 1) % 101]);
  }
  glEnd();
	glEndList();

  Lists[1] = glGenLists(1);
	glNewList(Lists[1], GL_COMPILE);
  glBegin(GL_QUADS);
	for (int i = 0; i <= 22; i++) for (int j = 0; j <= 102; j++)
  {
    float u = 2 * PI * (j - 1) / 100.0;
    float v = 2 * PI * (i - 1) / 20.0;
    if (u < PI) 
    {
      vertex[i][j][0] = (4 *(1 - 0.5 * cos(u)) * sin(v)) / 20;
      vertex[i][j][1] = (6 * cos(u) * (1 + sin(u)) + 4 * (1 - 0.5 * cos(u)) * cos(u) * cos(v)) / 20;
      vertex[i][j][2] = (-16 * sin(u) - 4 * (1 - 0.5 * cos(u) / 2) * sin(u) * cos(v)) / 20;
    } 
    else
    {
      vertex[i][j][0] = (4 * (1 - 0.5 * cos(u)) * sin(v)) / 20;
      vertex[i][j][1] = (6 * cos(u) * (1 + sin(u)) - 4 * (1 - 0.5 * cos(u)) * cos(v)) / 20;
      vertex[i][j][2] = (-16 * sin(u)) / 20;
    }      
  }
  for (int i = 1; i <= 21; i++) for (int j = 1; j <= 101; j++)
  {
    float xu = vertex[(i + 1)  % 23][j][0] - vertex[(i + 22) % 23][j][0];
    float yu = vertex[(i + 1)  % 23][j][1] - vertex[(i + 22) % 23][j][1];
    float zu = vertex[(i + 1)  % 23][j][2] - vertex[(i + 22) % 23][j][2];
    float xv = vertex[i][(j + 1) % 103][0] - vertex[i][(j + 102) % 103][0];
    float yv = vertex[i][(j + 1) % 103][1] - vertex[i][(j + 102) % 103][1];
    float zv = vertex[i][(j + 1) % 103][2] - vertex[i][(j + 102) % 103][2];
    float nx = yu * zv - zu * yv;
    float ny = zu * xv - xu * zv;
    float nz = xu * yv - yu * xv;
    float length = sqrt(nx * nx + ny * ny + nz * nz);
    nx /= length;
    ny /= length;
    nz /= length;
    normal[i][j][0] = nx;
    normal[i][j][1] = ny;
    normal[i][j][2] = nz;
  }
  for (int i = 1; i < 21; i++) for (int j = 1; j < 101; j++)
  {
    glTexCoord2f(4 * (j - 1) / 100.0, (i    ) / 20.0 - (j - 1) / 295.0);
    glNormal3fv(normal[(i    ) % 23][(j    ) % 103]);
    glVertex3fv(vertex[(i    ) % 23][(j    ) % 103]);
    glTexCoord2f(4 * (j - 1) / 100.0, (i + 1) / 20.0 - (j - 1) / 295.0);
    glNormal3fv(normal[(i + 1) % 23][(j    ) % 103]);
    glVertex3fv(vertex[(i + 1) % 23][(j    ) % 103]);
    glTexCoord2f(4 * (j    ) / 100.0, (i + 1) / 20.0 - (j - 1) / 295.0);
    glNormal3fv(normal[(i + 1) % 23][(j + 1) % 103]);
    glVertex3fv(vertex[(i + 1) % 23][(j + 1) % 103]);
    glTexCoord2f(4 * (j    ) / 100.0, (i    ) / 20.0 - (j - 1) / 295.0);
    glNormal3fv(normal[(i    ) % 23][(j + 1) % 103]);
    glVertex3fv(vertex[(i    ) % 23][(j + 1) % 103]);
  }
  glEnd();
	glEndList();

  Lists[2] = glGenLists(1);
	glNewList(Lists[2], GL_COMPILE);
  glBegin(GL_QUADS);
	for (int i = 0; i <= 22; i++) for (int j = 0; j <= 102; j++)
  {
    float u = 2 * PI * (j - 1) / 100.0;
    float v = 2 * PI * (i - 1) / 20.0;
    vertex[i][j][0] = ((2 + cos(u / 2) * sin(v) - sin(u / 2) * sin(2 * v)) * cos(u)) / 3; 
    vertex[i][j][1] = ((2 + cos(u / 2) * sin(v) - sin(u / 2) * sin(2 * v)) * sin(u)) / 3;
    vertex[i][j][2] = (sin(u / 2) * sin(v) + cos(u / 2) * sin(2 * v)) / 3;
  }
  for (int i = 0; i <= 22; i++) for (int j = 0; j <= 102; j++)
  {
    float xu = vertex[(i + 1)  % 23][j][0] - vertex[(i + 22) % 23][j][0];
    float yu = vertex[(i + 1)  % 23][j][1] - vertex[(i + 22) % 23][j][1];
    float zu = vertex[(i + 1)  % 23][j][2] - vertex[(i + 22) % 23][j][2];
    float xv = vertex[i][(j + 1)  % 103][0] - vertex[i][(j + 102) % 103][0];
    float yv = vertex[i][(j + 1)  % 103][1] - vertex[i][(j + 102) % 103][1];
    float zv = vertex[i][(j + 1)  % 103][2] - vertex[i][(j + 102) % 103][2];
    float nx = yu * zv - zu * yv;
    float ny = zu * xv - xu * zv;
    float nz = xu * yv - yu * xv;
    float length = sqrt(nx * nx + ny * ny + nz * nz);
    nx /= length;
    ny /= length;
    nz /= length;
    normal[i][j][0] = nx;
    normal[i][j][1] = ny;
    normal[i][j][2] = nz;
  }
  for (int i = 1; i < 21; i++) for (int j = 1; j < 101; j++)
  {
    glTexCoord2f(4 * (j - 1) / 100.0, (i    ) / 20.0);
    glNormal3fv(normal[(i    ) % 23][(j    ) % 103]);
    glVertex3fv(vertex[(i    ) % 23][(j    ) % 103]);
    glTexCoord2f(4 * (j - 1) / 100.0, (i + 1) / 20.0);
    glNormal3fv(normal[(i + 1) % 23][(j    ) % 103]);
    glVertex3fv(vertex[(i + 1) % 23][(j    ) % 103]);
    glTexCoord2f(4 * (j    ) / 100.0, (i + 1) / 20.0);
    glNormal3fv(normal[(i + 1) % 23][(j + 1) % 103]);
    glVertex3fv(vertex[(i + 1) % 23][(j + 1) % 103]);
    glTexCoord2f(4 * (j    ) / 100.0, (i    ) / 20.0);
    glNormal3fv(normal[(i    ) % 23][(j + 1) % 103]);
    glVertex3fv(vertex[(i    ) % 23][(j + 1) % 103]);
  }
  glEnd();
	glEndList();

  glEnable(GL_DEPTH_TEST);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_POINT_SMOOTH);
  glEnable(GL_NORMALIZE);
  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);

  GLfloat m_emissive[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
	GLfloat m_diffuse[4] = { 0.4f, 0.4f, 0.4f, 0.0f };
	GLfloat m_specular[4] = { 0.5f, 0.5f, 0.5f, 0.0f };
	GLfloat m_ambient[4] = { 0.3f, 0.3f, 0.3f, 0.0f };
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, m_emissive);
	glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);
	glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, m_specular);
	glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, m_ambient);
	glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 50);
  GLfloat l_diffuse[4] = { 1.0f, 1.0f, 1.0f, 0.0f };
	GLfloat l_specular[4] = { 1.0f, 1.0f, 1.0f, 0.0f };
	GLfloat l_ambient[4] = { 1.0f, 1.0f, 1.0f, 0.0f };
	glLightfv(GL_LIGHT0, GL_DIFFUSE, l_diffuse);
	glLightfv(GL_LIGHT0, GL_SPECULAR, l_specular);
	glLightfv(GL_LIGHT0, GL_AMBIENT, l_ambient);
	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);
	glShadeModel(GL_SMOOTH);

  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
	glGenTextures(3,Textures);
  int width, height;
  unsigned char *bitmap;
	bitmap = LoadTrueColorBMPFile("Grid.bmp", &width, &height);
	glBindTexture(GL_TEXTURE_2D, Textures[0]);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, width, height, GL_RGB, GL_UNSIGNED_BYTE, bitmap);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  delete[] bitmap;
	bitmap = LoadTrueColorBMPFile("Sky.bmp", &width, &height);
	glBindTexture(GL_TEXTURE_2D, Textures[1]);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, width, height, GL_RGB, GL_UNSIGNED_BYTE, bitmap);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  delete[] bitmap;

  DEVMODE devmode;        
  memset(&devmode, 0, sizeof(devmode));
  devmode.dmSize = sizeof(devmode);  
  devmode.dmBitsPerPel = 32;
  devmode.dmPelsWidth = 1024;    
  devmode.dmPelsHeight = 768;    
  devmode.dmFields = DM_PELSWIDTH | DM_BITSPERPEL | DM_PELSHEIGHT;
  ChangeDisplaySettings(&devmode, CDS_FULLSCREEN);
  glutFullScreen();  

  Sky = gluNewQuadric();
  gluQuadricTexture(Sky, GL_TRUE);


  glutMainLoop();
  return 0;
}