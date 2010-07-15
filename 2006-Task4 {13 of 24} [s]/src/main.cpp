#include <windows.h>
#include <gl\glut.h>
#include <math.h>
#include "loadbmp.h"

#define PI 3.141592

int windoww, windowh;

GLuint M_DList;
GLuint K_DList;
GLuint K8_DList;
GLuint Texture;

float xa = 0, ya = 0, ll = 0;

int model = 0;

bool setka = false;


void Point_K8(float u, float v, float& x, float& y, float& z)
{
  x = ((3 + cos(u / 2) * sin(v) - sin(u / 2) * sin(2 * v)) * cos(u)) / 3;
  y = ((3 + cos(u / 2) * sin(v) - sin(u / 2) * sin(2 * v)) * sin(u)) / 3;
  z = (sin(u / 2) * sin(v) + cos(u / 2) * sin(2 * v)) / 3;
}

void Point_K(float u, float v, float& x, float& y, float& z)
{
  if (u<PI)
  {
    x = 0.07 * (4 * 1.2 * (1 - cos(u) / 2) * sin(v));
    y = 0.07 * (6 * cos(u) * (1 + sin(u)) + 4 * 1.2 * (1 - cos(u) / 2) * cos(u) * cos(v));
    z = -0.07 * (16 * sin(u) + 4 * 1.2 * (1 - cos(u) / 2) * sin(u) * cos(v));
  }
  else
  {
    x = 0.07 * (4 * 1.2 * (1 - cos(u) / 2) * sin(v));
    y = 0.07 * (6 * cos(u) * (1 + sin(u)) - 4 * 1.2 * (1 - cos(u) / 2) * cos(v));
    z = -0.07 * (16 * sin(u));
  }
}

void Point_M(float u, float v, float& x, float& y, float& z)
{
  x = (v * cos(u / 2) / 2 + 1) * cos(u);
  y = (v * cos(u / 2) / 2 + 1) * sin(u);
  z =  v * sin(u / 2) / 2;
}


void PointNormal(float u, float v, void PointFunc(float u, float v, float& x, float& y, float& z), float& x, float& y, float& z)
{
  float xduu, yduu, zduu, xdud, ydud, zdud, xdvu, ydvu, zdvu, xdvd, ydvd, zdvd;
  PointFunc(u + 0.001, v, xduu, yduu, zduu);
  PointFunc(u - 0.001, v, xdud, ydud, zdud);
  PointFunc(u, v + 0.001, xdvu, ydvu, zdvu);
  PointFunc(u, v - 0.001, xdvd, ydvd, zdvd);
  float xdu = xduu - xdud;
  float ydu = yduu - ydud;
  float zdu = zduu - zdud;
  float xdv = xdvu - xdvd;
  float ydv = ydvu - ydvd;
  float zdv = zdvu - zdvd;
  x = ydu * zdv - zdu * ydv;
  y = zdu * xdv - xdu * zdv;
  z = xdu * ydv - ydu * xdv;
  float l = sqrt(x * x + y * y + z * z);
  x /= l;
  y /= l;
  z /= l;
}

void reshapefunc(int neww, int newh)
{
  glViewport(0, 0, neww, newh);
  windoww = neww;
  windowh = newh;
}

void drawfunc()
{
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(90.0, (float)windoww / windowh, 1, 20);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(3 * sin(xa) * cos(ya), 3 * cos(xa) * cos(ya), 3 * sin(ya),0,0,0,0,0,1);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	GLfloat light1[4] = {1.5 * sin (ll), 1.5 * cos(ll), 1, 1};
	GLfloat light2[4] = {-1.5 * sin (ll), -1.5 * cos(ll), -1, 1};
	glLightfv(GL_LIGHT0, GL_POSITION, light1);
	glLightfv(GL_LIGHT1, GL_POSITION, light2);

  glPointSize(6);
  glDisable(GL_LIGHTING);
  glDisable(GL_TEXTURE_2D);
  glBegin(GL_POINTS);
    glColor3f(1, 0, 0);
    glVertex3f(light1[0], light1[1], light1[2]);
    glColor3f(0, 0, 1);
    glVertex3f(light2[0], light2[1], light2[2]);
  glEnd();
  glEnable(GL_LIGHTING);
  glEnable(GL_TEXTURE_2D);

  if (setka)
  {
    glDisable(GL_TEXTURE_2D);
    glEnable(GL_POLYGON_OFFSET_FILL);
    glPolygonOffset(1, 4);
    if (model == 0)
      glCallList(M_DList);
    if (model == 1)
      glCallList(K_DList);
    if (model == 2)
      glCallList(K8_DList);    
    glDisable(GL_POLYGON_OFFSET_FILL);
    glDisable(GL_LIGHTING);
    glColor3f(0,0,0);
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    if (model == 0)
      glCallList(M_DList);
    if (model == 1)
      glCallList(K_DList);
    if (model == 2)
      glCallList(K8_DList);    
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    glEnable(GL_LIGHTING);
    glEnable(GL_TEXTURE_2D);
    }
  else
  {
    if (model == 0)
      glCallList(M_DList);
    if (model == 1)
      glCallList(K_DList);
    if (model == 2)
      glCallList(K8_DList);
  }

  glutSwapBuffers();
}

void keyfunc(unsigned char key, int x, int y)
{
  if(key == 27)
    exit(0);
  if (key == 'a')
    xa += 0.1;
  if (key == 'd')
    xa -= 0.1;
  if (key == 'w')
    ya += 0.1;
  if (key == 's')
    ya -= 0.1;
  if (key == 'e')
    model += 1;
  if (key == 'q')
    model += 2;
  if (key == 'z')
    ll += 0.1;
  if (key == 'c')
    ll -= 0.1;
  if (key == 'x')
    setka = !setka;

  model %= 3;
  if (ya < -PI / 2)
    ya = -PI / 2 + 0.000001;
  if (ya > PI / 2)
    ya = PI / 2 - 0.000001;
}

void idlefunc()
{
  glutPostRedisplay();
}

int main(int argc, char** argv)
{
  glutInit(&argc,argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
  glutInitWindowSize(700, 700);
  glutCreateWindow("One-Sided Surfaces");

  glutReshapeFunc(reshapefunc);
  glutDisplayFunc(drawfunc);
  glutKeyboardFunc(keyfunc);
  glutIdleFunc(idlefunc);

  glClearColor(0, 0, 0, 0); 
  glHint(GL_POINT_SMOOTH_HINT, GL_NICEST); 
  glEnable(GL_POINT_SMOOTH); 
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); 
  glEnable(GL_DEPTH_TEST);

  K8_DList = glGenLists(1);
  glNewList(K8_DList, GL_COMPILE);
  glBegin(GL_QUADS);
  for(int i = 0; i < 40; i++) for(int j = 0; j < 40; j++)
  {
    float x, y, z, nx, ny, nz, u, v;
    u = 2 * PI * i / 40.0, v = 2 * PI * j / 40.0;
    PointNormal(u, v, Point_K8, nx, ny, nz); Point_K8(u, v, x, y, z);
    glNormal3f(nx, ny, nz); glTexCoord2f(i / 20.0, j / 40.0); glVertex3f(x, y, z);
    u = 2 * PI * (i + 1) / 40.0, v = 2 * PI * j / 40.0;
    PointNormal(u, v, Point_K8, nx, ny, nz); Point_K8(u, v, x, y, z);
    glNormal3f(nx, ny, nz); glTexCoord2f((i + 1) / 20.0, j / 40.0); glVertex3f(x, y, z);
    u = 2 * PI * (i + 1) / 40.0, v = 2 * PI * (j + 1) / 40.0;
    PointNormal(u, v, Point_K8, nx, ny, nz); Point_K8(u, v, x, y, z);
    glNormal3f(nx, ny, nz); glTexCoord2f((i + 1) / 20.0, (j + 1) / 40.0); glVertex3f(x, y, z);
    u = 2 * PI * i / 40.0, v = 2 * PI * (j + 1) / 40.0;
    PointNormal(u, v, Point_K8, nx, ny, nz); Point_K8(u, v, x, y, z);
    glNormal3f(nx, ny, nz); glTexCoord2f(i / 20.0, (j + 1) / 40.0); glVertex3f(x, y, z);
  }
  glEnd();
  glEndList();

  K_DList = glGenLists(1);
  glNewList(K_DList, GL_COMPILE);
  glBegin(GL_QUADS);
  for(int i = 0; i < 100; i++) for(int j = 0; j < 20; j++)
  {
    float x, y, z, nx, ny, nz, u, v;
    u = 2 * PI * i / 100.0, v = 2 * PI * j / 20.0;
    PointNormal(u, v, Point_K, nx, ny, nz); Point_K(u, v, x, y, z);
    glNormal3f(nx, ny, nz); glTexCoord2f(i / 50.0, j / 20.0); glVertex3f(x, y, z);
    u = 2 * PI * (i + 1) / 100.0, v = 2 * PI * j / 20.0;
    PointNormal(u, v, Point_K, nx, ny, nz); Point_K(u, v, x, y, z);
    glNormal3f(nx, ny, nz); glTexCoord2f((i + 1) / 50.0, j / 20.0); glVertex3f(x, y, z);
    u = 2 * PI * (i + 1) / 100.0, v = 2 * PI * (j + 1) / 20.0;
    PointNormal(u, v, Point_K, nx, ny, nz); Point_K(u, v, x, y, z);
    glNormal3f(nx, ny, nz); glTexCoord2f((i + 1) / 50.0, (j + 1) / 20.0); glVertex3f(x, y, z);
    u = 2 * PI * i / 100.0, v = 2 * PI * (j + 1) / 20.0;
    PointNormal(u, v, Point_K, nx, ny, nz); Point_K(u, v, x, y, z);
    glNormal3f(nx, ny, nz); glTexCoord2f(i / 50.0, (j + 1) / 20.0); glVertex3f(x, y, z);
  }
  glEnd();
  glEndList();

  M_DList = glGenLists(1);
  glNewList(M_DList, GL_COMPILE);
  glBegin(GL_QUADS);
  for(int i = 0; i < 100; i++) for(int j = 0; j < 20; j++)
  {
    float x, y, z, nx, ny, nz, u, v;
    u = 2 * PI * i / 100.0, v = -1 + 2 * j / 20.0;
    PointNormal(u, v, Point_M, nx, ny, nz); Point_M(u, v, x, y, z);
    glNormal3f(nx, ny, nz); glTexCoord2f(i / 50.0, j / 20.0); glVertex3f(x, y, z);
    u = 2 * PI * (i + 1) / 100.0, v = -1 + 2 * j / 20.0;
    PointNormal(u, v, Point_M, nx, ny, nz); Point_M(u, v, x, y, z);
    glNormal3f(nx, ny, nz); glTexCoord2f((i + 1) / 50.0, j / 20.0); glVertex3f(x, y, z);
    u = 2 * PI * (i + 1) / 100.0, v = -1 + 2 * (j + 1) / 20.0;
    PointNormal(u, v, Point_M, nx, ny, nz); Point_M(u, v, x, y, z);
    glNormal3f(nx, ny, nz); glTexCoord2f((i + 1) / 50.0, (j + 1) / 20.0); glVertex3f(x, y, z);
    u = 2 * PI * i / 100.0, v = -1 + 2 * (j + 1) / 20.0;
    PointNormal(u, v, Point_M, nx, ny, nz); Point_M(u, v, x, y, z);
    glNormal3f(nx, ny, nz); glTexCoord2f(i / 50.0, (j + 1) / 20.0); glVertex3f(x, y, z);
  }
  glEnd();
  glEndList();

  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);

  GLfloat m_emissive[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
	GLfloat m_diffuse[4] = { 0.5f, 0.5f, 0.5f, 0.0f };
	GLfloat m_specular[4] = { 0.5f, 0.5f, 0.5f, 0.0f };
	GLfloat m_ambient[4] = { 0.5f, 0.5f, 0.5f, 0.0f };
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, m_emissive);
	glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);
	glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, m_specular);
	glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, m_ambient);
	glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 40);
  GLfloat l1_diffuse[4] = { 1.0f, 0.0f, 0.0f, 0.0f };
	GLfloat l1_specular[4] = { 1.0f, 0.0f, 0.0f, 0.0f };
	GLfloat l1_ambient[4] = { 0.5f, 0.5f, 0.5f, 0.0f };
	glLightfv(GL_LIGHT0, GL_DIFFUSE, l1_diffuse);
	glLightfv(GL_LIGHT0, GL_SPECULAR, l1_specular);
	glLightfv(GL_LIGHT0, GL_AMBIENT, l1_ambient);
  GLfloat l2_diffuse[4] = { 0.0f, 0.0f, 1.0f, 0.0f };
	GLfloat l2_specular[4] = { 0.0f, 0.0f, 1.0f, 0.0f };
	GLfloat l2_ambient[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
	glLightfv(GL_LIGHT1, GL_DIFFUSE, l2_diffuse);
	glLightfv(GL_LIGHT1, GL_SPECULAR, l2_specular);
	glLightfv(GL_LIGHT1, GL_AMBIENT, l2_ambient);
	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);
	glEnable(GL_LIGHT1);
	glShadeModel(GL_SMOOTH);

  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  glEnable(GL_TEXTURE_2D);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);
  IMAGE file;
  glPixelStorei(GL_UNPACK_ALIGNMENT,1);
  glGenTextures(1,&Texture);

  glBindTexture(GL_TEXTURE_2D,Texture);
  LoadBMP("tex.bmp", &file);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, file.width,file.height, GL_RGB, GL_UNSIGNED_BYTE, file.data);             
  free(file.data);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);

  glutMainLoop();
  return 0;
}
