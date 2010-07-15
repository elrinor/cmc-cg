#include <windows.h>
#include <gl\glut.h>
#include <stdio.h>
#include <math.h>
#include "loadbmp.h"

#define PI 3.141592653589793238462643383279

int window_width, window_height;

GLuint texture;

GLuint DispList[3];

float transform[16]={1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1};
                    
int index=0;

int wire=0;

void GetVertex1(float u, float v, float& x, float& y, float& z)
{
  x=0.8*v*sin(0.5*u)*0.5;
  y=0.8*sin(u)*(1+v*cos(0.5*u)*0.5);
  z=0.8*cos(u)*(1+v*cos(0.5*u)*0.5);
}

void GetPoint1(float u, float v, float& x, float& y, float& z, float& nx, float& ny, float& nz)
{
  GetVertex1(u,v,x,y,z);
  float dx1, dy1, dz1, dx2, dy2, dz2, x1, y1, z1, x2, y2, z2;
  GetVertex1(u+0.00001,v,x1,y1,z1);
  GetVertex1(u-0.00001,v,x2,y2,z2);
  dx1=x1-x2;
  dy1=y1-y2;
  dz1=z1-z2;
  GetVertex1(u,v+0.00001,x1,y1,z1);
  GetVertex1(u,v-0.00001,x2,y2,z2);
  dx2=x1-x2;
  dy2=y1-y2;
  dz2=z1-z2;
  nx=-(dy1*dz2-dz1*dy2);
  ny=-(dz1*dx2-dx1*dz2);
  nz=-(dx1*dy2-dy1*dx2);
}

void GetVertex2(float u, float v, float& x, float& y, float& z)
{
  if(u<PI)
  { 
    x=4*1*(1-0.5*cos(u))*sin(v);
    y=6*cos(u)*(1+sin(u))+4*1*(1-0.5*cos(u))*cos(u)*cos(v);
    z=16* sin(u)+4*1*(1-0.5*cos(u))*sin(u)*cos(v);
  }
  else
  { 
    x=4*1*(1-0.5*cos(u))*sin(v);
    y=6*cos(u)*(1+sin(u))-4*1*(1-0.5*cos(u))*cos(v);
    z=16*sin(u);
  } 
  x*=0.06;
  y*=-0.06;
  z*=-0.06;
}

void GetPoint2(float u, float v, float& x, float& y, float& z, float& nx, float& ny, float& nz)
{           
  GetVertex2(u,v,x,y,z);
  float dx1, dy1, dz1, dx2, dy2, dz2, x1, y1, z1, x2, y2, z2;
  GetVertex2(u+0.00001,v,x1,y1,z1);
  GetVertex2(u-0.00001,v,x2,y2,z2);
  dx1=x1-x2;
  dy1=y1-y2;
  dz1=z1-z2;
  GetVertex2(u,v+0.00001,x1,y1,z1);
  GetVertex2(u,v-0.00001,x2,y2,z2);
  dx2=x1-x2;
  dy2=y1-y2;
  dz2=z1-z2;
  nx=-(dy1*dz2-dz1*dy2);
  ny=-(dz1*dx2-dx1*dz2);
  nz=-(dx1*dy2-dy1*dx2);
}

void GetVertex3(float u, float v, float& x, float& y, float& z)
{
  x=0.2*(sin(0.5*u)*sin(v)+cos(0.5*u)*sin(2*v));
  y=0.2*((4+cos(0.5*u)*sin(v)-sin(0.5*u)*sin(2*v))*cos(u));
  z=0.2*((4+cos(0.5*u)*sin(v)-sin(0.5*u)*sin(2*v))*sin(u));
}

void GetPoint3(float u, float v, float& x, float& y, float& z, float& nx, float& ny, float& nz)
{
  GetVertex3(u,v,x,y,z);
  float dx1, dy1, dz1, dx2, dy2, dz2, x1, y1, z1, x2, y2, z2;
  GetVertex3(u+0.00001,v,x1,y1,z1);
  GetVertex3(u-0.00001,v,x2,y2,z2);
  dx1=x1-x2;
  dy1=y1-y2;
  dz1=z1-z2;
  GetVertex3(u,v+0.00001,x1,y1,z1);
  GetVertex3(u,v-0.00001,x2,y2,z2);
  dx2=x1-x2;
  dy2=y1-y2;
  dz2=z1-z2;
  nx=-(dy1*dz2-dz1*dy2);
  ny=-(dz1*dx2-dx1*dz2);
  nz=-(dx1*dy2-dy1*dx2);
}

void display()
{
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_STENCIL_BUFFER_BIT);
	
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(80.0,(float)window_width/window_height,1,10);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	gluLookAt(4,0,0,0,0,0,0,0,1);

  GLfloat l[4]={1.5*sin(0.001*GetTickCount()),1.5*cos(0.001*GetTickCount()),cos(0.0002*GetTickCount()),1};
  glLightfv(GL_LIGHT0,GL_POSITION,l);

  glPointSize(5);
  glEnable(GL_POINT_SMOOTH);
  glDisable(GL_LIGHTING);
  glDisable(GL_TEXTURE_2D);
  glBegin(GL_POINTS);
    glColor3f(0.2,0.2,1);
    glVertex4fv(l);
  glEnd();
  glEnable(GL_LIGHTING);
  glEnable(GL_TEXTURE_2D);

  if(!wire)
  {
    glPushMatrix();
    glMultMatrixf(transform);
    glCallList(DispList[index]);
    glPopMatrix();
  }
  else
  {
    glDisable(GL_TEXTURE_2D);
    glPushMatrix();
    glMultMatrixf(transform);
    glEnable(GL_POLYGON_OFFSET_FILL);
    glPolygonOffset(2,3);
    glCallList(DispList[index]);
    glDisable(GL_POLYGON_OFFSET_FILL);
    glDisable(GL_LIGHTING);
    glColor3f(0,0,0);
    glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
    glCallList(DispList[index]);
    glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
    glEnable(GL_LIGHTING);
    glPopMatrix();
    glEnable(GL_TEXTURE_2D);
  }
	
	glutSwapBuffers();
}

void reshape(int x, int y)
{
	window_width=x;
	window_height=y;
	glViewport(0,0,window_width,window_height);
}

void idle()
{
	glutPostRedisplay();
}

void keydown(unsigned char Key, int x, int y)
{
  if(Key==27)
	  exit(0);
  if(Key=='+')
    index++;
  if(Key=='-')
    index--;
  index=(index+3)%3;
  if(Key=='w')
    wire++;
  wire%=2;
}

void mousemove(int x, int y)
{
  if(x!=window_width/2 || y!=window_height/2)
  {
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();
    glLoadIdentity();
    glRotatef((x-window_width/2)*0.15,0,0,1);
    glRotatef((y-window_height/2)*0.15,0,1,0);
    glMultMatrixf(transform);
    glGetFloatv(GL_MODELVIEW_MATRIX, transform);
    glutWarpPointer(window_width/2,window_height/2);
  }
}

void do_init()
{
	glClearColor(0,0,0,0); 
  glEnable(GL_DEPTH_TEST); 
  glDisable(GL_CULL_FACE);
  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
  glShadeModel(GL_SMOOTH);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glEnable(GL_NORMALIZE);
  
  GLfloat m_emissive[4]={0.2,0.2,0.2,0.0};
  GLfloat m_diffuse[4] ={0.6,0.5,0.4,0.0};
  GLfloat m_specular[4]={0.5,0.6,0.5,0.0};
  GLfloat m_ambient[4] ={0.0,0.0,0.0,0.0};
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, m_emissive);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, m_specular);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, m_ambient);
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 30);

	glEnable(GL_LIGHT0);
  glEnable(GL_LIGHTING);
	GLfloat l_position[4]={0.0,0.0,0.0,1.0};
  GLfloat l_diffuse[4] ={0.2,0.3,2.0,1.0};
  GLfloat l_specular[4]={0.5,0.5,0.5,1.0};
  GLfloat l_ambient[4] ={0.0,0.0,0.0,1.0};
  glLightfv(GL_LIGHT0,GL_POSITION,l_position);
  glLightfv(GL_LIGHT0,GL_DIFFUSE,l_diffuse);
  glLightfv(GL_LIGHT0,GL_SPECULAR,l_specular);
  glLightfv(GL_LIGHT0,GL_AMBIENT,l_ambient);
  glLightModelf(GL_LIGHT_MODEL_TWO_SIDE, 1);

	glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
  glEnable(GL_TEXTURE_2D);
  IMAGE bitmap;
  LoadBMP("texture.bmp", &bitmap);
  glPixelStorei(GL_UNPACK_ALIGNMENT,1);
  glGenTextures(1,&texture);
  glBindTexture(GL_TEXTURE_2D,texture);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, bitmap.width,bitmap.height, GL_RGB, GL_UNSIGNED_BYTE, bitmap.data);             
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);

  DispList[0]=glGenLists(1);
  DispList[1]=glGenLists(1);
  DispList[2]=glGenLists(1);

  glNewList(DispList[0],GL_COMPILE);
  glBegin(GL_QUADS);
  for(int i=0; i<100; i++) for(int j=0; j<25; j++)
  {
    float x, y, z, nx, ny, nz, u, v;
    float du=2*PI*1/100.0, dv=2*1/25.0;
    u=2*PI*i/100.0, v=2*j/25.0-1;
    GetPoint1(u,v,x,y,z,nx,ny,nz);
    glNormal3f(nx,ny,nz); glTexCoord2f(v,4*0.5*(u)/PI);glVertex3f(x,y,z);
    GetPoint1(u,v+dv,x,y,z,nx,ny,nz);
    glNormal3f(nx,ny,nz); glTexCoord2f((v+dv),4*0.5*(u)/PI);glVertex3f(x,y,z);
    GetPoint1(u+du,v+dv,x,y,z,nx,ny,nz);
    glNormal3f(nx,ny,nz); glTexCoord2f((v+dv),4*0.5*(u+du)/PI);glVertex3f(x,y,z);
    GetPoint1(u+du,v,x,y,z,nx,ny,nz);
    glNormal3f(nx,ny,nz); glTexCoord2f(v,4*0.5*(u+du)/PI);glVertex3f(x,y,z);
  }
  glEnd();
  glEndList();
  glNewList(DispList[1],GL_COMPILE);
  glBegin(GL_QUADS);
  for(int i=0; i<100; i++) for(int j=0; j<25; j++)
  {
    float x, y, z, nx, ny, nz, u, v;
    float du=2*PI*1/100.0, dv=2*PI*1/25.0;
    u=2*PI*i/100.0, v=2*PI*j/25.0;
    GetPoint2(u,v,x,y,z,nx,ny,nz);
    glNormal3f(nx,ny,nz); glTexCoord2f(0.5*(u)/PI,0.5*(v)/PI);glVertex3f(x,y,z);
    GetPoint2(u,v+dv,x,y,z,nx,ny,nz);
    glNormal3f(nx,ny,nz); glTexCoord2f(0.5*(u)/PI,0.5*(v+dv)/PI);glVertex3f(x,y,z);
    GetPoint2(u+du,v+dv,x,y,z,nx,ny,nz);
    glNormal3f(nx,ny,nz); glTexCoord2f(0.5*(u+du)/PI,0.5*(v+dv)/PI);glVertex3f(x,y,z);
    GetPoint2(u+du,v,x,y,z,nx,ny,nz);
    glNormal3f(nx,ny,nz); glTexCoord2f(0.5*(u+du)/PI,0.5*(v)/PI);glVertex3f(x,y,z);
  }
  glEnd();
  glEndList();
    glNewList(DispList[2],GL_COMPILE);
  glBegin(GL_QUADS);
  for(int i=0; i<100; i++) for(int j=0; j<25; j++)
  {
    float x, y, z, nx, ny, nz, u, v;
    float du=2*PI*1/100.0, dv=2*PI*1/25.0;
    u=2*PI*i/100.0, v=2*PI*j/25.0;
    GetPoint3(u,v,x,y,z,nx,ny,nz);
    glNormal3f(nx,ny,nz); glTexCoord2f(0.5*(u)/PI,0.5*(v)/PI);glVertex3f(x,y,z);
    GetPoint3(u,v+dv,x,y,z,nx,ny,nz);
    glNormal3f(nx,ny,nz); glTexCoord2f(0.5*(u)/PI,0.5*(v+dv)/PI);glVertex3f(x,y,z);
    GetPoint3(u+du,v+dv,x,y,z,nx,ny,nz);
    glNormal3f(nx,ny,nz); glTexCoord2f(0.5*(u+du)/PI,0.5*(v+dv)/PI);glVertex3f(x,y,z);
    GetPoint3(u+du,v,x,y,z,nx,ny,nz);
    glNormal3f(nx,ny,nz); glTexCoord2f(0.5*(u+du)/PI,0.5*(v)/PI);glVertex3f(x,y,z);
  }
  glEnd();
  glEndList();
}

int main(int argc, char** argv)
{
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE|GLUT_DEPTH|GLUT_RGB);
  glutInitWindowSize(512,512);
  glutCreateWindow("Assignment4");
  glutDisplayFunc(display);
  glutReshapeFunc(reshape);
  glutIdleFunc(idle);
  glutKeyboardFunc(keydown);
  glutPassiveMotionFunc(mousemove); 
  glutSetCursor(GLUT_CURSOR_NONE);
  do_init();

  glutFullScreen();  

	glutMainLoop();
  return 0;
}
