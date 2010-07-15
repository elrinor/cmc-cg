#include <gl\glut.h>
#include <gl\glext.h>
#include <windows.h>
#include <math.h>
#include <gl\glaux.h>
#include <stdio.h>
#include <stdlib.h>

#include "libtexture\libtexture.h"

#define PI 3.1415

typedef union{
	float v[3];
	struct{
		float x,y,z;
	};
} vector_t;

typedef struct{
  vector_t v[4],n[4],t[4];
  vector_t c;
} quad_t;

int max_x, max_y;
float t=0, dt;
bool wk=false, ak=false, dk=false, sk=false;
vector_t cam;
float cam_fi=0, cam_psi=0;

GLuint Texture;

const int QSIZE=5000;
quad_t quads[QSIZE];
int q_index[QSIZE];
float q_dist[QSIZE];
int q_n=0;

inline float sqr(float a) { return a*a; }

int cmp(const void* p1, const void* p2)
{
  if(q_dist[*((int*)p1)]<q_dist[*((int*)p2)])
    return 1;
  else
    return -1;
}
void ObjectVertexXY(vector_t *v, vector_t *n, vector_t *t, float phi, float psi, float r1, float r2)
{
	n->x=cos(phi)*cos(psi);
	n->y=sin(phi)*cos(psi);
	n->z=sin(psi);
	v->x=r1*cos(phi)+r2*n->x;
  v->y=r1*sin(phi)+r2*n->y;
  v->z=r2*n->z;
  t->x=phi/(2*PI);
  t->y=psi/(2*PI);
  t->z=0;
}
void ObjectVertexZX(vector_t *v, vector_t *n, vector_t *t, float phi, float psi, float r1, float r2)
{
	n->x=cos(phi)*cos(psi);
	n->y=sin(psi);
	n->z=sin(phi)*cos(psi);
	v->x=r1*cos(phi)+r2*n->x+r1;
  v->y=r2*n->y;
  v->z=r1*sin(phi)+r2*n->z;
  t->x=phi/(2*PI);
  t->y=psi/(2*PI);
  t->z=0;
}
void Generate()
{
  const int MAXN1=64;
  const int MAXN2=32;
  const double R1=1;
  const double R2=0.3;
  for(int i=0; i<MAXN1; i++)
  {
    double fi1 = 2*i*PI/MAXN1;
		double fi2 = 2*(i+1)*PI/MAXN1;
    for(int j=0; j<MAXN2; j++)
    {
      double psi1 = 2*j*PI/MAXN2;
			double psi2 = 2*(j+1)*PI/MAXN2;
      ObjectVertexXY(&(quads[q_n].v[0]),&(quads[q_n].n[0]),&(quads[q_n].t[0]), fi1, psi1, R1, R2);
      ObjectVertexXY(&(quads[q_n].v[1]),&(quads[q_n].n[1]),&(quads[q_n].t[1]), fi2, psi1, R1, R2);
      ObjectVertexXY(&(quads[q_n].v[2]),&(quads[q_n].n[2]),&(quads[q_n].t[2]), fi2, psi2, R1, R2);
      ObjectVertexXY(&(quads[q_n].v[3]),&(quads[q_n].n[3]),&(quads[q_n].t[3]), fi1, psi2, R1, R2);
      quads[q_n].c.x=(quads[q_n].v[0].x+quads[q_n].v[1].x+quads[q_n].v[2].x+quads[q_n].v[3].x)/4;
      quads[q_n].c.y=(quads[q_n].v[0].y+quads[q_n].v[1].y+quads[q_n].v[2].y+quads[q_n].v[3].y)/4;
      quads[q_n].c.z=(quads[q_n].v[0].z+quads[q_n].v[1].z+quads[q_n].v[2].z+quads[q_n].v[3].z)/4;
      q_n++;
      ObjectVertexZX(&(quads[q_n].v[0]),&(quads[q_n].n[0]),&(quads[q_n].t[0]), fi1, psi2, R1, R2);
      ObjectVertexZX(&(quads[q_n].v[1]),&(quads[q_n].n[1]),&(quads[q_n].t[1]), fi2, psi2, R1, R2);
      ObjectVertexZX(&(quads[q_n].v[2]),&(quads[q_n].n[2]),&(quads[q_n].t[2]), fi2, psi1, R1, R2);
      ObjectVertexZX(&(quads[q_n].v[3]),&(quads[q_n].n[3]),&(quads[q_n].t[3]), fi1, psi1, R1, R2);
      quads[q_n].c.x=(quads[q_n].v[0].x+quads[q_n].v[1].x+quads[q_n].v[2].x+quads[q_n].v[3].x)/4;
      quads[q_n].c.y=(quads[q_n].v[0].y+quads[q_n].v[1].y+quads[q_n].v[2].y+quads[q_n].v[3].y)/4;
      quads[q_n].c.z=(quads[q_n].v[0].z+quads[q_n].v[1].z+quads[q_n].v[2].z+quads[q_n].v[3].z)/4;
      q_n++;
    }
  }
}

void SortAndDraw()
{
  for(int i=0; i<q_n; i++)
    q_dist[i]=sqr(quads[i].c.x-cam.x)+sqr(quads[i].c.y-cam.y)+sqr(quads[i].c.z-cam.z);
  qsort(q_index,q_n,sizeof(int),cmp);
  glBegin(GL_QUADS);
  for(int j=0;j<q_n;j++)
  {
    int i=q_index[j];
    glNormal3fv(quads[i].n[0].v);glTexCoord3fv(quads[i].t[0].v);glVertex3fv(quads[i].v[0].v);
    glNormal3fv(quads[i].n[1].v);glTexCoord3fv(quads[i].t[1].v);glVertex3fv(quads[i].v[1].v);
    glNormal3fv(quads[i].n[2].v);glTexCoord3fv(quads[i].t[2].v);glVertex3fv(quads[i].v[2].v);
    glNormal3fv(quads[i].n[3].v);glTexCoord3fv(quads[i].t[3].v);glVertex3fv(quads[i].v[3].v);
  }
  glEnd();
}

void GLUTCALLBACK display()
{

  glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(90.0,max_x/(GLdouble)max_y,0.01,40);
  glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
  gluLookAt(cam.x,cam.y,cam.z,cam.x+sin(cam_fi)*cos(cam_psi),cam.y+cos(cam_fi)*cos(cam_psi),cam.z+sin(cam_psi),0,0,2);

	GLfloat l_pos[4]={sin(t) + 0.6,0.5,cos(t),1};
	glLightfv(GL_LIGHT0,GL_POSITION,l_pos);

	glDisable(GL_TEXTURE_2D);
	glDisable(GL_LIGHTING);
	glColor3f(0.6, 0.9, 0.5);
	glPushMatrix();
	glTranslatef(l_pos[0], l_pos[1], l_pos[2]);
	glutSolidSphere(0.02, 10, 10);
	glPopMatrix();
	glEnable(GL_LIGHTING);
	glEnable(GL_TEXTURE_2D);

  glEnable(GL_BLEND);
  glMatrixMode(GL_TEXTURE);
  glLoadIdentity();
  glTranslatef(t/6,t/12,0);
  glMatrixMode(GL_MODELVIEW);
  SortAndDraw();
  glDisable(GL_BLEND);

  glutSwapBuffers();
}

void idle() 
{
  static int LastTime=GetTickCount();
  dt=(double)(GetTickCount()-LastTime)/1000;
  t+=dt;
  LastTime=GetTickCount();

  const float Speed=5;
  if(wk)
  {
    cam.x+=Speed*dt*sin(cam_fi)*cos(cam_psi);
    cam.y+=Speed*dt*cos(cam_fi)*cos(cam_psi);
    cam.z+=Speed*dt*sin(cam_psi);
  }
  if(sk)
  {
    cam.x-=Speed*dt*sin(cam_fi)*cos(cam_psi);
    cam.y-=Speed*dt*cos(cam_fi)*cos(cam_psi);
    cam.z-=Speed*dt*sin(cam_psi);
  }
  if(ak)
  {
    cam.y+=Speed*dt*sin(cam_fi);
    cam.x-=Speed*dt*cos(cam_fi);
  }
  if(dk)
  {
    cam.y-=Speed*dt*sin(cam_fi);
    cam.x+=Speed*dt*cos(cam_fi);
  }
  display();
}

void GLUTCALLBACK reshape(int w, int h)
{
  glViewport(0,0,w,h);
  if(h > 0) {
		max_x=w;
    max_y=h;
	}
}

void Init()
{
  glClearColor(0,0,0,1);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_TEXTURE_2D);
	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);
	glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);

	GLfloat m_emis[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
	GLfloat m_diff[4] = { 1.0f, 1.0f, 1.0f, 1.0f };
	GLfloat m_spec[4] = { 1.0f, 1.0f, 1.0f, 1.0f };
	GLfloat m_ambi[4] = { 1.0f, 1.0f, 1.0f, 1.0f };
	glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, m_emis);
	glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diff);
	glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, m_spec);
	glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, m_ambi);
	glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 15);

	GLfloat l_diff[4] = {0.7, 0.3, 0.1, 1.0};
	GLfloat l_spec[4] = {0.7, 0.3, 0.1, 1.0};
	GLfloat l_ambi[4] = {0.1, 0.1, 0.1, 1.0};
	glLightfv(GL_LIGHT0,GL_DIFFUSE,l_diff);
	glLightfv(GL_LIGHT0,GL_SPECULAR,l_spec);
	glLightfv(GL_LIGHT0,GL_AMBIENT,l_ambi);

  cam.x=0;
  cam.y=-5;
  cam.z=0;

  Texture = createTexture2D ( true,  "tex.bmp" );

  for(int i=0; i<QSIZE; i++)
    q_index[i]=i;
  Generate();
}

void GLUTCALLBACK MouseMove(int x, int y)
{
  if(!(x==max_x/2 && y==max_y/2))
  {
    cam_fi+=(x-max_x/2)*0.01; 
    cam_psi+=-(y-max_y/2)*0.01;
    if(cam_fi<0)     cam_fi+=2*PI; 
    if(cam_fi>=2*PI) cam_fi-=2*PI; 
    if(cam_psi>PI/2) cam_psi =PI/2; 
    if(cam_psi<-PI/2)cam_psi =-PI/2; 
    glutWarpPointer(max_x/2,max_y/2);
  }
} 

void GLUTCALLBACK KeyDown(unsigned char Key, int x, int y)
{
  if(Key==27)
    exit(0);
  if(Key=='w')
    wk=true;
  if(Key=='s')
    sk=true;
  if(Key=='a')
    ak=true;
  if(Key=='d')
    dk=true;
}

void GLUTCALLBACK KeyUp(unsigned char Key, int x, int y)
{
  if(Key=='w')
    wk=false;
  if(Key=='s')
    sk=false;
  if(Key=='a')
    ak=false;
  if(Key=='d')
    dk=false;
}


int main(int argc, char** argv)
{
  glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DOUBLE|GLUT_RGB|GLUT_DEPTH|GLUT_MULTISAMPLE|GLUT_STENCIL);
  glutCreateWindow("GL");
  glutDisplayFunc(display);
  glutReshapeFunc(reshape);
	glutIdleFunc(idle);

  glutIgnoreKeyRepeat(1);
  glutKeyboardUpFunc(KeyUp);
  glutKeyboardFunc(KeyDown);
  glutPassiveMotionFunc(MouseMove); 

  glutSetCursor(GLUT_CURSOR_NONE);
  Init();
  glutMainLoop();
	return 0;
}