#include <gl\glut.h>
#include <stdlib.h>
#include <math.h>
#define PI 3.1415926

// Вектор
typedef union{
	float v[3];
	struct{
		float x,y,z;
	};
} Vector;

// 4-угольник
typedef struct{
  Vector v[4],n[4],t[4];
  Vector c;
} Quad;

GLuint Texture; //текстура тора

const int MaxQn=2000; // максимум граней у тора
Quad Q[MaxQn]; // грани
int QNmbs[MaxQn]; // в каком порядке их выводить
float QRanges[MaxQn]; // расстояния до граней
int Qn=0; // сколько у тора граней

inline float sqr(float a)
{
  return a*a;
}

int cmp(const void* p1, const void* p2)
{
  if(QRanges[*((int*)p1)]<QRanges[*((int*)p2)])
    return 1;
  else
    return -1;
}

void TorusVertex(Vector *v, Vector *n, Vector *t, float phi, float psi, float r1, float r2)
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

void GenerateObject()
{
  const int N1=40;
  const int N2=20;
  const double R1=1;
  const double R2=0.2;
  for(int i=0; i<N1; i++)
  {
    double phi1 = 2*i*PI/N1;
		double phi2 = 2*(i+1)*PI/N1;
    for(int j=0; j<N2; j++)
    {
      double psi1 = 2*j*PI/N2;
			double psi2 = 2*(j+1)*PI/N2;
      TorusVertex(&(Q[Qn].v[0]),&(Q[Qn].n[0]),&(Q[Qn].t[0]), phi1, psi1, R1, R2);
      TorusVertex(&(Q[Qn].v[1]),&(Q[Qn].n[1]),&(Q[Qn].t[1]), phi2, psi1, R1, R2);
      TorusVertex(&(Q[Qn].v[2]),&(Q[Qn].n[2]),&(Q[Qn].t[2]), phi2, psi2, R1, R2);
      TorusVertex(&(Q[Qn].v[3]),&(Q[Qn].n[3]),&(Q[Qn].t[3]), phi1, psi2, R1, R2);
      Q[Qn].c.x=(Q[Qn].v[0].x+Q[Qn].v[1].x+Q[Qn].v[2].x+Q[Qn].v[3].x)/4;
      Q[Qn].c.y=(Q[Qn].v[0].y+Q[Qn].v[1].y+Q[Qn].v[2].y+Q[Qn].v[3].y)/4;
      Q[Qn].c.z=(Q[Qn].v[0].z+Q[Qn].v[1].z+Q[Qn].v[2].z+Q[Qn].v[3].z)/4;
      Qn++;
    }
  }
  for(int i=0; i<MaxQn; i++)
    QNmbs[i]=i;
}

void DrawObject(float x,float y,float z,float camx,float camy,float camz,float phi,float psi)
{
  Vector c1,c2;
  float sinf=sin(phi/180*PI),cosf=cos(phi/180*PI),sinp=sin(psi/180*PI),cosp=cos(psi/180*PI);
	// считаем расстояния до граней тора
  for(int i=0; i<Qn; i++)
  {
    c1=Q[i].c;
    c2.x=c1.x;
    c2.y=c1.y*cosp-c1.z*sinp;
    c2.z=c1.y*sinp+c1.z*cosp;
    c1=c2;
    c2.x=c1.x*cosf-c1.y*sinf;
    c2.y=c1.x*sinf+c1.y*cosf;
    c2.z=c1.z;
    QRanges[i]=sqr(c2.x+x-camx)+sqr(c2.y+y-camy)+sqr(c2.z+z-camz);
  }
	qsort(QNmbs,Qn,sizeof(int),cmp); // сортируем по глубине

	// и выводим
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glTranslatef(x,y,z);
  glScalef(2,2,2);
  glRotatef(phi,0,0,1);
  glRotatef(psi,1,0,0);
  glDisable(GL_CULL_FACE);
  glDisable(GL_LIGHTING);
  glEnable(GL_BLEND);
  glBegin(GL_QUADS);
  for(int j=0;j<Qn;j++)
  {
    int i=QNmbs[j];
    glNormal3fv(Q[i].n[0].v);glTexCoord3fv(Q[i].t[0].v);glVertex3fv(Q[i].v[0].v);
    glNormal3fv(Q[i].n[1].v);glTexCoord3fv(Q[i].t[1].v);glVertex3fv(Q[i].v[1].v);
    glNormal3fv(Q[i].n[2].v);glTexCoord3fv(Q[i].t[2].v);glVertex3fv(Q[i].v[2].v);
    glNormal3fv(Q[i].n[3].v);glTexCoord3fv(Q[i].t[3].v);glVertex3fv(Q[i].v[3].v);
  }
  glEnd();
  glPopMatrix();
  glDisable(GL_BLEND);
  glEnable(GL_LIGHTING);
  glEnable(GL_CULL_FACE);
}



