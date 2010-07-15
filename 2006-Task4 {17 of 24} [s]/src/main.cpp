#include <windows.h>
#include <gl\glut.h>
#include <stdio.h>
#include <math.h>
#include "loadbmp.h"
#include "Vector3D.h"

#define PI 3.141592

#define N 64
#define M 16
#define R 1.2
#define R2 2
#define POLYCOUNT N*M
#define CENTER 1

float now;

Vector3D l(0,0,1), c(3,0,5), vc(c);
float ofi=0,opsi=0;
int mousex, mousey;

GLuint txs[2];

struct Poly {
  Vector3D m;
  Vector3D v[4];
  Vector3D n[4];
  Vector3D t[4];
};

struct Object {
  Poly Polys[POLYCOUNT];
  int Index[POLYCOUNT];
  float Distance[POLYCOUNT];
};

Object Mebius, Klein, Klein2;

Object* CurrentObject;
Object* Objects[3];
int ObjectN=0;
int DrawMode=0;


Vector3D GetVertex3(float u, float v)
{
  return Vector3D( (R2+cos(u/2)*sin(v)-sin(u/2)*sin(2*v))*cos(u), 
                   (R2+cos(u/2)*sin(v)-sin(u/2)*sin(2*v))*sin(u),
                    sin(u/2)*sin(v)+cos(u/2)*sin(2*v))*0.2;
}

Vector3D GetVertex2(float u, float v)
{
  if(u<PI)
    return Vector3D( 4*R*(1-0.5*cos(u))*sin(v), 
                     6*cos(u)*(1+sin(u))+4*R*(1-0.5*cos(u))*cos(u)*cos(v),
                     -16*sin(u)-4*R*(1-0.5*cos(u)/2)*sin(u)*cos(v) )*0.05;
  else
    return Vector3D( 4*R*(1-0.5*cos(u))*sin(v),
                     6*cos(u)*(1+sin(u))-4*R*(1-0.5*cos(u))*cos(v),
                     -16*sin(u) )*0.05;
}

Vector3D GetVertex(float u, float v)
{
  return Vector3D( (1+cos(u/2)*v/2)*cos(u), (1+cos(u/2)*v/2)*sin(u), sin(u/2)*v/2)*0.5;
}

void GetVertexInfo(int i, int j, Vector3D& vertex, Vector3D& normal, Vector3D &texcoord)
{
  float u=2*PI*i/N;
  float v=2.0f*j/M-1.0f;
  Vector3D side1=(GetVertex(u+0.001,v)-GetVertex(u-0.001,v)).normalize();
  Vector3D side2=(GetVertex(u,v+0.001)-GetVertex(u,v-0.001)).normalize();
  normal=-side1^side2;
  vertex=GetVertex(u,v);
  texcoord=Vector3D(2*u/(2*PI),v/2,0);
}

void GetVertexInfo2(int i, int j, Vector3D& vertex, Vector3D& normal, Vector3D &texcoord)
{
  float u=2*PI*i/N;
  float v=2*PI*j/M;
  Vector3D side1=(GetVertex2(u+0.001,v)-GetVertex2(u-0.001,v)).normalize();
  Vector3D side2=(GetVertex2(u,v+0.001)-GetVertex2(u,v-0.001)).normalize();
  normal=-side1^side2;
  vertex=GetVertex2(u,v);
  texcoord=Vector3D(4*u/(2*PI),v/(2*PI),0);
}

void GetVertexInfo3(int i, int j, Vector3D& vertex, Vector3D& normal, Vector3D &texcoord)
{
  float u=2*PI*i/N;
  float v=2*PI*j/M;
  Vector3D side1=(GetVertex3(u+0.001,v)-GetVertex3(u-0.001,v)).normalize();
  Vector3D side2=(GetVertex3(u,v+0.001)-GetVertex3(u,v-0.001)).normalize();
  normal=-side1^side2;
  vertex=GetVertex3(u,v);
  texcoord=Vector3D(4*u/(2*PI),v/(2*PI),0);
}

void GenMebius()
{
  int n=0;
  for(int i=0; i<N; i++) for(int j=0; j<M; j++)
  {
    GetVertexInfo(i  ,j  ,Mebius.Polys[n].v[0],Mebius.Polys[n].n[0],Mebius.Polys[n].t[0]);
    GetVertexInfo(i  ,j+1,Mebius.Polys[n].v[1],Mebius.Polys[n].n[1],Mebius.Polys[n].t[1]);
    GetVertexInfo(i+1,j+1,Mebius.Polys[n].v[2],Mebius.Polys[n].n[2],Mebius.Polys[n].t[2]);
    GetVertexInfo(i+1,j  ,Mebius.Polys[n].v[3],Mebius.Polys[n].n[3],Mebius.Polys[n].t[3]);
    Mebius.Polys[n].m=(Mebius.Polys[n].v[0]+Mebius.Polys[n].v[1]+Mebius.Polys[n].v[2]+Mebius.Polys[n].v[3])/4;
    n++;
  }
}

void GenKlein2()
{
  int n=0;
  for(int i=0; i<N; i++) for(int j=0; j<M; j++)
  {
    GetVertexInfo3(i  ,j  ,Klein2.Polys[n].v[0],Klein2.Polys[n].n[0],Klein2.Polys[n].t[0]);
    GetVertexInfo3(i  ,j+1,Klein2.Polys[n].v[1],Klein2.Polys[n].n[1],Klein2.Polys[n].t[1]);
    GetVertexInfo3(i+1,j+1,Klein2.Polys[n].v[2],Klein2.Polys[n].n[2],Klein2.Polys[n].t[2]);
    GetVertexInfo3(i+1,j  ,Klein2.Polys[n].v[3],Klein2.Polys[n].n[3],Klein2.Polys[n].t[3]);
    Klein2.Polys[n].m=(Klein2.Polys[n].v[0]+Klein2.Polys[n].v[1]+Klein2.Polys[n].v[2]+Klein2.Polys[n].v[3])/4;
    n++;
  }
}

void GenKlein()
{
  int n=0;
  for(int i=0; i<N; i++) for(int j=0; j<M; j++)
  {
    GetVertexInfo2(i  ,j  ,Klein.Polys[n].v[0],Klein.Polys[n].n[0],Klein.Polys[n].t[0]);
    GetVertexInfo2(i  ,j+1,Klein.Polys[n].v[1],Klein.Polys[n].n[1],Klein.Polys[n].t[1]);
    GetVertexInfo2(i+1,j+1,Klein.Polys[n].v[2],Klein.Polys[n].n[2],Klein.Polys[n].t[2]);
    GetVertexInfo2(i+1,j  ,Klein.Polys[n].v[3],Klein.Polys[n].n[3],Klein.Polys[n].t[3]);
    Klein.Polys[n].m=(Klein.Polys[n].v[0]+Klein.Polys[n].v[1]+Klein.Polys[n].v[2]+Klein.Polys[n].v[3])/4;
    n++;
  }
}

int cmp(const void* a, const void* b)
{
  if(CurrentObject->Distance[*((int*)a)]>CurrentObject->Distance[*((int*)b)])
    return -1;
  else if(CurrentObject->Distance[*((int*)a)]<CurrentObject->Distance[*((int*)b)])
    return 1;
  else
    return 0;
}

void SortObject(float _px, float _py, float _pz)
{
  for(int i=0; i<POLYCOUNT; i++)
  {
    CurrentObject->Distance[i]=(Vector3D(_px,_py,_pz)-CurrentObject->Polys[i].m).lengthSq();
    CurrentObject->Index[i]=i;
  }
  qsort(&CurrentObject->Index,POLYCOUNT,sizeof(int),cmp);
}

void DrawObject(bool isShadow)
{
  if(isShadow)
  {
    glBegin(GL_QUADS);
    for(int i=0; i<POLYCOUNT; i++)
    {
      glNormal3f(CurrentObject->Polys[CurrentObject->Index[i]].n[0].x,CurrentObject->Polys[CurrentObject->Index[i]].n[0].y,CurrentObject->Polys[CurrentObject->Index[i]].n[0].z);
      glVertex3f(CurrentObject->Polys[CurrentObject->Index[i]].v[0].x,CurrentObject->Polys[CurrentObject->Index[i]].v[0].y,CurrentObject->Polys[CurrentObject->Index[i]].v[0].z);
      glNormal3f(CurrentObject->Polys[CurrentObject->Index[i]].n[1].x,CurrentObject->Polys[CurrentObject->Index[i]].n[1].y,CurrentObject->Polys[CurrentObject->Index[i]].n[1].z);
      glVertex3f(CurrentObject->Polys[CurrentObject->Index[i]].v[1].x,CurrentObject->Polys[CurrentObject->Index[i]].v[1].y,CurrentObject->Polys[CurrentObject->Index[i]].v[1].z);
      glNormal3f(CurrentObject->Polys[CurrentObject->Index[i]].n[2].x,CurrentObject->Polys[CurrentObject->Index[i]].n[2].y,CurrentObject->Polys[CurrentObject->Index[i]].n[2].z);
      glVertex3f(CurrentObject->Polys[CurrentObject->Index[i]].v[2].x,CurrentObject->Polys[CurrentObject->Index[i]].v[2].y,CurrentObject->Polys[CurrentObject->Index[i]].v[2].z);
      glNormal3f(CurrentObject->Polys[CurrentObject->Index[i]].n[3].x,CurrentObject->Polys[CurrentObject->Index[i]].n[3].y,CurrentObject->Polys[CurrentObject->Index[i]].n[3].z);
      glVertex3f(CurrentObject->Polys[CurrentObject->Index[i]].v[3].x,CurrentObject->Polys[CurrentObject->Index[i]].v[3].y,CurrentObject->Polys[CurrentObject->Index[i]].v[3].z);
    }
    glEnd();
    return;
  }
  if(DrawMode==0 || DrawMode==1)
  {
	  GLfloat m_diffuse[4] = { 0.5f, 0.5f, 0.5f, 0.5f };
	  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);

    glBindTexture(GL_TEXTURE_2D,txs[0]);

    glDisable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);
    if(DrawMode==1)
      glDisable(GL_TEXTURE_2D);
    glBegin(GL_QUADS);
    for(int i=0; i<POLYCOUNT; i++)
    {
      glNormal3f(CurrentObject->Polys[CurrentObject->Index[i]].n[0].x,CurrentObject->Polys[CurrentObject->Index[i]].n[0].y,CurrentObject->Polys[CurrentObject->Index[i]].n[0].z);
    glTexCoord3f(CurrentObject->Polys[CurrentObject->Index[i]].t[0].x,CurrentObject->Polys[CurrentObject->Index[i]].t[0].y,CurrentObject->Polys[CurrentObject->Index[i]].t[0].z);
      glVertex3f(CurrentObject->Polys[CurrentObject->Index[i]].v[0].x,CurrentObject->Polys[CurrentObject->Index[i]].v[0].y,CurrentObject->Polys[CurrentObject->Index[i]].v[0].z);
      glNormal3f(CurrentObject->Polys[CurrentObject->Index[i]].n[1].x,CurrentObject->Polys[CurrentObject->Index[i]].n[1].y,CurrentObject->Polys[CurrentObject->Index[i]].n[1].z);
    glTexCoord3f(CurrentObject->Polys[CurrentObject->Index[i]].t[1].x,CurrentObject->Polys[CurrentObject->Index[i]].t[1].y,CurrentObject->Polys[CurrentObject->Index[i]].t[1].z);
      glVertex3f(CurrentObject->Polys[CurrentObject->Index[i]].v[1].x,CurrentObject->Polys[CurrentObject->Index[i]].v[1].y,CurrentObject->Polys[CurrentObject->Index[i]].v[1].z);
      glNormal3f(CurrentObject->Polys[CurrentObject->Index[i]].n[2].x,CurrentObject->Polys[CurrentObject->Index[i]].n[2].y,CurrentObject->Polys[CurrentObject->Index[i]].n[2].z);
    glTexCoord3f(CurrentObject->Polys[CurrentObject->Index[i]].t[2].x,CurrentObject->Polys[CurrentObject->Index[i]].t[2].y,CurrentObject->Polys[CurrentObject->Index[i]].t[2].z);
      glVertex3f(CurrentObject->Polys[CurrentObject->Index[i]].v[2].x,CurrentObject->Polys[CurrentObject->Index[i]].v[2].y,CurrentObject->Polys[CurrentObject->Index[i]].v[2].z);
      glNormal3f(CurrentObject->Polys[CurrentObject->Index[i]].n[3].x,CurrentObject->Polys[CurrentObject->Index[i]].n[3].y,CurrentObject->Polys[CurrentObject->Index[i]].n[3].z);
    glTexCoord3f(CurrentObject->Polys[CurrentObject->Index[i]].t[3].x,CurrentObject->Polys[CurrentObject->Index[i]].t[3].y,CurrentObject->Polys[CurrentObject->Index[i]].t[3].z);
      glVertex3f(CurrentObject->Polys[CurrentObject->Index[i]].v[3].x,CurrentObject->Polys[CurrentObject->Index[i]].v[3].y,CurrentObject->Polys[CurrentObject->Index[i]].v[3].z);
    }
    glEnd();
    if(DrawMode==1)
      glEnable(GL_TEXTURE_2D);
    glDisable(GL_BLEND);
    glEnable(GL_DEPTH_TEST);
  }
  if(DrawMode==2)
  {
	  GLfloat m_diffuse[4] = { 0.5f, 0.4f, 0.5f, 1.0f };
    glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);
 
  	glEnable(GL_POLYGON_OFFSET_FILL);
   	glPolygonOffset(0.5,5);
    glDisable(GL_TEXTURE_2D);
    glBegin(GL_QUADS);
    for(int i=POLYCOUNT-1; i>=0; i--)
    {
      glNormal3f(CurrentObject->Polys[CurrentObject->Index[i]].n[0].x,CurrentObject->Polys[CurrentObject->Index[i]].n[0].y,CurrentObject->Polys[CurrentObject->Index[i]].n[0].z);
      glVertex3f(CurrentObject->Polys[CurrentObject->Index[i]].v[0].x,CurrentObject->Polys[CurrentObject->Index[i]].v[0].y,CurrentObject->Polys[CurrentObject->Index[i]].v[0].z);
      glNormal3f(CurrentObject->Polys[CurrentObject->Index[i]].n[1].x,CurrentObject->Polys[CurrentObject->Index[i]].n[1].y,CurrentObject->Polys[CurrentObject->Index[i]].n[1].z);
      glVertex3f(CurrentObject->Polys[CurrentObject->Index[i]].v[1].x,CurrentObject->Polys[CurrentObject->Index[i]].v[1].y,CurrentObject->Polys[CurrentObject->Index[i]].v[1].z);
      glNormal3f(CurrentObject->Polys[CurrentObject->Index[i]].n[2].x,CurrentObject->Polys[CurrentObject->Index[i]].n[2].y,CurrentObject->Polys[CurrentObject->Index[i]].n[2].z);
      glVertex3f(CurrentObject->Polys[CurrentObject->Index[i]].v[2].x,CurrentObject->Polys[CurrentObject->Index[i]].v[2].y,CurrentObject->Polys[CurrentObject->Index[i]].v[2].z);
      glNormal3f(CurrentObject->Polys[CurrentObject->Index[i]].n[3].x,CurrentObject->Polys[CurrentObject->Index[i]].n[3].y,CurrentObject->Polys[CurrentObject->Index[i]].n[3].z);
      glVertex3f(CurrentObject->Polys[CurrentObject->Index[i]].v[3].x,CurrentObject->Polys[CurrentObject->Index[i]].v[3].y,CurrentObject->Polys[CurrentObject->Index[i]].v[3].z);
    }
    glEnd();
    glDisable(GL_POLYGON_OFFSET_FILL);
  
    glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
    glDisable(GL_LIGHTING);
    glColor3f(0,0,0);
    glBegin(GL_QUADS);
    for(int i=POLYCOUNT-1; i>=0; i--)
    {
      glNormal3f(CurrentObject->Polys[CurrentObject->Index[i]].n[0].x,CurrentObject->Polys[CurrentObject->Index[i]].n[0].y,CurrentObject->Polys[CurrentObject->Index[i]].n[0].z);
      glVertex3f(CurrentObject->Polys[CurrentObject->Index[i]].v[0].x,CurrentObject->Polys[CurrentObject->Index[i]].v[0].y,CurrentObject->Polys[CurrentObject->Index[i]].v[0].z);
      glNormal3f(CurrentObject->Polys[CurrentObject->Index[i]].n[1].x,CurrentObject->Polys[CurrentObject->Index[i]].n[1].y,CurrentObject->Polys[CurrentObject->Index[i]].n[1].z);
      glVertex3f(CurrentObject->Polys[CurrentObject->Index[i]].v[1].x,CurrentObject->Polys[CurrentObject->Index[i]].v[1].y,CurrentObject->Polys[CurrentObject->Index[i]].v[1].z);
      glNormal3f(CurrentObject->Polys[CurrentObject->Index[i]].n[2].x,CurrentObject->Polys[CurrentObject->Index[i]].n[2].y,CurrentObject->Polys[CurrentObject->Index[i]].n[2].z);
      glVertex3f(CurrentObject->Polys[CurrentObject->Index[i]].v[2].x,CurrentObject->Polys[CurrentObject->Index[i]].v[2].y,CurrentObject->Polys[CurrentObject->Index[i]].v[2].z);
      glNormal3f(CurrentObject->Polys[CurrentObject->Index[i]].n[3].x,CurrentObject->Polys[CurrentObject->Index[i]].n[3].y,CurrentObject->Polys[CurrentObject->Index[i]].n[3].z);
      glVertex3f(CurrentObject->Polys[CurrentObject->Index[i]].v[3].x,CurrentObject->Polys[CurrentObject->Index[i]].v[3].y,CurrentObject->Polys[CurrentObject->Index[i]].v[3].z);
    }
    glEnd();
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_LIGHTING);
    glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
  }
}

void DisplayFunc()
{
  CurrentObject=Objects[ObjectN];

  vc.x=c.x;
  vc.y=c.y;
  vc.z=c.z-CENTER;
  Vector3D nc;

  nc.x=vc.x*cos(opsi/180*PI)-vc.z*sin(opsi/180*PI);
  nc.y=vc.y;
  nc.z=vc.x*sin(opsi/180*PI)+vc.z*cos(opsi/180*PI);

  vc=nc;

  nc.x=vc.x*cos(-ofi/180*PI)-vc.y*sin(-ofi/180*PI);
  nc.y=vc.x*sin(-ofi/180*PI)+vc.y*cos(-ofi/180*PI);
  nc.z=vc.z;
 
  vc=nc;

  l.z=1.9+0.2*sin(now)+0.2*cos(0.5*now);
  l.y=sin(2*now)*cos(now)+sin(1.7*now);
  l.x=cos(4*now)*sin(3*now)+0.5*cos(0.3*now);

  glClearColor(0.02,0.02,0.1,0.0);
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_STENCIL_BUFFER_BIT);

  now=(float)GetTickCount()/1000;

  glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
  gluLookAt(c.x,c.y,c.z,0,0,0,0,0,1);
  
	GLfloat l_position[4] = { l.x, l.y, l.z, 1 };
	glLightfv(GL_LIGHT0,GL_POSITION,l_position);
  GLfloat l_diffuse[4] = { 0.7+0.3*sin(0.3*now), 0.7+0.3*cos(now), 0.7+0.3*cos(3*now)*sin(0.5*now), 0.0f };
	GLfloat l_specular[4] = { 0.7+0.3*sin(0.3*now), 0.7+0.3*cos(now), 0.7+0.3*cos(3*now)*sin(0.5*now), 0.0f };
	GLfloat l_ambient[4] = { 1, 1, 1, 0.0f };
	glLightfv(GL_LIGHT0,GL_DIFFUSE,l_diffuse);
	glLightfv(GL_LIGHT0,GL_SPECULAR,l_specular);
	glLightfv(GL_LIGHT0,GL_AMBIENT,l_ambient);

  // floor
  glEnable(GL_STENCIL_TEST);
  glStencilFunc(GL_ALWAYS,1,1);
	glStencilOp(GL_KEEP,GL_KEEP,GL_REPLACE);
  glBindTexture(GL_TEXTURE_2D,txs[1]);
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glRotatef(-now*5,0,0,1);
  glBegin(GL_QUAD_STRIP);
    glNormal3f(0,0,1);
    for(int i=0; i<=16; i++)
    {                    
      float fi=(float)i/16*2*PI;
      float r=1.5+1*sin(3*fi)*sin(6*fi)*sin(9*fi)*sin(12*fi);
      float x=r*cos(fi);
      float y=r*sin(fi);
      float x1=0.2*cos(fi);
      float y1=0.2*sin(fi);
      glTexCoord2f(x1,y1);
      glVertex3f(x1,y1,0);
      glTexCoord2f(x,y);
      glVertex3f(x,y,0);
    }
  glEnd();
  glPopMatrix();
  
  // shadow
  glDisable(GL_DEPTH_TEST);
	glDisable(GL_LIGHTING);
  glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
  glStencilFunc(GL_EQUAL,1,1);
	glStencilOp(GL_KEEP,GL_INCR,GL_INCR);
  glPushMatrix();
  GLfloat Projection[16]={ l.z,   0,   0,   0,
                             0, l.z,   0,   0,
                          -l.x,-l.y,   0,  -1,
                             0,   0,   0, l.z};
  glMultMatrixf(Projection);
  glColor4f(0,0,0,0.5);
  glTranslatef(0,0,CENTER);
  glRotatef(opsi,0,1,0);
  glRotatef(ofi,0,0,1);
  DrawObject(true);
  glPopMatrix();
  glDisable(GL_BLEND);
	glEnable(GL_LIGHTING); 
  glEnable(GL_DEPTH_TEST);
  glDisable(GL_STENCIL_TEST);

  // light
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_LIGHTING);
  glColor4f(l_diffuse[0],l_diffuse[1],l_diffuse[2],1);
  glPointSize(10);
  glBegin(GL_POINTS);
    glVertex3f(l.x,l.y,l.z);
  glEnd();
  glEnable(GL_LIGHTING);
  glEnable(GL_TEXTURE_2D);
  
  // object
  glPushMatrix();
  SortObject(vc.x,vc.y,vc.z);
  glTranslatef(0,0,CENTER);
  glRotatef(opsi,0,1,0);
  glRotatef(ofi,0,0,1);
  DrawObject(false);
  glPopMatrix();

  glFlush();
  glutSwapBuffers();
}

void IdleFunc()
{
	glutPostRedisplay();
}

void KeyboardFunc(unsigned char key, int x, int y)
{
  if(key=='\033')
    exit(0);
  if(key=='\t')
    ObjectN=(ObjectN+1)%3;
  if(key==' ')
    DrawMode=(DrawMode+1)%3;
}

void PassiveMotionFunc(int x, int y)
{
  mousex=x;
  mousey=y;
}


void MotionFunc(int x, int y)
{
  ofi+=(x-mousex)*0.5;
  opsi+=(y-mousey)*0.5;
  mousex=x;
  mousey=y;
}

void ReshapeFunc(int w, int h)
{
	glViewport(0,0,w,h);
	if(h > 0) {
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		gluPerspective(60.0,w/(GLdouble)h,0.1,20);
	}
}

void Init()
{
  printf("* Initializing\n");

  printf("Generating Objects...\n");
  GenMebius();
  GenKlein();
  GenKlein2();

  Objects[0]=&Mebius;
  Objects[1]=&Klein;
  Objects[2]=&Klein2;

  printf("Loading Textures...\n");

	IMAGE img;
  glPixelStorei(GL_UNPACK_ALIGNMENT,1);
  glGenTextures(2,&txs[0]);

  glBindTexture(GL_TEXTURE_2D,txs[0]);
  LoadBMP("Data\\mebius.bmp", &img);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, img.width,img.height, GL_RGB, GL_UNSIGNED_BYTE, img.data);             
  free(img.data);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);

  glBindTexture(GL_TEXTURE_2D,txs[1]);
	LoadBMP("Data\\floor.bmp", &img);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, img.width,img.height, GL_RGB, GL_UNSIGNED_BYTE, img.data);             
  free(img.data);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);

  printf("Setting OpenGL Mode...\n");
  glEnable(GL_DEPTH_TEST);
  glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_POINT_SMOOTH);
  glEnable(GL_NORMALIZE);
  glLightModelf(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);

	GLfloat l_diffuse[4] = { 1.0f, 1.0f, 1.0f, 0.0f };
	GLfloat l_specular[4] = { 1.0f, 1.0f, 1.0f, 0.0f };
	GLfloat l_ambient[4] = { 1.0f, 1.0f, 1.0f, 0.0f };
	glLightfv(GL_LIGHT0,GL_DIFFUSE,l_diffuse);
	glLightfv(GL_LIGHT0,GL_SPECULAR,l_specular);
	glLightfv(GL_LIGHT0,GL_AMBIENT,l_ambient);

  GLfloat m_emissive[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
	GLfloat m_diffuse[4] = { 0.5f, 0.4f, 0.4f, 0.5f };
	GLfloat m_specular[4] = { 0.6f, 0.7f, 0.6f, 0.0f };
	GLfloat m_ambient[4] = { 0.3f, 0.3f, 0.4f, 0.0f };
	glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, m_emissive);
	glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);
	glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, m_specular);
	glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, m_ambient);
	glMaterialf(GL_FRONT_AND_BACK,GL_SHININESS,41);

	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);
	glShadeModel(GL_SMOOTH);
}

int main(int argc, char** argv)
{
  glutInit(&argc,argv);
	glutInitDisplayMode(GLUT_RGB|GLUT_DOUBLE|GLUT_STENCIL|GLUT_DEPTH);
	glutInitWindowSize(640,480);
	glutCreateWindow("Task4");

	Init();
	glutReshapeFunc(ReshapeFunc);
  glutPassiveMotionFunc(PassiveMotionFunc);
	glutDisplayFunc(DisplayFunc);
	glutKeyboardFunc(KeyboardFunc);
	glutIdleFunc(IdleFunc);
  glutMotionFunc(MotionFunc);

  DEVMODE devmode;        
  memset(&devmode,0,sizeof(devmode));
  devmode.dmSize=sizeof(devmode);  
  devmode.dmPelsWidth=1024;    
  devmode.dmPelsHeight=768;    
  devmode.dmBitsPerPel=32;      
  devmode.dmFields=DM_BITSPERPEL|DM_PELSWIDTH|DM_PELSHEIGHT;
  ChangeDisplaySettings(&devmode,CDS_FULLSCREEN);
  glutFullScreen(); 
  
  glutMainLoop();

  return 0;
}