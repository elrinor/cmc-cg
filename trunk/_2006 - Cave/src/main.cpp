#include <math.h>
#include <stdio.h>
#include <windows.h>
#include <gl\glut.h>
#pragma comment(lib, "glaux.lib")
#include <gl\glaux.h>
#include "bmp.h"
#include "Vector.h"
#include <gl\glext.h>
#include "Model.h"
#pragma comment(lib, "Bass.lib")
#include "bass.h"
#include "translucent.h"


#define PI 3.1415926

// ���-�� ��������� Snake
#define N 16
// ������ Snake
#define R0 2.0
// ��������� ������� Snake
#define DR 0.5
// ����� ������ Snake
#define R1 0.2
// ������������ "�������������" Snake
#define C0 3
#define C1 4

// ������� �� ���������� OpenGL
PFNGLPOINTPARAMETERFARBPROC  glPointParameterfARB  = NULL;
PFNGLPOINTPARAMETERFVARBPROC glPointParameterfvARB = NULL;
PFNGLACTIVETEXTUREARBPROC glActiveTextureARB = NULL;

// ����� ��� ������������ ������/�����
HSTREAM music;

// ������
typedef union{
	float v[3];
	struct{
		float x,y,z;
	};
} Vector;

double t=0,dt=0; // ����� ��������� � ������� ��������� / � ����������� ����� �����. (� ��������)
int t1=0; // ������� ������ �� �������
GLuint textures[10],ShadowMap; // ��������
TModel models[10]; // �������� 
int wnd_w,wnd_h; // ������� ����
bool fullscreen; // �� � ������ ������?
double x_angle=180,y_angle=0; // ���� ������ - �� ����������� � ���������

const int ListCount=256; //���-�� ���������� ������� ������� �� ������� ��� Snake
GLuint SnakeList; // ����� 1-�� �� ���� �������

const double M_Sensitivity=0.01; // ��������������� ����

Vector CamCoord; // ���������� ������
Vector LightCoord; // ���������� ������
Vector InitialTorusCoord; // ��������� ���������� Snake
Vector TorusCoord; // ������� ���������� Snake
float MirrorZ=-1.4; // Z-���������� ��������� ����

float mv[16],pr[16]; // ��� �������� ������ - �������� � ��������-�������
const int ShadowMapSize=512; // ������ ������� �����

bool key_w=false, key_a=false, key_d=false, key_s=false; // ������ �� W / A / D / S �����.

// ������� � ������� ������
typedef struct {
  float x,y,z,vx,vy,vz,ax,ay,az,size; // ����������, ��������, ���������, ������ �����.
  float r,g,b,a; // ����
  float life; // �����
  float lifespeed; // ��������� �����
} TParticle;
const int PCount=128; // ���-�� ������
TParticle particles[PCount]; // ������� ������

// ���������� ��������� �������� ����
void reshape(int w, int h)
{
  glViewport(0,0,w,h);
  if(h > 0) {
    if(fullscreen)
    {
      wnd_w=w;
      wnd_h=h;
    }
    else if(w<ShadowMapSize || h<ShadowMapSize) // �� ������ ���� ������ ��� ShadowMapSize*ShadowMapSize
      glutReshapeWindow(wnd_w,wnd_h);           // �.�. ����� ������� ����� ����� �������������� �����������
    else
    {
      wnd_w=w;
      wnd_h=h;
    }
  }
}

// �������� �������
void CreateParticle(int i)
{
  const int X=0,Y=0,Z=0;
  particles[i].x=X+0.1-0.2*rand()/RAND_MAX;
  particles[i].y=Y+0.1-0.2*rand()/RAND_MAX;
  particles[i].z=Z+0.1-0.2*rand()/RAND_MAX;
  particles[i].vx=0.5-1.0*rand()/RAND_MAX;
  particles[i].vy=0.5-1.0*rand()/RAND_MAX;
  particles[i].vz=0.5-1.0*rand()/RAND_MAX;
  particles[i].ax=0;
  particles[i].ay=0;
  particles[i].az=3;
  particles[i].r=1;
  particles[i].g=0;
  particles[i].b=0;
  particles[i].a=1;
  particles[i].life=1;
  particles[i].lifespeed=0.5+0.5*rand()/RAND_MAX;
}

// ����������� ��� �������
void CreateParticles()
{
  for(int i=0; i<PCount; i++)
    CreateParticle(i);
}

// �������������� ������� - �������� � �.�.
void ModifyParticles()
{
  const float T1=0.75; // ����� ������� ���������� ������
  const float T2=0.5;  // ����� ������� ���������� �����
  for(int i=0; i<PCount; i++)
  {
		// �������� ��������� � ����� �������
    particles[i].x+=particles[i].vx*dt;
    particles[i].y+=particles[i].vy*dt;
    particles[i].z+=particles[i].vz*dt;
    particles[i].vx+=particles[i].ax*dt;
    particles[i].vy+=particles[i].ay*dt;
    particles[i].vz+=particles[i].az*dt;
    particles[i].life-=dt*particles[i].lifespeed;
    if(particles[i].life<=0) // ���� ��� ������ ��
    {
      CreateParticle(i); // ������� �� �� ����� �����
    }
    else if(particles[i].life>T1) // ����� ������������� � ������������ � ��� ������� ����� � ��� ��������
      particles[i].g=1/(1-T1)*(1-particles[i].life);
    else if(particles[i].life>T2)
    {
      particles[i].g=1;
      particles[i].b=1/(T1-T2)*(T1-particles[i].life);
      particles[i].a=particles[i].life;
    }
    else
    {
      particles[i].r=1;
      particles[i].g=1;
      particles[i].b=1;
      particles[i].a=particles[i].life;
    }
  }
}

// ���������� �������
void DrawParticles()
{
  // ����� ������� ���� ���������� ��������!
  glPushAttrib(GL_ALL_ATTRIB_BITS);
  glDisable(GL_LIGHTING);
  glEnable(GL_BLEND);
  glEnable(GL_TEXTURE_2D);
  glDepthMask(0);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
  float quadratic[]={100.0/(wnd_h*wnd_h), 100.0/(wnd_h*wnd_h), 100.0/(wnd_h*wnd_h)}; 
  glPointParameterfvARB(GL_DISTANCE_ATTENUATION_EXT, quadratic); // ��� ������ ������ ������� � �����������
  glPointParameterfARB(GL_POINT_FADE_THRESHOLD_SIZE_ARB, 1.0f); // ������ ����� �������� ������� �������� "��������" � �� �����������
  glPointParameterfARB(GL_POINT_SIZE_MIN_ARB, 1.0f);   // ���. �
  glPointParameterfARB(GL_POINT_SIZE_MAX_ARB, 128.0f); // ����. ������ �������
  glEnable(GL_POINT_SPRITE_ARB);
  glPointSize(3.0);
  glTexEnvf(GL_POINT_SPRITE_ARB, GL_COORD_REPLACE_ARB, GL_TRUE);
  glBegin(GL_POINTS);
  for(int i=0; i<PCount; i++) // ������, ������ ������ �� ��������� :P
  {
    glColor4f(particles[i].r, particles[i].g, particles[i].b, particles[i].a);
    glVertex3f(particles[i].x, particles[i].y, particles[i].z);
  }
  glEnd();
  glDisable(GL_POINT_SPRITE_ARB);
  glPopAttrib();
}

// ������� fps
void fpsdisplay()
{
  static int lasttc=0;
  double fps;
  if(GetTickCount()-lasttc>1000)
  {
  fps=(double)t1*1000/(GetTickCount()-lasttc);
  lasttc=GetTickCount();
  t1=0;
  char s[20];
  sprintf(s,"%.10g",fps);
  glutSetWindowTitle(s);
  }
}

// ���������� Snake
void Build()
{
  double f,fn,fd;
  for(int i=0;i<2*N;i++)
  {
    f=PI*i/N;
    fn=PI*(i+1)/N;
    fd=2*PI*t;
    glBegin(GL_QUAD_STRIP);
    glColor3d(0.5,0.5,0.5);

    glTexCoord2d(0,0);
    glNormal3d(sin(fn*C1+fd)*cos(fn),sin(fn*C1+fd)*sin(fn),cos(fn*C1+fd));
    glVertex3d((R0+DR*sin(fn*C0)+R1*sin(fn*C1+fd))*cos(fn),(R0+DR*sin(fn*C0)+R1*sin(fn*C1+fd))*sin(fn),DR*cos(fn*C0)+R1*cos(fn*C1+fd));
    glTexCoord2d(0,1);
    glNormal3d(sin(f *C1+fd)*cos(f ),sin(f *C1+fd)*sin(f ),cos(f *C1+fd));
    glVertex3d((R0+DR*sin(f *C0)+R1*sin(f *C1+fd))*cos(f ),(R0+DR*sin(f *C0)+R1*sin(f *C1+fd))*sin(f ),DR*cos(f *C0)+R1*cos(f *C1+fd));
    
    glTexCoord2d(0.5,0);
    glNormal3d(sin(fn*C1+fd+PI/2)*cos(fn),sin(fn*C1+fd+PI/2)*sin(fn),cos(fn*C1+fd+PI/2));
    glVertex3d((R0+DR*sin(fn*C0)+R1*sin(fn*C1+fd+PI/2))*cos(fn),(R0+DR*sin(fn*C0)+R1*sin(fn*C1+fd+PI/2))*sin(fn),DR*cos(fn*C0)+R1*cos(fn*C1+fd+PI/2));
    glTexCoord2d(0.5,1);
    glNormal3d(sin(f *C1+fd+PI/2)*cos(f ),sin(f *C1+fd+PI/2)*sin(f ),cos(f *C1+fd+PI/2));
    glVertex3d((R0+DR*sin(f *C0)+R1*sin(f *C1+fd+PI/2))*cos(f ),(R0+DR*sin(f *C0)+R1*sin(f *C1+fd+PI/2))*sin(f ),DR*cos(f *C0)+R1*cos(f *C1+fd+PI/2));
    
    glTexCoord2d(1,0);
    glNormal3d(sin(fn*C1+fd+PI)*cos(fn),sin(fn*C1+fd+PI)*sin(fn),cos(fn*C1+fd+PI));
    glVertex3d((R0+DR*sin(fn*C0)+R1*sin(fn*C1+fd+PI))*cos(fn),(R0+DR*sin(fn*C0)+R1*sin(fn*C1+fd+PI))*sin(fn),DR*cos(fn*C0)+R1*cos(fn*C1+fd+PI));
    glTexCoord2d(1,1);
    glNormal3d(sin(f *C1+fd+PI)*cos(f ),sin(f *C1+fd+PI)*sin(f ),cos(f *C1+fd+PI));
    glVertex3d((R0+DR*sin(f *C0)+R1*sin(f *C1+fd+PI))*cos(f ),(R0+DR*sin(f *C0)+R1*sin(f *C1+fd+PI))*sin(f ),DR*cos(f *C0)+R1*cos(f *C1+fd+PI));
    
    glTexCoord2d(1.5,0);
    glNormal3d(sin(fn*C1+fd+3*PI/2)*cos(fn),sin(fn*C1+fd+3*PI/2)*sin(fn),cos(fn*C1+fd+3*PI/2));
    glVertex3d((R0+DR*sin(fn*C0)+R1*sin(fn*C1+fd+3*PI/2))*cos(fn),(R0+DR*sin(fn*C0)+R1*sin(fn*C1+fd+3*PI/2))*sin(fn),DR*cos(fn*C0)+R1*cos(fn*C1+fd+3*PI/2));
    glTexCoord2d(1.5,1);
    glNormal3d(sin(f *C1+fd+3*PI/2)*cos(f ),sin(f *C1+fd+3*PI/2)*sin(f ),cos(f *C1+fd+3*PI/2));
    glVertex3d((R0+DR*sin(f *C0)+R1*sin(f *C1+fd+3*PI/2))*cos(f ),(R0+DR*sin(f *C0)+R1*sin(f *C1+fd+3*PI/2))*sin(f ),DR*cos(f *C0)+R1*cos(f *C1+fd+3*PI/2));

    glTexCoord2d(2.0,0);
    glNormal3d(sin(fn*C1+fd)*cos(fn),sin(fn*C1+fd)*sin(fn),cos(fn*C1+fd));
    glVertex3d((R0+DR*sin(fn*C0)+R1*sin(fn*C1+fd))*cos(fn),(R0+DR*sin(fn*C0)+R1*sin(fn*C1+fd))*sin(fn),DR*cos(fn*C0)+R1*cos(fn*C1+fd));
    glTexCoord2d(2.0,1);
    glNormal3d(sin(f *C1+fd)*cos(f ),sin(f *C1+fd)*sin(f ),cos(f *C1+fd));
    glVertex3d((R0+DR*sin(f *C0)+R1*sin(f *C1+fd))*cos(f ),(R0+DR*sin(f *C0)+R1*sin(f *C1+fd))*sin(f ),DR*cos(f *C0)+R1*cos(f *C1+fd));
    glEnd();
  }
}

// ���������� ��� ������ (�� �� ���������� � ����)
void RenderFloor()
{
  glPushMatrix();
  glTranslated(-1,-1,-2);
  glScaled(2,2,2);
  glBindTexture(GL_TEXTURE_2D, textures[2]);
  glMatrixMode(GL_TEXTURE);
  glLoadIdentity();
  glScaled(0.5,0.5,0);
  glMatrixMode(GL_MODELVIEW);
  glDisable(GL_CULL_FACE);
  DrawModel(&models[0]);
  glEnable(GL_CULL_FACE);
  glPopMatrix();
}

// ���������� �� ����� ������ �� ������� ������������� ����
void RenderScene()
{
  // Cave
  glPushMatrix();
  glTranslated(-1,-1,-2);
  glScaled(2,2,2);
  glBindTexture(GL_TEXTURE_2D, textures[2]);
  glMatrixMode(GL_TEXTURE);
  glLoadIdentity();
  glScaled(0.5,0.5,0);
  glMatrixMode(GL_MODELVIEW);
  glDisable(GL_CULL_FACE);
  DrawModel(&models[1]);
  glEnable(GL_CULL_FACE);
  glPopMatrix();
}

// ���������� Snake
void RenderSnake()
{
  glPushMatrix();
  glBindTexture(GL_TEXTURE_2D, textures[0]);
  glScalef(0.5,0.5,0.5);
  glTranslatef(TorusCoord.x,TorusCoord.y,TorusCoord.z);
  glCallList(SnakeList+((int)(t*80))%ListCount);
  glRotated(30,0,0,1);
  glCallList(SnakeList+((int)(t*80))%ListCount);
  glRotated(30,0,0,1);
  glCallList(SnakeList+((int)(t*80))%ListCount);
  glRotated(30,0,0,1);
  glCallList(SnakeList+((int)(t*80))%ListCount);
  glPopMatrix();
}

// ���������� ���������� ������, � ����� (�� �� �����!)
void RenderBackWall()
{
  glPushMatrix();
  glTranslated(-1,-1,-2);
  glScaled(2,2,2);
  glBindTexture(GL_TEXTURE_2D, textures[2]);
  glMatrixMode(GL_TEXTURE);
  glLoadIdentity();
  glScaled(0.5,0.5,0);
  glMatrixMode(GL_MODELVIEW);
  glDisable(GL_CULL_FACE);
  DrawModel(&models[2]);
  glBegin(GL_QUADS);
    glNormal3d(0,0,-1);
    for(int i=-3;i<3;i++)
      for(int j=-3;j<3;j++)
      {
      glTexCoord2d(i  ,j  );glVertex3d(i  ,j  ,4);
      glTexCoord2d(i+1,j  );glVertex3d(i+1,j  ,4);
      glTexCoord2d(i+1,j+1);glVertex3d(i+1,j+1,4);
      glTexCoord2d(i  ,j+1);glVertex3d(i  ,j+1,4);
      }
  glEnd();
  glDisable(GL_CULL_FACE);
  glPopMatrix();

  // Torch
  glPushMatrix();
  glTranslated(LightCoord.x,LightCoord.y,LightCoord.z);
  glTranslated(-0.10,-0.10,-0.25);
  glRotated(25,-1,1,0);
  glBindTexture(GL_TEXTURE_2D, textures[4]);
  glDisable(GL_CULL_FACE);
  glBegin(GL_QUAD_STRIP);
    glNormal3d( sqrt(2.0), sqrt(2.0),0);glTexCoord2d(0   ,0);glVertex3d( 0.1, 0.1, 0.3);
    glNormal3d( sqrt(2.0), sqrt(2.0),0);glTexCoord2d(0   ,1);glVertex3d( 0.1, 0.1,-0.8);
    glNormal3d(-sqrt(2.0), sqrt(2.0),0);glTexCoord2d(0.25,0);glVertex3d(-0.1, 0.1, 0.3);
    glNormal3d(-sqrt(2.0), sqrt(2.0),0);glTexCoord2d(0.25,1);glVertex3d(-0.1, 0.1,-0.8);
    glNormal3d(-sqrt(2.0),-sqrt(2.0),0);glTexCoord2d(0.5 ,0);glVertex3d(-0.1,-0.1, 0.3);
    glNormal3d(-sqrt(2.0),-sqrt(2.0),0);glTexCoord2d(0.5 ,1);glVertex3d(-0.1,-0.1,-0.8);
    glNormal3d( sqrt(2.0),-sqrt(2.0),0);glTexCoord2d(0.75,0);glVertex3d( 0.1,-0.1, 0.3);
    glNormal3d( sqrt(2.0),-sqrt(2.0),0);glTexCoord2d(0.75,1);glVertex3d( 0.1,-0.1,-0.8);
    glNormal3d( sqrt(2.0), sqrt(2.0),0);glTexCoord2d(1   ,0);glVertex3d( 0.1, 0.1, 0.3);
    glNormal3d( sqrt(2.0), sqrt(2.0),0);glTexCoord2d(1   ,1);glVertex3d( 0.1, 0.1,-0.8);
  glEnd();
  glBegin(GL_QUADS);
    glNormal3d(0,0,1);glTexCoord2d(0,0);glVertex3d( 0.1, 0.1, 0.3);
    glNormal3d(0,0,1);glTexCoord2d(0,1);glVertex3d( 0.1,-0.1, 0.3);
    glNormal3d(0,0,1);glTexCoord2d(1,1);glVertex3d(-0.1,-0.1, 0.3);
    glNormal3d(0,0,1);glTexCoord2d(1,0);glVertex3d(-0.1, 0.1, 0.3);
  glEnd();
  glEnable(GL_CULL_FACE);
  glPopMatrix();
}

// ���������� �����
void RenderParticles()
{
  glBindTexture(GL_TEXTURE_2D, textures[1]);
  glPushMatrix();
  glTranslated(LightCoord.x,LightCoord.y,LightCoord.z);
  DrawParticles();
  glPopMatrix();
}

// ������� ������� �����
void MakeShadowMap()
{
  glColorMask(GL_FALSE,GL_FALSE,GL_FALSE,GL_FALSE);
  glDisable(GL_TEXTURE_2D);
  glEnable(GL_POLYGON_OFFSET_FILL);
  glPolygonOffset(4,4);
  glViewport(0,0,ShadowMapSize,ShadowMapSize);
  glClear(GL_DEPTH_BUFFER_BIT);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(60,1,0.1,60);
  gluLookAt(LightCoord.x,LightCoord.y,LightCoord.z,TorusCoord.x,TorusCoord.y,TorusCoord.z,0,0,1);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glGetFloatv(GL_MODELVIEW_MATRIX,mv);
  glGetFloatv(GL_PROJECTION_MATRIX,pr);

  RenderScene();
  RenderSnake();

  glBindTexture(GL_TEXTURE_2D,ShadowMap);
  glCopyTexSubImage2D(GL_TEXTURE_2D,0,1,1,1,1,ShadowMapSize-2,ShadowMapSize-2);
  glDisable(GL_POLYGON_OFFSET_FILL);
  glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
  glEnable(GL_TEXTURE_2D);
  reshape(wnd_w,wnd_h);
}

// Display �������
void display(void)
{
  // �������� �����������
  if(key_w)
  {
    CamCoord.x+=2*dt*sin(x_angle);
    CamCoord.y+=2*dt*cos(x_angle);
  }
  if(key_s)
  {
    CamCoord.x-=2*dt*sin(x_angle);
    CamCoord.y-=2*dt*cos(x_angle);
  }
  if(key_a)
  {
    CamCoord.x+=-2*dt*cos(x_angle);
    CamCoord.y+= 2*dt*sin(x_angle);
  }
  if(key_d)
  {
    CamCoord.x-=-2*dt*cos(x_angle);
    CamCoord.y-= 2*dt*sin(x_angle);
  }
  if(CamCoord.x>0)
    CamCoord.x=0;
  if(CamCoord.x<-1.6)
    CamCoord.x=-1.6;
  if(CamCoord.y>12)
    CamCoord.y=12;
  if(CamCoord.y<-2)
    CamCoord.y=-2;

  glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_STENCIL_BUFFER_BIT);
  
  ModifyParticles(); // ���������� �������
  MirrorZ=-1.4+0.2*sin(t); // �������� ������� ����

  TorusCoord=InitialTorusCoord; // �������� ��������� Snake
  TorusCoord.z+=cos(t*2/3)/2;

  MakeShadowMap(); // ������� ������� �����
  glClear(GL_DEPTH_BUFFER_BIT);

	// ���������� ������ ���� ����
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(90.0,wnd_w/(GLdouble)wnd_h,0.01,40);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(CamCoord.x,CamCoord.y,CamCoord.z,CamCoord.x+sin(x_angle)*cos(y_angle),CamCoord.y+cos(x_angle)*cos(y_angle),CamCoord.z+sin(y_angle),0,0,2);
  
  // ��������� Shadow Mapping
  glActiveTextureARB(GL_TEXTURE1_ARB);
  glBindTexture(GL_TEXTURE_2D,ShadowMap);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_TEXTURE_GEN_S);
  glEnable(GL_TEXTURE_GEN_T);
  glEnable(GL_TEXTURE_GEN_R);
  glEnable(GL_TEXTURE_GEN_Q);
  float v1[4]={1,0,0,0};
  float v2[4]={0,1,0,0};
  float v3[4]={0,0,1,0};
  float v4[4]={0,0,0,1};
  glTexGenfv(GL_S,GL_EYE_PLANE,v1);
  glTexGenfv(GL_T,GL_EYE_PLANE,v2);
  glTexGenfv(GL_R,GL_EYE_PLANE,v3);
  glTexGenfv(GL_Q,GL_EYE_PLANE,v4);
  glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
  glMatrixMode(GL_TEXTURE);
  //glPushMatrix();
  glLoadIdentity();
  glTranslatef(0.5,0.5,0.5 );     // remap from [-1,1]^2 to [0,1]^2
  glScalef(0.5,0.5,0.5);
  glMultMatrixf(pr);
  glMultMatrixf(mv);
  glActiveTextureARB(GL_TEXTURE0_ARB);
  glMatrixMode(GL_MODELVIEW);

  // ������������� ��������� ��������� �����
  GLfloat l_position0[4]={LightCoord.x,LightCoord.y,LightCoord.z,1};
  glLightfv(GL_LIGHT0,GL_POSITION,l_position0);
  GLfloat l_diffuse0[4] ={0.2+0.2*rand()/RAND_MAX,0.2*rand()/RAND_MAX,0.0,1.0};
  GLfloat l_specular0[4]={1,0.5+0.5*sin(t),0,1};
  GLfloat l_ambient0[4] ={1,1,1,1};
  glLightfv(GL_LIGHT0,GL_DIFFUSE,l_diffuse0);
  glLightfv(GL_LIGHT0,GL_SPECULAR,l_specular0);
  glLightfv(GL_LIGHT0,GL_AMBIENT,l_ambient0);
  
  // ��������
  RenderScene(); // - �� �� ��� ������������� ����
  RenderSnake(); // /
  glActiveTextureARB(GL_TEXTURE1_ARB ); // ��������� ����
  glDisable(GL_TEXTURE_2D);
  glActiveTextureARB(GL_TEXTURE0_ARB );
  RenderBackWall(); // \ .
  RenderFloor();    // - �������� ��� ���������
  RenderParticles();// / .
  glMatrixMode(GL_TEXTURE);
  glLoadIdentity();
  glTranslatef(t/4,t/6,0);
  glMatrixMode(GL_MODELVIEW);
  glBindTexture(GL_TEXTURE_2D, textures[5]);
  DrawObject(0,16,2,CamCoord.x,CamCoord.y,CamCoord.z,t*90,t*60); //������ �������������� ���
  
  // �������� � ������� ��������� - ������������� ��� ��� ��� � ��� ����
  glPushMatrix();
  glTranslated(0,0,MirrorZ);
  glEnable(GL_STENCIL_TEST);
  glStencilFunc(GL_ALWAYS,1,1);
  glStencilOp(GL_KEEP,GL_KEEP,GL_REPLACE);
  glBegin(GL_QUADS);
    glColor4d(1,1,1,0.3);
    glNormal3d(0,0,1);
    glTexCoord2d(1,1);glVertex3d(-9,-9,0);
    glTexCoord2d(1,0);glVertex3d( 9,-9,0);
    glTexCoord2d(0,0);glVertex3d( 9, 9,0);
    glTexCoord2d(0,1);glVertex3d(-9, 9,0);
  glEnd();
  glEnable(GL_LIGHTING);
  glPopMatrix();

  // ���������� ������ ������
  glPushMatrix();
  glClear(GL_DEPTH_BUFFER_BIT);
  glStencilFunc(GL_EQUAL,1,1);
  glTranslated(0,0,2*MirrorZ);
  glScaled(1,1,-1);
  glActiveTextureARB(GL_TEXTURE1_ARB );
  glEnable(GL_TEXTURE_2D);
  glMatrixMode(GL_TEXTURE);
  glTranslated(0,0,2*MirrorZ);
  glScaled(1,1,-1);
  glMatrixMode(GL_MODELVIEW);
  glActiveTextureARB(GL_TEXTURE0_ARB );
  RenderSnake(); // - �� �� ��� ������������� ����
  RenderScene(); // /
  glActiveTextureARB(GL_TEXTURE1_ARB );
  glDisable(GL_TEXTURE_2D);
  glActiveTextureARB(GL_TEXTURE0_ARB );
  RenderBackWall(); // - � ��� ��������� ��� ����� ����������
  RenderParticles();// /
  glPopMatrix();
  
  // ����
  glPushMatrix();
  glClear(GL_DEPTH_BUFFER_BIT);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  RenderFloor(); // ������������ ��� ��� ��� �����
  glDisable(GL_LIGHTING);
  glTranslated(0,0,MirrorZ);
  glBindTexture(GL_TEXTURE_2D, textures[3]);
  glMatrixMode(GL_TEXTURE);
  glLoadIdentity();
  glScaled(3,3,0);
  glRotated(t,0,0,1);
  glMatrixMode(GL_MODELVIEW);
  glBegin(GL_QUADS); // � ����� ����, ��������������
    glColor4d(1,1,1,0.1);
    glNormal3d(0,0,1);
    glTexCoord2d(1,1);glVertex3d(-9,-9,0);
    glTexCoord2d(1,0);glVertex3d( 9,-9,0);
    glTexCoord2d(0,0);glVertex3d( 9, 9,0);
    glTexCoord2d(0,1);glVertex3d(-9, 9,0);
  glEnd();
  glDisable(GL_BLEND);
  glEnable(GL_LIGHTING);
  glDisable(GL_STENCIL_TEST);
  glPopMatrix();

  glutSwapBuffers();
}

// Idle �������
void idle() 
{
  BASS_Update(); // ���� ����� ������
	// ������� ���������� � ����. ����� �������
  static int LastTC=GetTickCount();
  dt=(double)(GetTickCount()-LastTC)/1000;
  t+=dt;
  LastTC=GetTickCount();
  t1++;
  glutPostRedisplay(); // �������������� ����
  fpsdisplay(); // ������� ���
}

// �������������
void Init()
{
  printf("%s","Initializing... Please Wait.");
  
	// ������� ���������� ������ ��� ������ ��������� �������� snake
  SnakeList=glGenLists(ListCount);
  for(int i=0; i<ListCount; i++)
  {
    t=(double)i/ListCount;
    glNewList(SnakeList+i,GL_COMPILE);
    Build();
    glEndList();
  }

	// �������������� ������� ������
  CreateParticles();
  dt=0.1;
  for(int i=0; i<100; i++)
    ModifyParticles();

  srand(GetTickCount()); // RandSeed �������������

	// �������� ��������� �� �-�� ���������� OpenGL
  glPointParameterfARB  = (PFNGLPOINTPARAMETERFARBPROC)wglGetProcAddress("glPointParameterfARB");
  glPointParameterfvARB = (PFNGLPOINTPARAMETERFVARBPROC)wglGetProcAddress("glPointParameterfvARB");
  glActiveTextureARB=(PFNGLACTIVETEXTUREARBPROC)wglGetProcAddress("glActiveTextureARB");

	// ��������� ��������� �������������
  glClearColor(0,0,0,0);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_CULL_FACE);
  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHT1);
  glShadeModel(GL_SMOOTH);
  glHint(GL_POINT_SMOOTH_HINT,GL_NICEST);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glEnable(GL_TEXTURE_2D);
  
  glLightModelf(GL_LIGHT_MODEL_LOCAL_VIEWER,1);
  
	// ��������� ������
  CamCoord.x=0;
  CamCoord.y=1;
  CamCoord.z=3;

	// ��������� 1-�� ��������� �����
  GLfloat l_position1[4]={0,9,2,1};
  GLfloat l_diffuse1[4] ={26.0/256,133.0/256,236.0/256,1.0};
  GLfloat l_specular1[4]={26.0/256,133.0/256,236.0/256,1.0};
  GLfloat l_ambient1[4] ={0,0,0,1};
  glLightfv(GL_LIGHT1,GL_POSITION,l_position1);
  glLightfv(GL_LIGHT1,GL_DIFFUSE,l_diffuse1);
  glLightfv(GL_LIGHT1,GL_SPECULAR,l_specular1);
  glLightfv(GL_LIGHT1,GL_AMBIENT,l_ambient1);
  glLightf(GL_LIGHT1, GL_SPOT_EXPONENT, 1);

	// ��������� Snake
  InitialTorusCoord.x=-1;
  InitialTorusCoord.y=-1;
  InitialTorusCoord.z=2;

	// ��������� ���������
  GLfloat m_emissive[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
  GLfloat m_diffuse[4] =  { 0.5f, 0.5f, 0.5f, 0.5f };
  GLfloat m_specular[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
  GLfloat m_ambient[4] =  { 0.5f, 0.5f, 0.5f, 0.0f };
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, m_emissive);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, m_specular);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, m_ambient);
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS,1);
	
	// ��������� 0-�� ��������� �����
  glLightf(GL_LIGHT0, GL_SPOT_EXPONENT, 1);
  LightCoord.x=-3.7;
  LightCoord.y=-2.7;
  LightCoord.z=0;
  GLfloat l_position0[4]={LightCoord.x,LightCoord.y,LightCoord.z,1};
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glLightfv(GL_LIGHT0,GL_POSITION,l_position0);
  glLightf(GL_LIGHT0,GL_QUADRATIC_ATTENUATION,0.05);
  
	// ������ ��������
  glPixelStorei(GL_UNPACK_ALIGNMENT,1);
  AUX_RGBImageRec *TextureImage[10];        
  memset(TextureImage,0,sizeof(void*)*10);
  TextureImage[0]=LoadBMP("ancient02.bmp");
  TextureImage[1]=LoadBMP("Particle.bmp");
  TextureImage[2]=LoadBMP("FireAlpha.bmp");
  TextureImage[3]=LoadBMP("rock04.bmp");
  TextureImage[4]=LoadBMP("Water20.bmp");
  TextureImage[5]=LoadBMP("Wood19.bmp");
  TextureImage[6]=LoadBMP("Flow.bmp");
  TextureImage[7]=LoadBMP("FlowAlpha.bmp");
  TextureAddAlpha(TextureImage[1], TextureImage[2]);
  TextureAddAlpha(TextureImage[6], TextureImage[7]);
  glGenTextures(10, textures);      
  glBindTexture(GL_TEXTURE_2D, textures[0]);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TextureImage[0]->sizeX, TextureImage[0]->sizeY, GL_RGB, GL_UNSIGNED_BYTE, TextureImage[0]->data);
  glBindTexture(GL_TEXTURE_2D, textures[1]);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 4, TextureImage[1]->sizeX, TextureImage[1]->sizeY, GL_RGBA, GL_UNSIGNED_BYTE, TextureImage[1]->data);
  glBindTexture(GL_TEXTURE_2D, textures[2]);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TextureImage[3]->sizeX, TextureImage[3]->sizeY, GL_RGB, GL_UNSIGNED_BYTE, TextureImage[3]->data);
  glBindTexture(GL_TEXTURE_2D, textures[3]);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TextureImage[4]->sizeX, TextureImage[4]->sizeY, GL_RGB, GL_UNSIGNED_BYTE, TextureImage[4]->data);
  glBindTexture(GL_TEXTURE_2D, textures[4]);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TextureImage[5]->sizeX, TextureImage[5]->sizeY, GL_RGB, GL_UNSIGNED_BYTE, TextureImage[5]->data);
  glBindTexture(GL_TEXTURE_2D, textures[5]);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 4, TextureImage[6]->sizeX, TextureImage[6]->sizeY, GL_RGBA, GL_UNSIGNED_BYTE, TextureImage[6]->data);
  
  // ��������� ������� �����
  glGenTextures(1,&ShadowMap);
  glBindTexture(GL_TEXTURE_2D,ShadowMap);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_COMPARE_MODE_ARB,GL_COMPARE_R_TO_TEXTURE_ARB);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_COMPARE_FUNC_ARB,GL_LEQUAL);
  glTexGeni(GL_S,GL_TEXTURE_GEN_MODE,GL_EYE_LINEAR);
  glTexGeni(GL_T,GL_TEXTURE_GEN_MODE,GL_EYE_LINEAR);
  glTexGeni(GL_R,GL_TEXTURE_GEN_MODE,GL_EYE_LINEAR);
  glTexGeni(GL_Q,GL_TEXTURE_GEN_MODE,GL_EYE_LINEAR);
  unsigned char *a=new unsigned char[ShadowMapSize*ShadowMapSize];
  for(int i=0; i<ShadowMapSize*ShadowMapSize; i++)
    a[i]=255;
  glTexImage2D(GL_TEXTURE_2D,0,GL_DEPTH_COMPONENT,ShadowMapSize,ShadowMapSize,0,GL_DEPTH_COMPONENT,GL_UNSIGNED_BYTE,a);
  
	// ������ ��������
  LoadModel("cave.md",&models[0]);
  LoadModel("Walls.md",&models[1]);
  LoadModel("Walls2.md",&models[2]);

	// ������� �������������� ���
  GenerateObject();
}

// ���������� �������� ����
void GLUTCALLBACK passivemousehandler(int x, int y)
{
  if(!(x==wnd_w/2 && y==wnd_h/2))
  {
    x_angle+=(x-wnd_w/2)*M_Sensitivity; 
    y_angle+=-(y-wnd_h/2)*M_Sensitivity;
    if(x_angle<0)     x_angle+=2*PI; 
    if(x_angle>=2*PI) x_angle-=2*PI; 
    if(y_angle>PI/2)  y_angle =PI/2; 
    if(y_angle<-PI/2) y_angle =-PI/2; 
    glutWarpPointer(wnd_w/2,wnd_h/2);
  }
} 

// ���������� ������� �� �����
void GLUTCALLBACK keyhandler(unsigned char Key, int x, int y)
{
  static bool MouseHandlerOn=true;
  if(Key==27)
    exit(0);
  if(Key=='m')
  {
    if(MouseHandlerOn)
    {
      glutPassiveMotionFunc(0);
      glutSetCursor(GLUT_CURSOR_INHERIT);
    }
    else
    {
      glutSetCursor(GLUT_CURSOR_NONE);
      glutPassiveMotionFunc(passivemousehandler);
    }
    MouseHandlerOn=!MouseHandlerOn;
  }
  if(Key=='w' || Key=='W')
    key_w=true;
  if(Key=='s' || Key=='S')
    key_s=true;
  if(Key=='a' || Key=='A')
    key_a=true;
  if(Key=='d' || Key=='D')
    key_d=true;
}

// ���������� ���������� ������
void GLUTCALLBACK keyuphandler(unsigned char Key, int x, int y)
{
  if(Key=='w' || Key=='W')
    key_w=false;
  if(Key=='s' || Key=='S')
    key_s=false;
  if(Key=='a' || Key=='A')
    key_a=false;
  if(Key=='d' || Key=='D')
    key_d=false;
}


int main(int argc, char** argv)
{
	// �������������� glut
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE|GLUT_DEPTH|GLUT_RGB|GLUT_MULTISAMPLE|GLUT_STENCIL);
  glutInitWindowSize(512,512);
  glutCreateWindow("OpenGL");
  glutDisplayFunc(display);
  glutReshapeFunc(reshape);
  glutIdleFunc(idle);
  glutIgnoreKeyRepeat(1);
  glutKeyboardFunc(keyhandler);
  glutKeyboardUpFunc(keyuphandler);
  glutPassiveMotionFunc(passivemousehandler); 
  glutSetCursor(GLUT_CURSOR_NONE);
  
	// ���� ����������������
	Init();

	// ������������� Fullscreen ���� ����
  fullscreen=false;
  if (MessageBox(NULL,"Fullscreen Mode?", "Start FullScreen?",MB_YESNO|MB_ICONQUESTION)==IDYES)
  {
    DEVMODE dmScreenSettings;        
    memset(&dmScreenSettings,0,sizeof(dmScreenSettings));
    dmScreenSettings.dmSize=sizeof(dmScreenSettings);  
    dmScreenSettings.dmPelsWidth=640;    
    dmScreenSettings.dmPelsHeight=480;    
    dmScreenSettings.dmBitsPerPel=32;      
    dmScreenSettings.dmFields=DM_BITSPERPEL|DM_PELSWIDTH|DM_PELSHEIGHT;
    ChangeDisplaySettings(&dmScreenSettings,CDS_FULLSCREEN);
    glutFullScreen();  
    fullscreen=true;
  }
  // ���� ������
  BASS_Init(-1, 44100, BASS_DEVICE_NOTHREAD, 0);
  music = BASS_StreamCreateFile(false, "al_an_bugscave2.wav", 0, 0, 0); 
  if(music==NULL)
    printf("Error!");
  BASS_Start();
  BASS_ChannelSetPosition(music, (QWORD)MAKELONG(0, 0));
  BASS_StreamPlay(music, 0, BASS_SAMPLE_LOOP);
  
	// �����������!
	glutMainLoop();

	// ������� �� fullscreen'� ������������
  ChangeDisplaySettings(NULL,0);  
  return 0;
}
