#include <windows.h>
#include <gl\glut.h>
#include <stdio.h>
#include <math.h>
#include "BmpLoad.h"

#define PROGRAM_NAME "FireWorks Demo"
#define USER_X 0.5
#define USER_Y 0.5
#define USER_Z 1
#define MOUSE_SENSITIVITY 0.005
#define PI 3.1415926535897932384626433832795L
#define USK_G -0.5

struct TParticle
{
	float X,Y,Z;
	float dX,dY,dZ;
	float d2X,d2Y,d2Z;
	float R,G,B,A;
	float dR,dG,dB,dA;
	float Size;
	float dSize;
	float Life;
	float dLife;
	int Data;
	char Type; 
	/* 
	0 - head of the bullet 0
	1 - head of the bullet 1  
	2 - head of the bullet 2
	3 - head of the bullet 3
	4 - bullet trail
	5 - fragments 1
	*/
	TParticle *last, *next;
};

int w,h;

float Time, dTime;

float Phi=0, Psi=0;

TParticle* System=NULL;
int BulletCount=0;
bool SystemStopped=false;

float Lights[4][6];

GLuint SkyTex, WatTex, GraTex, ParTex;


void RemoveParticle(TParticle* p)
{
  if(p->last!=NULL)
		p->last->next=p->next;
	if(p->next!=NULL)
		p->next->last=p->last;
  delete p;
}

void AddParticle(TParticle* p)
{
  p->next=System;
  p->last=NULL;
  if(System!=NULL)
    System->last=p;
  System=p;
}

void GetRandomVector(float* x, float* y, float* z)
{
  do
  {
    (*x)=-1+2.0*rand()/RAND_MAX;
    (*y)=-1+2.0*rand()/RAND_MAX;
    (*z)=-1+2.0*rand()/RAND_MAX;
  }
  while((*x)*(*x)+(*y)*(*y)+(*z)*(*z)>1);
  float r=sqrt((*x)*(*x)+(*y)*(*y)+(*z)*(*z));
  (*x)/=r;
  (*y)/=r;
  (*z)/=r;
}

void MoveSystem()
{
	if(!SystemStopped)
	{
    TParticle* p=System;
    while(p)
    {
      bool Dead=false;
      bool Switched=false;

			p->Life+=p->dLife*dTime;
			if(p->Life<0)
        Dead=true;

			p->Size+=p->dSize*dTime;
			if(p->Size<=0)
        Dead=true;

      p->X+=p->dX*dTime;
			p->Y+=p->dY*dTime;
   		p->Z+=p->dZ*dTime;

 			p->dX+=p->d2X*dTime;
  		p->dY+=p->d2Y*dTime;
	  	p->dZ+=p->d2Z*dTime;

			p->R+=p->dR*dTime;
			p->G+=p->dG*dTime;
			p->B+=p->dB*dTime;
			p->A+=p->dA*dTime;

      if(Dead && p->Type>=4)
      {
        TParticle* p1=p->next;
        RemoveParticle(p);
        p=p1;
        Switched=true;
      }
      if(p->Type<4)
      {
        Lights[p->Type][0]=p->X;
        Lights[p->Type][1]=p->Y;
        Lights[p->Type][2]=p->Z;
        Lights[p->Type][3]=p->R;
        Lights[p->Type][4]=p->G;
        Lights[p->Type][5]=p->B;
      }
      if(p->Type<4 && p->Data!=-1)
      {
        // Trail Constructor
        TParticle* p1=new TParticle;
        AddParticle(p1);
        p1->Type=4;

        p1->X=p->X;
        p1->Y=p->Y;
        p1->Z=p->Z;

        p1->dX=0;
        p1->dY=0;
        p1->dZ=0;

        p1->d2X=0;
        p1->d2Y=0;
        p1->d2Z=0;

        p1->Size=0.25;
        p1->dSize=0;
        p1->Life=1;
        p1->dLife=-1;

        p1->R=p->R;
        p1->G=p->G;
        p1->B=p->B;
        p1->A=1;

        p1->dR=0;
        p1->dG=0;
        p1->dB=0;
        p1->dA=-1;     
      }

      if(Dead && p->Type<4 && p->Data>=0)
      {
        for(int i=0; i<300; i++)
        {
          // fragments Constructor
          TParticle* p1=new TParticle;
          AddParticle(p1);
          p1->Type=5;

          p1->X=p->X;
          p1->Y=p->Y;
          p1->Z=p->Z;

          GetRandomVector(&(p1->dX),&(p1->dY),&(p1->dZ));

          p1->d2X=0;
          p1->d2Y=0;
          p1->d2Z=USK_G;

          p1->Size=0.2;
          p1->dSize=0;
          p1->Life=3;
          p1->dLife=-1;

          if(p->Data==0)
          {
            p1->R=p->R;
            p1->G=p->G;
            p1->B=p->B;
          }
          if(p->Data==1)
          {
            p1->R=1.0*rand()/RAND_MAX;
            p1->G=1.0*rand()/RAND_MAX;
            p1->B=1.0*rand()/RAND_MAX;
          }
          p1->A=1;

          p1->dR=0;
          p1->dG=0;
          p1->dB=0;
          p1->dA=-0.33333333;
        }
        p->Data=-1;
        p->A=0;
        p->dX=0;
        p->dY=0;
        p->dZ=0;
        p->d2X=0;
        p->d2Y=0;
        p->d2Z=0;
      }

      if(Dead && p->Type<4 && p->Data==-1 && p->Life>=-1)
      {
        Lights[p->Type][3]=max(p->R*(1+p->Life)/(1),0);
        Lights[p->Type][4]=max(p->G*(1+p->Life)/(1),0);
        Lights[p->Type][5]=max(p->B*(1+p->Life)/(1),0);
      }

      if(Dead && p->Type<4 && p->Data==-1 && p->Life<-1)
      {
        p->X=-16;
        p->Y=0;
        p->Z=0.5;
        
        p->dX=-5+10.0*rand()/RAND_MAX;
        p->dY=5.0*rand()/RAND_MAX;
        p->dZ=2.0+1.5*rand()/RAND_MAX;

        p->d2X=0;
        p->d2Y=0;
        p->d2Z=USK_G;

        p->Size=0.3;
        p->dSize=0;
        p->Life=4+3.0*rand()/RAND_MAX;
        p->dLife=-1;

        p->R=1.0*rand()/RAND_MAX;
        p->G=1.0*rand()/RAND_MAX;
        p->B=1.0*rand()/RAND_MAX;
        p->A=1;

        p->dR=0;
        p->dG=0;
        p->dB=0;
        p->dA=0;

        p->Data=rand()%2;
      }

      if(!Switched)
        p=p->next;
    }
	}
}

void InitSystem()
{
  TParticle* p;
  p=new TParticle; p->Type=0; p->Data=-1; p->Life=-10; AddParticle(p);
  p=new TParticle; p->Type=1; p->Data=-1; p->Life=-10; AddParticle(p);
  p=new TParticle; p->Type=2; p->Data=-1; p->Life=-10; AddParticle(p);
  p=new TParticle; p->Type=3; p->Data=-1; p->Life=-10; AddParticle(p);
}

void DrawSystem()
{
  glBindTexture(GL_TEXTURE_2D, ParTex);
  glEnable(GL_BLEND);
  glDepthMask(GL_FALSE);
  float m[16];
	glGetFloatv(GL_MODELVIEW_MATRIX, m);
	for(TParticle* p=System;p!=NULL;p=p->next)
	{
    if(p->Z>0)
    {
      glBegin(GL_QUADS);
		  glColor4f(p->R,p->G,p->B,p->A);		
  		glTexCoord2f(0.0, 0.0); glVertex3f(p->X+(-m[0]-m[1])*p->Size/2, p->Y+(-m[4]-m[5])*p->Size/2, p->Z+(-m[8]-m[9])*p->Size/2);
	  	glTexCoord2f(1.0, 0.0); glVertex3f(p->X+( m[0]-m[1])*p->Size/2, p->Y+( m[4]-m[5])*p->Size/2, p->Z+( m[8]-m[9])*p->Size/2);
		  glTexCoord2f(1.0, 1.0); glVertex3f(p->X+( m[0]+m[1])*p->Size/2, p->Y+( m[4]+m[5])*p->Size/2, p->Z+( m[8]+m[9])*p->Size/2);
  		glTexCoord2f(0.0, 1.0); glVertex3f(p->X+(-m[0]+m[1])*p->Size/2, p->Y+(-m[4]+m[5])*p->Size/2, p->Z+(-m[8]+m[9])*p->Size/2);
    	glEnd();
    }
	}
  glDepthMask(GL_TRUE);
  glDisable(GL_BLEND);
}

void GetNormal(float r, float fi, float* x, float* y, float* z)
{
  float pr=2*r/((1+r*r)*(1+r*r));
  (*x)=pr*sin(fi);
  (*y)=pr*cos(fi);
  (*z)=1;
}

void DrawIsland(float x, float y)
{
  glBindTexture(GL_TEXTURE_2D, GraTex);
  glBegin(GL_QUADS);
  for(int i=0; i<10; i++) for(int j=0; j<10; j++)
  {
    float r0=3*i/10.0f,     fi0=2*PI*j/10.0f;
    float r1=3*(i+1)/10.0f, fi1=2*PI*j/10.0f;
    float r2=3*(i+1)/10.0f, fi2=2*PI*(j+1)/10.0f;
    float r3=3*i/10.0f,     fi3=2*PI*(j+1)/10.0f;
    float x0=x+r0*sin(fi0), y0=y+r0*cos(fi0), z0=1/(1+r0*r0)-0.1;
    float x1=x+r1*sin(fi1), y1=y+r1*cos(fi1), z1=1/(1+r1*r1)-0.1;
    float x2=x+r2*sin(fi2), y2=y+r2*cos(fi2), z2=1/(1+r2*r2)-0.1;
    float x3=x+r3*sin(fi3), y3=y+r3*cos(fi3), z3=1/(1+r3*r3)-0.1;
    float nx0,ny0,nz0;
    GetNormal(r0,fi0,&nx0,&ny0,&nz0);
    float nx1,ny1,nz1;
    GetNormal(r1,fi1,&nx1,&ny1,&nz1);
    float nx2,ny2,nz2;
    GetNormal(r2,fi2,&nx2,&ny2,&nz2);
    float nx3,ny3,nz3;
    GetNormal(r3,fi3,&nx3,&ny3,&nz3);
    glNormal3f(nx0,ny0,nz0); glTexCoord2f(x0,y0); glVertex3f(x0,y0,z0);
    glNormal3f(nx1,ny1,nz1); glTexCoord2f(x1,y1); glVertex3f(x1,y1,z1);
    glNormal3f(nx2,ny2,nz2); glTexCoord2f(x2,y2); glVertex3f(x2,y2,z2);
    glNormal3f(nx3,ny3,nz3); glTexCoord2f(x3,y3); glVertex3f(x3,y3,z3);
  }         
  glEnd();
}

void DrawUpScene()
{
  GLfloat l_position[4];
  GLfloat l_diffuse[4];
  GLfloat l_specular[4];

  l_position[0]=Lights[0][0];l_position[1]=Lights[0][1];l_position[2]=Lights[0][2];l_position[3]=1;
  l_diffuse[0]= Lights[0][3];l_diffuse[1]= Lights[0][4];l_diffuse[2]= Lights[0][5];l_diffuse[3]=1;
  l_specular[0]=Lights[0][3];l_specular[1]=Lights[0][4];l_specular[2]=Lights[0][5];l_specular[3]=1;
  glLightfv(GL_LIGHT0,GL_POSITION,l_position);
  glLightfv(GL_LIGHT0,GL_DIFFUSE,l_diffuse);
  glLightfv(GL_LIGHT0,GL_SPECULAR,l_specular);
  l_position[0]=Lights[1][0];l_position[1]=Lights[1][1];l_position[2]=Lights[1][2];l_position[3]=1;
  l_diffuse[0]= Lights[1][3];l_diffuse[1]= Lights[1][4];l_diffuse[2]= Lights[1][5];l_diffuse[3]=1;
  l_specular[0]=Lights[1][3];l_specular[1]=Lights[1][4];l_specular[2]=Lights[1][5];l_specular[3]=1;
  glLightfv(GL_LIGHT1,GL_POSITION,l_position);
  glLightfv(GL_LIGHT1,GL_DIFFUSE,l_diffuse);
  glLightfv(GL_LIGHT1,GL_SPECULAR,l_specular);
  l_position[0]=Lights[2][0];l_position[1]=Lights[2][1];l_position[2]=Lights[2][2];l_position[3]=1;
  l_diffuse[0]= Lights[2][3];l_diffuse[1]= Lights[2][4];l_diffuse[2]= Lights[2][5];l_diffuse[3]=1;
  l_specular[0]=Lights[2][3];l_specular[1]=Lights[2][4];l_specular[2]=Lights[2][5];l_specular[3]=1;
  glLightfv(GL_LIGHT2,GL_POSITION,l_position);
  glLightfv(GL_LIGHT2,GL_DIFFUSE,l_diffuse);
  glLightfv(GL_LIGHT2,GL_SPECULAR,l_specular);
  l_position[0]=Lights[3][0];l_position[1]=Lights[3][1];l_position[2]=Lights[3][2];l_position[3]=1;
  l_diffuse[0]= Lights[3][3];l_diffuse[1]= Lights[3][4];l_diffuse[2]= Lights[3][5];l_diffuse[3]=1;
  l_specular[0]=Lights[3][3];l_specular[1]=Lights[3][4];l_specular[2]=Lights[3][5];l_specular[3]=1;
  glLightfv(GL_LIGHT3,GL_POSITION,l_position);
  glLightfv(GL_LIGHT3,GL_DIFFUSE,l_diffuse);
  glLightfv(GL_LIGHT3,GL_SPECULAR,l_specular);

  glColor4f(1,1,1,1);
  glEnable(GL_LIGHTING);
  DrawIsland(0,0);
  DrawIsland(-3,-3);
  DrawIsland(-15,0);
  glDisable(GL_LIGHTING);
  glBindTexture(GL_TEXTURE_2D, SkyTex);
  glBegin(GL_QUADS);
    glTexCoord2f(0,0);glVertex3f(-80,-80, 80);
    glTexCoord2f(2,0);glVertex3f( 80,-80, 80);
    glTexCoord2f(2,2);glVertex3f( 80, 80, 80);
    glTexCoord2f(0,2);glVertex3f(-80, 80, 80);

    glTexCoord2f(0,0);glVertex3f(-80, 80, 80);
    glTexCoord2f(2,0);glVertex3f( 80, 80, 80);
    glTexCoord2f(2,1);glVertex3f( 80, 80, 0);
    glTexCoord2f(0,1);glVertex3f(-80, 80, 0);

    glTexCoord2f(0,0);glVertex3f(-80,-80, 80);
    glTexCoord2f(2,0);glVertex3f( 80,-80, 80);
    glTexCoord2f(2,1);glVertex3f( 80,-80, 0);
    glTexCoord2f(0,1);glVertex3f(-80,-80, 0);

    glTexCoord2f(0,0);glVertex3f(-80,-80, 80);
    glTexCoord2f(2,0);glVertex3f(-80, 80, 80);
    glTexCoord2f(2,1);glVertex3f(-80, 80, 0);
    glTexCoord2f(0,1);glVertex3f(-80,-80, 0);

    glTexCoord2f(0,0);glVertex3f( 80,-80, 80);
    glTexCoord2f(2,0);glVertex3f( 80, 80, 80);
    glTexCoord2f(2,1);glVertex3f( 80, 80, 0);
    glTexCoord2f(0,1);glVertex3f( 80,-80, 0);
  glEnd();  
  DrawSystem();
}

void OnDisplay()
{
  static int LastTime=GetTickCount();
  dTime=-(LastTime-GetTickCount())/1000.0f;
  Time+=dTime;
  LastTime=GetTickCount();

  glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
  
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(75.0,(float)w/h,0.1,160);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(USER_X,USER_Y,USER_Z,USER_X+sin(Phi)*cos(Psi),USER_Y+cos(Phi)*cos(Psi),USER_Z+sin(Psi),0,0,1);

  MoveSystem();

  //draw reflection
  glPushMatrix();
  glScalef(1,1,-1);
  DrawUpScene();
  glPopMatrix();

  //draw water
  glEnable(GL_BLEND);
  glBindTexture(GL_TEXTURE_2D, WatTex);
  glColor4f(1,1,1,0.5);
  glBegin(GL_QUADS);
  for(int x=-80; x<80; x++) for(int y=-80; y<80; y++)
  {
    glTexCoord2f(x*0.5f+0.1*Time,y*0.5f);        glVertex3f(x,y,0);
    glTexCoord2f((x+1)*0.5f+0.1*Time,y*0.5f);    glVertex3f((x+1),y,0);
    glTexCoord2f((x+1)*0.5f+0.1*Time,(y+1)*0.5f);glVertex3f((x+1),(y+1),0);
    glTexCoord2f(x*0.5f+0.1*Time,(y+1)*0.5f);    glVertex3f(x,(y+1),0);
  }
  glEnd();
  glDisable(GL_BLEND);

  //draw Scene
  DrawUpScene();
  
  glutSwapBuffers();
}

void OnReshape(int x, int y)
{
  w=x; h=y;
  glViewport(0,0,w,h);
}

void OnIdle()
{
  glutPostRedisplay();
}

void OnKeyDown(unsigned char Key, int x, int y)
{
  if(Key==27)
    exit(0);
}

void OnMouseMove(int x, int y)
{
  if(!(x==w/2 && y==h/2))
  {
    Phi+=(x-w/2)*MOUSE_SENSITIVITY;
    Psi+=(h/2-y)*MOUSE_SENSITIVITY;

    if(Psi> PI*0.45f) Psi= PI*0.45f; 
    if(Psi<-PI*0.45f) Psi=-PI*0.45f; 

    glutWarpPointer(w/2,h/2);
  }
}

bool Init()
{
  srand(timeGetTime());
  
  glClearColor(0,0,0,0); 
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_NORMALIZE);
  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
  glShadeModel(GL_SMOOTH);
  glDisable(GL_CULL_FACE);
  glDisable(GL_LIGHTING);
  glHint(GL_POINT_SMOOTH_HINT,GL_NICEST);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  
  glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER,1);
	glLightModeli(GL_LIGHT_MODEL_TWO_SIDE,0);

	glEnable(GL_LIGHT0);
  GLfloat l_position[4]={0,0,3,1};
  GLfloat l_diffuse[4] ={0,0.5,0.8,1};
  GLfloat l_specular[4]={0,0.4,0.8,1};
  GLfloat l_ambient[4] ={0.1,0.1,0.1,1};
  glLightfv(GL_LIGHT0,GL_POSITION,l_position);
  glLightfv(GL_LIGHT0,GL_DIFFUSE,l_diffuse);
  glLightfv(GL_LIGHT0,GL_SPECULAR,l_specular);
  glLightfv(GL_LIGHT0,GL_AMBIENT,l_ambient);
  glLightf(GL_LIGHT0,GL_QUADRATIC_ATTENUATION,0.05);
  glLightf(GL_LIGHT0,GL_SPOT_EXPONENT, 1);
	glEnable(GL_LIGHT1);
  glLightfv(GL_LIGHT1,GL_POSITION,l_position);
  glLightfv(GL_LIGHT1,GL_DIFFUSE,l_diffuse);
  glLightfv(GL_LIGHT1,GL_SPECULAR,l_specular);
  glLightfv(GL_LIGHT1,GL_AMBIENT,l_ambient);
  glLightf(GL_LIGHT1,GL_QUADRATIC_ATTENUATION,0.05);
  glLightf(GL_LIGHT1,GL_SPOT_EXPONENT, 1);
	glEnable(GL_LIGHT2);
  glLightfv(GL_LIGHT2,GL_POSITION,l_position);
  glLightfv(GL_LIGHT2,GL_DIFFUSE,l_diffuse);
  glLightfv(GL_LIGHT2,GL_SPECULAR,l_specular);
  glLightfv(GL_LIGHT2,GL_AMBIENT,l_ambient);
  glLightf(GL_LIGHT2,GL_QUADRATIC_ATTENUATION,0.05);
  glLightf(GL_LIGHT2,GL_SPOT_EXPONENT, 1);
	glEnable(GL_LIGHT3);
  glLightfv(GL_LIGHT3,GL_POSITION,l_position);
  glLightfv(GL_LIGHT3,GL_DIFFUSE,l_diffuse);
  glLightfv(GL_LIGHT3,GL_SPECULAR,l_specular);
  glLightfv(GL_LIGHT3,GL_AMBIENT,l_ambient);
  glLightf(GL_LIGHT3,GL_QUADRATIC_ATTENUATION,0.05);
  glLightf(GL_LIGHT3,GL_SPOT_EXPONENT, 1);
  
  GLfloat m_emissive[4]={0.1, 0.1, 0.1, 0.0};
  GLfloat m_diffuse[4] ={0.4, 0.4, 0.4, 0.0};
  GLfloat m_specular[4]={0.4, 0.4, 0.4, 0.0};
  GLfloat m_ambient[4] ={0.4, 0.4, 0.4, 0.0};
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, m_emissive);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, m_specular);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, m_ambient);
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS,8);

  glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
	glPixelStorei(GL_UNPACK_ALIGNMENT,1);
	glGenTextures(1,&SkyTex);
	glGenTextures(1,&WatTex);
	glGenTextures(1,&GraTex);
	glGenTextures(1,&ParTex);
  int w,h;
  unsigned char *bits;
	if((bits=LoadTrueColorBMPFile("Textures/Sky.bmp",&w,&h)) == NULL)
		return false;
	glBindTexture(GL_TEXTURE_2D,SkyTex);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, w, h, GL_RGB, GL_UNSIGNED_BYTE, bits);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  delete[] bits;
 	if((bits=LoadTrueColorBMPFile("Textures/Rock.bmp",&w,&h)) == NULL)
		return false;
	glBindTexture(GL_TEXTURE_2D,GraTex);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, w, h, GL_RGB, GL_UNSIGNED_BYTE, bits);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  delete[] bits;
  if((bits=LoadTrueColorBMPFile("Textures/Water.bmp",&w,&h)) == NULL)
		return false;
	glBindTexture(GL_TEXTURE_2D,WatTex);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, w, h, GL_RGB, GL_UNSIGNED_BYTE, bits);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  delete[] bits;
  if((bits=ConstructTexture(&w,&h,"Textures/Particle.bmp","Textures/ParticleAlphaChannel.bmp")) == NULL)
		return false;
	glBindTexture(GL_TEXTURE_2D,ParTex);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 4, w, h, GL_RGBA, GL_UNSIGNED_BYTE, bits);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  delete[] bits;

  InitSystem();

  return true;
}




int main(int argc, char** argv)
{
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE|GLUT_DEPTH|GLUT_RGB|GLUT_ALPHA);

  glutInitWindowSize(800,600);
  glutCreateWindow(PROGRAM_NAME);
 
  if(!Init())
  {
    printf("%s","Error in initialization!");
    return 1;
  }

  glutIdleFunc(OnIdle);
  glutPassiveMotionFunc(OnMouseMove); 
  glutDisplayFunc(OnDisplay);
  glutKeyboardFunc(OnKeyDown);
  glutReshapeFunc(OnReshape);

  glutSetCursor(GLUT_CURSOR_NONE);

  DEVMODE devmode;        
  memset(&devmode,0,sizeof(devmode));
  devmode.dmSize=sizeof(devmode);  
  devmode.dmBitsPerPel=32;
  devmode.dmPelsWidth=800;    
  devmode.dmPelsHeight=600;    
  devmode.dmFields=DM_PELSWIDTH|DM_BITSPERPEL|DM_PELSHEIGHT;
  ChangeDisplaySettings(&devmode,CDS_FULLSCREEN);
  glutFullScreen();  

  glutMainLoop();
  return 0;
}