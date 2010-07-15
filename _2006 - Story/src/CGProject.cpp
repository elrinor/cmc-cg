#include <gl/glut.h>
#include <math.h>
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include "BmpLoad.h"
#include "FPSCounter.h"
#include "Timer.h"
#include "Vector.h"


#define PI 3.1415926535897932
#define NFi 32
#define NPsi 16
#define BigR 1 
#define SmallR 0.3 
#define TorusPhaseCount 128


struct Quad{
  CVector v[4];
  CVector n[4];
  CVector t[4];
};

GLuint tex0, skytex, steeltex, metaltex, marbletex, texttex;

CVector Me, Target;

GlutFPSDisplay fps("FPS = %.2lf",1,0,1,0,20);
float now=0;

CVector a[NFi+1][NPsi+1][3][TorusPhaseCount];
float rads[NFi][NPsi];
int ind[NFi*NPsi][2];

GLUquadricObj *qobj;

void WriteText(char* text, float r=0, float g=1, float b=0, float fontSize=24)
{
	glPushAttrib(GL_ENABLE_BIT);
	glDisable(GL_LIGHTING);
	glDisable(GL_BLEND);
	glDisable(GL_DEPTH_TEST);
	glDisable(GL_TEXTURE_2D);
	glMatrixMode(GL_PROJECTION);
	glPushMatrix();
	glLoadIdentity();
	glOrtho(0,glutGet(GLUT_WINDOW_WIDTH),0,glutGet(GLUT_WINDOW_HEIGHT),0,1);
	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
	glLoadIdentity();
	double fontScale = fontSize/(119.05 + 33.33);
	glColor3d(r,g,b);
  glTranslated( (glutGet(GLUT_WINDOW_WIDTH) - glutStrokeLength(GLUT_STROKE_ROMAN, (const unsigned char*)text)*fontScale) / 2, 120*fontScale,0);
    glScalef(fontScale,fontScale,fontScale);
	for(const char *p = text; *p; p++)
		glutStrokeCharacter(GLUT_STROKE_ROMAN, *p);
	glPopMatrix();
	glMatrixMode(GL_PROJECTION);
	glPopMatrix();
	glPopAttrib();
}

void GenToruses()
{
  double fimax,r1,r2;
  double t,T;

  for(int i=0;i<TorusPhaseCount;i++)
  {
    t=((double)i/TorusPhaseCount)*5-2.5;
    T=min(max(t,-2),2);
    r1=T/2*BigR;
    r2=(2.5-1.5*abs(T/2))*BigR;
    fimax=2*PI*BigR/(abs(r1)+(3-abs(T))*abs(r2));
    for(int j=0; j<=NFi; j++)
    {
      double fi=fimax*((double)j/NFi-0.5)*2;
      double r=SmallR*min(1,sqrt(1-fi*fi/(fimax*fimax))+max(0,2*(abs(t)-2)));
      CVector Sn;
      if(abs(r-SmallR)<0.00001)
        Sn.Set(0,1,0);
      else
        Sn.Set(SmallR*BigR*fi/sqrt(fi*fi*BigR*BigR+r*r),fimax*BigR*r/sqrt(fi*fi*BigR*BigR+r*r),0);
      CVector u(0,0,1);
      CVector l(-r1*sin(fi),r2*cos(fi),0);
      CVector n(r2*cos(fi),r1*sin(fi),0);
      Sn.Normalize();
      l.Normalize();
      n.Normalize();
      for(int k=0;k<=NPsi;k++)
      {
        double psi=PI*2*(double)k/NPsi;
        a[j][k][1][i]=a[j][k][0][i]=n*r*cos(psi)+u*r*sin(psi);
        a[j][k][1][i].Normalize();
        a[j][k][0][i]+=CVector(r1*cos(fi),r2*sin(fi),0);
        a[j][k][2][i]=CVector(a[j][k][0][i].x,a[j][k][0][i].y,1);
        float tmp;
        tmp=a[j][k][0][i].x;a[j][k][0][i].x=a[j][k][0][i].z;a[j][k][0][i].z=tmp;
        tmp=a[j][k][1][i].x;a[j][k][1][i].x=a[j][k][1][i].z;a[j][k][1][i].z=tmp;
      }
    }
  }
}

int mycmp(const void* a, const void* b)
{
  if(rads[((int*)a)[0]][((int*)a)[1]]>rads[((int*)b)[0]][((int*)b)[1]])
    return -1;
  else if(rads[((int*)a)[0]][((int*)a)[1]]<rads[((int*)b)[0]][((int*)b)[1]])
    return 1;
  else 
    return 0;
}

void SortTorus()
{
  int n=((int)(now*0.5*TorusPhaseCount))%TorusPhaseCount;
  
  int c=0;
  for(int j=0;j<NFi;j++)
  {
    for(int k=0;k<NPsi;k++)
    {
      CVector v=a[j][k][0][n]-Me;
      rads[j][k]=v.x*v.x+v.y*v.y+v.z*v.z;
      ind[c][0]=j;
      ind[c][1]=k;
      c++;
    }
  }
  qsort(&ind,NFi*NPsi,2*sizeof(int),mycmp); 
}


void DrawTorus()
{
  int n=((int)(now*0.5*TorusPhaseCount))%TorusPhaseCount;
  for(int i=0;i<NFi*NPsi;i++)
  {
    glBegin(GL_QUADS);
    glNormal3fv(a[ind[i][0]  ][ind[i][1]  ][1][n].v);glTexCoord2fv(a[ind[i][0]  ][ind[i][1]  ][2][n].v);glVertex3fv(a[ind[i][0]  ][ind[i][1]  ][0][n].v);
    glNormal3fv(a[ind[i][0]+1][ind[i][1]  ][1][n].v);glTexCoord2fv(a[ind[i][0]+1][ind[i][1]  ][2][n].v);glVertex3fv(a[ind[i][0]+1][ind[i][1]  ][0][n].v);
    glNormal3fv(a[ind[i][0]+1][ind[i][1]+1][1][n].v);glTexCoord2fv(a[ind[i][0]+1][ind[i][1]+1][2][n].v);glVertex3fv(a[ind[i][0]+1][ind[i][1]+1][0][n].v);
    glNormal3fv(a[ind[i][0]  ][ind[i][1]+1][1][n].v);glTexCoord2fv(a[ind[i][0]  ][ind[i][1]+1][2][n].v);glVertex3fv(a[ind[i][0]  ][ind[i][1]+1][0][n].v);
    glEnd();
  }     
}

unsigned char *ConstructTexture(int *w,int *h)
{
	int width1,height1;
	unsigned char *tex1 = LoadIndexedBMPFile("crystal.bmp",&width1,&height1);
	if(!tex1)
		return NULL;
	int width2,height2;
	unsigned char *tex2 = LoadIndexedBMPFile("crystal4.bmp",&width2,&height2);
	if(!tex2) {
		delete[] tex1;
		return NULL;
	}
	if(width1 != width2 || height1 != height2) {
		delete[] tex1;
		delete[] tex2;
		return NULL;
	}
	unsigned char *result = new unsigned char [width1*height1*4];
	if(result != NULL) {
		for(int i = 0;i<width1*height1;i++) {
			result[4*i] = tex1[3*i];
			result[4*i + 1] = tex1[3*i + 1];
			result[4*i + 2] = tex1[3*i + 2];
			result[4*i + 3] = (unsigned char)(((int)tex2[3*i] + (int)tex2[3*i + 1] + (int)tex2[3*i + 2])/3);
		}
	}	
	delete[] tex1;
	delete[] tex2;
	*w = width1;
	*h = height1;
	return result;
}


void ShadowDroppers()
{
  glBindTexture(GL_TEXTURE_2D,steeltex);
  glPushMatrix();
  glTranslatef(sin(PI*now),0,1-cos(PI*now));
  gluSphere(qobj,0.3,16,16);
  glPopMatrix();

  glBindTexture(GL_TEXTURE_2D,tex0);
  glEnable(GL_BLEND);
  SortTorus();
  DrawTorus();
  glDisable(GL_BLEND);
}

void DrawEverything()
{
  glColor3f(1,1,1);

  // Sky
  glBindTexture(GL_TEXTURE_2D,skytex);
  glBegin(GL_QUAD_STRIP);
  for(int i=0; i<11; i++)
  {
    glTexCoord2f(2.8,i/5.0);glVertex3f(60*sin(2*PI*i/10),60*cos(2*PI*i/10),90);
    glTexCoord2f(0.8,i/5.0);glVertex3f(60*sin(2*PI*i/10),60*cos(2*PI*i/10),-90);
  }
  glEnd();

  // LightSource
  GLfloat l_position[4]={8,0,0,1};
  glLightfv(GL_LIGHT0,GL_POSITION,l_position);

  // Torch
  glBindTexture(GL_TEXTURE_2D,steeltex);
  glEnable(GL_LIGHTING);
  glBegin(GL_QUAD_STRIP);
  for(int i=0; i<21; i++)
  {
    glNormal3f(0,sin(2*PI*i/20),cos(2*PI*i/20));
    glTexCoord2f(0,i/20.0);glVertex3f(8, 1*sin(2*PI*i/20),1*cos(2*PI*i/20));
    glTexCoord2f(1,i/20.0);glVertex3f(10,0.5*sin(2*PI*i/20),0.5*cos(2*PI*i/20));
  }
  glEnd();
  glBindTexture(GL_TEXTURE_2D,metaltex);
  glDisable(GL_LIGHTING);
  glBegin(GL_TRIANGLES);
  for(int i=0; i<21; i++)
  {
    glNormal3f(-1,0,0);                         glTexCoord2f(0.5,i/20.0);glVertex3f(9,0,0);
    glNormal3f(0,sin(2*PI*i/20),cos(2*PI*i/20));glTexCoord2f(0,  i/20.0);glVertex3f(8,1*sin(2*PI*i/20),1*cos(2*PI*i/20));
    int j=i+1;
    glNormal3f(0,sin(2*PI*j/20),cos(2*PI*j/20));glTexCoord2f(0,  j/20.0);glVertex3f(8,1*sin(2*PI*j/20),1*cos(2*PI*j/20));
  }
  glEnd();

  // Sphere in the Torch
  glDisable(GL_TEXTURE_2D);
  glPushMatrix();
  glTranslatef(8,0,0);
  glColor3f(1,1,0);
  glutSolidSphere(0.1, 6, 6);
  glPopMatrix();
  glEnable(GL_TEXTURE_2D);

  // Screen
  glEnable(GL_LIGHTING);
  glDisable(GL_DEPTH_TEST);
  glEnable(GL_STENCIL_TEST);
  glStencilFunc(GL_ALWAYS,1,1);
	glStencilOp(GL_KEEP,GL_KEEP,GL_REPLACE);
  glBindTexture(GL_TEXTURE_2D,marbletex);
  glBegin(GL_QUADS);
  glNormal3f(1,0,0);
  for(int i=0; i<61; i++)
  {
    glTexCoord2f(2*sin(2*PI*i/60),2*cos(2*PI*i/60));        glVertex3f(-9, 6*sin(2*PI*i/60),6*cos(2*PI*i/60));
    glTexCoord2f(2*sin(2*PI*(i+1)/60),2*cos(2*PI*(i+1)/60));glVertex3f(-9, 6*sin(2*PI*(i+1)/60),6*cos(2*PI*(i+1)/60));
    glTexCoord2f(1/6.0*sin(2*PI*(i+1)/60),1/6.0*cos(2*PI*(i+1)/60));glVertex3f(-9, 0.5*sin(2*PI*(i+1)/60),0.5*cos(2*PI*(i+1)/60));
    glTexCoord2f(1/6.0*sin(2*PI*i/60),1/6.0*cos(2*PI*i/60));        glVertex3f(-9, 0.5*sin(2*PI*i/60),0.5*cos(2*PI*i/60));
  }
  glEnd();
  glDisable(GL_LIGHTING);

  // Shadows
  glColorMask(GL_FALSE,GL_FALSE,GL_FALSE,GL_FALSE);
  glStencilFunc(GL_EQUAL,3,1);
	glStencilOp(GL_KEEP,GL_KEEP,GL_REPLACE);
  glPushMatrix();
  glTranslatef(-9,0,0);
  glScalef(0,1,1);
  ShadowDroppers();
  glPopMatrix();
  
  glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
  glStencilFunc(GL_EQUAL,3,3);
  glPushMatrix();
  glLoadIdentity();
  gluLookAt(0,0,-1,0,0,0,1,0,0);
  glDisable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glColor4f(0,0,0,0.5);
  glBegin(GL_QUADS);
    glVertex3f(-2,-2,0);
    glVertex3f( 2,-2,0);
    glVertex3f( 2, 2,0);
    glVertex3f(-2, 2,0);
  glEnd();
  glDisable(GL_BLEND);
  glEnable(GL_TEXTURE_2D);
  glPopMatrix();  
  glDisable(GL_STENCIL_TEST);
  glEnable(GL_DEPTH_TEST);

  // Scene
  glEnable(GL_LIGHTING);
  ShadowDroppers();
  glDisable(GL_LIGHTING);

  // Text
  glDisable(GL_DEPTH_TEST);
  glPushMatrix();
  glLoadIdentity();
  gluLookAt(0,0,-1,0,0,0,1,0,0);
  glDisable(GL_TEXTURE_2D);
  glColor4f(0,0,0,0);
  glBegin(GL_QUADS);
    glVertex3f(-1,  -2,0);
    glVertex3f(-0.65,-2,0);
    glVertex3f(-0.65, 2,0);
    glVertex3f(-1,   2,0);
    glVertex3f(1,  -2,0);
    glVertex3f(0.65,-2,0);
    glVertex3f(0.65, 2,0);
    glVertex3f(1,   2,0);    
  glEnd();
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D,texttex);
  glColor4f(1,1,1,1);
  glBegin(GL_QUADS);
    glTexCoord2f(0,0.77-now/40);glVertex3f(-1,   -0.5,0);
    glTexCoord2f(0,1.07-now/40);glVertex3f(-0.65,-0.5,0);
    glTexCoord2f(1,1.07-now/40);glVertex3f(-0.65, 0.5,0);
    glTexCoord2f(1,0.77-now/40);glVertex3f(-1,    0.5,0);
  glEnd();
  glPopMatrix();  
  glEnable(GL_DEPTH_TEST);
}

void display(void)
{
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_STENCIL_BUFFER_BIT);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(Me.x,Me.y,Me.z,Target.x,Target.y,Target.z,0,0,1);

  DrawEverything();
  fps.Paint();

  glFlush();
  glutSwapBuffers();
}

void idle() 
{
  fps.NextFrame();
  static float last=timeGetTime()/1000.0;
  now+=timeGetTime()/1000.0-last;
  now=max(now,0);
  last=timeGetTime()/1000.0;

  
  float  position[5][3]={{6,0,0},{-3,-3,0},{2,-12,2},{6,3,0},{6,0,0}};
  float direction[5][3]={{8,0,0},{0,0,0},{-6,0,0},{0,0,0},{8,0,0}};
  
  float time[5]={0,10,20,30,40};

  int i;
  if(now>40)
    now-=40;
  {
    for(i=0; i<4; i++) 
      if(now>=time[i] && now<=time[i+1]) 
        break;
  }
  
  CVector p1( position[i  ][0], position[i  ][1], position[i  ][2]);
  CVector p2( position[i+1][0], position[i+1][1], position[i+1][2]);
  CVector d1(direction[i  ][0],direction[i  ][1],direction[i  ][2]);
  CVector d2(direction[i+1][0],direction[i+1][1],direction[i+1][2]);

  Me=(p1*(-now+time[i+1])+p2*(-time[i]+now))*(1/(time[i+1]-time[i]));
  Target=(d1*(-now+time[i+1])+d2*(-time[i]+now))*(1/(time[i+1]-time[i]));

	glutPostRedisplay();
}

void reshape(int w, int h)
{
	if(h > 0) {
		glViewport(0,0,w,h);
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		gluPerspective(75.0,w/(GLdouble)h,0.1,100);
    glMatrixMode(GL_MODELVIEW);
	}
}

bool init()
{
  glClearColor(0,0,0,0);
  glEnable(GL_DEPTH_TEST);
  glDisable(GL_CULL_FACE);
  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
  glDisable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glShadeModel(GL_SMOOTH);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glEnable(GL_TEXTURE_2D);
  
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER,GL_TRUE);
	glLightModeli(GL_LIGHT_MODEL_TWO_SIDE,GL_FALSE);

  unsigned char *data = NULL;
  int tex_width, tex_height;

	glPixelStorei(GL_UNPACK_ALIGNMENT,1);
	if((data = ConstructTexture(&tex_width,&tex_height)) == NULL)
		return false;
	glGenTextures(1,&tex0);
  glGenTextures(1,&skytex);
  glGenTextures(1,&steeltex);
  glGenTextures(1,&metaltex);
  glGenTextures(1,&marbletex);
  glGenTextures(1,&texttex);
  glBindTexture(GL_TEXTURE_2D,tex0);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 4, tex_width, tex_height, GL_RGBA, GL_UNSIGNED_BYTE, data);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  delete[] data;

  if((data=LoadTrueColorBMPFile("steel.bmp",&tex_width,&tex_height)) == NULL)
		return false;
	glBindTexture(GL_TEXTURE_2D,steeltex);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, tex_width, tex_height, GL_RGB, GL_UNSIGNED_BYTE, data);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  delete[] data;    

  if((data=LoadTrueColorBMPFile("stars.bmp",&tex_width,&tex_height)) == NULL)
		return false;
	glBindTexture(GL_TEXTURE_2D,skytex);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, tex_width, tex_height, GL_RGB, GL_UNSIGNED_BYTE, data);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  delete[] data;
  
  if((data=LoadTrueColorBMPFile("metal.bmp",&tex_width,&tex_height)) == NULL)
		return false;
	glBindTexture(GL_TEXTURE_2D,metaltex);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, tex_width, tex_height, GL_RGB, GL_UNSIGNED_BYTE, data);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  delete[] data;
  
  if((data=LoadTrueColorBMPFile("marble.bmp",&tex_width,&tex_height)) == NULL)
		return false;
	glBindTexture(GL_TEXTURE_2D,marbletex);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, tex_width, tex_height, GL_RGB, GL_UNSIGNED_BYTE, data);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  delete[] data;

  if((data=LoadTrueColorBMPFile("Text.bmp",&tex_width,&tex_height)) == NULL)
		return false;
	glBindTexture(GL_TEXTURE_2D,texttex);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, tex_width, tex_height, GL_RGB, GL_UNSIGNED_BYTE, data);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  delete[] data;
  
	glTexEnvi(GL_TEXTURE_2D,GL_TEXTURE_ENV_MODE,GL_MODULATE);
	glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);

  GLfloat l_position[4]={8,0,0,1};
  GLfloat l_diffuse_[4]={0.5,0.5,0.5,1};
  GLfloat l_specular[4]={0.5,0.5,0.5,1};
  GLfloat l_ambient_[4]={0,0,0,1};
  glLightfv(GL_LIGHT0,GL_POSITION,l_position);
  glLightfv(GL_LIGHT0,GL_DIFFUSE, l_diffuse_);
  glLightfv(GL_LIGHT0,GL_SPECULAR,l_specular);
  glLightfv(GL_LIGHT0,GL_AMBIENT, l_ambient_);

  GLfloat m_emissive[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
  GLfloat m_diffuse[4] =  { 0.5f, 0.5f, 0.5f, 1.0f };
  GLfloat m_specular[4] = { 0.5f, 0.5f, 0.5f, 0.0f };
  GLfloat m_ambient[4] =  { 0.5f, 0.5f, 0.5f, 0.0f };
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, m_emissive);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, m_specular);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, m_ambient);
  glMaterialf (GL_FRONT_AND_BACK,GL_SHININESS,20);

  GenToruses();

  qobj=gluNewQuadric();
  gluQuadricTexture(qobj,GL_TRUE);
}

void key(unsigned char Key, int x, int y)
{
  if(Key==27)
    exit(0);
}

int main(int argc, char** argv)
{
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE|GLUT_DEPTH|GLUT_RGB|GLUT_STENCIL);
  glutInitWindowSize(1024,768);
  glutCreateWindow("");
  glutSetCursor(GLUT_CURSOR_NONE);
  
  glutDisplayFunc(display);
  glutReshapeFunc(reshape);
  glutIdleFunc(idle);
  glutKeyboardFunc(key);
  
	if(!init())
  {
    printf("Initialization error...\n");
    exit(1);
  }

  DEVMODE dmSS;        
  memset(&dmSS,0,sizeof(dmSS));
  dmSS.dmSize=sizeof(dmSS);  
  dmSS.dmPelsWidth=1024;    
  dmSS.dmPelsHeight=768;    
  dmSS.dmBitsPerPel=32;      
  dmSS.dmFields=DM_BITSPERPEL|DM_PELSWIDTH|DM_PELSHEIGHT;
  ChangeDisplaySettings(&dmSS,CDS_FULLSCREEN);
  glutFullScreen(); 

  glutMainLoop();
  return 0;
}