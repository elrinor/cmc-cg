#include <gl\glut.h>
#include <windows.h>
#include <math.h>
#include <stdio.h>
#include "vector.h"
#include "meshes.h"
#include "BmpLoad.h"


#define P_MEBIUS_RIBBON 0
#define P_KLEIN_BOTTLE 1
#define P_KLEIN_BOTTLE_TYPE_2 2
#define PI 3.141592

int ptype=P_MEBIUS_RIBBON; //������� ��� �������

int wnd_w,wnd_h; // ������� ���� ���������
float t=0, dt; // ����� � ������ ������ ��������� / � ����������� ����� (���)
int m_x,m_y,m_dx=0,m_dy=0;  // ��������� ���� � �� �������� � ����������� �����
bool lmbpressed, rmbpressed; // ������ �� �����/������ ������ ����
bool Wireframe=false; // ���������� ������?
bool Shadow=true; // ���������� ����?

GLuint MainTexture; // �������� �������
GLuint FloorTexture; // �������� ����

Mesh Meshes[3]; // �������

CVector Camera(0,4,0), Light; // ���������� ������ � ��������� �����


// ����� ������� ���, t, dt � �������� �����������
void __idle()
{
  static int LastTC=GetTickCount(); 
  static int LastFpsTime=0;
  static int FramesPassed=0;
  dt=(double)(GetTickCount()-LastTC)/1000; 
  t+=dt; 
  LastTC=GetTickCount();
  FramesPassed++;
  if(GetTickCount()-LastFpsTime>1000) 
  {
    double fps;
    fps=1000*FramesPassed/(double)(GetTickCount()-LastFpsTime);
    FramesPassed=0;
    LastFpsTime=GetTickCount();
    char s[50];
    sprintf(s,"Task4 @ %ffps",fps);
    glutSetWindowTitle(s);
  }
  glutPostRedisplay(); 
}


// �������� ���� ��� ������� ������
void __motion(int x, int y) 
{
  if(lmbpressed)
  {
    m_dy=(-y+m_y)/dt;
    m_dx=(x-m_x)/dt;
  }
  m_x=x;
  m_y=y;
}


// ������� ����
void __mouse(int button, int state, int x, int y)
{
  if(button==GLUT_LEFT_BUTTON && state==GLUT_DOWN)
    lmbpressed=true;
  if(button==GLUT_LEFT_BUTTON && state==GLUT_UP)
    lmbpressed=false;
  __motion(x,y);
}


// ���������
void __draw()
{
  glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_STENCIL_BUFFER_BIT); // �������� �����

  //����������� ������
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();  
  gluLookAt(Camera.x,Camera.y,Camera.z,0,0,0,0,0,1); 

  // ��������� ������� ������
  Meshes[ptype].Rotate(m_dy*dt,0,m_dx*dt);

  // ���������� ���������� ��������� �����
  Light.Set(0.5*sin(t)-0.5*cos(5*t),0.5*cos(t)+0.5*sin(3*t),1.0+0.25*sin(2.5*t)+0.25*cos(1.75*t));

  // ��������� �������� ����� ���� ����
  GLfloat l_position[4]={Light.x,Light.y,Light.z,1};
  glLightfv(GL_LIGHT0,GL_POSITION,l_position);

  // ���������� �� ����� ��������� ����� �����
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_LIGHTING);
  glColor4f(1.0,0.5,0.2,4);
  glPointSize(10);
  glBegin(GL_POINTS);
    glVertex3fv(Light.v);
  glEnd();
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_LIGHTING);
  glEnable(GL_BLEND);

  // ����� �������� �� �������
  glMatrixMode(GL_TEXTURE);
  glLoadIdentity();
  glScalef(4,1,1);
  glMatrixMode(GL_MODELVIEW);

  if(Shadow) // ���� ���� ��������
  {
    // �� ������ ������� ��� (��� ������ � z-�����)
    glBindTexture(GL_TEXTURE_2D,FloorTexture);
    glDisable(GL_DEPTH_TEST);
    glBegin(GL_QUADS);
      glNormal3f(0,0,1);
      for(int i=-10; i<10; i++) for(int j=-10; j<10; j++)
      {
        glTexCoord2f(0.1*(i  ),0.2*(j  ));
        glVertex3i(       i  ,      j  ,-1);
        glTexCoord2f(0.1*(i+1),0.2*(j  ));
        glVertex3i(       i+1,      j  ,-1);
        glTexCoord2f(0.1*(i+1),0.2*(j+1));
        glVertex3i(       i+1,      j+1,-1);  
        glTexCoord2f(0.1*(i  ),0.2*(j+1));
        glVertex3i(       i  ,      j+1,-1);
      }
    glEnd();
    glDisable(GL_TEXTURE_2D);
    glDisable(GL_LIGHTING);  
    glEnable(GL_STENCIL_TEST); // ����� ������ ����, ��� ����� �������� ������ � ������� ���������
    glStencilFunc(GL_NOTEQUAL,1,1);
    glStencilOp(GL_KEEP,GL_KEEP,GL_REPLACE);
    glPushMatrix();
    glColor4f(0,0,0,0.3);
    glTranslatef(0,0,-1);
    GLfloat Matrix[16]={Light.z,0,0,0,0,Light.z,0,0,-Light.x,-Light.y,0,-1,0,0,0,Light.z}; // ������� �������������� �� ���������
    glMultMatrixf(Matrix);
    Meshes[ptype].Draw();
    glPopMatrix();
    glDisable(GL_STENCIL_TEST);
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_LIGHTING);
  }

  if(!Wireframe) // ���������� ���������
  {
    glBindTexture(GL_TEXTURE_2D,MainTexture);
    Meshes[ptype].Sort(Camera);
    Meshes[ptype].Draw();
  }
  else // ��������� ���������
  {
    // ������� ������ ����� ������
    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
    glEnable(GL_POLYGON_OFFSET_FILL); // �������� �������� � z-������
    glPolygonOffset(2,4);
    Meshes[ptype].Draw();
    glDisable(GL_POLYGON_OFFSET_FILL);
    // ����� ������ ������
    glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
    glDisable(GL_LIGHTING);
    glColor4f(0,0,0,1);
    Meshes[ptype].Draw();
    glEnable(GL_LIGHTING);
    glEnable(GL_BLEND);
    glEnable(GL_TEXTURE_2D);
    glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
  }
   
  glutSwapBuffers(); 
}


// ��������� �������� ����
void __reshape(int w, int h)
{
  glViewport(0,0,w,h);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
	gluPerspective(60,(float)w/h,0.5,8);
  wnd_w=w;
  wnd_h=h;
}


// �������������
void __init()
{
  srand(GetTickCount());
  
  glClearColor(0,0,0,0); // ����� ������ ������� ����� ����� �������
  glHint(GL_POINT_SMOOTH_HINT,GL_NICEST); // ���������� ����� �������
  glEnable(GL_BLEND); // ����������������
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA); // ����� ����������������
  glEnable(GL_POINT_SMOOTH); // ����� �������� ������� � �� ������������
  glEnable(GL_TEXTURE_2D); // ��������������� ��������
  glEnable(GL_LIGHT0); // �������� ������� �������� ������
  glEnable(GL_LIGHTING); // �������� ������� ���������
  glEnable(GL_DEPTH_TEST); // �������� z-����
  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE); // ������������ ��������� ��� ������ ������ �������� ��������
  
  // ��������� ���������
  GLfloat m_emissive[4]={0.0, 0.0, 0.0, 0.0};
  GLfloat m_diffuse[4] ={1.0, 1.0, 1.0, 0.7};
  GLfloat m_specular[4]={1.0, 1.0, 1.0, 0.0};
  GLfloat m_ambient[4] ={1.0, 1.0, 1.0, 0.0};
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, m_emissive);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, m_specular);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, m_ambient);
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS,10); 

  // ��������� ��������� �����
	GLfloat l_position[4]={0,0,3,1};
  GLfloat l_diffuse[4] ={0.4,0.2,0.1,1};
  GLfloat l_specular[4]={0.9,0.4,0.1,1};
  GLfloat l_ambient[4] ={0.3,0.3,0.3,1};
  glLightfv(GL_LIGHT0,GL_POSITION,l_position);
  glLightfv(GL_LIGHT0,GL_DIFFUSE,l_diffuse);
  glLightfv(GL_LIGHT0,GL_SPECULAR,l_specular);
  glLightfv(GL_LIGHT0,GL_AMBIENT,l_ambient);
  glLightf(GL_LIGHT0,GL_QUADRATIC_ATTENUATION,0.05); // ������������� ������� � �����������
  glLightf(GL_LIGHT0,GL_SPOT_EXPONENT,1);   

  // ������ ��������
  unsigned char *data = NULL;
  int tex_width, tex_height;
  glPixelStorei(GL_UNPACK_ALIGNMENT,1);
  glGenTextures(1,&MainTexture);
  glGenTextures(1,&FloorTexture);
  data=LoadTrueColorBMPFile("crystal.bmp",&tex_width,&tex_height);
	glBindTexture(GL_TEXTURE_2D,MainTexture);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, tex_width, tex_height, GL_RGB, GL_UNSIGNED_BYTE, data);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  delete[] data;
  data=LoadTrueColorBMPFile("rust.bmp",&tex_width,&tex_height);
	glBindTexture(GL_TEXTURE_2D,FloorTexture);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, tex_width, tex_height, GL_RGB, GL_UNSIGNED_BYTE, data);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  delete[] data;

  // ������� �������
  Meshes[0].CreateAsMebius();
  Meshes[1].CreateAsKlein();
  Meshes[2].CreateAsKlein2();
}


// ������� ������ �� ����������
void __keydown(unsigned char key, int x, int y)
{
  if(key==27)
    exit(0);
  if(key=='1')
    ptype=P_MEBIUS_RIBBON;
  if(key=='2')
    ptype=P_KLEIN_BOTTLE;
  if(key=='3')
    ptype=P_KLEIN_BOTTLE_TYPE_2;
  if(key=='w')
    Wireframe=!Wireframe;
  if(key=='s')
    Shadow=!Shadow;
}



int main(int argc, char** argv)
{
  glutInit(&argc,argv);
  glutInitDisplayMode(GLUT_DOUBLE|GLUT_RGB|GLUT_DEPTH|GLUT_STENCIL);
  glutCreateWindow("Task4");
  glutReshapeFunc(__reshape);
  glutIdleFunc(__idle);
  glutDisplayFunc(__draw);
  glutMouseFunc(__mouse);
  glutMotionFunc(__motion);
  glutPassiveMotionFunc(__motion);
  glutKeyboardFunc(__keydown);
  __init();

  if(MessageBox(NULL,"Fullscreen Mode?", "Start FullScreen?",MB_YESNO|MB_ICONQUESTION)==IDYES)
  {
    DEVMODE dmScreenSettings;        
    memset(&dmScreenSettings,0,sizeof(dmScreenSettings));
    dmScreenSettings.dmSize=sizeof(dmScreenSettings);  
    dmScreenSettings.dmPelsWidth=800;    
    dmScreenSettings.dmPelsHeight=600;    
    dmScreenSettings.dmBitsPerPel=32;      
    dmScreenSettings.dmFields=DM_BITSPERPEL|DM_PELSWIDTH|DM_PELSHEIGHT;
    ChangeDisplaySettings(&dmScreenSettings,CDS_FULLSCREEN);
    glutFullScreen();  
  }
  glutMainLoop();
  return 0;
}
