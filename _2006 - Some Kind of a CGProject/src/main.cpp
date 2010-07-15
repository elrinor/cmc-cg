#include <windows.h>
#include "Bass.h"
#pragma comment(lib, "Bass.lib")
#include <gl/glut.h>
#include <stdio.h>
#include "BmpLoad.h"
#include <math.h>

#define sqr(x) ((x)*(x))

#define PI 3.1415926

// Torus defs
#define MAX 2000
#define R1 1
#define R2 0.25
#define N 32 
#define M 16 

// 3-������ ������
union Vector
{
	struct
  {
		float x,y,z;
	};
	float v[3];
};

// ����� �� 4-� ������
struct Face
{
  Vector v[4],n[4],nowtime[4],m;
};

HSTREAM AudioStream;	
float mData[1024]; // ������ - �������. �������� - ���������
float sum, maxsum=0, bass, maxbass=0, sumk, bassk, red, green, lastred=0, lastgreen=0, objk=0, lastobjk=0;

float fps; // ����� � �������
float nowtime=0, dtime=0; // ����� � ������ ������ ��������� � ����� � ����������� ����� (�������)

GLuint Tex[16]; // �������������� �������

int w_w, w_h; // ������� ����

Vector vCam; // ��������� ������

GLUquadricObj *qSphere; // ������ �����

Face f[MAX]; // �����
int fIndex[MAX]; // ��������� ������
float fDistance[MAX]; // ���������� �� ������ �� ������
int fCount=0; // ���-�� ������

// ================================================================================ //
// Torus
// ================================================================================ //
// ��������� ����
void DoDrawT(float k)
{
  glBindTexture(GL_TEXTURE_2D,Tex[1]);
	glMatrixMode(GL_MODELVIEW);
  glEnable(GL_BLEND);
  glPushMatrix();
  glTranslatef(0,0,1);
  glScalef(2*k,2*k,2*k);
  glBegin(GL_QUADS);
  for(int i=0;i<fCount;i++)
  {
    glNormal3fv(f[fIndex[i]].n[0].v);glTexCoord3fv(f[fIndex[i]].nowtime[0].v);glVertex3fv(f[fIndex[i]].v[0].v);
    glNormal3fv(f[fIndex[i]].n[1].v);glTexCoord3fv(f[fIndex[i]].nowtime[1].v);glVertex3fv(f[fIndex[i]].v[1].v);
    glNormal3fv(f[fIndex[i]].n[2].v);glTexCoord3fv(f[fIndex[i]].nowtime[2].v);glVertex3fv(f[fIndex[i]].v[2].v);
    glNormal3fv(f[fIndex[i]].n[3].v);glTexCoord3fv(f[fIndex[i]].nowtime[3].v);glVertex3fv(f[fIndex[i]].v[3].v);
  }
  glEnd();
  glDisable(GL_BLEND);
  glPopMatrix();
}

// �������� ����� �������
void VertexT(Vector &v, Vector &n, Vector &nowtime, float ia, float ja)
{
	n.x=cos(ia)*cos(ja);
	n.y=sin(ia)*cos(ja);
	n.z=sin(ja);
	v.x=R1*cos(ia)+R2*n.x;
  v.y=R1*sin(ia)+R2*n.y;
  v.z=R2*n.z;
  nowtime.x=ia/(2*PI);
  nowtime.y=ja/(2*PI);
  nowtime.z=0;
}

// ��������� ����
void GenT()
{
  for(int i=0; i<N; i++)
  {
    float ia0=2*i*PI/N;
		float ia1=2*(i+1)*PI/N;
    for(int j=0; j<M; j++)
    {
      float ja0=2*j*PI/M;
			float ja1=2*(j+1)*PI/M;
      // ������ ������� �����
      VertexT(f[fCount].v[0],f[fCount].n[0],f[fCount].nowtime[0],ia0,ja0);
      VertexT(f[fCount].v[1],f[fCount].n[1],f[fCount].nowtime[1],ia1,ja0);
      VertexT(f[fCount].v[2],f[fCount].n[2],f[fCount].nowtime[2],ia1,ja1);
      VertexT(f[fCount].v[3],f[fCount].n[3],f[fCount].nowtime[3],ia0,ja1);
      // ������� ����� �����
      f[fCount].m.x=(f[fCount].v[0].x+f[fCount].v[1].x+f[fCount].v[2].x+f[fCount].v[3].x)/4;
      f[fCount].m.y=(f[fCount].v[0].y+f[fCount].v[1].y+f[fCount].v[2].y+f[fCount].v[3].y)/4;
      f[fCount].m.z=(f[fCount].v[0].z+f[fCount].v[1].z+f[fCount].v[2].z+f[fCount].v[3].z)/4;
      fCount++;
    }
  }
  for(int i=0; i<MAX; i++)
    fIndex[i]=i;
}

// �-� ��������� ��� ����������
int mycmp(const void* p1, const void* p2)
{
  if(fDistance[*((int*)p2)]>fDistance[*((int*)p1)])
    return 1;
  else
    return -1;
}

void DrawT()
{
  // ���������� �� ������ �� ������ ����
  for(int i=0; i<fCount; i++)
    fDistance[i]=sqr(f[i].m.x-vCam.x)+sqr(f[i].m.y-vCam.y)+sqr(f[i].m.z+1-vCam.z);
  // ���������
  qsort(fIndex,fCount,sizeof(int),mycmp);

  // ������� 2 �������������� ����
  glDepthMask(GL_FALSE);
  GLfloat m_diffuse[4]={0.4, 0.4, 0.4, 0.15};
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);
  DoDrawT(lastobjk*1.1);
  DoDrawT(lastobjk*0.9);
  glDepthMask(GL_TRUE);

  // ������� ������� ���
  GLfloat m_diffuse2[4]={0.4, 0.4, 0.4, 0.3};
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse2);
  DoDrawT(objk);
  GLfloat m_diffuse3[4]={0.4, 0.4, 0.4, 0.5};
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse3);
}   

// ================================================================================ //
// Drawing
// ================================================================================ //
void DrawMusic()
{
  glDisable(GL_LIGHTING);
  glDisable(GL_TEXTURE_2D);
  glLineWidth(2);
  glColor3f(red,green,0);
  glBegin(GL_LINE_STRIP);
  // ��� ������ �������
  for(int i=0; i<512; i++)
  {
    float r=0.5+2*mData[i*2];
    float f=i/512.0*PI;
    glVertex3f(r*sin(f),0,r*cos(f));
  }
  glEnd();
  glBegin(GL_LINE_STRIP);
  // ��� 2-�
  for(int i=0; i<512; i++)
  {
    float r=0.5+2*mData[i*2+1];
    float f=-i/512.0*PI;
    glVertex3f(r*sin(f),0,r*cos(f));
  }
  glEnd();
  glEnable(GL_LIGHTING);
  glEnable(GL_TEXTURE_2D);
}

void DrawScene()
{
  glPushMatrix();
  glTranslatef(0,0,2); // ����������� �����
  DrawMusic(); // ��������� �������
  glPopMatrix();

  // ������������� ��������� ���������
  GLfloat l_position[4]={0,0,3,1};
  glLightfv(GL_LIGHT0,GL_POSITION,l_position);
  // ������������� ���� �������� �����
  GLfloat l_diffuse[4] ={red,green,0.0,1};
  GLfloat l_specular[4]={red,green,0.0,1};
  glLightfv(GL_LIGHT0,GL_DIFFUSE,l_diffuse);
  glLightfv(GL_LIGHT0,GL_SPECULAR,l_specular);

  // ������� ��������� �����
  glBindTexture(GL_TEXTURE_2D,Tex[2]);
  for(int i=0; i<8; i++)
  {
    glPushMatrix();
    glTranslatef((1+1.5*objk)*sin(PI*(nowtime+i/4.0)),(1+1.5*objk)*cos(PI*(nowtime+i/4.0)),0.5);
    glRotatef(-180*(nowtime+i/4.0),0,0,1);

    // ������� ������� �������������� �����
    glEnable(GL_BLEND);
    glDepthMask(GL_FALSE);
    gluSphere(qSphere,1.2*objk/2,16,16);
    glDepthMask(GL_TRUE);
    glDisable(GL_BLEND);

    // ����� ����������
    gluSphere(qSphere,objk/2,16,16);
    glPopMatrix();
  }

  DrawT(); // ������ ���
}

// ================================================================================ //
// Main Loop
// ================================================================================ //
void _display(void)
{
  glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_STENCIL_BUFFER_BIT); //������� �������
  BASS_ChannelGetData(AudioStream, mData, BASS_DATA_FFT1024S); // �������� ������ �� BASS
 
  // ������� ��������� ������
  vCam.x=4*sin(nowtime);
  vCam.y=4*cos(nowtime);
  vCam.z=2;

  // ���������� ������
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(vCam.x,vCam.y,vCam.z,0,0,1,0,0,1);
  
  // ��������������� ������ BASS
  for(int i=0; i<1024; i++)
    mData[i]=sqrt(mData[i]*sqrt((float)(i+1)));

  // ����� ���� �������� � ����� �����
  sum=0;
  bass=0;
  maxsum-=5*dtime;  
  maxbass-=0.5*dtime;

  // ������� ����� ��������
  for(int i=0; i<1024; i++)
    sum+=mData[i];

  // ������ maxsum
  if(maxsum<sum)
    maxsum=sum;

  // ������� ����� �����
  for(int i=0; i<64; i++)
    bass+=mData[i];

  //
  if(maxbass<bass)
    maxbass=bass;

  // ������� ������������
  bassk=bass/maxbass;
  sumk=sum/maxsum;

  // ������� ����-� ����������
  lastobjk=objk;
  objk=bassk*bassk;

  // �� ���� ����-�� �������� ������� ������
  if(objk<lastobjk)
    objk=max(objk,lastobjk-dtime);
  if(objk>lastobjk)
    objk=min(objk,lastobjk+30*dtime);

  // ������� ������� � ������� ����������
  red=1.0*sumk;
  green=0.8*bassk;
  if(red<lastred)
    red=max(red, lastred-2*dtime);
  if(green<lastred)
    green=max(green, lastgreen-4*dtime);
  lastred=red;
  lastgreen=green;
  green=min(red*0.9,green); // ����� �� ���� ��������

  // ���������
  glPushMatrix();
  glScalef(1,1,-1);
  DrawScene();
  glPopMatrix();

  // ���
  glEnable(GL_BLEND);
  glBindTexture(GL_TEXTURE_2D,Tex[0]);
  glBegin(GL_QUADS);
    glNormal3f(0,0,-1);
    for(int i=-30; i<30; i++) for(int j=-30; j<30; j++)
    {
      glTexCoord2f((i+1)*0.5,(j  )*0.5);
      glVertex3f((i  ),(j  ),0);
      glTexCoord2f((i+1)*0.5,(j+1)*0.5);
      glVertex3f((i+1),(j  ),0);
      glTexCoord2f((i  )*0.5,(j+1)*0.5);
      glVertex3f((i+1),(j+1),0);
      glTexCoord2f((i  )*0.5,(j  )*0.5);
      glVertex3f((i  ),(j+1),0);
    }
  glEnd();
  glDisable(GL_BLEND);

  // ���������� �����
  DrawScene();

  glutSwapBuffers();
}

// ================================================================================ //
// Initialization
// ================================================================================ //
void Init()
{
  srand(GetTickCount()); // ������������� ���������� ��������� �����
  glClearColor(0,0,0,0); // ����, ������� ������� �����
  glEnable(GL_DEPTH_TEST); // ��������� z-����
  glEnable(GL_TEXTURE_2D); // �������� �����������
  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL); // ����� ��������� - �������
  glShadeModel(GL_SMOOTH); // ������ ��������� - �������������
  glHint(GL_POINT_SMOOTH_HINT,GL_NICEST); // ����� ����������� �����
	glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST); // ������������� ����������� ��� ��������� �������
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); // ����������� ������� ������������
  glEnable(GL_NORMALIZE); // ������� �������������

  // ��������� ��������
  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHTING);

  // ������������ ��������� ������ ��� ����� ������� ��������
  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE,0);

  // ��������� ��������� �����
  GLfloat l_diffuse[4] ={1,1,0.0,1};
  GLfloat l_specular[4]={1,1,0.0,1};
  GLfloat l_ambient[4] ={0.1,0.1,0.1,1};
  glLightfv(GL_LIGHT0,GL_DIFFUSE,l_diffuse);
  glLightfv(GL_LIGHT0,GL_SPECULAR,l_specular);
  glLightfv(GL_LIGHT0,GL_AMBIENT,l_ambient);
  glLightf(GL_LIGHT0,GL_SPOT_EXPONENT, 5);

  GLfloat m_emissive[4]={0.1, 0.1, 0.1, 0.0};
  GLfloat m_diffuse[4] ={0.4, 0.4, 0.4, 0.5};
  GLfloat m_specular[4]={0.4, 0.4, 0.4, 0.0};
  GLfloat m_ambient[4] ={0.2, 0.2, 0.2, 0.0};
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, m_emissive);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, m_specular);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, m_ambient);
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 8); // ��� ������ - ��� ������ ������ ����� �� ��������� ���������

  glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE); // ��������� ������������ ��� ��������� ��������
	

  // �������� ������
  glPixelStorei(GL_UNPACK_ALIGNMENT,1);
	glGenTextures(4,&Tex[0]);
  int w,h;
  unsigned char *pData;
	pData=LoadTrueColorBMPFile("Textures/Floor.bmp",&w,&h);
	glBindTexture(GL_TEXTURE_2D,Tex[0]);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, w, h, GL_RGB, GL_UNSIGNED_BYTE, pData);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  delete[] pData;
  pData=LoadTrueColorBMPFile("Textures/Torus.bmp",&w,&h);
	glBindTexture(GL_TEXTURE_2D,Tex[1]);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, w, h, GL_RGB, GL_UNSIGNED_BYTE, pData);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  delete[] pData;
  pData=LoadTrueColorBMPFile("Textures/Sphere.bmp",&w,&h);
	glBindTexture(GL_TEXTURE_2D,Tex[2]);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, w, h, GL_RGB, GL_UNSIGNED_BYTE, pData);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  delete[] pData;

  GenT(); //������� ���

  // ������� �����
  qSphere=gluNewQuadric();
  gluQuadricTexture(qSphere,GL_TRUE); // �������, ��� �� ����� ���� ����������� ��������
}


// ================================================================================ //
// Glut Routines
// ================================================================================ //
void _idle() 
{
  BASS_Update(); 
  static float lasttime=glutGet(GLUT_ELAPSED_TIME)/1000.0; // ����� ����������� �����
  static int frames=0; // ���-�� ������ � ����������� �������� fps
  static float fpstime=0; // ����� � ����������� �������� fps
  nowtime=glutGet(GLUT_ELAPSED_TIME)/1000.0;
  dtime=nowtime-lasttime;
  lasttime=nowtime;
  frames++;
  glutPostRedisplay(); // ���������

  // ����� fps � ��������� ����
  if(nowtime-fpstime>0.5)
  {
    float fps=(float)frames/(nowtime-fpstime);
    fpstime=nowtime;
    frames=0;
    char str[40];
    sprintf(str,"Music Visualiser. %lffps.",fps);
    glutSetWindowTitle(str);
  }
}

void _reshape(int w, int h)
{
  glViewport(0,0,w,h); // ��������� �������� ���� ������
  if(h > 0) 
  {
		w_w=w;
    w_h=h;
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(90,(float)w_w/w_h,0.1,20); // ���� ������, ��������� ������/������, �������� � ������ ���������� ���������
	}
}

void GLUTCALLBACK _keypress(unsigned char Key, int x, int y)
{
  if(Key==27) // esc
    exit(0);
}

// ================================================================================ //
// Load Music File
// ================================================================================ //
char FileName[256];       
OPENFILENAME musicFile;   
bool GetOpenFile()
{
	musicFile.lStructSize       = sizeof(OPENFILENAME);
	musicFile.hwndOwner         = NULL;
	musicFile.lpstrFilter       = "Music Files (*.mp3 *.wav)\0*.mp3;*.wav\0All Files (*.*)\0*.*\0\0"; 
	musicFile.lpstrCustomFilter = NULL;
	musicFile.nFilterIndex      = 1;
	musicFile.lpstrFile         = NULL;
	musicFile.lpstrFile         = FileName;
	musicFile.nMaxFile          = sizeof(FileName);
	musicFile.lpstrFileTitle    = NULL;
	musicFile.lpstrInitialDir   = NULL;
	musicFile.lpstrTitle        = "Select Music To Visualize";  
	musicFile.Flags             = OFN_HIDEREADONLY | OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST;
	musicFile.lpstrDefExt       = "*.MP3";  
	*FileName = '\0';
	if (!GetOpenFileName(&musicFile))
		return false; 
	return true;
}

// ������ ������
bool StartSong()
{
	AudioStream=BASS_StreamCreateFile(false,musicFile.lpstrFile,0,0,0); 
	if(!AudioStream)
	{	 
		MessageBox(NULL,"Couldn'nowtime load requested AudioStream file.","ERROR",MB_OK);
		return false;
	}
	BASS_Start();
	BASS_ChannelSetPosition(AudioStream,(QWORD)MAKELONG(0,0));
	BASS_StreamPlay(AudioStream,0,0);
	return true;
}


// ================================================================================ //
// Main
// ================================================================================ //
int main(int argc, char** argv)
{
  BASS_Init(-1, 44100, BASS_DEVICE_NOTHREAD, 0); //������������� BASS
  glutInit(&argc, argv); // �������������� GLUT
  glutInitDisplayMode(GLUT_DOUBLE|GLUT_DEPTH|GLUT_RGB); //������� ����������� + ����� ������� + ����� �����
  glutCreateWindow("Music Visualiser"); // ������� ����
  
  // �����������
  glutDisplayFunc(_display); // �-� ���������
  glutReshapeFunc(_reshape); // ���������� ��������� �������� ����
  glutIdleFunc(_idle); // �������� ����
  glutKeyboardFunc(_keypress); // ���������� ����������
  glutSetCursor(GLUT_CURSOR_NONE); // ������ ���������
  Init(); // ��������� ������� � �.�
  if(!GetOpenFile())
    exit(0);
  if(MessageBox(NULL,"Fullscreen Mode?", "Start FullScreen?",MB_YESNO|MB_ICONQUESTION)==IDYES)
  {
    DEVMODE s;				
	  memset(&s,0,sizeof(s));
	  s.dmSize=sizeof(s);	
  	s.dmPelsWidth=640;		
	  s.dmPelsHeight=480;		
	  s.dmBitsPerPel=32;			
	  s.dmFields=DM_PELSWIDTH|DM_PELSHEIGHT|DM_BITSPERPEL;
	  ChangeDisplaySettings(&s,CDS_FULLSCREEN);
    glutFullScreen();	
  }
  if(!StartSong())
    exit(0);
  glutMainLoop(); // ��������� �������� ����
  ChangeDisplaySettings(NULL,0);	
  return 0;
}

