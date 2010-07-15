#include <windows.h>
#include <gl\glut.h>

// ��� ����� �������������� �����? (40 ��� � �������)
#define FRAMETIME 1.0/40.0

bool pause; 

double wind; // �������� �����

int windoww, windowh; // ������� ����

// ���������� ��������� �������� ����
void reshapefunc(int neww, int newh)
{
  glViewport(0,0,neww,newh);
  windoww=neww;
  windowh=newh;
}

// ��������� �������
typedef struct{
  double x,y; // ���������
  double speedx, speedy; // ��������
  double blue; // ����� ���������� �����
  double razmer; // ������
  int lifetime; // ������� � ������� �����
  int maxlife; // � ������� � ��� ���� ����� ��� ��������
  bool draw; // ������������ �� �������?
  bool clicked; // ���� �� ������� ��������� � ���������� ����� �����?
} SnowFlake;

SnowFlake snow[3000];

// �����
void drawfunc()
{
  if(!pause) // ���� �� ����� �� ������� �������
  {
  int created=0;
  for(int i=0; i<3000; i++)
  {
    if(!snow[i].draw || snow[i].clicked) // ���� ������� ������ ��� ��������
    {
      if(!snow[i].clicked) // ���� ������� ���� ��������� �������� ���� �� � ��� �� ���� ������������� ��������� ����������
      {
        if(created>10)
          continue;
        created++;
        snow[i].x=(double)rand()/RAND_MAX;
        snow[i].y=1;
			}
			// ��� ��������� ���������
      snow[i].speedy=-0.01*rand()/RAND_MAX;
      snow[i].speedx=min(snow[i].speedy*0.2,0.002)*rand()/RAND_MAX;
      snow[i].blue=0.5+0.5*rand()/RAND_MAX;
      snow[i].lifetime=800*rand()/RAND_MAX;
      snow[i].maxlife=snow[i].lifetime;
      snow[i].razmer=12.0*rand()/RAND_MAX;
      snow[i].draw=true;
      snow[i].clicked=false;
    }
    else // ���� ������� ���� �� �������� �� ���������
    {
      if(snow[i].y>0)
      {
        snow[i].y+=snow[i].speedy;
        snow[i].x+=snow[i].speedx*(-2.0+4.0*rand()/RAND_MAX)+wind;
      }
      if(snow[i].y<0) // ���� ������� �������� �� ���� - �������������
      {
        snow[i].y=0;
        snow[i].speedx=0;
        snow[i].speedy=0;
      }
      snow[i].lifetime--;
      if(snow[i].lifetime<=0) // ���� ����� ��������� - �������
        snow[i].draw=false;
    }
  }
  }
	
	// ������������ ������ � ������ �����
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(-1,0,0,1,0.1,100);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(0,0,0,0,0,1,0,1,0);
  
	// � ������������ �������
  glClear(GL_COLOR_BUFFER_BIT);
  for(int i=0; i<3000; i++)
  {
    if(snow[i].draw)
    {
    glPointSize(1+snow[i].razmer*snow[i].lifetime/snow[i].maxlife);
    glBegin(GL_POINTS);
    glColor3f(snow[i].blue/2,snow[i].blue/2,snow[i].blue);
    glVertex3d(snow[i].x,snow[i].y,1);
    glEnd();
    }
  }
  glutSwapBuffers();
}

// ���������� ������� ����������
void keyfunc(unsigned char key, int x, int y)
{
  if(key==' ')
    pause=!pause;
  else if(key==27)
    exit(0);
}

// ���������� ������� ����
void clickfunc(int button, int state, int x, int y)
{
  int i=3000*rand()/RAND_MAX;
  snow[i].clicked=true;
  snow[i].x=1-(double)x/windoww;
  snow[i].y=1-(double)y/windowh;
}

// idle 
void idlefunc()
{
  static double lastFrameTime=0; // ����� ����� ��� ������� ���������� ����
  double currentTime=timeGetTime()*0.001; // ������� �����
  if(currentTime-lastFrameTime>=FRAMETIME)
  {
    lastFrameTime=currentTime;
    glutPostRedisplay();
  }
}

int main(int argc, char** argv)
{
	// ������������� glut
  glutInit(&argc,argv);
  glutInitDisplayMode(GLUT_DOUBLE|GLUT_RGB);

	// ������� ����
  glutInitWindowSize(700,700);
  glutCreateWindow("Particles");
  
	// ��������� ����������� �������
  glutReshapeFunc(reshapefunc);
  glutDisplayFunc(drawfunc);
  glutKeyboardFunc(keyfunc);
  glutMouseFunc(clickfunc);
  glutIdleFunc(idlefunc);

	// ��������� gl
  glClearColor(0,0,0,0); // ���� ������� ������� �����
  glHint(GL_POINT_SMOOTH_HINT,GL_NICEST); // ���������� ����� ������ �� ��������� �������
  glEnable(GL_POINT_SMOOTH); // �������� ����������� ����� (�������� ��������)
  glEnable(GL_BLEND); // �������� ������������
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA); // ����������� �-� ������������

	// ������� ���������� �� �������
  for(int i=0; i<3000; i++)
  {
    snow[i].draw=false;
    snow[i].clicked=false;
  }

	// ������������� ���������� ����. �����
  srand(timeGetTime());
  
	// ���� �����
  wind=0.004-0.008*rand()/RAND_MAX;

	// �������!
  glutMainLoop();
  return 0;
}
