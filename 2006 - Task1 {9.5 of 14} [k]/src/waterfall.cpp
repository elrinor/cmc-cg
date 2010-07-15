#include <gl\glut.h>
#include <windows.h>

// ������������ ���-�� ������
#define N 5000
// �������� �������� ������ � �������
#define MAXBIRTHS 1000 

float coords[2][N]; // ���������� ������
float hvosts[2][N]; // � �� �������
float colors[3][N]; // �� �����
float speeds[2][N]; // � �������� � �������
float sizes[N]; // �������
bool alive[N]; // ������� ���� ��� �� ���?
float life[N]; // ������� ������ ������� �������� �� �����������

int w,h; // ������� ����

bool ongoing=true; // ���� �� ��������

// ������� �������
void Birth(int i)
{
  coords[0][i]=10;
  coords[1][i]=10;
  hvosts[0][i]=10;
  hvosts[1][i]=10;
  speeds[0][i]=-3.0-3.0*rand()/RAND_MAX;
  speeds[1][i]=-5.0*rand()/RAND_MAX;
  colors[2][i]=(float)rand()/RAND_MAX;
  colors[0][i]=colors[2][i]/4;
  colors[1][i]=colors[2][i]/4;
  sizes[i]=10.0*rand()/RAND_MAX;
  life[i]=3.0+6.0*rand()/RAND_MAX;
  alive[i]=true;
}

// ���������� � ����� ������� ������
void OnDraw()
{
  // ������� ������� ������� ������ � ����. �����
  static int LastTime=GetTickCount();
  float t;
  t=(GetTickCount()-LastTime)/1000.0;
  LastTime=GetTickCount();
  t=min(t,0.1);

  // ���������� �������
  if(ongoing)
  {
  int born=0; // ���-�� ����������� �� ������ ����� ������
  for(int i=0; i<N; i++)
  {
    if(!alive[i]) // ���� ������� ������
    {
      if(born<MAXBIRTHS*t) // � ���� ����� �� ���������
      {
        born++;
        Birth(i); //�� ����������
      }
    }
    else // � ���� ������� ���� �� ������ �� ���������
    {
      hvosts[0][i]=coords[0][i];
      hvosts[1][i]=coords[1][i];
      coords[0][i]+=t*(speeds[0][i]-0.2+0.4*rand()/RAND_MAX);
      coords[1][i]+=t*speeds[1][i];
      speeds[1][i]+=-t*(5+5*rand()/RAND_MAX);
      life[i]-=t;
      if(coords[1][i]<-10) // ���� ��� �������� �� ���� ������
      {
        coords[1][i]=-10;
        if(rand()<RAND_MAX/4) // ��� ��� ��������������
        {
          speeds[1][i]=-speeds[1][i]/2*1.0*rand()/RAND_MAX;
          speeds[0][i]=speeds[0][i]*(1-2.0*rand()/RAND_MAX);
          sizes[i]=1+sizes[i]/2;
        }
        else // ��� ������ �����/������
        {
          speeds[1][i]=0;
          speeds[0][i]=5*(2*(i%2)-1);
        }
      }
      if(life[i]<0) // ���� ����� ��������� - �������
        alive[i]=false;
    }
  }
  }

  // ����� ������
  glClear(GL_COLOR_BUFFER_BIT);
  for(int i=0; i<N; i++)
  {
    if(alive[i])
    {
      glColor3f(colors[0][i],colors[1][i],colors[2][i]);
      glPointSize(sizes[i]);
      glBegin(GL_POINTS);
      glVertex3f(coords[0][i],coords[1][i],0);
      glVertex3f(hvosts[0][i],hvosts[1][i],0);
      glEnd();
    }
  }
  glutSwapBuffers(); 
}

void OnIdle()
{
  glutPostRedisplay();
}

// ���������� ��������� �������� ����
void OnReshape(int width, int height)
{
  glViewport(0,0,width,height);
  w=width;
  h=height;
}

// ������� ������ �� �����
void OnKeyDown(unsigned char key, int x, int y)
{
  if(key==27)
    exit(0);
  if(key==' ')
    ongoing=!ongoing;
}

// ������� ������ �� ����
void OnClick(int button, int state, int x, int y)
{
  int i=N*rand()/RAND_MAX;
  Birth(i);
  coords[0][i]=-10*(((float)x/w)*2-1);
  coords[1][i]=-10*(((float)y/h)*2-1);
  hvosts[0][i]=coords[0][i];
  hvosts[1][i]=coords[1][i];
}

int main(int argc, char** argv)
{
  srand(GetTickCount()); // �������������� ��������� ��������� �����

  // ������������� OpenGL
  glutInit(&argc,argv);
  glutInitDisplayMode(GLUT_DOUBLE|GLUT_RGB);
  glutCreateWindow("Waterfall");
  glutReshapeFunc(OnReshape);
  glutIdleFunc(OnIdle);
  glutDisplayFunc(OnDraw);
  glutMouseFunc(OnClick);
  glutKeyboardFunc(OnKeyDown);

  // � �������������?
  if(MessageBox(NULL,"Fullscreen?", "FullScreen?",MB_ICONQUESTION|MB_YESNO)==IDYES)
  {
		glutGameModeString("800x600:32");
    glutFullScreen();  
  }

  // ������������� ��������� OpenGL
  glClearColor(0,0,0,0);
  glHint(GL_POINT_SMOOTH_HINT,GL_NICEST);
  glEnable(GL_POINT_SMOOTH); // �������� ����� ����������
  glEnable(GL_BLEND); // ��������� ������������
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA); // ����������� ������ �������� ������ ��� ������������
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(-10,10,-10,10,0.5,1.5);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(0,0,-1,0,0,0,0,1,0);

  memset(&alive,0,sizeof(alive)); //��� ������� ���������� ������

  glutMainLoop(); // ��������� ���������
  return 0;
}
