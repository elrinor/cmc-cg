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

int ptype=P_MEBIUS_RIBBON; //текущий тип объекта

int wnd_w,wnd_h; // размеры окна программы
float t=0, dt; // время с начала работы программы / с предыдущего кадра (сек)
int m_x,m_y,m_dx=0,m_dy=0;  // положение мыши и ее смещение с предыдущего кадра
bool lmbpressed, rmbpressed; // нажата ли левая/правая кнопка мыши
bool Wireframe=false; // показывать каркас?
bool Shadow=true; // показывать тень?

GLuint MainTexture; // текстура объекта
GLuint FloorTexture; // текстура пола

Mesh Meshes[3]; // объекты

CVector Camera(0,4,0), Light; // координаты камеры и источника света


// здесь считаем фпс, t, dt и вызываем перерисовку
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


// движение мыши при зажатой кнопке
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


// нажание мыши
void __mouse(int button, int state, int x, int y)
{
  if(button==GLUT_LEFT_BUTTON && state==GLUT_DOWN)
    lmbpressed=true;
  if(button==GLUT_LEFT_BUTTON && state==GLUT_UP)
    lmbpressed=false;
  __motion(x,y);
}


// отрисовка
void __draw()
{
  glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_STENCIL_BUFFER_BIT); // очистить экран

  //переместить камеру
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();  
  gluLookAt(Camera.x,Camera.y,Camera.z,0,0,0,0,0,1); 

  // повернуть текущий объект
  Meshes[ptype].Rotate(m_dy*dt,0,m_dx*dt);

  // подсчитать координаты источника света
  Light.Set(0.5*sin(t)-0.5*cos(5*t),0.5*cos(t)+0.5*sin(3*t),1.0+0.25*sin(2.5*t)+0.25*cos(1.75*t));

  // поставить источник света куда надо
  GLfloat l_position[4]={Light.x,Light.y,Light.z,1};
  glLightfv(GL_LIGHT0,GL_POSITION,l_position);

  // нарисовать на месте источника света точку
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

  // сжать текстуру на объекте
  glMatrixMode(GL_TEXTURE);
  glLoadIdentity();
  glScalef(4,1,1);
  glMatrixMode(GL_MODELVIEW);

  if(Shadow) // если тень рисуется
  {
    // то рисуем сначала пол (без записи в z-буфер)
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
    glEnable(GL_STENCIL_TEST); // потом рисуем тень, для этого включаем работу с буфером трафарета
    glStencilFunc(GL_NOTEQUAL,1,1);
    glStencilOp(GL_KEEP,GL_KEEP,GL_REPLACE);
    glPushMatrix();
    glColor4f(0,0,0,0.3);
    glTranslatef(0,0,-1);
    GLfloat Matrix[16]={Light.z,0,0,0,0,Light.z,0,0,-Light.x,-Light.y,0,-1,0,0,0,Light.z}; // матрица проектирования на плоскость
    glMultMatrixf(Matrix);
    Meshes[ptype].Draw();
    glPopMatrix();
    glDisable(GL_STENCIL_TEST);
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_LIGHTING);
  }

  if(!Wireframe) // нормальная отрисовка
  {
    glBindTexture(GL_TEXTURE_2D,MainTexture);
    Meshes[ptype].Sort(Camera);
    Meshes[ptype].Draw();
  }
  else // каркасная отрисовка
  {
    // сначала рисуем белый объект
    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
    glEnable(GL_POLYGON_OFFSET_FILL); // включить смещение в z-буфере
    glPolygonOffset(2,4);
    Meshes[ptype].Draw();
    glDisable(GL_POLYGON_OFFSET_FILL);
    // потом черных каркас
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


// изменение размеров окна
void __reshape(int w, int h)
{
  glViewport(0,0,w,h);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
	gluPerspective(60,(float)w/h,0.5,8);
  wnd_w=w;
  wnd_h=h;
}


// инициализация
void __init()
{
  srand(GetTickCount());
  
  glClearColor(0,0,0,0); // каким цветом очищать экран между кадрами
  glHint(GL_POINT_SMOOTH_HINT,GL_NICEST); // сглаживать точки красиво
  glEnable(GL_BLEND); // полупрозрачность
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA); // режим полупрозрачности
  glEnable(GL_POINT_SMOOTH); // точки выводить кругами а не квадратиками
  glEnable(GL_TEXTURE_2D); // текстурирование включить
  glEnable(GL_LIGHT0); // включить нулевой источник всвета
  glEnable(GL_LIGHTING); // включить подсчет освещения
  glEnable(GL_DEPTH_TEST); // включить z-тест
  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE); // просчитывать освещение для разных сторон полигона отдельно
  
  // настройки материала
  GLfloat m_emissive[4]={0.0, 0.0, 0.0, 0.0};
  GLfloat m_diffuse[4] ={1.0, 1.0, 1.0, 0.7};
  GLfloat m_specular[4]={1.0, 1.0, 1.0, 0.0};
  GLfloat m_ambient[4] ={1.0, 1.0, 1.0, 0.0};
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, m_emissive);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, m_specular);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, m_ambient);
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS,10); 

  // настройки источника света
	GLfloat l_position[4]={0,0,3,1};
  GLfloat l_diffuse[4] ={0.4,0.2,0.1,1};
  GLfloat l_specular[4]={0.9,0.4,0.1,1};
  GLfloat l_ambient[4] ={0.3,0.3,0.3,1};
  glLightfv(GL_LIGHT0,GL_POSITION,l_position);
  glLightfv(GL_LIGHT0,GL_DIFFUSE,l_diffuse);
  glLightfv(GL_LIGHT0,GL_SPECULAR,l_specular);
  glLightfv(GL_LIGHT0,GL_AMBIENT,l_ambient);
  glLightf(GL_LIGHT0,GL_QUADRATIC_ATTENUATION,0.05); // интенсивность убывает с расстоянием
  glLightf(GL_LIGHT0,GL_SPOT_EXPONENT,1);   

  // грузим текстуры
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

  // создаем объекты
  Meshes[0].CreateAsMebius();
  Meshes[1].CreateAsKlein();
  Meshes[2].CreateAsKlein2();
}


// нажатие кнопки на клавиатуре
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
