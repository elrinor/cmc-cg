// To compile this project please add /entry:mainCRTStartup to linker options
#include <gl\glut.h>
#include <windows.h>
#include <math.h>
#include <stdio.h>

// хвост
typedef struct{
  union{ 
    struct{
    float x,y;
    };
    float v[2];
  };
}tail_t;

// частица
typedef struct{
  union{ // XYZ
    struct{
    float x,y;
    };
    float v[2];
  };
  union{ // Speed
    struct{
    float dx,dy;
    };
    float dv[2];
  };
  union{ // Color
    struct{
    float r,g,b,a;
    };
    float color[4];
  };
  union{ // dColor
    struct{
    float dr,dg,db,da;
    };
    float dcolor[4];
  };
  bool RandDx;
  bool RandDy;
  bool frozen;
  float life;
  float dlife;
  float size;
  float dsize;
  tail_t tail[30];
  int tailLength;
  int tailEndIndex;
  int tailBeginIndex;
} particle_t;

// Параметры системы частиц
#define PARTICLES_MAX 2000
#define P_FIRE 0
#define P_SNOW 1
#define P_TYPES_COUNT 2
const int MAX_REBIRTHS[2]={PARTICLES_MAX*0.7,PARTICLES_MAX/3}; // магические числа - за секунду можно добавить не более MAX_REBIRTHS[ptype] частиц
particle_t p[PARTICLES_MAX];
int pcount; // всего частиц в системе
int preborncount; // сколько частиц мы добавили на данном такте
float windx=0,windy=0; // скорость ветра
int ptype=P_FIRE; // текущий тип частиц

// Scene & Window state parameters
int wnd_w,wnd_h; // размеры окна
bool animation=true; // анимировать?
float t=0, dt; // время с начала работы и с предыдущего кадра соответственно
bool lmbpressed=false, rmbpressed=false; // состояние кнопок мыши
int m_x,m_y; // положение указателя мыши


// Рисуем частицы
void drawParticles()
{
  for(int i=0; i<pcount;i++)
  {
    glPointSize(p[i].size*wnd_w/640.0); 
    glColor4fv(p[i].color);
    for(int j=0;j<p[i].tailLength;j++)
    {
      glBegin(GL_POINTS);
      glVertex2fv(p[i].tail[j].v);
      glEnd();
    }
  }
}

// Убить данную частицу
void killParticle(int i)
{
  p[i].life=-1;
  p[i].tailLength=1;
  p[i].tailBeginIndex=0;
  p[i].tailEndIndex=0;
  p[i].tail[0].x=200; //перемещаем ее подальше чтоб никто не видел
}

// создать частицу
void createParticle(int i)
{
  if(ptype==P_FIRE)
  {
    p[i].r=1;
    p[i].g=0;
    p[i].b=0;
    p[i].a=1;
    p[i].dr=-0.5-1.0*rand()/RAND_MAX;
    p[i].dg=0;
    p[i].db=0;
    p[i].da=p[i].dr;
    p[i].x=-1+2.0*rand()/RAND_MAX;
    p[i].y=-1;
    p[i].dx=-0.5+1.0*rand()/RAND_MAX;
    p[i].dy=0.5*rand()/RAND_MAX;
    p[i].life=1;
    p[i].dlife=p[i].dr;
    p[i].size=3+4.0*rand()/RAND_MAX;
    p[i].dsize=-2.0*rand()/RAND_MAX;
    p[i].RandDx=true;
    p[i].RandDy=false;
    p[i].frozen=false;
    p[i].tailLength=1+28.0*rand()/RAND_MAX;
    p[i].tailEndIndex=0;
    for(int j=0; j<p[i].tailLength; j++)
    {
      p[i].tail[j].x=p[i].x;
      p[i].tail[j].y=p[i].y;
    }
  }
  if(ptype==P_SNOW)
  {
    p[i].b=0.5+0.5*rand()/RAND_MAX;
    p[i].r=p[i].b/2;
    p[i].g=p[i].b/2;
    p[i].a=1;
    p[i].dr=0;
    p[i].dg=0;
    p[i].db=0;
    p[i].da=0;
    p[i].x=-1+2.0*rand()/RAND_MAX;
    p[i].y=1;
    p[i].dx=-0.5+1.0*rand()/RAND_MAX;
    p[i].dy=-abs(p[i].dx)-0.5*rand()/RAND_MAX;
    p[i].life=2;
    p[i].dlife=p[i].dy*(0.66+0.66*rand()/RAND_MAX);
    p[i].size=3+4.0*rand()/RAND_MAX;
    p[i].dsize=-1.0*rand()/RAND_MAX;
    p[i].RandDx=true;
    p[i].RandDy=false;
    p[i].frozen=false;
    p[i].tailLength=1;
    p[i].tailEndIndex=0;
    p[i].tail[0].x=p[i].x;
    p[i].tail[0].y=p[i].y;
  }
}

// Пересчитать параметры системы частиц
void updateParticles()
{
  preborncount=0;
  for(int i=0; i<pcount; i++)
  {
    p[i].life+=p[i].dlife*dt;
    if(p[i].life<0)
    {
      if(p[i].tailEndIndex==p[i].tailBeginIndex) // если в хвосте только одна частица, то
      {
        killParticle(i); // убить
        if(preborncount<MAX_REBIRTHS[ptype]*dt) // и если можно, то
        {
          createParticle(i); // отреспавнить
          preborncount++;
        }
      }
      else
      {
        p[i].tail[p[i].tailBeginIndex].x=200; // иначе укоротить хвост
        p[i].tailBeginIndex=(p[i].tailBeginIndex+1)%p[i].tailLength;
      }
    }
    else
    {
      // ну а если частица жива то просто меняем ее параметры по известным законам
      if(!p[i].frozen)
      {
        if(p[i].RandDx)
          p[i].x+=(p[i].dx*(-1+2.0*rand()/RAND_MAX)+windx)*dt;
        else
          p[i].x+=(p[i].dx+windx)*dt;
        if(p[i].RandDy)
          p[i].y+=(p[i].dy*(-1+2.0*rand()/RAND_MAX)+windy)*dt;
        else
          p[i].y+=(p[i].dy+windy)*dt;
      }
      p[i].size+=p[i].dsize*dt;
      if(p[i].size<1)
        p[i].size=1;
      p[i].r+=p[i].dr*dt;
      p[i].g+=p[i].dg*dt;
      p[i].b+=p[i].db*dt;
      p[i].a+=p[i].da*dt;
      if(p[i].r<0)
        p[i].r=0;
      if(p[i].g<0)
        p[i].g=0;
      if(p[i].b<0)
        p[i].b=0;
      if(p[i].a<0)
        p[i].a=0;
      p[i].tailEndIndex=(p[i].tailEndIndex+1)%p[i].tailLength;
      p[i].tailBeginIndex=(p[i].tailEndIndex+1)%p[i].tailLength;
      p[i].tail[p[i].tailEndIndex].x=p[i].x;
      p[i].tail[p[i].tailEndIndex].y=p[i].y;
    }
  }
}

// функция простоя
void __idle()
{
  static int LastTC=GetTickCount(); // TickCount во время предыдущего кадра
  static int LastFpsTime=0; // TickCount во время предыдущего вывода FPS
  static int FramesPassed=0; // сколько кадров прошло со времени вывода FPS
  dt=(double)(GetTickCount()-LastTC)/1000; 
  t+=dt; 
  LastTC=GetTickCount();
  FramesPassed++;
  if(GetTickCount()-LastFpsTime>1000) // если пора выводить FPS
  {
    double fps;
    fps=1000*FramesPassed/(double)(GetTickCount()-LastFpsTime);
    FramesPassed=0;
    LastFpsTime=GetTickCount();
    char s[50];
    sprintf(s,"Particle System @ %ffps",fps);
    glutSetWindowTitle(s);
  }
  glutPostRedisplay(); // отрисовываем кадр
}

// обработчик перемещений мыши
void __motion(int x, int y) 
{
  if(rmbpressed) // если прав. кнопка нажата
  {
    windx+=-1.0*(x-m_x)/wnd_w; //меняем ветер
    windy+=-1.0*(y-m_y)/wnd_h;
  }
  if(lmbpressed) // если левая
  {
    // то создаем частицу 
    int i=(PARTICLES_MAX-1)*1.0*rand()/RAND_MAX;
    createParticle(i);
    p[i].x=-(float)x/(wnd_w/2)+1;
    p[i].y=-(float)y/(wnd_h/2)+1;
  }
  m_x=x;
  m_y=y;
}

// обработчик нажатий мыши
void __mouse(int button, int state, int x, int y)
{
  if(button==GLUT_LEFT_BUTTON && state==GLUT_DOWN)
    lmbpressed=true;
  if(button==GLUT_LEFT_BUTTON && state==GLUT_UP)
    lmbpressed=false;
  if(button==GLUT_RIGHT_BUTTON && state==GLUT_DOWN)
    rmbpressed=true;
  if(button==GLUT_RIGHT_BUTTON && state==GLUT_UP)
    rmbpressed=false;
  m_x=x;
  m_y=y;
  __motion(x,y);
}

// отрисовка кадра
void __draw()
{
  // устанавливаем правильную проекцию (ортогональную)
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(-1,1,-1,1,0.1,2);

  // и правильно перемещаем систему координат (чтобы то что нам надо в камеру попадало)
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();  
  gluLookAt(0,0,-1,0,0,0,0,1,0);
  
  glClear(GL_COLOR_BUFFER_BIT); // очищаем экран
  if(animation) 
  {
    updateParticles();
    if(ptype==P_SNOW) // отдельная обработка для снега, чтобы снежинки кучковались внизу экрана
      for(int i=0; i<pcount; i++)
        if(p[i].y<-1)
        {
          p[i].y=-1;
          p[i].frozen=true;
        }
  }
  __motion(m_x,m_y); // чтобы даже если мышь не двигается, обработчик вызывался
  drawParticles();
  glutSwapBuffers(); 
}

// Обработчик изменения размеров окна
void __reshape(int w, int h)
{
  glViewport(0,0,w,h);
  wnd_w=w;
  wnd_h=h;
}

// Инициализация
void __init()
{
  srand(GetTickCount()); // генератор случайных чисел
  
  //GL Init
  glClearColor(0,0,0,0); // очищать черным цветом
  glHint(GL_POINT_SMOOTH_HINT,GL_NICEST); // точки красивыми рисовать
  glEnable(GL_BLEND); // использовать alpha-компоненту цвета 
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA); // использовать ее во всеми привычном виде
  glEnable(GL_POINT_SMOOTH); // точечки рисовать кружочками
  
  //Particle Init
  pcount=PARTICLES_MAX;
  for(int i=0; i<pcount; i++)
    killParticle(i);
}

// Обработчик нажатий клавы
void __keydown(unsigned char key, int x, int y)
{
  if(key==' ')
    animation=!animation;
  if(key==27)
    exit(0);
  if(key=='\t')
    ptype=(ptype+1)%P_TYPES_COUNT; // сменить тип частиц
  if(key=='c' || key=='C')
  {
    for(int i=0;i<pcount;i++)
      killParticle(i);
  }
}

int main(int argc, char** argv)
{
  glutInit(&argc,argv);
  glutInitDisplayMode(GLUT_DOUBLE|GLUT_RGB);
  glutCreateWindow("Particle System");
  glutReshapeFunc(__reshape);
  glutIdleFunc(__idle);
  glutDisplayFunc(__draw);
  glutMouseFunc(__mouse);
  glutMotionFunc(__motion);
  glutKeyboardFunc(__keydown);
  __init();

  if(MessageBox(NULL,"Fullscreen Mode?", "Start FullScreen?",MB_YESNO|MB_ICONQUESTION)==IDYES)
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
  }
  glutMainLoop();
  return 0;
}
