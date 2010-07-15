#include <windows.h>
#include <gl\glut.h>

// как часто перерисовывать экран? (40 раз в секунду)
#define FRAMETIME 1.0/40.0

bool pause; 

double wind; // скорость ветра

int windoww, windowh; // размеры окна

// обработчик изменений размеров окна
void reshapefunc(int neww, int newh)
{
  glViewport(0,0,neww,newh);
  windoww=neww;
  windowh=newh;
}

// структура частицы
typedef struct{
  double x,y; // положение
  double speedx, speedy; // скорость
  double blue; // синяя компонента цвета
  double razmer; // размер
  int lifetime; // сколько у частицы жизни
  int maxlife; // и сколько у нее было жизни при рождении
  bool draw; // отрисовывать ли частицу?
  bool clicked; // была ли частица порождена в результате клика мышки?
} SnowFlake;

SnowFlake snow[3000];

// вывод
void drawfunc()
{
  if(!pause) // если не пауза то двигаем частицы
  {
  int created=0;
  for(int i=0; i<3000; i++)
  {
    if(!snow[i].draw || snow[i].clicked) // если частица мертва или кликнута
    {
      if(!snow[i].clicked) // если частица была порождена нажатием мыши то у нее не надо устанавливать начальные координаты
      {
        if(created>10)
          continue;
        created++;
        snow[i].x=(double)rand()/RAND_MAX;
        snow[i].y=1;
			}
			// все остальные параметры
      snow[i].speedy=-0.01*rand()/RAND_MAX;
      snow[i].speedx=min(snow[i].speedy*0.2,0.002)*rand()/RAND_MAX;
      snow[i].blue=0.5+0.5*rand()/RAND_MAX;
      snow[i].lifetime=800*rand()/RAND_MAX;
      snow[i].maxlife=snow[i].lifetime;
      snow[i].razmer=12.0*rand()/RAND_MAX;
      snow[i].draw=true;
      snow[i].clicked=false;
    }
    else // если частица жива то изменяем ее положение
    {
      if(snow[i].y>0)
      {
        snow[i].y+=snow[i].speedy;
        snow[i].x+=snow[i].speedx*(-2.0+4.0*rand()/RAND_MAX)+wind;
      }
      if(snow[i].y<0) // если частица долетела до низа - останавливаем
      {
        snow[i].y=0;
        snow[i].speedx=0;
        snow[i].speedy=0;
      }
      snow[i].lifetime--;
      if(snow[i].lifetime<=0) // если жизни кончились - убиваем
        snow[i].draw=false;
    }
  }
  }
	
	// переставляем камеру в нужное место
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(-1,0,0,1,0.1,100);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(0,0,0,0,0,1,0,1,0);
  
	// и отрисовываем частицы
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

// обработчик нажатий клавиатуры
void keyfunc(unsigned char key, int x, int y)
{
  if(key==' ')
    pause=!pause;
  else if(key==27)
    exit(0);
}

// обработчик нажатий мыши
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
  static double lastFrameTime=0; // время когда был выведен предыдущий кадр
  double currentTime=timeGetTime()*0.001; // текущее время
  if(currentTime-lastFrameTime>=FRAMETIME)
  {
    lastFrameTime=currentTime;
    glutPostRedisplay();
  }
}

int main(int argc, char** argv)
{
	// инициализация glut
  glutInit(&argc,argv);
  glutInitDisplayMode(GLUT_DOUBLE|GLUT_RGB);

	// создаем окно
  glutInitWindowSize(700,700);
  glutCreateWindow("Particles");
  
	// назначаем обработчики событий
  glutReshapeFunc(reshapefunc);
  glutDisplayFunc(drawfunc);
  glutKeyboardFunc(keyfunc);
  glutMouseFunc(clickfunc);
  glutIdleFunc(idlefunc);

	// параметры gl
  glClearColor(0,0,0,0); // цвет которым очищать экран
  glHint(GL_POINT_SMOOTH_HINT,GL_NICEST); // сглаживать точки лучшим из имеющихся методов
  glEnable(GL_POINT_SMOOTH); // включить сглаживание точек (кружками рисовать)
  glEnable(GL_BLEND); // включить прозрачность
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA); // стандартная ф-я прозрачности

	// частицы изначально не активны
  for(int i=0; i<3000; i++)
  {
    snow[i].draw=false;
    snow[i].clicked=false;
  }

	// инициализация генератора случ. чисел
  srand(timeGetTime());
  
	// сила ветра
  wind=0.004-0.008*rand()/RAND_MAX;

	// поехали!
  glutMainLoop();
  return 0;
}
