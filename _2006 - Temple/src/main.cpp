#include <windows.h>
#include <gl\glut.h>
#include <gl\glext.h>
#include <stdio.h>
#include <math.h>
#include "libTexture.h"
#include "perlin.h"
#include "particle.h"
#include "bass.h"
#pragma comment(lib, "Bass.lib")
#define PI 3.141592


int w_x, w_y; // размеры окна
float m_x=0, m_y=0; // угол камеры (по горизонтали и вертикали)
float p_x=3, p_y=3, p_z=2.5; // положение камеры
bool wk=false, ak=false, sk=false, dk=false; // зажата ли кнопка W/A/S/D

float t=0, tt=0; // прошедшее время с начала работы программы и с пред. кадра (в секундах)

GLuint textures[15];

HSTREAM snd; // поток звука

// рисуем одну колонну
void column(float x, float y, float r)
{
	glBegin(GL_QUAD_STRIP);
	for(int i=0; i<=20; i++)
	{
		glTexCoord2f(3,i/20.0);glNormal3f(cos(2*PI*i/20.0),sin(2*PI*i/20.0),0);glVertex3f(x+r*cos(2*PI*i/20.0),y+r*sin(2*PI*i/20.0),3);
		glTexCoord2f(0,i/20.0);glNormal3f(cos(2*PI*i/20.0),sin(2*PI*i/20.0),0);glVertex3f(x+r*cos(2*PI*i/20.0),y+r*sin(2*PI*i/20.0),0);
	}
	glEnd();
	glBegin(GL_QUAD_STRIP);
	for(int i=0; i<=20; i++)
	{
		glTexCoord2f(3,i/20.0);glNormal3f(cos(2*PI*i/20.0),sin(2*PI*i/20.0),0);glVertex3f(x+r*cos(2*PI*i/20.0),y+r*sin(2*PI*i/20.0),3);
		glTexCoord2f(4,i/20.0);glNormal3f(cos(2*PI*i/20.0),sin(2*PI*i/20.0),0);glVertex3f(x+r*1.3*cos(2*PI*i/20.0),y+r*1.3*sin(2*PI*i/20.0),4);
	}
	glEnd();
}

// рисуем скайбокс
void skyBox(float r)
{
	glEnable(GL_TEXTURE_2D);
	glDisable(GL_LIGHTING);
	glColor4f(1,1,1,1);
	glBindTexture(GL_TEXTURE_2D, textures[0]);
	glBegin(GL_QUADS);
		glTexCoord2f(0,0);glVertex3f(-r, r, r); 
		glTexCoord2f(1,0);glVertex3f( r, r, r);
		glTexCoord2f(1,1);glVertex3f( r,-r, r);
		glTexCoord2f(0,1);glVertex3f(-r,-r, r);
	glEnd();
  glBindTexture(GL_TEXTURE_2D, textures[1]);
	glBegin(GL_QUADS);
		glTexCoord2f(0,0);glVertex3f(-r,-r,-r); 
		glTexCoord2f(1,0);glVertex3f( r,-r,-r);
		glTexCoord2f(1,1);glVertex3f( r, r,-r);
		glTexCoord2f(0,1);glVertex3f(-r, r,-r);
	glEnd();
  glBindTexture(GL_TEXTURE_2D, textures[2]);
	glBegin(GL_QUADS);
		glTexCoord2f(0,0);glVertex3f(-r, r, r); 
		glTexCoord2f(1,0);glVertex3f(-r,-r, r);
		glTexCoord2f(1,1);glVertex3f(-r,-r,-r);
		glTexCoord2f(0,1);glVertex3f(-r, r,-r);
	glEnd();
  glBindTexture(GL_TEXTURE_2D, textures[3]);
	glBegin(GL_QUADS);
		glTexCoord2f(0,0);glVertex3f(-r,-r, r); 
		glTexCoord2f(1,0);glVertex3f( r,-r, r);
		glTexCoord2f(1,1);glVertex3f( r,-r,-r);
		glTexCoord2f(0,1);glVertex3f(-r,-r,-r);
	glEnd();
  glBindTexture(GL_TEXTURE_2D, textures[4]);
	glBegin(GL_QUADS);
		glTexCoord2f(0,0);glVertex3f( r,-r, r); 
		glTexCoord2f(1,0);glVertex3f( r, r, r);
		glTexCoord2f(1,1);glVertex3f( r, r,-r);
		glTexCoord2f(0,1);glVertex3f( r,-r,-r);
	glEnd();
  glBindTexture(GL_TEXTURE_2D, textures[5]);
	glBegin(GL_QUADS);
		glTexCoord2f(0,0);glVertex3f( r, r, r); 
		glTexCoord2f(1,0);glVertex3f(-r, r, r);
		glTexCoord2f(1,1);glVertex3f(-r, r,-r);
		glTexCoord2f(0,1);glVertex3f( r, r,-r);
	glEnd();
}

// рисуем сцену
void scene()
{
	glEnable(GL_TEXTURE_2D);

	// Свет
	glEnable(GL_LIGHTING);
	GLfloat l_position[4]={1,1,3,1};
  glLightfv(GL_LIGHT0,GL_POSITION,l_position);
	GLfloat l_position1[4]={-1,-1,3,1};
  glLightfv(GL_LIGHT1,GL_POSITION,l_position1);

	// Колонны
	glBindTexture(GL_TEXTURE_2D, textures[6]);
	for(int i=1; i<10; i++)
		column(5*cos(2*PI*i/10),5*sin(2*PI*i/10),0.35);
	
	// Потолок
	glBindTexture(GL_TEXTURE_2D, textures[8]);
	glBegin(GL_QUAD_STRIP);
		glNormal3f(0,0,-1);
		for(int i=0;i<=40;i++)
		{
			glTexCoord2f(2*cos(2*PI*i/40)/2,2*sin(2*PI*i/40)/2);glVertex3f(2*cos(2*PI*i/40),2*sin(2*PI*i/40),4);
			glTexCoord2f(7*cos(2*PI*i/40)/2,7*sin(2*PI*i/40)/2);glVertex3f(7*cos(2*PI*i/40),7*sin(2*PI*i/40),4);
		}
	glEnd();
	glBegin(GL_QUAD_STRIP);
		glNormal3f(0,0,-1);
		for(int i=0;i<=40;i++)
		{
			glTexCoord2f(2*cos(2*PI*i/40)/2,2*sin(2*PI*i/40)/2);  glVertex3f(2*cos(2*PI*i/40),2*sin(2*PI*i/40),4);
			glTexCoord2f(7*cos(2*PI*i/40)/2+2,7*sin(2*PI*i/40)/2);glVertex3f(2*cos(2*PI*i/40),2*sin(2*PI*i/40),5);
		}
	glEnd();

	// Стены
	glBindTexture(GL_TEXTURE_2D, textures[8]);
	glBegin(GL_QUAD_STRIP);
	for(int i=1; i<=39; i++)
	{
		glTexCoord2f(0,i/3.0);glNormal3f(-cos(2*PI*i/40.0),-sin(2*PI*i/40.0),0);glVertex3f(7*cos(2*PI*i/40.0),7*sin(2*PI*i/40.0),-0.01);
		glTexCoord2f(1,i/3.0);glNormal3f(-cos(2*PI*i/40.0),-sin(2*PI*i/40.0),0);glVertex3f(7*cos(2*PI*i/40.0),7*sin(2*PI*i/40.0),4);
	}
	glEnd();

	// Дверь
	glBindTexture(GL_TEXTURE_2D, textures[8]);
	glBegin(GL_QUADS);
		glTexCoord2f(0,1/3.0);glNormal3f(-cos(2*PI*1/40.0),-sin(2*PI*1/40.0),0);glVertex3f(7*cos(2*PI*1/40.0),7*sin(2*PI*1/40.0),-0.01);
		glTexCoord2f(1,1/3.0);glNormal3f(-cos(2*PI*1/40.0),-sin(2*PI*1/40.0),0);glVertex3f(7*cos(2*PI*1/40.0),7*sin(2*PI*1/40.0),4);
		glTexCoord2f(1,0/3.0);glNormal3f(-cos(2*PI*1/40.0),-sin(2*PI*1/40.0),0);glVertex3f(8*cos(2*PI*1/40.0),8*sin(2*PI*1/40.0),4);
		glTexCoord2f(0,0/3.0);glNormal3f(-cos(2*PI*1/40.0),-sin(2*PI*1/40.0),0);glVertex3f(8*cos(2*PI*1/40.0),8*sin(2*PI*1/40.0),-0.01);
		glTexCoord2f(0,39/3.0);glNormal3f(-cos(2*PI*39/40.0),-sin(2*PI*39/40.0),0);glVertex3f(7*cos(2*PI*39/40.0),7*sin(2*PI*39/40.0),-0.01);
		glTexCoord2f(1,39/3.0);glNormal3f(-cos(2*PI*39/40.0),-sin(2*PI*39/40.0),0);glVertex3f(7*cos(2*PI*39/40.0),7*sin(2*PI*39/40.0),4);
		glTexCoord2f(1,40/3.0);glNormal3f(-cos(2*PI*39/40.0),-sin(2*PI*39/40.0),0);glVertex3f(8*cos(2*PI*39/40.0),8*sin(2*PI*39/40.0),4);
		glTexCoord2f(0,40/3.0);glNormal3f(-cos(2*PI*39/40.0),-sin(2*PI*39/40.0),0);glVertex3f(8*cos(2*PI*39/40.0),8*sin(2*PI*39/40.0),-0.01);
		glTexCoord2f(1,1);glNormal3f(-cos(2*PI*39/40.0),-sin(2*PI*39/40.0),0);glVertex3f(7*cos(2*PI*39/40.0),7*sin(2*PI*39/40.0),3.99);
		glTexCoord2f(1,0);glNormal3f(-cos(2*PI*39/40.0),-sin(2*PI*39/40.0),0);glVertex3f(8*cos(2*PI*39/40.0),8*sin(2*PI*39/40.0),3.99);
		glTexCoord2f(0,0);glNormal3f(-cos(2*PI*1/40.0),-sin(2*PI*1/40.0),0);glVertex3f(8*cos(2*PI*1/40.0),8*sin(2*PI*1/40.0),3.99);
		glTexCoord2f(0,1);glNormal3f(-cos(2*PI*1/40.0),-sin(2*PI*1/40.0),0);glVertex3f(7*cos(2*PI*1/40.0),7*sin(2*PI*1/40.0),3.99);
		glTexCoord2f(1,1);glNormal3f(-cos(2*PI*39/40.0),-sin(2*PI*39/40.0),0);glVertex3f(7*cos(2*PI*39/40.0),7*sin(2*PI*39/40.0),4.00);
		glTexCoord2f(1,0);glNormal3f(-cos(2*PI*39/40.0),-sin(2*PI*39/40.0),0);glVertex3f(8*cos(2*PI*39/40.0),8*sin(2*PI*39/40.0),4.01);
		glTexCoord2f(0,0);glNormal3f(-cos(2*PI*1/40.0),-sin(2*PI*1/40.0),0);glVertex3f(8*cos(2*PI*1/40.0),8*sin(2*PI*1/40.0),4.01);
		glTexCoord2f(0,1);glNormal3f(-cos(2*PI*1/40.0),-sin(2*PI*1/40.0),0);glVertex3f(7*cos(2*PI*1/40.0),7*sin(2*PI*1/40.0),4.00);
	glEnd();

	// Коробка в которой находится ландшафт
	glBindTexture(GL_TEXTURE_2D, textures[10]);
	glBegin(GL_QUAD_STRIP);
		glNormal3f(-sqrt(2.0),-sqrt(2.0),0);
		glTexCoord2f(0,1);glVertex3f( 0.5, 0.5,1.2);
		glTexCoord2f(0,0);glVertex3f( 0.5, 0.5,1.6);
		glNormal3f( sqrt(2.0),-sqrt(2.0),0);
		glTexCoord2f(1,1);glVertex3f(-0.5, 0.5,1.2);
		glTexCoord2f(1,0);glVertex3f(-0.5, 0.5,1.6);
		glNormal3f( sqrt(2.0), sqrt(2.0),0);
		glTexCoord2f(2,1);glVertex3f(-0.5,-0.5,1.2);
		glTexCoord2f(2,0);glVertex3f(-0.5,-0.5,1.6);
		glNormal3f(-sqrt(2.0), sqrt(2.0),0);
		glTexCoord2f(3,1);glVertex3f( 0.5,-0.5,1.2);
		glTexCoord2f(3,0);glVertex3f( 0.5,-0.5,1.6);
		glNormal3f(-sqrt(2.0),-sqrt(2.0),0);
		glTexCoord2f(4,1);glVertex3f( 0.5, 0.5,1.2);
		glTexCoord2f(4,0);glVertex3f( 0.5, 0.5,1.6);
	glEnd();
	glBegin(GL_QUAD_STRIP);
		glNormal3f( sqrt(2.0), sqrt(2.0),0);
		glTexCoord2f(0.00,1);glVertex3f( 0.1, 0.1,0.0);
		glTexCoord2f(0.00,0);glVertex3f( 0.1, 0.1,1.2);
		glNormal3f(-sqrt(2.0), sqrt(2.0),0);
		glTexCoord2f(0.25,1);glVertex3f(-0.1, 0.1,0.0);
		glTexCoord2f(0.25,0);glVertex3f(-0.1, 0.1,1.2);
		glNormal3f(-sqrt(2.0),-sqrt(2.0),0);
		glTexCoord2f(0.50,1);glVertex3f(-0.1,-0.1,0.0);
		glTexCoord2f(0.50,0);glVertex3f(-0.1,-0.1,1.2);
		glNormal3f( sqrt(2.0),-sqrt(2.0),0);
		glTexCoord2f(0.75,1);glVertex3f( 0.1,-0.1,0.0);
		glTexCoord2f(0.75,0);glVertex3f( 0.1,-0.1,1.2);
		glNormal3f( sqrt(2.0), sqrt(2.0),0);
		glTexCoord2f(1.00,1);glVertex3f( 0.1, 0.1,0.0);
		glTexCoord2f(1.00,0);glVertex3f( 0.1, 0.1,1.2);
	glEnd();
	glBegin(GL_QUAD_STRIP);
		glNormal3f( sqrt(2.0), sqrt(2.0),0);
		glTexCoord2f(0,1);glVertex3f( 0.55, 0.55,1.6);
		glTexCoord2f(0,0);glVertex3f( 0.55, 0.55,1.19);
		glNormal3f(-sqrt(2.0), sqrt(2.0),0);
		glTexCoord2f(1,1);glVertex3f(-0.55, 0.55,1.6);
		glTexCoord2f(1,0);glVertex3f(-0.55, 0.55,1.19);
		glNormal3f(-sqrt(2.0),-sqrt(2.0),0);
		glTexCoord2f(2,1);glVertex3f(-0.55,-0.55,1.6);
		glTexCoord2f(2,0);glVertex3f(-0.55,-0.55,1.19);
		glNormal3f( sqrt(2.0),-sqrt(2.0),0);
		glTexCoord2f(3,1);glVertex3f( 0.55,-0.55,1.6);
		glTexCoord2f(3,0);glVertex3f( 0.55,-0.55,1.19);
		glNormal3f( sqrt(2.0), sqrt(2.0),0);
		glTexCoord2f(4,1);glVertex3f( 0.55, 0.55,1.6);
		glTexCoord2f(4,0);glVertex3f( 0.55, 0.55,1.19);
	glEnd();
	glBegin(GL_QUADS);
		glNormal3f(0,0,1);
		glTexCoord2f( 0.5, 0.5);  glVertex3f( 0.5 , 0.5 , 1.6);
		glTexCoord2f(-0.5, 0.5);  glVertex3f(-0.5 , 0.5 , 1.6);
		glTexCoord2f(-0.55,0.55); glVertex3f(-0.55, 0.55, 1.6);
		glTexCoord2f( 0.55,0.55); glVertex3f( 0.55, 0.55, 1.6);
		glTexCoord2f(-0.5,-0.5);  glVertex3f(-0.5 ,-0.5 , 1.6);
		glTexCoord2f(-0.5, 0.5);  glVertex3f(-0.5 , 0.5 , 1.6);
		glTexCoord2f(-0.55,0.55); glVertex3f(-0.55, 0.55, 1.6);
		glTexCoord2f(-0.55,-0.55);glVertex3f(-0.55,-0.55, 1.6);
		glTexCoord2f( 0.5, -0.5); glVertex3f( 0.5 ,-0.5 , 1.6);
		glTexCoord2f(-0.5, -0.5); glVertex3f(-0.5 ,-0.5 , 1.6);
		glTexCoord2f(-0.55,-0.55);glVertex3f(-0.55,-0.55, 1.6);
		glTexCoord2f( 0.55,-0.55);glVertex3f( 0.55,-0.55, 1.6);
		glTexCoord2f( 0.5,  0.5); glVertex3f( 0.5 , 0.5 , 1.6);
		glTexCoord2f( 0.5, -0.5); glVertex3f( 0.5 ,-0.5 , 1.6);
		glTexCoord2f( 0.55,-0.55);glVertex3f( 0.55,-0.55, 1.6);
		glTexCoord2f( 0.55, 0.55);glVertex3f( 0.55, 0.55, 1.6);
		glNormal3f(0,0,-1);
		glTexCoord2f(0,0);glVertex3f( 0.55, 0.55, 1.19);
		glTexCoord2f(0,1);glVertex3f( 0.55,-0.55, 1.19);
		glTexCoord2f(1,1);glVertex3f(-0.55,-0.55, 1.19);
		glTexCoord2f(1,0);glVertex3f(-0.55, 0.55, 1.19);
	glEnd();

	// Карта (ландшафт)
	glNormal3f(0,0,-1);
	drawPerlinLandscape(-0.5,-0.5,1.5); // рисуем карту
	glBindTexture(GL_TEXTURE_2D, textures[9]);
	glDisable(GL_LIGHTING);
	glEnable(GL_BLEND);
	glColor4f(0,0,1,0.5);
	glBegin(GL_QUADS); // рисуем воду
		glTexCoord2f(0,0);glVertex3f( 0.5, 0.5,1.5);
		glTexCoord2f(1,0);glVertex3f(-0.5, 0.5,1.5);
		glTexCoord2f(1,1);glVertex3f(-0.5,-0.5,1.5);
		glTexCoord2f(0,1);glVertex3f( 0.5,-0.5,1.5);
	glEnd();
	glDisable(GL_BLEND);
	glEnable(GL_LIGHTING);

	glDisable(GL_TEXTURE_2D);
}

// Отрисовка кадра
void render()
{
	// Сначала рисуем неотраженную сцену
	skyBox(40); 
	glEnable(GL_MULTISAMPLE_ARB);
	scene(); 
	glDisable(GL_MULTISAMPLE_ARB);

	// Потом заполняем Stencil-буфер
	glDepthMask(GL_FALSE);
	glColorMask(GL_FALSE,GL_FALSE,GL_FALSE,GL_FALSE);
	glEnable(GL_STENCIL_TEST);
	glStencilFunc(GL_ALWAYS,1,1);
	glStencilOp(GL_KEEP,GL_KEEP,GL_REPLACE);
	glBegin(GL_QUADS);
		glVertex3f( 8, 8,0);
		glVertex3f(-8, 8,0);
		glVertex3f(-8,-8,0);
		glVertex3f( 8,-8,0);
	glEnd();
	glDepthMask(GL_TRUE);
	glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
	glPushMatrix();
	glScalef(1,1,-1);
	glStencilFunc(GL_EQUAL,1,1);
	glEnable(GL_MULTISAMPLE_ARB);
	scene(); // рисуем отраженную сцену
	glDisable(GL_MULTISAMPLE_ARB);
	skyBox(39.5); // отраженный скайбокс
	drawParticleSystem(); // и отраженную систему частиц
	glPopMatrix(); 
	
	// Рисуем пол полупрозрачным
	glEnable(GL_TEXTURE_2D);
	glEnable(GL_LIGHTING);
	GLfloat l_position[4]={1,1,3,1};
  glLightfv(GL_LIGHT0,GL_POSITION,l_position);
	GLfloat l_position1[4]={-1,-1,3,1};
  glLightfv(GL_LIGHT1,GL_POSITION,l_position1);
	glEnable(GL_BLEND);
	glBindTexture(GL_TEXTURE_2D, textures[7]);
	glBegin(GL_QUADS);
	glNormal3f(0,0,1);
		for(int i=-8; i<8; i++)
			for(int j=-8; j<8; j++)
			{
				glTexCoord2f(i,  j);  glVertex3f(i,  j,  0);
				glTexCoord2f(i+1,j);  glVertex3f(i+1,j,  0);
				glTexCoord2f(i+1,j+1);glVertex3f(i+1,j+1,0);
				glTexCoord2f(i,  j+1);glVertex3f(i  ,j+1,0);
			}
	glEnd();
	glDisable(GL_STENCIL_TEST);
	drawParticleSystem(); // рисуем неотраженную систему частиц
	glDisable(GL_TEXTURE_2D);
	glDisable(GL_LIGHTING);
	glDisable(GL_BLEND);
}

// обработчик display
void do_display()
{
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_STENCIL_BUFFER_BIT);
	
	// ставим камеру в нужное место
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(80.0,(float)w_x/w_y,0.001,100);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	gluLookAt(p_x,p_y,p_z,p_x+sin(m_x)*cos(m_y),p_y+cos(m_x)*cos(m_y),p_z+sin(m_y),0,0,1);

  updateParticleSystem(tt); // обновляем систему частиц
	render(); // и рисуем кадр
	
	glutSwapBuffers();
}

// обработчик изменения размеров окна
void do_reshape(int x, int y)
{
	w_x=x;
	w_y=y;
	glViewport(0,0,w_x,w_y);
}

// обработчик idle
void do_idle()
{
	BASS_Update(); // пусть музыка играет

	// считаем fps, t и tt
	static int prevt=timeGetTime();
	static int prevfpst=timeGetTime();
	static int fpsframes=0;
	tt=(timeGetTime()-prevt)/1000.0;
	t+=tt;
	prevt=timeGetTime();
	fpsframes++;
	if(timeGetTime()-prevfpst>500)
	{
		char header[100];
		sprintf(header,"Temple. %f fps.",1000.0*fpsframes/(timeGetTime()-prevfpst));
		glutSetWindowTitle(header);
		prevfpst=timeGetTime();
		fpsframes=0;
	}

	// передвигаем камеру
	const float Speed=5;
  if(wk)
  {
    p_x+=Speed*tt*sin(m_x)*cos(m_y);
    p_y+=Speed*tt*cos(m_x)*cos(m_y);
    p_z+=Speed*tt*sin(m_y);
  }
  if(sk)
  {
    p_x-=Speed*tt*sin(m_x)*cos(m_y);
    p_y-=Speed*tt*cos(m_x)*cos(m_y);
    p_z-=Speed*tt*sin(m_y);
  }
  if(ak)
  {
    p_y+=Speed*tt*sin(m_x);
    p_x-=Speed*tt*cos(m_x);
  }
  if(dk)
  {
    p_y-=Speed*tt*sin(m_x);
    p_x+=Speed*tt*cos(m_x);
  }
	if(p_z<0.1)	
		p_z=0.1;
	if(p_z>3.8)	
		p_z=3.8;
	if(p_x*p_x+p_y*p_y<0.8*0.8)
	{
		float l=sqrt(p_x*p_x+p_y*p_y);
		p_x=p_x/l*0.8;
		p_y=p_y/l*0.8;
	}
	if(p_x*p_x+p_y*p_y>4.5*4.5)
	{
		float l=sqrt(p_x*p_x+p_y*p_y);
		p_x=p_x/l*4.5;
		p_y=p_y/l*4.5;
	}

	glutPostRedisplay(); // и рисуем
}

// обработчик нажатия кнопки на клаве
void do_keydown(unsigned char Key, int x, int y)
{
  if(Key==27)
	  exit(0);
	if(Key=='w')
    wk=true;
  if(Key=='s')
    sk=true;
  if(Key=='a')
    ak=true;
  if(Key=='d')
    dk=true;
}

// обработчик отпускания кнопки на клаве
void do_keyup(unsigned char Key, int x, int y)
{
  if(Key=='w')
    wk=false;
  if(Key=='s')
    sk=false;
  if(Key=='a')
    ak=false;
  if(Key=='d')
    dk=false;
}

// обработчик движений мыши
void do_mousemove(int x, int y)
{
	const float m_sens=0.01;
  if(x!=w_x/2 || y!=w_y/2)
  {
    m_x+=(x-w_x/2)*m_sens; 
    m_y+=-(y-w_y/2)*m_sens;
    if(m_x<0) m_x+=2*PI; 
    if(m_x>=2*PI) m_x-=2*PI; 
    if(m_y>PI*0.49) m_y=PI*0.49; 
    if(m_y<-PI*0.49) m_y=-PI*0.49; 
    glutWarpPointer(w_x/2,w_y/2);
  }
}

// инициализация
void do_init()
{
	// пытаемся включить мультисэмплинг
	int s;
  glGetIntegerv(GL_SAMPLES_ARB, &s);
  if (!glutExtensionSupported("GL_ARB_multisample") || s < 1) 
    printf("Warning: multisample antialiasing not supported.\n");
	else
		printf("Multisample antialiasing supported.\n");
  printf("GL_RENDERER = %s\n", (char *) glGetString(GL_RENDERER));
  printf("GL_SAMPLES_ARB = %d\n", s);
	glGetIntegerv(GL_MULTISAMPLE_ARB, &s);
  printf("GL_MULTISAMPLE_ARB = %d\n", s);

	srand(timeGetTime()); // инициализируем генератор случ. чисел
  
	// Инициализация OpenGL - установка параметров которые нам наиболее подходят
	glClearColor(0,0,0,0); 
  glEnable(GL_DEPTH_TEST); 
  glDisable(GL_CULL_FACE);
  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
  glShadeModel(GL_SMOOTH);
  glHint(GL_POINT_SMOOTH_HINT,GL_NICEST);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  
	// Установка параметров материала (он один на все)
  GLfloat m_emissive[4]={0.2, 0.2, 0.2, 0.0};
  GLfloat m_diffuse[4] ={0.5, 0.5, 0.5, 0.5}; // прозрачность 50% !
  GLfloat m_specular[4]={0.5, 0.5, 0.5, 0.0};
  GLfloat m_ambient[4] ={0.5, 0.5, 0.5, 0.0};
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, m_emissive);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, m_specular);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, m_ambient);
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS,1);

	// Установка параметров источников света
	glEnable(GL_LIGHT0);
	glEnable(GL_LIGHT1);
	GLfloat l_position[4]={0,0,3,1};
  GLfloat l_diffuse[4] ={0,0.5,0.8,1};
  GLfloat l_specular[4]={0,0.4,0.8,1};
  GLfloat l_ambient[4] ={0.3,0.3,0.3,1};
  glLightfv(GL_LIGHT0,GL_POSITION,l_position);
  glLightfv(GL_LIGHT0,GL_DIFFUSE,l_diffuse);
  glLightfv(GL_LIGHT0,GL_SPECULAR,l_specular);
  glLightfv(GL_LIGHT0,GL_AMBIENT,l_ambient);
  glLightf(GL_LIGHT0,GL_QUADRATIC_ATTENUATION,0.05);
  glLightf(GL_LIGHT0, GL_SPOT_EXPONENT, 1);
	glLightfv(GL_LIGHT1,GL_POSITION,l_position);
  glLightfv(GL_LIGHT1,GL_DIFFUSE,l_diffuse);
  glLightfv(GL_LIGHT1,GL_SPECULAR,l_specular);
  glLightfv(GL_LIGHT1,GL_AMBIENT,l_ambient);
  glLightf(GL_LIGHT1,GL_QUADRATIC_ATTENUATION,0.05);
  glLightf(GL_LIGHT1, GL_SPOT_EXPONENT, 1);
	
	glLightModelf(GL_LIGHT_MODEL_LOCAL_VIEWER,1);

	// режим наложения текстуры
	glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);

	// грузим текстуры
	textures[0]=createTexture2D(true, "Data/_up.bmp");
	textures[1]=createTexture2D(true, "Data/_down.bmp");
	textures[2]=createTexture2D(true, "Data/_side1.bmp");
	textures[3]=createTexture2D(true, "Data/_side2.bmp");
	textures[4]=createTexture2D(true, "Data/_side3.bmp");
	textures[5]=createTexture2D(true, "Data/_side4.bmp");
	glBindTexture(GL_TEXTURE_2D, textures[0]);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	glBindTexture(GL_TEXTURE_2D, textures[1]);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	glBindTexture(GL_TEXTURE_2D, textures[2]);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	glBindTexture(GL_TEXTURE_2D, textures[3]);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	glBindTexture(GL_TEXTURE_2D, textures[4]);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	glBindTexture(GL_TEXTURE_2D, textures[5]);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	textures[6]=createTexture2D(true, "Data/plaster15.bmp");
	textures[7]=createTexture2D(true, "Data/design21.bmp");
	textures[8]=createTexture2D(true, "Data/greymarble2.bmp");
	textures[9]=createTexture2D(true, "Data/water5.bmp");
	textures[10]=createTexture2D(true, "Data/wood21.bmp");

	createPerlinLandscape();
	initParticleSystem();
}

int main(int argc, char** argv)
{
	//инициализируем glut
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE|GLUT_DEPTH|GLUT_RGB|GLUT_ALPHA|GLUT_MULTISAMPLE|GLUT_STENCIL);
  glutInitWindowSize(600,600);
  glutCreateWindow("Temple");
  glutDisplayFunc(do_display);
  glutReshapeFunc(do_reshape);
  glutIdleFunc(do_idle);
  glutKeyboardFunc(do_keydown);
  glutKeyboardUpFunc(do_keyup);
  glutPassiveMotionFunc(do_mousemove); 
  glutSetCursor(GLUT_CURSOR_NONE);
  glutIgnoreKeyRepeat(1);
  do_init();

	// переходим в FullScreen если надо
  if (MessageBox(NULL,"Fullscreen Mode?", "Start FullScreen?",MB_YESNO|MB_ICONQUESTION)==IDYES)
  {
    DEVMODE d;        
    memset(&d,0,sizeof(d));
    d.dmSize=sizeof(d);  
    d.dmPelsWidth=640;    
    d.dmPelsHeight=480;    
    d.dmBitsPerPel=32;      
    d.dmFields=DM_BITSPERPEL|DM_PELSWIDTH|DM_PELSHEIGHT;
    ChangeDisplaySettings(&d,CDS_FULLSCREEN);
    glutFullScreen();  
  }

	// Включаем звук
  /*
	BASS_Init(-1, 44100, BASS_DEVICE_NOTHREAD, 0);
  snd=BASS_StreamCreateFile(false, "Data/complicated.mp3", 0, 0, 0); 
  BASS_Start();
  BASS_ChannelSetPosition(snd, (QWORD)MAKELONG(0, 0));
  BASS_StreamPlay(snd, 0, BASS_SAMPLE_LOOP);
	*/

	glutMainLoop();
  return 0;
}