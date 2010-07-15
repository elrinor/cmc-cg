#include <windows.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glaux.h>
#include <GL/glut.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
 
#pragma comment(lib, "gdi32.lib")
#pragma comment(lib, "opengl32.lib")
#pragma comment(lib, "glu32.lib")
#pragma comment(lib, "glut32.lib")
#pragma comment(lib, "glaux.lib")
 
#define PI 3.14159256
#define	MAX_PARTICLES	100

unsigned int tex[10]; //здесь хранятся текстуры
GLUquadricObj *gun, *torpedo; 
AUX_RGBImageRec	*image1, *image2, *image4;
GLuint shipList; //display list корабля

static int t = 0;
double gr = 200; // радиус небесной сферы, также границы воды
float lightpos[4] = {10, 50, 10, 1};
bool bSndDelay;
int sndDelayCount;
int sndDelay;

// параметры системы частиц           
float	slowdown = 5.0f;			// Slow Down Particles

typedef struct {					// Create A Structure For Particle
	bool	active;					// Active (Yes/No)
	float	life;					// Particle Life
	float	fade;					// Fade Speed
	float	x;						// X Position
	float	y;						// Y Position
	float	z;						// Z Position
	float	xi;						// X Direction
	float	yi;						// Y Direction
	float	zi;						// Z Direction
	float	xg;						// X Gravity
	float	yg;						// Y Gravity
	float	zg;						// Z Gravity
}particles;							// Particles Structure

particles particle[MAX_PARTICLES];	// Particle Array 

float shipX=90;
float shipZ=120;
float shipY=-3;
float gunAngle=0;
float torpedoAngle=0;
bool shipLDir=true;
bool bShotDone=false;
bool bShipSunk=false;

float torpedoZ, torpedoX, torpedoY, torpedoV=10, torpedoT;


// Обработчик изменения размеров окна
void CALLBACK reshape (int width, int height) {
	glViewport(0,0,width,height);
	glMatrixMode( GL_PROJECTION );
	glLoadIdentity();
	gluPerspective(90,width/float(height),1,300);
	gluLookAt(0,5,15, 0,5,-15,0,1,0);
	glMatrixMode( GL_MODELVIEW );
}   


// инициализация партиклов
void initFire () {
	for (int loop=0; loop < MAX_PARTICLES; loop++) {			// Initials All The Textures
		particle[loop].active = true;							// Make All The Particles Active
		particle[loop].life = 0.3;
		particle[loop].fade = float(rand()%100)/1000.0f+0.003f;	// Random Fade Speed
		particle[loop].xg = 0.0f;							    // Set Horizontal Pull To Zero
		particle[loop].yg = 1.0f;							    // Set Vertical Pull Upward
		particle[loop].zg = 0.0f;	
		particle[loop].xi = 0.0;
		particle[loop].yi = 0.0;
		particle[loop].zi = 0.0;
	}
}


// небо
void skyBox () {
	GLUquadricObj *quadObj; 
	static double s = 0; //вращение неба
	
	// инит
	quadObj = gluNewQuadric(); 
	gluQuadricTexture(quadObj, GL_TRUE);
	glEnable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, tex[0]); 
	
	// создаем небо
	s +=0.05;
	glPushMatrix();
		glRotated(s,1,0,0);
		glRotated(90,0,1,0);
		gluSphere(quadObj, gr, 16, 16); // радиус = gr, 16 разбиений по каждой из осей
	glPopMatrix();
	
	// выключаем то что включили
	glDisable(GL_TEXTURE_2D);
	gluDeleteQuadric(quadObj);
}


// Сгенерить display list кораблика
void genLists (void) {
	shipList = glGenLists(1);
	glNewList(shipList, GL_COMPILE);
		glColor4d(0.5, 0.5, 0.5, 1);
		glBegin(GL_QUADS); //GL_QUADS - Каждые четыре вызова glVertex задают четырехугольник.
			glVertex3f( -15, 0, 0);		
			glVertex3f( -20, 8, 0);		
			glVertex3f( 20, 8, 0);		
			glVertex3f( 17, 0, 0);		

			glVertex3f( -2, 12, 0);		
			glVertex3f( 2, 12, 0);		
			glVertex3f( 2, 16, 0);		
			glVertex3f( -2, 16, 0);		

			glVertex3f( -7, 8, 0);		
			glVertex3f( 7, 8, 0);		
			glVertex3f( 7, 12, 0);		
			glVertex3f( -7, 12, 0);		
		glEnd();
	glEndList();
}

// глобальная инитиализация
void init (void) {
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_COLOR_MATERIAL);
	glEnable(GL_LIGHTING); //разрешить освещение
	glEnable(GL_LIGHT0);   // "нулевая лампа"
	glEnable(GL_BLEND);	   // Enable Blending
	
	// пушку создаем
	gun = gluNewQuadric(); 
	gluQuadricDrawStyle(gun, GLU_FILL);
	
	// торпеду создаем
	torpedo = gluNewQuadric(); 
	gluQuadricDrawStyle(torpedo, GLU_FILL);
	
	// сгенерить display list корабля
	genLists();
	
	// получить 3 идентификатора текстур
	glGenTextures(3, tex);	

	// загрузить текстуру неба (идентификатор сохранить в tex[0])
	glBindTexture(GL_TEXTURE_2D, tex[0]); // делаем активной текстуру
	image1 = auxDIBImageLoad("../data/textures/sky.bmp"); //грузим 
	glPixelStorei(GL_UNPACK_ALIGNMENT, 1); //выравнивание идет по байту
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST); // для уменьшения и увеличения текстуры юзаем алгоритм 
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST); // GL_NEAREST
	glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL); // учитывается только цвет текстуры. Цвет объекта не рает роли
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT); // текстура повторяется
	glTexImage2D(GL_TEXTURE_2D, 
					0 /*детализация, 0 - исходная*/, 
					3 /*кол-во компонентов цвета*/, 
					image1->sizeX,
					image1->sizeY,
					0 /*ширина границы, 0*/, 
					GL_RGB /*формат хранения пикселей*/, 
					GL_UNSIGNED_BYTE /*тип для хранения пикселей*/,
					image1->data); // превращаем изображение в текстуру
	
	// все аналогично.
	glBindTexture(GL_TEXTURE_2D, tex[2]);
	image4 = auxDIBImageLoad("../data/textures/particle.bmp"); 
	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST); 
	glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_DECAL);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexImage2D(GL_TEXTURE_2D, 0, 3, 
					image4->sizeX,
					image4->sizeY,
					0, GL_RGB, GL_UNSIGNED_BYTE,
					image4->data);
	
	glBindTexture(GL_TEXTURE_2D, tex[1]);
	image2 = auxDIBImageLoad("../data/textures/water.bmp"); 
	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST); 
	glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexImage2D(GL_TEXTURE_2D, 0, 3, 
					image2->sizeX,
					image2->sizeY,
					0, GL_RGB, GL_UNSIGNED_BYTE,
					image2->data);
	
	// инициализируем систему частиц
	initFire();
}


// система частиц
void drawFire () {
	glPushAttrib(GL_ALL_ATTRIB_BITS);
	glEnable(GL_TEXTURE_2D);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE);	// Type Of Blending To Perform
    
	glBindTexture(GL_TEXTURE_2D, tex[2]);
	for (int loop=0; loop < MAX_PARTICLES; loop++) 			// Loop Through All The Particles
		if (particle[loop].active) {					// If The Particle Is Active
			float x = particle[loop].x;					// Grab Our Particle X Position
			float y = particle[loop].y;					// Grab Our Particle Y Position
			float z = particle[loop].z;					// Particle Z Pos + Zoom
			
			glBegin(GL_QUADS);	// Build Quad
			    glTexCoord2d(1,1); glVertex3f(x+0.3f,y+0.3f,z); // Top Right
				glTexCoord2d(0,1); glVertex3f(x-0.3f,y+0.3f,z); // Top Left
				glTexCoord2d(0,0); glVertex3f(x-0.3f,y-0.3f,z); // Bottom Left
				glTexCoord2d(1,0); glVertex3f(x+0.3f,y-0.3f,z); // Bottom Right
			glEnd();										// Done Building Triangle Strip

			particle[loop].x += particle[loop].xi/(slowdown*10);   // Move On The X Axis By X Speed
			particle[loop].y += particle[loop].yi/(slowdown*10);   // Move On The Y Axis By Y Speed
			particle[loop].z += particle[loop].zi/(slowdown*10);   // Move On The Z Axis By Z Speed

			particle[loop].xi += particle[loop].xg;			// Take Pull On X Axis Into Account
			particle[loop].yi += particle[loop].yg;			// Take Pull On Y Axis Into Account
			particle[loop].zi += particle[loop].zg;			// Take Pull On Z Axis Into Account
			particle[loop].life -= particle[loop].fade;		// Reduce Particles Life By 'Fade'

			if (particle[loop].life < 0.0f) {					// If Particle Is Burned Out
				particle[loop].life = 0.8f;					    // Give It New Life
				particle[loop].fade = 10*(float(rand()%50)/1000.0f+0.003f);	// Random Fade Value
				particle[loop].x = 0.0f;						// Center On X Axis
				particle[loop].y = 0.0f;						// Center On Y Axis
				particle[loop].z = 0.0f;						// Center On Z Axis
				particle[loop].xi = 0;	                    // X Axis Speed And Direction
				particle[loop].yi = 0;	                    // Y Axis Speed And Direction
				particle[loop].zi = 0;	                    // Z Axis Speed And Direction
			}
		}
	glPopAttrib();
}


// отрисовка воды
void water() {
	glPushMatrix();
	glEnable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, tex[1]);
	glBegin(GL_QUADS);
		glTexCoord2f(0+float(t)/100, 0); glVertex3f(-gr,0,-gr);
		glTexCoord2f(50+float(t)/100, 0); glVertex3f( gr,0,-gr);
		glTexCoord2f(50+float(t)/100,50); glVertex3f( gr,0, gr);
		glTexCoord2f(0+float(t)/100,50); glVertex3f(-gr,0, gr);
	glEnd();
	glDisable(GL_TEXTURE_2D);
	glPopMatrix();
}


// Рисуем кадр
void CALLBACK display (void) {

	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
	float ambient[4] = {0.5, 0.5, 0.5, 1};

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	glLightModelfv(GL_LIGHT_MODEL_AMBIENT, ambient);
	glLightfv(GL_LIGHT0, GL_POSITION, lightpos);
	
	// обработка звуков
	if (bSndDelay) {
		sndDelayCount++;
		if (sndDelayCount>15) {
			if (bShipSunk) 
				sndPlaySound("../data/sounds/burn.wav", SND_ASYNC);
			else 
				sndPlaySound("../data/sounds/battle.wav", SND_ASYNC);
			bSndDelay=false;
		}
	}

	water(); //воду рисуем
	skyBox(); // небо рисуем

	if (!bShipSunk) {
		//рисуем кораблик
		glPushMatrix();
			glTranslatef(shipX, shipY,-shipZ);
			glCallList(shipList);
		glPopMatrix();
	}else if (shipY>=-20) {
		glPushMatrix();
			glTranslatef(shipX, shipY,-shipZ);
			glRotatef(50, 0, 0, 1); //поворачиваем кораблик, чтобы он "тонул"
			glCallList(shipList); // и отрисовываем
		glPopMatrix();

		glPushMatrix();  // а потом рисуем дымок
			glTranslatef(shipX, 0,-110);
			glRotatef(180, 0, 1, 0);	
			glScalef(10, 10, 10);
			drawFire();
		glPopMatrix();
	}

	// ТОРПЕДНЫЙ АППАРАТ
	glColor3f(0.3f, 0.3f, 0.3f);
	glPushMatrix();
		glTranslatef(0, 1, 12);
		glRotatef(gunAngle, 0, 1, 0);
		glTranslatef(0, 1, -8);
		gluCylinder(gun, 1, 1, 10, 15, 15);  
	glPopMatrix();

	// ТОРПЕДА
	if (bShotDone) {
		glColor3f(0.4f, 0.4f, 0.4f);
		float s=torpedoV*torpedoT; //путь пройденный торпедой
		float angle=PI*torpedoAngle/180; // угол в радианах под которым летела торпеда
		torpedoZ=s*cos(angle); 
		torpedoX=s*sin(angle); 
		torpedoY-=0.06;
		if (torpedoY<0) torpedoY=0;

		glPushMatrix();
			glTranslatef(-torpedoX, 0, -torpedoZ+12);
			glRotatef(torpedoAngle, 0, 1, 0);
			glTranslatef(0, torpedoY, -8);
			gluCylinder(torpedo, 0.5, 0.5, 6, 8, 8);  
		glPopMatrix();

		if (torpedoZ>=shipZ) {			
			bShotDone=false;
			if (abs(torpedoX+shipX)<=15) {
				bShipSunk=true;
				sndPlaySound("../data/sounds/bomb.wav", SND_ASYNC);								
				bSndDelay=true;
				sndDelayCount=0;
				sndDelay=40;
			}
		}
		torpedoT+=0.1;
	}

	// корабль
	if (!bShipSunk) {
		if (shipLDir) {
			shipX-=0.3;
			if (shipX<=-90) shipLDir=false;
		}else {
			shipX+=0.3;
			if (shipX>=90) shipLDir=true;
		}
	}else shipY-=0.2;//топить его

	t++; 
	auxSwapBuffers();
}


// Поворот пушки влево
void CALLBACK keyLeftProc (void) {
	if (gunAngle<45) gunAngle+=5;
	 
}


// Обработчик нажатия на пробел
void CALLBACK keySpaceProc (void) {
	if (bShipSunk) {
		bShipSunk=false;
		bShotDone=false;
		sndPlaySound("../data/sounds/battle.wav", SND_ASYNC);
		if (shipLDir) {
			shipX=-90;
			shipLDir=false;
		}else {
			shipX=90;
			shipLDir=true;
		}
		shipY=-3;
	}
}


// Пушку вправо
void CALLBACK keyRightProc (void) {
	if (gunAngle>-45) gunAngle-=5;
}


// торпеду пустить
void CALLBACK keyEnterProc (void) {
	if (!bShotDone && shipY==-3) {
		sndPlaySound("../data/sounds/bazooka.wav", SND_ASYNC);
		sndDelay=20;
		sndDelayCount=0;
		bSndDelay=true;
		torpedoZ=0;
		torpedoX=0;
		torpedoY=2;
		torpedoT=0;
		torpedoAngle=gunAngle;
		bShotDone=true;
	}
}

// Main
void main(int argc, char *argv[]) {
	float pos[4] = {0,20,-10,1};
	float dir[3] = {0,0,0};
	float ambient[4] = {0.5, 0.5, 0.5, 1};
	
	auxInitPosition( 50, 10, 600, 600); // позиция и размеры окна
	auxInitDisplayMode( AUX_RGB | AUX_DEPTH | AUX_DOUBLE | AUX_STENCIL );
	auxInitWindow("SeaBattle (ENTER-shoot; Space-new ship; ESQ-exit)"); // инициализация окна (заголовком)

	init(); //инициализация наших переменных и т.п.

	auxKeyFunc(AUX_LEFT, keyLeftProc);    // назначаем обработчики клавиш
    auxKeyFunc(AUX_RIGHT, keyRightProc);  
	auxKeyFunc(AUX_RETURN, keyEnterProc);
	auxKeyFunc(AUX_SPACE, keySpaceProc);

	auxReshapeFunc(reshape);
	auxIdleFunc(display);

	sndPlaySound("../data/sounds/battle.wav", SND_ASYNC);
	
	auxMainLoop(display);
}
