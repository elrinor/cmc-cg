

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
 
void genLists (void);

unsigned int tex[10];

unsigned int particleTexID, waterTexID, skyTexID;


GLUquadricObj *gun, *torpedo; 

AUX_RGBImageRec	*image1, *image2, *image3,*image4;

/// идентификаторы дисплейных списков
GLuint shipList;


static int time = 0, ii = -1, t= 0,ll = -1, l = 0;
static int ip = 1;
const int N = 1000;
double x = 0,z=0, gr = 150;
int i = 0,j = 0, v_h = 3;
double vertx[N+1], vertz[2];
float lightpos[4] = {10, 50, 10, 1};
//float dir[3] = {0,0,0};


bool bSndDelay;
int sndDelayCount;
static int w_width, w_height; 
int sndDelay;

#define PI 3.14159256


#define	MAX_PARTICLES	100

/// параметры системы частиц           
float	slowdown = 5.0f;			/// Slow Down Particles
float	xspeed;						/// Base X Speed (To Allow Keyboard Direction Of Tail)
float	yspeed;						/// Base Y Speed (To Allow Keyboard Direction Of Tail)
float	zoom1 = -20.0f;				/// Used To Zoom Out

GLuint	loop;						/// Misc Loop Variable
GLuint	color;						/// Current Color Selection
GLuint	delay;						/// Rainbow Effect Delay


typedef struct {					/// Create A Structure For Particle
	bool	active;					/// Active (Yes/No)
	float	life;					/// Particle Life
	float	fade;					/// Fade Speed
	float	r;						/// Red Value
	float	g;						/// Green Value
	float	b;						/// Blue Value
	float	x;						/// X Position
	float	y;						/// Y Position
	float	z;						/// Z Position
	float	xi;						/// X Direction
	float	yi;						/// Y Direction
	float	zi;						/// Z Direction
	float	xg;						/// X Gravity
	float	yg;						/// Y Gravity
	float	zg;						/// Z Gravity
}particles;							/// Particles Structure

particles particle[MAX_PARTICLES];	/// Particle Array (Room For Particle Info)


static GLfloat colors[12][3] =		/// Rainbow Of Colors
{
	{1.0f,0.5f,0.5f},{1.0f,0.75f,0.5f},{1.0f,1.0f,0.5f},{0.75f,1.0f,0.5f},
	{0.5f,1.0f,0.5f},{0.5f,1.0f,0.75f},{0.5f,1.0f,1.0f},{0.5f,0.75f,1.0f},
	{0.5f,0.5f,1.0f},{0.75f,0.5f,1.0f},{1.0f,0.5f,1.0f},{1.0f,0.5f,0.75f}
};



float shipX=90;
float shipZ=120;
float shipY=-3;
float gunAngle=0;
float torpedoAngle=0;
bool shipLDir=true;
bool bShotDone=false;
bool bShipSunk=false;


float torpedoZ, torpedoX, torpedoY, torpedoV=10, torpedoT;


void CALLBACK reshape (int width, int height) {
	w_width = width;
	w_height = height;
	 glViewport(0,0,width,height);
	 glMatrixMode( GL_PROJECTION );
	 glLoadIdentity();
	 gluPerspective(90,1,1,300);
	 
	 gluLookAt(0,5,15, 0,5,-15,0,1,0);
	 glMatrixMode( GL_MODELVIEW );
}   


void initFire () {
	for (loop=0; loop < MAX_PARTICLES; loop++) {				/// Initials All The Textures
		particle[loop].active = true;							/// Make All The Particles Active
		particle[loop].life = 0.3;
		particle[loop].fade = float(rand()%100)/1000.0f+0.003f;	/// Random Fade Speed
		particle[loop].r = colors[loop*(12/MAX_PARTICLES)][0];	/// Select Red Rainbow Color
		particle[loop].g = colors[loop*(12/MAX_PARTICLES)][1];	/// Select Red Rainbow Color
		particle[loop].b = colors[loop*(12/MAX_PARTICLES)][2];	/// Select Red Rainbow Color
		particle[loop].xg = 0.0f;							    /// Set Horizontal Pull To Zero
		particle[loop].yg = 2.0f;							    /// Set Vertical Pull Downward
		particle[loop].zg = 0.0f;	
	
		particle[loop].xi = 0.0;
		particle[loop].yi = 0.0;
		particle[loop].zi = 0.0;
	}

}


void getNorm (double a[3], 
			  double b[3], 
			  double c[3], 
			  double *n) {
	double mult = 0;
	int i,j;
	
	n[0] = (b[1] - a[1])*(c[2] - a[2])-(b[2] - a[2])*(c[1] - a[1]);
	n[1] = (c[0] - a[0])*(b[2] - a[2])-(b[0] - a[0])*(c[2] - a[2]);
	n[2] = (b[0] - a[0])*(c[1] - a[1])-(c[0] - a[0])*(b[1] - a[1]);
	
	for(i = 0;i<3;i++)
		mult += a[i]*n[i];
	if (mult <= 0)
		for (j = 0; j < 3; j++)
			n[j] = -n[j];
}
 


void normal (double x1, double y1, double z1, 
			 double x2, double y2, double z2,  
			 double x3, double y3, double z3,
			 double *n) {

	double a[3],b[3],c[3];
	a[0] = x1;
	a[1] = y1;
	a[2] = z1;
	b[0] = x2;
	b[1] = y2;
	b[2] = z2;
	c[0] = x3;
	c[1] = y3;
	c[2] = z3;
	getNorm (a, b, c, n);
 
}


void skyBox () {
	GLUquadricObj *quadObj; 
	quadObj = gluNewQuadric(); 
	gluQuadricDrawStyle(quadObj, GLU_FILL);
	static double s = 0,col = 1;
	static int y = 1;
	glPushMatrix();
	glEnable (GL_ALPHA_TEST);
	glEnable (GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	
	glColor4d(0,0,0,col);
	

	
	glRotated(s,1,0,0);
	glRotated(90,0,1,0);
	if (y == 1)
		s += 0.05;
	else 
		s +=0.05;
	glEnable(GL_TEXTURE_2D);
	
	if(y == 1)
		glBindTexture(GL_TEXTURE_2D, tex[0]); 
	else
		glBindTexture(GL_TEXTURE_2D, tex[8]); 
	
	gluQuadricTexture(quadObj, GL_TRUE);
	gluQuadricDrawStyle(quadObj, GLU_FILL); 
	glPushMatrix();
	gluSphere(quadObj, gr, 16, 16); 
	glPopMatrix();
	glPopMatrix();
	glDisable(GL_TEXTURE_2D);
	gluDeleteQuadric(quadObj);
	glDisable (GL_ALPHA_TEST);
	glDisable (GL_BLEND);
}



void init (void) {
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_COLOR_MATERIAL);
	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);
	
	gun = gluNewQuadric(); 
	gluQuadricDrawStyle(gun, GLU_FILL);
	
	torpedo = gluNewQuadric(); 
	gluQuadricDrawStyle(torpedo, GLU_FILL);

	genLists();

	glGenTextures(4, tex);	

	glBindTexture(GL_TEXTURE_2D, tex[0]);
	image1 = auxDIBImageLoad("../data/textures/sky.bmp"); 
	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
	glTexImage2D(GL_TEXTURE_2D, 0, 3, 
					image1->sizeX,
					image1->sizeY,
					0, GL_RGB, GL_UNSIGNED_BYTE,
					image1->data);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST); 
	glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_REPLACE);
	glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

	glBindTexture(GL_TEXTURE_2D, tex[1]);
	image2 = auxDIBImageLoad("../data/textures/water.bmp"); 
	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
	glTexImage2D(GL_TEXTURE_2D, 0, 3, 
					image2->sizeX,
					image2->sizeY,
					0, GL_RGB, GL_UNSIGNED_BYTE,
					image2->data);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST); 
	glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_REPLACE);
	glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);


	glBindTexture(GL_TEXTURE_2D, tex[2]);
	image3 = auxDIBImageLoad("../data/textures/ship.bmp"); 
	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
	glTexImage2D(GL_TEXTURE_2D, 0, 3, 
					image3->sizeX,
					image3->sizeY,
					0, GL_RGB, GL_UNSIGNED_BYTE,
					image3->data);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST); 
	glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_REPLACE);
	glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

	
	glBindTexture(GL_TEXTURE_2D, tex[3]);
	image4 = auxDIBImageLoad("../data/textures/particle.bmp"); 
	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
	glTexImage2D(GL_TEXTURE_2D, 0, 3, 
					image4->sizeX,
					image4->sizeY,
					0, GL_RGB, GL_UNSIGNED_BYTE,
					image4->data);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST); 
	glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_REPLACE);
	glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

	initFire();


}



void genLists (void) {

	shipList = glGenLists(1);
	
	//////////////////////////////////////////////////////////
	/// SHIP
	glNewList(shipList, GL_COMPILE);
		glColor4d(0.5, 0.5, 0.5, 1);
		glBegin(GL_QUADS);
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



/// система частиц
void drawFire () {

	glPushAttrib(GL_ALL_ATTRIB_BITS);
	glShadeModel(GL_SMOOTH);							// Enable Smooth Shading
	glEnable(GL_BLEND);									// Enable Blending
	glBlendFunc(GL_SRC_ALPHA,GL_ONE);					// Type Of Blending To Perform
	
	glBindTexture(GL_TEXTURE_2D, tex[3]);
	for (loop=0; loop < MAX_PARTICLES; loop++) 			/// Loop Through All The Particles
		if (particle[loop].active) {					/// If The Particle Is Active

			float x = particle[loop].x;					/// Grab Our Particle X Position
			float y = particle[loop].y;					/// Grab Our Particle Y Position
			float z = particle[loop].z;					/// Particle Z Pos + Zoom
			
			
			/// Draw The Particle Using Our RGB Values, Fade The Particle Based On It's Life
			glColor4f(particle[loop].r,particle[loop].g,particle[loop].b,particle[loop].life);

			glBegin(GL_TRIANGLE_STRIP);	/// Build Quad From A Triangle Strip
				glNormal3f(1, 0, 0);      
			    glTexCoord2d(1,1); glVertex3f(x+0.3f,y+0.3f,z); /// Top Right
				glTexCoord2d(0,1); glVertex3f(x-0.3f,y+0.3f,z); /// Top Left
				glTexCoord2d(1,0); glVertex3f(x+0.3f,y-0.3f,z); /// Bottom Right
				glTexCoord2d(0,0); glVertex3f(x-0.3f,y-0.3f,z); /// Bottom Left
			glEnd();										/// Done Building Triangle Strip

			particle[loop].x += particle[loop].xi/(slowdown*10);   /// Move On The X Axis By X Speed
			particle[loop].y += particle[loop].yi/(slowdown*10);    /// Move On The Y Axis By Y Speed
			particle[loop].z += particle[loop].zi/(slowdown*10);   /// Move On The Z Axis By Z Speed

			particle[loop].xi += particle[loop].xg;			/// Take Pull On X Axis Into Account
			particle[loop].yi += particle[loop].yg;			/// Take Pull On Y Axis Into Account
			particle[loop].zi += particle[loop].zg;			/// Take Pull On Z Axis Into Account
			particle[loop].life -= particle[loop].fade;		/// Reduce Particles Life By 'Fade'

			if (particle[loop].life < 0.0f) {					/// If Particle Is Burned Out
		
				particle[loop].life = 0.8f;					    /// Give It New Life
				particle[loop].fade = 10*(float(rand()%50)/1000.0f+0.003f);	/// Random Fade Value
				particle[loop].x = 0.0f;						/// Center On X Axis
				particle[loop].y = 0.0f;						/// Center On Y Axis
				particle[loop].z = 0.0f;						/// Center On Z Axis
				particle[loop].xi = 0;	                    /// X Axis Speed And Direction
				particle[loop].yi = 0;	                    /// Y Axis Speed And Direction
				particle[loop].zi = 0;	                    /// Z Axis Speed And Direction
				
				particle[loop].r = colors[color][0];		/// Select Red From Color Table
				particle[loop].g = colors[color][1];		/// Select Green From Color Table
				particle[loop].b = colors[color][2];		/// Select Blue From Color Table
			}
			
		}

	glPopAttrib();
    
}



void water (int N) {
	static float w_col = 0.7;
	static int y;
	double  vertx[3000], vertz[2];	
	double d;
	for ( int i = 0; i <= N; i++)
		vertx[i] =  -gr + 2*i*gr/N;

	vertz[0] = -gr;
	vertz[1] = gr;

	glPushMatrix();

	glRotated(90, 0.0, 1.0, 0.0);
	glColor4f(0.3f, 0.3f, 0.75f,w_col);


	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glEnable(GL_ALPHA_TEST);
	glEnable(GL_NORMALIZE);
	glEnable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, tex[1]);
	
	glBegin(GL_QUAD_STRIP);
	for ( i = 0; i < N; i++)
	{ 
		d = sin((vertx[i] + t/10)*2);
	
		glNormal3d(0,(-1/tan((vertx[i] + t/10)*2))/v_h,0);
		glTexCoord2d(i%15,55);	glVertex3d(vertx[i],d/v_h,vertz[0]);
		glTexCoord2d(55,i%15);	glVertex3d(vertx[i],d/v_h,vertz[1]);
	}
	glEnd();
	glDisable(GL_NORMALIZE);
	glDisable(GL_TEXTURE_2D);
	glDisable(GL_ALPHA_TEST);	
	glDisable(GL_BLEND);
	glPopMatrix();
}



void CALLBACK display (void) {

	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
	float ambient[4] = {0.5, 0.5, 0.5, 1};

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	glLightModelfv(GL_LIGHT_MODEL_AMBIENT, ambient);
	glLightfv(GL_LIGHT0, GL_POSITION, lightpos);
	
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

	water(1000);
	skyBox();

	if (!bShipSunk) {
		glPushMatrix();
			glTranslatef(shipX, -3,-shipZ);
			glCallList(shipList);
		glPopMatrix();

	}else if (shipY>=-10) {
		glPushMatrix();
			glTranslatef(shipX, shipY,-shipZ);
			glRotatef(50, 0, 0, 1);
			glCallList(shipList);

		glPopMatrix();

			
		glPushMatrix();
			glTranslatef(shipX, 0,-110);
			glRotatef(180, 0, 1, 0);	
			glScalef(5, 5, 5);
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
		float s=torpedoV*torpedoT;
		float angle=PI*torpedoAngle/180;
		torpedoZ=s*cos(angle);
		torpedoX=s*sin(angle);

		glPushMatrix();
			glTranslatef(-torpedoX, 0, -torpedoZ);
			glTranslatef(0, 0, 12);
			glRotatef(torpedoAngle, 0, 1, 0);
			glTranslatef(0, torpedoY, -8);
			gluCylinder(torpedo, 0.5, 0.5, 6, 15, 15);  
		glPopMatrix();

		if (torpedoT>=12) {			
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
		if (torpedoT>2) torpedoY=0;
	}

	if (!bShipSunk) {
		if (shipLDir) {
			shipX-=0.3;
			if (shipX<=-90) shipLDir=false;

		}else {
			shipX+=0.3;
			if (shipX>=90) shipLDir=true;
		}
	}else shipY-=0.2;


	if (time==40 || time==-40)
		ii *= -1;
	
	if (ii >0) time ++;
	else  time --;

	if (time == 40 || time == -40) 
		ii *= -1;
	
	if (ii >0) time ++;
	else time--;

	t++; 

	auxSwapBuffers();

}



void CALLBACK keyLeftProc (void) {
	if (gunAngle<45) gunAngle+=5;
	 
}

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



void CALLBACK keyRightProc (void) {
	if (gunAngle>-45) gunAngle-=5;
 
}

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
		bShipSunk=false;
	}
 
}

/// процедура работы с меню (вызывается при нажатии правой клавиши мыши)
void menuProc (int entry) {
	//commKeyProc((unsigned char) entry, 0, 0);

}

void main(int argc, char *argv[]) {
	float pos[4] = {0,20,-10,1};
	float dir[3] = {0,0,0};
	float ambient[4] = {0.5, 0.5, 0.5, 1};

	
	auxInitPosition( 50, 10, 600, 600);
	auxInitDisplayMode( AUX_RGB | AUX_DEPTH | AUX_DOUBLE | AUX_STENCIL );
	auxInitWindow("SeaBattle (ENTER-shoot; Space-new ship; ESQ-exit)");

	init();

	auxKeyFunc(AUX_LEFT, keyLeftProc);
    auxKeyFunc(AUX_RIGHT, keyRightProc);
	auxKeyFunc(AUX_RETURN, keyEnterProc);
	auxKeyFunc(AUX_SPACE, keySpaceProc);

	auxReshapeFunc(reshape);
	auxIdleFunc(display);

	sndPlaySound("../data/sounds/battle.wav", SND_ASYNC);
	
	auxMainLoop(display);
	
}
