#include <stdio.h>
#include <gl\glut.h>
#include <windows.h>

#define N 10000

typedef struct
{
	float x,y;
	float x1,y1;
	float randomx,randomy;
	float x2,y2;
	float r,g,b;
	float s;
	float s1;
	float l;
	float l1;
	bool living;
	bool update;
} TParticle;

TParticle System[N];
int SystemType;
float t, tt;
int w_x, w_y;
bool time_on=true;

float kuchki[801];

void reinit_particle(int n, int type)
{
	if(type==0) // snow
	{
		System[n].x=800.0*rand()/RAND_MAX;
		System[n].y=600;
		System[n].x1=0;
		System[n].y1=-300.0*rand()/RAND_MAX;
		System[n].randomx=80.0*rand()/RAND_MAX;
		System[n].randomy=0;
		System[n].x2=0;
		System[n].y2=0;
		System[n].b=0.5+0.5*rand()/RAND_MAX;
		System[n].r=System[n].b/2;
		System[n].g=System[n].b/2;
		System[n].s=1+8*rand()/RAND_MAX;
		System[n].s1=-0.3-3.0*rand()/RAND_MAX;
		System[n].l=1*rand()/RAND_MAX;
		System[n].l1=System[n].s1/System[n].s+2.0*rand()/RAND_MAX;
		System[n].update=true;
		System[n].living=true;
	}
	else if(type==1) //waterfall
	{
		System[n].x=0;
		System[n].y=600;
		System[n].x1=300+200*rand()/RAND_MAX;
		System[n].y1=-60-300*rand()/RAND_MAX;
		System[n].randomx=150*rand()/RAND_MAX;
		System[n].randomy=150*rand()/RAND_MAX;
		System[n].x2=0;
		System[n].y2=-600;
		System[n].b=(float)rand()/RAND_MAX;
		System[n].r=System[n].b/4;
		System[n].g=System[n].b/4;
		System[n].l=1+6.0*rand()/RAND_MAX;
		System[n].l1=-1;
		System[n].s=1+6*rand()/RAND_MAX;
		System[n].s1=0;
		System[n].update=true;
		System[n].living=true;
	}
}

void move_system()
{
	for(int i=0;i<N;i++)
	{
		if(!System[i].living)
		{
			reinit_particle(i,SystemType);
		}
		else
		{
			if(System[i].update)
			{
				System[i].x+=tt*(System[i].x1+(1.0-2.0*rand()/RAND_MAX)*System[i].randomx);
				System[i].y+=tt*(System[i].y1+(1.0-2.0*rand()/RAND_MAX)*System[i].randomy);
				System[i].x1+=tt*System[i].x2;
				System[i].y1+=tt*System[i].y2;
				System[i].l+=tt*System[i].l1;
				System[i].s+=tt*System[i].s1;
				if(System[i].s<0.5 || System[i].l<0)
					System[i].living=false;
			}
		}
	}
}

void system_special_handler()
{
	if(SystemType==1)
		{
			for(int i=0; i<N; i++)
			{
				if(System[i].living && System[i].y<0)
				{
					System[i].y=0.001;
					if(i%5==0)
					{
						System[i].s/=2;
						System[i].y1*=-0.3;
						System[i].x1*=1-2.0*rand()/RAND_MAX;					
					}
					else
					{
						System[i].y1=0;
						System[i].y2=0;
						System[i].randomy=0;
						System[i].randomx=0;
						System[i].x1*=0.8;
						if(rand()>RAND_MAX/2)
							System[i].x1*=-1;
					}
				}
			}
		}
		else if(SystemType==0)
		{
			for(int i=0;i<801;i++)
			{
				kuchki[i]=max(0,kuchki[i]-2*tt);
				if(i!=0 && i!=800)
					kuchki[i]=(kuchki[i]+kuchki[i+1]+kuchki[i-1])/3;
			}
			for(int i=0;i<N;i++)
			{
				if(System[i].update)
				{
					if(System[i].x>0 && System[i].x<800)
					{
						int index=max(0,min(800,(int)System[i].x));
						if(System[i].y<=kuchki[(int)System[i].x])
						{
							if(System[i].s<2)
								System[i].living=false;
							else
							{
								System[i].update=false;
								int jmin=max(0,System[i].x-System[i].s*0.25);
								int jmax=min(800,System[i].x+System[i].s*0.25);
								for(int j=jmin;j<jmax;j++)
									kuchki[j]+=System[i].s*0.2;
							}
						}
					}
				}
				else
				{
					System[i].y-=2*tt;
					if(System[i].y<-3)
						System[i].living=false;
				}
			}
		}
}

void reinit_system(int type)
{
	SystemType=type;
	for(int i=0; i<N; i++)
		System[i].living=false;
	for(int i=0;i<=800;i++)
		kuchki[i]=0;
	tt=1/30.0;
	for(int i=0; i<120; i++)
	{
		move_system();
		system_special_handler();
	}
}

void draw_system()
{
	for(int i=0; i<N; i++)
	{
		if(System[i].living)
		{
			glColor3f(System[i].r,System[i].g,System[i].b);
			glPointSize(System[i].s);
			glBegin(GL_POINTS);
				glVertex3f(System[i].x,System[i].y,0);
			glEnd();
		}
	}
}

void do_display()
{
	glClear(GL_COLOR_BUFFER_BIT);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(0,800,0,600,0.1,100);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	gluLookAt(0,0,1,0,0,0,0,1,0);

	if(time_on)
	{
		move_system();
		system_special_handler();
	}

	draw_system();

	glutSwapBuffers();
}

void do_idle()
{
	static int prevt=timeGetTime();
	tt=(timeGetTime()-prevt)/1000.0;
	t+=tt;
	if(tt>1.0/70.0)
	{
		if(tt>0.05)
			tt=0.05;
		prevt=timeGetTime();
		glutPostRedisplay();
	}
}


void do_init()
{
	srand(timeGetTime());
  glClearColor(0,0,0,0);
  glEnable(GL_DEPTH_TEST);
  glDisable(GL_CULL_FACE);
  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
  glShadeModel(GL_SMOOTH);
  glHint(GL_POINT_SMOOTH_HINT,GL_NICEST);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
}

void do_mouse(int button, int state, int x, int y)
{
	static int n=0;
	n=(n+1)%N;
	reinit_particle(n,SystemType);
	System[n].x=x;
	System[n].y=600-y;
}

void do_keydown(unsigned char key, int x, int y)
{
  if(key==27)
    exit(0);
	if(key==' ')
		time_on=!time_on;
	if(key=='w' || key=='W')
		reinit_system(1);
	if(key=='s' || key=='S')
		reinit_system(0);
}

void do_reshape(int x, int y)
{
	w_x=x;
	w_y=y;
	glViewport(0,0,w_x,w_y);
}

int main(int argc, char** argv)
{
  glutInit(&argc,argv);
  glutInitDisplayMode(GLUT_DOUBLE|GLUT_RGB);
	glutInitWindowSize(800,600);
  glutCreateWindow("Particle System");

	DEVMODE devmode;        
  memset(&devmode,0,sizeof(devmode));
  devmode.dmSize=sizeof(devmode);  
  devmode.dmPelsWidth=800;    
  devmode.dmPelsHeight=600;    
  devmode.dmBitsPerPel=32;      
  devmode.dmFields=DM_BITSPERPEL|DM_PELSWIDTH|DM_PELSHEIGHT;
  ChangeDisplaySettings(&devmode,CDS_FULLSCREEN);
  glutFullScreen();  
 
	glutReshapeFunc(do_reshape);
  glutIdleFunc(do_idle);
  glutDisplayFunc(do_display);
  glutMouseFunc(do_mouse);
  glutKeyboardFunc(do_keydown);
 
	srand(GetTickCount());
  
  glClearColor(0,0,0,0);
  glHint(GL_POINT_SMOOTH_HINT,GL_NICEST); 
  glEnable(GL_BLEND); 
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
	glEnable(GL_POINT_SMOOTH); 
  
	reinit_system(0);

  glutMainLoop();
  return 0;
}
