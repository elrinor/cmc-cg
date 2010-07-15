#include<gl\glut.h>
#include<windows.h>
#include<math.h>
#include<stdio.h>
#include"BmpLoad.h"
#include"Vector3D.h"

#define PI 3.141592

#define KNOT_N 256
#define KNOT_M 32
#define KNOT_FACES KNOT_N*KNOT_M

int vp_height, vp_width;
int lasttime, time;
float sec;

float lx=0,ly=0,lz=1;
float px=3,py=3,pz=3;

GLuint texs[16];

GLUquadricObj *gluSphereQuadric;

struct Face {
  Vector3D v[4];
  Vector3D n[4];
};

Face Knot[KNOT_FACES];
int Order[KNOT_FACES];
float Dist[KNOT_FACES];

Vector3D GetKnotCenter(float phi)
{
  float r=0.8+0.3*cos(2.5*phi);
  float psi=PI/7*sin(2.5*phi);
  return Vector3D(r*sin(phi)*cos(psi),r*cos(phi)*cos(psi),r*sin(psi));
}

void GetKnotVertex(int i, int j, Vector3D& vertex, Vector3D& normal)
{
  float phi=4*PI*i/KNOT_N;
  float psi=2*PI*j/KNOT_M;
  Vector3D tangent=(GetKnotCenter(phi+PI/200)-GetKnotCenter(phi-PI/200)).normalize();
  Vector3D up(0,0,1);
  Vector3D side=up^tangent;
  normal=up*cos(psi)+side*sin(psi);
  vertex=GetKnotCenter(phi)+normal*0.1;
}

void GenKnot()
{
  int n=0;
  for(int i=0; i<KNOT_N; i++) for(int j=0; j<KNOT_M; j++)
  {
    GetKnotVertex(i  ,j  ,Knot[n].v[0],Knot[n].n[0]);
    GetKnotVertex(i  ,j+1,Knot[n].v[1],Knot[n].n[1]);
    GetKnotVertex(i+1,j+1,Knot[n].v[2],Knot[n].n[2]);
    GetKnotVertex(i+1,j  ,Knot[n].v[3],Knot[n].n[3]);
    n++;
  }
}

int cmp(const void* a, const void* b)
{
  if(Dist[*((int*)a)]>Dist[*((int*)b)])
    return -1;
  else if(Dist[*((int*)a)]<Dist[*((int*)b)])
    return 1;
  else
    return 0;
}

void SortKnot()
{
  for(int i=0; i<KNOT_FACES; i++)
  {
    Dist[i]=(Vector3D(px,py,pz)-Knot[i].v[0]).lengthSq();
    Order[i]=i;
  }
  qsort(&Order,KNOT_FACES,sizeof(int),cmp);
}

void Error(char* text)
{
  printf("%s\n",text);
  exit(0);
}

GLuint LoadTexture(char* filename)
{
  glPixelStorei(GL_UNPACK_ALIGNMENT,1);

  GLuint Texture;
  glGenTextures(1,&Texture);
	glBindTexture(GL_TEXTURE_2D,Texture);

	unsigned char *tex_bits;
	int tex_width;
	int tex_height;
	if((tex_bits = LoadTrueColorBMPFile(filename,&tex_width,&tex_height)) == NULL) 
			Error("Unable to read texture file.");
	// Загружаем текстуру
	glPixelStorei(GL_UNPACK_ALIGNMENT,1);
	gluBuild2DMipmaps(GL_TEXTURE_2D,   
		              3,                     // Формат текстуры
		              tex_width,tex_height,  // Размер текстуры
		              GL_RGB,                // Формат исходных данных
		              GL_UNSIGNED_BYTE,      // Тип исходных данных
  			          tex_bits);             // Указатель на исходные данные */
	delete[] tex_bits;
  // Устанавливаем параметры текстуры
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
	// Включаем перспективное текстурирования
	glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);

  return Texture;
}

void Init()
{
  gluSphereQuadric=gluNewQuadric();
  gluQuadricTexture(gluSphereQuadric,GL_TRUE);
  GenKnot();

  lasttime=timeGetTime();

  glEnable(GL_DEPTH_TEST);

  texs[0]=LoadTexture("floor.bmp");
  texs[1]=LoadTexture("tile.bmp");
	
  glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
  glEnable(GL_TEXTURE_2D);

  glEnable(GL_POINT_SMOOTH);
  glEnable(GL_NORMALIZE);


	// Устанавливаем материал
	GLfloat m_emissive[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
	GLfloat m_diffuse[4] = { 0.4f, 0.4f, 0.4f, 0.3f };
	GLfloat m_specular[4] = { 0.6f, 0.6f, 0.6f, 0.0f };
	GLfloat m_ambient[4] = { 0.3f, 0.3f, 0.3f, 0.0f };
	glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, m_emissive);
	glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);
	glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, m_specular);
	glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, m_ambient);
	glMaterialf(GL_FRONT_AND_BACK,GL_SHININESS,10);
	// Устанавливаем цвет
	GLfloat l_diffuse[4] = { 1.0f, 1.0f, 1.0f, 0.0f };
	GLfloat l_specular[4] = { 1.0f, 1.0f, 1.0f, 0.0f };
	GLfloat l_ambient[4] = { 1.0f, 1.0f, 1.0f, 0.0f };
	glLightfv(GL_LIGHT0,GL_DIFFUSE,l_diffuse);
	glLightfv(GL_LIGHT0,GL_SPECULAR,l_specular);
	glLightfv(GL_LIGHT0,GL_AMBIENT,l_ambient);
	// Включаем освещение
	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);
	glShadeModel(GL_SMOOTH);
}

void Idle()
{
	glutPostRedisplay();
}

void Objects()
{
  glMatrixMode(GL_MODELVIEW);

  // Сфера
  glBindTexture(GL_TEXTURE_2D,texs[0]);
  glPushMatrix();
  glTranslatef(1.5*sin(sec),1.5*cos(sec),1+0.5*sin(2*sec));
  gluSphere(gluSphereQuadric,0.3,16,16);
  glPopMatrix();

  // Ромбик
  glBindTexture(GL_TEXTURE_2D,texs[1]);
  glPushMatrix();
  glTranslatef(0,0,0.7+0.5*cos(2.5*sec));
  glRotatef(sec*100,0,0,1);
  glRotatef(sec*100,1,0,0);
  glBegin(GL_TRIANGLE_FAN);
    glNormal3f(0,0,1);glTexCoord2f(0.5,0.5);glVertex3f( 0, 0, 0.15);
    glNormal3f( 0.4, 0.4,1);glTexCoord2f(0,0);glVertex3f( 0.3+0.1*sin(sec), 0.3+0.1*sin(sec),0);
    glNormal3f(-0.4, 0.4,1);glTexCoord2f(0,1);glVertex3f(-0.3-0.1*cos(sec), 0.3+0.1*cos(sec),0);
    glNormal3f(-0.4,-0.4,1);glTexCoord2f(1,1);glVertex3f(-0.3-0.1*sin(sec),-0.3-0.1*sin(sec),0);
    glNormal3f( 0.4,-0.4,1);glTexCoord2f(1,0);glVertex3f( 0.3+0.1*cos(sec),-0.3-0.1*cos(sec),0);
    glNormal3f( 0.4, 0.4,1);glTexCoord2f(0,0);glVertex3f( 0.3+0.1*sin(sec), 0.3+0.1*sin(sec),0);
  glEnd();
  glBegin(GL_TRIANGLE_FAN);
    glNormal3f(0,0,-1);glTexCoord2f(0.5,0.5);glVertex3f( 0, 0, -0.15);
    glNormal3f( 0.4, 0.4,-1);glTexCoord2f(0,0);glVertex3f( 0.3+0.1*sin(sec), 0.3+0.1*sin(sec),0);
    glNormal3f(-0.4, 0.4,-1);glTexCoord2f(0,1);glVertex3f(-0.3-0.1*cos(sec), 0.3+0.1*cos(sec),0);
    glNormal3f(-0.4,-0.4,-1);glTexCoord2f(1,1);glVertex3f(-0.3-0.1*sin(sec),-0.3-0.1*sin(sec),0);
    glNormal3f( 0.4,-0.4,-1);glTexCoord2f(1,0);glVertex3f( 0.3+0.1*cos(sec),-0.3-0.1*cos(sec),0);
    glNormal3f( 0.4, 0.4,-1);glTexCoord2f(0,0);glVertex3f( 0.3+0.1*sin(sec), 0.3+0.1*sin(sec),0);
  glEnd();
  glPopMatrix();
}

void DrawKnot()
{
  // Узел
	GLfloat m_diffuse[4] = { 0.4f, 0.4f, 0.4f, 0.3f };
	glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);

  glEnable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
  glBegin(GL_QUADS);
  for(int i=0; i<KNOT_FACES; i++)
  {
    glNormal3f(Knot[Order[i]].n[0].x,Knot[Order[i]].n[0].y,Knot[Order[i]].n[0].z);
    glVertex3f(Knot[Order[i]].v[0].x,Knot[Order[i]].v[0].y,Knot[Order[i]].v[0].z);
    glNormal3f(Knot[Order[i]].n[1].x,Knot[Order[i]].n[1].y,Knot[Order[i]].n[1].z);
    glVertex3f(Knot[Order[i]].v[1].x,Knot[Order[i]].v[1].y,Knot[Order[i]].v[1].z);
    glNormal3f(Knot[Order[i]].n[2].x,Knot[Order[i]].n[2].y,Knot[Order[i]].n[2].z);
    glVertex3f(Knot[Order[i]].v[2].x,Knot[Order[i]].v[2].y,Knot[Order[i]].v[2].z);
    glNormal3f(Knot[Order[i]].n[3].x,Knot[Order[i]].n[3].y,Knot[Order[i]].n[3].z);
    glVertex3f(Knot[Order[i]].v[3].x,Knot[Order[i]].v[3].y,Knot[Order[i]].v[3].z);
  }
  glEnd();
  glEnable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);
}

void DrawLight()
{
  // Источник света
  glPointSize(5);
  glDisable(GL_LIGHTING);
  glDisable(GL_TEXTURE_2D);
  glColor4f(1,1,1,1);
  glBegin(GL_POINTS);
    glVertex3f(lx,ly,lz);
  glEnd();
  glEnable(GL_LIGHTING);
  glEnable(GL_TEXTURE_2D);
}

void DrawFloor()
{
  glBindTexture(GL_TEXTURE_2D,texs[0]);
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glRotatef(sec*10,0,0,1);
  glBegin(GL_TRIANGLE_FAN);
    glNormal3f(0,0,1);
    glTexCoord2f(0,0);glVertex3f(0,0,0);
    for(int i=0; i<=360; i++)
    {
      float phi=(float)i/360*2*PI;
      float r=2+0.5*sin(7*phi);
      float x=r*cos(phi);
      float y=r*sin(phi);
      glTexCoord2f(x,y);
      glVertex3f(x,y,0);
    }
  glEnd();
  glPopMatrix();
}

void display()
{

  lz=2.0+0.3*sin(sec);
  ly=sin(2*sec);
  lx=cos(4*sec);

  px=3*sin(0.3*sec);
  py=3*cos(0.3*sec);
  pz=3.5+sin(0.5*sec);

  glClearColor(0.1,0.08,0.06,0.0);
	glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_STENCIL_BUFFER_BIT);

  time=time+timeGetTime()-lasttime;
  lasttime=timeGetTime();
  sec=(float)time/1000;

  glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
  gluLookAt(px,py,pz,0,0,0,0,0,1);
  
	// Устанавливаем источник освещения
	GLfloat l_position[4] = { lx, ly, lz, 1 };
	glLightfv(GL_LIGHT0,GL_POSITION,l_position);

  // Пол Stencil
  glColorMask(GL_FALSE,GL_FALSE,GL_FALSE,GL_FALSE);
  glDepthMask(GL_FALSE);
  glEnable(GL_STENCIL_TEST);
  glStencilFunc(GL_ALWAYS,1,1);
	glStencilOp(GL_KEEP,GL_KEEP,GL_REPLACE);
  DrawFloor();
  glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
  glDepthMask(GL_TRUE);

  //Отражение
  glStencilFunc(GL_EQUAL,1,1);
	glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);
  glPushMatrix();
  glScalef(1,1,-1);
	glLightfv(GL_LIGHT0,GL_POSITION,l_position);
  Objects();
  DrawLight();
  pz-=1.1;pz*=-1;SortKnot();pz+=1.1;pz*=-1;
  glTranslatef(0,0,1.1);
  DrawKnot();
  glPopMatrix();
  glDisable(GL_STENCIL_TEST);


	glLightfv(GL_LIGHT0,GL_POSITION,l_position);

  // Пол
	GLfloat m_diffuse[4] = { 0.4f, 0.4f, 0.4f, 0.6f };
	glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);
  glEnable(GL_BLEND);
  DrawFloor();
  glDisable(GL_BLEND);

  // Фигуры
  Objects();
  DrawLight();
  glPushMatrix();
  pz-=1.1;SortKnot();pz+=1.1;
  glTranslatef(0,0,1.1);
  DrawKnot();
  glPopMatrix();

  // Тени
  glEnable(GL_POLYGON_OFFSET_FILL);
  glPolygonOffset(-4,-4);
  glEnable(GL_STENCIL_TEST);
  glStencilFunc(GL_EQUAL,1,1);
	glStencilOp(GL_KEEP,GL_INCR,GL_INCR);
	glDisable(GL_LIGHTING);
  glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
  glColor4f(0,0,0,0.4);
  glPushMatrix();
  GLfloat Projection[16]={lz,0,0,0,   0,lz,0,0,   -lx,-ly,0,-1,   0,0,0,lz};
  glMultMatrixf(Projection);
  Objects();
  glColor4f(0,0,0,0.2);
  glPushMatrix();
  pz-=1.1;SortKnot();pz+=1.1;
  glTranslatef(0,0,1.1);
  DrawKnot();
  glPopMatrix();
  glPopMatrix();
  glDisable(GL_BLEND);
	glEnable(GL_LIGHTING); 

  glDisable(GL_STENCIL_TEST);
  glDisable(GL_POLYGON_OFFSET_FILL);

  glFlush();
  glutSwapBuffers();
}

void keyboard(unsigned char key, int x, int y)
{
  if(key=='\033')
    exit(0);
}

void Reshape(int w, int h)
{
	vp_width = w;
	vp_height = h;
	glViewport(0,0,w,h);
	if(h > 0) {
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		gluPerspective(60.0,w/(GLdouble)h,0.1,45);
	}
}

int main(int argc, char** argv)
{
  glutInit(&argc,argv);
	glutInitDisplayMode(GLUT_RGB|GLUT_DOUBLE|GLUT_STENCIL);
	glutInitWindowSize(640,480);
	glutCreateWindow("OpenGL");

	Init();
	glutDisplayFunc(display);
	glutReshapeFunc(Reshape);
	glutKeyboardFunc(keyboard);
	glutIdleFunc(Idle);

  DEVMODE dmSS;        
  memset(&dmSS,0,sizeof(dmSS));
  dmSS.dmSize=sizeof(dmSS);  
  dmSS.dmPelsWidth=640;    
  dmSS.dmPelsHeight=480;    
  dmSS.dmBitsPerPel=32;      
  dmSS.dmFields=DM_BITSPERPEL|DM_PELSWIDTH|DM_PELSHEIGHT;
  ChangeDisplaySettings(&dmSS,CDS_FULLSCREEN);
  glutFullScreen(); 
  
  glutMainLoop();


  return 0;
}