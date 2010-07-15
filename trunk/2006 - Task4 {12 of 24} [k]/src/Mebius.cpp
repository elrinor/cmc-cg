#include <gl\glut.h>
#include <windows.h>
#include <math.h>
#include "loadbmp.h"

#define PI 3.1415926
#define sqr(x) ((x)*(x))
#define eps 0.0001

class	Vector
{
public:
  union{
    struct{
      float	x,y,z;
    };
    float a[3];
  };
	Vector() {}
  Vector(float X,float Y,float Z) {x=X;y=Y;z=Z;}
  Vector(const Vector& b) {x=b.x;y=b.y;z=b.z;}
	Vector& operator=(const Vector& b) {x=b.x;y=b.y;z=b.z;return *this;}
  Vector operator+()const {return *this;}
  Vector operator-()const {return Vector(-x,-y,-z);}
  Vector& operator+=(const Vector& b) {x+=b.x;y+=b.y;z+=b.z;return *this;}
  Vector& operator-=(const Vector& b) {x-=b.x;y-=b.y;z-=b.z;return *this;}
  Vector& operator*=(float f) {x*=f;y*=f;z*=f;return *this;}
  Vector& operator/=(float f) {x/=f;y/=f;z/=f;return *this;}
  float	length()const {return (float)sqrt(x*x+y*y+z*z);}
	Vector&	norm() {return (*this)/=length();}
	friend Vector operator+(const Vector&,const Vector&);
	friend Vector operator-(const Vector&,const Vector&);
	friend Vector operator*(float,        const Vector&);
	friend Vector operator*(const Vector&,float);
	friend Vector operator/(const Vector&,float);
	friend float  operator*(const Vector&,const Vector&);
	friend Vector operator^(const Vector&,const Vector&);
};
inline Vector operator+(const Vector& a,const Vector& b) {return Vector(a.x+b.x,a.y+b.y,a.z+b.z);}
inline Vector operator-(const Vector& a,const Vector& b) {return Vector(a.x-b.x,a.y-b.y,a.z-b.z);}
inline Vector operator*(const Vector& b,float a) {return Vector(b.x*a,b.y*a,b.z*a);}
inline Vector operator*(float a,const Vector& b) {return Vector(b.x*a,b.y*a,b.z*a);}
inline Vector operator/(const Vector& b,float a) {return Vector(b.x/a,b.y/a,b.z/a);}
inline float operator*(const Vector& a,const Vector& b) {return a.x*b.x+a.y*b.y+a.z*b.z;}
inline Vector operator^(const Vector& a,const Vector& b) {return Vector (a.y*b.z-a.z*b.y,a.z*b.x-a.x*b.z,a.x*b.y-a.y*b.x);}


int w,h;

bool ongoing=true;

GLUquadricObj *quadric;

const int CU=120;
const int CV=30;
const Vector Camera(5,0,0);

Vector Vertexs[CU+1][CV+1];
Vector Normals[CU+1][CV+1];
Vector Vertexs2[CU+1][CV+1];
Vector Normals2[CU+1][CV+1];
Vector Vertexs3[CU+1][CV+1];
Vector Normals3[CU+1][CV+1];

float m[16];

int state=1;

GLuint texture;

void OnDraw()
{
  static int LastTime=timeGetTime();
  static float dt, t=0;
  dt=(GetTickCount()-LastTime)/1000.0;
  LastTime=GetTickCount();
  dt=min(dt,0.1);
  if(ongoing)
    t+=dt;

  glClear(GL_DEPTH_BUFFER_BIT|GL_COLOR_BUFFER_BIT);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(Camera.x,Camera.y,Camera.z,0,0,0,0,0,1);
  
  glPushMatrix();
  glTranslatef(1,sin(t),cos(t));
  glColor3f(1,1,1);
  glDisable(GL_LIGHTING);
  gluSphere(quadric,0.03,16,16);
  glEnable(GL_LIGHTING);
  glPopMatrix();

  GLfloat l_p[4]={1,sin(t),cos(t),1};
  glLightfv(GL_LIGHT0,GL_POSITION,l_p);
  
  if(state==1)
  {
    glPushMatrix();
    glMultMatrixf(m);
    glBegin(GL_QUADS);
    for(int i=0; i<CU; i++) for(int j=0; j<CV; j++)
    {
      glNormal3fv(Normals[i  ][j  ].a);
      glTexCoord2f(3.0*(i  )/CU,1.0*(j  )/CV);
      glVertex3fv(Vertexs[i  ][j  ].a);
      glNormal3fv(Normals[i+1][j  ].a);
      glTexCoord2f(3.0*(i+1)/CU,1.0*(j  )/CV);
      glVertex3fv(Vertexs[i+1][j  ].a);
      glNormal3fv(Normals[i+1][j+1].a);
      glTexCoord2f(3.0*(i+1)/CU,1.0*(j+1)/CV);
      glVertex3fv(Vertexs[i+1][j+1].a);
      glNormal3fv(Normals[i  ][j+1].a);
      glTexCoord2f(3.0*(i  )/CU,1.0*(j+1)/CV);
      glVertex3fv(Vertexs[i  ][j+1].a);
    }
    glEnd();
    glPopMatrix();
  }
  if(state==2)
  {
    glPushMatrix();
    glMultMatrixf(m);
    glBegin(GL_QUADS);
    for(int i=0; i<CU; i++) for(int j=0; j<CV; j++)
    {
      glNormal3fv(Normals2[i  ][j  ].a);
      glTexCoord2f(3.0*(i  )/CU,1.0*(j  )/CV);
      glVertex3fv(Vertexs2[i  ][j  ].a);
      glNormal3fv(Normals2[i+1][j  ].a);
      glTexCoord2f(3.0*(i+1)/CU,1.0*(j  )/CV);
      glVertex3fv(Vertexs2[i+1][j  ].a);
      glNormal3fv(Normals2[i+1][j+1].a);
      glTexCoord2f(3.0*(i+1)/CU,1.0*(j+1)/CV);
      glVertex3fv(Vertexs2[i+1][j+1].a);
      glNormal3fv(Normals2[i  ][j+1].a);
      glTexCoord2f(3.0*(i  )/CU,1.0*(j+1)/CV);
      glVertex3fv(Vertexs2[i  ][j+1].a);
    }
    glEnd();
    glPopMatrix();
  }
  if(state==3)
  {
    glPushMatrix();
    glMultMatrixf(m);
    glBegin(GL_QUADS);
    for(int i=0; i<CU; i++) for(int j=0; j<CV; j++)
    {
      glNormal3fv(Normals3[i  ][j  ].a);
      glTexCoord2f(3.0*(i  )/CU,1.0*(j  )/CV);
      glVertex3fv(Vertexs3[i  ][j  ].a);
      glNormal3fv(Normals3[i+1][j  ].a);
      glTexCoord2f(3.0*(i+1)/CU,1.0*(j  )/CV);
      glVertex3fv(Vertexs3[i+1][j  ].a);
      glNormal3fv(Normals3[i+1][j+1].a);
      glTexCoord2f(3.0*(i+1)/CU,1.0*(j+1)/CV);
      glVertex3fv(Vertexs3[i+1][j+1].a);
      glNormal3fv(Normals3[i  ][j+1].a);
      glTexCoord2f(3.0*(i  )/CU,1.0*(j+1)/CV);
      glVertex3fv(Vertexs3[i  ][j+1].a);
    }
    glEnd();
    glPopMatrix();
  }
  glutSwapBuffers(); 
}

void OnIdle()
{
  glutPostRedisplay();
}

void OnReshape(int width, int height)
{
  glViewport(0,0,width,height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
	gluPerspective(45,1.0*width/height,1,10);
  w=width;
  h=height;
}

void OnKeyDown(unsigned char key, int x, int y)
{
  if(key==27)
    exit(0);
  if(key==' ')
    ongoing=!ongoing;
  if(key=='1')
    state=1;
  if(key=='2')
    state=2;
  if(key=='3')
    state=3;
}

void OnMouseMove(int x, int y)
{
  if(!(x==w/2 && y==h/2))
  {
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glRotatef(x-w/2, 0,0,1);
    glRotatef(y-h/2, 0,1,0);
    glMultMatrixf(m);
    glGetFloatv(GL_MODELVIEW_MATRIX, m);
    glutWarpPointer(w/2,h/2);
  }
}

Vector MebiusVertex(float u,float v)
{
   return Vector((1+v/2*cos(u/2))*cos(u),(1+v/2*cos(u/2))*sin(u),v/2*sin(u/2));
}

Vector KleinVertex(float u,float v)
{
  if(u>PI) return Vector((6*cos(u)*(1+sin(u))-4*(1-cos(u)/2)*cos(v))*0.07,(4*(1-cos(u)/2)*sin(v))*0.07,(-16*sin(u))*0.07);
  else     return Vector((6*cos(u)*(1+sin(u))+4*(1-cos(u)/2)*cos(u)*cos(v))*0.07,(4*(1-cos(u)/2)*sin(v))*0.07,(-16*sin(u)-4*(1-cos(u)/4)*sin(u)*cos(v))*0.07);
}

Vector Klein2Vertex(float u,float v)
{
   return Vector(((1+cos(u/2)*sin(v)-sin(u/2)*sin(2*v))*cos(u))/2,((1+cos(u/2)*sin(v)-sin(u/2)*sin(2*v))*sin(u))/2,(sin(u/2)*sin(v)+cos(u/2)*sin(2*v))/2);
}

int main(int argc, char** argv)
{
  srand(GetTickCount());

  glutInit(&argc,argv);
  glutInitDisplayMode(GLUT_DOUBLE|GLUT_RGB|GLUT_DEPTH);
  glutCreateWindow("Mebius&Klein");
  glutReshapeFunc(OnReshape);
  glutIdleFunc(OnIdle);
  glutDisplayFunc(OnDraw);
  glutPassiveMotionFunc(OnMouseMove);
  glutKeyboardFunc(OnKeyDown);
  glutSetCursor(GLUT_CURSOR_NONE);

  if(MessageBox(NULL,"Fullscreen?", "FullScreen?",MB_ICONQUESTION|MB_YESNO)==IDYES)
  {
		glutGameModeString("800x600:32");
    glutFullScreen();  
  }

  quadric=gluNewQuadric();

  glClearColor(0,0,0,0);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_TEXTURE_2D);
  glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
  glLightModelf(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);
	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);
	glShadeModel(GL_SMOOTH);

  GLfloat l_d[4]={1.0f,1.0f,1.0f,0.0f};
	GLfloat l_s[4]={1.0f,1.0f,1.0f,0.0f};
	GLfloat l_a[4]={1.0f,1.0f,1.0f,0.0f};
	glLightfv(GL_LIGHT0,GL_DIFFUSE,l_d);
	glLightfv(GL_LIGHT0,GL_SPECULAR,l_s);
	glLightfv(GL_LIGHT0,GL_AMBIENT,l_a);
  GLfloat m_e[4]={0.05f,0.05f,0.0f,0.0f };
	GLfloat m_s[4]={0.2f,0.5f,0.8f,0.0f };
	GLfloat m_a[4]={0.4f,0.3f,0.2f,0.0f };
	GLfloat m_d[4]={0.5f,0.6f,0.3f,0.5f };
	glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT,m_a);
	glMaterialfv(GL_FRONT_AND_BACK,GL_SPECULAR,m_s);
	glMaterialfv(GL_FRONT_AND_BACK,GL_DIFFUSE,m_d);
  glMaterialfv(GL_FRONT_AND_BACK,GL_EMISSION,m_e);
	glMaterialf(GL_FRONT_AND_BACK,GL_SHININESS,10);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glGetFloatv(GL_MODELVIEW_MATRIX, m);

  IMAGE image;
  glPixelStorei(GL_UNPACK_ALIGNMENT,1);
  glGenTextures(1,&texture);
  glBindTexture(GL_TEXTURE_2D,texture);
  LoadBMP("texture.bmp", &image);
  gluBuild2DMipmaps(GL_TEXTURE_2D,3,image.width,image.height,GL_RGB,GL_UNSIGNED_BYTE,image.data);             
  free(image.data);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_REPEAT);

  for(int i=0;i<=CU;i++) for(int j=0;j<=CV;j++)
  {
    float u=2*PI*i/CU;
    float v=-1.0+2.0*j/CV;
    Vertexs[i][j]=MebiusVertex(u,v);
    Vector du=(MebiusVertex(u+eps,v)-MebiusVertex(u-eps,v)).norm();
    Vector dv=(MebiusVertex(u,v+eps)-MebiusVertex(u,v-eps)).norm();
    Normals[i][j]=du^dv;
  }
  
  for(int i=0;i<=CU;i++) for(int j=0;j<=CV;j++)
  {
    float u=2*PI*i/CU;
    float v=2*PI*j/CV;
    Vertexs2[i][j]=KleinVertex(u,v);
    Vector du=(KleinVertex(u+eps,v)-KleinVertex(u-eps,v)).norm();
    Vector dv=(KleinVertex(u,v+eps)-KleinVertex(u,v-eps)).norm();
    Normals2[i][j]=du^dv;
  }

  for(int i=0;i<=CU;i++) for(int j=0;j<=CV;j++)
  {
    float u=2*PI*i/CU;
    float v=2*PI*j/CV;
    Vertexs3[i][j]=Klein2Vertex(u,v);
    Vector du=(Klein2Vertex(u+eps,v)-Klein2Vertex(u-eps,v)).norm();
    Vector dv=(Klein2Vertex(u,v+eps)-Klein2Vertex(u,v-eps)).norm();
    Normals3[i][j]=du^dv;
  }

  glutMainLoop();
  return 0;
}
