#include <Windows.h>
#include <gl/glut.h>
#include <gl/GLU.h>
#include <gl/GLAux.h>
#include <gl/glext.h>
#include <gl/GL.h>
#include <math.h>

#define RADIUS 8

#define ROOM 1
#define KNOT 2

GLuint floorMap, ceilMap, wallMap, marbleMap;

float time;

float startTime = GetTickCount() / 1000.0f;

float cx, cy, cz; // camera coord
float lx, ly, lz; // light coord

float cphi, cpsi; // camera position angles

int height, width;

struct Vector 
{
public:
  float x, y, z;
  Vector(float x, float y, float z): x(x), y(y), z(z) 
  {
    return;
  }
  Vector()
  {
    return;
  }
};

Vector operator+ (const Vector& v1, const Vector& v2)
{
  Vector result;
  result.x = v1.x + v2.x;
  result.y = v1.y + v2.y;
  result.z = v1.z + v2.z;
  return result;
}

Vector operator- (const Vector& v1, const Vector& v2)
{
  Vector result;
  result.x = v1.x - v2.x;
  result.y = v1.y - v2.y;
  result.z = v1.z - v2.z;
  return result;
}

Vector operator^ (const Vector& v1, const Vector& v2)
{
  Vector result;
  result.x =   v1.y * v2.z - v1.z * v2.y;
  result.y = - v1.x * v2.z + v1.z * v2.x;
  result.z =   v1.x * v2.y - v1.y * v2.x;
  return result;
}

Vector operator* (const Vector& v1, const float b)
{
  Vector result;
  result.x = v1.x * b;
  result.y = v1.y * b;
  result.z = v1.z * b;
  return result;
}

Vector operator* (const float b, const Vector& v1) 
{
  return v1 * b;
}

Vector norm(const Vector& v) {
  float b = 1 / sqrt(v.x * v.x + v.y * v.y + v.z * v.z);
  return v * b;
}

void drawMultiQuad(float x0, float y0, float z0, float n0, float n1, float dx0, float dy0, float dz0, float dx1, float dy1, float dz1, float nx, float ny, float nz, float txk, float tyk) 
{
  glBegin(GL_QUADS);
  glNormal3f(nx, ny, nz);
  for(int i = 0; i < n0; i++) 
  {
    for(int j = 0; j < n1; j++) 
    {
      float x00 = x0 + i * dx0 + j * dx1;
      float y00 = y0 + i * dy0 + j * dy1;
      float z00 = z0 + i * dz0 + j * dz1;

      float x01 = x0 + i * dx0 + (j + 1) * dx1;
      float y01 = y0 + i * dy0 + (j + 1) * dy1;
      float z01 = z0 + i * dz0 + (j + 1) * dz1;

      float x11 = x0 + (i + 1) * dx0 + (j + 1) * dx1;
      float y11 = y0 + (i + 1) * dy0 + (j + 1) * dy1;
      float z11 = z0 + (i + 1) * dz0 + (j + 1) * dz1;

      float x10 = x0 + (i + 1) * dx0 + j * dx1;
      float y10 = y0 + (i + 1) * dy0 + j * dy1;
      float z10 = z0 + (i + 1) * dz0 + j * dz1;

      glTexCoord2f(i * txk, j * tyk);
      glVertex3f(x00, y00, z00);
      glTexCoord2f(i * txk, (j + 1) * tyk);
      glVertex3f(x01, y01, z01);
      glTexCoord2f((i + 1) * txk, (j + 1) * tyk);
      glVertex3f(x11, y11, z11);
      glTexCoord2f((i + 1) * txk, j * tyk);
      glVertex3f(x10, y10, z10);
    }
  }
  glEnd();
}

Vector torusKnotCenterPoint(float phi) 
{
  return 0.5 * Vector( (2 + cos(3 * phi / 2)) * cos(phi), (2 + cos(3 * phi / 2)) * sin(phi), sin(3 * phi / 2) );
}

Vector torusKnotPoint(float phi, float psi) 
{
  Vector p = torusKnotCenterPoint(phi);
  Vector up = Vector(0, 0, 1);
  Vector nup = norm(up ^ (torusKnotCenterPoint(phi + 0.1) - p));
  return p + 0.4 * sin(psi) * up + 0.4 * cos(psi) * nup;
}

void placeTorusKnotPoint(float phi, float psi) {
  Vector p = torusKnotPoint(phi, psi);
  Vector n = (torusKnotPoint(phi, psi + 0.001) - p) ^ (torusKnotPoint(phi + 0.001, psi) - p);
  Vector t = Vector(2 * phi / 3.1415926f, psi / 3.1415926f, 0);
  glTexCoord2f(t.x, t.y);
  glNormal3f(n.x, n.y, n.z);
  glVertex3f(p.x, p.y, p.z);
}

void renderTorusKnot() {
  glBindTexture(GL_TEXTURE_2D, marbleMap);
  int n = 150;
  int k = 50;
  glBegin(GL_QUADS);
  for(int i = 0; i < n; i++)
  {
    float phi0 = 4 * 3.1415926f * i / n;
    float phi1 = 4 * 3.1415926f * (i + 1) / n;
    for(int j = 0; j < k; j++) 
    {
      float psi0 = 2 * 3.1415926f * j / k;
      float psi1 = 2 * 3.1415926f * (j + 1) / k;
      placeTorusKnotPoint(phi0, psi0);
      placeTorusKnotPoint(phi1, psi0);
      placeTorusKnotPoint(phi1, psi1);
      placeTorusKnotPoint(phi0, psi1);
    }
  }
  glEnd();
}

void renderScene(float dx, float dy, float dz)
{
  glClear(GL_DEPTH_BUFFER_BIT | GL_COLOR_BUFFER_BIT);
  glPushMatrix();
  gluLookAt(cx, cy, cz, dx, dy, 5 + dz, 0, 0, 1);

  GLfloat l_position[] = {lx, ly, lz, 1.0};
  glLightfv(GL_LIGHT0, GL_POSITION, l_position);

  glCallList(ROOM);

  glPushMatrix();
  glTranslatef(0, 0, 4);
  glRotatef(time * 50, 1, 1, 1);
  glRotatef(time * -50, -1, -1, 1);
  glCallList(KNOT);
  glPopMatrix();

  glPopMatrix();
}

void display ()
{
  cx = RADIUS * sin(cphi) * cos(cpsi);
  cy = RADIUS * cos(cphi) * cos(cpsi);
  cz = 5 + RADIUS * sin(cpsi);

  lx = 0;
  ly = 0;
  lz = 9;

  float d = RADIUS * 0.6f / height;
  
  glClear(GL_ACCUM_BUFFER_BIT);

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();

  unsigned int n = 4;

  renderScene(-d * cos(cphi),  d * sin(cphi),  0);
  glAccum(GL_ACCUM, 1.0f / n);
  renderScene( d * cos(cphi), -d * sin(cphi),  0);
  glAccum(GL_ACCUM, 1.0f / n);
  renderScene( d * sin(cphi) * sin(cpsi),  d * cos(cphi) * sin(cpsi), -d * cos(cpsi));
  glAccum(GL_ACCUM, 1.0f / n);
  renderScene(-d * sin(cphi) * sin(cpsi), -d * cos(cphi) * sin(cpsi),  d * cos(cpsi));
  glAccum(GL_ACCUM, 1.0f / n);
  
  glAccum(GL_RETURN, 1.0f);

  glPopMatrix();
  glutSwapBuffers ();
}

void reshape(int w, int h)
{
  glViewport(0, 0, (GLsizei)w, (GLsizei)h );
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  gluPerspective(60.0, (GLfloat)w/(GLfloat)h, 0.1, 60.0);
  glMatrixMode(GL_MODELVIEW);

  height = h;
  width = w;
}

void key(unsigned char key, int x, int y)
{
  if(key == 27 || key == 'q' || key == 'Q')
    exit(0);
  if(key == 'a' || key == 'A')
    cphi += 0.05;
  if(key == 'd' || key == 'D')
    cphi -= 0.05;
  if(key == 'w' || key == 'W')
    cpsi += 0.05;
  if(key == 's' || key == 'S')
    cpsi -= 0.05;
  if(cpsi < -0.5) 
    cpsi = -0.5;
  if(cpsi > 0.5) 
    cpsi = 0.5;
}

void animate()
{
  time = GetTickCount() * 0.001f - startTime;
  glutPostRedisplay ();
}

void init()
{
  glClearColor(0.0, 0.0, 0.0, 1.0);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LESS);

  glHint(GL_POLYGON_SMOOTH_HINT,         GL_NICEST);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

  glEnable(GL_NORMALIZE);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);

  // light & materials
  GLfloat l_diffuse[] =  {0.8, 0.8, 0.5, 1};
  GLfloat l_specular[] = {1.0, 1.0, 1.0, 1};
  GLfloat l_nothing[] =  {0.0, 0.0, 0.0, 1};
  GLfloat l_ambient[] =  {0.1, 0.1, 0.1, 1};
  glLightfv(GL_LIGHT0, GL_DIFFUSE,  l_diffuse);
  glLightfv(GL_LIGHT0, GL_SPECULAR, l_specular);
  glLightfv(GL_LIGHT0, GL_AMBIENT,  l_ambient);
  glLightf(GL_LIGHT0,  GL_LINEAR_ATTENUATION, 0.05);

  GLfloat m_diffuse[] =  {1.0, 1.0, 1.0, 1};
  GLfloat m_specular[] = {1.0, 1.0, 1.0, 1};
  GLfloat m_ambient[] =  {0.5, 0.5, 0.5, 1};
  GLfloat m_emission[] = {0,   0,   0,   1};
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT,  m_ambient);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE,  m_diffuse);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, m_specular);
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, m_emission);
  glMaterialf(GL_FRONT_AND_BACK,  GL_SHININESS, 21);

  // textures
  AUX_RGBImageRec *TextureImage;

  TextureImage = auxDIBImageLoad("wood_floor.bmp");
  glGenTextures(1, &floorMap);
  glBindTexture(GL_TEXTURE_2D, floorMap);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TextureImage->sizeX, TextureImage->sizeY, GL_RGB, GL_UNSIGNED_BYTE, TextureImage->data);

  TextureImage = auxDIBImageLoad("ceiling_wood.bmp");
  glGenTextures(1, &ceilMap);
  glBindTexture(GL_TEXTURE_2D, ceilMap);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TextureImage->sizeX, TextureImage->sizeY, GL_RGB, GL_UNSIGNED_BYTE, TextureImage->data);

  TextureImage = auxDIBImageLoad("wall_wallpaper.bmp");
  glGenTextures(1, &wallMap);
  glBindTexture(GL_TEXTURE_2D, wallMap);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TextureImage->sizeX, TextureImage->sizeY, GL_RGB, GL_UNSIGNED_BYTE, TextureImage->data);

  TextureImage = auxDIBImageLoad("marble.bmp");
  glGenTextures(1, &marbleMap);
  glBindTexture(GL_TEXTURE_2D, marbleMap);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TextureImage->sizeX, TextureImage->sizeY, GL_RGB, GL_UNSIGNED_BYTE, TextureImage->data);

  // room list
  glNewList(ROOM, GL_COMPILE);
  glLightfv(GL_LIGHT0, GL_SPECULAR, l_nothing);
  glBindTexture(GL_TEXTURE_2D, floorMap);
  drawMultiQuad(-10, -10, 0, 10, 10, 2, 0, 0, 0, 2, 0, 0, 0, 1, 1, 1);

  glBindTexture(GL_TEXTURE_2D, wallMap);
  drawMultiQuad(-10, -10, 0, 10, 10, 2, 0, 0, 0, 0, 1, 0, 1, 0, 0.5, 0.25);
  drawMultiQuad(-10, -10, 0, 10, 10, 0, 2, 0, 0, 0, 1, 1, 0, 0, 0.5, 0.25);
  drawMultiQuad( 10,  10, 0, 10, 10, -2, 0, 0, 0, 0, 1, 0, -1, 0, 0.5, 0.25);
  drawMultiQuad( 10,  10, 0, 10, 10, 0, -2, 0, 0, 0, 1, -1, 0, 0, 0.5, 0.25);

  glBindTexture(GL_TEXTURE_2D, ceilMap);
  drawMultiQuad(-10, -10, 10, 10, 10, 2, 0, 0, 0, 2, 0, 0, 0, -1, 0.5, 0.5);

  glBindTexture(GL_TEXTURE_2D, marbleMap);
  drawMultiQuad(-1, 0, 0, 2, 3, 0.5,  0.5, 0, 0, 0, 0.5, -1, -1, 0, 0.25, 0.5);
  drawMultiQuad( 0, 1, 0, 2, 3, 0.5, -0.5, 0, 0, 0, 0.5, -1,  1, 0, 0.25, 0.5);
  drawMultiQuad( 1, 0, 0, 2, 3,-0.5, -0.5, 0, 0, 0, 0.5,  1,  1, 0, 0.25, 0.5);
  drawMultiQuad( 0,-1, 0, 2, 3,-0.5,  0.5, 0, 0, 0, 0.5,  1, -1, 0, 0.25, 0.5);
  drawMultiQuad( 1, 0, 1.5, 2, 2, -0.5, 0.5, 0, -0.5, -0.5, 0, 0, 0, 1, 0.5, 0.5);

  glDisable(GL_TEXTURE_2D);
  glDisable(GL_LIGHTING);
  glColor3f(1, 1, 1);
  drawMultiQuad(-1, -1, 9.95, 2, 2, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0);
  glEnable(GL_LIGHTING);
  glEnable(GL_TEXTURE_2D);

  glEndList();

  // torusknot list
  glNewList(KNOT, GL_COMPILE);
  glLightfv(GL_LIGHT0, GL_SPECULAR, l_specular);
  renderTorusKnot();
  glEndList();
}


int main(int argc, char* argv[]) 
{
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGBA | GLUT_DEPTH | GLUT_STENCIL | GLUT_ACCUM);
  glutInitWindowSize(640, 480);
  glutCreateWindow("OpenGL");

  glutDisplayFunc(display);
  glutReshapeFunc(reshape);
  glutKeyboardFunc(key);
  glutIdleFunc(animate);

  init();

  glutMainLoop ();
  return 0;
}
