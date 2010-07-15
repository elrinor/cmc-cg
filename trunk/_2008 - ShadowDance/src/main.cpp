#define _USE_MATH_DEFINES

#include <gl/glaux.h>
#include <gl/glu.h>
#include <gl/glut.h>
#include <gl/gl.h>
#include <cmath>
#include <vector>
#include <algorithm>

#pragma comment(lib, "glaux.lib")

#define FLOOR_LIST 1

using namespace std;

GLuint tFloor, tLava, tFire;

float t, dt;

int windowW, windowH;

class Vector3D {
public:
  float x, y, z, w;
  Vector3D() {}
  Vector3D(float x, float y, float z, float w = 1.0): x(x), y(y), z(z), w(w) {}
  Vector3D operator+ (Vector3D a) { return Vector3D(x + a.x, y + a.y, z + a.z); }
  Vector3D operator- () { return Vector3D(-x, -y, -z); }
  Vector3D operator- (Vector3D a) { return *this + (-a); }
  Vector3D operator* (float a) { return Vector3D(x * a, y * a, z * a); }
  Vector3D operator/ (float a) { return *this * (1 / a); }
  float operator* (Vector3D a) { return x * a.x + y * a.y + z * a.z; }
  float lenghtSq() { return *this * *this; }
  float length() { return sqrt(lenghtSq()); }
  Vector3D normalize() { return *this / length(); }
  Vector3D operator^ (Vector3D a) { return Vector3D(y*a.z - z*a.y, z*a.x - x*a.z, x*a.y - y*a.x); }
  float* c_array() { static float a[4]; a[0] = x; a[1] = y; a[2] = z; a[3] = w; return a; }
};

Vector3D v(float x, float y, float z, float w = 1.0) { return Vector3D(x, y, z, w); }

Vector3D randV() { return Vector3D(1.0f - 2.0f * rand() / RAND_MAX, 1.0f - 2.0f * rand() / RAND_MAX, 1.0f - 2.0f * rand() / RAND_MAX); }

Vector3D eye, target, light;

void glVertex(Vector3D v) { glVertex3f(v.x, v.y, v.z); }
void glNormal(Vector3D n) { glNormal3f(n.x, n.y, n.z); }
void glTexCoord(Vector3D t) {glTexCoord2f(t.x, t.y); }

struct Quad {
  Vector3D v[4], t[4], n[4];
  float dist;

  float life;
  float dlife;
  bool isParticle;
};

struct QuadCmp {
  bool operator()(const Quad* a, const Quad* b) { return a->dist > b->dist; }
};


vector<Quad*> quads;

Quad genParticle() {
  Quad p;
  p.v[0] = light + randV() * 0.5;
  p.v[1] = (v(0, 0, 5) + randV()) * 0.3;
  p.life = 1.0;
  p.dlife = -0.3 - 0.7 * rand() / RAND_MAX;
  p.isParticle = true;
  return p;
}

void updateParticles(float dt) {
  for(unsigned int i = 0; i < quads.size(); i++) if(quads[i]->isParticle) {
    quads[i]->life += dt * quads[i]->dlife;
    quads[i]->v[0] = quads[i]->v[0] + quads[i]->v[1] * dt;
    if(quads[i]->life < 0)
      *quads[i] = genParticle();
  }
}

void placeLight(GLuint id, Vector3D p, Vector3D c) {
  glLightfv(id, GL_DIFFUSE,  c.c_array());
  glLightfv(id, GL_SPECULAR, c.c_array());
  glLightfv(id, GL_AMBIENT,  c.c_array());
  glLightfv(id, GL_POSITION, p.c_array());
}

void prepareLight() {
  placeLight(GL_LIGHT0, light, v(1.5, 1.0, 0));
}

void drawWorld() {
  // sort
  for(unsigned int i = 0; i < quads.size(); i++) {
    quads[i]->dist = (quads[i]->v[0] - eye).lenghtSq();
    if(quads[i]->isParticle)
      quads[i]->dist *= 0.95;
  }
  sort(quads.begin(), quads.end(), QuadCmp());
  
  // translucent objects
  static GLfloat matrix[16];
  glGetFloatv(GL_MODELVIEW_MATRIX, matrix);
  Vector3D u = Vector3D(matrix[1], matrix[5], matrix[9]).normalize() * 0.5;
  Vector3D l = Vector3D(matrix[0], matrix[4], matrix[8]).normalize() * 0.5;

  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE,  v(1.0, 1.0, 1.0, 0.7).c_array());
  glEnable(GL_BLEND);

  bool prevIsParticle = false;

  // quad intro
  glBindTexture(GL_TEXTURE_2D, tLava);
  glBegin(GL_QUADS);

  for(unsigned int i = 0; i < quads.size(); i++) {
    Quad* q = quads[i];
    if(q->isParticle && !prevIsParticle) {
      // finalize quad
      glEnd();

      // start particle
      glBindTexture(GL_TEXTURE_2D, tFire);
      glDepthMask(GL_FALSE);
      glDisable(GL_LIGHTING);
      glBegin(GL_QUADS);

      prevIsParticle = true;
    }

    if(!q->isParticle && prevIsParticle) {
      // finalize particle
      glEnd();
      glEnable(GL_LIGHTING);
      glDepthMask(GL_TRUE);

      // start quad
      glBindTexture(GL_TEXTURE_2D, tLava);
      glBegin(GL_QUADS);

      prevIsParticle = false;
    }

    if(q->isParticle) {
      glColor4f(min(2 * q->life, 1.0f), q->life, 0, 0.5 * q->life);
      glTexCoord2f(0, 0); glVertex(q->v[0] + u + l);
      glTexCoord2f(1, 0); glVertex(q->v[0] - u + l);
      glTexCoord2f(1, 1); glVertex(q->v[0] - u - l);
      glTexCoord2f(0, 1); glVertex(q->v[0] + u - l);
    } else {
      glNormal  (q->n[0]);
      glTexCoord(q->t[0]);
      glVertex  (q->v[0]);
      glNormal  (q->n[1]);
      glTexCoord(q->t[1]);
      glVertex  (q->v[1]);
      glNormal  (q->n[2]);
      glTexCoord(q->t[2]);
      glVertex  (q->v[2]);
      glNormal  (q->n[3]);
      glTexCoord(q->t[3]);
      glVertex  (q->v[3]);
    }
  }

  if(prevIsParticle) {
    glEnd();
    glEnable(GL_LIGHTING);
    glDepthMask(GL_TRUE);
  } else {
    glEnd();
  }

  glDisable(GL_BLEND);
}

void displayFunc(void) {
  updateParticles(dt);

  eye = v(5 * sin(0.96 * t), 4 * cos(1.07 * t), 5 + 1.2 * cos(1.3 * t));
  target = v(0, 0, 0);
  light = v(2.5 * sin(1.17 * t), 2.3 * cos(1.38 * t), 1.5 + 1.2 * sin(3 * t));

  glClearColor(0.2, 0.0, 0.0, 0);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(111, (float) windowW / windowH, 0.1, 30);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(eye.x, eye.y, eye.z, target.x, target.y, target.z, 0, 0, 1);

  // reflection
  glPushMatrix();
  glScalef(1, 1, -1);
  eye.z *= -1;
  prepareLight();
  drawWorld();
  eye.z *= -1;
  glPopMatrix();

  // get light back
  prepareLight();

  // floor
  glEnable(GL_FOG);
  glFogfv(GL_FOG_COLOR, v(0.2, 0.0, 0.0).c_array());
  glFogf(GL_FOG_START, 3.0f);
  glFogf(GL_FOG_END, 12.0f);
  glFogi(GL_FOG_MODE, GL_LINEAR);

  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE,  v(1.0, 1.0, 1.0, 0.5).c_array());
  glEnable(GL_BLEND);
  glCallList(FLOOR_LIST);
  glDisable(GL_BLEND);

  glDisable(GL_FOG);

  // upper world
  drawWorld();

  glutSwapBuffers();
}

void reshapeFunc(int w, int h){
  glViewport(0, 0, w, h);
  windowW = w;
  windowH = h;
}

void GLUTCALLBACK idleFunc() {
  static int prevTime = GetTickCount();
  dt = (float) (GetTickCount() - prevTime) / 1000.0f;
  t += dt;
  prevTime = GetTickCount();
  glutPostRedisplay();
}

void GLUTCALLBACK keyFunc(unsigned char key, int x, int y) {
  if(key == 27)
    exit(0);
}

GLuint loadBmpFile(char* file) {
  AUX_RGBImageRec* img = auxDIBImageLoad(file);
  GLuint textureId;
  glGenTextures(1, &textureId);
  glBindTexture(GL_TEXTURE_2D, textureId);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, img->sizeX, img->sizeY, GL_RGB, GL_UNSIGNED_BYTE, img->data);
  return textureId;
}

GLuint loadBmpFile(char* file, char* alpha) {
  AUX_RGBImageRec* img = auxDIBImageLoad(file);
  AUX_RGBImageRec* alp = auxDIBImageLoad(alpha);
  char* data = new char[4 * img->sizeX * img->sizeY];
  for(int y = 0; y < img->sizeY; y++) {
    for(int x = 0; x < img->sizeX; x++) {
      data[(y * img->sizeX + x) * 4 + 0] = img->data[(y * img->sizeX + x) * 3 + 0];
      data[(y * img->sizeX + x) * 4 + 1] = img->data[(y * img->sizeX + x) * 3 + 1];
      data[(y * img->sizeX + x) * 4 + 2] = img->data[(y * img->sizeX + x) * 3 + 2];
      data[(y * img->sizeX + x) * 4 + 3] = alp->data[(y * img->sizeX + x) * 3 + 0];
    }
  }
  GLuint textureId;
  glGenTextures(1, &textureId);
  glBindTexture(GL_TEXTURE_2D, textureId);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 4, img->sizeX, img->sizeY, GL_RGBA, GL_UNSIGNED_BYTE, data);
  return textureId;
}

Vector3D knotCenterVertex(float a) {
  float u = a * 12 * M_PI;
  Vector3D result;
  static const float R = 2;
  static const float r = 0.5;
  result.x = (R + 1.5  * r * cos(u / 2)) * cos(u / 3);
  result.y = (R + 1.5  * r * cos(u / 2)) * sin(u / 3);
  result.z = (    1.05 * r * sin(u / 2)) + 1.5;
  return result;
}

Vector3D knotVertex(float a, float b) {
  float v = b * 2 * M_PI;
  Vector3D result;
  static const float r = 0.6;
  Vector3D f = (knotCenterVertex(a) - knotCenterVertex(a + 0.001));
  Vector3D x = (f ^ Vector3D(0, 0, 1)).normalize();
  Vector3D y = (x ^ f).normalize();
  result = knotCenterVertex(a) + (x * sin(v) + y * cos(v)) * r;
  return result;
}

Vector3D knotNormal(float a, float b) {
  return (knotVertex(a + 0.001, b) - knotVertex(a, b)) ^ (knotVertex(a, b + 0.001) - knotVertex(a, b));
}

void knotPoint(float a, float b, Vector3D& v, Vector3D& n, Vector3D& t) {
  v = knotVertex(a, b);
  n = knotNormal(a, b);
  t = Vector3D(a * 20, b * 2, 0);
}

int main(int argc, char** argv) {
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_DEPTH | GLUT_RGB | GLUT_MULTISAMPLE | GLUT_STENCIL);
  glutInitWindowSize(800, 600);
  glutCreateWindow("");
  glutDisplayFunc(displayFunc);
  glutReshapeFunc(reshapeFunc);
  glutIdleFunc(idleFunc);
  glutKeyboardFunc(keyFunc);
  glutIgnoreKeyRepeat(1);
  glutSetCursor(GLUT_CURSOR_NONE);

  glClearColor(0, 0, 0, 0);
  glEnable(GL_DEPTH_TEST);
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glShadeModel(GL_SMOOTH);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glEnable(GL_TEXTURE_2D);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_NORMALIZE);

  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, v(0.0, 0.0, 0.0).c_array());
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE,  v(1.0, 1.0, 1.0, 1.0).c_array());
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, v(0.8, 0.8, 0.8).c_array());
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT,  v(0.3, 0.3, 0.3).c_array());
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 12);

  glLightf(GL_LIGHT0, GL_QUADRATIC_ATTENUATION, 0.02);

  tLava  = loadBmpFile("textures/lava.bmp", "textures/lava_a.bmp");
  tFloor = loadBmpFile("textures/floor.bmp");
  tFire  = loadBmpFile("textures/fire.bmp", "textures/fire_a.bmp");
  
  // gen floor list
  glNewList(FLOOR_LIST, GL_COMPILE);
  glBindTexture(GL_TEXTURE_2D, tFloor);
  glBegin(GL_QUADS);
  glNormal3f(0, 0, 1);
  for(int i = -40; i < 40; i++) {
    for(int j = -40; j < 40; j++) {
      glTexCoord2f((i  ) * 0.5, (j  ) * 0.5);
      glVertex3f(i  , j  , 0);
      glTexCoord2f((i+1) * 0.5, (j  ) * 0.5);
      glVertex3f(i+1, j  , 0);
      glTexCoord2f((i+1) * 0.5, (j+1) * 0.5);
      glVertex3f(i+1, j+1, 0);
      glTexCoord2f((i  ) * 0.5, (j+1) * 0.5);
      glVertex3f(i  , j+1, 0);
    }
  }
  glEnd();
  glEndList();

  // fill polygons
  static const int maxI = 80;
  static const int maxJ = 20;
  for(int i = 0; i < maxI; i++) {
    for(int j = 0; j < maxJ; j++) {
      Quad* p = new Quad;
      float a0 = (float) (i  ) / maxI;
      float a1 = (float) (i+1) / maxI;
      float b0 = (float) (j  ) / maxJ;
      float b1 = (float) (j+1) / maxJ;
      knotPoint(a0, b0, p->v[0], p->n[0], p->t[0]);
      knotPoint(a1, b0, p->v[1], p->n[1], p->t[1]);
      knotPoint(a1, b1, p->v[2], p->n[2], p->t[2]);
      knotPoint(a0, b1, p->v[3], p->n[3], p->t[3]);
      p->isParticle = false;
      quads.push_back(p);
    }
  }

  // gen particles
  light = v(100,100,100);
  for(int i = 0; i < 300; i++)
    quads.push_back(new Quad(genParticle()));
  for(int i = 0; i < 100; i++)
    updateParticles(0.1);

  srand(GetTickCount());

  glutMainLoop();

  return 0;
}