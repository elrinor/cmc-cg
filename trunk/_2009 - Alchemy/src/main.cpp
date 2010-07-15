#include <gl/GLAux.h>
#include <gl/glut.h>
#include <gl/GLU.h>
#include <gl/GL.h>
#include <stdlib.h>
#include <vector>
#include <algorithm>
#include "Vector3D.h"

using namespace std;

#define PI 3.14159265f

float random(float maxVal) {
  return maxVal * rand() / RAND_MAX;
}

struct Particle {
  Vector3D coord, speed, color;
};

struct Quad {
  Vector3D v[4];
  Vector3D n[4];
  float dist;
};

vector<Quad> sortedObject;
vector<int> sortedOrder;

vector<Particle> particles;

Vector3D player, light;

GLuint woodTexture, stoneTexture;

GLuint sphereList, floorList, stuffList;

float lastTime, time, dtime;

Vector3D ObjCenter(float phi) {
  float r = 0.6f + 0.2f * cos(4.5f * phi);
  float psi = 0.1 * PI * sin(4.5f * phi);
  return Vector3D(r * sin(phi) * cos(psi), r * cos(phi) * cos(psi), r * sin(psi));
}

void ObjVertex(int i, int j, int in, int jn, Vector3D& vertex, Vector3D& normal) {
  float phi = 4 * PI * i / in;
  float psi = 2 * PI * j / jn;
  Vector3D dir = (ObjCenter(phi + 0.001) - ObjCenter(phi)).normalize();
  Vector3D up(0, 0, 1);
  Vector3D side = up ^ dir;
  normal = up * cos(psi) + side * sin(psi);
  vertex = Vector3D(0, 0, 0.3) + ObjCenter(phi) + normal * 0.05f;
}

void GenerateObject() {
  const int in = 256;
  const int jn = 16;

  for(int i = 0; i < in; i++) for(int j = 0; j < jn; j++) {
    sortedObject.push_back(Quad());
    Quad& f = sortedObject.back();
    ObjVertex(i  , j  , in, jn, f.v[0], f.n[0]);
    ObjVertex(i  , j+1, in, jn, f.v[1], f.n[1]);
    ObjVertex(i+1, j+1, in, jn, f.v[2], f.n[2]);
    ObjVertex(i+1, j  , in, jn, f.v[3], f.n[3]);
  }

  sortedOrder.resize(sortedObject.size());
  for(unsigned int i = 0; i < sortedOrder.size(); i++)
    sortedOrder[i] = i;
}

struct Compare {
  bool operator()(int a, int b) {
    return sortedObject[a].dist > sortedObject[b].dist;
  }
};

void SortObject() {
  for(unsigned int i = 0; i < sortedObject.size(); i++)
    sortedObject[i].dist = (player - sortedObject[i].v[0]).lengthSq();
  sort(sortedOrder.begin(), sortedOrder.end(), Compare());
}

Particle NewParticle() {
  Particle p;
  p.coord = Vector3D(random(0.04) - 0.02, random(0.04) - 0.02, 0.2 + random(0.2));
  p.color = Vector3D(0.5, 0, 0);
  p.speed = 0.4 * Vector3D(random(1.0) - 0.5, random(1.0) - 0.5, 3 + random(3.0));
  return p;
}

void MoveParticles() {
  for(unsigned int i = 0; i < particles.size(); i++) {
    Particle& p = particles[i];
    p.speed.z -= dtime * 2;
    p.coord = p.coord + p.speed * dtime;
    if(p.speed.z < 0)
      p.color.x -= 0.5 * dtime;
    if(p.color.x < 0)
      p = NewParticle();
  }
}

void GenerateParticles() {
  for(int i = 0; i < 200; i++)
    particles.push_back(NewParticle());
  dtime = 0.05;
  for(int i = 0; i < 500; i++)
    MoveParticles();
}

void RenderObjects() {
  glCallList(stuffList);

  glPushAttrib(GL_ALL_ATTRIB_BITS);
  glDisable(GL_TEXTURE_2D);
  for(unsigned int i = 0; i < particles.size(); i++) {
    Particle& p = particles[i];
    float m_color[4] = {p.color.x, p.color.y, p.color.z, 1.0};
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, m_color);
    glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_color);
    glPushMatrix();
    glTranslatef(p.coord.x, p.coord.y, p.coord.z);
    glScalef(0.05 * p.color.x, 0.05 * p.color.x, 0.05 * p.color.x);
    glCallList(sphereList);
    glPopMatrix();
  }
  glEnable(GL_TEXTURE_2D);
  glPopAttrib();

  glPushAttrib(GL_ALL_ATTRIB_BITS);
  float m_color[4] = {0.4, 0.3, 0.2, 0.3};
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, m_color);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_color);
  glEnable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
  glBegin(GL_QUADS);
  for(unsigned int i = 0; i < sortedObject.size(); i++) {
    Quad& f = sortedObject[sortedOrder[i]];
    glNormal3f(f.n[0].x, f.n[0].y, f.n[0].z);
    glVertex3f(f.v[0].x, f.v[0].y, f.v[0].z);
    glNormal3f(f.n[1].x, f.n[1].y, f.n[1].z);
    glVertex3f(f.v[1].x, f.v[1].y, f.v[1].z);
    glNormal3f(f.n[2].x, f.n[2].y, f.n[2].z);
    glVertex3f(f.v[2].x, f.v[2].y, f.v[2].z);
    glNormal3f(f.n[3].x, f.n[3].y, f.n[3].z);
    glVertex3f(f.v[3].x, f.v[3].y, f.v[3].z);
  }
  glEnd();
  glEnable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);
  glPopAttrib();
}

void RenderFloor() {
  glCallList(floorList);
}

void RenderLight() {
  glDisable(GL_LIGHTING);
  glDisable(GL_TEXTURE_2D);
  glColor4f(1, 1, 1, 1);
  glPushMatrix();
  glTranslatef(light.x, light.y, light.z);
  glScalef(0.015, 0.015, 0.015);
  glCallList(sphereList);
  glPopMatrix();
  glEnable(GL_LIGHTING);
  glEnable(GL_TEXTURE_2D);
}

void DisplayFunc() {
  player = Vector3D(3 * sin(0.5f * time), 3 * cos(0.5f * time), 2);

  light = Vector3D(sin(2 * time) * (1 + 0.5 * sin(5 * time)), cos(2 * time) * (1 + 0.5 * sin(5 * time)), 1.5f + 0.25f * sin(time));

  glClearColor(0, 0, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

  time = timeGetTime() / 1000.0f;
  dtime = time - lastTime;
  lastTime = time;

  MoveParticles();

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(player.x, player.y , player.z, 0, 0, 0, 0, 0, 1);

  GLfloat l_position[4] = { light.x, light.y, light.z, 1 };
  glLightfv(GL_LIGHT0, GL_POSITION, l_position);

  glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
  glDepthMask(GL_FALSE);
  glEnable(GL_STENCIL_TEST);
  glStencilFunc(GL_ALWAYS, 1, 1);
  glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);
  RenderFloor();
  glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
  glDepthMask(GL_TRUE);

  glStencilFunc(GL_EQUAL, 1, 1);
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  glPushMatrix();
  glScalef(1, 1, -1);
  glLightfv(GL_LIGHT0, GL_POSITION, l_position);
  player.z *= -1;
  SortObject();
  player.z *= -1;
  RenderLight();
  RenderObjects();
  glPopMatrix();
  glDisable(GL_STENCIL_TEST);

  glLightfv(GL_LIGHT0, GL_POSITION, l_position);

  glEnable(GL_BLEND);
  RenderFloor();
  glDisable(GL_BLEND);

  SortObject();
  RenderObjects();
  RenderLight();

  glEnable(GL_POLYGON_OFFSET_FILL);
  glPolygonOffset(-4, -4);
  glEnable(GL_STENCIL_TEST);
  glStencilFunc(GL_EQUAL, 1, 3);
  glStencilOp(GL_KEEP, GL_INCR, GL_INCR);
  glDisable(GL_LIGHTING);
  glDisable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glColor4f(0, 0, 0, 0.4);
  glPushMatrix();
  GLfloat projection[16] = {light.z, 0, 0, 0,   0, light.z, 0, 0,   -light.x, -light.y, 0, -1,   0, 0, 0, light.z};
  glMultMatrixf(projection);
  RenderObjects();
  glPopMatrix();
  glDisable(GL_BLEND);
  glEnable(GL_LIGHTING); 
  glEnable(GL_TEXTURE_2D);
  glDisable(GL_STENCIL_TEST);
  glDisable(GL_POLYGON_OFFSET_FILL);

  glutSwapBuffers();
}


void IdleFunc() {
  glutPostRedisplay();
}

void KeyboardFunc(unsigned char key, int x, int y) {
  if(key=='\033')
    exit(0);
}

void ReshapeFunc(int w, int h) {
  glViewport(0, 0, w, h);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(60.0f, (float) w / h, 0.1f, 45);
  glMatrixMode(GL_MODELVIEW);
}

GLuint LoadTexture(const char* name, GLint sParam, GLint tParam) {
  GLuint result;

  glGenTextures(1, &result);
  AUX_RGBImageRec* image = auxDIBImageLoad(name);
  
  glBindTexture(GL_TEXTURE_2D, result);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, sParam);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, tParam);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, image->sizeX, image->sizeY, GL_RGB, GL_UNSIGNED_BYTE, image->data);

  return result;
}

int main(int argc, char** argv) {
  glutInit(&argc,argv);
  glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE | GLUT_STENCIL);
  glutInitWindowSize(640, 480);
  glutCreateWindow("OpenGL");

  woodTexture = LoadTexture("textures/wood.bmp", GL_REPEAT, GL_REPEAT);
  stoneTexture = LoadTexture("textures/stone.bmp", GL_REPEAT, GL_REPEAT);

  GLUquadric* quadric = gluNewQuadric();
  gluQuadricTexture(quadric, GL_TRUE);

  sphereList = glGenLists(1);
  glNewList(sphereList, GL_COMPILE);
  gluSphere(quadric, 1, 8, 8);
  glEndList();
  
  floorList = glGenLists(1);
  glNewList(floorList, GL_COMPILE);
  glBindTexture(GL_TEXTURE_2D, woodTexture);
  glBegin(GL_QUADS);
  glNormal3f(0, 0, 1);
  for(int i = 0; i < 16; i++) {
    for(int j = 0; j < 16; j++) {
      float kx = 2.0f / 16;
      float ky = 2.0f / 16;
      float dx = 2.6 / 16;
      float dy = 2.6 / 16;
      float x = -1.3 + i * dx;
      float y = -1.3 + j * dy;

      glTexCoord2f(i * kx, j * ky);
      glVertex3f(x, y, 0);
      glTexCoord2f(i * kx, (j + 1) * ky);
      glVertex3f(x, y + dy, 0);
      glTexCoord2f((i + 1) * kx, (j + 1) * ky);
      glVertex3f(x + dx, y + dy, 0);
      glTexCoord2f((i + 1) * kx, j * ky);
      glVertex3f(x + dx, y, 0);
    }
  }
  glEnd();
  glEndList();

  stuffList = glGenLists(1);
  glNewList(stuffList, GL_COMPILE);
  glBindTexture(GL_TEXTURE_2D, stoneTexture);
  gluCylinder(quadric, 0.25, 0.15, 0.5, 16, 16);
  gluCylinder(quadric, 0, 0.1, 0.5, 16, 16);
  glPushMatrix();
  glTranslatef(0, 0, 0.5);
  gluDisk(quadric, 0.1, 0.15, 16, 1);
  glPopMatrix();

  glPushMatrix();
  glTranslatef(0.5f, 1.0f, 0.25f);
  gluSphere(quadric, 0.25f, 16, 16);
  glPopMatrix();

  glPushMatrix();
  glTranslatef(-1.0f, -0.5f, 0.2f);
  gluSphere(quadric, 0.2f, 16, 16);
  glPopMatrix();
  glEndList();

  GenerateObject();
  GenerateParticles();

  lastTime = timeGetTime() / 1000.0f;

  GLfloat m_emissive[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
  GLfloat m_diffuse[4] = { 0.4f, 0.4f, 0.4f, 0.3f };
  GLfloat m_specular[4] = { 0.6f, 0.6f, 0.6f, 0.0f };
  GLfloat m_ambient[4] = { 0.3f, 0.3f, 0.3f, 0.0f };
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, m_emissive);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, m_specular);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, m_ambient);
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 10);

  GLfloat l_diffuse[4] = { 1.0f, 1.0f, 1.0f, 0.0f };
  GLfloat l_specular[4] = { 1.0f, 1.0f, 1.0f, 0.0f };
  GLfloat l_ambient[4] = { 1.0f, 1.0f, 1.0f, 0.0f };
  glLightfv(GL_LIGHT0, GL_DIFFUSE, l_diffuse);
  glLightfv(GL_LIGHT0, GL_SPECULAR ,l_specular);
  glLightfv(GL_LIGHT0, GL_AMBIENT, l_ambient);

  glEnable(GL_DEPTH_TEST);
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_NORMALIZE);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glShadeModel(GL_SMOOTH);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glutDisplayFunc(DisplayFunc);
  glutReshapeFunc(ReshapeFunc);
  glutKeyboardFunc(KeyboardFunc);
  glutIdleFunc(IdleFunc);

  glutMainLoop();

  return 0;
}