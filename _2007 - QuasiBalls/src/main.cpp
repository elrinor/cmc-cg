#define _CRT_SECURE_NO_DEPRECATE
#include <Windows.h>
#include <gl/GL.h>
#include <gl/glext.h>
#include <gl/glut.h>
#include <gl/GLAux.h>
#include <iostream>
#include <vector>
#include <cmath>
#include <algorithm>
#include <limits>

#pragma comment(lib, "glaux.lib")

using namespace std;

#define WINDOW_SIZE_X 800
#define WINDOW_SIZE_Y 600

#define STACKS 128
#define SLICES 32

#define QSTACKS 128
#define QSLICES SLICES
// QSLICES == SLICES is significant!

#define FLOORPARTS 128


#define Q_ANIMATION_CYCLE 10.0f

#define EPS 0.0001f

//#define INF (numeric_limits<float>::infinity())
#define INF 1.0e30f

#define PI 3.1415926f

#define sqr(x) ((x)*(x))

// -------------------------------------------------------------------------- //
// Global Types
// -------------------------------------------------------------------------- //
class Vertex
  {
  public:
    float x, y, z;
    Vertex(float x, float y, float z): x(x), y(y), z(z) {};
    Vertex() {};
    Vertex operator+ (const Vertex& that)
    {
      return Vertex(this->x + that.x, this->y + that.y, this->z + that.z);
    }
    Vertex operator- (const Vertex& that)
    {
      return Vertex(this->x - that.x, this->y - that.y, this->z - that.z);
    }
    Vertex operator^ (const Vertex& that)
    {
      return Vertex(
               this->y * that.z - this->z * that.y,
               - this->x * that.z + this->z * that.x,
               this->x * that.y - this->y * that.x
             );
    }
    float operator* (const Vertex& that)
    {
      return this->x * that.x + this->y * that.y + this->z * that.z;
    }
    Vertex operator* (const float c)
    {
      return Vertex(this->x * c, this->y * c, this->z * c);
    }
    Vertex operator/ (const float c)
    {
      return Vertex(this->x / c, this->y / c, this->z / c);
    }
    float absSqr()
    {
      return *this**this;
    }
    float abs()
    {
      return sqrt(this->absSqr());
    }
    Vertex normalize()
    {
      return *this / this->abs();
    }
  };

class Quad
  {
  public:
    Vertex v[4], t[4], n[4];
    float dist;
    bool exists;
  };

class QuadPtrCmp
  {
  public:
    bool operator() (const Quad* a, const Quad* b)
    {
      return a->dist > b->dist;
    }
  };

// -------------------------------------------------------------------------- //
// Fast Ops
// -------------------------------------------------------------------------- //
float* genTable(float (*f)(float))
{
  float* result = new float[SLICES];
  for (int i = 0; i < SLICES; i++)
    result[i] = f(2 * PI * i / SLICES);
  return result;
}

float tSin(int sliceN)
{
  static float* sinTable = genTable(sin);
  return sinTable[sliceN];
}

float tCos(int sliceN)
{
  static float* cosTable = genTable(cos);
  return cosTable[sliceN];
}

float sgn(float a)
{
  int tmp = (((*(int*)&a) & 0x80000000) | 0x3f800000);
  return *((float*)&tmp);
}

// -------------------------------------------------------------------------- //
// QuasiBalls
// -------------------------------------------------------------------------- //
class QuasiBalls
  {
  private:
    Vertex v[STACKS][SLICES], t[STACKS][SLICES], n[STACKS][SLICES];

    float realR[STACKS + 2];
    float *r;

    Vertex cubesRK[SLICES];

    float c; // z center coordinate
    // float zr; // z-radius

    float singleMetaBallR;
    float doubleMetaBallR;
    float metaBallBorder;

    float metaBallFuncFun;

    float cubePower;
    float cubeZR;
    float cubeYR;

    float maxC;

  public:
    Quad q[STACKS][SLICES];

    float getZ(int iz)
    {
      return 9.0f * iz / STACKS - 4.5f;
    }

    float metaBallFunc(float z, float r)
    {
      return 1 / sqrt(sqr(z - c) + sqr(r)) + 1 / sqrt((abs(z + c) + abs(r)) * metaBallFuncFun + (sqr(z + c) + sqr(r)) * (1 - metaBallFuncFun)) - metaBallBorder;
    }

    float findLineXAxisIntersection(float x1, float y1, float x2, float y2)
    {
      return x1 - y1 * (x1 - x2) / (y1 - y2);
    }

    void postGenerate()
    {
      // Points
      for (int iz = 0; iz < STACKS; iz++)
        {
          float z = getZ(iz);
          float zNormal = 0.0f;
          if (r[iz] < EPS && r[iz + 1] < EPS && r[iz - 1] < EPS)
            continue;
          if (r[iz] < EPS && r[iz + 1] > EPS)
            {
              z = (getZ(iz + 1) + findLineXAxisIntersection(getZ(iz + 1), r[iz + 1], getZ(iz + 2), r[iz + 2])) / 2;
              zNormal = -1.0f;
            }
          else if (r[iz] < EPS && r[iz - 1] > EPS)
            {
              z = (getZ(iz - 1) + findLineXAxisIntersection(getZ(iz - 1), r[iz - 1], getZ(iz - 2), r[iz - 2])) / 2;
              //z = c * sgn(z) + zr;
              zNormal = 1.0f;
            }

          // TexCoords
          float xCoord = (PI / 2 + atan2(abs(z) - c, r[iz])) / PI;
          for (int ia = 0; ia < SLICES; ia++)
            t[iz][ia].x = xCoord;

          // Normals
          if (zNormal != 0.0f)
            {
              for (int ia = 0; ia < SLICES; ia++)
                {
                  n[iz][ia] = Vertex(0, 0, zNormal);
                  v[iz][ia] = Vertex(0, 0, z);
                }
            }
          else
            {
              n[iz][0] = (v[iz][SLICES - 1] - v[iz][0]) ^ (v[iz - 1][0] - v[iz][0]);
              for (int ia = 1; ia < SLICES; ia++)
                n[iz][ia] = (v[iz][ia - 1] - v[iz][ia]) ^ (v[iz - 1][ia] - v[iz][ia]);
            }
        }

      // Quads
      for (int iz = 0; iz < STACKS - 1; iz++)
        {
          if (r[iz] < EPS && r[iz + 1] < EPS)
            for (int ia = 0; ia < SLICES; ia++)
              q[iz][ia].exists = false;
          else
            {
              for (int ia = 0; ia < SLICES; ia++)
                {
                  q[iz][ia].v[0] = v[iz    ][ia];
                  q[iz][ia].t[0] = t[iz    ][ia];
                  q[iz][ia].n[0] = n[iz    ][ia];
                  q[iz][ia].v[1] = v[iz + 1][ia];
                  q[iz][ia].t[1] = t[iz + 1][ia];
                  q[iz][ia].n[1] = n[iz + 1][ia];
                  q[iz][ia].v[2] = v[iz + 1][(ia + 1) % SLICES];
                  q[iz][ia].t[2] = t[iz + 1][(ia + 1) % SLICES];
                  q[iz][ia].n[2] = n[iz + 1][(ia + 1) % SLICES];
                  q[iz][ia].v[3] = v[iz    ][(ia + 1) % SLICES];
                  q[iz][ia].t[3] = t[iz    ][(ia + 1) % SLICES];
                  q[iz][ia].n[3] = n[iz    ][(ia + 1) % SLICES];
                  q[iz][ia].exists = true;
                }
              q[iz][SLICES - 1].t[2].y = 1.0;
              q[iz][SLICES - 1].t[3].y = 1.0;
            }
        }
    }

    float metaBallR(float z)
    {
      float rd = 0, ru = 10, rc;
      float rcf = metaBallFunc(z, rd);
      if (rcf < 0)
        return 0;
      do
        {
          rc = (rd + ru) / 2;
          rcf = metaBallFunc(z, rc);
          if (rcf < 0)
            ru = rc;
          else
            rd = rc;
        }
      while (abs(rcf) > EPS);
      return rc;
    }

    Vertex roundVertex(int ia, float r, float z)
    {
      return Vertex(r * tCos(ia), r * tSin(ia), z);
    }

    void generateMetaBalls()
    {
      for (int iz = 0; iz < STACKS; iz++)
        {
          float z = getZ(iz);
          r[iz] = metaBallR(z);
          if (r[iz] > EPS)
            for (int ia = 0; ia < SLICES; ia++)
              v[iz][ia] = roundVertex(ia, r[iz], z);
        }
      // zr = 0.0f;
      postGenerate();
    }

    float cubesR(float z)
    {
      float tmp = abs(abs(z) - c) / cubeZR;
      if (tmp > 1)
        return 0;
      else
        return cubeYR * exp(log(1 - exp(log(tmp) * 2 / cubePower)) * cubePower / 2);
    }

    Vertex cubesVertex(int ia, float r, float z)
    {
      return Vertex(r * cubesRK[ia].x, r * cubesRK[ia].y, z);
    }

    void calculateCubesRK()
    {
      for (int ia = 0; ia < SLICES; ia++)
        cubesRK[ia] = Vertex(exp(log(abs(tCos(ia))) * cubePower) * sgn(tCos(ia)), exp(log(abs(tSin(ia))) * cubePower) * sgn(tSin(ia)), 1);
    }

    void generateCubes()
    {
      calculateCubesRK();
      for (int iz = 0; iz < STACKS; iz++)
        {
          float z = getZ(iz);
          r[iz] = cubesR(z);
          for (int ia = 0; ia < SLICES; ia++)
            v[iz][ia] = cubesVertex(ia, r[iz], z);
        }
      // zr = cubeZR;
      postGenerate();
    }

    void generate(float time)
    {
      float t = fmod(time, Q_ANIMATION_CYCLE) / Q_ANIMATION_CYCLE * 45.0f;
      if (t < 15)
        {
          float k = t / 15;
          c = maxC * (1 - k);
          metaBallBorder = 1.0 + (0.5 - 0.5 * (2 * abs(k - 0.5)));
          metaBallFuncFun = min(k * 2, 1);
          generateMetaBalls();
        }
      else if (t < 20)
        {
          float k = (t - 15) / 5;
          c = 0;
          metaBallBorder = 1.0;
          metaBallFuncFun = 1 - k;
          generateMetaBalls();
        }
      else if (t < 25)
        {
          float k = (t - 20) / 5;
          cubeYR = doubleMetaBallR;
          cubeZR = doubleMetaBallR;
          cubePower = 1 - k * 0.3;
          c = 0;
          generateCubes();
        }
      else if (t < 30)
        {
          float k = (t - 25) / 5;
          c = singleMetaBallR * k;
          cubeZR = doubleMetaBallR * (1 - k) + 0.5 * singleMetaBallR * k;
          cubeYR = doubleMetaBallR;
          cubePower = 0.7;
          generateCubes();
        }
      else if (t < 45)
        {
          float k = (t - 30) / 15;
          cubeZR = (0.5 + 0.5 * k) * singleMetaBallR;
          cubeYR = doubleMetaBallR * (1 - k) + singleMetaBallR * k;
          c = singleMetaBallR * (1 - k) + maxC * k;
          cubePower = 0.7 + k * 0.3;
          generateCubes();
        }
    }

    QuasiBalls()
    {
      maxC = 3.0f;

      metaBallBorder = 1.0f;
      c = maxC;
      singleMetaBallR = metaBallR(maxC);
      c = 0;
      doubleMetaBallR = metaBallR(0);

      realR[0] = 0;
      realR[STACKS + 1] = 0;
      r = &realR[1];

      for (int iz = 0; iz < STACKS; iz++)
        {
          for (int ia = 0; ia < SLICES; ia++)
            {
              t[iz][ia].y = (float) ia / SLICES;
              t[iz][ia].z = 0.0f;
            }
        }
    }
  };

// -------------------------------------------------------------------------- //
// QuasiTorus
// -------------------------------------------------------------------------- //
class QuasiTorus
  {
  private:
    Vertex v[QSTACKS][QSLICES * 2], t[QSTACKS][QSLICES * 2], n[QSTACKS][QSLICES * 2];

    float realR[QSTACKS + 2];
    float *r;

    float c; // z center coordinate

    float metaBallBorder;

    float maxC;

    float torusR;

  public:
    Quad q[QSTACKS][QSLICES * 2];

    float getZ(int iz)
    {
      return 9.0f * iz / QSTACKS - 4.5f;
    }

    float metaBallFunc(float z, float r)
    {
      return 1 / sqrt(sqr(z - c) + sqr(r)) + 1 / sqrt(sqr(z + c) + sqr(r)) - metaBallBorder;
    }

    float findLineXAxisIntersection(float x1, float y1, float x2, float y2)
    {
      return x1 - y1 * (x1 - x2) / (y1 - y2);
    }

    Vertex roundVertex(int ia, float r, float z)
    {
      return Vertex(r * tCos(ia), r * tSin(ia), z);
    }

    void postGenerate()
    {
      // Points
      for (int iz = 0; iz < QSTACKS; iz++)
        {
          float z = getZ(iz);
          float zNormal = 0.0f;
          if (r[iz] < EPS && r[iz + 1] < EPS && r[iz - 1] < EPS)
            continue;
          if (r[iz] < EPS && r[iz + 1] > EPS)
            {
              z = (getZ(iz + 1) + findLineXAxisIntersection(getZ(iz + 1), r[iz + 1], getZ(iz + 2), r[iz + 2])) / 2;
              zNormal = -1.0f;
            }
          else if (r[iz] < EPS && r[iz - 1] > EPS)
            {
              z = (getZ(iz - 1) + findLineXAxisIntersection(getZ(iz - 1), r[iz - 1], getZ(iz - 2), r[iz - 2])) / 2;
              zNormal = 1.0f;
            }

          // Vertexes
          if (r[iz] > EPS || zNormal != 0.0f)
            {
              for (int ia = 0; ia < QSLICES; ia++)
                {
                  v[iz][ia] = roundVertex(ia, torusR + r[iz], z);
                  v[iz][ia + QSLICES] = roundVertex(ia, torusR - r[iz], z);
                }
            }

          // TexCoords
          float xCoord = (atan2(abs(z) - c, r[iz])) / PI;
          for (int ia = 0; ia < QSLICES; ia++)
            {
              t[iz][ia].x = xCoord;
              t[iz][ia + QSLICES].x = xCoord;
            }

          // Normals
          if (zNormal != 0.0f)
            {
              for (int ia = 0; ia < QSLICES; ia++)
                {
                  n[iz][ia] = Vertex(0, 0, zNormal);
                  v[iz][ia].z = z;
                  n[iz][ia + QSLICES] = Vertex(0, 0, zNormal);
                  v[iz][ia + QSLICES].z = z;
                }
            }
          else
            {
              n[iz][0] = (v[iz][QSLICES - 1] - v[iz][0]) ^ (v[iz - 1][0] - v[iz][0]);
              n[iz][QSLICES] = (v[iz - 1][QSLICES] - v[iz][QSLICES]) ^ (v[iz][2 * QSLICES - 1] - v[iz][0]);
              for (int ia = 1; ia < QSLICES; ia++)
                {
                  n[iz][ia] = (v[iz][ia - 1] - v[iz][ia]) ^ (v[iz - 1][ia] - v[iz][ia]);
                  n[iz][ia + QSLICES] = (v[iz - 1][ia + QSLICES] - v[iz][ia + QSLICES]) ^ (v[iz][ia - 1 + QSLICES] - v[iz][ia + QSLICES]);
                }
            }
        }

      // Quads
      for (int iz = 0; iz < QSTACKS - 1; iz++)
        {
          if (r[iz] < EPS && r[iz + 1] < EPS)
            for (int ia = 0; ia < 2 * QSLICES; ia++)
              q[iz][ia].exists = false;
          else
            {
              for (int ia = 0; ia < QSLICES; ia++)
                {
                  q[iz][ia].v[0] = v[iz    ][ia];
                  q[iz][ia].t[0] = t[iz    ][ia];
                  q[iz][ia].n[0] = n[iz    ][ia];
                  q[iz][ia].v[1] = v[iz + 1][ia];
                  q[iz][ia].t[1] = t[iz + 1][ia];
                  q[iz][ia].n[1] = n[iz + 1][ia];
                  q[iz][ia].v[2] = v[iz + 1][(ia + 1) % QSLICES];
                  q[iz][ia].t[2] = t[iz + 1][(ia + 1) % QSLICES];
                  q[iz][ia].n[2] = n[iz + 1][(ia + 1) % QSLICES];
                  q[iz][ia].v[3] = v[iz    ][(ia + 1) % QSLICES];
                  q[iz][ia].t[3] = t[iz    ][(ia + 1) % QSLICES];
                  q[iz][ia].n[3] = n[iz    ][(ia + 1) % QSLICES];
                  q[iz][ia].exists = true;
                }
              q[iz][SLICES - 1].t[2].y = 1.0;
              q[iz][SLICES - 1].t[3].y = 1.0;
              for (int ia = QSLICES; ia < 2 * QSLICES - 1; ia++)
                {
                  q[iz][ia].v[0] = v[iz    ][ia];
                  q[iz][ia].t[0] = t[iz    ][ia];
                  q[iz][ia].n[0] = n[iz    ][ia];
                  q[iz][ia].v[1] = v[iz + 1][ia];
                  q[iz][ia].t[1] = t[iz + 1][ia];
                  q[iz][ia].n[1] = n[iz + 1][ia];
                  q[iz][ia].v[2] = v[iz + 1][ia + 1];
                  q[iz][ia].t[2] = t[iz + 1][ia + 1];
                  q[iz][ia].n[2] = n[iz + 1][ia + 1];
                  q[iz][ia].v[3] = v[iz    ][ia + 1];
                  q[iz][ia].t[3] = t[iz    ][ia + 1];
                  q[iz][ia].n[3] = n[iz    ][ia + 1];
                  q[iz][ia].exists = true;
                }
              int ia = 2 * QSLICES - 1;
              q[iz][ia].v[0] = v[iz    ][ia];
              q[iz][ia].t[0] = t[iz    ][ia];
              q[iz][ia].n[0] = n[iz    ][ia];
              q[iz][ia].v[1] = v[iz + 1][ia];
              q[iz][ia].t[1] = t[iz + 1][ia];
              q[iz][ia].n[1] = n[iz + 1][ia];
              q[iz][ia].v[2] = v[iz + 1][QSLICES];
              q[iz][ia].t[2] = t[iz + 1][QSLICES];
              q[iz][ia].n[2] = n[iz + 1][QSLICES];
              q[iz][ia].v[3] = v[iz    ][QSLICES];
              q[iz][ia].t[3] = t[iz    ][QSLICES];
              q[iz][ia].n[3] = n[iz    ][QSLICES];
              q[iz][ia].exists = true;
              q[iz][ia].t[2].y = 1.0;
              q[iz][ia].t[3].y = 1.0;
            }
        }
    }

    float metaBallR(float z)
    {
      float rd = 0, ru = 10, rc;
      float rcf = metaBallFunc(z, rd);
      if (rcf < 0)
        return 0;
      do
        {
          rc = (rd + ru) / 2;
          rcf = metaBallFunc(z, rc);
          if (rcf < 0)
            ru = rc;
          else
            rd = rc;
        }
      while (abs(rcf) > EPS);
      return rc;
    }

    void generateMetaTorus()
    {
      for (int iz = 0; iz < QSTACKS; iz++)
        {
          float z = getZ(iz);
          r[iz] = metaBallR(z);
        }
      postGenerate();
    }

    void generateByInternalTime(float t)
    {
      if (t < 10)
        {
          float k = t / 10;
          c = 0.5 * maxC * k;
          metaBallBorder = 1.0 + 1.0 * k;
          torusR = 2.2 + 1.0 * k;
          generateMetaTorus();
        }
      else
        {
          float k = (t - 10) / 10;
          c = 0.5 * maxC + 0.5 * maxC * k;
          torusR = 3.2 - 1.0 * k;
          metaBallBorder = 2.0 - 1.0 * k;
          generateMetaTorus();
        }
    }

    void generate(float time)
    {
      float t = fmod(time, Q_ANIMATION_CYCLE) / Q_ANIMATION_CYCLE * 40.0f;
      if (t < 20)
        generateByInternalTime(t);
      else
        generateByInternalTime(40 - t);
    }

    QuasiTorus()
    {
      maxC = 3.0f;

      metaBallBorder = 1.0f;

      realR[0] = 0;
      realR[QSTACKS + 1] = 0;
      r = &realR[1];

      for (int iz = 0; iz < QSTACKS; iz++)
        {
          for (int ia = 0; ia < QSLICES; ia++)
            {
              t[iz][ia].y = (float) ia / QSLICES;
              t[iz][ia].z = 0.0f;
              t[iz][ia + QSLICES].y = (float) ia / QSLICES;
              t[iz][ia].z = 0.0f;
            }
        }
    }
  };


// -------------------------------------------------------------------------- //
// Useful functions
// -------------------------------------------------------------------------- //
void glVertex(Vertex v, Vertex t, Vertex n)
{
  glNormal3f  (n.x, n.y, n.z);
  glTexCoord3f(t.x, t.y, t.z);
  glVertex3f  (v.x, v.y, v.z);
}

// -------------------------------------------------------------------------- //
// Globals
// -------------------------------------------------------------------------- //
float time, dtime;

int windX, windY;

vector<Quad*> quads;

QuasiBalls quasiBalls;
QuasiTorus quasiTorus;

Vertex camera;
Vertex light;

GLuint quasiTexture, skyTexture, floorTexture, lightTexture;

Vertex floorCoords[FLOORPARTS], floorTexCoords[FLOORPARTS], floorNormals[FLOORPARTS];

GLUquadricObj *lightSphere;

// -------------------------------------------------------------------------- //
// Drawing & Event Handlers
// -------------------------------------------------------------------------- //
void drawTranslucentObjects()
{
  //glDisable(GL_DEPTH_TEST);
  glDepthMask(GL_FALSE);
  glEnable(GL_BLEND);
  glPushMatrix();
  glTranslatef(0, 0, 2.2);
  glRotatef(90, 1, 0, 0);
  glScalef(0.5, 0.5, 0.5);
  glBindTexture(GL_TEXTURE_2D, quasiTexture);

  GLfloat modelViewH[32];
  float *modelView = (float*)(((unsigned long) modelViewH + 15) & 0xFFFFFFF0);
  glGetFloatv(GL_MODELVIEW_MATRIX, modelView);
  Vertex trans(modelView[2], modelView[6], modelView[10]);

  for (unsigned int i = 0; i < quads.size(); i++)
    quads[i]->dist = -(quads[i]->v[0] * trans);
  sort(quads.begin(), quads.end(), QuadPtrCmp());

  glBegin(GL_QUADS);
  for (unsigned int i = 0; i < quads.size(); i++)
    if (quads[i]->exists)
      for (int j = 0; j < 4; j++)
        glVertex(quads[i]->v[j], quads[i]->t[j], quads[i]->n[j]);
  glEnd();

  glPopMatrix();
  glDepthMask(GL_TRUE);
  glDisable(GL_BLEND);
  //glEnable(GL_DEPTH_TEST);
}

void drawSky()
{
  float r = 50;
  glDisable(GL_LIGHTING);
  glColor3f(1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, skyTexture);
  glBegin(GL_QUADS);
  glTexCoord2f(0, 0);
  glVertex3f( r,  r,  r);
  glTexCoord2f(1, 0);
  glVertex3f(-r,  r,  r);
  glTexCoord2f(1, 1);
  glVertex3f(-r, -r,  r);
  glTexCoord2f(0, 1);
  glVertex3f( r, -r,  r);

  glTexCoord2f(0, 0);
  glVertex3f( r,  r, -r);
  glTexCoord2f(1, 0);
  glVertex3f(-r,  r, -r);
  glTexCoord2f(1, 1);
  glVertex3f(-r, -r, -r);
  glTexCoord2f(0, 1);
  glVertex3f( r, -r, -r);

  glTexCoord2f(0, 0);
  glVertex3f(-r, -r, -r);
  glTexCoord2f(1, 0);
  glVertex3f(-r,  r, -r);
  glTexCoord2f(1, 1);
  glVertex3f(-r,  r,  r);
  glTexCoord2f(0, 1);
  glVertex3f(-r, -r,  r);

  glTexCoord2f(0, 0);
  glVertex3f( r,  r, -r);
  glTexCoord2f(1, 0);
  glVertex3f( r, -r, -r);
  glTexCoord2f(1, 1);
  glVertex3f( r, -r,  r);
  glTexCoord2f(0, 1);
  glVertex3f( r,  r,  r);

  glTexCoord2f(0, 0);
  glVertex3f(-r,  r, -r);
  glTexCoord2f(1, 0);
  glVertex3f( r,  r, -r);
  glTexCoord2f(1, 1);
  glVertex3f( r,  r,  r);
  glTexCoord2f(0, 1);
  glVertex3f(-r,  r,  r);

  glTexCoord2f(0, 0);
  glVertex3f( r, -r, -r);
  glTexCoord2f(1, 0);
  glVertex3f(-r, -r, -r);
  glTexCoord2f(1, 1);
  glVertex3f(-r, -r,  r);
  glTexCoord2f(0, 1);
  glVertex3f( r, -r,  r);
  glEnd();
  glEnable(GL_LIGHTING);
}


void preCalcFloorTexCoords()
{
  for (int i = 0; i < FLOORPARTS; i++)
    {
      float a = 2 * PI * i / FLOORPARTS;
      float r = 1.5;
      floorTexCoords[i] = Vertex(r * cos(a), r * sin(a), 0);
    }
}

void preCalcFloor()
{
  for (int i = 0; i < FLOORPARTS; i++)
    {
      float a = 2 * PI * i / FLOORPARTS;
      float tmp = 6 * a / (2 * PI);
      float tmp2 = abs(tmp - (int)tmp - 0.5) * 2;
      float r = 3.0 + 1.5 * sin(1.0f * time) * tmp2 * tmp2 * tmp2;
      floorCoords[i] = Vertex(r * cos(a), r * sin(a), 0);
    }
  for (int i = 0; i < FLOORPARTS; i++)
    {
      int i1 = (i + 1) % FLOORPARTS;
      floorNormals[i] = Vertex(0, 0, 1) ^ (floorCoords[i] - floorCoords[i1]);
    }
}

void drawFloor()
{
  glBindTexture(GL_TEXTURE_2D, floorTexture);
  glBegin(GL_TRIANGLE_FAN);
  glNormal3f(0, 0, 1);
  glTexCoord2f(0, 0);
  glVertex3f(0, 0, 0);
  for (int i = 0; i < FLOORPARTS; i++)
    {
      glTexCoord2f(floorTexCoords[i].x, floorTexCoords[i].y);
      glVertex3f(floorCoords[i].x, floorCoords[i].y, floorCoords[i].z);
    }
  glTexCoord2f(floorTexCoords[0].x, floorTexCoords[0].y);
  glVertex3f(floorCoords[0].x, floorCoords[0].y, floorCoords[0].z);
  glEnd();
}

void drawFloorColumn()
{
  glBindTexture(GL_TEXTURE_2D, floorTexture);
  for (int j = 0; j < 15; j++)
    {
      glBegin(GL_QUAD_STRIP);
      for (int i = 0; i < FLOORPARTS + 1; i++)
        {
          int i0 = i % FLOORPARTS;
          glNormal3f(floorNormals[i0].x, floorNormals[i0].y, 0);
          glTexCoord2f(floorTexCoords[i0].x * (1 + 0.5 * j), floorTexCoords[i0].y * (1 + 0.5 * j));
          glVertex3f(floorCoords[i0].x, floorCoords[i0].y, floorCoords[i0].z - j);
          glTexCoord2f(floorTexCoords[i0].x * (1 + 0.5 * (j + 1)), floorTexCoords[i0].y * (1 + 0.5 * (j + 1)));
          glVertex3f(floorCoords[i0].x, floorCoords[i0].y, floorCoords[i0].z - j - 1);
        }
      glEnd();
    }
}

void drawLight()
{

  GLfloat modelViewH[32];
  float *modelView = (float*)(((unsigned long) modelViewH + 15) & 0xFFFFFFF0);
  glGetFloatv(GL_MODELVIEW_MATRIX, modelView);
  Vertex right = Vertex(modelView[0], modelView[4], modelView[8]).normalize() * 0.2;
  Vertex up = Vertex(modelView[1], modelView[5], modelView[9]).normalize() * 0.2;

  glMatrixMode(GL_TEXTURE);
  glPushMatrix();
  glTranslatef(0.5, 0.5, 0);
  glRotatef(time * 100, 0, 0, 1);
  glTranslatef(-0.5, -0.5, 0);

  glDisable(GL_DEPTH_TEST);
  glBindTexture(GL_TEXTURE_2D, lightTexture);
  glEnable(GL_BLEND);
  glDisable(GL_LIGHTING);
  glColor4f(1, 1, 1, 1);
  glBegin(GL_QUADS);
  glVertex(light + up + right, Vertex(0, 0, 0), Vertex(0, 0, 1));
  glVertex(light - up + right, Vertex(1, 0, 0), Vertex(0, 0, 1));
  glVertex(light - up - right, Vertex(1, 1, 0), Vertex(0, 0, 1));
  glVertex(light + up - right, Vertex(0, 1, 0), Vertex(0, 0, 1));
  glEnd();
  glEnable(GL_LIGHTING);
  glDisable(GL_BLEND);
  glEnable(GL_DEPTH_TEST);

  glPopMatrix();
  glMatrixMode(GL_MODELVIEW);
}


void display()
{
  static unsigned long lastTC = GetTickCount();
  static unsigned long fpsFrames = 0;
  static unsigned long lastFpsTC = GetTickCount();
  unsigned long tc = GetTickCount();
  dtime = (tc - lastTC) / 1000.0f;
  time += dtime;
  lastTC = tc;
  fpsFrames++;
  if (tc - lastFpsTC > 1000)
    {
      char header[100];
      sprintf(header, "QuasiBalls. %f fps.", 1000.0f * fpsFrames / (tc - lastFpsTC));
      glutSetWindowTitle(header);
      lastFpsTC = tc;
      fpsFrames = 0;
    }

  preCalcFloor();
  quasiBalls.generate(time);
  quasiTorus.generate(time);

  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

  camera = Vertex(6 * cos(0.2 * time), 6 * sin(0.2 * time), 4.5);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(80, (float) windX / windY, 0.1, 100);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(camera.x, camera.y, camera.z, 0, 0, 2, 0, 0, 1);

  light = Vertex(2 * cos(2 * time), 2 * sin(2 * time), 4.5);
  GLfloat lPos[4] = {light.x, light.y, light.z, 1.0f};
  glLightfv(GL_LIGHT0, GL_POSITION, lPos);

  // Stencil Floor
  glDepthMask(GL_FALSE);
  glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
  glEnable(GL_STENCIL_TEST);
  glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);
  glStencilFunc(GL_ALWAYS, 1, 1);
  drawFloor();
  glDepthMask(GL_TRUE);
  glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);

  // Mirrored World
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  glStencilFunc(GL_EQUAL, 1, 1);
  glPushMatrix();
  glScalef(1, 1, -1);
  glLightfv(GL_LIGHT0, GL_POSITION, lPos);
  drawSky();
  drawLight();
  drawTranslucentObjects();
  glPopMatrix();
  glDisable(GL_STENCIL_TEST);

  // Floor
  glEnable(GL_BLEND);
  drawFloor();
  glDisable(GL_BLEND);

  // Normal World
  glLightfv(GL_LIGHT0, GL_POSITION, lPos);
  drawSky();
  drawFloorColumn();
  drawLight();
  drawTranslucentObjects();

  glFlush();
  glutSwapBuffers();
}


void GLUTCALLBACK keydown(unsigned char key, int x, int y)
{
  if (key == 27)
    exit(0);
}

void GLUTCALLBACK reshape(int w, int h)
{
  glViewport(0, 0, w, h);
  windX = w;
  windY = h;
}

void addAlpha(AUX_RGBImageRec* dst, AUX_RGBImageRec* alpha)
{
  unsigned char *result = new unsigned char [dst->sizeX * dst->sizeY * 4];
  for (int i = 0; i < dst->sizeX * dst->sizeY; i++)
    {
      result[4 * i]     = dst->data[3 * i];
      result[4 * i + 1] = dst->data[3 * i + 1];
      result[4 * i + 2] = dst->data[3 * i + 2];
      result[4 * i + 3] = (unsigned char) alpha->data[3 * i];
    }
  dst->data = result;
}

void loadTexture(GLuint texId, GLint sParam, GLint tParam, GLint components, GLenum format, AUX_RGBImageRec* image)
{
  glBindTexture(GL_TEXTURE_2D, texId);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, sParam);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, tParam);
  gluBuild2DMipmaps(GL_TEXTURE_2D, components, image->sizeX, image->sizeY, format, GL_UNSIGNED_BYTE, image->data);
}

int main(int argc, char** argv)
{
  // libc init
  srand(GetTickCount());

  // Init GLUT
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_DEPTH | GLUT_RGB | GLUT_MULTISAMPLE | GLUT_STENCIL);
  glutInitWindowSize(WINDOW_SIZE_X, WINDOW_SIZE_Y);
  glutCreateWindow("QuasiBalls");
  glutDisplayFunc(display);
  glutReshapeFunc(reshape);
  glutIdleFunc(display);
  glutKeyboardFunc(keydown);
  glutSetCursor(GLUT_CURSOR_NONE);

  DEVMODE devMode;
  memset(&devMode, 0, sizeof(devMode));
  devMode.dmSize = sizeof(devMode);
  devMode.dmPelsWidth = WINDOW_SIZE_X;
  devMode.dmPelsHeight = WINDOW_SIZE_Y;
  devMode.dmBitsPerPel = 32;
  devMode.dmFields = DM_BITSPERPEL | DM_PELSWIDTH | DM_PELSHEIGHT;
  ChangeDisplaySettings(&devMode, CDS_FULLSCREEN);
  glutFullScreen();

  // Init GL
  glClearColor(0, 0, 0, 0);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_TEXTURE_2D);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  glShadeModel(GL_SMOOTH);
  glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_NORMALIZE);

  // Lights
  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHTING);
  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, 0);
  GLfloat dL[4] = {1.0, 1.0, 1.0, 1.0};
  GLfloat sL[4] = {1.0, 1.0, 1.0, 0.0};
  GLfloat aL[4] = {0.0, 0.0, 0.0, 0.0};
  glLightfv(GL_LIGHT0, GL_DIFFUSE, dL);
  glLightfv(GL_LIGHT0, GL_SPECULAR, sL);
  glLightfv(GL_LIGHT0, GL_AMBIENT, aL);
  glLightf(GL_LIGHT0, GL_QUADRATIC_ATTENUATION, 0.04);

  // Materials
  GLfloat eM[4] = {0.4f, 0.4f, 0.4f, 0.0f};
  GLfloat dM[4] = {1.0f, 1.0f, 1.0f, 0.6f};
  GLfloat sM[4] = {0.4f, 0.4f, 0.4f, 0.0f};
  GLfloat aM[4] = {0.0f, 0.0f, 0.0f, 0.0f};
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, eM);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, dM);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, sM);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, aM);
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 10);

  // Load textures
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  AUX_RGBImageRec *image, *alpha;

  glGenTextures(1, &quasiTexture);
  image = auxDIBImageLoad("data/quasi.bmp");
  alpha = auxDIBImageLoad("data/quasi_alpha.bmp");
  addAlpha(image, alpha);
  loadTexture(quasiTexture, GL_REPEAT, GL_REPEAT, 4, GL_RGBA, image);

  glGenTextures(1, &skyTexture);
  image = auxDIBImageLoad("data/sky.bmp");
  loadTexture(skyTexture, GL_CLAMP, GL_CLAMP, 3, GL_RGB, image);

  glGenTextures(1, &floorTexture);
  image = auxDIBImageLoad("data/floor.bmp");
  loadTexture(floorTexture, GL_REPEAT, GL_REPEAT, 3, GL_RGB, image);

  glGenTextures(1, &lightTexture);
  image = auxDIBImageLoad("data/light.bmp");
  alpha = auxDIBImageLoad("data/light_alpha.bmp");
  addAlpha(image, alpha);
  loadTexture(lightTexture, GL_CLAMP, GL_CLAMP, 4, GL_RGBA, image);

  // Init our data
  windX = WINDOW_SIZE_X;
  windY = WINDOW_SIZE_Y;
  time = 0;
  for (int i = 0; i < STACKS; i++)
    for (int j = 0; j < SLICES; j++)
      quads.push_back(&quasiBalls.q[i][j]);

  for (int i = 0; i < QSTACKS; i++)
    for (int j = 0; j < QSLICES * 2; j++)
      quads.push_back(&quasiTorus.q[i][j]);

  preCalcFloorTexCoords();

  lightSphere = gluNewQuadric();

  glutMainLoop();
  return 0;
}