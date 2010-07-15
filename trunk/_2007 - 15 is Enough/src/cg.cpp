#include <gl/glut.h>
#include <gl/gl.h>
#include <gl/glu.h>
#include <gl/glaux.h>
#include <gl/glext.h>
#include <cmath>
#include <cstdlib>
#include <vector>
#include <algorithm>
#pragma comment(lib, "glaux.lib")

#define SCENE_TIME 8.0f

#ifndef M_PI
#  define M_PI 3.141592f
#endif

using namespace std;

class Vector
{
public:
    float x, y, z;
    Vector() {}
    Vector(float x, float y, float z): x(x), y(y), z(z) {}
    Vector operator+ (Vector a)
    {
        return Vector(x + a.x, y + a.y, z + a.z);
    }
    Vector operator- ()
    {
        return Vector(-x, -y, -z);
    }
    Vector operator- (Vector a)
    {
        return *this + (-a);
    }
    Vector operator* (float a)
    {
        return Vector(x * a, y * a, z * a);
    }
    Vector operator/ (float a)
    {
        return *this * (1 / a);
    }
    float operator* (Vector a)
    {
        return x * a.x + y * a.y + z * a.z;
    }
    float lenghtSq()
    {
        return *this * *this;
    }
    float length()
    {
        return sqrt(lenghtSq());
    }
    Vector normalize()
    {
        return *this / length();
    }
    Vector operator^ (Vector a)
    {
        return Vector(y*a.z - z*a.y, z*a.x - x*a.z, x*a.y - y*a.x);
    }
    float* asArray(float w = 1.0f)
    {
        static float a[4];
        a[0] = x;
        a[1] = y;
        a[2] = z;
        a[3] = w;
        return a;
    }
};

class Particle
{
public:
    Vector coord, speed, color;
};

float t, dt;

int wnd_w, wnd_h;

GLuint texText, texSphere, texFloor, texTree, texGrass, texNX, texPX, texNY, texPY, texNZ, texPZ, texKnot, texFin;

Vector lightPos;

inline Vector vec(float x, float y, float z)
{
    return Vector(x, y, z);
}

float random(float a)
{
    return a * rand() / RAND_MAX;
}

void drawText(int index)
{
    glPushMatrix();
    glLoadIdentity();
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_LIGHTING);
    glDisable(GL_TEXTURE_2D);
    glBegin(GL_QUADS);
    glColor3f(0, 0, 0);
    glVertex3f(-3, 0.8, -1);
    glVertex3f( 3, 0.8, -1);
    glVertex3f( 3, 1.0, -1);
    glVertex3f(-3, 1.0, -1);
    glVertex3f(-3, -0.8, -1);
    glVertex3f( 3, -0.8, -1);
    glVertex3f( 3, -1.0, -1);
    glVertex3f(-3, -1.0, -1);
    glEnd();
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, texText);
    glBegin(GL_QUADS);
    glColor3f(1, 1, 1);
    glTexCoord2f(0, 1.0f - 1.0f * (index + 1) / 10);
    glVertex3f(-1, 0.8, -1);
    glTexCoord2f(1, 1.0f - 1.0f * (index + 1) / 10);
    glVertex3f( 1, 0.8, -1);
    glTexCoord2f(1, 1.0f - 1.0f * (index    ) / 10);
    glVertex3f( 1, 1.0, -1);
    glTexCoord2f(0, 1.0f - 1.0f * (index    ) / 10);
    glVertex3f(-1, 1.0, -1);
    glEnd();
    glEnable(GL_LIGHTING);
    glEnable(GL_DEPTH_TEST);
    glPopMatrix();
}

void defaultMaterial()
{
    glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, vec(0.0, 0.0, 0.0).asArray());
    glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE,  vec(0.3, 0.3, 0.3).asArray(0.5));
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, vec(0.5, 0.5, 0.5).asArray());
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT,  vec(0.3, 0.3, 0.3).asArray());
    glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 24);
}

void placeLight(Vector v, Vector c = vec(1, 1, 1))
{
    lightPos = v;
    glLightfv(GL_LIGHT0, GL_DIFFUSE,  c.asArray());
    glLightfv(GL_LIGHT0, GL_SPECULAR, c.asArray());
    glLightfv(GL_LIGHT0, GL_AMBIENT,  c.asArray());
    glLightfv(GL_LIGHT0, GL_POSITION, v.asArray());
    static GLUquadric* qSphere = gluNewQuadric();
    glDisable(GL_LIGHTING);
    glDisable(GL_TEXTURE_2D);
    glPushMatrix();
    glTranslatef(v.x, v.y, v.z);
    glColor3f(c.x, c.y, c.z);
    gluSphere(qSphere, 0.05, 8, 8);
    glPopMatrix();
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_LIGHTING);
}

GLUquadric* gluNewTexturedQuadric()
{
    GLUquadric* q = gluNewQuadric();
    gluQuadricTexture(q, GL_TRUE);
    return q;
}

void placeSphere(Vector v, float r, GLuint texture, int slices = 16, int stacks = 16)
{
    static GLUquadric* qSphere = gluNewTexturedQuadric();
    glBindTexture(GL_TEXTURE_2D, texture);
    glPushMatrix();
    glTranslatef(v.x, v.y, v.z);
    gluSphere(qSphere, r, slices, stacks);
    glPopMatrix();
}

void clearScreen(float r, float g, float b, float a)
{
    glClearColor(r, g, b, a);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
}

void fadeInOut(float t)
{
    if (t < 1 || t > SCENE_TIME - 1)
    {
        float alpha = max(max(1 - t, 0), max(t - (SCENE_TIME - 1), 0));
        glDisable(GL_DEPTH_TEST);
        glDisable(GL_LIGHTING);
        glDisable(GL_TEXTURE_2D);
        glEnable(GL_BLEND);
        glPushMatrix();
        glLoadIdentity();
        glBegin(GL_QUADS);
        glColor4f(0, 0, 0, alpha);
        glVertex3f(-2, -2, -1);
        glVertex3f( 2, -2, -1);
        glVertex3f( 2,  2, -1);
        glVertex3f(-2,  2, -1);
        glEnd();
        glPopMatrix();
        glDisable(GL_BLEND);
        glEnable(GL_TEXTURE_2D);
        glEnable(GL_LIGHTING);
        glEnable(GL_DEPTH_TEST);
    }
}

void placeZPlane(GLuint texture, float texXScale, float texYScale)
{
    glBindTexture(GL_TEXTURE_2D, texture);
    glBegin(GL_QUADS);
    glNormal3f(0, 0, 1);
    for (int i = -20; i < 20; i++)
    {
        for (int j = -20; j < 20; j++)
        {
            glTexCoord2f((i  ) * texXScale, (j  ) * texYScale);
            glVertex3f(i  , j  , 0);
            glTexCoord2f((i+1) * texXScale, (j  ) * texYScale);
            glVertex3f(i+1, j  , 0);
            glTexCoord2f((i+1) * texXScale, (j+1) * texYScale);
            glVertex3f(i+1, j+1, 0);
            glTexCoord2f((i  ) * texXScale, (j+1) * texYScale);
            glVertex3f(i  , j+1, 0);
        }
    }
    glEnd();
}

void placeCamera(Vector eye, Vector center, float angle)
{
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(angle, (float) wnd_w / wnd_h, 0.1, 100);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    gluLookAt(eye.x, eye.y, eye.z, center.x, center.y, center.z, 0, 0, 1);
}

void scene0()
{
    clearScreen(0, 0, 0, 0);
    placeCamera(vec(3 * sin(t), 3 * cos(t), 2), vec(0, 0, 0), 90);
    placeLight(vec(sin(2 * t), cos(3 * t), 1), vec(1, 1, 0));
    placeSphere(vec(0, 0, 0), 0.5, texSphere);
}

void scene1Sphere()
{
    placeSphere(vec(0, 0, 1), 0.5, texSphere);
}

void shadow(void (*renderFunc)(void))
{
    glDisable(GL_LIGHTING);
    glDisable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glEnable(GL_STENCIL_TEST);
    glStencilFunc(GL_EQUAL, 0, 1);
    glStencilOp(GL_KEEP, GL_KEEP, GL_INCR);
    glEnable(GL_POLYGON_OFFSET_FILL);
    glPolygonOffset(-5, -5);
    glColor4f(0, 0, 0, 0.4);
    glPushMatrix();
    float projMatrix[16] = {lightPos.z, 0, 0, 0, 0, lightPos.z, 0, 0, -lightPos.x, -lightPos.y, 0, -1, 0, 0, 0, lightPos.z};
    glMultMatrixf(projMatrix);
    renderFunc();
    glPopMatrix();
    glDisable(GL_POLYGON_OFFSET_FILL);
    glDisable(GL_STENCIL_TEST);
    glDisable(GL_BLEND);
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_LIGHTING);
}

void scene1()
{
    clearScreen(0, 0, 0, 0);

    placeCamera(vec(3 * sin(t), 3 * cos(t), 2), vec(0, 0, 1), 90);
    placeLight(vec(sin(2 * t), cos(3 * t), 2), vec(1, 1, 1));
    scene1Sphere();

    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT,  vec(0, 0, 0).asArray());
    glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 4);
    placeZPlane(texFloor, 0.5, 0.5);
    defaultMaterial();

    shadow(scene1Sphere);
}

Particle scene2NewParticle()
{
    Particle p;
    p.coord = vec(random(0.2) - 0.1, random(0.2) - 0.1, random(0.2) - 0.1);
    p.color = vec(0, 0, 1);
    p.speed = vec(random(1.0) - 0.5, random(1.0) - 0.5, 3 + random(3.0));
    return p;
}

void scene2MoveParticles(vector<Particle>& p, float dt)
{
    for (unsigned int i = 0; i < p.size(); i++)
    {
        p[i].coord = p[i].coord + p[i].speed * dt;
        p[i].speed.z -= dt * 5;
        if (p[i].speed.z < 0)
            p[i].color.z -= dt;
        if (p[i].color.z < 0)
            p[i] = scene2NewParticle();
    }
}

vector<Particle> scene2GenParticleVector()
{
    vector<Particle> result;
    for (int i = 0; i < 200; i++)
        result.push_back(scene2NewParticle());
    for (int i = 0; i < 500; i++)
        scene2MoveParticles(result, 0.05);
    return result;
}

void scene2()
{
    static vector<Particle> particles = scene2GenParticleVector();
    scene2MoveParticles(particles, dt);

    clearScreen(0, 0, 0, 0);
    placeCamera(vec(5 * sin(t), 5 * cos(t), 5), vec(0, 0, 2), 90);
    placeLight(vec(3 * sin(3 * t), 3 * cos(1 * t), 2), vec(1, 1, 1));

    glDisable(GL_TEXTURE_2D);
    for (unsigned int i = 0; i < particles.size(); i++)
    {
        glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT,  (particles[i].color / 2).asArray());
        glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE,  (particles[i].color / 2).asArray());
        placeSphere(particles[i].coord, 0.1, 0, 8, 8);
    }
    glEnable(GL_TEXTURE_2D);
    defaultMaterial();
}

void scene3()
{
    clearScreen(0, 0, 0.3, 0);
    placeCamera(vec(-3, 0, 3), vec(0, 0, 1), 90);
    placeLight(vec(sin(3.0 * t), cos(3.0 * t), 2), vec(1, 1, 1));

    glEnable(GL_FOG);
    glFogfv(GL_FOG_COLOR, vec(0, 0, 0.3).asArray());
    glFogf(GL_FOG_START, 3.0f);
    glFogf(GL_FOG_END, 9.0f);
    glFogi(GL_FOG_MODE, GL_LINEAR);

    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT,  vec(0, 0, 0).asArray());
    glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 4);
    placeZPlane(texFloor, 0.5, 0.5);
    defaultMaterial();

    placeSphere(vec(3 - 3 * cos(3 * t), 2 *sin(3 * t), 1), 0.5, texSphere);
    glDisable(GL_FOG);
}

void scene4SkyBox()
{
    float w = 20;
    float h = 20;
    float d = 20;
    float x = - w / 2;
    float y = - h / 2;
    float z = - d / 2;

    glBindTexture(GL_TEXTURE_2D, texNZ);
    glBegin(GL_QUADS);
    glNormal3f(0,0,1);
    glTexCoord2f(1.0f, 0.0f);
    glVertex3f(x + w, y,     z);
    glTexCoord2f(1.0f, 1.0f);
    glVertex3f(x + w, y + h, z);
    glTexCoord2f(0.0f, 1.0f);
    glVertex3f(x,     y + h, z);
    glTexCoord2f(0.0f, 0.0f);
    glVertex3f(x,     y,     z);
    glEnd();

    glBindTexture(GL_TEXTURE_2D, texPZ);
    glBegin(GL_QUADS);
    glNormal3f(0,0,1);
    glTexCoord2f(0.0f, 1.0f);
    glVertex3f(x,     y,     z + d);
    glTexCoord2f(0.0f, 0.0f);
    glVertex3f(x,     y + h, z + d);
    glTexCoord2f(1.0f, 0.0f);
    glVertex3f(x + w, y + h, z + d);
    glTexCoord2f(1.0f, 1.0f);
    glVertex3f(x + w, y,     z + d);
    glEnd();

    glBindTexture(GL_TEXTURE_2D, texPY);
    glBegin(GL_QUADS);
    glNormal3f(0,0,1);
    glTexCoord2f(0.0f, 0.0f);
    glVertex3f(x + w, y + h, z);
    glTexCoord2f(0.0f, 1.0f);
    glVertex3f(x + w, y + h, z + d);
    glTexCoord2f(1.0f, 1.0f);
    glVertex3f(x,     y + h, z + d);
    glTexCoord2f(1.0f, 0.0f);
    glVertex3f(x,     y + h, z);
    glEnd();

    glBindTexture(GL_TEXTURE_2D, texNY);
    glBegin(GL_QUADS);
    glNormal3f(0,0,1);
    glTexCoord2f(0.0f, 0.0f);
    glVertex3f(x,     y, z);
    glTexCoord2f(0.0f, 1.0f);
    glVertex3f(x,     y, z + d);
    glTexCoord2f(1.0f, 1.0f);
    glVertex3f(x + w, y, z + d);
    glTexCoord2f(1.0f, 0.0f);
    glVertex3f(x + w, y, z);
    glEnd();

    glBindTexture(GL_TEXTURE_2D, texNX);
    glBegin(GL_QUADS);
    glNormal3f(0,0,1);
    glTexCoord2f(0.0f, 0.0f);
    glVertex3f(x, y + h, z);
    glTexCoord2f(0.0f, 1.0f);
    glVertex3f(x, y + h, z + d);
    glTexCoord2f(1.0f, 1.0f);
    glVertex3f(x, y,     z + d);
    glTexCoord2f(1.0f, 0.0f);
    glVertex3f(x, y,     z);
    glEnd();

    glBindTexture(GL_TEXTURE_2D, texPX);
    glBegin(GL_QUADS);
    glNormal3f(0,0,1);
    glTexCoord2f(0.0f, 0.0f);
    glVertex3f(x + w, y,     z);
    glTexCoord2f(0.0f, 1.0f);
    glVertex3f(x + w, y,     z + d);
    glTexCoord2f(1.0f, 1.0f);
    glVertex3f(x + w, y + h, z + d);
    glTexCoord2f(1.0f, 0.0f);
    glVertex3f(x + w, y + h, z);
    glEnd();
}

void scene4()
{
    clearScreen(0, 0, 0.3, 0);
    placeCamera(vec(2 * sin(t), 2 * cos(t), 1 + 2 * sin(t)), vec(0, 0, 1), 90);
    placeLight(vec(sin(3.0 * t), cos(3.0 * t), 2), vec(0.6, 0.6, 1.0));

    glLightfv(GL_LIGHT0, GL_AMBIENT,  vec(0, 0, 0).asArray());
    glEnable(GL_LIGHT1);
    glLightfv(GL_LIGHT1, GL_AMBIENT,  vec(0.5, 0.5, 0.5).asArray());
    glLightfv(GL_LIGHT1, GL_DIFFUSE,  vec(1, 1, 0).asArray());
    glLightfv(GL_LIGHT1, GL_SPECULAR, vec(1, 1, 0).asArray());
    glLightfv(GL_LIGHT1, GL_POSITION, vec(-5, 20, 0).asArray());
    placeSphere(vec(0, 0, 1), 0.5, texSphere);
    glDisable(GL_LIGHT1);

    glDisable(GL_LIGHTING);
    glColor3f(1, 1, 1);
    scene4SkyBox();
    glEnable(GL_LIGHTING);
}

void scene5SubScene()
{
    Vector s(sin(t), cos(3 * t), 1.5);
    Vector l = s + vec(sin(3 * t), cos(3 * t), sin(3 * t));
    placeLight(l, vec(1, 1, 1));
    placeSphere(s, 0.5, texSphere);
}

void scene5()
{
    clearScreen(0, 0, 0, 0);
    placeCamera(vec(3 * sin(t), 3 * cos(t), 3), vec(0, 0, 1), 90);

    glPushMatrix();
    glScalef(1, 1, -1);
    scene5SubScene();
    glPopMatrix();

    scene5SubScene();

    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT,  vec(0, 0, 0).asArray(0.5));
    glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 4);
    glEnable(GL_BLEND);
    placeZPlane(texFloor, 0.5, 0.5);
    glDisable(GL_BLEND);
    defaultMaterial();
}

vector<Vector> scene6GenTrees()
{
    vector<Vector> result;
    for (int i = 0; i < 100; i++)
    {
        Vector p = vec(random(30) - 15, random(30) - 15, 0);
        if (p.lenghtSq() > 7)
            result.push_back(p);
    }
    return result;
}

void scene6()
{
    static vector<Vector> trees = scene6GenTrees();

    Vector forward = vec(sin(0.5 * t), cos(0.5 * t), 0);
    Vector left = (forward ^ vec(0, 0, 1)).normalize();

    clearScreen(0, 0.1, 0, 0);
    placeCamera(vec(0, 0, 1), vec(0, 0, 1) + forward, 90);
    placeLight(forward * 2 + vec(sin(3 * t), cos(3 * t), 3) * 0.5, vec(0.7, 0.9, 0.7));

    class sorter
    {
    private:
        Vector dotVector;
    public:
        sorter()
        {
            static float modelView[16];
            glGetFloatv(GL_MODELVIEW_MATRIX, modelView);
            dotVector = vec(modelView[2], modelView[6], modelView[10]);
        }
        bool operator() (Vector& a, Vector& b)
        {
            return a * dotVector < b * dotVector;
        }
    };
    sort(trees.begin(), trees.end(), sorter());

    glEnable(GL_FOG);
    glFogfv(GL_FOG_COLOR, vec(0, 0.1, 0).asArray());
    glFogf(GL_FOG_START, 5.0f);
    glFogf(GL_FOG_END, 20.0f);
    glFogi(GL_FOG_MODE, GL_LINEAR);

    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT,  vec(0, 0, 0).asArray());
    glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 4);
    placeZPlane(texGrass, 0.5, 0.5);
    defaultMaterial();

    glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE,  vec(0.5, 0.5, 0.5).asArray(1.0));
    glBindTexture(GL_TEXTURE_2D, texTree);
    glDepthMask(GL_FALSE);
    glEnable(GL_BLEND);
    glBegin(GL_QUADS);
    for (unsigned int i = 0; i < trees.size(); i++)
    {
        Vector v00 = trees[i] - left * 1.2f;
        Vector v01 = trees[i] + left * 1.2f;
        Vector v11 = trees[i] + left * 1.2f + vec(0, 0, 3);
        Vector v10 = trees[i] - left * 1.2f + vec(0, 0, 3);
        glTexCoord2f(1, 0);
        glVertex3f(v00.x, v00.y, v00.z);
        glTexCoord2f(1, 1);
        glVertex3f(v10.x, v10.y, v10.z);
        glTexCoord2f(0, 1);
        glVertex3f(v11.x, v11.y, v11.z);
        glTexCoord2f(0, 0);
        glVertex3f(v01.x, v01.y, v01.z);
    }
    glEnd();
    glDepthMask(GL_TRUE);
    glDisable(GL_BLEND);
    defaultMaterial();

    glDisable(GL_FOG);
}

Vector scene7KnotVertex(float pu, float pv)
{
    float u = pu * 12 * M_PI;
    float v = pv * 2 * M_PI;
    static const float R = 2;
    static const float r = 0.3;
    Vector result;
    result.x = (R + 1.5 * r * cos(u / 2)) * cos(u / 3) + r * cos(u / 3) * cos(v - M_PI);
    result.y = (R + 1.5 * r * cos(u / 2)) * sin(u / 3) + r * sin(u / 3) * cos(v - M_PI);
    result.z = (    2.5 * r * sin(u / 2))              + r * 1.0f       * sin(v - M_PI) + 3;
    return result;
}

Vector scene7KnotNormal(float pu, float pv)
{
    Vector current = scene7KnotVertex(pu, pv);
    return (scene7KnotVertex(pu + 0.0001, pv) - current) ^ (scene7KnotVertex(pu, pv + 0.0001) - current);
}

void scene7KnotGlPoint(float pu, float pv)
{
    glTexCoord2f(pu * 12, pv);
    Vector n = scene7KnotNormal(pu, pv);
    glNormal3f(n.x, n.y, n.z);
    Vector v = scene7KnotVertex(pu, pv);
    glVertex3f(v.x, v.y, v.z);
}

GLuint scene7GenList()
{
    static const int stacks = 256;
    static const int slices = 16;
    GLuint result = 1;
    glNewList(result, GL_COMPILE);
    glBegin(GL_QUADS);
    for (int i = 0; i < stacks; i++)
    {
        for (int j = 0; j < slices; j++)
        {
            scene7KnotGlPoint((float) (i  ) / stacks, (float) (j  ) / slices);
            scene7KnotGlPoint((float) (i+1) / stacks, (float) (j  ) / slices);
            scene7KnotGlPoint((float) (i+1) / stacks, (float) (j+1) / slices);
            scene7KnotGlPoint((float) (i  ) / stacks, (float) (j+1) / slices);
        }
    }
    glEnd();
    glEndList();
    return result;
}

void scene7Knot()
{
    glCallList(1);
}

void scene7()
{
    static GLuint knotList = scene7GenList();

    clearScreen(0, 0, 0, 0);
    placeCamera(vec(6 * sin(t), 6 * cos(t), 5 + 4 * sin(t)), vec(0, 0, 3), 90);
    placeLight(vec(3 * sin(3 * t), 3 * cos(2 * t), 4.5), vec(1, 1, 1));

    glBindTexture(GL_TEXTURE_2D, texKnot);
    glCallList(knotList);

    glBindTexture(GL_TEXTURE_2D, texFloor);
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT,  vec(0, 0, 0).asArray());
    glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 4);
    placeZPlane(texFloor, 0.5, 0.5);
    defaultMaterial();

    shadow(scene7Knot);
}

void scene8()
{
    clearScreen(0, 0, 0, 0);
    placeCamera(vec(1, 1, 1), vec(0, 0, 0), 90);
    glBindTexture(GL_TEXTURE_2D, texFin);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_LIGHTING);
    glPushMatrix();
    glLoadIdentity();
    glBegin(GL_QUADS);
    glColor3f(1, 1, 1);
    glTexCoord2f(0, 0);
    glVertex3f(-1, -1, -1);
    glTexCoord2f(1, 0);
    glVertex3f( 1, -1, -1);
    glTexCoord2f(1, 1);
    glVertex3f( 1,  1, -1);
    glTexCoord2f(0, 1);
    glVertex3f(-1,  1, -1);
    glEnd();
    glPopMatrix();
    glEnable(GL_LIGHTING);
    glEnable(GL_DEPTH_TEST);
}

void GLUTCALLBACK displayHandler(void)
{
    // if (t < 6 * SCENE_TIME) t = 6 * SCENE_TIME;

    int sceneN = (int) (t / SCENE_TIME);
    float sceneT = fmod(t, SCENE_TIME);

    switch (sceneN)
    {
    case 0:
        scene0();
        break;
    case 1:
        scene1();
        break;
    case 2:
        scene2();
        break;
    case 3:
        scene3();
        break;
    case 4:
        scene4();
        break;
    case 5:
        scene5();
        break;
    case 6:
        scene6();
        break;
    case 7:
        scene7();
        break;
    case 8:
        scene8();
        break;
    default:
        exit(0);
    }

    drawText(sceneN);
    fadeInOut(sceneT);
    glutSwapBuffers();
}


void GLUTCALLBACK reshapeHandler(int w, int h)
{
    glViewport(0, 0, w, h);
    wnd_w = w;
    wnd_h = h;
}

void GLUTCALLBACK idleHandler()
{
    static int LastTC = GetTickCount();
    dt = (float) (GetTickCount() - LastTC) / 1000.0f;
    t += dt;
    LastTC = GetTickCount();
    glutPostRedisplay();
}

void GLUTCALLBACK keyHandler(unsigned char key, int x, int y)
{
    if (key == 27)
        exit(0);
}

GLuint loadBitmap(char* fileName, bool clamp = false)
{
    AUX_RGBImageRec* image = auxDIBImageLoad(fileName);
    GLuint textureId;
    glGenTextures(1, &textureId);
    glBindTexture(GL_TEXTURE_2D, textureId);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    if (!clamp)
    {
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    }
    else
    {
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    }
    gluBuild2DMipmaps(GL_TEXTURE_2D, 3, image->sizeX, image->sizeY, GL_RGB, GL_UNSIGNED_BYTE, image->data);
    return textureId;
}

GLuint loadBitmap(char* fileName, char* alphaName)
{
    AUX_RGBImageRec* image = auxDIBImageLoad(fileName);
    AUX_RGBImageRec* alpha = auxDIBImageLoad(alphaName);

    char* data = new char[4 * image->sizeX * image->sizeY];
    for (int y = 0; y < image->sizeY; y++)
    {
        for (int x = 0; x < image->sizeX; x++)
        {
            data[(y * image->sizeX + x) * 4 + 0] = image->data[(y * image->sizeX + x) * 3 + 0];
            data[(y * image->sizeX + x) * 4 + 1] = image->data[(y * image->sizeX + x) * 3 + 1];
            data[(y * image->sizeX + x) * 4 + 2] = image->data[(y * image->sizeX + x) * 3 + 2];
            data[(y * image->sizeX + x) * 4 + 3] = alpha->data[(y * image->sizeX + x) * 3 + 0];
        }
    }

    GLuint textureId;
    glGenTextures(1, &textureId);
    glBindTexture(GL_TEXTURE_2D, textureId);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    gluBuild2DMipmaps(GL_TEXTURE_2D, 4, image->sizeX, image->sizeY, GL_RGBA, GL_UNSIGNED_BYTE, data);
    return textureId;
}

int main(int argc, char** argv)
{
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_DEPTH | GLUT_RGB | GLUT_MULTISAMPLE | GLUT_STENCIL);
    glutInitWindowSize(800, 600);
    glutCreateWindow("OpenGL Demo");
    glutDisplayFunc(displayHandler);
    glutReshapeFunc(reshapeHandler);
    glutIdleFunc(idleHandler);
    glutKeyboardFunc(keyHandler);
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

    defaultMaterial();

    glLightf(GL_LIGHT0, GL_QUADRATIC_ATTENUATION, 0.07);

    texText   = loadBitmap("textures/text.bmp");
    texSphere = loadBitmap("textures/sphere.bmp");
    texFloor  = loadBitmap("textures/floor.bmp");
    texTree   = loadBitmap("textures/tree.bmp", "textures/tree_alpha.bmp");
    texGrass  = loadBitmap("textures/grass.bmp");
    texNX     = loadBitmap("textures/nx.bmp", true);
    texNY     = loadBitmap("textures/ny.bmp", true);
    texNZ     = loadBitmap("textures/nz.bmp", true);
    texPX     = loadBitmap("textures/px.bmp", true);
    texPY     = loadBitmap("textures/py.bmp", true);
    texPZ     = loadBitmap("textures/pz.bmp", true);
    texKnot   = loadBitmap("textures/knot.bmp");
    texFin    = loadBitmap("textures/fin.bmp");

    srand(GetTickCount());

    glutMainLoop();
    return 0;
}