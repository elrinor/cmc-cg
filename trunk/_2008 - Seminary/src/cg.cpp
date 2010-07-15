#include <Windows.h>
#include <gl/glut.h>
#include <gl/GLU.h>
#include <gl/GLAux.h>
#include <gl/glext.h>
#include <gl/GL.h>
#include <cstdlib>
#include <cmath>
#include <algorithm>
#include <vector>
#include "Vector.h"

#undef max
#undef min

#define WALL_LIST 3
#define FLOOR_LIST 4
#define CUBE_LIST 666

#define MODE_NORMAL 0
#define MODE_FSAA 1
#define MODE_DEPTH 2

#define PI 3.1415926535897932384626433832795f

using namespace std;

float time, startTime;

GLuint mapFloor, mapWall, mapGen, mapParticle;

int vHeight, vWidth;

Vector cam, dir, light;

int mode = 0;

float focusLength = 5.0f;

void vectorNormal(Vector v) {
    glNormal3f(v.x, v.y, v.z);
}

void vectorVertex(Vector v) {
    glVertex3f(v.x, v.y, v.z);
}


float rnd() {
    return (1.0f * rand()) / (RAND_MAX + 1);
}

void drawWalls() {
    glCallList(WALL_LIST);
}

void drawObjects() {
    glBindTexture(GL_TEXTURE_2D, mapGen);
    for(int i = 0; i < 8; i++) {
        glPushMatrix();
        glRotatef(360.0f * i / 8, 0, 0, 1);
        glTranslatef(3, 0, 2);
        glScalef(0.2, 0.2, 0.2);
        glRotatef(180.0f * time, sin(2 * PI * i / 8), -cos(2 * PI * i / 8), sin(time));
        glCallList(CUBE_LIST);
        glPopMatrix();

        glPushMatrix();
        glRotatef(360.0f * i / 8, 0, 0, 1);
        glTranslatef(0, 0, 3);
        glScalef(0.6, 0.6, 0.6);
        glRotatef(90.0f * (time + i * 3), sin(2 * PI * i / 16), -cos(2 * PI * i / 16), sin(time));
        glCallList(CUBE_LIST);
        glPopMatrix();
    }
}

struct Particle {
    Vector x;
    Vector y;
    float r, g, b, a;
};

vector<Particle> genParticles() {
    vector<Particle> v;
    for(int i = 0; i < 25; i++) {
        Particle p;
        Vector dir = Vector(1 - 2 * rnd(), 1 - 2 * rnd(), 1 - 2 * rnd());
        p.x = normalize(dir ^ Vector(0, 0, 1)) * 1.3;
        p.y = normalize(p.x ^ dir) * 1.3;
        p.r = rnd();
        p.g = rnd();
        p.b = rnd();
        p.a = 0.5;
        v.push_back(p);
    }
    return v;
}

void drawTranslucentObjects() {
    static vector<Particle> particles = genParticles();

    static float viewMatrix[16];
    glGetFloatv(GL_MODELVIEW_MATRIX, viewMatrix);
    Vector r = Vector(viewMatrix[0], viewMatrix[4], viewMatrix[8]) * 0.2;
    Vector u = Vector(viewMatrix[1], viewMatrix[5], viewMatrix[9]) * 0.2;

    glDepthMask(GL_FALSE);
    glDisable(GL_LIGHTING);
    glEnable(GL_BLEND);
    glBindTexture(GL_TEXTURE_2D, mapParticle);
    glBegin(GL_QUADS);

    for(unsigned int i = 0; i < particles.size(); i++) {
        Particle p = particles[i];
        Vector pos = Vector(0, 0, 3) + p.x * sin(time + i) + p.y * cos(time + i);
        glColor4f(p.r, p.g, p.b, p.a);
        glTexCoord2f(0, 0); vectorVertex(pos + r + u);
        glTexCoord2f(1, 0); vectorVertex(pos - r + u);
        glTexCoord2f(1, 1); vectorVertex(pos - r - u);
        glTexCoord2f(0, 1); vectorVertex(pos + r - u);
    }
    
    glEnd();
    glDisable(GL_BLEND);
    glDepthMask(GL_TRUE);
    glEnable(GL_LIGHTING);
}

void drawMirrorFloor() {
    glCallList(FLOOR_LIST);
}

void drawScene() 
{
    glClear(GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
    glLoadIdentity();
    gluLookAt(cam.x, cam.y, cam.z, cam.x + dir.x , cam.y + dir.y, cam.z + dir.z, 0, 0, 1);

    GLfloat l_position[] = {light.x, light.y, light.z, 1.0};

    glPushMatrix();
    glScalef(1, 1, -1);
    glLightfv(GL_LIGHT0, GL_POSITION, l_position);
    drawObjects();
    drawWalls();
    drawTranslucentObjects();
    glPopMatrix();

    glLightfv(GL_LIGHT0, GL_POSITION, l_position);
    drawObjects();
    drawWalls();

    glEnable(GL_STENCIL_TEST);
    glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
    glDepthMask(GL_FALSE);
    glStencilFunc(GL_ALWAYS, 1, 1);
    glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);
    GLfloat projection[] = {light.z, 0, 0, 0, 0, light.z, 0, 0, -light.x, -light.y, 0, -1, 0, 0, 0, light.z};
    glPushMatrix();
    glMultMatrixf(projection);
    drawObjects();
    glPopMatrix();
    glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
    glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
    glDepthMask(GL_TRUE);

    glEnable(GL_BLEND);
    glStencilFunc(GL_EQUAL, 1, 1);
    glDisable(GL_LIGHT0);
    drawMirrorFloor();
    glEnable(GL_LIGHT0);

    glStencilFunc(GL_NOTEQUAL, 1, 1);
    drawMirrorFloor();
    glDisable(GL_BLEND);
    glDisable(GL_STENCIL_TEST);

    drawTranslucentObjects();
}

void renderPass(Vector newCam, Vector newFocus, int divisor) {
    Vector oldCam = cam;
    cam = newCam;
    Vector oldDir = dir;
    dir = newFocus - newCam;
    drawScene();
    glAccum(GL_ACCUM, 1.0f / divisor);
    dir = oldDir;
    cam = oldCam;
}

void animate()
{
    time = GetTickCount() * 0.001f - startTime;

    cam = Vector(5 * sin(time), 5 * cos(time), 3 - 2 * sin(0.1 * time));
    dir = Vector(0, 0, 3) - cam;

    light = Vector(0, 0, 6.5);

    glClear(GL_ACCUM_BUFFER_BIT);

    if(mode == MODE_NORMAL) 
    {
        drawScene();
        glAccum(GL_ACCUM, 1.0f);
        glAccum(GL_RETURN, 1.0f);
    }
    else {
        Vector focus = cam + normalize(dir);
        Vector dx = normalize((focus - cam) ^ Vector(0, 0, 1)) * (0.4 / vHeight);
        Vector dy = normalize(dx ^ (focus - cam))              * (0.4 / vHeight);
        if(mode == MODE_FSAA)
        {
            renderPass(cam, focus + 2 * dx +     dy, 4);
            renderPass(cam, focus -     dx + 2 * dy, 4);
            renderPass(cam, focus - 2 * dx -     dy, 4);
            renderPass(cam, focus +     dx - 2 * dy, 4);
            glAccum(GL_RETURN, 1.0f);
        }
        else
        {
            Vector newFocus = cam + normalize(dir) * focusLength;
            dx = dx * 50;
            dy = dy * 50;
            renderPass(cam + 1.0 * dx + 2.0 * dy, newFocus, 8);
            renderPass(cam + 1.5 * dx + 0.5 * dy, newFocus, 8);
            renderPass(cam + 2.0 * dx - 1.0 * dy, newFocus, 8);
            renderPass(cam + 0.5 * dx - 1.5 * dy, newFocus, 8);
            renderPass(cam - 1.0 * dx - 2.0 * dy, newFocus, 8);
            renderPass(cam - 1.5 * dx - 0.5 * dy, newFocus, 8);
            renderPass(cam - 2.0 * dx + 1.0 * dy, newFocus, 8);
            renderPass(cam - 0.5 * dx + 1.5 * dy, newFocus, 8);
            glAccum(GL_RETURN, 1.0f);
        }
    }

    glutSwapBuffers();
}

void idle()
{
    glutPostRedisplay();
}

void reshape(int w, int h)
{
    glViewport(0, 0, (GLsizei)w, (GLsizei)h );
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();

    gluPerspective(90.0, (GLfloat)w/(GLfloat)h, 0.1, 30.0);
    glMatrixMode(GL_MODELVIEW);

    vHeight = h;
    vWidth = w;
}

void key(unsigned char key, int x, int y)
{
    if(key == 27 || key == 'q' || key == 'Q')
        exit(0);
    if(key == ' ')
        mode = (mode + 1) % 3;
    if(key == 'w' || key == 'W')
        focusLength += 0.1;
    if(key == 's' || key == 'S')
        focusLength -= 0.1;

    if(focusLength < 2)
        focusLength = 2;
    if(focusLength > 30)
        focusLength = 30;
}

void initialize() 
{
    srand(GetTickCount());

    startTime = GetTickCount() * 0.001f;

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
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    GLfloat l_diffuse[] =  {1.0, 1.0, 1.0, 1};
    GLfloat l_specular[] = {0.2, 0.2, 0.2, 1};
    GLfloat l_ambient[] =  {0.0, 0.0, 0.0, 1};
    glLightfv(GL_LIGHT0, GL_DIFFUSE,  l_diffuse);
    glLightfv(GL_LIGHT0, GL_SPECULAR, l_specular);
    glLightfv(GL_LIGHT0, GL_AMBIENT,  l_ambient);
    glLightf(GL_LIGHT0,  GL_LINEAR_ATTENUATION, 0.05);

    GLfloat m_diffuse[] =  {1.0, 1.0, 1.0, 0.5};
    GLfloat m_specular[] = {0.5, 0.5, 0.5, 1};
    GLfloat m_ambient[] =  {0.5, 0.5, 0.5, 1};
    GLfloat m_emission[] = {0.1, 0.1, 0.1, 1};
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT,  m_ambient);
    glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE,  m_diffuse);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, m_specular);
    glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, m_emission);
    glMaterialf(GL_FRONT_AND_BACK,  GL_SHININESS, 20);

    AUX_RGBImageRec *auxImage;

    auxImage = auxDIBImageLoad("data/pave.bmp");
    glGenTextures(1, &mapFloor);
    glBindTexture(GL_TEXTURE_2D, mapFloor);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
    gluBuild2DMipmaps(GL_TEXTURE_2D, 3, auxImage->sizeX, auxImage->sizeY, GL_RGB, GL_UNSIGNED_BYTE, auxImage->data);

    auxImage = auxDIBImageLoad("data/wall.bmp");
    glGenTextures(1, &mapWall);
    glBindTexture(GL_TEXTURE_2D, mapWall);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
    gluBuild2DMipmaps(GL_TEXTURE_2D, 3, auxImage->sizeX, auxImage->sizeY, GL_RGB, GL_UNSIGNED_BYTE, auxImage->data);

    unsigned char* data = new unsigned char[64 * 64 * 3];
    for(int y = 0; y < 64; y++)
    {
        for(int x = 0; x < 64; x++)
        {
            unsigned char* p = data + (y * 64 + x) * 3;
            p[0] = (unsigned char) (64 + 64 * sin(2 * PI * (x + y - 10 * sin(2 * PI * x / 32)) / 32));
            p[1] = (unsigned char) (192 + 63 * sin(2 * PI * (-x + y + 10 * sin(2 * PI * x / 32)) / 32));
            p[2] = (unsigned char) (96 + 64 * cos(2 * PI * (x - 3 * y + 10 * cos(2 * PI * y / 16)) / 32));
        }
    }
    glGenTextures(1, &mapGen);
    glBindTexture(GL_TEXTURE_2D, mapGen);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
    gluBuild2DMipmaps(GL_TEXTURE_2D, 3, 64, 64, GL_RGB, GL_UNSIGNED_BYTE, data);

    unsigned char* data4 = new unsigned char[64 * 64 * 4];
    for(int y = 0; y < 64; y++)
    {
        for(int x = 0; x < 64; x++)
        {
            unsigned char* p = data4 + (y * 64 + x) * 4;
            p[0] = p[1] = p[2] = 255;
            float r2 = (x - 32) * (x - 32) + (y - 32) * (y - 32);
            float a = atan2(1.0f * (y - 32), 1.0f * (x - 32));
            p[3] = (unsigned char) (255 * max(0.0, (1.1 / (r2 * (1 + 0.5 * sin(6 * a)) / 64 + 1) - 0.1)));
        }
    }
    glGenTextures(1, &mapParticle);
    glBindTexture(GL_TEXTURE_2D, mapParticle);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
    gluBuild2DMipmaps(GL_TEXTURE_2D, 4, 64, 64, GL_RGBA, GL_UNSIGNED_BYTE, data4);

    glNewList(FLOOR_LIST, GL_COMPILE);
    glBindTexture(GL_TEXTURE_2D, mapFloor);
    glBegin(GL_QUADS);
    glNormal3f(0, 0, 1);
    for(int x = -10; x < 10; x++) 
    {
        for(int y = -10; y < 10; y++) 
        {
            glTexCoord2f(0.5f * (x    ), 0.5f * (y    ));  glVertex3f(x    , y    , 0);
            glTexCoord2f(0.5f * (x + 1), 0.5f * (y    ));  glVertex3f(x + 1, y    , 0);
            glTexCoord2f(0.5f * (x + 1), 0.5f * (y + 1));  glVertex3f(x + 1, y + 1, 0);
            glTexCoord2f(0.5f * (x    ), 0.5f * (y + 1));  glVertex3f(x    , y + 1, 0);
        }
    }
    glEnd();
    glEndList();

    glNewList(WALL_LIST, GL_COMPILE);
    glBindTexture(GL_TEXTURE_2D, mapWall);
    for(int z = 0; z < 7; z++) {
        glBegin(GL_QUAD_STRIP);
        const int slices = 64;
        for(int i = 0; i <= slices; i++) 
        {
            float a = 2 * PI * i / slices;
            glNormal3f(-cos(a), -sin(a), 0);
            glTexCoord2f((z    ) * 0.3f, i * 0.2f); glVertex3f(10 * cos(a), 10 * sin(a), z);
            glTexCoord2f((z + 1) * 0.3f, i * 0.2f); glVertex3f(10 * cos(a), 10 * sin(a), z + 1);
        }
        glEnd();
    }
    
    glBegin(GL_QUADS);
    glNormal3f(0, 0, -1);
    for(int x = -10; x < 10; x++) 
    {
        for(int y = -10; y < 10; y++) 
        {
            glTexCoord2f(0.5f * (x    ), 0.5f * (y    ));  glVertex3f(x    , y    , 7);
            glTexCoord2f(0.5f * (x + 1), 0.5f * (y    ));  glVertex3f(x + 1, y    , 7);
            glTexCoord2f(0.5f * (x + 1), 0.5f * (y + 1));  glVertex3f(x + 1, y + 1, 7);
            glTexCoord2f(0.5f * (x    ), 0.5f * (y + 1));  glVertex3f(x    , y + 1, 7);
        }
    }
    glEnd();

    glDisable(GL_LIGHTING);
    glDisable(GL_TEXTURE_2D);
    glColor3f(1, 1, 1);
    glBegin(GL_QUADS);
    glVertex3f(-0.5, -0.5, 6.99);
    glVertex3f( 0.5, -0.5, 6.99);
    glVertex3f( 0.5,  0.5, 6.99);
    glVertex3f(-0.5,  0.5, 6.99);
    glEnd();
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_LIGHTING);
    glEndList();

    glNewList(CUBE_LIST, GL_COMPILE);
    glBegin(GL_QUADS);
    glNormal3f(1,0,0);
    glTexCoord2f(1, 0); glVertex3f(+1, +1, +1);
    glTexCoord2f(1, 1); glVertex3f(+1, +1, -1);
    glTexCoord2f(0, 1); glVertex3f(+1, -1, -1);
    glTexCoord2f(0, 0); glVertex3f(+1, -1, +1);
    glNormal3f(-1, 0, 0);
    glTexCoord2f(1, 0); glVertex3f(-1, +1, +1);
    glTexCoord2f(1, 1); glVertex3f(-1, +1, -1);
    glTexCoord2f(0, 1); glVertex3f(-1, -1, -1);
    glTexCoord2f(0, 0); glVertex3f(-1, -1, +1);
    glNormal3f(0, 1, 0);
    glTexCoord2f(1, 0); glVertex3f(+1, +1, +1);
    glTexCoord2f(1, 1); glVertex3f(+1, +1, -1);
    glTexCoord2f(0, 1); glVertex3f(-1, +1, -1);
    glTexCoord2f(0, 0); glVertex3f(-1, +1, +1);
    glNormal3f(0, -1, 0);
    glTexCoord2f(1, 0); glVertex3f(+1, -1, +1);
    glTexCoord2f(1, 1); glVertex3f(+1, -1, -1);
    glTexCoord2f(0, 1); glVertex3f(-1, -1, -1);
    glTexCoord2f(0, 0); glVertex3f(-1, -1, +1);
 	glNormal3f(0, 0, 1);
    glTexCoord2f(1, 0); glVertex3f(+1, +1, +1);
    glTexCoord2f(1, 1); glVertex3f(-1, +1, +1);
    glTexCoord2f(0, 1); glVertex3f(-1, -1, +1);
    glTexCoord2f(0, 0); glVertex3f(+1, -1, +1);
    glNormal3f(0, 0, -1);
    glTexCoord2f(1, 0); glVertex3f(+1, +1, -1);
    glTexCoord2f(1, 1); glVertex3f(-1, +1, -1);
    glTexCoord2f(0, 1); glVertex3f(-1, -1, -1);
    glTexCoord2f(0, 0); glVertex3f(+1, -1, -1);
    glEnd();
    glEndList();
}

int main(int argc, char** argv) {
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGBA | GLUT_DEPTH | GLUT_STENCIL | GLUT_ACCUM);
    glutInitWindowSize(640, 480);
    glutCreateWindow("");

    DEVMODE d;
    memset(&d, 0, sizeof(d));
    d.dmSize = sizeof(d);
    d.dmPelsWidth = 640;
    d.dmPelsHeight = 480;
    d.dmBitsPerPel = 32;
    d.dmFields = DM_BITSPERPEL | DM_PELSWIDTH | DM_PELSHEIGHT;
    ChangeDisplaySettings(&d, CDS_FULLSCREEN);
    glutFullScreen();

    glutDisplayFunc(animate);
    glutIdleFunc(idle);
    glutReshapeFunc(reshape);
    glutKeyboardFunc(key);

    initialize();

    glutMainLoop();
    return 0;
}


