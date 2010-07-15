#include <gl/glaux.h>
#include <gl/glu.h>
#include <gl/glut.h>
#include <gl/gl.h>
#include <cmath>
#include <vector>
#include <algorithm>
#include "BmpLoad.h"
#include "Vector3D.h"

#define PI 3.141592

#define CONELIST 1
#define FLOORLIST 2

using namespace std;

int viewportHeight, viewportWidth;
int prevtime, time;
float secs;

Vector3D l, p;

GLuint tSphere, tFloor;

GLUquadricObj *gluSphereQuadric;

void Normal(Vector3D v) {
    glNormal3f(v.x, v.y, v.z);
}

void Vertex(Vector3D v) {
    glVertex3f(v.x, v.y, v.z);
}

void TexCoord(Vector3D v) {
    glTexCoord2f(v.x, v.y);
}

struct Poly {
    Vector3D v[4];
    Vector3D n[4];
    float dist;
};

std::vector<Poly*> torus;

Vector3D TorusCenter(float phi) {
    float r=0.5+0.2*cos(0.5*phi); 
    return Vector3D(r*sin(phi),r*cos(phi),1.5+0.2*sin(0.5*phi));
}

Vector3D TorusPoint(float phi, float psi) {
    Vector3D center=TorusCenter(phi);
    Vector3D z=(TorusCenter(phi+0.001)-center).normalize();
    Vector3D y=z^Vector3D(0,0,1);
    Vector3D x=y^z;
    return center+0.1*(x*sin(psi)+y*cos(psi));
}

Vector3D TorusNormal(float phi, float psi) {
    return (TorusPoint(phi+0.001,psi)-TorusPoint(phi,psi))^(TorusPoint(phi,psi+0.001)-TorusPoint(phi,psi));
}

void TorusVertex(float phi, float psi, Vector3D& vertex, Vector3D& normal) {
    vertex=TorusPoint(phi,psi);
    normal=TorusNormal(phi,psi);
}

void GenTorus() {
    const int I=80;
    const int J=20;
    int n=0;
    for(int i=0; i<I; i++) {
        for(int j=0; j<J; j++) {
            float i0 = (float) i/I*4*PI;
            float i1 = (float) (i+1)/I*4*PI;
            float j0 = (float) j/J*2*PI;
            float j1 = (float) (j+1)/J*2*PI;
            Poly* t;

            t = new Poly();
            TorusVertex(i0, j0, t->v[0], t->n[0]);
            TorusVertex(i1, j0, t->v[1], t->n[1]);
            TorusVertex(i1, j1, t->v[2], t->n[2]);
            TorusVertex(i0, j1, t->v[3], t->n[3]);
            torus.push_back(t);
        }
    }
}

struct DistComparer {
    bool operator() (Poly* a, Poly* b) {
        return a->dist>b->dist;
    }
};

void SortTorus() {
    for(unsigned int i=0; i<torus.size(); i++)
        torus[i]->dist = (p-torus[i]->v[0]).lengthSq();
    sort(torus.begin(), torus.end(), DistComparer());
}

void DrawTorus() {
    GLfloat m_diffuse[4] = { 0.3f,0.3f,1.0f,0.3f };
    glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);

    glEnable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
    glBegin(GL_QUADS);
    for(unsigned int i=0; i<torus.size(); i++) {
        Normal(torus[i]->n[0]);
        Vertex(torus[i]->v[0]);
        Normal(torus[i]->n[1]);
        Vertex(torus[i]->v[1]);
        Normal(torus[i]->n[2]);
        Vertex(torus[i]->v[2]);
        Normal(torus[i]->n[3]);
        Vertex(torus[i]->v[3]);
    }
    glEnd();
    glEnable(GL_TEXTURE_2D);
    glDisable(GL_BLEND);
}

Vector3D FloorPoint(float phi) {
    float r=2.0+1.0*sin(4*phi);
    return Vector3D(r*cos(phi),r*sin(phi),0);
}

void GenFloor() {
    glNewList(FLOORLIST, GL_COMPILE);
    glBegin(GL_TRIANGLE_FAN);
    glNormal3f(0,0,1);
    glTexCoord2f(0,0);glVertex3f(0,0,0);
    for(int i=0; i<=100; i++) {
        Vector3D v = FloorPoint((float)i/100*2*PI);
        TexCoord(v);
        Vertex(v);
    }
    glEnd();
    glEndList();
}

void DrawFloor() {
    glBindTexture(GL_TEXTURE_2D,tFloor);
    glCallList(FLOORLIST);
}

void DrawSphere() {
    static GLfloat sphereMatrix[16] = {1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1};
    static Vector3D sphereLastPos(0,0,0);
    Vector3D spherePos=0.8*FloorPoint(secs/3)+Vector3D(0,0,0.3);
    Vector3D rotAxis=Vector3D(0,0,1)^(spherePos-sphereLastPos);
  
    glPushMatrix();
    glLoadIdentity();
    glRotatef((spherePos - sphereLastPos).length()*360/(2*PI*0.3),rotAxis.x,rotAxis.y,rotAxis.z);
    glMultMatrixf(sphereMatrix);
    glGetFloatv(GL_MODELVIEW_MATRIX, sphereMatrix);
    glPopMatrix();

    glBindTexture(GL_TEXTURE_2D,tSphere);
    glPushMatrix();
    glTranslatef(spherePos.x, spherePos.y, spherePos.z);
    glMultMatrixf(sphereMatrix);
    gluSphere(gluSphereQuadric,0.3,16,16);
    glPopMatrix();

    sphereLastPos = spherePos;
}

Vector3D ConePoint(float phi, float d) {
    float r = 0.5*(1-d)*(1-d)*(1+0.2*sin(4*phi+8*PI*d));
    return Vector3D(r*sin(phi),r*cos(phi),d*3.0);
}

Vector3D ConeNormal(float phi, float d) {
    return (ConePoint(phi,d+0.001)-ConePoint(phi,d))^(ConePoint(phi+0.001,d)-ConePoint(phi,d));
}

void ConeVertex(float phi, float d) {
    Normal(ConeNormal(phi,d));
    glTexCoord2f(phi/(2*PI),d);
    Vertex(ConePoint(phi,d));
}

void GenCone() {
    glNewList(CONELIST, GL_COMPILE);
    const int I=64;
    const int J=32;
    glBegin(GL_QUADS);
    for(int i = 0; i < I; i++) {
        for(int j = 0; j < J; j++) {
            float i0 = (float) i/I*2*PI;
            float i1 = (float) (i+1)/I*2*PI;
            float j0 = (float) j/J;
            float j1 = (float) (j+1)/J;
            ConeVertex(i0, j0);
            ConeVertex(i1, j0);
            ConeVertex(i1, j1);
            ConeVertex(i0, j1);
        }
    }
    glEnd();
    glEndList();
}

void DrawCone() {
    glBindTexture(GL_TEXTURE_2D,tSphere);
    glCallList(CONELIST);
}

void DrawLight() {
    glPointSize(5);
    glDisable(GL_LIGHTING);
    glDisable(GL_TEXTURE_2D);
    glColor4f(1,1,1,1);
    glBegin(GL_POINTS);
    Vertex(l);
    glEnd();
    glEnable(GL_LIGHTING);
    glEnable(GL_TEXTURE_2D);
}

void display() {
    time=time+timeGetTime()-prevtime;
    prevtime=timeGetTime();
    secs=(float)time/1000;

    l.z=2.0+0.3*sin(secs);
    l.y=sin(2*secs);
    l.x=cos(2*secs);

    p.x=2.5*sin(0.3*secs);
    p.y=2.5*cos(0.3*secs);
    p.z=3.5+sin(0.5*secs);

    glClearColor(0.0,0.0,0.15,0.0);
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_STENCIL_BUFFER_BIT);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(90.0,viewportWidth/(float)viewportHeight,0.1,60);

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    gluLookAt(p.x,p.y,p.z,0,0,0,0,0,1);

    GLfloat l_position[4] = { l.x, l.y, l.z, 1 };
    glLightfv(GL_LIGHT0,GL_POSITION,l_position);

    // Stencil floor
    glColorMask(GL_FALSE,GL_FALSE,GL_FALSE,GL_FALSE);
    glDepthMask(GL_FALSE);
    glEnable(GL_STENCIL_TEST);
    glStencilFunc(GL_ALWAYS,1,1);
    glStencilOp(GL_KEEP,GL_KEEP,GL_REPLACE);
    DrawFloor();
    glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
    glDepthMask(GL_TRUE);

    // Reflection
    glStencilFunc(GL_EQUAL,1,1);
    glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);
    glPushMatrix();
    glScalef(1,1,-1);
    glLightfv(GL_LIGHT0,GL_POSITION,l_position);
    DrawSphere();
    DrawCone();
    DrawLight();
    p.z*=-1;
    SortTorus();
    p.z*=-1;
    DrawTorus();
    glPopMatrix();
    glDisable(GL_STENCIL_TEST);


    glLightfv(GL_LIGHT0,GL_POSITION,l_position);

    // Floor
    GLfloat m_diffuse[4] = { 0.4f, 0.4f, 0.4f, 0.6f };
    glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);
    glEnable(GL_BLEND);
    DrawFloor();
    glDisable(GL_BLEND);

    // Upper World
    DrawSphere();
    DrawCone();
    DrawLight();
    glPushMatrix();
    SortTorus();
    DrawTorus();
    glPopMatrix();

    // Shadows
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
    GLfloat proj[16]={l.z,0,0,0,   0,l.z,0,0,   -l.x,-l.y,0,-1,   0,0,0,l.z};
    glMultMatrixf(proj);
    DrawSphere();
    DrawCone();
    glColor4f(0,0,0,0.2);
    DrawTorus();
    glPopMatrix();
    glDisable(GL_BLEND);
    glEnable(GL_LIGHTING); 

    glDisable(GL_STENCIL_TEST);
    glDisable(GL_POLYGON_OFFSET_FILL);

    glFlush();
    glutSwapBuffers();
}

void idle() {
    glutPostRedisplay();
}

void keyboard(unsigned char key, int x, int y) {
    if(key=='\033')
        exit(0);
}

void reshape(int w, int h) {
    viewportWidth=w;
    viewportHeight=h;
    glViewport(0,0,w,h);
}

GLuint LoadTexture(char* filename) {
    glPixelStorei(GL_UNPACK_ALIGNMENT,1);

    GLuint Texture;
    glGenTextures(1,&Texture);
    glBindTexture(GL_TEXTURE_2D,Texture);

    unsigned char *tex_data;
    int tex_width;
    int tex_height;
    tex_data = LoadTrueColorBMPFile(filename,&tex_width,&tex_height);

    glPixelStorei(GL_UNPACK_ALIGNMENT,1);
    gluBuild2DMipmaps(GL_TEXTURE_2D, 3, tex_width, tex_height, GL_RGB, GL_UNSIGNED_BYTE, tex_data);
    delete[] tex_data;
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);
    return Texture;
}

void Init() {
    gluSphereQuadric=gluNewQuadric();
    gluQuadricTexture(gluSphereQuadric,GL_TRUE);
    
    GenTorus();

    GenCone();

    GenFloor();

    prevtime=timeGetTime();

    glEnable(GL_DEPTH_TEST);

    tFloor=LoadTexture("data/floor.bmp");
    tSphere=LoadTexture("data/sphere.bmp");

    glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_POINT_SMOOTH);
    glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);
    glEnable(GL_NORMALIZE);

    GLfloat m_emissive[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
    GLfloat m_diffuse[4] = { 0.4f, 0.4f, 0.4f, 0.3f };
    GLfloat m_specular[4] = { 0.6f, 0.6f, 0.6f, 0.0f };
    GLfloat m_ambient[4] = { 0.3f, 0.3f, 0.3f, 0.0f };
    glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, m_emissive);
    glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, m_specular);
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, m_ambient);
    glMaterialf(GL_FRONT_AND_BACK,GL_SHININESS,10);

    GLfloat l_diffuse[4] = { 1.0f, 1.0f, 1.0f, 0.0f };
    GLfloat l_specular[4] = { 1.0f, 1.0f, 1.0f, 0.0f };
    GLfloat l_ambient[4] = { 1.0f, 1.0f, 1.0f, 0.0f };
    glLightfv(GL_LIGHT0,GL_DIFFUSE,l_diffuse);
    glLightfv(GL_LIGHT0,GL_SPECULAR,l_specular);
    glLightfv(GL_LIGHT0,GL_AMBIENT,l_ambient);

    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);
    glShadeModel(GL_SMOOTH);
}

int main(int argc, char** argv) {
    glutInit(&argc,argv);
    glutInitDisplayMode(GLUT_RGB|GLUT_DOUBLE|GLUT_STENCIL);
    glutInitWindowSize(800,600);
    glutCreateWindow("");

    Init();
    glutDisplayFunc(display);
    glutReshapeFunc(reshape);
    glutKeyboardFunc(keyboard);
    glutIdleFunc(idle);

    DEVMODE dmSS;
    memset(&dmSS,0,sizeof(dmSS));
    dmSS.dmSize=sizeof(dmSS);
    dmSS.dmPelsWidth=800;
    dmSS.dmPelsHeight=600;
    dmSS.dmBitsPerPel=32;
    dmSS.dmFields=DM_BITSPERPEL|DM_PELSWIDTH|DM_PELSHEIGHT;
    ChangeDisplaySettings(&dmSS,CDS_FULLSCREEN);
    glutFullScreen(); 

    glutMainLoop();

    return 0;
}