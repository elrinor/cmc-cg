#include <Windows.h>
#include <gl/glu.h>
#include <gl/glut.h>
#include <gl/gl.h>
#include <cmath>
#include <cstdlib>

const int floorList = 1;
const int lightList = 2;
const int worldList = 3;
const int ringList = 4;

const float PI = 3.141592653589793238462643383279f;

GLuint texMarble, texSphere[10];

float t = 0;
float initialTime = timeGetTime() / 1000.0f;

float lightPos[4], cameraPos[4];

unsigned int random(int max) {
    return (max * rand() / (RAND_MAX + 1));
}

float random() {
    return 1.0f * rand() / RAND_MAX;
}

void DrawLight() {
    glLightfv(GL_LIGHT0, GL_POSITION, lightPos);
    glPushMatrix();
    glTranslatef(lightPos[0], lightPos[1], lightPos[2]);
    glCallList(lightList);
    glPopMatrix();
}

void DrawWorld() 
{
    glCallList(worldList);

    glPushMatrix();
    glTranslatef(0, 0, 7);
    glRotatef(t * 90, 1, cos(t * PI / 2), sin(t * PI / 2));
    glCallList(ringList);
    glPopMatrix();
}

void DrawFloor() 
{
    glEnable(GL_BLEND);
    glCallList(floorList);
    glDisable(GL_BLEND);
}

void DisplayFunc(void) 
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

    t = timeGetTime() / 1000.0f - initialTime;

    lightPos[0] = 0;
    lightPos[1] = 0;
    lightPos[2] = 4 + 3.0 * sin(t * PI / 2);
    lightPos[3] = 1;

    cameraPos[0] = 3 + 6 * sin(t);
    cameraPos[1] = 3.5 * cos(t);
    cameraPos[2] = 6 + 5 * sin(t);

    glLoadIdentity();
    gluLookAt(cameraPos[0], cameraPos[1], cameraPos[2], 0, 0, 2, 0, 0, 1);

    glPushMatrix();
    glScalef(1, 1, -1);
    DrawLight();
    DrawWorld();
    glPopMatrix();
    DrawLight();
    DrawWorld();

    DrawFloor();

    glColor4f(0, 0, 0, 0.5);
    glEnable(GL_POLYGON_OFFSET_FILL);
    glPolygonOffset(-4, -4);
    glDepthMask(GL_FALSE);
    glDisable(GL_LIGHTING);
    glEnable(GL_STENCIL_TEST);
    glEnable(GL_BLEND);
    glStencilFunc(GL_EQUAL, 0, 1);
    glStencilOp(GL_KEEP, GL_KEEP, GL_INCR);
    GLfloat zProjectionMatrix[] = {lightPos[2], 0, 0, 0, 0, lightPos[2], 0, 0, -lightPos[0], -lightPos[1], 0, -1, 0, 0, 0, lightPos[2]};
    glPushMatrix();
    glMultMatrixf(zProjectionMatrix);
    DrawWorld();
    glPopMatrix();
    glDisable(GL_BLEND);
    glDisable(GL_STENCIL_TEST);
    glEnable(GL_LIGHTING);
    glDepthMask(GL_TRUE);
    glDisable(GL_POLYGON_OFFSET_FILL);

    glutSwapBuffers();
    glutPostRedisplay();
}

void GenRing(float innerRadius, float outerRadius, float deltaZ, int slices)
{
    glBegin(GL_QUAD_STRIP);
    for(int i = 0; i <= slices; i++) 
    {
        float a = 2 * PI * i / slices;
        glNormal3f(0, 0, 1);
        glTexCoord2f((float) i / slices, 0);    glVertex3f(outerRadius * cos(a), outerRadius * sin(a),  deltaZ / 2);
        glTexCoord2f((float) i / slices, 0.75); glVertex3f(innerRadius * cos(a), innerRadius * sin(a),  deltaZ / 2);
    }
    glEnd();
    glBegin(GL_QUAD_STRIP);
    for(int i = 0; i <= slices; i++) 
    {
        float a = 2 * PI * i / slices;
        glNormal3f(0, 0, -1);
        glTexCoord2f((float) i / slices, 0.5);  glVertex3f(innerRadius * cos(a), innerRadius * sin(a), -deltaZ / 2);
        glTexCoord2f((float) i / slices, 0.25); glVertex3f(outerRadius * cos(a), outerRadius * sin(a), -deltaZ / 2);
    }
    glEnd();
    glBegin(GL_QUAD_STRIP);
    for(int i = 0; i <= slices; i++) 
    {
        float a = 2 * PI * i / slices;
        glNormal3f(cos(a), sin(a), 0);
        glTexCoord2f((float) i / slices, 0);    glVertex3f(outerRadius * cos(a), outerRadius * sin(a),  deltaZ / 2);
        glTexCoord2f((float) i / slices, 0.25); glVertex3f(outerRadius * cos(a), outerRadius * sin(a), -deltaZ / 2);
    }
    glEnd();
    glBegin(GL_QUAD_STRIP);
    for(int i = 0; i <= slices; i++) 
    {
        float a = 2 * PI * i / slices;
        glNormal3f(-cos(a), -sin(a), 0);
        glTexCoord2f((float) i / slices, 0.75); glVertex3f(innerRadius * cos(a), innerRadius * sin(a),  deltaZ / 2);
        glTexCoord2f((float) i / slices, 0.5);  glVertex3f(innerRadius * cos(a), innerRadius * sin(a), -deltaZ / 2);
    }
    glEnd();
}

void GenSphere(float radius, int slices, int stacks)
{
    GLUquadric* quadric = gluNewQuadric();
    gluQuadricTexture(quadric, GL_TRUE);
    gluSphere(quadric, radius, slices, stacks);
    gluDeleteQuadric(quadric);
}

void GenBox(float centerX, float centerY, float centerZ, float deltaX, float deltaY, float deltaZ) 
{
    glBegin(GL_QUADS);
    glNormal3f(1, 0, 0);
    glTexCoord2f(0, 0); glVertex3f(centerX + deltaX, centerY + deltaY, centerZ + deltaZ);
    glTexCoord2f(1, 0); glVertex3f(centerX + deltaX, centerY - deltaY, centerZ + deltaZ);
    glTexCoord2f(1, 1); glVertex3f(centerX + deltaX, centerY - deltaY, centerZ - deltaZ);
    glTexCoord2f(0, 1); glVertex3f(centerX + deltaX, centerY + deltaY, centerZ - deltaZ);

    glNormal3f(-1, 0, 0);
    glTexCoord2f(0, 0); glVertex3f(centerX - deltaX, centerY + deltaY, centerZ + deltaZ);
    glTexCoord2f(1, 0); glVertex3f(centerX - deltaX, centerY - deltaY, centerZ + deltaZ);
    glTexCoord2f(1, 1); glVertex3f(centerX - deltaX, centerY - deltaY, centerZ - deltaZ);
    glTexCoord2f(0, 1); glVertex3f(centerX - deltaX, centerY + deltaY, centerZ - deltaZ);

    glNormal3f(0, 1, 0);
    glTexCoord2f(0, 0); glVertex3f(centerX + deltaX, centerY + deltaY, centerZ + deltaZ);
    glTexCoord2f(1, 0); glVertex3f(centerX - deltaX, centerY + deltaY, centerZ + deltaZ);
    glTexCoord2f(1, 1); glVertex3f(centerX - deltaX, centerY + deltaY, centerZ - deltaZ);
    glTexCoord2f(0, 1); glVertex3f(centerX + deltaX, centerY + deltaY, centerZ - deltaZ);

    glNormal3f(0, -1, 0);
    glTexCoord2f(0, 0); glVertex3f(centerX + deltaX, centerY - deltaY, centerZ + deltaZ);
    glTexCoord2f(1, 0); glVertex3f(centerX - deltaX, centerY - deltaY, centerZ + deltaZ);
    glTexCoord2f(1, 1); glVertex3f(centerX - deltaX, centerY - deltaY, centerZ - deltaZ);
    glTexCoord2f(0, 1); glVertex3f(centerX + deltaX, centerY - deltaY, centerZ - deltaZ);

    glNormal3f(0, 0, 1);
    glTexCoord2f(0, 0); glVertex3f(centerX + deltaX, centerY + deltaY, centerZ + deltaZ);
    glTexCoord2f(1, 0); glVertex3f(centerX - deltaX, centerY + deltaY, centerZ + deltaZ);
    glTexCoord2f(1, 1); glVertex3f(centerX - deltaX, centerY - deltaY, centerZ + deltaZ);
    glTexCoord2f(0, 1); glVertex3f(centerX + deltaX, centerY - deltaY, centerZ + deltaZ);

    glNormal3f(0, 0, -1);
    glTexCoord2f(0, 0); glVertex3f(centerX + deltaX, centerY + deltaY, centerZ - deltaZ);
    glTexCoord2f(1, 0); glVertex3f(centerX - deltaX, centerY + deltaY, centerZ - deltaZ);
    glTexCoord2f(1, 1); glVertex3f(centerX - deltaX, centerY - deltaY, centerZ - deltaZ);
    glTexCoord2f(0, 1); glVertex3f(centerX + deltaX, centerY - deltaY, centerZ - deltaZ);
    glEnd();
}

const int noiseWidth = 256;
const int noiseHeight = 256;
float noise[noiseWidth][noiseHeight];

void InitNoise() {
    for(int x = 0; x < noiseWidth; x++) 
        for(int y = 0; y < noiseHeight; y++)
            noise[x][y] = (rand() % 32768) / 32768.0;
}

float GetNoise(float x, float y, int modX, int modY) {  
    float fracX = x - (int) x;
    float fracY = y - (int) y;
    int x0 = ((int)x + modX) % modX;
    int y0 = ((int)y + modY) % modY;
    int x1 = (x0 + modX - 1) % modX;
    int y1 = (y0 + modY - 1) % modY;
    return fracX * fracY * noise[x0][y0] + fracX * (1 - fracY) * noise[x0][y1] + (1 - fracX) * fracY * noise[x1][y0] + (1 - fracX) * (1 - fracY) * noise[x1][y1];
}

float TurbulenceFunction(float x, float y, float size) {
    float result = 0.0;
    float initialSize = size;
    while(size >= 1)
    {
        result += GetNoise(x / size, y / size, (int) (noiseWidth / size), (int) (noiseHeight / size)) * size;
        size /= 2.0;
    }
    return(0.5 * result / initialSize);
}

GLuint GenTexture(int width, int height, float xStep, float yStep, float turbPower, float turbSize, float mul, float rAdd, float gAdd, float bAdd, float rMul, float gMul, float bMul) {
    unsigned char* data = new unsigned char[width * height * 3];
    for(int y = 0; y < height; y++)
    {
        for(int x = 0; x < width; x++) 
        {
            float v = mul * fabs(sin((x * xStep / noiseHeight + y * yStep / noiseWidth + turbPower * TurbulenceFunction(x, y, turbSize)) * PI));
            data[3 * (y * width + x) + 0] = (unsigned char) (rMul * v + rAdd);
            data[3 * (y * width + x) + 1] = (unsigned char) (gMul * v + gAdd);
            data[3 * (y * width + x) + 2] = (unsigned char) (bMul * v + bAdd);
        } 
    }
    GLuint result;
    glGenTextures(1, &result);
    glBindTexture(GL_TEXTURE_2D, result);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    gluBuild2DMipmaps(GL_TEXTURE_2D, 3, width, height, GL_RGB, GL_UNSIGNED_BYTE, data);
    delete[] data;
    return result;
}

void Init(void) 
{
    srand(timeGetTime());

    glEnable(GL_DEPTH_TEST);
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_NORMALIZE);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);

    GLfloat lightdiff[] = {1.0, 1.0, 1.0, 1.0};
    GLfloat lightspec[] = {0.0, 0.0, 0.0, 1.0};
    GLfloat lightambt[] = {0.0, 0.0, 0.0, 1.0};
    glLightfv(GL_LIGHT0, GL_SPECULAR, lightspec);
    glLightfv(GL_LIGHT0, GL_DIFFUSE,  lightdiff);
    glLightfv(GL_LIGHT0, GL_AMBIENT,  lightambt);

    GLfloat material[] = {1, 1, 1, 0.6};
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT,  material);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, material);
    glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE,  material);

    GLfloat fogcolor[] = {0, 0, 0, 0};
    glEnable(GL_FOG);
    glFogfv(GL_FOG_COLOR, fogcolor);
    glFogf(GL_FOG_START, 10.0f);
    glFogf(GL_FOG_END, 20.0f);
    glFogi(GL_FOG_MODE, GL_LINEAR);

    InitNoise();
    texMarble = GenTexture(256, 256, 1, 2, 5, 32, 128, 80, 80, 100, 1, 1, 1);

    for(int i = 0; i < 10; i++)
    {
        InitNoise();
        texSphere[i] = GenTexture(256, 256, 1 + random(15), 1 + random(15), 2 + 8 * random(), 32, 128, 
            20 + random(100), 20 + random(100), 20 + random(100), 0.5 + 0.5 * random(), 0.5 + 0.5 * random(), 0.5 + 0.5 * random());
    }

    glNewList(lightList, GL_COMPILE);
    glDisable(GL_TEXTURE_2D);
    glDisable(GL_LIGHTING);
    glColor3f(1, 1, 1);
    GenSphere(0.1, 4, 4);
    glEnable(GL_LIGHTING);
    glEnable(GL_TEXTURE_2D);
    glEndList();

    glNewList(floorList, GL_COMPILE);
    glBindTexture(GL_TEXTURE_2D, texMarble);
    glBegin(GL_QUADS);
    glNormal3f(0, 0, 1);
    for(int i = -30; i < 30; i++) for(int j = -30; j < 30; j++) 
    {
        glTexCoord2f(0.5 * (i    ), 0.5 * (j    )); glVertex3f(i    , j    , 0);
        glTexCoord2f(0.5 * (i + 1), 0.5 * (j    )); glVertex3f(i + 1, j    , 0);
        glTexCoord2f(0.5 * (i + 1), 0.5 * (j + 1)); glVertex3f(i + 1, j + 1, 0);
        glTexCoord2f(0.5 * (i    ), 0.5 * (j + 1)); glVertex3f(i    , j + 1, 0);
    }
    glEnd();
    glEndList();

    glNewList(worldList, GL_COMPILE);
    for(int i = 0; i < 48; i++) 
    {
        float a = 2 * PI * i / 16;
        float R = sqrt(1.0f - 1.0f * i / 48) * 2;
        float r = (0.1 + 0.9 * (1.0f - 1.0f * i / 48)) * (0.6 + 0.8 * random()) * 0.5;
        glBindTexture(GL_TEXTURE_2D, texSphere[random(10)]);
        glPushMatrix();
        glTranslatef(R * sin(a), R * cos(a), 0.1 + i / 8.0f);
        glRotatef(360 * random(), random(), random(), random());
        GenSphere(r, 10, 10);
        glPopMatrix();
    }

    for(int i = 0; i < 200; i++) 
    {
        float phi = 2 * PI * random();
        float psi = 0.5 * PI * random();
        float r = 12 + 5 * random();
        glBindTexture(GL_TEXTURE_2D, texSphere[random(10)]);
        glPushMatrix();
        glTranslatef(r * sin(phi) * sin(psi), r * cos(phi) * sin(psi), 1.3 * r * cos(psi) + 0.5);
        glRotatef(360 * random(), random(), random(), random());
        GenBox(0, 0, 0, random(), random(), random());
        glPopMatrix();
    }

    for(int i = 0; i < 12; i++) 
    {
        float phi = 2 * PI * random();
        float r = 4 + 5 * random();
        float rr = 0.1 + 0.9 * random();
        glBindTexture(GL_TEXTURE_2D, texSphere[random(10)]);
        glPushMatrix();
        glTranslatef(r * sin(phi), r * cos(phi), 1.3 + 2 * random());
        glRotatef(360 * random(), random(), random(), random());
        GenRing(rr, rr + 1.0 * random(), 0.5 * random(), 40);
        glPopMatrix();
    }
    glEndList();

    glNewList(ringList, GL_COMPILE);
    glBindTexture(GL_TEXTURE_2D, texSphere[random(10)]);
    GenRing(1.0, 1.0 + 0.1 + 0.1 * random(), 0.1 + 0.1 * random(), 40);
    glEndList();
}

void KeyboardFunc(unsigned char c, int x, int y) 
{
    switch (c) 
    {
    case 27:
        exit(0);
        break;
    }
}

void ReshapeFunc(int w, int h) 
{
    glViewport(0, 0, w, h);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(110, (float) w / h, 0.1, 50);
    glMatrixMode(GL_MODELVIEW);
}

int main(int argc, char **argv) 
{
    srand(GetTickCount());

    glutInitWindowSize(800, 600);
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_DEPTH | GLUT_RGBA | GLUT_STENCIL);
    glutCreateWindow("");

    glutReshapeFunc(ReshapeFunc);
    glutDisplayFunc(DisplayFunc);
    glutKeyboardFunc(KeyboardFunc);

    Init();	

    glutMainLoop();
    return 0;
}