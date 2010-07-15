#define _USE_MATH_DEFINES
#define _CRT_SECURE_NO_DEPRECATE
#include <Windows.h>
#include <gl/glew.h>
#include <gl/gl.h>
#include <gl/glu.h>
#include <gl/glut.h>
#include <gl/GLAux.h>
#include <cstdlib>
#include <cstdio>
#include <cmath>
#include <vector>
#include "Vector3D.h"
#pragma comment(lib, "glew32.lib")
#pragma comment(lib, "glaux.lib")

#define FBO_WIDTH 1024
#define FBO_HEIGHT 1024

#define CAMERASPEED 3
#define MOUSESPEED 0.005

#define SCENE_R 100

//#define FREEDOM

using namespace std;

int window_x, window_y;

GLuint reflectionFBO, refractionFBO, waterDepthFBO; // frame buffer object

GLuint underWaterTexture, terrainTexture, reflectionTexture, refractionTexture, waterRippleTexture, horizonTexture, cloudTexture, waterDepthTexture, fireTexture, torusTexture;

GLuint waterShader, skyShader, islandUpShader, islandDownShader, islandDepthShader, torusShader;

Vector3D camera, direction;

float cameraPhi, cameraPsi;

bool movingForward, movingBackward, movingLeft, movingRight;

float t, dt;

float projection[16], modelView[16]; // for reflected / refracted calculations

GLuint islandList, smallIslandList, smallDownIslandList, torusList;

void glVertex(Vector3D v)
{
    glVertex3f(v.x, v.y, v.z);
}

void glNormal(Vector3D v)
{
    glNormal3f(v.x, v.y, v.z);
}

Vector3D randomVector()
{
    return Vector3D(1.0 - 2.0 * rand() / RAND_MAX, 1.0 - 2.0 * rand() / RAND_MAX, 1.0 - 2.0 * rand() / RAND_MAX);
}

float random(float maxValue)
{
    return maxValue * rand() / RAND_MAX;
}

float* floatArray(float a, float b, float c, float d)
{
    static float v[4];
    v[0] = a;
    v[1] = b;
    v[2] = c;
    v[3] = d;
    return v;
}

class FireParticle
{
public:
    Vector3D position;
    Vector3D direction;
    Vector3D color;
    float alpha;
    float size;
    float hp;
    float hpDecreaseSpeed;

    void update(float dt)
    {
        hp -= hpDecreaseSpeed * dt;
        size = 0.5 + hp / 2;
        alpha = hp / 4;
        color = Vector3D(1, 1, 0.5) * hp + Vector3D(1, 0, 0) * (1 - hp);
        position += direction * dt;
    }

    bool isDead()
    {
        return hp < 0;
    }

    static FireParticle generate()
    {
        FireParticle p;
        p.position = Vector3D(0, 0, 3.0) + randomVector() * 1;
        p.direction = (Vector3D(0, 0, 10) + randomVector()).normalize() * (10.0 + random(10.0));
        p.hp = 1;
        p.hpDecreaseSpeed = 1.0 + random(5.0);
        p.update(0);
        return p;
    }
};

vector<FireParticle> fire;

void moveParticles(float dt)
{
    for (unsigned int i = 0; i < fire.size(); i++)
    {
        fire[i].update(dt);
        if (fire[i].isDead())
            fire[i] = FireParticle::generate();
    }
}

void generateParticles()
{
    for (int i = 0; i < 1000; i++)
        fire.push_back(FireParticle::generate());
    for (int i = 0; i < 100; i++)
        moveParticles(0.1);
}

void renderParticles()
{
    static float modelView[16];
    glGetFloatv(GL_MODELVIEW_MATRIX, modelView);
    Vector3D x = Vector3D(modelView[0], modelView[4], modelView[8]).normalize();
    Vector3D y = Vector3D(modelView[1], modelView[5], modelView[9]).normalize();

    glBindTexture(GL_TEXTURE_2D, fireTexture);
    glEnable(GL_BLEND);
    glDepthMask(GL_FALSE);
    glDisable(GL_LIGHTING);
    glBegin(GL_QUADS);
    for (unsigned int i = 0; i < fire.size(); i++)
    {
        glColor4f(fire[i].color.x, fire[i].color.y, fire[i].color.z, fire[i].alpha);
        glTexCoord2f(0, 0);
        glVertex(fire[i].position + ( x + y) * fire[i].size);
        glTexCoord2f(1, 0);
        glVertex(fire[i].position + (-x + y) * fire[i].size);
        glTexCoord2f(1, 1);
        glVertex(fire[i].position + (-x - y) * fire[i].size);
        glTexCoord2f(0, 1);
        glVertex(fire[i].position + ( x - y) * fire[i].size);
    }
    glEnd();
    glEnable(GL_LIGHTING);
    glDepthMask(GL_TRUE);
    glDisable(GL_BLEND);

}

Vector3D islandVertex(float x, float y)
{
    return Vector3D(x, y, 15 / (1 + 0.02 * x * x + 0.02 * y * y) - 9 - 1 / (0.25 +  0.2 * x * x + 0.2 * y * y));
}

Vector3D smallIslandVertex(float x, float y)
{
    Vector3D tmp = islandVertex(x, y);
    tmp.z /= 10;
    return tmp;
}

Vector3D smallDownIslandVertex(float x, float y)
{
    Vector3D tmp = islandVertex(x, y);
    tmp.z /= -10;
    return tmp;
}

Vector3D surfaceNormal(float x, float y, Vector3D (*vertexProvider)(float x, float y))
{
    return ((vertexProvider(x + 0.001, y) - vertexProvider(x - 0.001, y)) ^ (vertexProvider(x, y + 0.001) - vertexProvider(x, y - 0.001))).normalize();
}

void buildIslandList()
{
    islandList = 666;
    smallIslandList = 667;
    glNewList(islandList, GL_COMPILE);
    glBegin(GL_QUADS);
    for (int x = -10; x < 10; x++)
    {
        for (int y = -10; y < 10; y++)
        {
            glNormal(surfaceNormal(x  , y  , islandVertex));
            glVertex(islandVertex(x  , y  ));
            glNormal(surfaceNormal(x+1, y  , islandVertex));
            glVertex(islandVertex(x+1, y  ));
            glNormal(surfaceNormal(x+1, y+1, islandVertex));
            glVertex(islandVertex(x+1, y+1));
            glNormal(surfaceNormal(x  , y+1, islandVertex));
            glVertex(islandVertex(x  , y+1));
        }
    }
    glEnd();
    glEndList();

    smallIslandList = 667;
    glNewList(smallIslandList, GL_COMPILE);
    glBegin(GL_QUADS);
    for (int x = -10; x < 10; x++)
    {
        for (int y = -10; y < 10; y++)
        {
            glNormal(-surfaceNormal(x  , y  , smallIslandVertex));
            glVertex(smallIslandVertex(x  , y  ));
            glNormal(-surfaceNormal(x+1, y  , smallIslandVertex));
            glVertex(smallIslandVertex(x+1, y  ));
            glNormal(-surfaceNormal(x+1, y+1, smallIslandVertex));
            glVertex(smallIslandVertex(x+1, y+1));
            glNormal(-surfaceNormal(x  , y+1, smallIslandVertex));
            glVertex(smallIslandVertex(x  , y+1));
        }
    }
    glEnd();
    glEndList();

    smallDownIslandList = 668;
    glNewList(smallDownIslandList, GL_COMPILE);
    glBegin(GL_QUADS);
    for (int x = -10; x < 10; x++)
    {
        for (int y = -10; y < 10; y++)
        {
            glNormal(surfaceNormal(x  , y  , smallDownIslandVertex));
            glVertex(smallDownIslandVertex(x  , y  ));
            glNormal(surfaceNormal(x+1, y  , smallDownIslandVertex));
            glVertex(smallDownIslandVertex(x+1, y  ));
            glNormal(surfaceNormal(x+1, y+1, smallDownIslandVertex));
            glVertex(smallDownIslandVertex(x+1, y+1));
            glNormal(surfaceNormal(x  , y+1, smallDownIslandVertex));
            glVertex(smallDownIslandVertex(x  , y+1));
        }
    }
    glEnd();
    glEndList();
}

Vector3D torusVertex(float u, float v)
{
    float R = 4.5 + 0.5 * sin(8 * u);
    float r = 0.2;
    return Vector3D(0.5 * cos(8 * u) + r * sin(-v), (R + r * cos(-v)) * sin(u), 6 + (R + r * cos(-v)) * cos(u));
}

void buildTorusList()
{
    const int maxI = 200;
    const int maxJ = 10;

    torusList = 1;
    glNewList(torusList, GL_COMPILE);
    glBegin(GL_QUADS);
    for (int i = 0; i < maxI; i++)
    {
        for (int j = 0; j < maxJ; j++)
        {
            float u0 = (i  ) * 2 * M_PI / maxI;
            float u1 = (i+1) * 2 * M_PI / maxI;
            float v0 = (j  ) * 2 * M_PI / maxJ;
            float v1 = (j+1) * 2 * M_PI / maxJ;

            float tx0 = u0 * 30;
            float tx1 = u1 * 30;
            float ty0 = v0;
            float ty1 = v1;

            glTexCoord2f(tx0, ty0);
            glNormal(surfaceNormal(u0, v0, torusVertex));
            glVertex(torusVertex(u0, v0));
            glTexCoord2f(tx1, ty0);
            glNormal(surfaceNormal(u1, v0, torusVertex));
            glVertex(torusVertex(u1, v0));
            glTexCoord2f(tx1, ty1);
            glNormal(surfaceNormal(u1, v1, torusVertex));
            glVertex(torusVertex(u1, v1));
            glTexCoord2f(tx0, ty1);
            glNormal(surfaceNormal(u0, v1, torusVertex));
            glVertex(torusVertex(u0, v1));
        }
    }
    glEnd();
    glEndList();
}

void renderTorus()
{
    glUseProgram(torusShader);

    GLint cameraLoc = glGetUniformLocation(torusShader, "camera");
    glUniform3f(cameraLoc, camera.x, camera.y, camera.z);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, torusTexture);
    GLint tex0Loc = glGetUniformLocation(torusShader, "torusMap");
    glUniform1i(tex0Loc, 0);

    glCallList(torusList);

    glUseProgram(0);
}

void renderIsland(GLuint shader)
{
    glUseProgram(shader);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, underWaterTexture);
    GLint tex1Loc = glGetUniformLocation(shader, "underwaterMap");
    glUniform1i(tex1Loc, 1);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, terrainTexture);
    GLint tex0Loc = glGetUniformLocation(shader, "terrainMap");
    glUniform1i(tex0Loc, 0);

    glCallList(islandList);
    if (shader == islandUpShader)
        glCallList(smallIslandList);
    if (shader == islandDownShader)
        glCallList(smallDownIslandList);

    glUseProgram(0);
}

void renderUnderWorld()
{
    renderIsland(islandDownShader);
}

void renderSky()
{
    glUseProgram(skyShader);

    GLint timeLoc = glGetUniformLocation(skyShader, "time");
    glUniform1f(timeLoc, t);

    GLint cameraLoc = glGetUniformLocation(skyShader, "camera");
    glUniform3f(cameraLoc, camera.x, camera.y, camera.z);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, cloudTexture);
    GLint tex1Loc = glGetUniformLocation(skyShader, "cloudMap");
    glUniform1i(tex1Loc, 1);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, horizonTexture);
    GLint tex0Loc = glGetUniformLocation(skyShader, "horizonMap");
    glUniform1i(tex0Loc, 0);

    float r = SCENE_R;

    glBegin(GL_QUADS);
    glVertex3f(camera.x + r, camera.y - r, r);
    glVertex3f(camera.x - r, camera.y - r, r);
    glVertex3f(camera.x - r, camera.y + r, r);
    glVertex3f(camera.x + r, camera.y + r, r);
    glEnd();
    glBegin(GL_QUAD_STRIP);
    glVertex3f(camera.x + r, camera.y + r, -5);
    glVertex3f(camera.x + r, camera.y + r, r);
    glVertex3f(camera.x - r, camera.y + r, -5);
    glVertex3f(camera.x - r, camera.y + r, r);
    glVertex3f(camera.x - r, camera.y - r, -5);
    glVertex3f(camera.x - r, camera.y - r, r);
    glVertex3f(camera.x + r, camera.y - r, -5);
    glVertex3f(camera.x + r, camera.y - r, r);
    glVertex3f(camera.x + r, camera.y + r, -5);
    glVertex3f(camera.x + r, camera.y + r, r);
    glEnd();

    glUseProgram(0);
}

void renderUpperWorld()
{
    //renderBalls();
    renderTorus();
    renderSky();
    renderIsland(islandUpShader);
    renderParticles();
}

void renderReflection()
{
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, reflectionFBO);
    glPushAttrib(GL_VIEWPORT_BIT);
    glViewport(0, 0, FBO_WIDTH, FBO_HEIGHT);

    glClearColor(0, 0, 0, 0);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glPushMatrix();
    glScalef(1,1,-1);

    renderUpperWorld();

    glPopMatrix();

    glPopAttrib();
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);

    glBindTexture(GL_TEXTURE_2D, reflectionTexture);
    glGenerateMipmapEXT(GL_TEXTURE_2D);
}

void renderRefraction()
{
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, refractionFBO);
    glPushAttrib(GL_VIEWPORT_BIT);
    glViewport(0, 0, FBO_WIDTH, FBO_HEIGHT);

    glClearColor(0, 0, 0, 0);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    renderUnderWorld();

    glPopAttrib();
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);

    glBindTexture(GL_TEXTURE_2D, refractionTexture);
    glGenerateMipmapEXT(GL_TEXTURE_2D);
}

void renderWaterDepth()
{
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, waterDepthFBO);
    glPushAttrib(GL_VIEWPORT_BIT);
    glViewport(0, 0, FBO_WIDTH, FBO_HEIGHT);

    glClearColor(0, 0, 0, 0);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    renderIsland(islandDepthShader);

    glPopAttrib();
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);

    glBindTexture(GL_TEXTURE_2D, waterDepthTexture);
    glGenerateMipmapEXT(GL_TEXTURE_2D);
}

void renderWater()
{
    glMatrixMode(GL_TEXTURE);
    glPushMatrix();
    glTranslatef(0.5, 0.5, 0);
    glScalef(0.5, 0.5, 0);
    glMultMatrixf(projection);
    glMultMatrixf(modelView);

    glUseProgram(waterShader);

    GLint cameraLoc = glGetUniformLocation(waterShader, "camera");
    glUniform3f(cameraLoc, camera.x, camera.y, camera.z);

    GLint timeLoc = glGetUniformLocation(waterShader, "time");
    glUniform1f(timeLoc, t);

    glActiveTexture(GL_TEXTURE3);
    glBindTexture(GL_TEXTURE_2D, waterDepthTexture);
    GLint tex3Loc = glGetUniformLocation(waterShader, "waterDepthMap");
    glUniform1i(tex3Loc, 3);

    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, reflectionTexture);
    GLint tex2Loc = glGetUniformLocation(waterShader, "reflectedMap");
    glUniform1i(tex2Loc, 2);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, refractionTexture);
    GLint tex1Loc = glGetUniformLocation(waterShader, "refractedMap");
    glUniform1i(tex1Loc, 1);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, waterRippleTexture);
    GLint tex0Loc = glGetUniformLocation(waterShader, "normalMap");
    glUniform1i(tex0Loc, 0);

    float r = SCENE_R;

    glBegin(GL_QUADS);
    glVertex3f(camera.x + r, camera.y - r, 0);
    glVertex3f(camera.x - r, camera.y - r, 0);
    glVertex3f(camera.x - r, camera.y + r, 0);
    glVertex3f(camera.x + r, camera.y + r, 0);
    glEnd();

    glUseProgram(0);
    glPopMatrix();
    glMatrixMode(GL_MODELVIEW);
}


void display()
{
    static unsigned long lastTime = timeGetTime();
    float dt = (timeGetTime() - lastTime) / 1000.0f;
    t += dt;
    lastTime = timeGetTime();

#ifdef FREEDOM
    direction = Vector3D(sin(cameraPhi) * cos(cameraPsi), cos(cameraPhi) * cos(cameraPsi), sin(cameraPsi));
    if (movingForward)
        camera += CAMERASPEED * direction * dt;
    if (movingBackward)
        camera -= CAMERASPEED * direction * dt;
    if (movingLeft)
    {
        camera.x += - CAMERASPEED * cos(cameraPhi) * dt;
        camera.y +=   CAMERASPEED * sin(cameraPhi) * dt;
    }
    if (movingRight)
    {
        camera.x -= - CAMERASPEED * cos(cameraPhi) * dt;
        camera.y -=   CAMERASPEED * sin(cameraPhi) * dt;
    }
#else
    float cameraR = max(90 - t * 5, 15);
    float cameraZ = max(10 - t * 0.5, 5 + 4 * sin(0.3 * t));

    camera = Vector3D(cameraR * sin(0.2 * t), cameraR * cos(0.2 * t), cameraZ);
    direction = Vector3D(0, 0, 3) - camera;
#endif

    moveParticles(dt);

    float lightR = 0.9 + 0.1 * sin(8 * t);
    float lightG = lightR * (0.3 + 0.2 * cos(10 * t));
    glLightfv(GL_LIGHT0, GL_DIFFUSE, floatArray(lightR, lightG, 0, 0));
    glLightfv(GL_LIGHT0, GL_SPECULAR, floatArray(lightR, lightG, 0, 0));

    // We need to set light position using identity modelview matrix!
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glLightfv(GL_LIGHT0, GL_POSITION, floatArray(0.2 * sin(20 * t), 0.2 * cos(20 * t), 6, 0));

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(120.0, (float) window_x / window_y, 0.1, 1000);
    gluLookAt(camera.x, camera.y, camera.z, camera.x + direction.x, camera.y + direction.y, camera.z + direction.z, 0, 0, 1);
    glGetFloatv(GL_PROJECTION_MATRIX, projection);
    glGetFloatv(GL_MODELVIEW_MATRIX, modelView);

    renderReflection();
    renderRefraction();
    renderWaterDepth();

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(90.0, (float) window_x / window_y, 0.1, 1000);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    gluLookAt(camera.x, camera.y, camera.z, camera.x + direction.x, camera.y + direction.y, camera.z + direction.z, 0, 0, 1);

    glClearColor(0, 0, 0, 0);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    renderWater();
    renderUpperWorld();

    glutSwapBuffers();
}

void keypress(unsigned char key, int x, int y)
{
    if (key == '\033')
        exit(0);
#ifdef FREEDOM
    if (key == 'w')
        movingForward = true;
    if (key == 's')
        movingBackward = true;
    if (key == 'a')
        movingLeft = true;
    if (key == 'd')
        movingRight = true;
#endif
}

void reshape(int w, int h)
{
    window_x = w;
    window_y = h;
    glViewport(0, 0, w, h);
}

void animate()
{
    glutPostRedisplay();
}

void keyup(unsigned char key, int x, int y)
{
#ifdef FREEDOM
    if (key == 'w')
        movingForward = false;
    if (key == 's')
        movingBackward = false;
    if (key == 'a')
        movingLeft = false;
    if (key == 'd')
        movingRight = false;
#endif
}

void mouse(int x, int y)
{
#ifdef FREEDOM
    if (!(x == window_x / 2 && y == window_y / 2))
    {
        cameraPhi += (x - window_x / 2) * MOUSESPEED;
        cameraPsi += (window_y / 2 - y) * MOUSESPEED;

        if (cameraPsi >  M_PI * 0.4f) cameraPsi =  M_PI * 0.4f;
        if (cameraPsi < -M_PI * 0.4f) cameraPsi = -M_PI * 0.4f;

        glutWarpPointer(window_x / 2, window_y / 2);
    }
#endif
}

void newFBO(GLuint* frameBufferId, GLuint* textureId)
{
    glGenFramebuffersEXT(1, frameBufferId);
    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, *frameBufferId);

    GLuint depthbuffer;
    glGenRenderbuffersEXT(1, &depthbuffer);
    glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, depthbuffer);
    glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT, FBO_WIDTH, FBO_HEIGHT);
    glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, depthbuffer);

    glGenTextures(1, textureId);
    glBindTexture(GL_TEXTURE_2D, *textureId);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, FBO_WIDTH, FBO_HEIGHT, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glGenerateMipmapEXT(GL_TEXTURE_2D);
    glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, *textureId, 0);

    bool status = (glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT) == GL_FRAMEBUFFER_COMPLETE_EXT);

    glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
}

char *readFile(const char *fileName)
{
    FILE *f;
    char *result = NULL;
    int count = 0;

    f = fopen(fileName, "rt");
    fseek(f, 0, SEEK_END);
    count = ftell(f);
    rewind(f);

    if (count > 0)
    {
        result = (char *) malloc(sizeof(char) * (count + 1));
        count = fread(result, sizeof(char), count, f);
        result[count] = '\0';
    }
    fclose(f);
    return result;
}

void checkShaderStatus(GLuint shader)
{
    GLint status;
    glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
    if (status == GL_FALSE)
    {
        GLsizei len;
        static GLchar buf[4096];
        glGetShaderInfoLog(shader, 4096, &len, buf);
        buf[len] = '\0';
        MessageBox(NULL, buf, "Error", MB_OK);
        exit(0);
    }
}

void checkProgramStatus(GLuint program)
{
    GLint status;
    glGetShaderiv(program, GL_LINK_STATUS, &status);
    if (status == GL_FALSE)
    {
        GLsizei len;
        static GLchar buf[4096];
        glGetProgramInfoLog(program, 4096, &len, buf);
        buf[len] = '\0';
        MessageBox(NULL, buf, "Error", MB_OK);
        exit(0);
    }
}

GLuint loadShader(const char* pixelShaderFileName, const char* vertexShaderFileName)
{
    GLuint shaderV = glCreateShader(GL_VERTEX_SHADER);
    GLuint shaderP = glCreateShader(GL_FRAGMENT_SHADER);
    char* v = readFile(vertexShaderFileName);
    char* p = readFile(pixelShaderFileName);
    glShaderSource(shaderV, 1, (const char**) &v, NULL);
    glShaderSource(shaderP, 1, (const char**) &p, NULL);
    glCompileShader(shaderV);
    glCompileShader(shaderP);
    GLuint result = glCreateProgram();
    glAttachShader(result, shaderV);
    glAttachShader(result, shaderP);
    glLinkProgram(result);

    checkShaderStatus(shaderV);
    checkShaderStatus(shaderP);
    checkProgramStatus(result);

    return result;
}

GLuint loadShader(const char* pixelShaderFileName1, const char* pixelShaderFileName2, const char* vertexShaderFileName)
{
    GLuint shaderV = glCreateShader(GL_VERTEX_SHADER);
    GLuint shaderP1 = glCreateShader(GL_FRAGMENT_SHADER);
    GLuint shaderP2 = glCreateShader(GL_FRAGMENT_SHADER);
    char* v = readFile(vertexShaderFileName);
    char* p1 = readFile(pixelShaderFileName1);
    char* p2 = readFile(pixelShaderFileName2);
    glShaderSource(shaderV, 1, (const char**) &v, NULL);
    glShaderSource(shaderP1, 1, (const char**) &p1, NULL);
    glShaderSource(shaderP2, 1, (const char**) &p2, NULL);
    glCompileShader(shaderV);
    glCompileShader(shaderP1);
    glCompileShader(shaderP2);
    GLuint result = glCreateProgram();
    glAttachShader(result, shaderV);
    glAttachShader(result, shaderP1);
    glAttachShader(result, shaderP2);
    glLinkProgram(result);

    checkShaderStatus(shaderV);
    checkShaderStatus(shaderP1);
    checkShaderStatus(shaderP2);
    checkProgramStatus(result);

    return result;
}


GLuint loadTexture(char* fileName, char* alphaName, bool clamp)
{
    GLuint result;
    glGenTextures(1, &result);

    GLuint components = 3;
    GLuint format = GL_RGB;

    AUX_RGBImageRec* img = auxDIBImageLoad(fileName);
    if (alphaName != NULL)
    {
        AUX_RGBImageRec* alpha = auxDIBImageLoad(alphaName);
        unsigned char *mix = new unsigned char [img->sizeX * img->sizeY * 4];
        for (int i = 0; i < img->sizeX * img->sizeY; i++)
        {
            mix[4 * i]     = img->data[3 * i];
            mix[4 * i + 1] = img->data[3 * i + 1];
            mix[4 * i + 2] = img->data[3 * i + 2];
            mix[4 * i + 3] = alpha->data[3 * i];
        }
        img->data = mix;
        components = 4;
        format = GL_RGBA;
    }

    glBindTexture(GL_TEXTURE_2D, result);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    if (clamp)
    {
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    }
    else
    {
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    }
    gluBuild2DMipmaps(GL_TEXTURE_2D, components, img->sizeX, img->sizeY, format, GL_UNSIGNED_BYTE, img->data);

    return result;
}

int main(int argc, char **argv)
{
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
    glutInitWindowSize(1024, 1024);
    glutCreateWindow("");

    glutDisplayFunc(display);
    glutReshapeFunc(reshape);
    glutKeyboardFunc(keypress);
    glutKeyboardUpFunc(keyup);
    glutIdleFunc(animate);
    glutPassiveMotionFunc(mouse);
    glutSetCursor(GLUT_CURSOR_NONE);

    DEVMODE dm;
    memset(&dm, 0, sizeof(dm));
    dm.dmSize = sizeof(dm);
    dm.dmFields = DM_BITSPERPEL | DM_PELSWIDTH | DM_PELSHEIGHT;
    dm.dmBitsPerPel = 32;
    dm.dmPelsWidth = 800;
    dm.dmPelsHeight = 600;
    ChangeDisplaySettings(&dm, CDS_FULLSCREEN);
    glutFullScreen();

    glewInit();
    newFBO(&reflectionFBO, &reflectionTexture);
    newFBO(&refractionFBO, &refractionTexture);
    newFBO(&waterDepthFBO, &waterDepthTexture);

    waterShader       = loadShader("data/shaders/water.p", "data/shaders/water.v");
    skyShader         = loadShader("data/shaders/sky.p", "data/shaders/sky.v");
    islandUpShader    = loadShader("data/shaders/island.p", "data/shaders/island_up.p", "data/shaders/island.v");
    islandDownShader  = loadShader("data/shaders/island.p", "data/shaders/island_down.p", "data/shaders/island.v");
    islandDepthShader = loadShader("data/shaders/island_depth.p", "data/shaders/island_depth.v");
    torusShader       = loadShader("data/shaders/torus.p", "data/shaders/torus.v");

    underWaterTexture  = loadTexture("data/underwater.bmp", NULL, false);
    terrainTexture     = loadTexture("data/terrain.bmp",    NULL, false);
    waterRippleTexture = loadTexture("data/water.bmp",      NULL, false);
    horizonTexture     = loadTexture("data/horizon.bmp",    NULL, true);
    cloudTexture       = loadTexture("data/cloud.bmp",      NULL, false);
    fireTexture        = loadTexture("data/fire.bmp", "data/fire_alpha.bmp", true);
    torusTexture       = loadTexture("data/torus.bmp",      NULL, false);

    glClearColor(0.0, 0.0, 0.0, 1.0);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_TEXTURE_2D);
    glDepthFunc(GL_LEQUAL);
    glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
    glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
    glEnable(GL_NORMALIZE);
    glShadeModel(GL_SMOOTH);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);

    t = 0;
    camera = Vector3D(0, 0, 10);
    cameraPhi = 0;
    cameraPsi = 0;

    movingForward = false;
    movingBackward = false;
    movingLeft = false;
    movingRight = false;

    buildIslandList();
    buildTorusList();
    generateParticles();

    glutMainLoop();

    return 0;
}