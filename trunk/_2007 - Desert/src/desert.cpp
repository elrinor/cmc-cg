#define _CRT_SECURE_NO_DEPRECATE
#define GLUT_DISABLE_ATEXIT_HACK
#include <Windows.h>
#include <gl/glut.h>
#include <gl/gl.h>
#include <gl/glu.h>
#include <gl/glext.h>
#include <gl/glaux.h>
#include <cstdlib>
#include <vector>
#include <algorithm>
#pragma comment(lib, "glaux.lib")

#include "BmpLoad.h"
#include "Vector3D.h"

#define PI 3.14159265358979

#define LAND_SIZE 128
#define LAND_STEP 0.5f
#define LAND_MAX_COORD (LAND_SIZE * LAND_STEP / 2)
#define sqr(x) ((x)*(x))

#define FULLSCREEN_X 640
#define FULLSCREEN_Y 480

#define SPHERE_PHITESS 32
#define SPHERE_PSITESS 32

using namespace std;

void Normal ( Vector3D vector )
    {
    glNormal3f ( vector.x, vector.y, vector.z );
    }
    
void Vertex ( Vector3D vector )
    {
    glVertex3f ( vector.x, vector.y, vector.z );
    }
    
void TexCoord ( Vector3D vector )
    {
    glTexCoord3f ( vector.x, vector.y, vector.z );
    }
    
class Face
    {
public:
    Vector3D v[4], t[4], n[4];
    float dist;
    void draw() {
        for ( int i = 0; i < 4; i++ ) {
            Normal ( n[i] );
            TexCoord ( t[i] );
            Vertex ( v[i] );
            }
        }
    };
    
float t = 0, dt;

int viewport_x, viewport_y; // viewport size

Vector3D camera, direction; // camera position

float m_phi, m_psi; // camera rotation angles

GLuint LAND = 1, SKY = 2; // display lists

GLuint DIRT, WATER, PZ, NZ, PX, NX, PY, NY, SPHERE; // textures

bool motionF, motionB, motionL, motionR; // moving?

bool zoomP, zoomM; // zooming?

float focus = 5;

enum {NORMAL = 0, FSAA = 1, FIELD = 2};
int renderMode;

Face sphere[SPHERE_PHITESS][SPHERE_PSITESS];
vector<Face*> sortedUp, sortedDown;
vector<Face*>* sorted;

Vector3D** land; // land

Vector3D getLandPoint ( float x, float y )
    {
    int ix1 = x / LAND_STEP + LAND_SIZE / 2;
    int ix2 = ix1 + 1;
    int iy1 = y / LAND_STEP + LAND_SIZE / 2;
    int iy2 = iy1 + 1;
    
    float x1 = ( ix1 - LAND_SIZE / 2 ) * LAND_STEP;
    float x2 = ( ix2 - LAND_SIZE / 2 ) * LAND_STEP;
    float y1 = ( iy1 - LAND_SIZE / 2 ) * LAND_STEP;
    float y2 = ( iy2 - LAND_SIZE / 2 ) * LAND_STEP;
    
    Vector3D up = ( land[ix1][iy1] * ( y2 - y ) + land[ix1][iy2] * ( y - y1 ) ) / ( y2 - y1 );
    Vector3D dn = ( land[ix2][iy1] * ( y2 - y ) + land[ix2][iy2] * ( y - y1 ) ) / ( y2 - y1 );
    Vector3D result = ( up * ( x2 - x ) + dn * ( x - x1 ) ) / ( x2 - x1 );
    return result;
    }
    
float* makeArray ( float f1, float f2, float f3, float f4 )
    {
    static float arr[4];
    arr[0] = f1;
    arr[1] = f2;
    arr[2] = f3;
    arr[3] = f4;
    return arr;
    }
    
double* makeArrayD ( double f1, double f2, double f3, double f4 )
    {
    static double arr[4];
    arr[0] = f1;
    arr[1] = f2;
    arr[2] = f3;
    arr[3] = f4;
    return arr;
    }
    
void spherePoint ( int i, int j, Vector3D& v, Vector3D& n, Vector3D& t )
    {
    float phi = 2 * PI * i / SPHERE_PHITESS;
    float psi = 2 * PI * j / SPHERE_PSITESS;
    n = Vector3D ( sin ( phi ) * cos ( psi ), cos ( phi ) * cos ( psi ), sin ( psi ) );
    v = 0.5 * n + Vector3D ( 0, 0, 5 ) + Vector3D ( sin ( phi ), cos ( phi ), 0);
    t = Vector3D ( 6 * phi / ( 2 * PI ), 2 * psi / (2 * PI), 0 );
    }
    
void genSphere()
    {
    for ( int i = 0; i < SPHERE_PHITESS; i++ ) {
        for ( int j = 0; j < SPHERE_PSITESS; j++ ) {
            Face f;
            spherePoint ( i  , j  , f.v[0], f.n[0], f.t[0] );
            spherePoint ( i+1, j  , f.v[1], f.n[1], f.t[1] );
            spherePoint ( i+1, j+1, f.v[2], f.n[2], f.t[2] );
            spherePoint ( i  , j+1, f.v[3], f.n[3], f.t[3] );
            sphere[i][j] = f;
            sortedUp.push_back ( &sphere[i][j] );
            sortedDown.push_back ( &sphere[i][j] );
            }
        }
    }
    
void sortSphere()
    {
    for ( unsigned int i = 0; i < sorted->size(); i++ )
        ( *sorted ) [i]->dist = ( ( *sorted ) [i]->v[0] - camera ).lengthSq();
    class cmp
        {
    public:
        bool operator() ( const Face* a, const Face* b ) {
            return a->dist > b->dist;
            }
        };
    sort ( sorted->begin(), sorted->end(), cmp() );
    }
    
void drawSphere()
    {
    glEnable ( GL_BLEND );
    glBindTexture ( GL_TEXTURE_2D, SPHERE );
    glBegin ( GL_QUADS );
    for ( unsigned int i = 0; i < sorted->size(); i++ )
        ( *sorted ) [i]->draw();
    glEnd();
    glDisable ( GL_BLEND );
    }
    
float rnd ( float f )
    {
    return f * rand() / RAND_MAX;
    }
    
void addMound ( Vector3D** land, float x, float y, float r, float h )
    {
    for ( int ix = 0; ix < LAND_SIZE; ix++ )
        for ( int iy = 0; iy < LAND_SIZE; iy++ )
            land[ix][iy].z += h * max ( 1 / ( 1 + ( sqr ( land[ix][iy].x - x ) + sqr ( land[ix][iy].y - y ) ) * 4 / sqr ( r ) ) - 0.2, 0 );
    }
    
void landVertex ( Vector3D** land, int ix, int iy )
    {
    TexCoord ( land[ix][iy] / 10 );
    Normal ( ( land[ix + 1][iy] - land[ix - 1][iy] ) ^ ( land[ix][iy + 1] - land[ix][iy - 1] ) );
    Vertex ( land[ix][iy] );
    }
    
void generateDesertList()
    {
    land = new Vector3D*[LAND_SIZE];
    for ( int i = 0; i < LAND_SIZE; i++ )
        land[i] = new Vector3D[LAND_SIZE];
        
    for ( int ix = 0; ix < LAND_SIZE; ix++ ) {
        for ( int iy = 0; iy < LAND_SIZE; iy++ ) {
            land[ix][iy].x = ( ix - LAND_SIZE / 2 ) * LAND_STEP;
            land[ix][iy].y = ( iy - LAND_SIZE / 2 ) * LAND_STEP;
            land[ix][iy].z = 3;
            }
        }
        
    addMound ( land, 0, 0, 15, -15 );
    
    for ( int i = 0; i < 1000; i++ ) {
        float x = rnd ( LAND_MAX_COORD ) * 2 - LAND_MAX_COORD;
        float y = rnd ( LAND_MAX_COORD ) * 2 - LAND_MAX_COORD;
        if ( sqr ( x ) + sqr ( y ) < 25 )
            continue;
        addMound ( land, x, y, 1.0 + rnd ( 8 ), rnd ( 4 ) - 2 );
        }
        
    for ( int i = 0; i < 64; i++ ) {
        float phi = 2 * PI * i / 64;
        float x = LAND_MAX_COORD * sin ( phi );
        float y = LAND_MAX_COORD * cos ( phi );
        addMound ( land, x, y, 5.0 + rnd ( 8 ), 5 + rnd ( 5 ) );
        }
        
    glNewList ( LAND, GL_COMPILE );
    glBegin ( GL_QUADS );
    for ( int ix = 1; ix < LAND_SIZE - 2; ix++ ) {
        for ( int iy = 1; iy < LAND_SIZE - 2; iy++ ) {
            landVertex ( land, ix    , iy    );
            landVertex ( land, ix + 1, iy    );
            landVertex ( land, ix + 1, iy + 1 );
            landVertex ( land, ix    , iy + 1 );
            }
        }
    glEnd();
    glEndList();
    }
    
void quad ( Vector3D v, Vector3D x, Vector3D y )
    {
    glBegin ( GL_QUADS );
    glTexCoord2f ( 0, 0 ); Vertex ( v        );
    glTexCoord2f ( 1, 0 ); Vertex ( v + x    );
    glTexCoord2f ( 1, 1 ); Vertex ( v + x + y );
    glTexCoord2f ( 0, 1 ); Vertex ( v     + y );
    glEnd();
    }
    
void generateSkyList()
    {
    const float r = 100;
    glNewList ( SKY, GL_COMPILE );
    glBindTexture ( GL_TEXTURE_2D, PZ );
    quad ( Vector3D ( -r, r, r ), Vector3D ( 2 * r, 0, 0 ), Vector3D ( 0, -2 * r, 0 ) );
    glBindTexture ( GL_TEXTURE_2D, PX );
    quad ( Vector3D ( r, r, -r ), Vector3D ( 0, -2 * r, 0 ), Vector3D ( 0, 0, 2 * r ) );
    glBindTexture ( GL_TEXTURE_2D, NX );
    quad ( Vector3D ( -r, -r, -r ), Vector3D ( 0, 2 * r, 0 ), Vector3D ( 0, 0, 2 * r ) );
    glBindTexture ( GL_TEXTURE_2D, PY );
    quad ( Vector3D ( -r, r, -r ), Vector3D ( 2 * r, 0, 0 ), Vector3D ( 0, 0, 2 * r ) );
    glBindTexture ( GL_TEXTURE_2D, NY );
    quad ( Vector3D ( r, -r, -r ), Vector3D ( -2 * r, 0, 0 ), Vector3D ( 0, 0, 2 * r ) );
    glEndList();
    }
    
void drawLand()
    {
    glBindTexture ( GL_TEXTURE_2D, DIRT );
    glCallList ( LAND );
    }
    
void drawSky()
    {
    glPushMatrix();
    glTranslatef ( camera.x, camera.y, camera.z );
    glDisable ( GL_LIGHTING );
    glColor3f ( 1, 1, 1 );
    glCallList ( SKY );
    glPopMatrix();
    glEnable ( GL_LIGHTING );
    }
    
void render()
    {
    glClear ( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT );
    glMatrixMode ( GL_PROJECTION );
    glLoadIdentity();
    gluPerspective ( 60.0, ( float ) viewport_x / viewport_y, 0.01, 200 );
    glMatrixMode ( GL_MODELVIEW );
    glLoadIdentity();
    gluLookAt ( camera.x, camera.y, camera.z, camera.x + direction.x, camera.y + direction.y, camera.z + direction.z, 0, 0, 1 );
    
    glPushMatrix();
    glScalef ( 1, 1, -1 );
    glLightfv ( GL_LIGHT0, GL_POSITION, makeArray ( 0.0, 100.0, 100.0, 1.0 ) );
    glEnable ( GL_CLIP_PLANE0 );
    glClipPlane ( GL_CLIP_PLANE0, makeArrayD ( 0, 0, 1, 0.01 ) );
    drawLand();
    drawSky();
    sorted = &sortedDown;
    drawSphere();
    glDisable ( GL_CLIP_PLANE0 );
    glPopMatrix();
    glLightfv ( GL_LIGHT0, GL_POSITION, makeArray ( 0.0, 100.0, 100.0, 1.0 ) );
    
    glBindTexture ( GL_TEXTURE_2D, WATER );
    glEnable ( GL_BLEND );
    glDisable ( GL_LIGHTING );
    glColor4f ( 1, 1, 1, 0.3 );
    glBegin ( GL_QUADS );
    float texdx = 0.2 * sin ( 0.3 * t );
    float texdy = 0.2 * cos ( 0.3 * t );
    glTexCoord2f ( -3 + texdx, -3 + texdy ); glVertex3f ( -30, -30, 0 );
    glTexCoord2f ( 3 + texdx, -3 + texdy ); glVertex3f ( 30, -30, 0 );
    glTexCoord2f ( 3 + texdx,  3 + texdy ); glVertex3f ( 30,  30, 0 );
    glTexCoord2f ( -3 + texdx,  3 + texdy ); glVertex3f ( -30,  30, 0 );
    glEnd();
    glEnable ( GL_LIGHTING );
    glDisable ( GL_BLEND );
    
    drawLand();
    drawSky();
    sorted = &sortedUp;
    drawSphere();
    }
    
void fsaaPass ( Vector3D oldCamera, Vector3D focusPoint, Vector3D d, int count )
    {
    camera = oldCamera;
    direction = focusPoint + d - camera;
    render();
    glAccum ( GL_ACCUM, 1.0f / count );
    }
    
void fieldPass ( Vector3D oldCamera, Vector3D focusPoint, Vector3D d, int count )
    {
    camera = oldCamera + d;
    direction = focusPoint - camera;
    render();
    glAccum ( GL_ACCUM, 1.0f / count );
    }
    
void _display()
    {
    glClear ( GL_ACCUM_BUFFER_BIT );
    
    sorted = &sortedUp;
    sortSphere();
    
    camera.z = -camera.z;
    sorted = &sortedDown;
    sortSphere();
    camera.z = -camera.z;
    
    if ( renderMode == NORMAL ) {
        direction = Vector3D ( sin ( m_phi ) * cos ( m_psi ), cos ( m_phi ) * cos ( m_psi ), sin ( m_psi ) );
        render();
        glAccum ( GL_ACCUM, 1.0f );
        glAccum ( GL_RETURN, 1.0f );
        }
    else if ( renderMode == FSAA ) {
        Vector3D focusPoint = camera + Vector3D ( sin ( m_phi ) * cos ( m_psi ), cos ( m_phi ) * cos ( m_psi ), sin ( m_psi ) );
        Vector3D dx = ( ( focusPoint - camera ) ^ Vector3D ( 0, 0, 1 ) ).normalize();
        Vector3D dy = ( dx ^ ( focusPoint - camera ) ).normalize();
        float focusK = 0.0008;
        dx *= focusK;
        dy *= focusK;
        Vector3D oldCamera = camera;
        int count = 9;
        fsaaPass ( oldCamera, focusPoint, + dx + dy, count );
        fsaaPass ( oldCamera, focusPoint, - dx + dy, count );
        fsaaPass ( oldCamera, focusPoint, - dx - dy, count );
        fsaaPass ( oldCamera, focusPoint, + dx - dy, count );
        fsaaPass ( oldCamera, focusPoint,      + dy, count );
        fsaaPass ( oldCamera, focusPoint,      - dy, count );
        fsaaPass ( oldCamera, focusPoint, + dx     , count );
        fsaaPass ( oldCamera, focusPoint, - dx     , count );
        fsaaPass ( oldCamera, focusPoint, Vector3D ( 0, 0, 0 ), count );
        glAccum ( GL_RETURN, 1.0f );
        camera = oldCamera;
        }
    else if ( renderMode == FIELD ) {
        Vector3D focusPoint = camera + Vector3D ( sin ( m_phi ) * cos ( m_psi ), cos ( m_phi ) * cos ( m_psi ), sin ( m_psi ) ) * focus;
        Vector3D dx = ( ( focusPoint - camera ) ^ Vector3D ( 0, 0, 1 ) ).normalize();
        Vector3D dy = ( dx ^ ( focusPoint - camera ) ).normalize();
        float focusK = min ( 0.004 * focus, 0.05 );
        // float focusK = 0.02;
        dx *= focusK;
        dy *= focusK;
        Vector3D oldCamera = camera;
        int count = 9;
        fieldPass ( oldCamera, focusPoint, + dx + dy, count );
        fieldPass ( oldCamera, focusPoint, - dx + dy, count );
        fieldPass ( oldCamera, focusPoint, - dx - dy, count );
        fieldPass ( oldCamera, focusPoint, + dx - dy, count );
        fieldPass ( oldCamera, focusPoint,      + dy, count );
        fieldPass ( oldCamera, focusPoint,      - dy, count );
        fieldPass ( oldCamera, focusPoint, + dx     , count );
        fieldPass ( oldCamera, focusPoint, - dx     , count );
        fieldPass ( oldCamera, focusPoint, Vector3D ( 0, 0, 0 ), count );
        glAccum ( GL_RETURN, 1.0f );
        camera = oldCamera;
        }
        
    glutSwapBuffers();
    }
    
void _idle()
    {
    static float lastt = GetTickCount() / 1000.0f;
    dt = GetTickCount() / 1000.0f - lastt;
    t += dt;
    lastt = lastt + dt;
    glutPostRedisplay();
    
    const float speed = 10;
    
    if ( motionF )
        camera += Vector3D ( sin ( m_phi ) * cos ( m_psi ), cos ( m_phi ) * cos ( m_psi ), sin ( m_psi ) ) * dt * speed;
    if ( motionB )
        camera -= Vector3D ( sin ( m_phi ) * cos ( m_psi ), cos ( m_phi ) * cos ( m_psi ), sin ( m_psi ) ) * dt * speed;
    if ( motionL )
        camera -= Vector3D ( cos ( m_phi ), -sin ( m_phi ), 0 ) * dt * speed;
    if ( motionR )
        camera += Vector3D ( cos ( m_phi ), -sin ( m_phi ), 0 ) * dt * speed;
        
    if ( camera.z < 3 )
        camera.z = 3;
    if ( camera.z > 10 )
        camera.z = 10;
    if ( sqr ( camera.x ) + sqr ( camera.y ) > sqr ( LAND_MAX_COORD - 6 ) ) {
        float r = sqrt ( sqr ( camera.x ) + sqr ( camera.y ) );
        float maxR = LAND_MAX_COORD - 6;
        camera.x = camera.x * maxR / r;
        camera.y = camera.y * maxR / r;
        }
    float height = getLandPoint ( camera.x, camera.y ).z;
    if ( camera.z < height + 3 )
        camera.z = height + 3;
        
    const float zoomSpeed = 10;
    if ( zoomP )
        focus += zoomSpeed * dt;
    if ( zoomM )
        focus -= zoomSpeed * dt;
        
    if ( focus < 1 )
        focus = 1;
    if ( focus > 60 )
        focus = 60;
    }
    
GLuint loadBMP ( char* name )
    {
    int w, h;
    unsigned char* pData = LoadTrueColorBMPFile ( name, &w, &h );
    GLuint tex;
    glGenTextures ( 1, &tex );
    glBindTexture ( GL_TEXTURE_2D, tex );
    gluBuild2DMipmaps ( GL_TEXTURE_2D, 3, w, h, GL_RGB, GL_UNSIGNED_BYTE, pData );
    glTexParameteri ( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );
    glTexParameteri ( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );
    glTexParameteri ( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
    glTexParameteri ( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR );
    return tex;
    }
    
GLuint loadBMPClamp ( char* name )
    {
    int w, h;
    unsigned char* pData = LoadTrueColorBMPFile ( name, &w, &h );
    GLuint tex;
    glGenTextures ( 1, &tex );
    glBindTexture ( GL_TEXTURE_2D, tex );
    gluBuild2DMipmaps ( GL_TEXTURE_2D, 3, w, h, GL_RGB, GL_UNSIGNED_BYTE, pData );
    glTexParameteri ( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE );
    glTexParameteri ( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE );
    glTexParameteri ( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
    glTexParameteri ( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR );
    return tex;
    }
    
void _key ( unsigned char key, int x, int y )
    {
    if ( key == 27 )
        exit ( 0 );
    if ( key == 'w' || key == 'W' )
        motionF = true;
    if ( key == 's' || key == 'S' )
        motionB = true;
    if ( key == 'a' || key == 'A' )
        motionL = true;
    if ( key == 'd' || key == 'D' )
        motionR = true;
    if ( key == ' ' )
        renderMode = ( renderMode + 1 ) % 3;
    if ( key == '+' )
        zoomP = true;
    if ( key == '-' )
        zoomM = true;
    }
    
void _keyup ( unsigned char key, int x, int y )
    {
    if ( key == 'w' || key == 'W' )
        motionF = false;
    if ( key == 's' || key == 'S' )
        motionB = false;
    if ( key == 'a' || key == 'A' )
        motionL = false;
    if ( key == 'd' || key == 'D' )
        motionR = false;
    if ( key == '+' )
        zoomP = false;
    if ( key == '-' )
        zoomM = false;
    }
    
void _mousemove ( int x, int y )
    {
    const float sense = 0.01f;
    if ( x != viewport_x / 2 || y != viewport_y / 2 ) {
        m_phi += ( x - viewport_x / 2 ) * sense;
        m_psi -= ( y - viewport_y / 2 ) * sense;
        if ( m_psi >  PI * 0.4 )
            m_psi =  PI * 0.4;
        if ( m_psi < -PI * 0.4 )
            m_psi = -PI * 0.4;
        glutWarpPointer ( viewport_x / 2, viewport_y / 2 );
        }
    }
    
    
void _reshape ( int w, int h )
    {
    glViewport ( 0, 0, w, h );
    viewport_x = w;
    viewport_y = h;
    }
    
int main ( int argc, char** argv )
    {
    glutInit ( &argc,argv );
    glutInitDisplayMode ( GLUT_DOUBLE | GLUT_DEPTH | GLUT_RGBA | GLUT_STENCIL | GLUT_ACCUM );
    glutInitWindowSize ( FULLSCREEN_X, FULLSCREEN_Y );
    glutCreateWindow ( "" );
    
    glutIdleFunc ( _idle );
    glutDisplayFunc ( _display );
    glutKeyboardFunc ( _key );
    glutKeyboardUpFunc ( _keyup );
    glutPassiveMotionFunc ( _mousemove );
    glutReshapeFunc ( _reshape );
    
    DEVMODE d;
    memset ( &d, 0, sizeof ( d ) );
    d.dmSize = sizeof ( d );
    d.dmPelsWidth = FULLSCREEN_X;
    d.dmPelsHeight = FULLSCREEN_Y;
    d.dmBitsPerPel = 32;
    d.dmFields = DM_BITSPERPEL | DM_PELSWIDTH | DM_PELSHEIGHT;
    ChangeDisplaySettings ( &d, CDS_FULLSCREEN );
    glutFullScreen();
    
    glutSetCursor ( GLUT_CURSOR_NONE );
    glutIgnoreKeyRepeat ( 1 );
    glutWarpPointer ( FULLSCREEN_X / 2, FULLSCREEN_Y / 2 );
    
    DIRT   = loadBMP ( "data/dirt.bmp" );
    WATER  = loadBMP ( "data/water.bmp" );
    SPHERE = loadBMP ( "data/tux.bmp" );
    PX     = loadBMPClamp ( "data/skybox_px.bmp" );
    NX     = loadBMPClamp ( "data/skybox_nx.bmp" );
    PY     = loadBMPClamp ( "data/skybox_py.bmp" );
    NY     = loadBMPClamp ( "data/skybox_ny.bmp" );
    PZ     = loadBMPClamp ( "data/skybox_pz.bmp" );
    NZ     = loadBMPClamp ( "data/skybox_nz.bmp" );
    srand ( GetTickCount() );
    generateDesertList();
    generateSkyList();
    genSphere();
    camera = Vector3D ( 0, 0, 0 );
    renderMode = NORMAL;
    
    glClearColor ( 0, 0, 0, 0 );
    glPolygonMode ( GL_FRONT_AND_BACK, GL_FILL );
    glShadeModel ( GL_SMOOTH );
    glHint ( GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST );
    glBlendFunc ( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
    glEnable ( GL_TEXTURE_2D );
    glEnable ( GL_NORMALIZE );
    glEnable ( GL_DEPTH_TEST );
    
    glMaterialfv ( GL_FRONT_AND_BACK, GL_EMISSION, makeArray ( 0.4, 0.4, 0.4, 0.0 ) );
    glMaterialfv ( GL_FRONT_AND_BACK, GL_DIFFUSE,  makeArray ( 1.0, 1.0, 1.0, 0.3 ) );
    glMaterialfv ( GL_FRONT_AND_BACK, GL_SPECULAR, makeArray ( 1.0, 1.0, 1.0, 0.0 ) );
    glMaterialfv ( GL_FRONT_AND_BACK, GL_AMBIENT,  makeArray ( 1.0, 1.0, 1.0, 0.0 ) );
    
    glEnable ( GL_LIGHTING );
    glEnable ( GL_LIGHT0 );
    glLightfv ( GL_LIGHT0, GL_POSITION, makeArray ( 0.0, 0.0, 9.9, 1.0 ) );
    glLightfv ( GL_LIGHT0, GL_DIFFUSE,  makeArray ( 1.0, 1.0, 1.0, 1.0 ) );
    glLightfv ( GL_LIGHT0, GL_SPECULAR, makeArray ( 0.0, 0.0, 0.0, 1.0 ) );
    glLightfv ( GL_LIGHT0, GL_AMBIENT,  makeArray ( 0.0, 0.0, 0.0, 1.0 ) );
    
    glutMainLoop();
    return 0;
    }