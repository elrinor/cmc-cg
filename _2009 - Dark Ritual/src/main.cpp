#include <Windows.h>
#include <gl/glew.h>
#include <gl/GLAux.h>
#include <gl/glut.h>
#include <gl/GLU.h>
#include <gl/glext.h>
#include <vector>
#include <algorithm>
#include <map>
#include <iostream>
#include "Vector3D.h"
#include "Matrix4x4.h"

#pragma comment(lib, "glaux.lib")
#pragma comment(lib, "glew32.lib")

#ifndef M_PI
#  define M_PI 3.141592653589793238462643383
#endif

using namespace std;

// -------------------------------------------------------------------------- //
// 
// -------------------------------------------------------------------------- //
void glNormal(const Vector3D& v) {
  glNormal3f(v.x, v.y, v.z);
}

void glVertex(const Vector3D& v) {
  glVertex3f(v.x, v.y, v.z);
}

void glTexCoord(const Vector3D& v) {
  glTexCoord2f(v.x, v.y);
}

/**
 * Случ. число от 0 до 1
 */
float random() {
  return 1.0f * rand() / (RAND_MAX + 1);
}

/**
 * Случ. вектор единичной длины
 */
Vector3D randomUnitVector() {
  return Vector3D(random() - 0.5, random() - 0.5, random() - 0.5).normalize();
}


/**
 * Кривая, задающая круг в плоскости xy
 */
class GCircle {
public:
  GCircle(float r): mR(r) {}

  /* u in [0..1) */
  Vector3D operator()(float u) const {
    return Vector3D(mR * sin(u * 2 * M_PI), mR * cos(u * 2 * M_PI), 0);
  }

private:
  float mR;
};

/**
 * Кривая, задающая узел
 */
class GKnot {
public:
  GKnot(float r, float rp): mR(r), mRp(rp) {}

  /* u in [0..1) */
  Vector3D operator()(float u) const {
    u = u * 12 * M_PI;
    Vector3D result;
    static const float R = 2;
    static const float r = 0.5;
    result.x = (mR + 1.5  * mRp * cos(u / 2)) * cos(u / 3);
    result.y = (mR + 1.5  * mRp * cos(u / 2)) * sin(u / 3);
    result.z = (     1.05 * mRp * sin(u / 2));
    return result;
  }

private:
  float mR, mRp;
};

/**
 * Поверхность, полученная протягиванием одной кривой по другой
 */
template<class InnerCurve, class OuterCurve>
class GTorus {
public:
  GTorus(const InnerCurve& innerCurve, const OuterCurve& outerCurve): mInnerCurve(innerCurve), mOuterCurve(outerCurve) {}

  /* u, v in [0..1) */
  Vector3D operator()(float u, float v) const {
    Vector3D c = mInnerCurve(u);
    Vector3D z = (mInnerCurve(u + 0.001) - c).normalize();
    Vector3D x = z ^ Vector3D(0,0,1);
    Vector3D y = z ^ x;
    Vector3D d = mOuterCurve(v);
    return c + x * d.x + y * d.y + z * d.z;
  }

private:
  InnerCurve mInnerCurve;
  OuterCurve mOuterCurve;
};

/**
 * Нормаль для данной поверхности
 */
template<class Surface>
class GNormal {
public:
  GNormal(const Surface& surface): mSurface(surface) {}

  Vector3D operator()(float u, float v) const {
    return ((mSurface(u + 0.001, v) - mSurface(u, v)) ^ (mSurface(u, v + 0.001) - mSurface(u, v))).normalize();
  }

private:
  const Surface& mSurface;
};


// -------------------------------------------------------------------------- //
// 
// -------------------------------------------------------------------------- //
/**
 * "Универсальный" класс модели
 */
class Model {
public:
  /**
   * Создание модели-"коробки"
   */
  Model(const Vector3D& c, Vector3D& x, const Vector3D& y, const Vector3D& z) {
    mCoords.resize(8);
    mCoords[0] = c;
    mCoords[1] = c + x;
    mCoords[2] = c + y;
    mCoords[3] = c + x + y;
    mCoords[4] = c + z;
    mCoords[5] = c + z + x;
    mCoords[6] = c + z + y;
    mCoords[7] = c + z + x + y;

    mTexCoords.resize(4);
    mTexCoords[0] = Vector3D(0, 0, 0);
    mTexCoords[1] = Vector3D(0, 1, 0);
    mTexCoords[2] = Vector3D(1, 1, 0);
    mTexCoords[3] = Vector3D(1, 0, 0);

    addQuad(0, 1, 3, 2, 0, 1, 2, 3);
    addQuad(1, 5, 7, 3, 0, 1, 2, 3);
    addQuad(3, 7, 6, 2, 0, 1, 2, 3);
    addQuad(2, 6, 4, 0, 0, 1, 2, 3);
    addQuad(0, 4, 5, 1, 0, 1, 2, 3);
    addQuad(4, 6, 7, 5, 0, 1, 2, 3);

    makeEdges();
    setTransform(Vector3D(0, 0, 0), 0, 0);
    mCompiled = false;
    mSorted = false;
  }

  /**
   * Создание модели-поверхности
   *
   * un, vn - разбиения по u и v
   * ti, tv - растяжение текстур
   */
  template<class Surface>
  Model(const Surface& surface, int un, int vn, float tu, float tv) {
    GNormal<Surface> normal(surface);

    // Вычисляем координаты / нормали / касательные
    mCoords.resize(un * vn);
    mNormals.resize(un * vn);
    muTangents.resize(un * vn);
    mvTangents.resize(un * vn);
    mTexCoords.resize((un + 1) * (vn + 1));
    for(int iu = 0; iu <= un; iu++) {
      for(int iv = 0; iv <= vn; iv++) {
        float u = 1.0 * iu / un, v = 1.0 * iv / vn;
        if(iu < un && iv < vn) {
          mCoords[iu * vn + iv] = surface(u, v);
          mNormals[iu * vn + iv] = normal(u, v);
          muTangents[iu * vn + iv] = (surface(u + 0.001, v) - surface(u, v)).normalize();
          mvTangents[iu * vn + iv] = (surface(u, v + 0.001) - surface(u, v)).normalize();
        }
        mTexCoords[iu * (vn + 1) + iv] = Vector3D(u * tu, v * tv, 0);
      }
    }

    // добавляем грани
    for(int iu0 = 0; iu0 < un; iu0++) {
      int iu1 = (iu0 + 1) % un;
      for(int iv0 = 0; iv0 < vn; iv0++) {
        int iv1 = (iv0 + 1) % vn;
        addQuad(
          iu0 *vn + iv0, iu0 * vn + iv1, iu1 * vn + iv1, iu1 * vn + iv0,
          iu0 * vn + iv0, iu0 * vn + iv1, iu1 * vn + iv1, iu1 * vn + iv0,
          iu0 * (vn + 1) + iv0, iu0 * (vn + 1) + iv0 + 1, (iu0 + 1) * (vn + 1) + iv0 + 1, (iu0 + 1) * (vn + 1) + iv0
        );
      }
    }

    // создаем ребра
    makeEdges();

    setTransform(Vector3D(0, 0, 0), 0, 0);
    mCompiled = false;
    mSorted = false;
  }

  /**
   * Задать трансформацию для модели
   */
  void setTransform(Vector3D pos, float phi, float psi) {
    glPushMatrix();
    glLoadIdentity();
    glTranslatef(pos.x, pos.y, pos.z);
    glRotatef(phi / M_PI * 180, 0, 0, 1);
    glRotatef(psi / M_PI * 180, 1, 0, 0);
    glGetFloatv(GL_MODELVIEW_MATRIX, (float*) objToWorld.m);

    worldToObj = objToWorld.inverse();
    glPopMatrix();
  }

  /**
   * Отсортировать грани по удалению от камеры
   *
   * pcam - мировые координаты камеры
   */
  void sort(const Vector3D& pcam) {
    Vector3D cam = worldToObj * pcam;

    /* Заполняем массив, задающий порядок, числами от 0 до ... */
    if(mOrder.size() != mFaces.size()) {
      mOrder.resize(mFaces.size());
      for(unsigned int i = 0; i < mFaces.size(); i++)
        mOrder[i] = i;
    }

    /* Считаем расстояния от камеры до треугольника */
    for(unsigned int i = 0; i < mFaces.size(); i++)
      mFaces[i].dist = (*mFaces[i].c0 - cam).lengthSq();

    /* Компаратор */
    class OrderCmp {
    public:
      OrderCmp(const vector<Face>& v): mV(v) {}

      bool operator()(int a, int b) const {
        return mV[a].dist > mV[b].dist;
      }

    private:
      const vector<Face>& mV;
    };

    /* Сортируем по рассотянию */
    std::sort(mOrder.begin(), mOrder.end(), OrderCmp(mFaces));

    mSorted = true;
  }

  /**
   * Подготовка к отрисовке теневых объемов
   *
   * pl - мировые координаты источника света
   */
  void prepareShadowVolumes(const Vector3D& pl) {
    Vector3D l = worldToObj * pl;

    for(unsigned int i = 0; i < mFaces.size(); i++)
      mFaces[i].facingLight = (mFaces[i].n & (*mFaces[i].c0 - l)) > 0;
  }

  /**
   * Отрисовать часть теневого объема
   *
   * pl - мировые координаты источника света
   * cullFront - обрезать передние / задние полигоны теневого объема
   */
  template<bool cullFront>
  void drawShadowVolume(const Vector3D& pl) {
    Vector3D l = worldToObj * pl;
    glPushMatrix();
    glMultMatrixf((float*) objToWorld.m);

    glBegin(GL_QUADS);
    for(unsigned int i = 0; i < mEdges.size(); i++) {
      if((cullFront && mEdges[i].f0->facingLight && !mEdges[i].f1->facingLight) || (!cullFront && !mEdges[i].f0->facingLight && mEdges[i].f1->facingLight)) {
        const Vector3D& v1 = *mEdges[i].c0;
        const Vector3D& v2 = *mEdges[i].c1;

        glVertex(v2);
        glVertex(v1);
        glVertex4f(v1.x - l.x, v1.y - l.y, v1.z - l.z, 0.0f);
        glVertex4f(v2.x - l.x, v2.y - l.y, v2.z - l.z, 0.0f);
      }
    }
    glEnd();

    glPopMatrix();
  } 

  /**
   * Просто отрисовать
   */
  void draw() const {
    glPushMatrix();
    glMultMatrixf((float*) objToWorld.m);

    if(mCompiled)
      glCallList(mList); // если откомпилировано - вызываем соотв. дисплейный список
    else {
      if(mSorted) { // если отсортировано - выводим в нужном порядке
        glBegin(GL_TRIANGLES);
        for(unsigned int i = 0; i < mOrder.size(); i++)
          mFaces[mOrder[i]].draw();
        glEnd();
      } else { // иначе просто выводим
        glBegin(GL_TRIANGLES);
        for(unsigned int i = 0; i < mFaces.size(); i++)
          mFaces[i].draw();
        glEnd();
      }
    }

    glPopMatrix();
  }

  /**
   * Отрисовать с bump-mapping'ом
   * 
   * pl - мировые координаты источника света
   */
  void drawBumped(const Vector3D& pl) {
    Vector3D l = worldToObj * pl; // считаем координаты источника света в коорд. системе объекта

    glPushMatrix();
    glMultMatrixf((float*) objToWorld.m);
    glBegin(GL_TRIANGLES);
    for(unsigned int i = 0; i < mFaces.size(); i++)
      mFaces[i].drawBumbed(l);
    glEnd();
    glPopMatrix();
  }

  /**
   * Скомпилировать модель
   */
  void compile() {
    if(!mCompiled) {
      mList = glGenLists(1);
      glNewList(mList, GL_COMPILE);
      glBegin(GL_TRIANGLES);
      for(unsigned int i = 0; i < mFaces.size(); i++)
        mFaces[i].draw();
      glEnd();
      glEndList();

      mCompiled = true;
    }
  }

private:
  /**
   * Создать массив ребер
   */
  void makeEdges() {
    map<std::pair<Vector3D*, Vector3D*>, Edge> edges;
    for(unsigned int i = 0; i < mFaces.size(); i++) {
      Face& f = mFaces[i];
      f.n = ((*f.c1 - *f.c0) ^ (*f.c2 - *f.c0)).normalize(); // считаем "реальную" нормаль в треугольнике
      
      edges[make_pair(f.c0, f.c1)].f0 = &f;
      edges[make_pair(f.c1, f.c2)].f0 = &f;
      edges[make_pair(f.c2, f.c0)].f0 = &f;

      edges[make_pair(f.c1, f.c0)].f1 = &f;
      edges[make_pair(f.c2, f.c1)].f1 = &f;
      edges[make_pair(f.c0, f.c2)].f1 = &f;
    }

    for(map<std::pair<Vector3D*, Vector3D*>, Edge>::iterator i = edges.begin(); i != edges.end(); i++) {
      Edge e = i->second;
      e.c0 = i->first.first; // устанавливаем указатели на концы ребра
      e.c1 = i->first.second;
      mEdges.push_back(e);
    }
  }

  /**
  * Добавить 4-угольник к модели
  *
  * cXX - индексы координат вершин
  * tXX - индексы текстурных координат в вершинах
  * nXX - индексы нормалей в вершинах
  */
  void addQuad(int c00, int c01, int c11, int c10, int n00, int n01, int n11, int n10, int t00, int t01, int t11, int t10) {
    Face f1;
    f1.c0 = &mCoords[c00];
    f1.c1 = &mCoords[c10];
    f1.c2 = &mCoords[c01];
    f1.n0 = &mNormals[n00];
    f1.n1 = &mNormals[n10];
    f1.n2 = &mNormals[n01];
    f1.t0 = &mTexCoords[t00];
    f1.t1 = &mTexCoords[t10];
    f1.t2 = &mTexCoords[t01];
    f1.ut0 = &muTangents[n00];
    f1.ut1 = &muTangents[n10];
    f1.ut2 = &muTangents[n01];
    f1.vt0 = &mvTangents[n00];
    f1.vt1 = &mvTangents[n10];
    f1.vt2 = &mvTangents[n01];
    
    Face f2;
    f2.c0 = &mCoords[c10];
    f2.c1 = &mCoords[c11];
    f2.c2 = &mCoords[c01];
    f2.n0 = &mNormals[n10];
    f2.n1 = &mNormals[n11];
    f2.n2 = &mNormals[n01];
    f2.t0 = &mTexCoords[t10];
    f2.t1 = &mTexCoords[t11];
    f2.t2 = &mTexCoords[t01];
    f2.ut0 = &muTangents[n10];
    f2.ut1 = &muTangents[n11];
    f2.ut2 = &muTangents[n01];
    f2.vt0 = &mvTangents[n10];
    f2.vt1 = &mvTangents[n11];
    f2.vt2 = &mvTangents[n01];

    mFaces.push_back(f1);
    mFaces.push_back(f2);
  }

  /**
   * Добавить 4-угольник к модели
   *
   * cXX - индексы координат вершин
   * tXX - индексы текстурных координат в вершинах
   */
  void addQuad(int c00, int c01, int c11, int c10, int t00, int t01, int t11, int t10) {
    mNormals.push_back(((mCoords[c10] - mCoords[c00]) ^ (mCoords[c01] - mCoords[c00])).normalize());
    muTangents.push_back((mCoords[c10] - mCoords[c00]).normalize());
    mvTangents.push_back((mCoords[c01] - mCoords[c00]).normalize());
    int n = mNormals.size() - 1;

    addQuad(c00, c01, c11, c10, n, n, n, n, t00, t01, t11, t10);
  }

  /**
   * Треугольник
   */
  struct Face {
    /** отрисовать треугольник */
    void draw() const {
      glTexCoord(*t0);
      glNormal(*n0);
      glVertex(*c0);
      glTexCoord(*t1);
      glNormal(*n1);
      glVertex(*c1);
      glTexCoord(*t2);
      glNormal(*n2);
      glVertex(*c2);
    }

    /** отрисовать треугольник с bump-mapping'ом.
     *
     * relLightPos - положение источника света в координатном пространстве объекта. */
    void drawBumbed(const Vector3D& relLightPos) const {
      Vector3D l0 = relLightPos - *c0;
      Vector3D x0 = Vector3D(*ut0 & l0, *vt0 & l0, *n0 & l0);
      glMultiTexCoord3fARB(GL_TEXTURE1_ARB, x0.x, x0.y, x0.z);
      glTexCoord(*t0);
      glNormal(*n0);
      glVertex(*c0);

      Vector3D l1 = relLightPos - *c1;
      Vector3D x1 = Vector3D(*ut1 & l1, *vt1 & l1, *n1 & l1);
      glMultiTexCoord3fARB(GL_TEXTURE1_ARB, x1.x, x1.y, x1.z);
      glTexCoord(*t1);
      glNormal(*n1);
      glVertex(*c1);

      Vector3D l2 = relLightPos - *c2;
      Vector3D x2 = Vector3D(*ut2 & l2, *vt2 & l2, *n2 & l2);
      glMultiTexCoord3fARB(GL_TEXTURE1_ARB, x2.x, x2.y, x2.z);
      glTexCoord(*t2);
      glNormal(*n2);
      glVertex(*c2);
    }

    Vector3D *c0, *c1, *c2; // указатели на координаты вершин
    Vector3D *n0, *n1, *n2; // указатели на нормали в вершинах
    Vector3D *t0, *t1, *t2; // указатели на текстурные координаты в вершинах
    Vector3D *ut0, *ut1, *ut2; // указатели на u-касательные в вершинах
    Vector3D *vt0, *vt1, *vt2; // указатели на v-касательные в вершинах
    Vector3D n; // "реальная" нормаль к треугольнику
    bool facingLight; // повернут ли треугольник к источнику света

    float dist; // расстояние от треугольника до камеры
  };

  /**
   * Ребро
   */
  struct Edge {
    Edge(): f0(NULL), f1(NULL) {}

    Vector3D *c0, *c1; // указатели на концы ребра
    Face *f0, *f1; // указатели на соседние треугольники
  };

  Matrix4x4 worldToObj, objToWorld; // матрицы преобразований

  vector<Vector3D> mCoords; // координаты вершин
  vector<Vector3D> mNormals; // нормали в вершинах
  vector<Vector3D> mTexCoords; // текстурные координаты в вершинах
  vector<Vector3D> muTangents; // u-касатальные
  vector<Vector3D> mvTangents; // v-касательные
  vector<Face> mFaces; // треугольники
  vector<Edge> mEdges; // ребра
  vector<int> mOrder; // порядок вывода (если нужен)

  bool mCompiled; // модель откомпилирована?
  bool mSorted; // модель отсортирована и использует порядок?
  
  GLuint mList; // дисплейный список для случая, когда модель откомпилирована
};

/**
 * Нарисовать 4-угольник 
 *
 * c, u, v - задают положение 4-угольника в пространстве
 * un, vn - на сколько quad'ов его разбить при отрисовке по u и v
 * tu, tv - коэф-т растяжения текстуры по u и v
 */
void drawQuad(const Vector3D& c, const Vector3D& u, const Vector3D& v, int un, int vn, float tu, float tv) {
  glBegin(GL_QUADS);
  glNormal((u ^ v).normalize());
  for(int iu = 0; iu < un; iu++) {
    float fu0 = 1.0 * iu / un;
    float fu1 = 1.0 * (iu + 1) / un;
    for(int iv = 0; iv < vn; iv++) {
      float fv0 = 1.0 * iv / vn;
      float fv1 = 1.0 * (iv + 1) / vn;
      
      glTexCoord(Vector3D(fu0 * tu, fv0 * tv, 0));
      glVertex(c + u * fu0 + v * fv0);
      glTexCoord(Vector3D(fu1 * tu, fv0 * tv, 0));
      glVertex(c + u * fu1 + v * fv0);
      glTexCoord(Vector3D(fu1 * tu, fv1 * tv, 0));
      glVertex(c + u * fu1 + v * fv1);
      glTexCoord(Vector3D(fu0 * tu, fv1 * tv, 0));
      glVertex(c + u * fu0 + v * fv1);
    }
  }
  glEnd();
}

/**
 * Нарисовать "коробку"
 */
void drawBox(const Vector3D& c, const Vector3D& x, const Vector3D& y, const Vector3D& z, int xn, int yn, int zn, float tx, float ty, float tz) {
  drawQuad(c, y, x, yn, xn, ty, tx);
  drawQuad(c, z, y, zn, yn, tz, ty);
  drawQuad(c, x, z, xn, zn, tx, tz);

  Vector3D s = c + x + y + z;
  drawQuad(s, -x, -y, xn, yn, tx, ty);
  drawQuad(s, -y, -z, yn, zn, ty, tz);
  drawQuad(s, -z, -x, zn, xn, tz, tx);
}


/**
 * Класс частицы
 */
class Particle {
public:
  Vector3D pos; // положение
  Vector3D speed; // вектор скорости движения
  Vector3D color; // цвет
  float alpha; // альфа-компонента
  float size; // размер
  float life; // "жизнь" от 1 до 0
  float lifeDecreaseSpeed; // скорость уменьшения "жизни", ед / сек.

  /**
   * Обновление частицы на время dt
   */
  void update(float dt) {
    life -= lifeDecreaseSpeed * dt;
    size = 0.25 + 0.25 * life;
    alpha = life / 4;
    color = Vector3D(1, 1, 0.5) * life + Vector3D(1, 0, 0) * (1 - life);
    pos += speed * dt;
  }

  /**
   * Сгенерировать частицу
   */
  static Particle generate() {
    Particle p;
    p.pos = Vector3D(0, 0, 3.0) + randomUnitVector() * 0.2;
    p.speed = (Vector3D(0, 0, 10) + randomUnitVector()).normalize() * (2.0 + 6.0 * random());
    p.life = 1;
    p.lifeDecreaseSpeed = 2.0 + 3.0 * random();
    p.update(0);
    return p;
  }
};


// -------------------------------------------------------------------------- //
// 
// -------------------------------------------------------------------------- //
GLuint texFire, texLeafs, texGranite, texMarble, texBumpMap, texNormalizationCubeMap, texYellow; // идентификаторы текстур

float t; // время
float dt; // время с предыдущего кадра
float lastLightChange; // время с предыдущего изменения положения источника света

float mousePhi, mousePsi; // углы поворота мыши

float camAlpha; // угол на окружности, по которой ездит камера

Model* torusModel, *torusModel2, *torusModel3, *torusModel4; // модельки торов

vector<Model*> staticModels; // список моделек, отбрасывающих тени

int windowWidth, windowHeight; // размеры окна

Vector3D cam; // положение камеры
Vector3D dir; // направление камеры
Vector3D light; // положение источника света

Vector3D lightcolor; // цвет источника света

GLuint listFloor, listBoxes, listAltarTop, listAltar; // дисплейные списки

bool motionL, motionR; // флаги движения

vector<Particle> fire; // список частиц

// -------------------------------------------------------------------------- //
// 
// -------------------------------------------------------------------------- //
/**
 * Сдвиг частиц на время dt
 */
void moveParticles(float dt) {
  for (unsigned int i = 0; i < fire.size(); i++)   {
    fire[i].update(dt);
    if(fire[i].life < 0)
      fire[i] = Particle::generate();
  }
}

void generateParticles() {
  for(int i = 0; i < 100; i++)
    fire.push_back(Particle::generate());
}

/**
 * Отрисовка частиц
 */
void drawParticles() {
  static float mvm[16];
  glGetFloatv(GL_MODELVIEW_MATRIX, mvm); // получаем матрицу модельно-видового преобразования
  Vector3D x = Vector3D(mvm[0], mvm[4], mvm[8]).normalize();
  Vector3D y = Vector3D(mvm[1], mvm[5], mvm[9]).normalize();

  glBindTexture(GL_TEXTURE_2D, texFire);
  glEnable(GL_BLEND);
  glDepthMask(GL_FALSE); // не отрисовывать в depth буфер
  glDisable(GL_LIGHTING); // не учитывать освещение
  glBegin(GL_QUADS);
  for(unsigned int i = 0; i < fire.size(); i++) {
    glColor4f(fire[i].color.x, fire[i].color.y, fire[i].color.z, fire[i].alpha);
    glTexCoord2f(0, 0);
    glVertex(fire[i].pos + ( x + y) * fire[i].size);
    glTexCoord2f(1, 0);
    glVertex(fire[i].pos + (-x + y) * fire[i].size);
    glTexCoord2f(1, 1);
    glVertex(fire[i].pos + (-x - y) * fire[i].size);
    glTexCoord2f(0, 1);
    glVertex(fire[i].pos + ( x - y) * fire[i].size);
  }
  glEnd();
  glEnable(GL_LIGHTING);
  glDepthMask(GL_TRUE);
  glDisable(GL_BLEND);
}


/**
 * Отрисовывает коробки и торы
 */
void drawObjects() {
  // Отрисовать коробки
  glCallList(listBoxes);

  // Рисуем два тора, на которых нет никаких эффектов
  glBindTexture(GL_TEXTURE_2D, texGranite);
  torusModel->draw();
  torusModel2->draw();

  // отрисовка тора с bump mapping'ом
  glActiveTextureARB(GL_TEXTURE0_ARB); // работаем с нулевой текстурой
  glBindTexture(GL_TEXTURE_2D, texBumpMap); 
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB); // режим наложения
  glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB, GL_TEXTURE); // использовать значение из текстуры
  glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB, GL_REPLACE); // и только его
  glActiveTextureARB(GL_TEXTURE1_ARB); // работаем с первой текстурой
  glEnable(GL_TEXTURE_CUBE_MAP_ARB); // включить cube map
  glBindTexture(GL_TEXTURE_CUBE_MAP_ARB, texNormalizationCubeMap);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB); // режим наложения
  glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB, GL_TEXTURE); // нулевой операнд - текстура
  glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB, GL_DOT3_RGB_ARB); // операция - скалярное произведение
  glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_ARB, GL_PREVIOUS_ARB); // первый операнд - текстура с предыдущего шага

  glActiveTextureARB(GL_TEXTURE0_ARB);
  torusModel3->drawBumped(light); // отрисовываем тор первый раз

  // возвращаем режим наложения как он был
  glActiveTextureARB(GL_TEXTURE1_ARB);
  glDisable(GL_TEXTURE_CUBE_MAP_ARB);
  glActiveTextureARB(GL_TEXTURE0_ARB);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

  glBlendFunc(GL_DST_COLOR, GL_ZERO); // отрисовка умножением
  glEnable(GL_BLEND); 
  glBindTexture(GL_TEXTURE_2D, texGranite); 
  torusModel3->draw(); // отрисовываем тор еще раз, на этот раз текстурированный
  glDisable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); // отрисовка стандратная - смешивание с учетом полупрозрачности
}

void drawKnot() {
  // отрисовываем загогулину полупрозрачную
  glEnable(GL_BLEND); 
  glDisable(GL_CULL_FACE); // отключаем обрезание граней
  glPushAttrib(GL_LIGHTING_BIT); // сохранить атрибуты освещения
  GLfloat m_ambient[4] = { 0.3f, 0.3f, 0.3f, 0.0f };
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, m_ambient); // меняем материал
  glBindTexture(GL_TEXTURE_2D, texYellow);
  torusModel4->draw(); // рисуем загогулину
  glPopAttrib(); // возвращаем все как было
  glEnable(GL_CULL_FACE);
  glDisable(GL_BLEND);
}

void drawAltar() {
  glCallList(listAltar);
}

void drawAltarTop() {
  glCallList(listAltarTop);
}

void drawFloor() {
  glCallList(listFloor);
}


/**
 * Функция отрисовки
 */
void display() {
  // Вычисляем dt, обновляем t
  float tt = t;
  t = timeGetTime() / 1000.0;
  dt = t - tt;

  // обновляем положение камеры
  if(motionL)
    camAlpha += dt;
  if(motionR)
    camAlpha -= dt;
  cam = Vector3D(sin(camAlpha) * 3, cos(camAlpha) * 3, 4);
  dir = Vector3D(sin(mousePhi) * cos(mousePsi), cos(mousePhi) * cos(mousePsi), sin(mousePsi));

  // сдвигаем частицы
  moveParticles(dt);

  // обновляем положение торов
  torusModel->setTransform(Vector3D(0.7 * sin(2 * t), 0.7 * cos(2 * t), 2.5), t / 3, t / 2);
  torusModel2->setTransform(Vector3D(1.5 * sin(2 * t), 1.5 * cos(7 * t / 3), 2.5),13 * t / 7,21 * t / 5);
  torusModel3->setTransform(Vector3D(1.0 * sin(3 * t), 1.0 * cos(2 * t / 5), 1.0),5 * t / 3,5 * t / 1.5);
  torusModel4->setTransform(Vector3D((0.7 + 0.3 * sin(3 * t)) * sin(3 * t), 0.6 * cos(3 * t), 2.0), 7 * t / 2.6, 8 * t / 1.9);

  // изменение положения и цвета источника света - раз в 0.05 секунды
  if(t - lastLightChange > 0.05) {
    light = Vector3D(0, 0, 3) + randomUnitVector() * random() * 0.07;
    float red = 0.7 + 0.3 * random();
    lightcolor = 1.3 * Vector3D(red, red * (1 - 0.1 * random()), 0.5f);
    lastLightChange = t;
  }
  GLfloat l_color[4] = { lightcolor.x, lightcolor.y, lightcolor.z, 1.0f };
  GLfloat nol_color[4] = {0.3 * lightcolor.x, 0.3 * lightcolor.y, 0.3 * lightcolor.z, 1.0f };
  GLfloat l_position[4] = { light.x, light.y, light.z, 1 };

  // очистить экран
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

  // установить параметры камеры 
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(90.0, 1.0 * windowWidth / windowHeight, 0.1f, -1.0f);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(cam.x, cam.y, cam.z, cam.x + dir.x, cam.y + dir.y, cam.z + dir.z, 0, 0, 1);


  //////////////////////////////////////////////////////////////////////////
  // отрисовка отражения

  // Включаем свет
  glLightfv(GL_LIGHT0, GL_DIFFUSE, l_color);
  glLightfv(GL_LIGHT0, GL_SPECULAR, l_color);

  glEnable(GL_STENCIL_TEST); // включить работу с буфером трафарета

  // Рисуем крышку алтаря в буфере трафарета
  glDepthMask(GL_FALSE); // не рисовать в буфег глубины
  glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE); // не рисовать в буфер цвета
  glStencilFunc(GL_NOTEQUAL, 1, 1); // рисовать только если в буфере трафарета не 1
  glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE); // заменять значение в буфере трафарета на 1
  drawAltarTop();
  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
  glDepthMask(GL_TRUE);

  // рисуем отражение
  glDisable(GL_CULL_FACE); // в отражении - другая система координат
  glPushMatrix();
  glScalef(1, 1, -1);
  glStencilFunc(GL_EQUAL, 1, 1);
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  glLightfv(GL_LIGHT0, GL_POSITION, l_position); // поставили источник света в отражении
  torusModel4->sort(cam * Vector3D(1, 1, -1)); // упорядочиваем грани для изогнутого тора
  drawObjects();
  drawParticles();
  drawKnot();
  glPopMatrix();
  glEnable(GL_CULL_FACE);
  glLightfv(GL_LIGHT0, GL_POSITION, l_position); // вернули источник света на место

  torusModel4->sort(cam); // упорядочиваем грани для изогнутого тора

  // Стираем крышку алтаря в буфере трафарета
  // При этом "фиксируем" буфер глубины на крышке алтаря
  glEnable(GL_POLYGON_OFFSET_FILL);
  glPolygonOffset(-4, -4);
  glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
  glStencilFunc(GL_NOTEQUAL, 0, 1);
  glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);
  drawAltarTop();
  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
  glDisable(GL_POLYGON_OFFSET_FILL);

  glDisable(GL_STENCIL_TEST);


  //////////////////////////////////////////////////////////////////////////
  // отрисовка в тени

  // устанавливаем интенсивность света на "выключено"
  glLightfv(GL_LIGHT0, GL_DIFFUSE, nol_color);
  glLightfv(GL_LIGHT0, GL_SPECULAR, nol_color);

  // отрисовываем пол и алтарь
  drawFloor();
  drawAltar();

  // отрисовываем крышку с полупрозрачностью
  glEnable(GL_POLYGON_OFFSET_FILL);
  glPolygonOffset(-4, -4);
  glEnable(GL_BLEND); // включить полупрозрачность
  drawAltarTop();
  glDisable(GL_BLEND);
  glDisable(GL_POLYGON_OFFSET_FILL);
  
  // отрисовать коробки и торы
  drawObjects();
  drawKnot();


  //////////////////////////////////////////////////////////////////////////
  // теневые объемы
  glEnable(GL_STENCIL_TEST);

  // подготовка теневых объемов
  for(unsigned int i = 0; i < staticModels.size(); i++)
    staticModels[i]->prepareShadowVolumes(light);
  glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
  glDepthMask(GL_FALSE);
  //glDisable(GL_LIGHTING);
  //glDisable(GL_TEXTURE_2D);
  //glColor3f(1, 1, 1);

  // задняя сторона
  glStencilFunc(GL_ALWAYS, 0, ~0);
  glStencilOp(GL_KEEP, GL_KEEP, GL_INCR);
  for(unsigned int i = 0; i < staticModels.size(); i++)
    staticModels[i]->drawShadowVolume<false>(light);

  // передняя сторона
  glStencilOp(GL_KEEP, GL_KEEP, GL_DECR);
  for(unsigned int i = 0; i < staticModels.size(); i++)
    staticModels[i]->drawShadowVolume<true>(light);

  glDepthMask(GL_TRUE);
  glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);


  //////////////////////////////////////////////////////////////////////////
  // отрисовка сцены со включенным светом

  // включаем свет
  glLightfv(GL_LIGHT0, GL_DIFFUSE, l_color);
  glLightfv(GL_LIGHT0, GL_SPECULAR, l_color);

  // рисуем освещенную сцену
  glStencilFunc(GL_EQUAL, 0, ~0);
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  glEnable(GL_STENCIL_TEST);
  drawFloor();
  drawAltar();
  glEnable(GL_POLYGON_OFFSET_FILL);
  glPolygonOffset(-4, -4);
  glEnable(GL_BLEND);
  drawAltarTop();
  glDisable(GL_BLEND);
  glDisable(GL_POLYGON_OFFSET_FILL);
  drawObjects();
  drawKnot();
  glDisable(GL_STENCIL_TEST);

  // рисуем частицы
  drawParticles();

  glFlush();
  glutSwapBuffers();
}


/**
 * Обработчик "ничегонеделания"
 */
void idle() {
  glutPostRedisplay();
}


/**
 * Обработчик нажатия кнопки на клавиатуре
 */
void keyboard(unsigned char key, int x, int y) {
  if(key=='\033')
    exit(0);

  if(key == 'a' || key == 'A')
    motionL = true;
  if(key == 'd' || key == 'D')
    motionR = true;
}


/**
 * Обработчик "отпускания" кнопки на клавиатуре
 */
void keyboardup(unsigned char key, int x, int y) {
  if(key == 'a' || key == 'A')
    motionL = false;
  if(key == 'd' || key == 'D')
    motionR = false;
}


/**
 * Обработчик движений мыши
 */
void motion(int x, int y) {
  const float sense = 0.01f;
  if(x != windowWidth / 2 || y != windowHeight / 2) {
    mousePhi += (x - windowWidth / 2) * sense;
    mousePsi -= (y - windowHeight / 2) * sense;
    if(mousePsi >  M_PI * 0.4)
      mousePsi =  M_PI * 0.4;
    if(mousePsi < -M_PI * 0.4)
      mousePsi = -M_PI * 0.4;
    glutWarpPointer(windowWidth / 2, windowHeight / 2);
  }
}


/**
 * Обработчик изменения размеров окна.
 */
void reshape(int w, int h) {
  windowWidth = w;
  windowHeight = h;
  glViewport(0, 0, w, h);
}


/**
 * Загрузка текстуры и добавление альфа-канала
 */
GLuint loadTextureAlpha(char* fileName, char* alphaName, GLuint mode) {
  GLuint result;
  glGenTextures(1, &result);

  AUX_RGBImageRec* imageRec = auxDIBImageLoad(fileName);
  AUX_RGBImageRec* alphaRec = auxDIBImageLoad(alphaName);
  unsigned char *newData = new unsigned char [imageRec->sizeX * imageRec->sizeY * 4];
  for(int i = 0; i < imageRec->sizeX * imageRec->sizeY; i++) {
    newData[4 * i]     = imageRec->data[3 * i];
    newData[4 * i + 1] = imageRec->data[3 * i + 1];
    newData[4 * i + 2] = imageRec->data[3 * i + 2];
    newData[4 * i + 3] = alphaRec->data[3 * i]; /* alpha */
  }

  glBindTexture(GL_TEXTURE_2D, result);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, mode);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, mode);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 4, imageRec->sizeX, imageRec->sizeY, GL_RGBA, GL_UNSIGNED_BYTE, newData);
  return result;
}

/**
 * Загрузка текстуры (без альфа-канала)
 */
GLuint loadTexture(char* fileName, GLuint mode) {
  GLuint result;
  glGenTextures(1, &result);

  AUX_RGBImageRec* imageRec = auxDIBImageLoad(fileName);
  glBindTexture(GL_TEXTURE_2D, result);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, mode);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, mode);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, imageRec->sizeX, imageRec->sizeY, GL_RGB, GL_UNSIGNED_BYTE, imageRec->data);
  return result;
}

/**
 * Генерирует нормализационную карту
 */
bool genNormalisationMap() {
  unsigned char * data = new unsigned char[32*32*3];

  int size = 32;
  float offset = 0.5f;
  float halfSize = 16.0f;
  Vector3D vec;
  unsigned char *ptr;

  /* positive x */
  ptr = data;

  for(int j = 0; j < size; j++) {
    for(int i = 0; i < size; i++) {
      vec.x = halfSize;
      vec.y = -(j+offset-halfSize);
      vec.z = -(i+offset-halfSize);

      vec.normalize();
      vec = vec * 0.5f + Vector3D(0.5f, 0.5f, 0.5f);

      ptr[0] = (unsigned char)(vec.x * 255);
      ptr[1] = (unsigned char)(vec.y * 255);
      ptr[2] = (unsigned char)(vec.z * 255);

      ptr += 3;
    }
  }

  glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB, 0, 3, 32, 32, 0, GL_RGB, GL_UNSIGNED_BYTE, data);

  /* negative x */
  ptr = data;

  for(int j = 0; j < size; j++) {
    for(int i = 0; i < size; i++) {
      vec.x = -halfSize;
      vec.y = -(j+offset-halfSize);
      vec.z = (i+offset-halfSize);

      vec.normalize();
      vec = vec * 0.5f + Vector3D(0.5f, 0.5f, 0.5f);

      ptr[0] = (unsigned char)(vec.x * 255);
      ptr[1] = (unsigned char)(vec.y * 255);
      ptr[2] = (unsigned char)(vec.z * 255);

      ptr += 3;
    }
  }
  glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB, 0, 3, 32, 32, 0, GL_RGB, GL_UNSIGNED_BYTE, data);

  /* positive y */
  ptr = data;

  for(int j = 0; j < size; j++) {
    for(int i = 0; i < size; i++) {
      vec.x = (i+offset-halfSize);
      vec.y = (halfSize);
      vec.z = ((j+offset-halfSize));

      vec.normalize();
      vec = vec * 0.5f + Vector3D(0.5f, 0.5f, 0.5f);

      ptr[0] = (unsigned char)(vec.x * 255);
      ptr[1] = (unsigned char)(vec.y * 255);
      ptr[2] = (unsigned char)(vec.z * 255);

      ptr += 3;
    }
  }
  glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB, 0, 3, 32, 32, 0, GL_RGB, GL_UNSIGNED_BYTE, data);

  /* negative y */
  ptr = data;

  for(int j = 0; j < size; j++) {
    for(int i = 0; i < size; i++) {
      vec.x = (i+offset-halfSize);
      vec.y = (-halfSize);
      vec.z = (-(j+offset-halfSize));

      vec.normalize();
      vec = vec * 0.5f + Vector3D(0.5f, 0.5f, 0.5f);

      ptr[0] = (unsigned char)(vec.x * 255);
      ptr[1] = (unsigned char)(vec.y * 255);
      ptr[2] = (unsigned char)(vec.z * 255);

      ptr += 3;
    }
  }
  glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB, 0, 3, 32, 32, 0, GL_RGB, GL_UNSIGNED_BYTE, data);

  /* positive z */
  ptr = data;

  for(int j = 0; j < size; j++) {
    for(int i = 0; i < size; i++) {
      vec.x = (i+offset-halfSize);
      vec.y = (-(j+offset-halfSize));
      vec.z = (halfSize);

      vec.normalize();
      vec = vec * 0.5f + Vector3D(0.5f, 0.5f, 0.5f);

      ptr[0] = (unsigned char)(vec.x * 255);
      ptr[1] = (unsigned char)(vec.y * 255);
      ptr[2] = (unsigned char)(vec.z * 255);

      ptr += 3;
    }
  }
  glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB, 0, 3, 32, 32, 0, GL_RGB, GL_UNSIGNED_BYTE, data);

  /* negative z */
  ptr = data;

  for(int j = 0; j < size; j++) {
    for(int i = 0; i < size; i++) {
      vec.x = (-(i+offset-halfSize));
      vec.y = (-(j+offset-halfSize));
      vec.z = (-halfSize);

      vec.normalize();
      vec = vec * 0.5f + Vector3D(0.5f, 0.5f, 0.5f);

      ptr[0] = (unsigned char)(vec.x * 255);
      ptr[1] = (unsigned char)(vec.y * 255);
      ptr[2] = (unsigned char)(vec.z * 255);

      ptr += 3;
    }
  }
  glTexImage2D(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB, 0, 3, 32, 32, 0, GL_RGB, GL_UNSIGNED_BYTE, data);

  delete[] data;

  return true;
}

/**
 * Инициализациия всего.
 */
void init() {
  // Загрузка текстур
  texFire = loadTextureAlpha("data/fire.bmp", "data/fire_alpha.bmp", GL_CLAMP);
  texLeafs = loadTexture("data/leafs.bmp", GL_REPEAT);
  texGranite = loadTexture("data/granite.bmp", GL_REPEAT);
  texMarble = loadTexture("data/marble.bmp", GL_REPEAT);
  texBumpMap = loadTexture("data/normal.bmp", GL_REPEAT);
  texYellow = loadTexture("data/yellow_marble.bmp", GL_REPEAT);

  // Создание нормализущей карты
  glGenTextures(1, &texNormalizationCubeMap);
  glBindTexture(GL_TEXTURE_CUBE_MAP_ARB, texNormalizationCubeMap);
  genNormalisationMap();
  glTexParameteri(GL_TEXTURE_CUBE_MAP_ARB, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_CUBE_MAP_ARB, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

  // изначальное значение времени
  t = timeGetTime() / 1000.0f;

  // генерируем большой тор
  torusModel = new Model(GTorus<GCircle, GCircle>(GCircle(0.25), GCircle(0.1)), 16, 16, 1, 1);
  staticModels.push_back(torusModel);
  torusModel->compile();

  // генерируем маленький тор
  torusModel2 = new Model(GTorus<GCircle, GCircle>(GCircle(0.15), GCircle(0.05)), 16, 16, 1, 1);
  staticModels.push_back(torusModel2);
  torusModel2->compile();

  // генерируем тор для bump-mapping'а
  torusModel3 = new Model(GTorus<GCircle, GCircle>(GCircle(0.50), GCircle(0.20)), 16, 16, 1, 0.5);
  staticModels.push_back(torusModel3);

  // генерируем загогулину
  torusModel4 = new Model(GTorus<GKnot, GCircle>(GKnot(0.26, 0.1), GCircle(0.09)), 64, 16, 1, 0.5);
  staticModels.push_back(torusModel4);

  // floor list
  listFloor = glGenLists(1);
  glNewList(listFloor, GL_COMPILE);
  glBindTexture(GL_TEXTURE_2D, texLeafs);
  drawQuad(Vector3D(-30, -30, -1), Vector3D(60, 0, 0), Vector3D(0, 60, 0), 20, 20, 30, 30);
  glEndList();

  // boxes list
  listBoxes = glGenLists(1);
  glNewList(listBoxes, GL_COMPILE);
  glBindTexture(GL_TEXTURE_2D, texGranite);
  for(int i = 0; i < 30; i++) {
    float phi = random() * 2 * M_PI;
    float r = 5 + random() * 10;
    Vector3D c = r * Vector3D(sin(phi), cos(phi), 0) + Vector3D(0, 0, -r / 6);
    Vector3D x = (Vector3D(0, 0, 1.5) + randomUnitVector()).normalize() * r * (0.5 * random() + 0.2);
    Vector3D y = ((x ^ (randomUnitVector() * Vector3D(1, 1, 0))).normalize() * 1.1 + randomUnitVector()).normalize() * r * (0.5 * random() + 0.2);
    if(y.z < 0)
      y = -y;
    Vector3D z = (x ^ y).normalize() * (0.3 + 0.5 * random());
    staticModels.push_back(new Model(c, x, y, z));

    drawBox(c, x, y, z, 10, 10, 1, x.length() / 2, y.length() / 2, z.length() / 2);
  }
  glEndList();

  // Altar list
  listAltar = glGenLists(1);
  glNewList(listAltar, GL_COMPILE);
  glDisable(GL_BLEND);
  glBindTexture(GL_TEXTURE_2D, texMarble);
  Vector3D c1 = Vector3D(0, 0, -1), x1 = Vector3D(-1.5, 1.0, 0), y1 = Vector3D(-1.5, -1.0, 0), z1 = Vector3D(0, 0, 1);
  drawBox(c1, x1, y1, z1, 10, 10, 2, 0.5, 0.5, 1);
  staticModels.push_back(new Model(c1, x1, y1, z1));
  Vector3D c2 = Vector3D(0, 0, -1), x2 = Vector3D(-1.0, -1.5, 0), y2 = Vector3D(1.0, -1.5, 0), z2 = Vector3D(0, 0, 1);
  drawBox(c2, x2, y2, z2, 10, 10, 2, 0.5, 0.5, 1);
  staticModels.push_back(new Model(c2, x2, y2, z2));
  Vector3D c3 = Vector3D(0, 0, -1), x3 = Vector3D(1.5, -1.0, 0), y3 = Vector3D(1.5, 1.0, 0), z3 = Vector3D(0, 0, 1);
  drawBox(c3, x3, y3, z3, 10, 10, 2, 0.5, 0.5, 1);
  staticModels.push_back(new Model(c3, x3, y3, z3));
  Vector3D c4 = Vector3D(0, 0, -1), x4 = Vector3D(1.0, 1.5, 0), y4 = Vector3D(-1.0, 1.5, 0), z4 = Vector3D(0, 0, 1);
  drawBox(c4, x4, y4, z4, 10, 10, 2, 0.5, 0.5, 1);
  staticModels.push_back(new Model(c4, x4, y4, z4));
  glEndList();

  // Altar top list
  listAltarTop = glGenLists(1);
  glNewList(listAltarTop, GL_COMPILE);
  glBindTexture(GL_TEXTURE_2D, texMarble);
  drawQuad(c1 + z1, x1, y1, 10, 10, 0.5, 0.5);
  drawQuad(c2 + z2, x2, y2, 10, 10, 0.5, 0.5);
  drawQuad(c3 + z3, x3, y3, 10, 10, 0.5, 0.5);
  drawQuad(c4 + z4, x4, y4, 10, 10, 0.5, 0.5);
  glEnd();
  glEndList();

  // Particles
  generateParticles();

  // Параметры OpenGL
  glEnable(GL_DEPTH_TEST); // включить тест глубины
  glEnable(GL_TEXTURE_2D); // включить текстурирование
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE); // стандартный режим наложения текстур
  glEnable(GL_NORMALIZE); // нормализовывать нормали
  glEnable(GL_CULL_FACE); // включить "отрезание" граней
  glCullFace(GL_BACK); // не рисовать задние грани
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); // стандартное смешинвание для полупрозрачных текстур
  glDepthFunc(GL_LEQUAL); // функция сравнения для теста глубины

  // установка материала
  GLfloat m_emissive[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
  GLfloat m_diffuse[4] = { 0.5f, 0.5f, 0.5f, 0.3f };
  GLfloat m_specular[4] = { 0.2f, 0.2f, 0.2f, 0.0f };
  GLfloat m_ambient[4] = { 0.03f, 0.03f, 0.03f, 0.0f };
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, m_emissive);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, m_specular);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, m_ambient);
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 5);

  // фоновая освещенность
  GLfloat l_ambient[4] = { 1.0f, 1.0f, 1.0f, 0.0f };
  glLightfv(GL_LIGHT0, GL_AMBIENT, l_ambient);

  // туман
  GLfloat fog_color[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
  glEnable(GL_FOG);
  glFogfv(GL_FOG_COLOR, fog_color);
  glFogf(GL_FOG_START, 10.0f);
  glFogf(GL_FOG_END, 20.0f);
  glFogi(GL_FOG_MODE, GL_LINEAR);

  glEnable(GL_LIGHTING); // включить рассчет освещения
  glEnable(GL_LIGHT0); // включить 0-й источник света
  glShadeModel(GL_SMOOTH); // модель затенения - smooth
}

int main(int argc, char** argv) {
  srand(timeGetTime());

  glutInit(&argc,argv);
  glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE | GLUT_STENCIL);
  glutInitWindowSize(800, 600);
  glutCreateWindow("");

  // инициализация glew
  glewInit();

  init();

  // устанавливаем обработчики glut.
  glutDisplayFunc(display);
  glutReshapeFunc(reshape);
  glutKeyboardFunc(keyboard);
  glutKeyboardUpFunc(keyboardup);
  glutIdleFunc(idle);
  glutPassiveMotionFunc(motion);

  // чтобы не показывал курсор поверх окна
  glutSetCursor(GLUT_CURSOR_NONE);

  // запустить цикл обработки сообщений
  glutMainLoop();

  return 0;
}