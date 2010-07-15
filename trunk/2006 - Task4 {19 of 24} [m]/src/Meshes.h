#ifndef __MESHES
#define __MESHES
#include "Vector.h"

#define STACKS 100
#define SLICES 20
#define QUADS_MAX (SLICES*STACKS)
#define PI 3.1415926

typedef struct{
  CVector ver[4],nor[4],tex[4];
} quad_t;

struct Mesh{
  int Count; // кол-во четырехугольников
  float Matrix[16]; // матрица поворота
  float BackMatrix[16]; // и обратная к ней
  int Order[QUADS_MAX]; // порядок, в котором выводить грани
  float Distance[QUADS_MAX]; // растояния до граней от камеры
  quad_t p[QUADS_MAX]; // грани
  void CreateAsMebius(); // инициализация лентой мебиуса
  void CreateAsKlein();  // ...
  void CreateAsKlein2(); // ...
  void Draw(); // отрисовка
  void Rotate(float Xang, float Yang, float Zang); // повернуть объект
  void Sort(CVector Camera); // отсортировать грани по расстоянию до камеры
};

#endif