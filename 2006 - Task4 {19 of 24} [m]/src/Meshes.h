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
  int Count; // ���-�� �����������������
  float Matrix[16]; // ������� ��������
  float BackMatrix[16]; // � �������� � ���
  int Order[QUADS_MAX]; // �������, � ������� �������� �����
  float Distance[QUADS_MAX]; // ��������� �� ������ �� ������
  quad_t p[QUADS_MAX]; // �����
  void CreateAsMebius(); // ������������� ������ �������
  void CreateAsKlein();  // ...
  void CreateAsKlein2(); // ...
  void Draw(); // ���������
  void Rotate(float Xang, float Yang, float Zang); // ��������� ������
  void Sort(CVector Camera); // ������������� ����� �� ���������� �� ������
};

#endif