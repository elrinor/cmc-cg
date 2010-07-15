#ifndef __VECTOR
#define __VECTOR

#pragma once
#include <math.h>

class CVector
{
public:
	union{
		float v[3];
		struct{
			float x, y, z;
		};
	};

	CVector();
	CVector(CVector &v);
	CVector(float x, float y, float z);

	CVector& operator=(CVector &v);
	CVector operator*(float s);
	CVector& operator*=(float s);
	float operator*(CVector &v);
	CVector operator|(CVector &v);
  CVector operator+(CVector &v);
	CVector& operator+=(CVector &v);
	CVector operator-(CVector &v);
	CVector operator-();
	CVector& operator-=(CVector &v);
  bool operator==(CVector &v);
	CVector& Normalize(float n=1.0f);
	void Set(float x, float y, float z);
  float Abs2();
};

#endif