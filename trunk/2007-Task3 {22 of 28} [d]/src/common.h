#ifndef __COMMON_H__
#define __COMMON_H__
#include <cmath>

template<class T>
class Matrix 
{
private:
	T** elem;
	int x;
	int y;

	Matrix& operator= (const Matrix&);
	Matrix(const Matrix&);

public:
	T* operator[] (int index) 
	{
		return elem[index];
	}

	int getX() 
	{
		return this->x;
	}

	int getY() 
	{
		return this->y;
	}

	Matrix(int x, int y) 
	{
		this->x = x;
		this->y = y;
		elem = new T*[x];
		for (int i = 0; i < x; i++)
			elem[i] = new T[y];
	}

	~Matrix() 
	{
		for (int i = 0; i < x; i++)
			delete[] elem[i];
		delete[] elem;
	}

	void fill(const T& value) 
	{
		for (int i = 0; i < this->x; i++)
			for (int j = 0; j < this->y; j++)
				elem[i][j] = value;
	}
};

class Point2d 
{
public:
	int x;
	int y;
	Point2d(int x, int y): x(x), y(y) {}
	Point2d() {}
};

class Vector3d 
{
public:
	float x;
	float y;
	float z;
	Vector3d(float x, float y, float z): x(x), y(y), z(z) {}
	Vector3d() {}

	Vector3d operator- (const Vector3d& v) const 
	{
		Vector3d result;
		result.x = this->x - v.x;
		result.y = this->y - v.y;
		result.z = this->z - v.z;
		return result;
	}

	Vector3d operator+ (const Vector3d& v) const 
	{
		Vector3d result;
		result.x = this->x + v.x;
		result.y = this->y + v.y;
		result.z = this->z + v.z;
		return result;
	}

	float abs2() const 
	{
		return x*x + y*y + z*z;
	}

	Vector3d operator* (const float a) const 
	{
		Vector3d result;
		result.x = this->x * a;
		result.y = this->y * a;
		result.z = this->z * a;
		return result;
	}

	float operator* (const Vector3d& v) const 
	{
		return x * v.x + y * v.y + z * v.z;
	}

	Vector3d normalize() const 
	{
		return *this * (1.0f / sqrt(abs2()));
	}


};

#endif