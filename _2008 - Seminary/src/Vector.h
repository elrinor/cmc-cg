#ifndef VECTOR_H
#define VECTOR_H

#include <cmath>

struct Vector {
public:
    float x, y, z;
    Vector(float x, float y, float z): x(x), y(y), z(z) 
    {
        return;
    }
    Vector()
    {
        return;
    }
};

Vector operator* (const Vector& vec1, const float c)
{
    Vector v;
    v.x = vec1.x * c;
    v.y = vec1.y * c;
    v.z = vec1.z * c;
    return v;
}

Vector operator* (const float c, const Vector& vec) 
{
    return vec * c;
}

Vector operator+ (const Vector& vec1, const Vector& vec2)
{
    Vector v;
    v.x = vec1.x + vec2.x;
    v.y = vec1.y + vec2.y;
    v.z = vec1.z + vec2.z;
    return v;
}

Vector operator- (const Vector& vec1, const Vector& vec2)
{
    Vector v;
    v.x = vec1.x - vec2.x;
    v.y = vec1.y - vec2.y;
    v.z = vec1.z - vec2.z;
    return v;
}

Vector operator^ (const Vector& vec1, const Vector& vec2)
{
    Vector v;
    v.x =   vec1.y * vec2.z - vec1.z * vec2.y;
    v.y = - vec1.x * vec2.z + vec1.z * vec2.x;
    v.z =   vec1.x * vec2.y - vec1.y * vec2.x;
    return v;
}

Vector normalize(const Vector& vec) 
{
    float b = 1 / sqrt(vec.x * vec.x + vec.y * vec.y + vec.z * vec.z);
    return vec * b;
}


#endif