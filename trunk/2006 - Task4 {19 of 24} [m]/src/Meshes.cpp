#include <gl\glut.h>
#include <math.h>
#include <stdlib.h>
#include "Meshes.h"

inline float sqr(float a) {return a*a;}

Mesh* CurrentMesh;
int mycmp(const void* p1, const void* p2)
{  
  if(CurrentMesh->Distance[*((int*)p1)]<CurrentMesh->Distance[*((int*)p2)]) return 1;
  else                                                                      return -1;
}

// ф-ии, выдающие по u и v координаты точки на ленте мебиуса и нормаль
CVector VertexM(float u, float v)
{
  return CVector( (1+v/2*cos(u/2))*cos(u),
                  (1+v/2*cos(u/2))*sin(u),
                  v/2*sin(u/2) )*0.8;
} 

CVector NormalM(float u, float v)
{
  return ((VertexM(u+0.01,v)-VertexM(u-0.01,v)).Normalize())|((VertexM(u,v+0.01)-VertexM(u,v-0.01)).Normalize());
}

// инициализаци€ лентой мебиуса
void Mesh::CreateAsMebius()
{
  Count=0;
  for(int i=0; i<QUADS_MAX; i++)
    Order[i]=i;
  float m[16]={1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1};
  for(int i=0; i<16; i++)
    Matrix[i]=BackMatrix[i]=m[i];                // матрицы - в единичные
  Count=0;
  for(int i=0; i<STACKS; i++) for(int j=0; j<SLICES; j++)
  {
    // задаем нормали / координаты / текстурные координаты точек полигона
    float u00=2*PI*(i  )/STACKS;
    float v00=2.0*(j  )/SLICES-1;
    float u10=2*PI*(i+1)/STACKS;
    float v10=2.0*(j  )/SLICES-1;
    float u11=2*PI*(i+1)/STACKS;
    float v11=2.0*(j+1)/SLICES-1;
    float u01=2*PI*(i  )/STACKS;
    float v01=2.0*(j+1)/SLICES-1;
    p[Count].ver[0]=VertexM(u00,v00);
    p[Count].nor[0]=NormalM(u00,v00);
    p[Count].tex[0]=CVector(1.0*(i  )/STACKS, 1.0*(j  )/SLICES, 0);
    p[Count].ver[1]=VertexM(u10,v10);
    p[Count].nor[1]=NormalM(u10,v10);
    p[Count].tex[1]=CVector(1.0*(i+1)/STACKS, 1.0*(j  )/SLICES, 0);
    p[Count].ver[2]=VertexM(u11,v11);
    p[Count].nor[2]=NormalM(u11,v11);
    p[Count].tex[2]=CVector(1.0*(i+1)/STACKS, 1.0*(j+1)/SLICES, 0);
    p[Count].ver[3]=VertexM(u01,v01);
    p[Count].nor[3]=NormalM(u01,v01);
    p[Count].tex[3]=CVector(1.0*(i  )/STACKS, 1.0*(j+1)/SLICES, 0);
    Count++;
  }
}

CVector VertexK(float u, float v)
{
  if(u>PI) return CVector( 6*cos(u)*(1+sin(u))-4*(1-cos(u)/2)*cos(v), 
                           -16*sin(u), 
                           4*(1-cos(u)/2)*sin(v) )*0.05;
  else     return CVector( 6*cos(u)*(1+sin(u))+4*(1-cos(u)/2)*cos(u)*cos(v),
                           -16*sin(u)-4*(1-cos(u)/4)*sin(u)*cos(v),
                           4*(1-cos(u)/2)*sin(v) )*0.05;
} 

CVector NormalK(float u, float v)
{
  return ((VertexK(u+0.01,v)-VertexK(u-0.01,v)).Normalize())|((VertexK(u,v+0.01)-VertexK(u,v-0.01)).Normalize());
}

void Mesh::CreateAsKlein()
{
  Count=0;
  for(int i=0; i<QUADS_MAX; i++)
    Order[i]=i;
  float m[16]={1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1};
  for(int i=0; i<16; i++)
    Matrix[i]=BackMatrix[i]=m[i];
  Count=0;
  for(int i=0; i<STACKS; i++) for(int j=0; j<SLICES; j++)
  {
    float u00=2*PI*(i  )/STACKS;
    float v00=2*PI*(j  )/SLICES;
    float u10=2*PI*(i+1)/STACKS;
    float v10=2*PI*(j  )/SLICES;
    float u11=2*PI*(i+1)/STACKS;
    float v11=2*PI*(j+1)/SLICES;
    float u01=2*PI*(i  )/STACKS;
    float v01=2*PI*(j+1)/SLICES;
    p[Count].ver[0]=VertexK(u00,v00);
    p[Count].nor[0]=NormalK(u00,v00);
    p[Count].tex[0]=CVector(1.0*(i  )/STACKS, 1.0*(j  )/SLICES, 0);
    p[Count].ver[1]=VertexK(u10,v10);
    p[Count].nor[1]=NormalK(u10,v10);
    p[Count].tex[1]=CVector(1.0*(i+1)/STACKS, 1.0*(j  )/SLICES, 0);
    p[Count].ver[2]=VertexK(u11,v11);
    p[Count].nor[2]=NormalK(u11,v11);
    p[Count].tex[2]=CVector(1.0*(i+1)/STACKS, 1.0*(j+1)/SLICES, 0);
    p[Count].ver[3]=VertexK(u01,v01);
    p[Count].nor[3]=NormalK(u01,v01);
    p[Count].tex[3]=CVector(1.0*(i  )/STACKS, 1.0*(j+1)/SLICES, 0);
    Count++;
  }
}

CVector VertexK2(float u, float v)
{
   return CVector( (3+cos(u/2)*sin(v)-sin(u/2)*sin(2*v))*cos(u),
                   (3+cos(u/2)*sin(v)-sin(u/2)*sin(2*v))*sin(u),
                   sin(u/2)*sin(v)+cos(u/2)*sin(2*v) )*0.25;
} 

CVector NormalK2(float u, float v)
{
  return ((VertexK2(u+0.01,v)-VertexK2(u-0.01,v)).Normalize())|((VertexK2(u,v+0.01)-VertexK2(u,v-0.01)).Normalize());
}

void Mesh::CreateAsKlein2()
{
  Count=0;
  for(int i=0; i<QUADS_MAX; i++)
    Order[i]=i;
  float m[16]={1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1};
  for(int i=0; i<16; i++)
    Matrix[i]=BackMatrix[i]=m[i];
  Count=0;
  for(int i=0; i<STACKS; i++) for(int j=0; j<SLICES; j++)
  {
    float u00=2*PI*(i  )/STACKS;
    float v00=2*PI*(j  )/SLICES;
    float u10=2*PI*(i+1)/STACKS;
    float v10=2*PI*(j  )/SLICES;
    float u11=2*PI*(i+1)/STACKS;
    float v11=2*PI*(j+1)/SLICES;
    float u01=2*PI*(i  )/STACKS;
    float v01=2*PI*(j+1)/SLICES;
    p[Count].ver[0]=VertexK2(u00,v00);
    p[Count].nor[0]=NormalK2(u00,v00);
    p[Count].tex[0]=CVector(1.0*(i  )/STACKS, 1.0*(j  )/SLICES, 0);
    p[Count].ver[1]=VertexK2(u10,v10);
    p[Count].nor[1]=NormalK2(u10,v10);
    p[Count].tex[1]=CVector(1.0*(i+1)/STACKS, 1.0*(j  )/SLICES, 0);
    p[Count].ver[2]=VertexK2(u11,v11);
    p[Count].nor[2]=NormalK2(u11,v11);
    p[Count].tex[2]=CVector(1.0*(i+1)/STACKS, 1.0*(j+1)/SLICES, 0);
    p[Count].ver[3]=VertexK2(u01,v01);
    p[Count].nor[3]=NormalK2(u01,v01);
    p[Count].tex[3]=CVector(1.0*(i  )/STACKS, 1.0*(j+1)/SLICES, 0);
    Count++;
  }
}

// повернуть объект, при этом измен€ютс€ только матрицы поворота
void Mesh::Rotate(float Xang, float Yang, float Zang)
{
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();
  glRotatef(Xang,1,0,0);
  glRotatef(Yang,0,1,0);
  glRotatef(Zang,0,0,1);
  glMultMatrixf(Matrix);
  glGetFloatv(GL_MODELVIEW_MATRIX,Matrix);
  glPopMatrix();

  glPushMatrix();
  glLoadIdentity();
  glMultMatrixf(BackMatrix);
  glRotatef(-Zang,0,0,1);
  glRotatef(-Yang,0,1,0);
  glRotatef(-Xang,1,0,0);
  glGetFloatv(GL_MODELVIEW_MATRIX,BackMatrix);
  glPopMatrix();
}

// отрисовать объект
void Mesh::Draw()
{
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glMultMatrixf(Matrix);
  glBegin(GL_QUADS);
  for(int j=0;j<Count;j++)
  {
    int i=Order[j];
    glNormal3fv(p[i].nor[0].v);glTexCoord3fv(p[i].tex[0].v);glVertex3fv(p[i].ver[0].v);
    glNormal3fv(p[i].nor[1].v);glTexCoord3fv(p[i].tex[1].v);glVertex3fv(p[i].ver[1].v);
    glNormal3fv(p[i].nor[2].v);glTexCoord3fv(p[i].tex[2].v);glVertex3fv(p[i].ver[2].v);
    glNormal3fv(p[i].nor[3].v);glTexCoord3fv(p[i].tex[3].v);glVertex3fv(p[i].ver[3].v);
  }
  glEnd();
  glPopMatrix();
}

// отсортировать грани объекта по рассто€нию до камеры
void Mesh::Sort(CVector Camera)
{
  CVector Camera2;
  // вместо вращени€ всего объекта крутим камеру в противоположном направлении
  Camera2.x=Camera.x*BackMatrix[0]+Camera.y*BackMatrix[4]+Camera.z*BackMatrix[8];
  Camera2.y=Camera.x*BackMatrix[1]+Camera.y*BackMatrix[5]+Camera.z*BackMatrix[9];
  Camera2.z=Camera.x*BackMatrix[2]+Camera.y*BackMatrix[6]+Camera.z*BackMatrix[10];
  // считаем рассто€ни€
  for(int i=0; i<Count; i++)
    Distance[i]=(Camera2-p[i].ver[0]).Abs2();
  CurrentMesh=this;
  // сортируем
  qsort(Order,Count,sizeof(int),mycmp);
}
