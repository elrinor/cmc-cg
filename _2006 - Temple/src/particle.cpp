#include <math.h>
#include <gl/glut.h>
#include <stdlib.h>
#include "libTexture.h"
#include <stdio.h>

#define N 100

typedef struct {
	float x,y,z;
	float dx,dy,dz;
	float r,g,b,a;
	float life,dlife;
	float size;
} particle_t;

GLuint part_texture;

particle_t particle[N];

void updateParticleSystem(float dt)
{
	for(int i=0; i<N; i++)
	{
		if(particle[i].life<0)
		{
			particle[i].x=0.5-1.0*rand()/RAND_MAX;
			particle[i].y=0.5-1.0*rand()/RAND_MAX;
			particle[i].z=1.5;
			particle[i].dx=0.1-0.2*rand()/RAND_MAX;
			particle[i].dy=0.1-0.2*rand()/RAND_MAX;
			particle[i].dz=0.3+0.3*rand()/RAND_MAX;
			particle[i].size=0.05+0.05*rand()/RAND_MAX;
			particle[i].life=1;
			particle[i].dlife=-0.1-0.2*rand()/RAND_MAX;
			particle[i].r=/*0.5*rand()/RAND_MAX*/0.8+0.2*rand()/RAND_MAX;
			particle[i].g=/*0.5*rand()/RAND_MAX*/0.8+0.2*rand()/RAND_MAX;
			particle[i].b=/*0.5+0.5*rand()/RAND_MAX*/0.8+0.2*rand()/RAND_MAX;
			particle[i].a=0;
		}
		else
		{
			particle[i].x+=particle[i].dx*dt;
			particle[i].y+=particle[i].dy*dt;
			particle[i].z+=particle[i].dz*dt;
			particle[i].life+=particle[i].dlife*dt;
			particle[i].a=1-2*abs(particle[i].life-0.5);
		}
	}
}

void initParticleSystem()
{
	for(int i=0; i<N; i++)
		particle[i].life=-1;
	part_texture=createTexture2D(true, "Data/particle.bmp");
	for(int i=0; i<20; i++)
		updateParticleSystem(1);
}

void drawParticleSystem()
{
	float viewMatrix[16];
	glGetFloatv(GL_MODELVIEW_MATRIX, viewMatrix);
	
	// финтим - из матрицы ModelView извлекаем векторы направленные вверх и вправо
	float rx=viewMatrix[0], ry=viewMatrix[4], rz=viewMatrix[8];
	float ux=viewMatrix[1], uy=viewMatrix[5], uz=viewMatrix[9];

	printf("%.2g %.2g %.2g %.2g %.2g %.2g %.2g %.2g %.2g %.2g %.2g %.2g\n",viewMatrix[0],viewMatrix[1],viewMatrix[2],
																																				 viewMatrix[4],viewMatrix[5],viewMatrix[6],
																																				 viewMatrix[8],viewMatrix[9],viewMatrix[10],
																																				 viewMatrix[12],viewMatrix[13],viewMatrix[14]);

	glDepthMask(GL_FALSE);
	glDisable(GL_LIGHTING);
	glEnable(GL_BLEND);
	glEnable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, part_texture);

	glBegin(GL_QUADS);
	for(int i=0; i<N; i++)
	{
		float size=particle[i].size/2;
		float x=particle[i].x, y=particle[i].y, z=particle[i].z;
		glColor4f(particle[i].r,particle[i].g,particle[i].b,particle[i].a);		
		glTexCoord2f(0.0, 0.0); glVertex3f(x-(rx+ux)*size, y-(ry+uy)*size, z-(rz+uz)*size);
		glTexCoord2f(1.0, 0.0); glVertex3f(x+(rx-ux)*size, y+(ry-uy)*size, z+(rz-uz)*size);
		glTexCoord2f(1.0, 1.0); glVertex3f(x+(rx+ux)*size, y+(ry+uy)*size, z+(rz+uz)*size);
		glTexCoord2f(0.0, 1.0); glVertex3f(x+(-rx+ux)*size,y+(-ry+uy)*size,z+(-rz+uz)*size);
	}
	glEnd();
	
	glDisable(GL_BLEND);
	glDepthMask(GL_TRUE);
}
	