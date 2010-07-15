#include <gl/glut.h>
#include <stdlib.h>
#include <math.h>

GLuint Texture;
GLuint DList;

#define SIZE 256
#define START_AMPLITUDE 64
#define START_STEP 32

float heights[SIZE][SIZE];
char texture[SIZE][SIZE][3];

GLuint land_texture;

// цвет в зависимости от высоты
const char ColorMap[256][3] = {{(char)255,(char)245,(char)25},{(char)255,(char)243,(char)3},{(char)255,(char)243,(char)3},{(char)255,(char)243,(char)3},{(char)255,(char)243,(char)3},{(char)255,(char)219,(char)23},{(char)255,(char)219,(char)23},{(char)255,(char)219,(char)23},{(char)131,(char)231,(char)103},{(char)131,(char)231,(char)103},{(char)131,(char)231,(char)103},{(char)131,(char)231,(char)103},{(char)131,(char)231,(char)103},{(char)131,(char)231,(char)103},{(char)131,(char)231,(char)103},{(char)131,(char)231,(char)103},{(char)131,(char)231,(char)103},{(char)131,(char)231,(char)103},{(char)131,(char)231,(char)103},{(char)131,(char)231,(char)103},{(char)131,(char)231,(char)103},{(char)131,(char)231,(char)103},{(char)131,(char)231,(char)103},{(char)131,(char)231,(char)103},{(char)131,(char)231,(char)103},{(char)131,(char)231,(char)103},{(char)131,(char)231,(char)103},{(char)115,(char)215,(char)87},{(char)115,(char)215,(char)87},{(char)115,(char)215,(char)87},
	{(char)115,(char)215,(char)87},{(char)115,(char)215,(char)87},{(char)115,(char)215,(char)87},{(char)115,(char)215,(char)87},{(char)115,(char)215,(char)87},{(char)115,(char)215,(char)87},{(char)115,(char)215,(char)87},{(char)115,(char)215,(char)87},{(char)115,(char)215,(char)87},{(char)115,(char)215,(char)87},{(char)115,(char)215,(char)87},{(char)115,(char)215,(char)87},{(char)103,(char)203,(char)71},{(char)103,(char)203,(char)71},{(char)103,(char)203,(char)71},{(char)103,(char)203,(char)71},{(char)103,(char)203,(char)71},{(char)103,(char)203,(char)71},{(char)103,(char)203,(char)71},{(char)103,(char)203,(char)71},{(char)103,(char)203,(char)71},{(char)103,(char)203,(char)71},{(char)95,(char)191,(char)59},{(char)95,(char)191,(char)59},{(char)95,(char)191,(char)59},{(char)95,(char)191,(char)59},{(char)95,(char)191,(char)59},{(char)95,(char)191,(char)59},{(char)95,(char)191,(char)59},{(char)95,(char)191,(char)59},{(char)95,(char)191,(char)59},{(char)95,(char)191,(char)59},
	{(char)95,(char)191,(char)59},{(char)83,(char)179,(char)47},{(char)83,(char)179,(char)47},{(char)83,(char)179,(char)47},{(char)83,(char)179,(char)47},{(char)83,(char)179,(char)47},{(char)83,(char)179,(char)47},{(char)83,(char)179,(char)47},{(char)83,(char)179,(char)47},{(char)83,(char)179,(char)47},{(char)83,(char)179,(char)47},{(char)83,(char)179,(char)47},{(char)83,(char)179,(char)47},{(char)71,(char)167,(char)35},{(char)71,(char)167,(char)35},{(char)71,(char)167,(char)35},{(char)71,(char)167,(char)35},{(char)71,(char)167,(char)35},{(char)63,(char)151,(char)27},{(char)63,(char)151,(char)27},{(char)63,(char)151,(char)27},{(char)63,(char)151,(char)27},{(char)63,(char)151,(char)27},{(char)63,(char)151,(char)27},{(char)63,(char)151,(char)27},{(char)63,(char)151,(char)27},{(char)63,(char)151,(char)27},{(char)63,(char)151,(char)27},{(char)63,(char)151,(char)27},{(char)63,(char)151,(char)27},{(char)63,(char)151,(char)27},{(char)63,(char)151,(char)27},{(char)55,(char)139,(char)15},
	{(char)55,(char)139,(char)15},{(char)55,(char)139,(char)15},{(char)55,(char)139,(char)15},{(char)55,(char)139,(char)15},{(char)55,(char)139,(char)15},{(char)47,(char)127,(char)11},{(char)47,(char)127,(char)11},{(char)47,(char)127,(char)11},{(char)47,(char)127,(char)11},{(char)47,(char)127,(char)11},{(char)47,(char)127,(char)11},{(char)35,(char)103,(char)3},{(char)35,(char)103,(char)3},{(char)51,(char)87,(char)7},{(char)51,(char)87,(char)7},{(char)51,(char)87,(char)7},{(char)51,(char)87,(char)7},{(char)51,(char)87,(char)7},{(char)51,(char)87,(char)7},{(char)51,(char)87,(char)7},{(char)95,(char)67,(char)15},{(char)95,(char)67,(char)15},{(char)95,(char)67,(char)15},{(char)95,(char)67,(char)15},{(char)111,(char)79,(char)23},{(char)111,(char)79,(char)23},{(char)111,(char)79,(char)23},{(char)111,(char)79,(char)23},{(char)111,(char)79,(char)23},{(char)111,(char)79,(char)23},{(char)111,(char)79,(char)23},{(char)123,(char)91,(char)31},{(char)123,(char)91,(char)31},
	{(char)123,(char)91,(char)31},{(char)123,(char)91,(char)31},{(char)123,(char)91,(char)31},{(char)123,(char)91,(char)31},{(char)139,(char)103,(char)43},{(char)139,(char)103,(char)43},{(char)139,(char)103,(char)43},{(char)139,(char)103,(char)43},{(char)139,(char)103,(char)43},{(char)139,(char)103,(char)43},{(char)151,(char)119,(char)55},{(char)151,(char)119,(char)55},{(char)151,(char)119,(char)55},{(char)151,(char)119,(char)55},{(char)151,(char)119,(char)55},{(char)151,(char)119,(char)55},{(char)151,(char)119,(char)55},{(char)159,(char)123,(char)59},{(char)167,(char)135,(char)71},{(char)167,(char)135,(char)71},{(char)167,(char)135,(char)71},{(char)167,(char)135,(char)71},{(char)167,(char)135,(char)71},{(char)167,(char)135,(char)71},{(char)167,(char)135,(char)71},{(char)167,(char)135,(char)71},{(char)183,(char)147,(char)83},{(char)183,(char)147,(char)83},{(char)183,(char)147,(char)83},{(char)183,(char)147,(char)83},{(char)183,(char)147,(char)83},{(char)187,(char)151,(char)87},
	{(char)187,(char)151,(char)87},{(char)195,(char)163,(char)99},{(char)195,(char)163,(char)99},{(char)195,(char)163,(char)99},{(char)195,(char)163,(char)99},{(char)195,(char)163,(char)99},{(char)195,(char)163,(char)99},{(char)195,(char)163,(char)99},{(char)203,(char)167,(char)103},{(char)211,(char)179,(char)119},{(char)211,(char)179,(char)119},{(char)211,(char)179,(char)119},{(char)211,(char)179,(char)119},{(char)211,(char)179,(char)119},{(char)211,(char)179,(char)119},{(char)211,(char)179,(char)119},{(char)211,(char)179,(char)119},{(char)211,(char)179,(char)119},{(char)223,(char)195,(char)135},{(char)223,(char)195,(char)135},{(char)223,(char)195,(char)135},{(char)223,(char)195,(char)135},{(char)223,(char)195,(char)135},{(char)223,(char)195,(char)135},{(char)223,(char)195,(char)135},{(char)223,(char)195,(char)135},{(char)239,(char)211,(char)155},{(char)239,(char)211,(char)155},{(char)239,(char)211,(char)155},{(char)239,(char)211,(char)155},{(char)239,(char)211,(char)155},
	{(char)239,(char)211,(char)155},{(char)239,(char)211,(char)155},{(char)239,(char)211,(char)155},{(char)239,(char)211,(char)155},{(char)239,(char)211,(char)155},{(char)255,(char)231,(char)179},{(char)255,(char)231,(char)179},{(char)255,(char)231,(char)179},{(char)171,(char)171,(char)171},{(char)171,(char)171,(char)171},{(char)171,(char)171,(char)171},{(char)175,(char)175,(char)175},{(char)175,(char)175,(char)175},{(char)175,(char)175,(char)175},{(char)183,(char)183,(char)183},{(char)183,(char)183,(char)183},{(char)183,(char)183,(char)183},{(char)183,(char)183,(char)183},{(char)183,(char)183,(char)183},{(char)191,(char)191,(char)191},{(char)191,(char)191,(char)191},{(char)191,(char)191,(char)191},{(char)191,(char)191,(char)191},{(char)191,(char)191,(char)191},{(char)199,(char)199,(char)199},{(char)199,(char)199,(char)199},{(char)199,(char)199,(char)199},{(char)199,(char)199,(char)199},{(char)199,(char)199,(char)199},{(char)199,(char)199,(char)199},{(char)207,(char)207,(char)207},
	{(char)207,(char)207,(char)207},{(char)207,(char)207,(char)207},{(char)207,(char)207,(char)207},{(char)207,(char)207,(char)207},{(char)207,(char)207,(char)207},{(char)211,(char)211,(char)211},{(char)211,(char)211,(char)211},{(char)219,(char)219,(char)219},{(char)219,(char)219,(char)219},{(char)219,(char)219,(char)219},{(char)219,(char)219,(char)219},{(char)219,(char)219,(char)219},{(char)219,(char)219,(char)219},{(char)227,(char)227,(char)227},{(char)227,(char)227,(char)227},{(char)227,(char)227,(char)227},{(char)227,(char)227,(char)227},{(char)227,(char)227,(char)227},{(char)235,(char)235,(char)235},{(char)235,(char)235,(char)235},{(char)235,(char)235,(char)235},{(char)235,(char)235,(char)235},{(char)235,(char)235,(char)235},{(char)235,(char)235,(char)235},{(char)255,(char)255,(char)255},{(char)255,(char)255,(char)255},{(char)255,(char)255,(char)255},{(char)255,(char)255,(char)255},{(char)255,(char)255,(char)255},{(char)255,(char)255,(char)255},{(char)255,(char)255,(char)255},
	{(char)255,(char)255,(char)255},{(char)255,(char)255,(char)255},{(char)255,(char)255,(char)255}};

// создать один слой шума
float* createNoiseLayer(int step, float ampl)
{
	float* a=new float[SIZE*SIZE];
	for(int y=0; y<SIZE; y++)
		for(int x=0; x<SIZE; x++)
			a[y*SIZE+x]=0;
	
	for(int y=0; y<SIZE; y+=step)
		for(int x=0; x<SIZE; x+=step)
			if(x!=0 && y!=0)
				a[y*SIZE+x]=ampl*(1.0-2.0*rand()/RAND_MAX);
			else
				a[y*SIZE+x]=-ampl;
	
	for(int y=0; y<SIZE; y+=step)
		for(int x=0; x<SIZE; x++)
			a[y*SIZE+x]=(a[y*SIZE+x-x%step]*(step-x%step)+a[y*SIZE+(x-x%step+step)%SIZE]*(x%step))/step;
	for(int y=0; y<SIZE; y++) if(y%step!=0)
		for(int x=0; x<SIZE; x++)
			a[y*SIZE+x]=(a[(y-y%step)*SIZE+x]*(step-y%step)+a[(y-y%step+step)%SIZE*SIZE+x]*(y%step))/step;

	return a;
}

// Создать Perlin-Noise ландшафт - создаем карту высот и текстуру
void createPerlinLandscape()
{
	for(int x=0; x<SIZE; x++)
		for(int y=0; y<SIZE; y++)
			heights[x][y]=0;
	for(int step=64; step>1; step/=2)
	{
		float* a=createNoiseLayer(step,step);
		for(int x=0; x<SIZE; x++)
			for(int y=0; y<SIZE; y++)
				heights[x][y]+=a[y*SIZE+x];
		delete[] a;
	}

	float maxh=1;
	for(int x=0; x<SIZE; x++)
		for(int y=0; y<SIZE; y++)
			if(heights[y][x]>maxh)
				maxh=heights[y][x];
	for(int x=0; x<SIZE; x++)
		for(int y=0; y<SIZE; y++)
		{
			float h=heights[y][x]/maxh*254;
			if(h>0)
			{
				texture[x][y][0]=ColorMap[(int)h][0];
				texture[x][y][1]=ColorMap[(int)h][1];
				texture[x][y][2]=ColorMap[(int)h][2];
			}
			else
			{
				texture[x][y][0]=(char)(0.8*255);
				texture[x][y][1]=(char)(0.7*255);
				texture[x][y][2]=(char)(0.5*255);
			}
		}
	glBindTexture(GL_TEXTURE_2D, land_texture);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, SIZE, SIZE, GL_RGB, GL_UNSIGNED_BYTE, texture);
}

// отрисовать ландшафт
void drawPerlinLandscape(float x, float y, float z)
{
	glBindTexture(GL_TEXTURE_2D, land_texture);
	glBegin(GL_QUADS);
		for(int i=0; i<SIZE-4; i+=4)
			for(int j=0; j<SIZE-4; j+=4)
			{
				glTexCoord2f(1.0*(i  )/SIZE,1.0*(j  )/SIZE);glVertex3f(x+1.0*(i  )/SIZE,y+1.0*(j  )/SIZE,z+heights[i  ][j  ]/(SIZE*2));
				glTexCoord2f(1.0*(i+4)/SIZE,1.0*(j  )/SIZE);glVertex3f(x+1.0*(i+4)/SIZE,y+1.0*(j  )/SIZE,z+heights[i+4][j  ]/(SIZE*2));
				glTexCoord2f(1.0*(i+4)/SIZE,1.0*(j+4)/SIZE);glVertex3f(x+1.0*(i+4)/SIZE,y+1.0*(j+4)/SIZE,z+heights[i+4][j+4]/(SIZE*2));
				glTexCoord2f(1.0*(i  )/SIZE,1.0*(j+4)/SIZE);glVertex3f(x+1.0*(i  )/SIZE,y+1.0*(j+4)/SIZE,z+heights[i  ][j+4]/(SIZE*2));
			}
	glEnd();
}

