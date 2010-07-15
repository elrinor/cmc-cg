#include "Processing.h"
#include "SimpleBitmap.h"
#include "math.h"

inline int crop(int x, int minx, int maxx){return max(min(x,maxx),minx);}
inline int random(int max){return (max*rand())/RAND_MAX;}
#define SQUARE(x) ((x)*(x))

#define KERNEL_MAX_R 100
typedef struct 
{
	int r;
	float kernel[2*KERNEL_MAX_R+1][2*KERNEL_MAX_R+1];
} filter2d;

typedef struct
{
	int r;
	float kernel[2*KERNEL_MAX_R+1];
} filter1d;

void GeneralConvolution2D(DSimpleBitmap *img2, filter2d* conv)
{
	int wdth=(img2->GetWidth()*3+3)&-4;
	int bmpW=img2->GetWidth(), bmpH=img2->GetHeight();
  DSimpleBitmap img=*img2;
	unsigned char *B=(unsigned char *)img.GetLinePointer(0);
	unsigned char *B2=(unsigned char *)img2->GetLinePointer(0);
	float c[3];
	int x,y,dx,dy,x1,y1;
	for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++)
	{
		c[0]=0;c[1]=0;c[2]=0;
		for(dx=-conv->r; dx<=conv->r; dx++) for(dy=-conv->r; dy<=conv->r; dy++)
		{
			x1=crop(x+dx,0,bmpW-1);
			y1=crop(y+dy,0,bmpH-1);
			c[0]+=B[y1*wdth+x1*3+0]*conv->kernel[conv->r+dx][conv->r+dy];
			c[1]+=B[y1*wdth+x1*3+1]*conv->kernel[conv->r+dx][conv->r+dy];
			c[2]+=B[y1*wdth+x1*3+2]*conv->kernel[conv->r+dx][conv->r+dy];
		}
		B2[y*wdth+x*3+0]=crop((int)c[0],0,255);
		B2[y*wdth+x*3+1]=crop((int)c[1],0,255);
		B2[y*wdth+x*3+2]=crop((int)c[2],0,255);
	}
}

void GeneralConvolution1D(DSimpleBitmap *img, filter1d* conv)
{
	int wdth=(img->GetWidth()*3+3)&-4;
	int bmpW=img->GetWidth(), bmpH=img->GetHeight();
  DSimpleBitmap img2=*img;
	unsigned char *B=(unsigned char *)img->GetLinePointer(0);
	unsigned char *B2=(unsigned char *)img2.GetLinePointer(0);
	float c[3];
	int x,y,dx,dy,x1,y1;
	for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++)
	{
		c[0]=0;c[1]=0;c[2]=0;
		for(dx=-conv->r; dx<=conv->r; dx++)
		{
			x1=crop(x+dx,0,bmpW-1);
			c[0]+=B[y*wdth+x1*3+0]*conv->kernel[conv->r+dx];
			c[1]+=B[y*wdth+x1*3+1]*conv->kernel[conv->r+dx];
			c[2]+=B[y*wdth+x1*3+2]*conv->kernel[conv->r+dx];
		}
		B2[y*wdth+x*3+0]=crop((int)c[0],0,255);
		B2[y*wdth+x*3+1]=crop((int)c[1],0,255);
		B2[y*wdth+x*3+2]=crop((int)c[2],0,255);
	}
	for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++)
	{
		c[0]=0;c[1]=0;c[2]=0;
		for(dy=-conv->r; dy<=conv->r; dy++)
		{
			y1=crop(y+dy,0,bmpH-1);
			c[0]+=B2[y1*wdth+x*3+0]*conv->kernel[conv->r+dy];
			c[1]+=B2[y1*wdth+x*3+1]*conv->kernel[conv->r+dy];
			c[2]+=B2[y1*wdth+x*3+2]*conv->kernel[conv->r+dy];
		}
		B[y*wdth+x*3+0]=crop((int)c[0],0,255);
		B[y*wdth+x*3+1]=crop((int)c[1],0,255);
		B[y*wdth+x*3+2]=crop((int)c[2],0,255);
	}
}

void WhiteNoise(DSimpleBitmap *img, int Noise)
{
	if(Noise==0)
		return;
	int wdth=(img->GetWidth()*3+3)&-4;
	unsigned char *B=(unsigned char *)img->GetLinePointer(0);
  for(int y=0; y<img->GetHeight(); y++) for (int x=0; x<img->GetWidth(); x++)
  {
    B[y*wdth+x*3+0]=crop(B[y*wdth+x*3+0]+random(Noise)-random(Noise),0,255);
    B[y*wdth+x*3+1]=crop(B[y*wdth+x*3+1]+random(Noise)-random(Noise),0,255);
    B[y*wdth+x*3+2]=crop(B[y*wdth+x*3+2]+random(Noise)-random(Noise),0,255);
  }
}

void ImpulseNoise(DSimpleBitmap *img, int Probability)
{
	if(Probability==0)
		return;
	int wdth=(img->GetWidth()*3+3)&-4;
	unsigned char *B=(unsigned char *)img->GetLinePointer(0);
  for(int y=0; y<img->GetHeight(); y++) for (int x=0; x<img->GetWidth(); x++)	if(random(100)<Probability)
	{
	  B[y*wdth+x*3+0]=crop(B[y*wdth+x*3+0]+random(200)-random(200),0,255);
		B[y*wdth+x*3+1]=crop(B[y*wdth+x*3+1]+random(200)-random(200),0,255);
		B[y*wdth+x*3+2]=crop(B[y*wdth+x*3+2]+random(200)-random(200),0,255);
	}
}

void Sharpen(DSimpleBitmap *img)
{
	filter2d conv;
	memset(&conv,0,sizeof(filter2d));
	conv.r=1;
	conv.kernel[conv.r+0][conv.r+0]=22.0f/10;
	conv.kernel[conv.r+1][conv.r+0]=-2.0f/10;
	conv.kernel[conv.r+0][conv.r+1]=-2.0f/10;
	conv.kernel[conv.r+1][conv.r+1]=-1.0f/10;
	conv.kernel[conv.r-1][conv.r+1]=-1.0f/10;
	conv.kernel[conv.r+1][conv.r-1]=-1.0f/10;
	conv.kernel[conv.r-1][conv.r-1]=-1.0f/10;
	conv.kernel[conv.r+0][conv.r-1]=-2.0f/10;
	conv.kernel[conv.r-1][conv.r+0]=-2.0f/10;
	GeneralConvolution2D(img, &conv);
}

void Blur(DSimpleBitmap *img)
{
	filter2d conv;
	memset(&conv,0,sizeof(filter2d));
	conv.r=1;
	conv.kernel[conv.r+0][conv.r+0]=3.0f/15;
	conv.kernel[conv.r+1][conv.r+0]=2.0f/15;
	conv.kernel[conv.r-1][conv.r+0]=2.0f/15;
	conv.kernel[conv.r-1][conv.r+1]=1.0f/15;
	conv.kernel[conv.r+1][conv.r+1]=1.0f/15;
	conv.kernel[conv.r-1][conv.r-1]=1.0f/15;
	conv.kernel[conv.r+1][conv.r-1]=1.0f/15;
	conv.kernel[conv.r+0][conv.r-1]=2.0f/15;
	conv.kernel[conv.r+0][conv.r+1]=2.0f/15;
	GeneralConvolution2D(img, &conv);
}

void FindEdges(DSimpleBitmap *img)
{
	filter2d conv;
	memset(&conv,0,sizeof(filter2d));
	conv.r=1;
	conv.kernel[conv.r+0][conv.r+0]= 4.0f;
	conv.kernel[conv.r-1][conv.r+0]=-1.0f;
	conv.kernel[conv.r+0][conv.r+1]=-1.0f;
	conv.kernel[conv.r+1][conv.r+0]=-1.0f;
	conv.kernel[conv.r+0][conv.r-1]=-1.0f;
	GeneralConvolution2D(img, &conv);
}

void GaussianBlur1D(DSimpleBitmap *img, float h)
{
	if(h==0)
		return;
	filter1d conv;
	memset(&conv,0,sizeof(filter1d));
	conv.r=min((int)(1+3*h),KERNEL_MAX_R);
  for(int k=-conv.r; k<=conv.r; k++)
		conv.kernel[conv.r+k]=exp(-SQUARE(k/h)/2);
  float s=0;
  for(int k=-conv.r; k<=conv.r; k++)
		s+=conv.kernel[conv.r+k];
  for(int k=-conv.r; k<=conv.r; k++)
    conv.kernel[conv.r+k]/=s;
  GeneralConvolution1D(img,&conv);
}

void GaussianBlur2D(DSimpleBitmap *img, float h)
{
	if(h==0)
		return;
	filter2d conv;
	memset(&conv,0,sizeof(filter2d));
	conv.r=min((int)(1+3*h),KERNEL_MAX_R);
  for(int x=-conv.r; x<=conv.r; x++) for(int y=-conv.r; y<=conv.r; y++)
		conv.kernel[conv.r+x][conv.r+y]=exp(-(SQUARE(x)+SQUARE(y))/(2*SQUARE(h)));
  float s=0;
  for(int x=-conv.r; x<=conv.r; x++) for(int y=-conv.r; y<=conv.r; y++)
		s+=conv.kernel[conv.r+x][conv.r+y];
  for(int x=-conv.r; x<=conv.r; x++) for(int y=-conv.r; y<=conv.r; y++)
    conv.kernel[conv.r+x][conv.r+y]/=s;
  GeneralConvolution2D(img,&conv);
}

void MedianNormal(DSimpleBitmap *img2, int radius)
{
	if(radius==0)
		return;
	char Cols[256][3];
	int  Cnts[256];
	int wdth=(img2->GetWidth()*3+3)&-4;
	int bmpW=img2->GetWidth(), bmpH=img2->GetHeight();
  DSimpleBitmap img=*img2;
	unsigned char *B=(unsigned char *)img.GetLinePointer(0);
	unsigned char *B2=(unsigned char *)img2->GetLinePointer(0);
	unsigned char *G=new unsigned char[bmpW*bmpH];
	int x,y,x1,y1;
	for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++)
    G[y*bmpW+x]=(unsigned char)(0.114*B[y*wdth+x*3+0]+0.587*B[y*wdth+x*3+1]+0.299*B[y*wdth+x*3+2]);
	for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++)
	{
		memset(&Cnts,0,sizeof(Cnts));
		int xD=max(0,x-radius), xU=min(bmpW-1,x+radius), yD=max(0,y-radius), yU=min(bmpH-1,y+radius);
		int Median=(xU-xD+1)*(yU-yD+1)/2;
		for(x1=xD; x1<=xU; x1++) for(y1=yD; y1<=yU; y1++)
		{
			if(Cnts[G[bmpW*y1+x1]]==0)
			{
				Cols[G[bmpW*y1+x1]][0]=B[y1*wdth+x1*3+0];
				Cols[G[bmpW*y1+x1]][1]=B[y1*wdth+x1*3+1];
				Cols[G[bmpW*y1+x1]][2]=B[y1*wdth+x1*3+2];
				Cnts[G[bmpW*y1+x1]]=1;
			}
      else
				Cnts[G[bmpW*y1+x1]]++;
		}
		int c=0;
		int k;
    for(k=0; k<=255; k++) if(Cnts[k]!=0)
		{
			c+=Cnts[k];
			if(c>Median)
				break;
		}
    B2[y*wdth+x*3+0]=Cols[k][0];
    B2[y*wdth+x*3+1]=Cols[k][1];
    B2[y*wdth+x*3+2]=Cols[k][2];
	}
	delete[] G;
}

void MedianVector(DSimpleBitmap *img2, int radius)
{
	if(radius==0)
		return;
	unsigned char *Cols=new unsigned char[3*SQUARE(2*radius+1)];
	int wdth=(img2->GetWidth()*3+3)&-4;
	int bmpW=img2->GetWidth(), bmpH=img2->GetHeight();
  DSimpleBitmap img=*img2;
	unsigned char *B=(unsigned char *)img.GetLinePointer(0);
	unsigned char *B2=(unsigned char *)img2->GetLinePointer(0);
	int x,y,x1,y1;
	for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++)
	{
		int xD=max(0,x-radius), xU=min(bmpW-1,x+radius), yD=max(0,y-radius), yU=min(bmpH-1,y+radius);
		int Cnt=0;
		for(x1=xD; x1<=xU; x1++) for(y1=yD; y1<=yU; y1++)
		{
			Cols[3*Cnt+0]=B[y1*wdth+x1*3+0];
      Cols[3*Cnt+1]=B[y1*wdth+x1*3+1];
      Cols[3*Cnt+2]=B[y1*wdth+x1*3+2];
			Cnt++;
		}
		int minL1=1000000001;
		int best;
		for(int k=0; k<Cnt; k++)
		{
			int L1=0;
			for(int i=0; i<Cnt; i++)
			{
				L1+=abs(Cols[3*k+0]-Cols[3*i+0])+abs(Cols[3*k+1]-Cols[3*i+1])+abs(Cols[3*k+2]-Cols[3*i+2]);
				if(L1>minL1)
					break;
			}
			if(L1<minL1)
			{
				minL1=L1;
				best=k;
			}
		}
    B2[y*wdth+x*3+0]=Cols[3*best+0];
    B2[y*wdth+x*3+1]=Cols[3*best+1];
    B2[y*wdth+x*3+2]=Cols[3*best+2];
	}
	delete[] Cols;
}

void KNN(DSimpleBitmap *img2, int radius, float h)
{
	if(radius==0 || h==0)
		return;
	int wdth=(img2->GetWidth()*3+3)&-4;
	int bmpW=img2->GetWidth(), bmpH=img2->GetHeight();
  DSimpleBitmap img=*img2;
	unsigned char *B=(unsigned char *)img.GetLinePointer(0);
	unsigned char *B2=(unsigned char *)img2->GetLinePointer(0);
	int x,y,x1,y1;
	float Exponent[256];
	for(int k=0; k<256; k++)
		Exponent[k]=exp(-SQUARE(k)/SQUARE(h));
	for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++)
	{
		float Col[3]={0,0,0};
		float K=0;
		int xD=max(0,x-radius), xU=min(bmpW-1,x+radius), yD=max(0,y-radius), yU=min(bmpH-1,y+radius);
		for(x1=xD; x1<=xU; x1++) for(y1=yD; y1<=yU; y1++)
		{
			int similarity=(int)sqrt(1/3.0f*((SQUARE(B[y1*wdth+x1*3+0]-B[y*wdth+x*3+0])+SQUARE(B[y1*wdth+x1*3+1]-B[y*wdth+x*3+1])+SQUARE(B[y1*wdth+x1*3+2]-B[y*wdth+x*3+2]))));
			K+=Exponent[similarity];
			Col[0]+=B[y1*wdth+x1*3+0]*Exponent[similarity];
      Col[1]+=B[y1*wdth+x1*3+1]*Exponent[similarity];
      Col[2]+=B[y1*wdth+x1*3+2]*Exponent[similarity];
		}
		B2[y*wdth+x*3+0]=(unsigned char)(Col[0]/K);
    B2[y*wdth+x*3+1]=(unsigned char)(Col[1]/K);
    B2[y*wdth+x*3+2]=(unsigned char)(Col[2]/K);
	}
}

int GetNoiseValue(DSimpleBitmap *img)
{
	DSimpleBitmap img2=*img;
	FindEdges(&img2);
	int Cnts[256];
	int wdth=(img2.GetWidth()*3+3)&-4;
	int bmpW=img2.GetWidth(), bmpH=img2.GetHeight();
  unsigned char *B2=(unsigned char *)img2.GetLinePointer(0);
	memset(&Cnts,0,sizeof(Cnts));
	for(int y=0; y<bmpH; y++) for(int x=0; x<bmpW; x++)
		Cnts[(unsigned char)(0.114*B2[y*wdth+x*3+0]+0.587*B2[y*wdth+x*3+1]+0.299*B2[y*wdth+x*3+2])]++;
	int c=0;
	int k;
	int median=bmpW*bmpH/2;
  for(k=0; k<=255; k++) if(Cnts[k]!=0)
	{
		c+=Cnts[k];
		if(c>median)
			break;
	}
	return k;
}

void KNNAutomatic(DSimpleBitmap *img2)
{
	int Noiseness=GetNoiseValue(img2);
	if (Noiseness>0)
		KNN(img2,min(1+(int)(Noiseness*0.24), 8),28+Noiseness*0.33f);
}

unsigned int SAD8x8_SSE(const byte *pSrc, const byte *pDst, unsigned int uWidth)
{
	unsigned int uSum;
	__asm
	{
		mov		esi, pSrc		; ds:[esi] points to the source image block
		mov		edi, pDst		; ds:[edi] points to the destination image block
		mov		ebx, uWidth		; ebx = uWidth
		mov		edx, ebx		; edx = uWidth
		shl		edx, 1			; edx = uWidth * 2

		; Load source rows in mm registers
		movq	mm0, [esi]
		movq	mm1, [esi+ebx]
		add		esi, edx
		movq	mm2, [esi]
		movq	mm3, [esi+ebx]
		add		esi, edx
		movq	mm4, [esi]
		movq	mm5, [esi+ebx]
		add		esi, edx
		movq	mm6, [esi]
		movq	mm7, [esi+ebx]

		; Calculate SADs with destination rows
		psadbw	mm0, [edi]
		psadbw	mm1, [edi+ebx]
		add		edi, edx
		psadbw	mm2, [edi]
		psadbw	mm3, [edi+ebx]
		add		edi, edx
		psadbw	mm4, [edi]
		psadbw	mm5, [edi+ebx]
		add		edi, edx
		psadbw	mm6, [edi]
		psadbw	mm7, [edi+ebx]

		; Sum all SADs
		paddusw	mm0, mm1
		paddusw	mm0, mm2
		paddusw	mm0, mm3
		paddusw	mm0, mm4
		paddusw	mm0, mm5
		paddusw	mm0, mm6
		paddusw	mm0, mm7

		movd	uSum, mm0		; store sum
		emms					; empty MMX state
		mov		eax, uSum		; function result: eax
	}
}

void NLM(DSimpleBitmap *img2, int radius, float h)
{
	if(radius==0 || h==0)
		return;
	int wdth=(img2->GetWidth()*3+3)&-4;
	int bmpW=img2->GetWidth(), bmpH=img2->GetHeight();
  DSimpleBitmap img=*img2;
	unsigned char *B=(unsigned char *)img.GetLinePointer(0);
	unsigned char *B2=(unsigned char *)img2->GetLinePointer(0);
	unsigned char *cR=new unsigned char[(bmpW+8)*(bmpH+8)];
	unsigned char *cG=new unsigned char[(bmpW+8)*(bmpH+8)];
	unsigned char *cB=new unsigned char[(bmpW+8)*(bmpH+8)];
	int x,y,x1,y1;
  for(y=0; y<bmpH+8; y++) for(x=0; x<bmpW+8; x++)
	{
		int x1=crop(x-4,0,bmpW-1);
		int y1=crop(y-4,0,bmpH-1);
		cB[y*(bmpW+8)+x]=B[y1*wdth+x1*3+0];
		cG[y*(bmpW+8)+x]=B[y1*wdth+x1*3+1];
		cR[y*(bmpW+8)+x]=B[y1*wdth+x1*3+2];
	}
	int gw=bmpW+8;
	float Exponent[512];
	for(int k=0; k<512; k++)
		Exponent[k]=exp(-SQUARE(k*0.5f)/SQUARE(h));
	for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++)
	{
		float Col[3]={0,0,0};
		float K[3]={0,0,0};
		int xD=max(0,x-radius), xU=min(bmpW-1,x+radius), yD=max(0,y-radius), yU=min(bmpH-1,y+radius);
		for(x1=xD; x1<=xU; x1++) for(y1=yD; y1<=yU; y1++)
		{
			int similarity;
			similarity=SAD8x8_SSE(&cB[y*gw+x],&cB[y1*gw+x1],gw)/32;
			K[0]+=Exponent[similarity];
			Col[0]+=B[y1*wdth+x1*3+0]*Exponent[similarity];
			similarity=SAD8x8_SSE(&cG[y*gw+x],&cG[y1*gw+x1],gw)/32;
			K[1]+=Exponent[similarity];
			Col[1]+=B[y1*wdth+x1*3+1]*Exponent[similarity];
			similarity=SAD8x8_SSE(&cR[y*gw+x],&cR[y1*gw+x1],gw)/32;
			K[2]+=Exponent[similarity];
			Col[2]+=B[y1*wdth+x1*3+2]*Exponent[similarity];
		}
		B2[y*wdth+x*3+0]=(unsigned char)(Col[0]/K[0]);
    B2[y*wdth+x*3+1]=(unsigned char)(Col[1]/K[1]);
    B2[y*wdth+x*3+2]=(unsigned char)(Col[2]/K[2]);
	}
	delete[] cR;
	delete[] cG;
	delete[] cB;
}

void NLMAutomatic(DSimpleBitmap *img2)
{
	int Noiseness=GetNoiseValue(img2);
	if(Noiseness>0)
		NLM(img2,min(1+(int)(Noiseness*0.29), 11),8+Noiseness*0.372f);
}




