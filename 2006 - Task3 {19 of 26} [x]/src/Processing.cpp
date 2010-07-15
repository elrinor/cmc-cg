#include "Processing.h"
#include "SimpleBitmap.h"
#include "math.h"

inline int crop(int x, int minx, int maxx){return max(min(x,maxx),minx);}
inline int random(int max){return (max*rand())/RAND_MAX;}
#define SQUARE(x) ((x)*(x))
#define L1(r1,g1,b1,r2,g2,b2) (abs((r1)-(r2))+abs((g1)-(g2))+abs((b1)-(b2)))

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

void Contrast(DSimpleBitmap *img2)
{
	int wdth=(img2->GetWidth()*3+3)&-4;
	int bmpW=img2->GetWidth(), bmpH=img2->GetHeight();
  DSimpleBitmap img=*img2;
	unsigned char *B=(unsigned char *)img.GetLinePointer(0);
	unsigned char *B2=(unsigned char *)img2->GetLinePointer(0);
	int Hystogramm[256];
  memset(&Hystogramm,0,sizeof(int)*256);
  int x,y;
  for(x=0; x<bmpW; x++) for(y=0; y<bmpH; y++)
    Hystogramm[(int)(0.114*B[y*wdth+x*3+0]+0.587*B[y*wdth+x*3+1]+0.299*B[y*wdth+x*3+2])]++;
  int n=0, breakn=bmpH*bmpW/30, ymin, ymax;
  for(ymin=0; ymin<256; ymin++)
  {
    n+=Hystogramm[ymin];
    if(n>breakn) break;
  }
  n=0;
  for(ymax = 255; ymax >= 0; ymax--)
  {
    n+=Hystogramm[ymax];
    if(n>breakn) break;
  }
  for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++)
  {
    B2[y*wdth+x*3+0]=crop((B[y*wdth+x*3+0]-ymin)*256/(ymax-ymin+1),0,255);
    B2[y*wdth+x*3+1]=crop((B[y*wdth+x*3+1]-ymin)*256/(ymax-ymin+1),0,255);
    B2[y*wdth+x*3+2]=crop((B[y*wdth+x*3+2]-ymin)*256/(ymax-ymin+1),0,255);
  }
}

void Binarization(DSimpleBitmap *img2)
{
	int wdth=(img2->GetWidth()*3+3)&-4;
	int bmpW=img2->GetWidth(), bmpH=img2->GetHeight();
  DSimpleBitmap img=*img2;
	unsigned char *B=(unsigned char *)img.GetLinePointer(0);
	unsigned char *B2=(unsigned char *)img2->GetLinePointer(0);
	int x,y;
	int Col1[3]={0,0,0};
	int Col2[3]={255,255,255};
  for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++)
	{
    if(B[y*wdth+x*3+0]>Col1[0]) Col1[0]=B[y*wdth+x*3+0];
    if(B[y*wdth+x*3+1]>Col1[1]) Col1[1]=B[y*wdth+x*3+1];
    if(B[y*wdth+x*3+2]>Col1[2]) Col1[2]=B[y*wdth+x*3+2];
    if(B[y*wdth+x*3+0]<Col2[0]) Col2[0]=B[y*wdth+x*3+0];
    if(B[y*wdth+x*3+1]<Col2[1]) Col2[1]=B[y*wdth+x*3+1];
    if(B[y*wdth+x*3+2]<Col2[2]) Col2[2]=B[y*wdth+x*3+2];
  }
  int k1,k2,nCol1[3],nCol2[3];
  nCol1[0]=Col1[0];nCol1[1]=Col1[1];nCol1[2]=Col1[2];
  nCol2[0]=Col2[0];nCol2[1]=Col2[1];nCol2[2]=Col2[2];
  do
  {
    Col1[0]=nCol1[0];Col1[1]=nCol1[1];Col1[2]=nCol1[2];
    Col2[0]=nCol2[0];Col2[1]=nCol2[1];Col2[2]=nCol2[2];
    k1=k2=nCol1[0]=nCol1[1]=nCol1[2]=nCol2[0]=nCol2[1]=nCol2[2]=0;
    for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++)
    {
      if(L1(B[y*wdth+x*3+0],B[y*wdth+x*3+1],B[y*wdth+x*3+2],Col1[0],Col1[1],Col1[2])<
         L1(B[y*wdth+x*3+0],B[y*wdth+x*3+1],B[y*wdth+x*3+2],Col2[0],Col2[1],Col2[2]))
      {
        k1++;
        nCol1[0]+=B[y*wdth+x*3+0];
        nCol1[1]+=B[y*wdth+x*3+1];
        nCol1[2]+=B[y*wdth+x*3+2];
      }
      else
      {
        k2++;
        nCol2[0]+=B[y*wdth+x*3+0];
        nCol2[1]+=B[y*wdth+x*3+1];
        nCol2[2]+=B[y*wdth+x*3+2];
      }
    }
    for(int i=0; i<3; i++)
    {
      if(k1!=0) nCol1[i]/=k1; else nCol1[i]=Col1[i];
      if(k2!=0) nCol2[i]/=k2; else nCol2[i]=Col2[i];
    }
  }
  while (L1(Col1[0],Col1[1],Col1[2],nCol1[0],nCol1[1],nCol1[2])+L1(Col2[0],Col2[1],Col2[2],nCol2[0],nCol2[1],nCol2[2])>40);
  Col1[0]=nCol1[0];Col1[1]=nCol1[1];Col1[2]=nCol1[2];
  Col2[0]=nCol2[0];Col2[1]=nCol2[1];Col2[2]=nCol2[2];
  if(k1>k2)
  {
    for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++)
    {
      if(L1(B[y*wdth+x*3+0],B[y*wdth+x*3+1],B[y*wdth+x*3+2],Col1[0],Col1[1],Col1[2])<
         L1(B[y*wdth+x*3+0],B[y*wdth+x*3+1],B[y*wdth+x*3+2],Col2[0],Col2[1],Col2[2]))
      {
        B2[y*wdth+x*3+0]=0;
        B2[y*wdth+x*3+1]=0;
        B2[y*wdth+x*3+2]=0;
      }
      else
      {
        B2[y*wdth+x*3+0]=255;
        B2[y*wdth+x*3+1]=255;
        B2[y*wdth+x*3+2]=255;
      }
    }
  }
  else
  {
    for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++)
    {
      if(L1(B[y*wdth+x*3+0],B[y*wdth+x*3+1],B[y*wdth+x*3+2],Col1[0],Col1[1],Col1[2])<
         L1(B[y*wdth+x*3+0],B[y*wdth+x*3+1],B[y*wdth+x*3+2],Col2[0],Col2[1],Col2[2]))
      {
        B2[y*wdth+x*3+0]=255;
        B2[y*wdth+x*3+1]=255;
        B2[y*wdth+x*3+2]=255;
      }
      else
      {
        B2[y*wdth+x*3+0]=0;
        B2[y*wdth+x*3+1]=0;
        B2[y*wdth+x*3+2]=0;
      }
    }
  }
}

void BinaryDilation(DSimpleBitmap *img, int radius)
{
	if(radius==0)
		return;
	int wdth=(img->GetWidth()*3+3)&-4;
	int bmpW=img->GetWidth(), bmpH=img->GetHeight();
  DSimpleBitmap img2=*img;
	unsigned char *B=(unsigned char *)img->GetLinePointer(0);
	unsigned char *B2=(unsigned char *)img2.GetLinePointer(0);
	int x,y,x1,y1;
	for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++) if(B[y*wdth+x*3+0]==255)
	{
		int xD=max(0,x-radius), xU=min(bmpW-1,x+radius);
		for(x1=xD; x1<=xU; x1++)
      B2[y*wdth+x1*3+0]=255;
  }
	for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++) if(B2[y*wdth+x*3+0]==255)
	{
		int yD=max(0,y-radius), yU=min(bmpH-1,y+radius);
		for(y1=yD; y1<=yU; y1++)
      B[y1*wdth+x*3+0]=255;
  }
	for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++) if(B[y*wdth+x*3+0]==255)
  {
    B[y*wdth+x*3+0]=255;  
    B[y*wdth+x*3+1]=255;  
    B[y*wdth+x*3+2]=255;  
  }
}

void BinaryErosion(DSimpleBitmap *img, int radius)
{
	if(radius==0)
		return;
	int wdth=(img->GetWidth()*3+3)&-4;
	int bmpW=img->GetWidth(), bmpH=img->GetHeight();
  DSimpleBitmap img2=*img;
	unsigned char *B=(unsigned char *)img->GetLinePointer(0);
	unsigned char *B2=(unsigned char *)img2.GetLinePointer(0);
	int x,y,x1,y1;
	for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++) if(B[y*wdth+x*3+0]==0)
	{
		int xD=max(0,x-radius), xU=min(bmpW-1,x+radius);
		for(x1=xD; x1<=xU; x1++)
      B2[y*wdth+x1*3+0]=0;
  }
	for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++) if(B2[y*wdth+x*3+0]==0)
	{
		int yD=max(0,y-radius), yU=min(bmpH-1,y+radius);
		for(y1=yD; y1<=yU; y1++)
      B[y1*wdth+x*3+0]=0;
  }
	for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++) if(B[y*wdth+x*3+0]==0)
  {
    B[y*wdth+x*3+0]=0;  
    B[y*wdth+x*3+1]=0;  
    B[y*wdth+x*3+2]=0;  
  }
}

void BinaryClose(DSimpleBitmap *img, int radius)
{
  BinaryDilation(img,radius);
  BinaryErosion(img,radius);
}

void BinaryOpen(DSimpleBitmap *img, int radius)
{
  BinaryErosion(img,radius);
  BinaryDilation(img,radius);
}

void RemovePoints(DSimpleBitmap *img)
{
	int wdth=(img->GetWidth()*3+3)&-4;
	int bmpW=img->GetWidth(), bmpH=img->GetHeight();
	unsigned char *B=(unsigned char *)img->GetLinePointer(0);
	int x,y,c;
  for(y=0; y<bmpH; y++)
  {
    B[y*wdth+0*3+0]=0;
    B[y*wdth+(bmpW-1)*3+0]=0;
  }
  for(x=0; x<bmpW; x++)
  {
    B[0*wdth+x*3+0]=0;
    B[(bmpH-1)*wdth+x*3+0]=0;
  }
	for(y=1; y<bmpH-1; y++) for(x=1; x<bmpW-1; x++) 
  {
    if(B[y*wdth+x*3+0]==255)
  	{
      c=0;
      c+=B[(y+1)*wdth+x*3+0]+B[(y-1)*wdth+x*3+0]+B[y*wdth+(x+1)*3+0]+B[y*wdth+(x-1)*3+0];
      if(c<=255)
        B[y*wdth+x*3+0]=0;
    }
  }
	for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++) if(B[y*wdth+x*3+0]==0)
  {
    B[y*wdth+x*3+0]=0;  
    B[y*wdth+x*3+1]=0;  
    B[y*wdth+x*3+2]=0;  
  }
}


namespace nScan{

typedef struct{
  bool used;
  int x,y,s,col[3],color,xU,xD,yU,yD;
  float m02,m20,m11,exc;
  int kind;
  int eats;
} object;

int* BB;
int bmpW,bmpH;
int N;

void MyFloodFill(int X, int Y)
{
int *p1=new int[sizeof(int)*4*(bmpW+bmpH)];
int *p2=new int[sizeof(int)*4*(bmpW+bmpH)];
int *p3;
int n1=1,n2=0;
p1[0]=X;
p1[1]=Y;
while (true)
{
  for(int i=0; i<n1; i++)
  {
    BB[p1[i*2+1]*bmpW+p1[i*2+0]]=N;
    if(p1[i*2+0]>0 && BB[p1[i*2+1]*bmpW+p1[i*2+0]-1]==-10)
    {
      BB[p1[i*2+1]*bmpW+p1[i*2+0]-1]=N;
      p2[n2*2+0]=p1[i*2+0]-1;
      p2[n2*2+1]=p1[i*2+1];
      n2++;
    }
    if(p1[i*2+0]<bmpW-1 && BB[p1[i*2+1]*bmpW+p1[i*2+0]+1]==-10)
    {
      BB[p1[i*2+1]*bmpW+p1[i*2+0]+1]=N;
      p2[n2*2+0]=p1[i*2+0]+1;
      p2[n2*2+1]=p1[i*2+1];
      n2++;
    }
    if(p1[i*2+1]>0 && BB[(p1[i*2+1]-1)*bmpW+p1[i*2+0]]==-10)
    {
      BB[(p1[i*2+1]-1)*bmpW+p1[i*2+0]]=N;
      p2[n2*2+0]=p1[i*2+0];
      p2[n2*2+1]=p1[i*2+1]-1;
      n2++;
    }
    if(p1[i*2+1]<bmpH-1 && BB[(p1[i*2+1]+1)*bmpW+p1[i*2+0]]==-10)
    {
      BB[(p1[i*2+1]+1)*bmpW+p1[i*2+0]]=N;
      p2[n2*2+0]=p1[i*2+0];
      p2[n2*2+1]=p1[i*2+1]+1;
      n2++;
    }
  }
  if(n2==0) break;
  n1=n2;n2=0;
  p3=p1;p1=p2;p2=p3;
}
delete[] p1;
delete[] p2;
}

void Scan(DSimpleBitmap *img, DSimpleBitmap *bin, string *Report, int mins)
{
  if (img->GetHeight()!=bin->GetHeight() || img->GetWidth()!=bin->GetWidth())
    return;
  int wdth=(img->GetWidth()*3+3)&-4;
	bmpW=img->GetWidth();
  bmpH=img->GetHeight();
	unsigned char *B=(unsigned char *)img->GetLinePointer(0);
	unsigned char *B2=(unsigned char *)bin->GetLinePointer(0);
  int x,y,i,j;
  BB=new int[bmpW*bmpH];
	for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++)
  {
    if (B2[y*wdth+x*3+0]==0)
      BB[y*bmpW+x]=-1;
    else
      BB[y*bmpW+x]=-10;
  }
  for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++) if(BB[y*bmpW+x]==-10)
  {
    MyFloodFill(x,y);
    N++;
  }
  object* o=new object[N];
  memset(o,0,N*sizeof(object));
  for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++) if(BB[y*bmpW+x]>=0)
  {
    o[BB[y*bmpW+x]].x+=x;
    o[BB[y*bmpW+x]].y+=y;
    o[BB[y*bmpW+x]].col[0]+=B[y*wdth+x*3+0];
    o[BB[y*bmpW+x]].col[1]+=B[y*wdth+x*3+1];
    o[BB[y*bmpW+x]].col[2]+=B[y*wdth+x*3+2];
    o[BB[y*bmpW+x]].s++;
  }
  for(i=0; i<N; i++)
  {
    if(o[i].s<mins)
      o[i].used=false;
    else
    {
      o[i].used=true;
      o[i].col[0]/=o[i].s;
      o[i].col[1]/=o[i].s;
      o[i].col[2]/=o[i].s;
      if(abs(o[i].col[2]-o[i].col[1])<=3 && abs(o[i].col[1]-o[i].col[0])<=3 && abs(o[i].col[0]-o[i].col[2])<=3)
        o[i].color=0;
      else
      {
        int mean=(max(o[i].col[0],max(o[i].col[1],o[i].col[2]))+min(o[i].col[0],min(o[i].col[1],o[i].col[2])))/2;
             if(o[i].col[2]>=mean && o[i].col[0]>=mean)               o[i].color=1;
        else if(o[i].col[2]>=mean && o[i].col[1]>=mean)               o[i].color=2;
        else if(o[i].col[1]>=o[i].col[2] && o[i].col[1]>=o[i].col[0]) o[i].color=3;
        else if(o[i].col[2]>=o[i].col[1] && o[i].col[2]>=o[i].col[0]) o[i].color=4;
        else if(o[i].col[0]>=o[i].col[2] && o[i].col[0]>=o[i].col[1]) o[i].color=5;
      }
      o[i].xD=bmpW;
      o[i].yD=bmpH;
      o[i].x/=o[i].s;
      o[i].y/=o[i].s;
    }
  }
  for(y=0; y<bmpH; y++) for(x=0; x<bmpW; x++) if(BB[y*bmpW+x]>=0 && o[BB[y*bmpW+x]].used)
  {
    if(x>o[BB[y*bmpW+x]].xU) o[BB[y*bmpW+x]].xU=x;
    if(x<o[BB[y*bmpW+x]].xD) o[BB[y*bmpW+x]].xD=x;
    if(y>o[BB[y*bmpW+x]].yU) o[BB[y*bmpW+x]].yU=y;
    if(y<o[BB[y*bmpW+x]].yD) o[BB[y*bmpW+x]].yD=y;
    o[BB[y*bmpW+x]].m11+=(x-o[BB[y*bmpW+x]].x)*(y-o[BB[y*bmpW+x]].y);
    o[BB[y*bmpW+x]].m02+=SQUARE(y-o[BB[y*bmpW+x]].y);
    o[BB[y*bmpW+x]].m20+=SQUARE(x-o[BB[y*bmpW+x]].x);
  }
  int NN=0;
  for(i=0; i<N; i++) if(o[i].used)
  {
    o[i].exc=(o[i].m02+o[i].m20+sqrt(SQUARE(o[i].m20-o[i].m02)+4*SQUARE(o[i].m11))) /
             (o[i].m02+o[i].m20-sqrt(SQUARE(o[i].m20-o[i].m02)+4*SQUARE(o[i].m11)));
    if(o[i].exc>16)                   o[i].kind=1;
    else if(o[i].exc>1 && o[i].exc<5) o[i].kind=2;
    NN++;
  }
  object* o2=new object[NN];
  j=0;
  for(i=0; i<N; i++) if(o[i].used)
  {
    o2[j]=o[i];
    o2[j].eats=-1;
    j++;
  }
  for(i=0; i<NN; i++) for(j=i+1; j<NN; j++)
    if(o2[i].y>o2[j].y)
    {
      object temp;
      temp=o2[j];
      o2[j]=o2[i];
      o2[i]=temp;
    }
  for(i=0; i<NN; i++) if(o2[i].kind==1)
  {
    int jbest=-1;
    int l=999999999;
    for(j=0; j<NN; j++) if(o2[j].kind==2 && (o2[j].color==0 || o2[i].color==o2[j].color))
    {
      int d=SQUARE(o2[i].x-o2[j].x)+SQUARE(o2[i].y-o2[j].y);
      if(d<l)
      {
        l=d;
        jbest=j;
      }
    }
    if(jbest!=-1)
      o2[i].eats=jbest;
  }
  CDC dc;
  dc.Attach(img->m_hDC);
  CPen pen1,pen2;
  pen1.CreatePen(PS_SOLID,2,RGB(64,255,64));
  pen2.CreatePen(PS_SOLID,2,RGB(64,64,255));
  dc.SelectObject(&pen1);
  for(i=0; i<NN; i++) if(o2[i].kind==1)
  {
    dc.MoveTo(o2[i].xD,o2[i].yD);
    dc.LineTo(o2[i].xU,o2[i].yD);
    dc.LineTo(o2[i].xU,o2[i].yU);
    dc.LineTo(o2[i].xD,o2[i].yU);
    dc.LineTo(o2[i].xD,o2[i].yD);
  }
  dc.SelectObject(&pen2);
  for(i=0; i<NN; i++) if(o2[i].kind==2)
  {
    dc.MoveTo(o2[i].xD,o2[i].yD);
    dc.LineTo(o2[i].xU,o2[i].yD);
    dc.LineTo(o2[i].xU,o2[i].yU);
    dc.LineTo(o2[i].xD,o2[i].yU);
    dc.LineTo(o2[i].xD,o2[i].yD);
  }
  for(i=0; i<NN; i++) if(o2[i].kind==2)
  {
    x=o2[i].x;
    y=o2[i].y;
    dc.MoveTo(o2[i].x-3,o2[i].y-3);
    dc.LineTo(o2[i].x+3,o2[i].y-3);
    dc.LineTo(o2[i].x+3,o2[i].y+3);
    dc.LineTo(o2[i].x-3,o2[i].y+3);
    dc.LineTo(o2[i].x-3,o2[i].y-3);
    dc.MoveTo(x,y);
    while(true)
    {
      int jbest=-1;
      int l=999999999;
      for(j=0; j<NN; j++) if(o2[j].kind==1 && o2[j].eats==i && o2[j].used)
      {
        int d=SQUARE(o2[j].x-x)+SQUARE(o2[j].y-y);
        if(d<l)
        {
          l=d;
          jbest=j;
        }
      }
      if(jbest!=-1)
      {
        dc.MoveTo(x,y);
        x=o2[jbest].x;
        y=o2[jbest].y;
        dc.LineTo(x,y);
        dc.MoveTo(x-3,y-3);
        dc.LineTo(x+3,y-3);
        dc.LineTo(x+3,y+3);
        dc.LineTo(x-3,y+3);
        dc.LineTo(x-3,y-3);
        o2[jbest].used=false;
      }
      else break;
    }
  }
  Report->clear();
  j=0;
  for(i=0; i<NN; i++) if(o2[i].kind==2)
  {
    j++;
    int n=0;
    for(int k=0; k<NN; k++)
      if(o2[k].eats==i && o2[k].kind==1)
        n++;
    char js[30],es[30];
    sprintf(js,"%d",j);
    sprintf(es,"%d",n);
    *Report+="Elephant ";
    *Report+=js;
    *Report+=" - ";
    *Report+=es;
    *Report+=" bamboos\n";
  }
  delete[] o;
  delete[] o2;
  delete[] BB;
}

}

void Scan(DSimpleBitmap *img, DSimpleBitmap *bin, string *Report, int mins)
{
  nScan::Scan(img, bin, Report, mins);
}