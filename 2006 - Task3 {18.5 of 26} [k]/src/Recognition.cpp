//---------------------------------------------------------------------------


#pragma hdrstop

#include <algorithm>
#include "Progress.h"
#include "Recognition.h"
#include <math.h>
#include <Math.hpp>
using std::min;
using std::max;

#define sqr(x) ((x)*(x))

//---------------------------------------------------------------------------

#pragma package(smart_init)

struct Area
{
  int Type;
  float m11, m02, m20, Elongation, a, sina, cosa, hu, hd, wu, wd;
  union
  {
    int Eater;
    int Eaten;
  };
  int Color[3], ColorType;
  int x,y, Square;
};

const int Elephant = 1;
const int Bamboo = 2;

String Results = "";
int *map;
int AreaSquare;
int AreaType;
int EdgeType;
int SquareMin;
int w,h;

String LastRecognitionResult()
{
  return Results;
}


void Rec(int x, int y)
{
  if (x == 0 || y == 0 || x == w - 1 || y == h - 1)
    return;

  for (int xn = x - 1; xn >= 0; xn--)
    if (map[w * y + xn] == EdgeType)
      map[w * y + xn] = AreaType;
    else
    {
      AreaSquare += x - 1 - xn;
      break;
    }
    
  for (int xn = x; xn < w; xn++)
    if (map[w * y + xn] == EdgeType)
      map[w * y + xn] = AreaType;
    else
    {
      AreaSquare += xn - x;
      break;
    }

  for (int xn = x - 1; xn >= 0; xn--)
  {
    if (map[w * y + xn] == AreaType)
    {
      if (map[ w * (y - 1) + xn] == EdgeType)
        Rec(xn, y - 1);
      if (map[ w * (y + 1) + xn] == EdgeType)
        Rec(xn, y + 1);
    }
    else
      break;
  }

  for (int xn = x; xn < w; xn++)
  {
    if (map[w * y + xn] == AreaType)
    {
      if (map[ w * (y - 1) + xn] == EdgeType)
        Rec(xn, y - 1);
      if (map[ w * (y + 1) + xn] == EdgeType)
        Rec(xn, y + 1);
    }
    else
      break;
  }
}


void FillArea(int x, int y, int* AreaN)
{
  EdgeType = -1;
  AreaType = *AreaN + 1;
  AreaSquare = 0;
  Rec(x, y);
  if (AreaSquare > SquareMin)
    (*AreaN)++;
  else
  {
    EdgeType = *AreaN + 1;
    AreaType = 0;
    Rec(x, y);
  }
}

float Elongation(float m11, float m02, float m20)
{
  return (m02 + m20 + sqrt(sqr(m20 - m02) + 4 * sqr(m11))) / (m02 + m20 - sqrt(sqr(m20 - m02) + 4 * sqr(m11)));
}


int Recognition(Graphics::TBitmap *rgb_pImage, Graphics::TBitmap *bin_pImage, Graphics::TBitmap *out_pImage, int SquareThreshold = 500)
{
  if (rgb_pImage->Width != bin_pImage->Width || bin_pImage->Width != out_pImage->Width || out_pImage->Width != rgb_pImage->Width ||
      rgb_pImage->Height != bin_pImage->Height || bin_pImage->Height != out_pImage->Height || out_pImage->Height != rgb_pImage->Height)
  {
    Results = "Error.\nInput images have different sizes.";
    return -1;
  }

  SquareMin = SquareThreshold;

  int iBytesPerLine = (rgb_pImage->Width * 3 + 3) & -4;
  w = rgb_pImage->Width;
  h = rgb_pImage->Height;

  ProgressNextFilter(5);

  rgb_pImage->PixelFormat = pf24bit;
  bin_pImage->PixelFormat = pf24bit;
  out_pImage->PixelFormat = pf24bit;

  unsigned char *pBin = (unsigned char *) bin_pImage->ScanLine[bin_pImage->Height - 1];
  unsigned char *pRgb = (unsigned char *) rgb_pImage->ScanLine[rgb_pImage->Height - 1];
  unsigned char *pDst = (unsigned char *) out_pImage->ScanLine[out_pImage->Height - 1];

  map = new int[w*h];

  for (int x = 0; x < w; x++) for (int y = 0; y < h; y++)
  {
    if (pBin[y * iBytesPerLine + x * 3] == 0)
      map[y * w + x] = 0;
    else
      map[y * w + x] = -1;
  }

  ProgressUpdate();

  int AreaN = 0;

  for (int x = 0; x < w; x++) for (int y = 0; y < h; y++)
    if (map[y * w + x] == -1)
      FillArea(x, y, &AreaN);

  ProgressUpdate();

  Area* a = new Area[AreaN];

  memset((void*) a, 0, AreaN * sizeof(Area));

  for (int x = 0; x < w; x++) for (int y = 0; y < h; y++)
  {
    if (map[y * w + x] > 0)
    {
      int n = map[y * w + x] - 1;
      a[n].x += x;
      a[n].y += y;
      a[n].Color[0] += pRgb[y * iBytesPerLine + x * 3];
      a[n].Color[1] += pRgb[y * iBytesPerLine + x * 3 + 1];
      a[n].Color[2] += pRgb[y * iBytesPerLine + x * 3 + 2];
      a[n].Square++;
    }
    else if (map[y * w + x] < 0)
      map[y * w + x] = 0;
  }

  for (int i = 0; i < AreaN; i++)
  {
    a[i].x /= a[i].Square;
    a[i].y /= a[i].Square;
    a[i].Color[0] /= a[i].Square;
    a[i].Color[1] /= a[i].Square;
    a[i].Color[2] /= a[i].Square;
    if (abs(a[i].Color[2] - a[i].Color[1]) < 4 && abs(a[i].Color[1] - a[i].Color[0]) < 4 && abs(a[i].Color[0] - a[i].Color[2]) < 4)
      a[i].ColorType = 0;
    else
    {
      int mean = (max(a[i].Color[0], max(a[i].Color[1], a[i].Color[2])) + min(a[i].Color[0], min(a[i].Color[1], a[i].Color[2]))) / 2;
           if (a[i].Color[2] > mean && a[i].Color[1] > mean)
        a[i].ColorType = 1;
      else if (a[i].Color[2] > mean && a[i].Color[0] > mean)
        a[i].ColorType = 2;
      else if (a[i].Color[0] > a[i].Color[1] && a[i].Color[0] > a[i].Color[2])
        a[i].ColorType = 3;
      else if (a[i].Color[1] > a[i].Color[0] && a[i].Color[1] > a[i].Color[2])
        a[i].ColorType = 4;
      else if (a[i].Color[2] > a[i].Color[0] && a[i].Color[2] > a[i].Color[1])
        a[i].ColorType = 5;
    }
  }

  ProgressUpdate();

  for (int x = 0; x < w; x++) for (int y = 0; y < h; y++) if (map[y * w + x] != 0)
  {
    int n = map[y * w + x] - 1;
    a[n].m02 += (y-a[n].y)*(y-a[n].y);
    a[n].m20 += (x-a[n].x)*(x-a[n].x);
    a[n].m11 += (x-a[n].x)*(y-a[n].y);
  }

  for (int i = 0; i < AreaN; i++)
  {
    a[i].a = ArcTan2(2 * a[i].m11, a[i].m20 - a[i].m02) / 2;
    a[i].sina = sin(a[i].a);
    a[i].cosa = cos(a[i].a);
    a[i].Elongation = Elongation(a[i].m11, a[i].m02, a[i].m20);
    if (a[i].Elongation > 1 && a[i].Elongation < 4)
      a[i].Type = Elephant;
    else if (a[i].Elongation > 15)
      a[i].Type = Bamboo;
  }

  ProgressUpdate();

  for (int x = 0; x < w; x++) for (int y = 0; y < h; y++) if (map[y * w + x] != 0)
  {
    int n = map[y * w + x] - 1;
    float tmp;

    tmp = (x-a[n].x)*a[n].cosa+(y-a[n].y)*a[n].sina;
    if (tmp > a[n].hu)
      a[n].hu = tmp;
    if (tmp < a[n].hd)
      a[n].hd = tmp;

    tmp = (x-a[n].x)*a[n].sina-(y-a[n].y)*a[n].cosa;
    if (tmp > a[n].wu)
      a[n].wu = tmp;
    if (tmp < a[n].wd)
      a[n].wd = tmp;
  }

  delete[] map;

  for (int i = 0; i < AreaN; i++) if (a[i].Type == Bamboo)
  {
    int nearest = 2000000001;
    int nearestj = 0;
    for (int j = 0; j < AreaN; j++) if (a[j].Type == Elephant && (a[i].ColorType == a[j].ColorType || a[j].ColorType == 0))
    {
      int distance = sqr(a[i].x - a[j].x) + sqr(a[i].y - a[j].y);
      if (distance < nearest)
      {
        nearest = distance;
        nearestj = j;
      }
    }
    if (nearest < 2000000001)
    {
      a[i].Eater = nearestj;
      a[nearestj].Eaten++;
    }
  }

  for (int i = 0; i < AreaN; i++)
  {
    TPoint points[4];
    points[0] = Point(a[i].x + a[i].cosa * a[i].hu + a[i].sina * a[i].wu, h - a[i].y - a[i].sina * a[i].hu + a[i].cosa * a[i].wu);
    points[1] = Point(a[i].x + a[i].cosa * a[i].hd + a[i].sina * a[i].wu, h - a[i].y - a[i].sina * a[i].hd + a[i].cosa * a[i].wu);
    points[2] = Point(a[i].x + a[i].cosa * a[i].hd + a[i].sina * a[i].wd, h - a[i].y - a[i].sina * a[i].hd + a[i].cosa * a[i].wd);
    points[3] = Point(a[i].x + a[i].cosa * a[i].hu + a[i].sina * a[i].wd, h - a[i].y - a[i].sina * a[i].hu + a[i].cosa * a[i].wd);
    if (a[i].Type == Bamboo)
    {
      out_pImage->Canvas->Brush->Color = clTeal;
      out_pImage->Canvas->Brush->Style = bsFDiagonal;
      out_pImage->Canvas->Pen->Color = clTeal;
      out_pImage->Canvas->Pen->Width = 3;
      out_pImage->Canvas->Polygon(points, 3);
    }
    if (a[i].Type == Elephant)
    {
      out_pImage->Canvas->Brush->Color = clMaroon;
      out_pImage->Canvas->Brush->Style = bsBDiagonal;
      out_pImage->Canvas->Pen->Color = clMaroon;
      out_pImage->Canvas->Pen->Width = 3;
      out_pImage->Canvas->Polygon(points, 3);
    }
  }

  for (int i = 0; i < AreaN; i++) if (a[i].Type == Elephant)
  {
    int x = a[i].x;
    int y = a[i].y;
    out_pImage->Canvas->Pen->Color = clNavy;
    out_pImage->Canvas->Pen->Width = 3;
    out_pImage->Canvas->MoveTo(x - 6, h - y + 6); out_pImage->Canvas->LineTo(x + 6, h - y - 6);
    out_pImage->Canvas->MoveTo(x - 6, h - y - 6); out_pImage->Canvas->LineTo(x + 6, h - y + 6);
    for (int t = 0; t < a[i].Eaten; t++)
    {
      int nearest = 2000000001;
      int nearestj = 0;
      for (int j = 0; j < AreaN; j++) if (a[j].Type == Bamboo && a[j].Eater == i)
      {
        int distance = sqr(x - a[j].x) + sqr(y - a[j].y);
        if (distance < nearest)
        {
          nearest = distance;
          nearestj = j;
        }
      }
      if (nearest < 2000000001)
      {
        out_pImage->Canvas->Pen->Color = clNavy;
        out_pImage->Canvas->Pen->Width = 3;
        out_pImage->Canvas->MoveTo(x, h - y);
        x = a[nearestj].x;
        y = a[nearestj].y;
        out_pImage->Canvas->LineTo(x, h - y);
        out_pImage->Canvas->Pen->Color = clNavy;
        out_pImage->Canvas->Pen->Width = 3;
        out_pImage->Canvas->MoveTo(x - 6, h - y + 6); out_pImage->Canvas->LineTo(x + 6, h - y - 6);
        out_pImage->Canvas->MoveTo(x - 6, h - y - 6); out_pImage->Canvas->LineTo(x + 6, h - y + 6);
        a[nearestj].Eater = -1;
      }
    }
  }

  for (int i = 0; i < AreaN; i++) for (int j = 0; j < AreaN; j++)
    if (a[i].y * w + a[i].x > a[j].y * w + a[j].x)
    {
      Area t;
      t = a[j];
      a[j] = a[i];
      a[i] = t;
    }

  Results = "";
  int k = 0;
  for (int i = 0; i < AreaN; i++) if (a[i].Type == Elephant)
  {
    k++;
    if (a[i].Eaten == 1)
      Results += "Elephant " + IntToStr(k) + " ate 1  bamboo\n";
    else
      Results += "Elephant " + IntToStr(k) + " ate " + IntToStr(a[i].Eaten) + "  bamboos\n";
  }

  ProgressUpdate();

  delete[] a;

  return 0;
}
