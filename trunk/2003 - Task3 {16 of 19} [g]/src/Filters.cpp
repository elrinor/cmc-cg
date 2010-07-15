//---------------------------------------------------------------------------


#pragma hdrstop

#include "Filters.h"

#include <algorithm>
#include "math.hpp"
#include "math.h"
#define sqr(x) ((x)*(x))
#define round(x) (((x) - floor(x) > 0.5)?(ceil(x)):(floor(x)))

#define PI 3.141592653589793238462643383279

using std::min;
using std::max;

//---------------------------------------------------------------------------
void Binar(Graphics::TBitmap *Bmp1)
{
  Graphics::TBitmap *Bmp2 = new Graphics::TBitmap;
  Bmp2->Assign(Bmp1);

  int wb = (Bmp2->Width * 3 + 3) & -4; //считаем кол-во байт в строке изображения
  int w = Bmp2->Width;
  int h = Bmp2->Height;

  Bmp2->PixelFormat = pf24bit; //3 байта на один пиксель
  Bmp1->PixelFormat = pf24bit;

  //указатели на начало массива
  unsigned char *P2 = (unsigned char *) Bmp2->ScanLine[Bmp2->Height - 1];
  unsigned char *P1 = (unsigned char *) Bmp1->ScanLine[Bmp1->Height - 1];

  unsigned char Cl1[3], Cl2[3];
  int NewCl1[3], NewCl2[3];
  int Cl1Count, Cl2Count;

  Cl1[0] = 255; Cl1[1] = 255; Cl1[2] = 255; Cl2[0] = 0; Cl2[1] = 0; Cl2[2] = 0;
  for(int x = 0; x < w; x++) for (int y = 0; y < h; y++)
  {
    Cl1[0] = min(Cl1[0], P2[y * wb + x * 3]); Cl1[1] = min(Cl1[1], P2[y * wb + x * 3 + 1]); Cl1[2] = min(Cl1[2], P2[y * wb + x * 3 + 2]);
    Cl2[0] = max(Cl2[0], P2[y * wb + x * 3]); Cl2[1] = max(Cl2[1], P2[y * wb + x * 3 + 1]); Cl2[2] = max(Cl2[2], P2[y * wb + x * 3 + 2]);
  }

  while(true)
  {
    Cl1Count = 0; Cl2Count = 0;
    NewCl1[0] = 0; NewCl1[1] = 0; NewCl1[2] = 0; NewCl2[0] = 0; NewCl2[1] = 0; NewCl2[2] = 0;
    for(int x = 0; x < w; x++) for (int y = 0; y < h; y++)
    {
      if(abs(P2[y * wb + x * 3] - Cl1[0]) + abs(P2[y * wb + x * 3 + 1] - Cl1[1]) + abs(P2[y * wb + x * 3 + 2] - Cl1[2]) >
         abs(P2[y * wb + x * 3] - Cl2[0]) + abs(P2[y * wb + x * 3 + 1] - Cl2[1]) + abs(P2[y * wb + x * 3 + 2] - Cl2[2]))
      {
        NewCl2[0] += P2[y * wb + x * 3]; NewCl2[1] += P2[y * wb + x * 3 + 1]; NewCl2[2] += P2[y * wb + x * 3 + 2];
        Cl2Count++;
      }
      else
      {
        NewCl1[0] += P2[y * wb + x * 3]; NewCl1[1] += P2[y * wb + x * 3 + 1]; NewCl1[2] += P2[y * wb + x * 3 + 2];
        Cl1Count++;
      }
    }
    NewCl1[0] /= Cl1Count; NewCl1[1] /= Cl1Count; NewCl1[2] /= Cl1Count;
    NewCl2[0] /= Cl2Count; NewCl2[1] /= Cl2Count; NewCl2[2] /= Cl2Count;

    int Raznost = abs(Cl1[0] - NewCl1[0]) + abs(Cl1[1] - NewCl1[1]) + abs(Cl1[2] - NewCl1[2]) +
                  abs(Cl2[0] - NewCl2[0]) + abs(Cl2[1] - NewCl2[1]) + abs(Cl2[2] - NewCl2[2]);
    Cl1[0]=NewCl1[0]; Cl1[1]=NewCl1[1]; Cl1[2]=NewCl1[2];
    Cl2[0]=NewCl2[0]; Cl2[1]=NewCl2[1]; Cl2[2]=NewCl2[2];
    if(Raznost < 25)
      break;
  }

  for (int x = 0; x < w; x++) for (int y = 0; y < h; y++)
  {
    if(abs(P2[y * wb + x * 3] - Cl1[0]) + abs(P2[y * wb + x * 3 + 1] - Cl1[1]) + abs(P2[y * wb + x * 3 + 2] - Cl1[2]) >
       abs(P2[y * wb + x * 3] - Cl2[0]) + abs(P2[y * wb + x * 3 + 1] - Cl2[1]) + abs(P2[y * wb + x * 3 + 2] - Cl2[2]))
    {
      P1[y * wb + x * 3] = 255; P1[y * wb + x * 3 + 1] = 255; P1[y * wb + x * 3 + 2] = 255;
    }
    else
    {
      P1[y * wb + x * 3] = 0; P1[y * wb + x * 3 + 1] = 0; P1[y * wb + x * 3 + 2] = 0;
    }
  }

  delete Bmp2;
}


//---------------------------------------------------------------------------
void Szhatie(Graphics::TBitmap *Bmp1, int r)
{
  if (r <= 0)
    return;

  Graphics::TBitmap *Bmp2 = new Graphics::TBitmap;
  Bmp2->Assign(Bmp1);

  int wb = (Bmp2->Width * 3 + 3) & -4;
  int w = Bmp2->Width;
  int h = Bmp2->Height;

  Bmp2->PixelFormat = pf24bit;
  Bmp1->PixelFormat = pf24bit;

  unsigned char *P2 = (unsigned char *) Bmp2->ScanLine[Bmp2->Height - 1];
  unsigned char *P1 = (unsigned char *) Bmp1->ScanLine[Bmp1->Height - 1];

  for (int x = 0; x < w; x++)
  {
    for (int y = 0; y < h; y++) if (P1[y * wb + x * 3] == 255)
    {
      int xl = max(0, x - r);
      int xh = min(w - 1, x + r);
      for(int xn = xl; xn <= xh; xn++)
        P2[y * wb + xn * 3] = 255;
    }
  }

  Bmp1->Assign(Bmp2);
  P1 = (unsigned char *) Bmp1->ScanLine[Bmp1->Height - 1];

  for (int x = 0; x < w; x++)
  {
    for (int y = 0; y < h; y++) if (P2[y * wb + x * 3] == 255)
    {
      int yl = max(0, y - r);
      int yh = min(h - 1, y + r);
      for(int yn = yl; yn <= yh; yn++)
        P1[yn * wb + x * 3] = 255;
    }
  }

  for (int x = 0; x < w; x++) for (int y = 0; y < h; y++)
    if (P1[y * wb + x * 3] == 255)
    {
      P1[y * wb + x * 3] = 255; P1[y * wb + x * 3 + 1] = 255; P1[y * wb + x * 3 + 2] = 255;
    }

  delete Bmp2;
}

//---------------------------------------------------------------------------
void Rasshirenie(Graphics::TBitmap *Bmp1, int r)
{
  if (r <= 0)
    return;

  Graphics::TBitmap *Bmp2 = new Graphics::TBitmap;
  Bmp2->Assign(Bmp1);

  int wb = (Bmp2->Width * 3 + 3) & -4;
  int w = Bmp2->Width;
  int h = Bmp2->Height;

  Bmp2->PixelFormat = pf24bit;
  Bmp1->PixelFormat = pf24bit;

  unsigned char *P2 = (unsigned char *) Bmp2->ScanLine[Bmp2->Height - 1];
  unsigned char *P1 = (unsigned char *) Bmp1->ScanLine[Bmp1->Height - 1];

  for (int x = 0; x < w; x++)
  {
    for (int y = 0; y < h; y++) if (P1[y * wb + x * 3] == 0)
    {
      int xl = max(0, x - r);
      int xh = min(w - 1, x + r);
      for(int xn = xl; xn <= xh; xn++)
        P2[y * wb + xn * 3] = 0;
    }
  }

  Bmp1->Assign(Bmp2);
  P1 = (unsigned char *) Bmp1->ScanLine[Bmp1->Height - 1];

  for (int x = 0; x < w; x++)
  {
    for (int y = 0; y < h; y++) if (P2[y * wb + x * 3] == 0)
    {
      int yl = max(0, y - r);
      int yh = min(h - 1, y + r);
      for(int yn = yl; yn <= yh; yn++)
        P1[yn * wb + x * 3] = 0;
    }
  }

  for (int x = 0; x < w; x++) for (int y = 0; y < h; y++)
    if (P1[y * wb + x * 3] == 0)
    {
      P1[y * wb + x * 3] = 0; P1[y * wb + x * 3 + 1] = 0; P1[y * wb + x * 3 + 2] = 0;
    }

  delete Bmp2;
}

//---------------------------------------------------------------------------
void Otkritie(Graphics::TBitmap *Bmp1, int r)
{
  if (r == 0)
    return;
  Szhatie(Bmp1, r);
  Rasshirenie(Bmp1, r);
}

//---------------------------------------------------------------------------
void Zakritie(Graphics::TBitmap *Bmp1, int r)
{
  if (r == 0)
    return;
  Rasshirenie(Bmp1, r);
  Szhatie(Bmp1, r);
}


//---------------------------------------------------------------------------
void Median(Graphics::TBitmap *Bmp1, int r)
{
  if (r <= 0)
    return;

  Graphics::TBitmap *Bmp2 = new Graphics::TBitmap;
  Bmp2->Assign(Bmp1);

  int wb = (Bmp2->Width * 3 + 3) & -4;
  int w = Bmp2->Width;
  int h = Bmp2->Height;

  Bmp2->PixelFormat = pf24bit;
  Bmp1->PixelFormat = pf24bit;

  unsigned char *P2 = (unsigned char *) Bmp2->ScanLine[Bmp2->Height - 1];
  unsigned char *P1 = (unsigned char *) Bmp1->ScanLine[Bmp1->Height - 1];

  int Cnt[256];
  char Cls[256][3];
  unsigned char *Seryi = new unsigned char[w*h];

  for (int x = 0; x < w; x++) for (int y = 0; y < h; y++)
    Seryi[y * w + x] = (P2[y * wb + x * 3] + P2[y * wb + x * 3 + 1] + P2[y * wb + x * 3 + 2]) / 3;

  int i;
  for (int x = 0; x < w; x++)
  {
    for (int y = 0; y < h; y++)
    {
      int xl = max(0, x - r);
      int xh = min(w - 1, x + r);
      int yl = max(0, y - r);
      int yh = min(h - 1, y + r);
      memset(&Cnt, 0, sizeof(int) * 256);
      for(int xn = xl; xn <= xh; xn++) for(int yn = yl; yn <= yh; yn++)
      {
        i = Seryi[yn * w + xn];
        Cnt[i]++;
        if (Cnt[i] == 1)
        {
          Cls[i][0] = P2[yn * wb + xn * 3]; Cls[i][1] = P2[yn * wb + xn * 3 + 1]; Cls[i][2] = P2[yn * wb + xn * 3 + 2];
        }
      }
      int q = 0, maxq = (xh - xl + 1) * (yh - yl + 1) / 2;
      for (i = 0; i < 256; i++)
      {
        q += Cnt[i];
        if (q > maxq)
          break;
      }
      P1[y * wb + x * 3] = Cls[i][0]; P1[y * wb + x * 3 + 1] = Cls[i][1]; P1[y * wb + x * 3 + 2] = Cls[i][2];
    }
  }

  delete[] Seryi;
  delete Bmp2;
}



//---------------------------------------------------------------------------
struct TOblast
{
  int Type; // 1 - точка      2 - 12 часов      3 - стрелки
  float m11, m02, m20, Udlinennost;
  int ux, uy, dx, dy, lx, ly, rx, ry;
  int x, y, Ploshad;
};

//---------------------------------------------------------------------------
int ShagZalivki(int x, int y, int Target, int Filler, int w, int h, int* oblasti)
{
  int Result = 0;
  if (x == 0 || y == 0 || x == w - 1 || y == h - 1)
    return 0;

  for (int xn = x - 1; xn >= 0; xn--)
    if (oblasti[w * y + xn] == Target)
      oblasti[w * y + xn] = Filler;
    else
    {
      Result += x - 1 - xn;
      break;
    }

  for (int xn = x; xn < w; xn++)
    if (oblasti[w * y + xn] == Target)
      oblasti[w * y + xn] = Filler;
    else
    {
      Result += xn - x;
      break;
    }

  for (int xn = x - 1; xn >= 0; xn--)
  {
    if (oblasti[w * y + xn] == Filler)
    {
      if (oblasti[ w * (y - 1) + xn] == Target)
        Result += ShagZalivki(xn, y - 1, Target, Filler, w, h, oblasti);
      if (oblasti[ w * (y + 1) + xn] == Target)
        Result += ShagZalivki(xn, y + 1, Target, Filler, w, h, oblasti);
    }
    else
      break;
  }

  for (int xn = x; xn < w; xn++)
  {
    if (oblasti[w * y + xn] == Filler)
    {
      if (oblasti[ w * (y - 1) + xn] == Target)
        Result += ShagZalivki(xn, y - 1, Target, Filler, w, h, oblasti);
      if (oblasti[ w * (y + 1) + xn] == Target)
        Result += ShagZalivki(xn, y + 1, Target, Filler, w, h, oblasti);
    }
    else
      break;
  }

  return Result;
}


//---------------------------------------------------------------------------
int Zalivka(int x, int y, int oblastn, int w, int h, int* oblasti)
{
  if (ShagZalivki(x, y, -1, oblastn + 1, w, h, oblasti) > 80)
    return 1;
  else
  {
    ShagZalivki(x, y, oblastn + 1, 0, w, h, oblasti);
    return 0;
  }
}

//---------------------------------------------------------------------------
int FindClock(Graphics::TBitmap *Bmp, Graphics::TBitmap *Bin)
{
  int wb = (Bmp->Width * 3 + 3) & -4;
  int w = Bmp->Width;
  int h = Bmp->Height;

  Bin->PixelFormat = pf24bit;
  Bmp->PixelFormat = pf24bit;

  unsigned char *Pb = (unsigned char *) Bin->ScanLine[Bin->Height - 1];

  int *oblasti = new int[w * h];

  for (int x = 0; x < w; x++) for (int y = 0; y < h; y++)
  {
    if (Pb[y * wb + x * 3] == 0)
      oblasti[y * w + x] = -1;
    else
      oblasti[y * w + x] = 0;
  }

  int oblastn = 0;

  for (int x = 0; x < w; x++) for (int y = 0; y < h; y++)
    if (oblasti[y * w + x] == -1)
      oblastn += Zalivka(x, y, oblastn, w, h, oblasti);

  TOblast* o = new TOblast[oblastn];

  memset((void*) o, 0, oblastn * sizeof(TOblast));

  for (int x = 0; x < w; x++) for (int y = 0; y < h; y++)
  {
    if (oblasti[y * w + x] > 0)
    {
      int n = oblasti[y * w + x] - 1;
      o[n].x += x;
      o[n].y += y;
      o[n].Ploshad++;
    }
    else if(oblasti[y * w + x] < 0)
      oblasti[y * w + x] = 0;
  }

  for (int i = 0; i < oblastn; i++)
  {
    o[i].x /= o[i].Ploshad;
    o[i].y /= o[i].Ploshad;
    o[i].rx = -1000;
    o[i].lx = 99999999;
    o[i].uy = -1000;
    o[i].dy = 99999999;
  }

  for (int x = 0; x < w; x++) for (int y = 0; y < h; y++) if (oblasti[y * w + x] != 0)
  {
    int n = oblasti[y * w + x] - 1;
    o[n].m11 += (x - o[n].x) * (y - o[n].y);
    o[n].m02 += (y - o[n].y) * (y - o[n].y);
    o[n].m20 += (x - o[n].x) * (x - o[n].x);
    if (x > o[n].rx)
    {
      o[n].rx = x;
      o[n].ry = y;
    }
    if (x < o[n].lx)
    {
      o[n].lx = x;
      o[n].ly = y;
    }
    if (y > o[n].uy)
    {
      o[n].ux = x;
      o[n].uy = y;
    }
    if (y < o[n].dy)
    {
      o[n].dx = x;
      o[n].dy = y;
    }
  }

  int cx = 0, cy = 0, tn, sn, q = 0, r = 0;
  float angle12;

  for (int i = 0; i < oblastn; i++)
  {
    o[i].Udlinennost = (o[i].m02 + o[i].m20 + sqrt(sqr(o[i].m20 - o[i].m02) + 4 * sqr(o[i].m11))) / (o[i].m02 + o[i].m20 - sqrt(sqr(o[i].m20 - o[i].m02) + 4 * sqr(o[i].m11)));

    if (abs(o[i].Udlinennost - 1) < 0.5 && o[i].Ploshad < 2000 && o[i].Ploshad > 100)
      o[i].Type = 1;
    if (abs(o[i].Udlinennost - 3) < 1 && o[i].Ploshad < 2000 && o[i].Ploshad > 100)
    {
      o[i].Type = 2;
      tn = i;
    }
    if (abs(o[i].Udlinennost - 24) < 19 && o[i].Ploshad < 2000 && o[i].Ploshad > 100)
    {
      o[i].Type = 3;
      sn = i;
    }
    if (o[i].Type == 1 || o[i].Type == 2)
    {
      cx += o[i].x;
      cy += o[i].y;
      q++;
    }
  }

  cx /= q;
  cy /= q;

  for (int i = 0; i < oblastn; i++)
  {
    if(o[i].Type == 1 || o[i].Type == 2)
      r += sqrt(sqr(cx - o[i].x) + sqr(cy - o[i].y));
  }
  r /= q;

  Bmp->Canvas->Pen->Color = clBlue;
  Bmp->Canvas->Pen->Width = 3;
  Bmp->Canvas->Arc(cx - r, h - (cy + r), cx + r, h - (cy - r), cx - r, h - (cy + r), cx - r, h - (cy + r));

  angle12 = ArcTan2(o[tn].y - cy, o[tn].x - cx);
  Bmp->Canvas->MoveTo(cx + r * cos(angle12), h - (cy + r * sin(angle12)));
  Bmp->Canvas->LineTo(cx - r * cos(angle12), h - (cy - r * sin(angle12)));

  float rsx[4], rsy[4], rss[4];
  rsx[0] = o[sn].ux - cx;
  rsy[0] = o[sn].uy - cy;
  rsx[1] = o[sn].dx - cx;
  rsy[1] = o[sn].dy - cy;
  rsx[2] = o[sn].lx - cx;
  rsy[2] = o[sn].ly - cy;
  rsx[3] = o[sn].rx - cx;
  rsy[3] = o[sn].ry - cy;
  for(int i = 0; i < 4; i++)
    rss[i] = sqrt(sqr(rsx[i]) + sqr(rsy[i]));

  for(int i = 0; i < 4; i++) for (int j = i + 1; j < 4; j++) if (rss[j] > rss[i])
  {
    float t;
    t = rss[i]; rss[i] = rss[j]; rss[j] = t;
    t = rsx[i]; rsx[i] = rsx[j]; rsx[j] = t;
    t = rsy[i]; rsy[i] = rsy[j]; rsy[j] = t;
  }

  for(int i = 1; i < 4; i++)
    rss[i] /= rss[0];
  rss[0] = 1;

  float anglem = ArcTan2(rsy[0], rsx[0]);
  int nh = 1;
  while (rss[nh] > 0.7) nh++;
  float angleh = ArcTan2(rsy[nh], rsx[nh]);

  int timeh, timem;
  timeh = floor((angle12 - angleh) * 12 / (2 * PI));
  if (timeh < 0)
    timeh += 12;
  timem = round((angle12 - anglem) * 60 / (2 * PI));
  if (timem < 0)
    timem += 60;

  AnsiString time = ((timeh < 10)?("0"):("")) +  IntToStr(timeh) + ":" + ((timem < 10)?("0"):("")) + IntToStr(timem);

  Bmp->Canvas->Pen->Color = clGreen;
  Bmp->Canvas->MoveTo(cx + 0.6 * r * cos(angleh), h - (cy + 0.6 * r * sin(angleh)));
  Bmp->Canvas->LineTo(cx, h - cy);
  Bmp->Canvas->MoveTo(cx + r * cos(anglem), h - (cy + r * sin(anglem)));
  Bmp->Canvas->LineTo(cx, h - cy);

  Bmp->Canvas->Brush->Style = bsClear;
  Bmp->Canvas->Font->Size = 15;
  Bmp->Canvas->Font->Style = TFontStyles() << fsBold;
  Bmp->Canvas->Font->Name = "Arial";
  Bmp->Canvas->Font->Color = clRed;
  Bmp->Canvas->TextOutA(cx - Bmp->Canvas->TextWidth(time) / 2, h - (cy + r / 2), time);

  delete[] o;
  delete[] oblasti;

  return 0;
}

                                



#pragma package(smart_init)
