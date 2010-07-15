//---------------------------------------------------------------------------


#pragma hdrstop

#include "Binarization.h"
#include <algorithm>
#include "Progress.h"
using std::min;
using std::max;

#pragma package(smart_init)
//---------------------------------------------------------------------------



void Binarization(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage)
{
  ProgressNextFilter(100);

  int iBytesPerLine = (in_pImage->Width * 3 + 3) & -4;
  int w = in_pImage->Width;
  int h = in_pImage->Height;

  in_pImage->PixelFormat = pf24bit;
  out_pImage->PixelFormat = pf24bit;

  unsigned char *pSrc = (unsigned char *)  in_pImage->ScanLine[ in_pImage->Height - 1];
  unsigned char *pDst = (unsigned char *) out_pImage->ScanLine[out_pImage->Height - 1];

  unsigned char Color1[3], Color2[3];
  int NewColor1[3], NewColor2[3];
  int Color1Count, Color2Count;

  Color1[0] = 255;
  Color1[1] = 255;
  Color1[2] = 255;
  Color2[0] = 0;
  Color2[1] = 0;
  Color2[2] = 0;
  for (int x = 0; x < w; x++) for (int y = 0; y < h; y++)
  {
    Color1[0] = min( Color1[0], pSrc[y * iBytesPerLine + x * 3] );
    Color1[1] = min( Color1[1], pSrc[y * iBytesPerLine + x * 3 + 1] );
    Color1[2] = min( Color1[2], pSrc[y * iBytesPerLine + x * 3 + 2] );
    Color2[0] = max( Color2[0], pSrc[y * iBytesPerLine + x * 3] );
    Color2[1] = max( Color2[1], pSrc[y * iBytesPerLine + x * 3 + 1] );
    Color2[2] = max( Color2[2], pSrc[y * iBytesPerLine + x * 3 + 2] );
  }

  ProgressUpdateSet(10);

  while (true)
  {
    Color1Count = 0;
    Color2Count = 0;
    NewColor1[0] = 0;
    NewColor1[1] = 0;
    NewColor1[2] = 0;
    NewColor2[0] = 0;
    NewColor2[1] = 0;
    NewColor2[2] = 0;
    for (int x = 0; x < w; x++) for (int y = 0; y < h; y++)
    {
      if ( abs(pSrc[y * iBytesPerLine + x * 3]     - Color1[0]) +
           abs(pSrc[y * iBytesPerLine + x * 3 + 1] - Color1[1]) +
           abs(pSrc[y * iBytesPerLine + x * 3 + 2] - Color1[2]) >
           abs(pSrc[y * iBytesPerLine + x * 3]     - Color2[0]) +
           abs(pSrc[y * iBytesPerLine + x * 3 + 1] - Color2[1]) +
           abs(pSrc[y * iBytesPerLine + x * 3 + 2] - Color2[2]) )
      {
        NewColor2[0] += pSrc[y * iBytesPerLine + x * 3];
        NewColor2[1] += pSrc[y * iBytesPerLine + x * 3 + 1];
        NewColor2[2] += pSrc[y * iBytesPerLine + x * 3 + 2];
        Color2Count++;
      }
      else
      {
        NewColor1[0] += pSrc[y * iBytesPerLine + x * 3];
        NewColor1[1] += pSrc[y * iBytesPerLine + x * 3 + 1];
        NewColor1[2] += pSrc[y * iBytesPerLine + x * 3 + 2];
        Color1Count++;
      }
    }
    NewColor1[0] /= Color1Count;
    NewColor1[1] /= Color1Count;
    NewColor1[2] /= Color1Count;
    NewColor2[0] /= Color2Count;
    NewColor2[1] /= Color2Count;
    NewColor2[2] /= Color2Count;

    int Difference = abs(Color1[0] - NewColor1[0]) + abs(Color1[1] - NewColor1[1]) + abs(Color1[2] - NewColor1[2]) +
                     abs(Color2[0] - NewColor2[0]) + abs(Color2[1] - NewColor2[1]) + abs(Color2[2] - NewColor2[2]);
    ProgressUpdateSet(max(10,min(100 - 0.25*(Difference - 25),100)));
    if ( Difference < 25)
      {
      Color1[0]=NewColor1[0];
      Color1[1]=NewColor1[1];
      Color1[2]=NewColor1[2];
      Color2[0]=NewColor2[0];
      Color2[1]=NewColor2[1];
      Color2[2]=NewColor2[2];
      break;
      }
    Color1[0]=NewColor1[0];
    Color1[1]=NewColor1[1];
    Color1[2]=NewColor1[2];
    Color2[0]=NewColor2[0];
    Color2[1]=NewColor2[1];
    Color2[2]=NewColor2[2];
  }

  if (Color1Count < Color2Count)
  {
    for (int x = 0; x < w; x++) for (int y = 0; y < h; y++)
    {
      if ( abs(pSrc[y * iBytesPerLine + x * 3]     - Color1[0]) +
           abs(pSrc[y * iBytesPerLine + x * 3 + 1] - Color1[1]) +
           abs(pSrc[y * iBytesPerLine + x * 3 + 2] - Color1[2]) >
           abs(pSrc[y * iBytesPerLine + x * 3]     - Color2[0]) +
           abs(pSrc[y * iBytesPerLine + x * 3 + 1] - Color2[1]) +
           abs(pSrc[y * iBytesPerLine + x * 3 + 2] - Color2[2]) )
      {
        pDst[y * iBytesPerLine + x * 3]     = 0;
        pDst[y * iBytesPerLine + x * 3 + 1] = 0;
        pDst[y * iBytesPerLine + x * 3 + 2] = 0;
      }
      else
      {
        pDst[y * iBytesPerLine + x * 3]     = 255;
        pDst[y * iBytesPerLine + x * 3 + 1] = 255;
        pDst[y * iBytesPerLine + x * 3 + 2] = 255;
      }
    }
  }
  else
  {
    for (int x = 0; x < w; x++) for (int y = 0; y < h; y++)
    {
      if ( abs(pSrc[y * iBytesPerLine + x * 3]     - Color1[0]) +
           abs(pSrc[y * iBytesPerLine + x * 3 + 1] - Color1[1]) +
           abs(pSrc[y * iBytesPerLine + x * 3 + 2] - Color1[2]) >
           abs(pSrc[y * iBytesPerLine + x * 3]     - Color2[0]) +
           abs(pSrc[y * iBytesPerLine + x * 3 + 1] - Color2[1]) +
           abs(pSrc[y * iBytesPerLine + x * 3 + 2] - Color2[2]) )
      {
        pDst[y * iBytesPerLine + x * 3]     = 255;
        pDst[y * iBytesPerLine + x * 3 + 1] = 255;
        pDst[y * iBytesPerLine + x * 3 + 2] = 255;
      }
      else
      {
        pDst[y * iBytesPerLine + x * 3]     = 0;
        pDst[y * iBytesPerLine + x * 3 + 1] = 0;
        pDst[y * iBytesPerLine + x * 3 + 2] = 0;
      }
    }
  }
}

