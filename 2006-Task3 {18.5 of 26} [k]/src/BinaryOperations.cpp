//---------------------------------------------------------------------------


#pragma hdrstop

#include "BinaryOperations.h"
#include <math.h>
#include <algorithm>
#include "Progress.h"
using std::min;
using std::max;

#pragma package(smart_init)
//---------------------------------------------------------------------------

void Dilatation(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage, int R)
{
  if (R <= 0)
    return;

  int iBytesPerLine = (in_pImage->Width * 3 + 3) & -4;
  int w = in_pImage->Width;
  int h = in_pImage->Height;

  ProgressNextFilter(w);

  if (R > 500)
    R = 500;
  int R1[1001];
  for (int i = -R; i <= R; i++)
    R1[i + 500] = sqrt(R * R - i * i);

  in_pImage->PixelFormat = pf24bit;
  out_pImage->PixelFormat = pf24bit;

  unsigned char *pSrc = (unsigned char *)  in_pImage->ScanLine[ in_pImage->Height - 1];
  unsigned char *pDst = (unsigned char *) out_pImage->ScanLine[out_pImage->Height - 1];

  for (int x = 0; x < w; x++)
  {
    ProgressUpdate();
    for (int y = 0; y < h; y++) if (pSrc[y * iBytesPerLine + x * 3] == 255)
    {
      int x1 = max(0, x - R);
      int x2 = min(w - 1, x + R);
      for(int xn = x1; xn <= x2; xn++)
      {
        int y1 = max(0, y - R1[xn - x + 500]);
        int y2 = min(h - 1, y + R1[xn - x + 500]);
        for(int yn = y1; yn <= y2; yn++)
          pDst[yn * iBytesPerLine + xn * 3] = 255;
      }
    }
  }

  for (int x = 0; x < w; x++) for (int y = 0; y < h; y++)
    if (pDst[y * iBytesPerLine + x * 3] == 255 && pDst[y * iBytesPerLine + x * 3 + 1] != 255)
    {
      pDst[y * iBytesPerLine + x * 3]     = 255;
      pDst[y * iBytesPerLine + x * 3 + 1] = 255;
      pDst[y * iBytesPerLine + x * 3 + 2] = 255;
    }
}

void Erosion(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage, int R)
{
  if (R <= 0)
    return;

  int iBytesPerLine = (in_pImage->Width * 3 + 3) & -4;
  int w = in_pImage->Width;
  int h = in_pImage->Height;

  ProgressNextFilter(w);

  if (R > 500)
    R = 500;
  int R1[1001];
  for (int i = -R; i <= R; i++)
    R1[i + 500] = sqrt(R * R - i * i);

  in_pImage->PixelFormat = pf24bit;
  out_pImage->PixelFormat = pf24bit;

  unsigned char *pSrc = (unsigned char *)  in_pImage->ScanLine[ in_pImage->Height - 1];
  unsigned char *pDst = (unsigned char *) out_pImage->ScanLine[out_pImage->Height - 1];

  for (int x = 0; x < w; x++)
  {
    ProgressUpdate();
    for (int y = 0; y < h; y++) if (pSrc[y * iBytesPerLine + x * 3] == 0)
    {
      int x1 = max(0, x - R);
      int x2 = min(w - 1, x + R);
      for(int xn = x1; xn <= x2; xn++)
      {
        int y1 = max(0, y - R1[xn - x + 500]);
        int y2 = min(h - 1, y + R1[xn - x + 500]);
        for(int yn = y1; yn <= y2; yn++)
          pDst[yn * iBytesPerLine + xn * 3] = 0;
      }
    }
  }

  for (int x = 0; x < w; x++) for (int y = 0; y < h; y++)
    if (pDst[y * iBytesPerLine + x * 3] == 0 && pDst[y * iBytesPerLine + x * 3 + 1] != 0)
    {
      pDst[y * iBytesPerLine + x * 3]     = 0;
      pDst[y * iBytesPerLine + x * 3 + 1] = 0;
      pDst[y * iBytesPerLine + x * 3 + 2] = 0;
    }
}

void Opening(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage, int R)
{
  if (R == 0)
    return;

  Graphics::TBitmap *pTmpImage = new Graphics::TBitmap();
  Erosion(in_pImage, out_pImage, R);
  pTmpImage->Assign(out_pImage);
  Dilatation(pTmpImage, out_pImage, R);
  delete pTmpImage;
}

void Closing(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage, int R)
{
  if (R == 0)
    return;

  Graphics::TBitmap *pTmpImage = new Graphics::TBitmap();
  Dilatation(in_pImage, out_pImage, R);
  pTmpImage->Assign(out_pImage);
  Erosion(pTmpImage, out_pImage, R);
  delete pTmpImage;
}

