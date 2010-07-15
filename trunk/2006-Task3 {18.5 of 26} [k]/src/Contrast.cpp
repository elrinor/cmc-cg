//---------------------------------------------------------------------------


#pragma hdrstop

#include "Contrast.h"
#include <algorithm>
#include "Progress.h"
using std::min;
using std::max;

//---------------------------------------------------------------------------

#pragma package(smart_init)

void Contrast(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage)
{
  int iBytesPerLine = (in_pImage->Width * 3 + 3) & -4;
  int w = in_pImage->Width;
  int h = in_pImage->Height;

  ProgressNextFilter(w + 2);

  in_pImage->PixelFormat = pf24bit;
  out_pImage->PixelFormat = pf24bit;

  unsigned char *pSrc = (unsigned char *)  in_pImage->ScanLine[ in_pImage->Height - 1];
  unsigned char *pDst = (unsigned char *) out_pImage->ScanLine[out_pImage->Height - 1];

  int N[256];
  memset(&N, 0, sizeof(int) * 256);

  for (int x = 0; x < w; x++) for (int y = 0; y < h; y++)
    N[(pSrc[y * iBytesPerLine + x * 3] + pSrc[y * iBytesPerLine + x * 3 + 1] + pSrc[y * iBytesPerLine + x * 3 + 2]) / 3]++;

  ProgressUpdate();

  int k = 0, maxk = w * h / 25;
  int ming, maxg;
  for (ming = 0; ming < 256; ming++)
  {
    k += N[ming];
    if (k > maxk)
      break;
  }
  k = 0;
  for (maxg = 255; maxg >= 0; maxg--)
  {
    k += N[maxg];
    if (k > maxk)
      break;
  }

  ProgressUpdate();  

  for (int x = 0; x < w; x++)
  {
    ProgressUpdate();
    for (int y = 0; y < h; y++)
    {
      pDst[y * iBytesPerLine + x * 3]     = max(0, min(255, (pSrc[y * iBytesPerLine + x * 3]     - ming) * 256 / (maxg - ming + 1)));
      pDst[y * iBytesPerLine + x * 3 + 1] = max(0, min(255, (pSrc[y * iBytesPerLine + x * 3 + 1] - ming) * 256 / (maxg - ming + 1)));
      pDst[y * iBytesPerLine + x * 3 + 2] = max(0, min(255, (pSrc[y * iBytesPerLine + x * 3 + 2] - ming) * 256 / (maxg - ming + 1)));
    }
  }
}
