#ifndef ProcessingH
#define ProcessingH
#include <string>
#include <afxwin.h>
using namespace std;


class DSimpleBitmap;

void MedianNormal(DSimpleBitmap *bmp2, int r);
void Contrast(DSimpleBitmap *img2);
void Binarization(DSimpleBitmap *img2);
void BinaryDilation(DSimpleBitmap *img, int radius);
void BinaryErosion(DSimpleBitmap *img, int radius);
void BinaryClose(DSimpleBitmap *img, int radius);
void BinaryOpen(DSimpleBitmap *img, int radius);
void RemovePoints(DSimpleBitmap *img);
void Scan(DSimpleBitmap *img, DSimpleBitmap *bin, string *Report, int mins);

#endif
