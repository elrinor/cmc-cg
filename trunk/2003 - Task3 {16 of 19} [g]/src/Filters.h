//---------------------------------------------------------------------------

#ifndef FiltersH
#define FiltersH

#include <Graphics.hpp>
//---------------------------------------------------------------------------

void Binar(Graphics::TBitmap *Bmp1);

void Otkritie(Graphics::TBitmap *Bmp1, int r);
void Zakritie(Graphics::TBitmap *Bmp1, int r);
void Rasshirenie(Graphics::TBitmap *Bmp1, int r);
void Szhatie(Graphics::TBitmap *Bmp1, int r);
void Median(Graphics::TBitmap *Bmp1, int r);

int FindClock(Graphics::TBitmap *Bmp, Graphics::TBitmap *Bin);

#endif
