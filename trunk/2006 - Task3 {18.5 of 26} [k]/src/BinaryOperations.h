//---------------------------------------------------------------------------

#ifndef BinaryOperationsH
#define BinaryOperationsH

#include <Graphics.hpp>
//---------------------------------------------------------------------------

void Dilatation(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage, int R);
void Erosion(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage, int R);
void Opening(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage, int R);
void Closing(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage, int R);
#endif
