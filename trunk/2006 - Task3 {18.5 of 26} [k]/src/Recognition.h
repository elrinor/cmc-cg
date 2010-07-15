//---------------------------------------------------------------------------

#ifndef RecognitionH
#define RecognitionH
#include <vcl.h>
//---------------------------------------------------------------------------

int Recognition(Graphics::TBitmap *rgb_pImage, Graphics::TBitmap *bin_pImage, Graphics::TBitmap *out_pImage, int SquareThreshold);
String LastRecognitionResult();

#endif
