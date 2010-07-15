#ifndef ProcessingH
#define ProcessingH

class DSimpleBitmap;

void WhiteNoise(DSimpleBitmap *bmp, int Noise);
void ImpulseNoise(DSimpleBitmap *bmp, int Probability);
void Sharpen(DSimpleBitmap *bmp);
void Blur(DSimpleBitmap *bmp);
void FindEdges(DSimpleBitmap *bmp);
void GaussianBlur1D(DSimpleBitmap *bmp, float h);
void GaussianBlur2D(DSimpleBitmap *bmp, float h);
void MedianNormal(DSimpleBitmap *bmp2, int r);
void MedianVector(DSimpleBitmap *bmp2, int r);
void KNN(DSimpleBitmap *bmp2, int r, float h);
void NLM(DSimpleBitmap *bmp2, int r, float h);
void KNNAutomatic(DSimpleBitmap *bmp2);
void NLMAutomatic(DSimpleBitmap *bmp2);


#endif
