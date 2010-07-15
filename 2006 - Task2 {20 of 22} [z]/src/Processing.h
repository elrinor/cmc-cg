#if C_GRAPHICSMEDIA_LAB_DMOROZ_C
#/*******************************************************************************
# * (C) Copyright 2003/2004 by Vladimir Vezhnevets <vvp@graphics.cs.msu.su>     *
# *******************************************************************************/
#endif
//---------------------------------------------------------------------------

#ifndef ProcessingH
#define ProcessingH

void AddWhiteNoise(Graphics::TBitmap *out_pImage, int Amplitude);
void AddImpulseNoise(Graphics::TBitmap *out_pImage, int Probability);
void BlurFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage);
void SharpenFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage);
void FindEdgesFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage);
void _2dGaussianBlurFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage, float h);
void _1dGaussianBlurFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage, float h);
void MedianFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage, int r);
void AdvancedMedianFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage, int r);
void KNNFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage, int r, float h);
void NLMFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage, int r, float h);
void AutomaticNLMFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage);
void AutomaticKNNFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage);

//---------------------------------------------------------------------------
#endif
