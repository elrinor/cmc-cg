#if C_GRAPHICSMEDIA_LAB_DMOROZ_C
#/*******************************************************************************
# * (C) Copyright 2003/2004 by Vladimir Vezhnevets <vvp@graphics.cs.msu.su>     *
# *******************************************************************************/
#endif

//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Processing.h"
#include "InProgress.h"
#include "SDIMain.h"
#include <math.h>

#pragma package(smart_init)

//---------------------------------------------------------------------------
// ѕростейший пример одномерной размывающей фильтрации изображени€ - усреднение трех соседних пикселей в строке


void BoxFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage)
{
  // ќдномерное усреднение по строкам
  for (int iY = 0; iY < in_pImage->Height; iY ++)
  {
    unsigned char *pcSrcLine, *pcDstLine;

    // Ѕыстрый способ обработки пикселей - пр€мой доступ к пам€ти
    pcSrcLine = (unsigned char *)in_pImage->ScanLine[iY];
    pcDstLine = (unsigned char *)out_pImage->ScanLine[iY];

    for (int iX = 1; iX < in_pImage->Width - 1; iX ++)
    {
      // blue
      pcDstLine[iX * 3] = (pcSrcLine[iX * 3 - 3] + pcSrcLine[iX * 3] + pcSrcLine[iX * 3 + 3]) / 3.0;
      // green
      pcDstLine[iX * 3 + 1] = (pcSrcLine[iX * 3 - 3 + 1] + pcSrcLine[iX * 3 + 1] + pcSrcLine[iX * 3 + 3 + 1]) / 3.0;
      // red
      pcDstLine[iX * 3 + 2] = (pcSrcLine[iX * 3 - 3 + 2] + pcSrcLine[iX * 3 + 2] + pcSrcLine[iX * 3 + 3 + 2]) / 3.0;
    }
  }
}

//---------------------------------------------------------------------------
// ѕростейший пример раскраски изображени€


void Colorize(Graphics::TBitmap *out_pImage)
{
  for (int iY = 0; iY < out_pImage->Height; iY ++)
  {
    unsigned char *pcSrcLine, *pcDstLine;

    // Ѕыстрый способ обработки пикселей - пр€мой доступ к пам€ти
    pcDstLine = (unsigned char *)out_pImage->ScanLine[iY];

    for (int iX = 0; iX < out_pImage->Width; iX ++)
    {
      /// ƒелаем плавно измен€ющийс€ вдоль строк градиент
      // blue
      pcDstLine[iX * 3] = iX % 256;
      // green
      pcDstLine[iX * 3 + 1] = 255 - iX % 256;
      // red
      pcDstLine[iX * 3 + 2] = abs(iX - out_pImage->Width / 2) % 256;
    }
  }
}


void AddWhiteNoise(Graphics::TBitmap *out_pImage, int Amplitude)
{
  Form3->ProgressBar1->Position = 0;
  Form3->ProgressBar1->Max = out_pImage->Height;
  Form3->Show();
  SDIAppForm->Enabled = false;

  for (int iY = 0; iY < out_pImage->Height; iY ++)
  {
    Form3->ProgressBar1->Position ++;
    Application->ProcessMessages();

    unsigned char *pcDstLine;

    pcDstLine = (unsigned char *)out_pImage->ScanLine[iY];

    for (int iX = 0; iX < out_pImage->Width; iX ++)
    {
      int Value = pcDstLine[iX * 3] + random(Amplitude) - random(Amplitude);
      if(Value < 0)
        Value = 0;
      if(Value > 255)
        Value = 255;
      pcDstLine[iX * 3] = Value;
      Value = pcDstLine[iX * 3 + 1] + random(Amplitude) - random(Amplitude);
      if(Value < 0)
        Value = 0;
      if(Value > 255)
        Value = 255;
      pcDstLine[iX * 3 + 1] = Value;
      Value = pcDstLine[iX * 3 + 2] + random(Amplitude) - random(Amplitude);
      if(Value < 0)
        Value = 0;
      if(Value > 255)
        Value = 255;
      pcDstLine[iX * 3 + 2] = Value;
    }
  }

  Form3->Hide();
  SDIAppForm->Enabled = true;
}

void AddImpulseNoise(Graphics::TBitmap *out_pImage, int Probability)
{
  Form3->ProgressBar1->Position = 0;
  Form3->ProgressBar1->Max = out_pImage->Height;
  Form3->Show();
  SDIAppForm->Enabled = false;

  for (int iY = 0; iY < out_pImage->Height; iY ++)
  {
    Form3->ProgressBar1->Position ++;
    Application->ProcessMessages();

    unsigned char *pcDstLine;

    pcDstLine = (unsigned char *)out_pImage->ScanLine[iY];

    for (int iX = 0; iX < out_pImage->Width; iX ++)
    {
      if(random(100)<Probability)
        {
        int Value = pcDstLine[iX * 3] + random(256) - random(256);
        if(Value < 0)
          Value = 0;
        if(Value > 255)
          Value = 255;
        pcDstLine[iX * 3] = Value;
        Value = pcDstLine[iX * 3 + 1] + random(256) - random(256);
        if(Value < 0)
          Value = 0;
        if(Value > 255)
          Value = 255;
        pcDstLine[iX * 3 + 1] = Value;
        Value = pcDstLine[iX * 3 + 2] + random(256) - random(256);
        if(Value < 0)
          Value = 0;
        if(Value > 255)
          Value = 255;
        pcDstLine[iX * 3 + 2] = Value;
        }
    }
  }

  Form3->Hide();
  SDIAppForm->Enabled = true;
}

void BlurFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage)
{
  Form3->ProgressBar1->Position = 0;
  Form3->ProgressBar1->Max = out_pImage->Height;
  Form3->Show();
  SDIAppForm->Enabled = false;

  unsigned char *pcSrcLine[3], *pcDstLine;
  pcSrcLine[0] = (unsigned char *)in_pImage->ScanLine[0];
  pcSrcLine[1] = (unsigned char *)in_pImage->ScanLine[1];
  pcSrcLine[2] = (unsigned char *)in_pImage->ScanLine[2];
  int iX, iY;

  for (iY = 1; iY < in_pImage->Height - 1; iY ++)
  {
    Form3->ProgressBar1->Position ++;
    Application->ProcessMessages();

    pcSrcLine[0] = pcSrcLine[1];
    pcSrcLine[1] = pcSrcLine[2];
    pcSrcLine[2] = (unsigned char *)in_pImage->ScanLine[iY + 1];
    pcDstLine = (unsigned char *)out_pImage->ScanLine[iY];
    for (iX = 1; iX < out_pImage->Width - 1; iX ++)
    {
      pcDstLine[iX * 3] =     (pcSrcLine[0][(iX - 1) * 3] + 2 * pcSrcLine[0][iX * 3] +     pcSrcLine[0][(iX + 1) * 3] +
                           2 * pcSrcLine[1][(iX - 1) * 3] + 3 * pcSrcLine[1][iX * 3] + 2 * pcSrcLine[1][(iX + 1) * 3] +
                               pcSrcLine[2][(iX - 1) * 3] + 2 * pcSrcLine[2][iX * 3] +     pcSrcLine[2][(iX + 1) * 3]) / 15;
      pcDstLine[iX * 3 + 1] = (pcSrcLine[0][(iX - 1) * 3 + 1] + 2 * pcSrcLine[0][iX * 3 + 1] +     pcSrcLine[0][(iX + 1) * 3 + 1] +
                           2 * pcSrcLine[1][(iX - 1) * 3 + 1] + 3 * pcSrcLine[1][iX * 3 + 1] + 2 * pcSrcLine[1][(iX + 1) * 3 + 1] +
                               pcSrcLine[2][(iX - 1) * 3 + 1] + 2 * pcSrcLine[2][iX * 3 + 1] +     pcSrcLine[2][(iX + 1) * 3 + 1]) / 15;
      pcDstLine[iX * 3 + 2] = (pcSrcLine[0][(iX - 1) * 3 + 2] + 2 * pcSrcLine[0][iX * 3 + 2] +     pcSrcLine[0][(iX + 1) * 3 + 2] +
                           2 * pcSrcLine[1][(iX - 1) * 3 + 2] + 3 * pcSrcLine[1][iX * 3 + 2] + 2 * pcSrcLine[1][(iX + 1) * 3 + 2] +
                               pcSrcLine[2][(iX - 1) * 3 + 2] + 2 * pcSrcLine[2][iX * 3 + 2] +     pcSrcLine[2][(iX + 1) * 3 + 2]) / 15;
    }

    pcDstLine[0] = (2 * pcSrcLine[0][0] +     pcSrcLine[0][3] +
                    3 * pcSrcLine[1][0] + 2 * pcSrcLine[1][3] +
                    2 * pcSrcLine[2][0] +     pcSrcLine[2][3]) / 11;
    pcDstLine[1] = (2 * pcSrcLine[0][1] +     pcSrcLine[0][4] +
                    3 * pcSrcLine[1][1] + 2 * pcSrcLine[1][4] +
                    2 * pcSrcLine[2][1] +     pcSrcLine[2][4]) / 11;
    pcDstLine[2] = (2 * pcSrcLine[0][2] +     pcSrcLine[0][5] +
                    3 * pcSrcLine[1][2] + 2 * pcSrcLine[1][5] +
                    2 * pcSrcLine[2][2] +     pcSrcLine[2][5]) / 11;
    iX = in_pImage->Width - 1;
    pcDstLine[iX * 3] =     (pcSrcLine[0][(iX - 1) * 3] + 2 * pcSrcLine[0][iX * 3] +
                         2 * pcSrcLine[1][(iX - 1) * 3] + 3 * pcSrcLine[1][iX * 3] +
                             pcSrcLine[2][(iX - 1) * 3] + 2 * pcSrcLine[2][iX * 3]) / 11;
    pcDstLine[iX * 3 + 1] = (pcSrcLine[0][(iX - 1) * 3 + 1] + 2 * pcSrcLine[0][iX * 3 + 1] +
                         2 * pcSrcLine[1][(iX - 1) * 3 + 1] + 3 * pcSrcLine[1][iX * 3 + 1] +
                             pcSrcLine[2][(iX - 1) * 3 + 1] + 2 * pcSrcLine[2][iX * 3 + 1]) / 11;
    pcDstLine[iX * 3 + 2] = (pcSrcLine[0][(iX - 1) * 3 + 2] + 2 * pcSrcLine[0][iX * 3 + 2] +
                         2 * pcSrcLine[1][(iX - 1) * 3 + 2] + 3 * pcSrcLine[1][iX * 3 + 2] +
                             pcSrcLine[2][(iX - 1) * 3 + 2] + 2 * pcSrcLine[2][iX * 3 + 2]) / 11;
  }

  Form3->ProgressBar1->Position ++;
  Application->ProcessMessages();
  pcSrcLine[1] = (unsigned char *)in_pImage->ScanLine[0];
  pcSrcLine[2] = (unsigned char *)in_pImage->ScanLine[1];
  pcDstLine = (unsigned char *)out_pImage->ScanLine[0];
  for (iX = 1; iX < out_pImage->Width - 1; iX ++)
  {
    pcDstLine[iX * 3] =     (2 * pcSrcLine[1][(iX - 1) * 3] + 3 * pcSrcLine[1][iX * 3] + 2 * pcSrcLine[1][(iX + 1) * 3] +
                                 pcSrcLine[2][(iX - 1) * 3] + 2 * pcSrcLine[2][iX * 3] +     pcSrcLine[2][(iX + 1) * 3]) / 11;
    pcDstLine[iX * 3 + 1] = (2 * pcSrcLine[1][(iX - 1) * 3 + 1] + 3 * pcSrcLine[1][iX * 3 + 1] + 2 * pcSrcLine[1][(iX + 1) * 3 + 1] +
                                 pcSrcLine[2][(iX - 1) * 3 + 1] + 2 * pcSrcLine[2][iX * 3 + 1] +     pcSrcLine[2][(iX + 1) * 3 + 1]) / 11;
    pcDstLine[iX * 3 + 2] = (2 * pcSrcLine[1][(iX - 1) * 3 + 2] + 3 * pcSrcLine[1][iX * 3 + 2] + 2 * pcSrcLine[1][(iX + 1) * 3 + 2] +
                                 pcSrcLine[2][(iX - 1) * 3 + 2] + 2 * pcSrcLine[2][iX * 3 + 2] +     pcSrcLine[2][(iX + 1) * 3 + 2]) / 11;
  }

  pcDstLine[0] = (3 * pcSrcLine[1][0] + 2 * pcSrcLine[1][3] +
                  2 * pcSrcLine[2][0] +     pcSrcLine[2][3]) / 8;
  pcDstLine[1] = (3 * pcSrcLine[1][1] + 2 * pcSrcLine[1][4] +
                  2 * pcSrcLine[2][1] +     pcSrcLine[2][4]) / 8;
  pcDstLine[2] = (3 * pcSrcLine[1][2] + 2 * pcSrcLine[1][5] +
                  2 * pcSrcLine[2][2] +     pcSrcLine[2][5]) / 8;
  iX = in_pImage->Width - 1;
  pcDstLine[iX * 3] =     (2 * pcSrcLine[1][(iX - 1) * 3] + 3 * pcSrcLine[1][iX * 3] +
                              pcSrcLine[2][(iX - 1) * 3] + 2 * pcSrcLine[2][iX * 3]) / 8;
  pcDstLine[iX * 3 + 1] = (2 * pcSrcLine[1][(iX - 1) * 3 + 1] + 3 * pcSrcLine[1][iX * 3 + 1] +
                               pcSrcLine[2][(iX - 1) * 3 + 1] + 2 * pcSrcLine[2][iX * 3 + 1]) / 8;
  pcDstLine[iX * 3 + 2] = (2 * pcSrcLine[1][(iX - 1) * 3 + 2] + 3 * pcSrcLine[1][iX * 3 + 2] +
                               pcSrcLine[2][(iX - 1) * 3 + 2] + 2 * pcSrcLine[2][iX * 3 + 2]) / 8;

  Form3->ProgressBar1->Position ++;
  Application->ProcessMessages();
  pcSrcLine[0] = (unsigned char *)in_pImage->ScanLine[in_pImage->Height - 2];
  pcSrcLine[1] = (unsigned char *)in_pImage->ScanLine[in_pImage->Height - 1];
  pcDstLine = (unsigned char *)out_pImage->ScanLine[out_pImage->Height - 1];
  for (iX = 1; iX < out_pImage->Width - 1; iX ++)
  {
    pcDstLine[iX * 3] =     (pcSrcLine[0][(iX - 1) * 3] + 2 * pcSrcLine[0][iX * 3] +     pcSrcLine[0][(iX + 1) * 3] +
                         2 * pcSrcLine[1][(iX - 1) * 3] + 3 * pcSrcLine[1][iX * 3] + 2 * pcSrcLine[1][(iX + 1) * 3]) / 11;
    pcDstLine[iX * 3 + 1] = (pcSrcLine[0][(iX - 1) * 3 + 1] + 2 * pcSrcLine[0][iX * 3 + 1] +     pcSrcLine[0][(iX + 1) * 3 + 1] +
                         2 * pcSrcLine[1][(iX - 1) * 3 + 1] + 3 * pcSrcLine[1][iX * 3 + 1] + 2 * pcSrcLine[1][(iX + 1) * 3 + 1]) / 11;
    pcDstLine[iX * 3 + 2] = (pcSrcLine[0][(iX - 1) * 3 + 2] + 2 * pcSrcLine[0][iX * 3 + 2] +     pcSrcLine[0][(iX + 1) * 3 + 2] +
                         2 * pcSrcLine[1][(iX - 1) * 3 + 2] + 3 * pcSrcLine[1][iX * 3 + 2] + 2 * pcSrcLine[1][(iX + 1) * 3 + 2]) / 11;
  }

  pcDstLine[0] = (2 * pcSrcLine[0][0] +     pcSrcLine[0][3] +
                  3 * pcSrcLine[1][0] + 2 * pcSrcLine[1][3]) / 8;
  pcDstLine[1] = (2 * pcSrcLine[0][1] +     pcSrcLine[0][4] +
                  3 * pcSrcLine[1][1] + 2 * pcSrcLine[1][4]) / 8;
  pcDstLine[2] = (2 * pcSrcLine[0][2] +     pcSrcLine[0][5] +
                  3 * pcSrcLine[1][2] + 2 * pcSrcLine[1][5]) / 8;
  iX = in_pImage->Width - 1;
  pcDstLine[iX * 3] =     (pcSrcLine[0][(iX - 1) * 3] + 2 * pcSrcLine[0][iX * 3] +
                       2 * pcSrcLine[1][(iX - 1) * 3] + 3 * pcSrcLine[1][iX * 3]) / 8;
  pcDstLine[iX * 3 + 1] = (pcSrcLine[0][(iX - 1) * 3 + 1] + 2 * pcSrcLine[0][iX * 3 + 1] +
                       2 * pcSrcLine[1][(iX - 1) * 3 + 1] + 3 * pcSrcLine[1][iX * 3 + 1]) / 8;
  pcDstLine[iX * 3 + 2] = (pcSrcLine[0][(iX - 1) * 3 + 2] + 2 * pcSrcLine[0][iX * 3 + 2] +
                       2 * pcSrcLine[1][(iX - 1) * 3 + 2] + 3 * pcSrcLine[1][iX * 3 + 2]) / 8;
                               
  Form3->Hide();
  SDIAppForm->Enabled = true;
}

void SharpenFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage)
{
  Form3->ProgressBar1->Position = 0;
  Form3->ProgressBar1->Max = out_pImage->Height;
  Form3->Show();
  SDIAppForm->Enabled = false;

  unsigned char *pcSrcLine[3], *pcDstLine;
  pcSrcLine[1] = (unsigned char *)in_pImage->ScanLine[0];
  pcSrcLine[2] = (unsigned char *)in_pImage->ScanLine[1];

  for (int iY = 0; iY < in_pImage->Height; iY ++)
  {
    Form3->ProgressBar1->Position ++;
    Application->ProcessMessages();

    pcSrcLine[0] = pcSrcLine[1];
    pcSrcLine[1] = pcSrcLine[2];
    if(iY + 1 < in_pImage->Height)
      pcSrcLine[2] = (unsigned char *)in_pImage->ScanLine[iY + 1];
    pcDstLine = (unsigned char *)out_pImage->ScanLine[iY];

    for (int iX = 0; iX < in_pImage->Width; iX ++)
    {
      int Divider = 0;
      int Color[3] = {0, 0, 0};
      if (iX > 0)
      {
        Color[0] += -2 * pcSrcLine[1][(iX - 1) * 3];
        Color[1] += -2 * pcSrcLine[1][(iX - 1) * 3 + 1];
        Color[2] += -2 * pcSrcLine[1][(iX - 1) * 3 + 2];
        Divider -= 2;
      }
      if (iY > 0)
      {
        Color[0] += -2 * pcSrcLine[0][iX * 3];
        Color[1] += -2 * pcSrcLine[0][iX * 3 + 1];
        Color[2] += -2 * pcSrcLine[0][iX * 3 + 2];
        Divider -= 2;
      }
      if (iX < in_pImage->Width - 1)
      {
        Color[0] += -2 * pcSrcLine[1][(iX + 1) * 3];
        Color[1] += -2 * pcSrcLine[1][(iX + 1) * 3 + 1];
        Color[2] += -2 * pcSrcLine[1][(iX + 1) * 3 + 2];
        Divider -= 2;
      }
      if (iY < in_pImage->Height - 1)
      {
        Color[0] += -2 * pcSrcLine[2][iX * 3];
        Color[1] += -2 * pcSrcLine[2][iX * 3 + 1];
        Color[2] += -2 * pcSrcLine[2][iX * 3 + 2];
        Divider -= 2;
      }
      if (iX > 0 && iY > 0)
      {
        Color[0] -= pcSrcLine[0][(iX - 1) * 3];
        Color[1] -= pcSrcLine[0][(iX - 1) * 3 + 1];
        Color[2] -= pcSrcLine[0][(iX - 1) * 3 + 2];
        Divider--;
      }
      if (iX > 0 && iY < in_pImage->Height - 1)
      {
        Color[0] -= pcSrcLine[2][(iX - 1) * 3];
        Color[1] -= pcSrcLine[2][(iX - 1) * 3 + 1];
        Color[2] -= pcSrcLine[2][(iX - 1) * 3 + 2];
        Divider--;
      }
      if (iX < in_pImage->Width - 1 && iY > 0)
      {
        Color[0] -= pcSrcLine[0][(iX + 1) * 3];
        Color[1] -= pcSrcLine[0][(iX + 1) * 3 + 1];
        Color[2] -= pcSrcLine[0][(iX + 1) * 3 + 2];
        Divider--;
      }
      if (iX < in_pImage->Width - 1 && iY < in_pImage->Height - 1)
      {
        Color[0] -= pcSrcLine[2][(iX + 1) * 3];
        Color[1] -= pcSrcLine[2][(iX + 1) * 3 + 1];
        Color[2] -= pcSrcLine[2][(iX + 1) * 3 + 2];
        Divider--;
      }
      Color[0] += 22 * pcSrcLine[1][iX * 3];
      Color[1] += 22 * pcSrcLine[1][iX * 3 + 1];
      Color[2] += 22 * pcSrcLine[1][iX * 3 + 2];
      Divider += 22;
      Color[0] /= Divider;
      Color[1] /= Divider;
      Color[2] /= Divider;
      if(Color[0] < 0)
        Color[0] = 0;
      if(Color[0] > 255)
        Color[0] = 255;
      if(Color[1] < 0)
        Color[1] = 0;
      if(Color[1] > 255)
        Color[1] = 255;
      if(Color[2] < 0)
        Color[2] = 0;
      if(Color[2] > 255)
        Color[2] = 255;
      pcDstLine[iX * 3]     = Color[0];
      pcDstLine[iX * 3 + 1] = Color[1];
      pcDstLine[iX * 3 + 2] = Color[2];
    }
  }

  Form3->Hide();
  SDIAppForm->Enabled = true;
}

void FindEdgesFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage)
{
  Form3->ProgressBar1->Position = 0;
  Form3->ProgressBar1->Max = out_pImage->Height;
  Form3->Show();
  SDIAppForm->Enabled = false;

  unsigned char *pcSrcLine[3], *pcDstLine;
  pcSrcLine[1] = (unsigned char *)in_pImage->ScanLine[0];
  pcSrcLine[2] = (unsigned char *)in_pImage->ScanLine[1];

  for (int iY = 0; iY < in_pImage->Height; iY ++)
  {
    Form3->ProgressBar1->Position ++;
    Application->ProcessMessages();

    pcSrcLine[0] = pcSrcLine[1];
    pcSrcLine[1] = pcSrcLine[2];
    if(iY + 1 < in_pImage->Height)
      pcSrcLine[2] = (unsigned char *)in_pImage->ScanLine[iY + 1];
    pcDstLine = (unsigned char *)out_pImage->ScanLine[iY];

    for (int iX = 0; iX < in_pImage->Width; iX ++)
    {
      int Divider = 0;
      int Color[3] = {0, 0, 0};
      if (iX > 0)
      {
        Color[0] -= pcSrcLine[1][(iX - 1) * 3];
        Color[1] -= pcSrcLine[1][(iX - 1) * 3 + 1];
        Color[2] -= pcSrcLine[1][(iX - 1) * 3 + 2];
        Divider++;
      }
      if (iY > 0)
      {
        Color[0] -= pcSrcLine[0][iX * 3];
        Color[1] -= pcSrcLine[0][iX * 3 + 1];
        Color[2] -= pcSrcLine[0][iX * 3 + 2];
        Divider++;
      }
      if (iX < in_pImage->Width - 1)
      {
        Color[0] -= pcSrcLine[1][(iX + 1) * 3];
        Color[1] -= pcSrcLine[1][(iX + 1) * 3 + 1];
        Color[2] -= pcSrcLine[1][(iX + 1) * 3 + 2];
        Divider++;
      }
      if (iY < in_pImage->Height - 1)
      {
        Color[0] -= pcSrcLine[2][iX * 3];
        Color[1] -= pcSrcLine[2][iX * 3 + 1];
        Color[2] -= pcSrcLine[2][iX * 3 + 2];
        Divider++;
      }
      Color[0] += Divider * pcSrcLine[1][iX * 3];
      Color[1] += Divider * pcSrcLine[1][iX * 3 + 1];
      Color[2] += Divider * pcSrcLine[1][iX * 3 + 2];
      if(Color[0] < 0)
        Color[0] = 0;
      if(Color[0] > 255)
        Color[0] = 255;
      if(Color[1] < 0)
        Color[1] = 0;
      if(Color[1] > 255)
        Color[1] = 255;
      if(Color[2] < 0)
        Color[2] = 0;
      if(Color[2] > 255)
        Color[2] = 255;
      pcDstLine[iX * 3]     = Color[0];
      pcDstLine[iX * 3 + 1] = Color[1];
      pcDstLine[iX * 3 + 2] = Color[2];
    }
  }

  Form3->Hide();
  SDIAppForm->Enabled = true;
}

void _2dGaussianBlurFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage, float h)
{
  Form3->ProgressBar1->Position = 0;
  Form3->ProgressBar1->Max = out_pImage->Height;
  Form3->Show();
  SDIAppForm->Enabled = false;

  int r = h * 2.7;
  if (r < 1)
    r = 1;
  if (r > 100)
    r = 100;

  int iBytesPerLine = (in_pImage->Width * 3 + 3) & -4;

  unsigned char *pcSrcLine, *pcDstLine;
  pcSrcLine = (unsigned char *) in_pImage->ScanLine[ in_pImage->Height - 1];
  pcDstLine = (unsigned char *)out_pImage->ScanLine[out_pImage->Height - 1];

  float M[201][201];
  for (int iX = -r; iX <= r; iX ++)
    for (int iY = -r; iY <= r; iY ++)
      M[100 + iX][100 + iY] = exp(-(iX * iX + iY * iY) / (2 * h * h));
  float S = 0;
  for (int iX = -r; iX <= r; iX ++)
    for (int iY = -r; iY <= r; iY++)
      S += M[100 + iX][100 + iY];
  for (int iX = -r; iX <= r; iX ++)
    for (int iY = -r; iY <= r; iY++)
      M[100 + iX][100 + iY] /= S;

  for (int iY = 0; iY < in_pImage->Height; iY ++)
  {
    Form3->ProgressBar1->Position ++;
    Application->ProcessMessages();

    for (int iX = 0; iX < in_pImage->Width; iX ++)
    {
      float Color[3] = {0, 0, 0};
      for (int dX = -r; dX <= r; dX ++)
        for (int dY = -r; dY <= r; dY ++)
        {
          int nX = iX + dX;
          int nY = iY + dY;
          if (nX < 0)
            nX = 0;
          if (nY < 0)
            nY = 0;
          if (nX > in_pImage->Width - 1)
            nX = in_pImage->Width - 1;
          if (nY > in_pImage->Height - 1)
            nY = in_pImage->Height - 1;
          Color[0] += M[100 + dX][100 + dY] * pcSrcLine[iBytesPerLine * nY + nX * 3];
          Color[1] += M[100 + dX][100 + dY] * pcSrcLine[iBytesPerLine * nY + nX * 3 + 1];
          Color[2] += M[100 + dX][100 + dY] * pcSrcLine[iBytesPerLine * nY + nX * 3 + 2];
        }
      pcDstLine[iBytesPerLine * iY + iX * 3]     = Color[0];
      pcDstLine[iBytesPerLine * iY + iX * 3 + 1] = Color[1];
      pcDstLine[iBytesPerLine * iY + iX * 3 + 2] = Color[2];
    }
  }

  Form3->Hide();
  SDIAppForm->Enabled = true;
}

void _1dGaussianBlurFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage, float h)
{
  Form3->ProgressBar1->Position = 0;
  Form3->ProgressBar1->Max = out_pImage->Height * 2;
  Form3->Show();
  SDIAppForm->Enabled = false;

  int r = h * 2.7;
  if (r < 1)
    r = 1;
  if (r > 100)
    r = 100;  

  int iBytesPerLine = (in_pImage->Width * 3 + 3) & -4;

  float M[201];
  for (int iX = -r; iX <= r; iX ++)
    M[100 + iX] = exp(-(iX * iX) / (2 * h * h));
  float S = 0;
  for (int iX = -r; iX <= r; iX ++)
    S += M[100 + iX];
  for (int iX = -r; iX <= r; iX ++)
    M[100 + iX] /= S;

  Graphics::TBitmap *pTempImage = new Graphics::TBitmap();
  pTempImage->Assign(in_pImage);
    
  unsigned char *pcSrcLine, *pcDstLine;
  pcSrcLine = (unsigned char *) in_pImage->ScanLine[ in_pImage->Height - 1];
  pcDstLine = (unsigned char *)pTempImage->ScanLine[pTempImage->Height - 1];

  for (int iY = 0; iY < in_pImage->Height; iY ++)
  {
    Form3->ProgressBar1->Position ++;
    Application->ProcessMessages();

    for (int iX = 0; iX < in_pImage->Width; iX ++)
    {
      float Color[3] = {0, 0, 0};
      for (int dX = -r; dX <= r; dX++)
      {
        int nX = iX + dX;
        if (nX < 0)
          nX = 0;
        if (nX > in_pImage->Width - 1)
          nX = in_pImage->Width - 1;
        Color[0] += M[100 + dX] * pcSrcLine[iBytesPerLine * iY + nX * 3];
        Color[1] += M[100 + dX] * pcSrcLine[iBytesPerLine * iY + nX * 3 + 1];
        Color[2] += M[100 + dX] * pcSrcLine[iBytesPerLine * iY + nX * 3 + 2];
      }
      pcDstLine[iBytesPerLine * iY + iX * 3]     = Color[0];
      pcDstLine[iBytesPerLine * iY + iX * 3 + 1] = Color[1];
      pcDstLine[iBytesPerLine * iY + iX * 3 + 2] = Color[2];
    }
  }

  pcSrcLine = (unsigned char *)pTempImage->ScanLine[pTempImage->Height - 1];
  pcDstLine = (unsigned char *)out_pImage->ScanLine[out_pImage->Height - 1];

  for (int iY = 0; iY < in_pImage->Height; iY ++)
  {
    Form3->ProgressBar1->Position ++;
    Application->ProcessMessages();

    for (int iX = 0; iX < in_pImage->Width; iX ++)
    {
      float Color[3] = {0, 0, 0};
      for (int dY = -r; dY <= r; dY++)
      {
        int nY = iY + dY;
        if (nY < 0)
          nY = 0;
        if (nY > in_pImage->Height - 1)
          nY = in_pImage->Height - 1;
        Color[0] += M[100 + dY] * pcSrcLine[iBytesPerLine * nY + iX * 3];
        Color[1] += M[100 + dY] * pcSrcLine[iBytesPerLine * nY + iX * 3 + 1];
        Color[2] += M[100 + dY] * pcSrcLine[iBytesPerLine * nY + iX * 3 + 2];
      }
      pcDstLine[iBytesPerLine * iY + iX * 3]     = Color[0];
      pcDstLine[iBytesPerLine * iY + iX * 3 + 1] = Color[1];
      pcDstLine[iBytesPerLine * iY + iX * 3 + 2] = Color[2];
    }
  }

  delete pTempImage;

  Form3->Hide();
  SDIAppForm->Enabled = true;
}

void MedianFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage, int r)
{
  Form3->ProgressBar1->Position = 0;
  Form3->ProgressBar1->Max = out_pImage->Height;
  Form3->Show();
  SDIAppForm->Enabled = false;

  int iBytesPerLine = (in_pImage->Width * 3 + 3) & -4;

  unsigned char *pcSrcLine, *pcDstLine;
  pcSrcLine = (unsigned char *) in_pImage->ScanLine[ in_pImage->Height - 1];
  pcDstLine = (unsigned char *)out_pImage->ScanLine[out_pImage->Height - 1];

  int Counts[256];
  char Colors[256][3];

  for (int iY = 0; iY < in_pImage->Height; iY ++)
  {
    Form3->ProgressBar1->Position ++;
    Application->ProcessMessages();

    for (int iX = 0; iX < in_pImage->Width; iX ++)
    {
      memset(&Counts, 0, 1024); 
      for (int dX = -r; dX <= r; dX ++)
      {
        for (int dY = -r; dY <= r; dY ++)
        {
          int nX = iX + dX;
          int nY = iY + dY;
          if (nX < 0)
            nX = 0;
          if (nY < 0)
            nY = 0;
          if (nX > in_pImage->Width - 1)
            nX = in_pImage->Width - 1;
          if (nY > in_pImage->Height - 1)
            nY = in_pImage->Height - 1;
          int index = 0.114 * pcSrcLine[iBytesPerLine * nY + nX * 3] +
                      0.587 * pcSrcLine[iBytesPerLine * nY + nX * 3 + 1] +
                      0.299 * pcSrcLine[iBytesPerLine * nY + nX * 3 + 2];
          Counts[index] ++;
          Colors[index][0] = pcSrcLine[iBytesPerLine * nY + nX * 3];
          Colors[index][1] = pcSrcLine[iBytesPerLine * nY + nX * 3 + 1];
          Colors[index][2] = pcSrcLine[iBytesPerLine * nY + nX * 3 + 2];
        }
      }
      int quantity = 0;
      int index;
      for (index = 0; index < 256; index ++)
      {
        quantity += Counts[index];
        if (quantity >= (2 * r + 1) * (2 * r + 1) / 2)
          break;
      }
      pcDstLine[iBytesPerLine * iY + iX * 3]     = Colors[index][0];
      pcDstLine[iBytesPerLine * iY + iX * 3 + 1] = Colors[index][1];
      pcDstLine[iBytesPerLine * iY + iX * 3 + 2] = Colors[index][2];
    }
  }

  Form3->Hide();
  SDIAppForm->Enabled = true;
}

void AdvancedMedianFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage, int r)
{
  Form3->ProgressBar1->Position = 0;
  Form3->ProgressBar1->Max = out_pImage->Width;
  Form3->Show();
  SDIAppForm->Enabled = false;

  unsigned char Colors[10000][3];

  if (r > 49)
    r = 49;

  int iBytesPerLine = (in_pImage->Width * 3 + 3) & -4;

  unsigned char *pcSrcLine, *pcDstLine;
  pcSrcLine = (unsigned char *) in_pImage->ScanLine[ in_pImage->Height - 1];
  pcDstLine = (unsigned char *)out_pImage->ScanLine[out_pImage->Height - 1];

  for (int iX = 0; iX < in_pImage->Width; iX ++)
  {
    Form3->ProgressBar1->Position ++;
    Application->ProcessMessages();

    for (int iY = 0; iY < in_pImage->Height; iY ++)
    {
      int Count = 0;
      int nXl = iX - r;
      int nXh = iX + r;
      int nYl = iY - r;
      int nYh = iY + r;
      if (nXl < 0)
        nXl = 0;
      if (nYl < 0)
        nYl = 0;
      if (nXh > in_pImage->Width - 1)
        nXh = in_pImage->Width - 1;
      if (nYh > in_pImage->Height - 1)
        nYh = in_pImage->Height - 1;

      for (int nX = nXl; nX <= nXh; nX ++)
      {
        for (int nY = nYl; nY <= nYh; nY ++)
        {
          Colors[Count][0] = pcSrcLine[iBytesPerLine * nY + nX * 3];
          Colors[Count][1] = pcSrcLine[iBytesPerLine * nY + nX * 3 + 1];
          Colors[Count][2] = pcSrcLine[iBytesPerLine * nY + nX * 3 + 2];
          Count ++;
        }
      }
  		int mindist = 2000000000;
	  	int index;
		  for(int i=0; i<Count; i++)
  		{
	  		int dist=0;
		  	for(int j=0; j<Count; j++)
        {
  				dist+=abs(Colors[i][0]-Colors[j][0])+abs(Colors[i][1]-Colors[j][1])+abs(Colors[i][2]-Colors[j][2]);
          if (dist > mindist)
            break;
        }
  			if(dist < mindist)
	  		{
		  		mindist = dist;
			  	index = i;
  			}
	  	}
      pcDstLine[iBytesPerLine * iY + iX * 3]     = Colors[index][0];
      pcDstLine[iBytesPerLine * iY + iX * 3 + 1] = Colors[index][1];
      pcDstLine[iBytesPerLine * iY + iX * 3 + 2] = Colors[index][2];
    }
  }

  Form3->Hide();
  SDIAppForm->Enabled = true;
}

void KNNFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage, int r, float h)
{
  Form3->ProgressBar1->Position = 0;
  Form3->ProgressBar1->Max = out_pImage->Height;
  Form3->Show();
  SDIAppForm->Enabled = false;

  int iBytesPerLine = (in_pImage->Width * 3 + 3) & -4;

  unsigned char *pcSrcLine, *pcDstLine;
  pcSrcLine = (unsigned char *) in_pImage->ScanLine[ in_pImage->Height - 1];
  pcDstLine = (unsigned char *)out_pImage->ScanLine[out_pImage->Height - 1];

  for (int iY = 0; iY < in_pImage->Height; iY ++)
  {
    Form3->ProgressBar1->Position ++;
    Application->ProcessMessages();

    for (int iX = 0; iX < in_pImage->Width; iX ++)
    {
      int nXl = iX - r;
      int nXh = iX + r;
      int nYl = iY - r;
      int nYh = iY + r;
      if (nXl < 0)
        nXl = 0;
      if (nYl < 0)
        nYl = 0;
      if (nXh > in_pImage->Width - 1)
        nXh = in_pImage->Width - 1;
      if (nYh > in_pImage->Height - 1)
        nYh = in_pImage->Height - 1;

      float Color[3] = {0, 0, 0};
      float Divider = 0;

      for (int nX = nXl; nX <= nXh; nX ++)
      {
        for (int nY = nYl; nY <= nYh; nY ++)
        {
          float K = abs(pcSrcLine[iBytesPerLine * nY + nX * 3]     - pcSrcLine[iBytesPerLine * iY + iX * 3]) +
                    abs(pcSrcLine[iBytesPerLine * nY + nX * 3 + 1] - pcSrcLine[iBytesPerLine * iY + iX * 3 + 1]) +
                    abs(pcSrcLine[iBytesPerLine * nY + nX * 3 + 2] - pcSrcLine[iBytesPerLine * iY + iX * 3 + 2]);
          K = exp(-(K * K) / (6 * h * h));
          Divider += K;
          Color[0] += K * pcSrcLine[iBytesPerLine * nY + nX * 3];
          Color[1] += K * pcSrcLine[iBytesPerLine * nY + nX * 3 + 1];
          Color[2] += K * pcSrcLine[iBytesPerLine * nY + nX * 3 + 2];
        }
      }

      Color[0] /= Divider;
      Color[1] /= Divider;
      Color[2] /= Divider;

      pcDstLine[iBytesPerLine * iY + iX * 3]     = Color[0];
      pcDstLine[iBytesPerLine * iY + iX * 3 + 1] = Color[1];
      pcDstLine[iBytesPerLine * iY + iX * 3 + 2] = Color[2];
    }
  }

  Form3->Hide();
  SDIAppForm->Enabled = true;
}

unsigned int SAD8x8_SSE(const byte *pSrc, const byte *pDst, unsigned int uWidth)
{
	unsigned int uSum;
	__asm
	{
		mov		esi, pSrc
		mov		edi, pDst
		mov		ebx, uWidth
		mov		edx, ebx
		shl		edx, 1


		movq	mm0, [esi]
		movq	mm1, [esi+ebx]
		add		esi, edx
		movq	mm2, [esi]
		movq	mm3, [esi+ebx]
		add		esi, edx
		movq	mm4, [esi]
		movq	mm5, [esi+ebx]
		add		esi, edx
		movq	mm6, [esi]
		movq	mm7, [esi+ebx]


		psadbw	mm0, [edi]
		psadbw	mm1, [edi+ebx]
		add		edi, edx
		psadbw	mm2, [edi]
		psadbw	mm3, [edi+ebx]
		add		edi, edx
		psadbw	mm4, [edi]
		psadbw	mm5, [edi+ebx]
		add		edi, edx
		psadbw	mm6, [edi]
		psadbw	mm7, [edi+ebx]


		paddusw	mm0, mm1
		paddusw	mm0, mm2
		paddusw	mm0, mm3
		paddusw	mm0, mm4
		paddusw	mm0, mm5
		paddusw	mm0, mm6
		paddusw	mm0, mm7

		movd	uSum, mm0
		emms
		mov		eax, uSum	
	}
}

void NLMFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage, int r, float h)
{
  Form3->ProgressBar1->Position = 0;
  Form3->ProgressBar1->Max = out_pImage->Height;
  Form3->Show();
  SDIAppForm->Enabled = false;

  int iBytesPerLine = (in_pImage->Width * 3 + 3) & -4;

  unsigned char *pcSrcLine, *pcDstLine;
  pcSrcLine = (unsigned char *) in_pImage->ScanLine[ in_pImage->Height - 1];
  pcDstLine = (unsigned char *)out_pImage->ScanLine[out_pImage->Height - 1];

  unsigned char *red, *green, *blue;
  red = new unsigned char[(in_pImage->Height + 8) * (in_pImage->Width + 8)];
  green = new unsigned char[(in_pImage->Height + 8) * (in_pImage->Width + 8)];
  blue = new unsigned char[(in_pImage->Height + 8) * (in_pImage->Width + 8)];

  int layerwidth = in_pImage->Width + 8;

  for (int iY = 0; iY < in_pImage->Height + 8; iY ++)
  {
    for (int iX = 0; iX < in_pImage->Width + 8; iX ++)
    {
      int nX = iX - 4;
      int nY = iY - 4;
      if (nX < 0)
        nX = 0;
      if (nY < 0)
        nY = 0;
      if (nX > in_pImage->Width - 1)
        nX = in_pImage->Width - 1;
      if (nY > in_pImage->Height - 1)
        nY = in_pImage->Height - 1;
        red[layerwidth * iY + iX] = pcSrcLine[iBytesPerLine * nY + nX * 3 + 2];
      green[layerwidth * iY + iX] = pcSrcLine[iBytesPerLine * nY + nX * 3 + 1];
       blue[layerwidth * iY + iX] = pcSrcLine[iBytesPerLine * nY + nX * 3];
    }
  }

  for (int iY = 0; iY < in_pImage->Height; iY ++)
  {
    Form3->ProgressBar1->Position ++;
    Application->ProcessMessages();

    for (int iX = 0; iX < in_pImage->Width; iX ++)
    {
      int nXl = iX - r;
      int nXh = iX + r;
      int nYl = iY - r;
      int nYh = iY + r;
      if (nXl < 0)
        nXl = 0;
      if (nYl < 0)
        nYl = 0;
      if (nXh > in_pImage->Width - 1)
        nXh = in_pImage->Width - 1;
      if (nYh > in_pImage->Height - 1)
        nYh = in_pImage->Height - 1;

      float Color[3] = {0, 0, 0};
      float Divider[3] = {0, 0, 0};

      for (int nX = nXl; nX <= nXh; nX ++)
      {
        for (int nY = nYl; nY <= nYh; nY ++)
        {
          float Kr = SAD8x8_SSE(  &red[layerwidth * nY + nX],   &red[layerwidth * iY + iX], layerwidth) / 64.0f;
          float Kg = SAD8x8_SSE(&green[layerwidth * nY + nX], &green[layerwidth * iY + iX], layerwidth) / 64.0f;
          float Kb = SAD8x8_SSE( &blue[layerwidth * nY + nX],  &blue[layerwidth * iY + iX], layerwidth) / 64.0f;
          float K = (Kr + Kb + Kg) / 3;
          K = exp(-2 * (K * K) /  (h * h));
          /*Kr = exp(-2 * (Kr * Kr) / (h * h));
          Kg = exp(-2 * (Kg * Kg) / (h * h));
          Kb = exp(-2 * (Kb * Kb) / (h * h));*/
          Divider[0] += K;
          Divider[1] += K;
          Divider[2] += K;
          Color[0] += K * pcSrcLine[iBytesPerLine * nY + nX * 3];
          Color[1] += K * pcSrcLine[iBytesPerLine * nY + nX * 3 + 1];
          Color[2] += K * pcSrcLine[iBytesPerLine * nY + nX * 3 + 2];
        }
      }

      Color[0] /= Divider[0];
      Color[1] /= Divider[1];
      Color[2] /= Divider[2];

      pcDstLine[iBytesPerLine * iY + iX * 3]     = Color[0];
      pcDstLine[iBytesPerLine * iY + iX * 3 + 1] = Color[1];
      pcDstLine[iBytesPerLine * iY + iX * 3 + 2] = Color[2];
    }
  }

  delete[] red;
  delete[] green;
  delete[] blue;

  Form3->Hide();
  SDIAppForm->Enabled = true;
}

void AutomaticNLMFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage)
{
  FindEdgesFilter(in_pImage, out_pImage);

  Form3->ProgressBar1->Position = 0;
  Form3->ProgressBar1->Max = out_pImage->Height;
  Form3->Show();
  SDIAppForm->Enabled = false;

  int Counts[256];
  memset(&Counts, 0, 1024);

  int iBytesPerLine = (in_pImage->Width * 3 + 3) & -4;

  unsigned char *pcSrcLine, *pcDstLine;
  pcSrcLine = (unsigned char *)out_pImage->ScanLine[ in_pImage->Height - 1];

  for (int iY = 0; iY < in_pImage->Height; iY ++)
  {
    Form3->ProgressBar1->Position ++;
    Application->ProcessMessages();

    for (int iX = 0; iX < in_pImage->Width; iX ++)
      Counts[(int)(0.114 * pcSrcLine[iBytesPerLine * iY + iX * 3] +
                   0.587 * pcSrcLine[iBytesPerLine * iY + iX * 3 + 1] +
                   0.299 * pcSrcLine[iBytesPerLine * iY + iX * 3 + 2])] ++;
  }

  int quantity = 0;
  int Noise;
  int neededquantity = (in_pImage->Height * in_pImage->Width) / 2;
  for (Noise = 0; Noise < 256; Noise ++)
  {
    quantity += Counts[Noise];
    if (quantity >= neededquantity)
      break;
  }

  Form3->Hide();
  SDIAppForm->Enabled = true;

  int r = Noise / 3;
  if (r > 8)
    r = 8;
  float h = Noise;
  if (h > 80)
    h = 80;
  if (Noise > 0)
    NLMFilter(in_pImage, out_pImage, r, h);
}

void AutomaticKNNFilter(Graphics::TBitmap *in_pImage, Graphics::TBitmap *out_pImage)
{
  FindEdgesFilter(in_pImage, out_pImage);

  Form3->ProgressBar1->Position = 0;
  Form3->ProgressBar1->Max = out_pImage->Height;
  Form3->Show();
  SDIAppForm->Enabled = false;

  int Counts[256];
  memset(&Counts, 0, 1024);

  int iBytesPerLine = (in_pImage->Width * 3 + 3) & -4;

  unsigned char *pcSrcLine, *pcDstLine;
  pcSrcLine = (unsigned char *)out_pImage->ScanLine[ in_pImage->Height - 1];

  for (int iY = 0; iY < in_pImage->Height; iY ++)
  {
    Form3->ProgressBar1->Position ++;
    Application->ProcessMessages();

    for (int iX = 0; iX < in_pImage->Width; iX ++)
      Counts[(int)(0.114 * pcSrcLine[iBytesPerLine * iY + iX * 3] +
                   0.587 * pcSrcLine[iBytesPerLine * iY + iX * 3 + 1] +
                   0.299 * pcSrcLine[iBytesPerLine * iY + iX * 3 + 2])] ++;
  }

  int quantity = 0;
  int Noise;
  int neededquantity = (in_pImage->Height * in_pImage->Width) / 2;
  for (Noise = 0; Noise < 256; Noise ++)
  {
    quantity += Counts[Noise];
    if (quantity >= neededquantity)
      break;
  }

  Form3->Hide();
  SDIAppForm->Enabled = true;

  int r = Noise / 3;
  if (r > 8)
    r = 8;
  float h = Noise * 1.8f;
  if (h > 80)
    h = 80;
  if (Noise > 0)
    KNNFilter(in_pImage, out_pImage, r, h);
}
