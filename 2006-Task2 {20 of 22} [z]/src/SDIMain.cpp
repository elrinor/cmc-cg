#if C_GRAPHICSMEDIA_LAB_DMOROZ_C
#/*******************************************************************************
# * (C) Copyright 2003/2004 by Vladimir Vezhnevets <vvp@graphics.cs.msu.su>     *
# *******************************************************************************/
#endif

//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "SDIMain.h"
#include "Processing.h"
#include "Dialog1.h"
#include "Dialog2.h"

//---------------------------------------------------------------------
#pragma resource "*.dfm"
TSDIAppForm *SDIAppForm;
//---------------------------------------------------------------------
__fastcall TSDIAppForm::TSDIAppForm(TComponent *AOwner)
	: TForm(AOwner)
{
}
//---------------------------------------------------------------------


void __fastcall TSDIAppForm::FileOpen1Execute(TObject *Sender)
{
  if (OpenDialog->Execute())
  {
    try
    {
      DocImage->Picture->LoadFromFile(OpenDialog->FileName);
    }
    catch (EInvalidGraphic &Ex)
    {
      /// Загрузить не удалось
      ShowMessage("Ошибка загрузки изображения"); 
    }
  }
}
//---------------------------------------------------------------------------

void __fastcall TSDIAppForm::FileSave1Execute(TObject *Sender)
{
  /// Вызов диалога сохранения 
  if (SaveDialog->Execute())
  {
    DocImage->Picture->SaveToFile(SaveDialog->FileName);
  }
}
//---------------------------------------------------------------------------


void __fastcall TSDIAppForm::FileExit1Execute(TObject *Sender)
{
  Close();        
}
//---------------------------------------------------------------------------



void __fastcall TSDIAppForm::WhiteNoise1Click(TObject *Sender)
{
  if(DocImage->Picture->Bitmap->Empty)
    return;
  Form1->Label1->Caption = "Noise Amplitude:";
  if(Form1->ShowModal() == mrOk)
  {
    int Amplitude = StrToInt(Form1->Edit1->Text);
    ::AddWhiteNoise(DocImage->Picture->Bitmap, Amplitude);
    DocImage->Repaint();
  }
}
//---------------------------------------------------------------------------

void __fastcall TSDIAppForm::ImpulseNoise1Click(TObject *Sender)
{
  if(DocImage->Picture->Bitmap->Empty)
    return;
  Form1->Label1->Caption = "Noise Probability:";
  if(Form1->ShowModal() == mrOk)
  {
    int Probability = StrToInt(Form1->Edit1->Text);
    ::AddImpulseNoise(DocImage->Picture->Bitmap, Probability);
    DocImage->Repaint();
  }
}
//---------------------------------------------------------------------------

void __fastcall TSDIAppForm::BlurFilter1Click(TObject *Sender)
{
  if(DocImage->Picture->Bitmap->Empty)
    return;
  Graphics::TBitmap *pTempImage = new Graphics::TBitmap();
  pTempImage->Assign(DocImage->Picture->Bitmap);
  ::BlurFilter(pTempImage, DocImage->Picture->Bitmap);
  delete pTempImage;
  DocImage->Repaint();
}
//---------------------------------------------------------------------------

void __fastcall TSDIAppForm::SharpenFilter1Click(TObject *Sender)
{
  if(DocImage->Picture->Bitmap->Empty)
    return;
  Graphics::TBitmap *pTempImage = new Graphics::TBitmap();
  pTempImage->Assign(DocImage->Picture->Bitmap);
  ::SharpenFilter(pTempImage, DocImage->Picture->Bitmap);
  delete pTempImage;
  DocImage->Repaint();
}
//---------------------------------------------------------------------------

void __fastcall TSDIAppForm::FindEdgesFilter1Click(TObject *Sender)
{
  if(DocImage->Picture->Bitmap->Empty)
    return;
  Graphics::TBitmap *pTempImage = new Graphics::TBitmap();
  pTempImage->Assign(DocImage->Picture->Bitmap);
  ::FindEdgesFilter(pTempImage, DocImage->Picture->Bitmap);
  delete pTempImage;
  DocImage->Repaint();
}
//---------------------------------------------------------------------------


void __fastcall TSDIAppForm::N2DimensionalGaussianBlur1Click(
      TObject *Sender)
{
  if(DocImage->Picture->Bitmap->Empty)
    return;
  Form1->Label1->Caption = "Gaussian Blur Radius:";
  if(Form1->ShowModal() == mrOk)
  {
    float Radius = StrToFloat(Form1->Edit1->Text);
    Graphics::TBitmap *pTempImage = new Graphics::TBitmap();
    pTempImage->Assign(DocImage->Picture->Bitmap);
    ::_2dGaussianBlurFilter(pTempImage, DocImage->Picture->Bitmap, Radius);
    delete pTempImage;
    DocImage->Repaint();
  }
}
//---------------------------------------------------------------------------

void __fastcall TSDIAppForm::N1DimensionalGaussianBlur1Click(
      TObject *Sender)
{
  if(DocImage->Picture->Bitmap->Empty)
    return;
  Form1->Label1->Caption = "Gaussian Blur Radius:";
  if(Form1->ShowModal() == mrOk)
  {
    float Radius = StrToFloat(Form1->Edit1->Text);
    Graphics::TBitmap *pTempImage = new Graphics::TBitmap();
    pTempImage->Assign(DocImage->Picture->Bitmap);
    ::_1dGaussianBlurFilter(pTempImage, DocImage->Picture->Bitmap, Radius);
    delete pTempImage;
    DocImage->Repaint();
  }
}
//---------------------------------------------------------------------------

void __fastcall TSDIAppForm::MedianFilter1Click(TObject *Sender)
{
  if(DocImage->Picture->Bitmap->Empty)
    return;
  Form1->Label1->Caption = "Median Radius:";
  if(Form1->ShowModal() == mrOk)
  {
    int Radius = StrToInt(Form1->Edit1->Text);
    Graphics::TBitmap *pTempImage = new Graphics::TBitmap();
    pTempImage->Assign(DocImage->Picture->Bitmap);
    ::MedianFilter(pTempImage, DocImage->Picture->Bitmap, Radius);
    delete pTempImage;
    DocImage->Repaint();
  }
}
//---------------------------------------------------------------------------

void __fastcall TSDIAppForm::MedianFilterByL1VectorDistance1Click(
      TObject *Sender)
{
  if(DocImage->Picture->Bitmap->Empty)
    return;
  Form1->Label1->Caption = "Median Radius:";
  if(Form1->ShowModal() == mrOk)
  {
    int Radius = StrToInt(Form1->Edit1->Text);
    Graphics::TBitmap *pTempImage = new Graphics::TBitmap();
    pTempImage->Assign(DocImage->Picture->Bitmap);
    ::AdvancedMedianFilter(pTempImage, DocImage->Picture->Bitmap, Radius);
    delete pTempImage;
    DocImage->Repaint();
  }
}
//---------------------------------------------------------------------------

void __fastcall TSDIAppForm::KNearestNeighborsFilter1Click(TObject *Sender)
{
  if(DocImage->Picture->Bitmap->Empty)
    return;
  Form2->Label1->Caption = "KNN Radius:";
  Form2->Label2->Caption = "KNN Noise Level:";
  if(Form2->ShowModal() == mrOk)
  {
    int Radius = StrToInt(Form2->Edit1->Text);
    float Noise = StrToFloat(Form2->Edit2->Text);
    Graphics::TBitmap *pTempImage = new Graphics::TBitmap();
    pTempImage->Assign(DocImage->Picture->Bitmap);
    ::KNNFilter(pTempImage, DocImage->Picture->Bitmap, Radius, Noise);
    delete pTempImage;
    DocImage->Repaint();
  }
}
//---------------------------------------------------------------------------

void __fastcall TSDIAppForm::NonLocalMeansFilter1Click(TObject *Sender)
{
  if(DocImage->Picture->Bitmap->Empty)
    return;
  Form2->Label1->Caption = "NLM Radius:";
  Form2->Label2->Caption = "NLM Noise Level:";
  if(Form2->ShowModal() == mrOk)
  {
    int Radius = StrToInt(Form2->Edit1->Text);
    float Noise = StrToFloat(Form2->Edit2->Text);
    Graphics::TBitmap *pTempImage = new Graphics::TBitmap();
    pTempImage->Assign(DocImage->Picture->Bitmap);
    ::NLMFilter(pTempImage, DocImage->Picture->Bitmap, Radius, Noise);
    delete pTempImage;
    DocImage->Repaint();
  }
}
//---------------------------------------------------------------------------

void __fastcall TSDIAppForm::Automatic1Click(TObject *Sender)
{
  if(DocImage->Picture->Bitmap->Empty)
    return;
  Graphics::TBitmap *pTempImage = new Graphics::TBitmap();
  pTempImage->Assign(DocImage->Picture->Bitmap);
  ::AutomaticNLMFilter(pTempImage, DocImage->Picture->Bitmap);
  delete pTempImage;
  DocImage->Repaint();
}
//---------------------------------------------------------------------------

void __fastcall TSDIAppForm::AutomaticNonLocalMeansFilter1Click(
      TObject *Sender)
{
  if(DocImage->Picture->Bitmap->Empty)
    return;
  Graphics::TBitmap *pTempImage = new Graphics::TBitmap();
  pTempImage->Assign(DocImage->Picture->Bitmap);
  ::AutomaticKNNFilter(pTempImage, DocImage->Picture->Bitmap);
  delete pTempImage;
  DocImage->Repaint();
}
//---------------------------------------------------------------------------

