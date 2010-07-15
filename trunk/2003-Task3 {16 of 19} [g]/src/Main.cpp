#include <vcl.h>
#pragma hdrstop

#include "Filters.h"
#include "Main.h"
#include "dlg.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Open1Click(TObject *Sender)
{
  if(OpenDialog1->Execute())
  {
   Image1->Picture->LoadFromFile(OpenDialog1->FileName);
   Image1->Width = Image1->Picture->Width;
   Image1->Height = Image1->Picture->Height;
  }
}
//---------------------------------------------------------------------------



void __fastcall TForm1::Binarize1Click(TObject *Sender)
{
  Binar(Image1->Picture->Bitmap);
  Image1->Refresh();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::N2Click(TObject *Sender)
{
  Rasshirenie(Image1->Picture->Bitmap, GetR());
  Image1->Refresh();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::N3Click(TObject *Sender)
{
  Szhatie(Image1->Picture->Bitmap, GetR());
  Image1->Refresh();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::N4Click(TObject *Sender)
{
  Otkritie(Image1->Picture->Bitmap, GetR());
  Image1->Refresh();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::N5Click(TObject *Sender)
{
  Zakritie(Image1->Picture->Bitmap, GetR());
  Image1->Refresh();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::N6Click(TObject *Sender)
{
  Median(Image1->Picture->Bitmap, GetR());
  Image1->Refresh();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::N8Click(TObject *Sender)
{
  FindClock(Image1->Picture->Bitmap, Image1->Picture->Bitmap);
  Image1->Refresh();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Simple1Click(TObject *Sender)
{
  Graphics::TBitmap *bin = new Graphics::TBitmap;
  bin->Assign(Image1->Picture->Bitmap);
  Binar(bin);
  FindClock(Image1->Picture->Bitmap, bin);
  delete bin;
  Image1->Refresh();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Noisy21311Click(TObject *Sender)
{
  Graphics::TBitmap *bin = new Graphics::TBitmap;
  bin->Assign(Image1->Picture->Bitmap);
  Median(bin, 1);
  Binar(bin);
  Median(bin, 1);
  Zakritie(bin, 3);
  Median(bin, 1);
  FindClock(Image1->Picture->Bitmap, bin);
  delete bin;
  Image1->Refresh();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::SimpleNoisy11Click(TObject *Sender)
{
  Graphics::TBitmap *bin = new Graphics::TBitmap;
  bin->Assign(Image1->Picture->Bitmap);
  Median(bin, 2);
  Binar(bin);
  Median(bin, 1);
  Median(bin, 1);
  Median(bin, 1);
  Median(bin, 1);
  Median(bin, 2);
  Median(bin, 2);
  Median(bin, 2);
  Zakritie(bin, 3);
  FindClock(Image1->Picture->Bitmap, bin);
  delete bin;
  Image1->Refresh();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Noisy1Click(TObject *Sender)
{
  Graphics::TBitmap *bin = new Graphics::TBitmap;
  bin->Assign(Image1->Picture->Bitmap);
  Binar(bin);
  Szhatie(bin, 1);
  Zakritie(bin, 3);
  Rasshirenie(bin, 2);
  Median(bin, 3);
  Median(bin, 3);
  Median(bin, 3);
  FindClock(Image1->Picture->Bitmap, bin);
  delete bin;
  Image1->Refresh();
}
//---------------------------------------------------------------------------


