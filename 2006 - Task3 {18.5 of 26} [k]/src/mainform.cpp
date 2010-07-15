//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "mainform.h"
#include "Processing.h"
#include "RequestIntDlg.h"
#include "Progress.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "Unit1"
#pragma link "Frame"
#pragma link "pies"
#pragma link "CGAUGES"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::OpenBtnClick(TObject *Sender)
{
  /// LOAD IMAGE
  TabControl->SetFocus();
  if (OpenDialog->Execute())
  {
    try
    {
      Graphics::TBitmap *pTempImage = new Graphics::TBitmap();
      pTempImage->LoadFromFile(OpenDialog->FileName);
      AddBitmap(pTempImage, ExtractFileName(OpenDialog->FileName),false);
    }
    catch (...)
    {
      ShowMessage("Ошибка загрузки изображения");
    }
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormCreate(TObject *Sender)
{
  TabControl->Tabs->Clear();
  BitmapCount = 0;
  CreateNewWindows = false;
  Image = Frame->Image;
  InProgress = false;
  RePaint();
}
//---------------------------------------------------------------------------

void TForm1::ButtonStateUpdate()
{
  if (TabControl->TabIndex != -1 && !InProgress)
  {
    PopupMenu1->Items->Items[0]->Enabled = true;
    SaveBtn->Enabled = true;
    SmpBtn->Enabled = true;
    Med12Btn->Enabled = true;
    Med3Btn->Enabled = true;
    if (Binary[TabControl->Tabs->Strings[TabControl->TabIndex]])
    {
      BinBtn->Enabled = false;
      DilBtn->Enabled = true;
      EroBtn->Enabled = true;
      ClosBtn->Enabled = true;
      OpenBtn->Enabled = true;
      ContBtn->Enabled = false;
    }
    else
    {
      BinBtn->Enabled = true;
      DilBtn->Enabled = false;
      EroBtn->Enabled = false;
      ClosBtn->Enabled = false;
      OpenBtn->Enabled = false;
      ContBtn->Enabled = true;
    }
    if (Bitmaps[RGBImage->Text] == NULL || Bitmaps[BinImage->Text] == NULL)
      RecognBtn->Enabled = false;
    else if (Bitmaps[RGBImage->Text]->Width != Bitmaps[BinImage->Text]->Width || Bitmaps[RGBImage->Text]->Height != Bitmaps[BinImage->Text]->Height)
      RecognBtn->Enabled = false;
    else
      RecognBtn->Enabled = true;
  }
  else
  {
    PopupMenu1->Items->Items[0]->Enabled = false;
    BinBtn->Enabled = false;
    DilBtn->Enabled = false;
    EroBtn->Enabled = false;
    ClosBtn->Enabled = false;
    OpenBtn->Enabled = false;
    RecognBtn->Enabled = false;
    SaveBtn->Enabled = false;
    ContBtn->Enabled = false;
    SmpBtn->Enabled = false;
    Med12Btn->Enabled = false;
    Med3Btn->Enabled = false;
  }
}

void TForm1::RePaint()
{
  ButtonStateUpdate();
  if (TabControl->TabIndex != -1)
  {
    Image->Picture->Bitmap = Bitmaps[TabControl->Tabs->Strings[TabControl->TabIndex]];
    Image->Height = Image->Picture->Bitmap->Height;
    Image->Width = Image->Picture->Bitmap->Width;
  }
  else
  {
    Image->Width = 0;
    Image->Height = 0;
  }
}

//---------------------------------------------------------------------------

void TForm1::AddBitmap(Graphics::TBitmap* Bitmap, String Name, bool IsBinary)
{
  /// ADD TAB
  String S = IntToStr(BitmapCount) + ". " + ExtractFileName(Name);
  Bitmaps[S] = Bitmap;
  Binary[S] = IsBinary;
  TabControl->TabIndex=TabControl->Tabs->Add(S);
  BitmapCount++;
  RePaint();
}

//---------------------------------------------------------------------------

String TForm1::GetBitmapName(String Name)
{
  /// GET TAB "REAL" NAME
  for (int i = 1; i <= Name.Length(); i++)
    if (Name.SubString(i, 1) == " ")
      return Name.Delete(1,i);
}

String TForm1::GetBitmapName(int TabIndex)
{
  /// GET TAB "REAL" NAME
  if (TabIndex == -1 || TabIndex >= TabControl->Tabs->Count)
    return "";
  return GetBitmapName(TabControl->Tabs->Strings[TabIndex]);
}

//---------------------------------------------------------------------------

void TForm1::CopyBitmap(int TabIndex)
{
  /// COPY TAB
  if (TabIndex == -1 || TabIndex >= TabControl->Tabs->Count)
    return;
  Graphics::TBitmap *pTempImage = new Graphics::TBitmap();
  pTempImage->Assign(Bitmaps[TabControl->Tabs->Strings[TabIndex]]);
  AddBitmap(pTempImage, GetBitmapName(TabIndex), Binary[TabControl->Tabs->Strings[TabIndex]]);
}

//---------------------------------------------------------------------------

void TForm1::DeleteTab(int TabIndex)
{
  /// DELETE TAB
  bool NeedRePaint = false;
  if (TabIndex == -1)
    return;
  if (TabIndex == TabControl->TabIndex)
    NeedRePaint = true;
  delete Bitmaps[TabControl->Tabs->Strings[TabIndex]];
  Bitmaps.erase(TabControl->Tabs->Strings[TabIndex]);
  TabControl->Tabs->Delete(TabIndex);
  if (NeedRePaint)
  {
    if (TabIndex < TabControl->Tabs->Count)
      TabControl->TabIndex = TabIndex;
    else
      TabControl->TabIndex = TabControl->Tabs->Count - 1;
    RePaint();
  }
  CheckBinRGB();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::TabControlChange(TObject *Sender)
{
  RePaint();
}

//---------------------------------------------------------------------------


void __fastcall TForm1::FrameContextPopup(TObject *Sender,
      TPoint &MousePos, bool &Handled)
{
  /// SHOW POPUP
  TTabControl* ref_b = dynamic_cast <TTabControl*> (Sender);
  if (ref_b == NULL)  {    Handled = true;    return;  }  int ClickedTab = ref_b->IndexOfTabAt(MousePos.x,MousePos.y);  if (ClickedTab == -1)  {    Handled = true;    return;  }  ref_b->TabIndex = ClickedTab;  ref_b->OnChange(Sender);}//---------------------------------------------------------------------------

void __fastcall TForm1::CloseTab1Click(TObject *Sender)
{
  DeleteTab(TabControl->TabIndex);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CopyPage1Click(TObject *Sender)
{
  CopyBitmap(TabControl->TabIndex);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::SaveBtnClick(TObject *Sender)
{
  /// SAVE IMAGE
  TabControl->SetFocus();
  if (TabControl->TabIndex == -1)
    return;
  if (SaveDialog->Execute())
  {
    String S = SaveDialog->FileName;
    if (ExtractFileExt(S) != ".bmp")
      S = S + ".bmp";
    Bitmaps[TabControl->Tabs->Strings[TabControl->TabIndex]]->SaveToFile(S);
  }
}

//---------------------------------------------------------------------------

void __fastcall TForm1::BinBtnClick(TObject *Sender)
{
  /// BINARIZATION
  TabControl->SetFocus();
  if (TabControl->TabIndex == -1)
    return;
  String S = TabControl->Tabs->Strings[TabControl->TabIndex];
  Graphics::TBitmap *pOldImage = Bitmaps[TabControl->Tabs->Strings[TabControl->TabIndex]];
  Graphics::TBitmap *pNewImage = new Graphics::TBitmap();
  pNewImage->Assign(pOldImage);
  ProgressInit(1);
  Binarization(pOldImage, pNewImage);
  ProgressClear();
  if (CreateNewWindows)
  {
    AddBitmap(pNewImage, GetBitmapName(S), true);
  }
  else
  {
    delete pOldImage;
    Bitmaps[S] = pNewImage;
    Binary[S] = true;
  }
  RePaint();

}
//---------------------------------------------------------------------------


void __fastcall TForm1::CheckBox1Click(TObject *Sender)
{
  TCheckBox* ref_b = dynamic_cast <TCheckBox*> (Sender);
  if (ref_b == NULL)
    return;  CreateNewWindows = ref_b->Checked;
  TabControl->SetFocus();
}
//---------------------------------------------------------------------------


void __fastcall TForm1::NextTab1Click(TObject *Sender)
{
  /// NEXT TAB
  if (TabControl->TabIndex == TabControl->Tabs->Count - 1)
    TabControl->TabIndex = 0;
  else
    TabControl->TabIndex++;
  TabControl->OnChange(Sender);  
}//---------------------------------------------------------------------------


void __fastcall TForm1::BitBtn2Click(TObject *Sender)
{
  /// DILATATION
  TabControl->SetFocus();
  if (TabControl->TabIndex == -1)
    return;
  String S = TabControl->Tabs->Strings[TabControl->TabIndex];
  Graphics::TBitmap *pOldImage = Bitmaps[TabControl->Tabs->Strings[TabControl->TabIndex]];
  Graphics::TBitmap *pNewImage = new Graphics::TBitmap();
  pNewImage->Assign(pOldImage);
  ProgressInit(1);
  Dilatation(pOldImage, pNewImage, RequestInt("Dilatation","Dilatation Radius:",0));
  ProgressClear();
  if (CreateNewWindows)
  {
    AddBitmap(pNewImage, GetBitmapName(S), true);
  }
  else
  {
    delete pOldImage;
    Bitmaps[S] = pNewImage;
    Binary[S] = true;
  }
  RePaint();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::BitBtn3Click(TObject *Sender)
{
  /// EROSION
  TabControl->SetFocus();
  if (TabControl->TabIndex == -1)
    return;
  String S = TabControl->Tabs->Strings[TabControl->TabIndex];
  Graphics::TBitmap *pOldImage = Bitmaps[TabControl->Tabs->Strings[TabControl->TabIndex]];
  Graphics::TBitmap *pNewImage = new Graphics::TBitmap();
  pNewImage->Assign(pOldImage);
  ProgressInit(1);
  Erosion(pOldImage, pNewImage, RequestInt("Erosion","Erosion Radius:",0));
  ProgressClear();
  if (CreateNewWindows)
  {
    AddBitmap(pNewImage, GetBitmapName(S), true);
  }
  else
  {
    delete pOldImage;
    Bitmaps[S] = pNewImage;
    Binary[S] = true;
  }
  RePaint();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::BitBtn4Click(TObject *Sender)
{
  /// OPENING
  TabControl->SetFocus();
  if (TabControl->TabIndex == -1)
    return;
  String S = TabControl->Tabs->Strings[TabControl->TabIndex];
  Graphics::TBitmap *pOldImage = Bitmaps[TabControl->Tabs->Strings[TabControl->TabIndex]];
  Graphics::TBitmap *pNewImage = new Graphics::TBitmap();
  pNewImage->Assign(pOldImage);
  ProgressInit(2);
  Opening(pOldImage, pNewImage, RequestInt("Opening","Opening Radius:",0));
  ProgressClear();
  if (CreateNewWindows)
  {
    AddBitmap(pNewImage, GetBitmapName(S), true);
  }
  else
  {
    delete pOldImage;
    Bitmaps[S] = pNewImage;
    Binary[S] = true;
  }
  RePaint();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::BitBtn5Click(TObject *Sender)
{
  /// CLOSING
  TabControl->SetFocus();
  if (TabControl->TabIndex == -1)
    return;
  String S = TabControl->Tabs->Strings[TabControl->TabIndex];
  Graphics::TBitmap *pOldImage = Bitmaps[TabControl->Tabs->Strings[TabControl->TabIndex]];
  Graphics::TBitmap *pNewImage = new Graphics::TBitmap();
  pNewImage->Assign(pOldImage);
  ProgressInit(2);
  Closing(pOldImage, pNewImage, RequestInt("Closing","Closing Radius:",0));
  ProgressClear();
  if (CreateNewWindows)
  {
    AddBitmap(pNewImage, GetBitmapName(S), true);
  }
  else
  {
    delete pOldImage;
    Bitmaps[S] = pNewImage;
    Binary[S] = true;
  }
  RePaint();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::RGBButtonClick(TObject *Sender)
{
  if (TabControl->TabIndex == -1)
    return;
  RGBImage->Text = TabControl->Tabs->Strings[TabControl->TabIndex];
  RGBButton->Color = 0x005BFF5B;
  RGBButton->Caption = " + ";
  ButtonStateUpdate();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::BinButtonClick(TObject *Sender)
{
  if (TabControl->TabIndex == -1)
    return;
  if (!Binary[TabControl->Tabs->Strings[TabControl->TabIndex]])
    return;
  BinImage->Text = TabControl->Tabs->Strings[TabControl->TabIndex];
  BinButton->Color = 0x005BFF5B;
  BinButton->Caption = " + ";
  ButtonStateUpdate();
}
//---------------------------------------------------------------------------

void TForm1::CheckBinRGB()
{
  if (Bitmaps[RGBImage->Text] == NULL)
  {
    RGBImage->Text = "";
    RGBButton->Color = 0x005B5BFF;
    RGBButton->Caption = " ? ";
  }
  if (Bitmaps[BinImage->Text] == NULL)
  {
    BinImage->Text = "";
    BinButton->Color = 0x005B5BFF;
    BinButton->Caption = " ? ";
  }
}

//---------------------------------------------------------------------------

void TForm1::NullBinRGB()
{
  RGBImage->Text = "";
  RGBButton->Color = 0x005B5BFF;
  RGBButton->Caption = " ? ";
  BinImage->Text = "";
  BinButton->Color = 0x005B5BFF;
  BinButton->Caption = " ? ";
}


//---------------------------------------------------------------------------

void __fastcall TForm1::RecognBtnClick(TObject *Sender)
{
  TabControl->SetFocus();
  /// RECOGNITION
  if (Bitmaps[RGBImage->Text] == NULL || Bitmaps[BinImage->Text] == NULL)
  {
    ShowMessage("You must select RGB and Binary Images First!");
    return;
  }

  Graphics::TBitmap *pRgbImage = Bitmaps[RGBImage->Text];
  Graphics::TBitmap *pBinImage = Bitmaps[BinImage->Text];
  if (pRgbImage->Width != pBinImage->Width || pRgbImage->Height != pBinImage->Height)
  {
    ShowMessage("Error.\nInput images have different sizes.");
    return;
  }
  int MinimalArea = RequestInt("Recognition","Minimal Area:",-1);
  if (MinimalArea == -1)
    return;
  String S = TabControl->Tabs->Strings[TabControl->TabIndex];
  Graphics::TBitmap *pNewImage = new Graphics::TBitmap();
  pNewImage->Assign(pRgbImage);
  ProgressInit(1);
  if (Recognition(pRgbImage, pBinImage, pNewImage, MinimalArea) != 0)
    {
    ShowMessage(LastRecognitionResult());
    delete pNewImage;
    ProgressClear();
    NullBinRGB();
    return;
    }
  ProgressClear();
  if (CreateNewWindows)
  {
    AddBitmap(pNewImage, GetBitmapName(RGBImage->Text), false);
  }
  else
  {
    delete pRgbImage;
    Bitmaps[RGBImage->Text] = pNewImage;
    Binary[RGBImage->Text] = false;
    for (int i = 0; i < TabControl->Tabs->Count; i++)
      if (TabControl->Tabs->Strings[i] == RGBImage->Text)
      {
        TabControl->TabIndex = i;
        RePaint();
        break;
      }
  }
  NullBinRGB();
  ShowMessage(LastRecognitionResult());
}


//---------------------------------------------------------------------------

void __fastcall TForm1::ContBtnClick(TObject *Sender)
{
  TabControl->SetFocus();
  if (TabControl->TabIndex == -1)
    return;
  String S = TabControl->Tabs->Strings[TabControl->TabIndex];
  Graphics::TBitmap *pOldImage = Bitmaps[TabControl->Tabs->Strings[TabControl->TabIndex]];
  Graphics::TBitmap *pNewImage = new Graphics::TBitmap();
  pNewImage->Assign(pOldImage);
  ProgressInit(1);
  Contrast(pOldImage, pNewImage);
  ProgressClear();
  if (CreateNewWindows)
  {
    AddBitmap(pNewImage, GetBitmapName(S), false);
  }
  else
  {
    delete pOldImage;
    Bitmaps[S] = pNewImage;
    Binary[S] = false;
  }
  RePaint();
}
//---------------------------------------------------------------------------


void __fastcall TForm1::SmpBtnClick(TObject *Sender)
{
  TabControl->SetFocus();
  if (TabControl->TabIndex == -1)
    return;
  String S = TabControl->Tabs->Strings[TabControl->TabIndex];
  Graphics::TBitmap *pOldImage = Bitmaps[TabControl->Tabs->Strings[TabControl->TabIndex]];
  Graphics::TBitmap *pNewImage = new Graphics::TBitmap();
  Graphics::TBitmap *pTmpImage = new Graphics::TBitmap();
  ProgressInit(5);
  pNewImage->Assign(pOldImage); Binarization(pOldImage, pNewImage);
  pTmpImage->Assign(pNewImage); Erosion(pNewImage, pTmpImage, 1);
  pNewImage->Assign(pTmpImage); Closing(pTmpImage, pNewImage, 5);
  pTmpImage->Assign(pNewImage); Dilatation(pNewImage, pTmpImage, 1);
  pNewImage->Assign(pOldImage); Recognition(pOldImage, pTmpImage, pNewImage, 1000);
  delete pTmpImage;
  ProgressClear();
  if (CreateNewWindows)
  {
    AddBitmap(pNewImage, GetBitmapName(S), false);
  }
  else
  {
    delete pOldImage;
    Bitmaps[S] = pNewImage;
    Binary[S] = false;
  }
  RePaint();
  ShowMessage(LastRecognitionResult());
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Med12BtnClick(TObject *Sender)
{
  TabControl->SetFocus();
  if (TabControl->TabIndex == -1)
    return;
  String S = TabControl->Tabs->Strings[TabControl->TabIndex];
  Graphics::TBitmap *pOldImage = Bitmaps[TabControl->Tabs->Strings[TabControl->TabIndex]];
  Graphics::TBitmap *pNewImage = new Graphics::TBitmap();
  Graphics::TBitmap *pTmpImage = new Graphics::TBitmap();
  ProgressInit(3);
  pNewImage->Assign(pOldImage); Contrast(pOldImage, pNewImage);
  pTmpImage->Assign(pNewImage); Binarization(pNewImage, pTmpImage);
  pNewImage->Assign(pOldImage); Recognition(pOldImage, pTmpImage, pNewImage, 500);
  delete pTmpImage;
  ProgressClear();
  if (CreateNewWindows)
  {
    AddBitmap(pNewImage, GetBitmapName(S), false);
  }
  else
  {
    delete pOldImage;
    Bitmaps[S] = pNewImage;
    Binary[S] = false;
  }
  RePaint();
  ShowMessage(LastRecognitionResult());
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Med3BtnClick(TObject *Sender)
{
  TabControl->SetFocus();
  if (TabControl->TabIndex == -1)
    return;
  String S = TabControl->Tabs->Strings[TabControl->TabIndex];
  Graphics::TBitmap *pOldImage = Bitmaps[TabControl->Tabs->Strings[TabControl->TabIndex]];
  Graphics::TBitmap *pNewImage = new Graphics::TBitmap();
  Graphics::TBitmap *pTmpImage = new Graphics::TBitmap();
  ProgressInit(5);
  pNewImage->Assign(pOldImage); Binarization(pOldImage, pNewImage);
  pTmpImage->Assign(pNewImage); Erosion(pNewImage, pTmpImage, 4);
  pNewImage->Assign(pTmpImage); Dilatation(pTmpImage, pNewImage, 7);
  pTmpImage->Assign(pNewImage); Erosion(pNewImage, pTmpImage, 3);
  pNewImage->Assign(pOldImage); Recognition(pOldImage, pTmpImage, pNewImage, 1000);
  delete pTmpImage;
  ProgressClear();
  if (CreateNewWindows)
  {
    AddBitmap(pNewImage, GetBitmapName(S), false);
  }
  else
  {
    delete pOldImage;
    Bitmaps[S] = pNewImage;
    Binary[S] = false;
  }
  RePaint();
  ShowMessage(LastRecognitionResult());
}
//---------------------------------------------------------------------------

