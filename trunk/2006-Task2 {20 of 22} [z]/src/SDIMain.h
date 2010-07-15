#if C_GRAPHICSMEDIA_LAB_DMOROZ_C
#/*******************************************************************************
# * (C) Copyright 2003/2004 by Vladimir Vezhnevets <vvp@graphics.cs.msu.su>     *
# *******************************************************************************/
#endif

//----------------------------------------------------------------------------
#ifndef SDIMainH
#define SDIMainH
//----------------------------------------------------------------------------
#include <vcl\ComCtrls.hpp>
#include <vcl\ExtCtrls.hpp>
#include <vcl\Buttons.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Dialogs.hpp>
#include <vcl\Menus.hpp>
#include <vcl\Controls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Graphics.hpp>
#include <vcl\Classes.hpp>
#include <vcl\Windows.hpp>
#include <vcl\System.hpp>
#include <ActnList.hpp>
#include <ImgList.hpp>
#include <StdActns.hpp>
#include <ToolWin.hpp>
#include <ExtDlgs.hpp>
//----------------------------------------------------------------------------
class TSDIAppForm : public TForm
{
__published:
    TToolBar *ToolBar1;
    TToolButton *ToolButton1;
    TToolButton *ToolButton2;
    TActionList *ActionList1;
    TAction *FileOpen1;
    TAction *FileSave1;
    TAction *FileSaveAs1;
    TAction *FileExit1;
    TAction *HelpAbout1;
    TStatusBar *StatusBar;
    TImageList *ImageList1;
    TMainMenu *MainMenu1;
    TMenuItem *File1;
    TMenuItem *FileOpenItem;
    TMenuItem *FileSaveItem;
    TMenuItem *N1;
    TMenuItem *FileExitItem;
  TOpenPictureDialog *OpenDialog;
  TSavePictureDialog *SaveDialog;
  TImage *DocImage;
  TMenuItem *Process1;
  TAction *BoxFilter;
  TAction *Colorize;
        TMenuItem *WhiteNoise1;
  TMenuItem *ImpulseNoise1;
  TMenuItem *BlurFilter1;
  TMenuItem *SharpenFilter1;
  TMenuItem *FindEdgesFilter1;
  TMenuItem *N2DimensionalGaussianBlur1;
  TMenuItem *N1DimensionalGaussianBlur1;
  TMenuItem *MedianFilter1;
  TMenuItem *MedianFilterByL1VectorDistance1;
  TMenuItem *KNearestNeighborsFilter1;
  TMenuItem *NonLocalMeansFilter1;
  TMenuItem *Automatic1;
  TMenuItem *AutomaticNonLocalMeansFilter1;
        void __fastcall FileOpen1Execute(TObject *Sender);
        void __fastcall FileSave1Execute(TObject *Sender);
        void __fastcall FileExit1Execute(TObject *Sender);
        void __fastcall WhiteNoise1Click(TObject *Sender);
  void __fastcall ImpulseNoise1Click(TObject *Sender);
  void __fastcall BlurFilter1Click(TObject *Sender);
  void __fastcall SharpenFilter1Click(TObject *Sender);
  void __fastcall FindEdgesFilter1Click(TObject *Sender);
  void __fastcall N2DimensionalGaussianBlur1Click(TObject *Sender);
  void __fastcall N1DimensionalGaussianBlur1Click(TObject *Sender);
  void __fastcall MedianFilter1Click(TObject *Sender);
  void __fastcall MedianFilterByL1VectorDistance1Click(TObject *Sender);
  void __fastcall KNearestNeighborsFilter1Click(TObject *Sender);
  void __fastcall NonLocalMeansFilter1Click(TObject *Sender);
  void __fastcall Automatic1Click(TObject *Sender);
  void __fastcall AutomaticNonLocalMeansFilter1Click(TObject *Sender);
private:
public:
    virtual __fastcall TSDIAppForm(TComponent *AOwner);
};
//----------------------------------------------------------------------------
extern TSDIAppForm *SDIAppForm;
//----------------------------------------------------------------------------
#endif
