//---------------------------------------------------------------------------

#ifndef mainformH
#define mainformH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Buttons.hpp>
#include <Dialogs.hpp>
#include <ExtDlgs.hpp>
#include "Frame.h"
#include <Menus.hpp>
#include "pies.h"
#include "CGAUGES.h"
#include <map>
#include <Graphics.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
  TPanel *Panel1;
  TPanel *Panel2;
  TPanel *Panel3;
  TTabControl *TabControl;
  TOpenPictureDialog *OpenDialog;
  TSavePictureDialog *SaveDialog;
  TFrame1 *Frame;
  TPopupMenu *PopupMenu1;
  TMenuItem *CloseTab1;
  TMenuItem *CopyPage1;
  TCheckBox *CheckBox1;
  TMenuItem *NextTab1;
  TGroupBox *GroupBox1;
  TGroupBox *GroupBox2;
  TBitBtn *LoadBtn;
  TBitBtn *SaveBtn;
  TCGauge *CGauge1;
  TGroupBox *GroupBox3;
  TBitBtn *BinBtn;
  TBitBtn *DilBtn;
  TBitBtn *EroBtn;
  TBitBtn *OpenBtn;
  TBitBtn *ClosBtn;
  TGroupBox *GroupBox4;
  TLabel *Label1;
  TEdit *RGBImage;
  TLabel *Label2;
  TEdit *BinImage;
  TStaticText *RGBButton;
  TStaticText *BinButton;
  TBitBtn *RecognBtn;
  TBevel *Bevel1;
  TGroupBox *GroupBox5;
  TBitBtn *ContBtn;
  TBitBtn *SmpBtn;
  TBitBtn *Med12Btn;
  TBitBtn *Med3Btn;
  void __fastcall OpenBtnClick(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall TabControlChange(TObject *Sender);
  void __fastcall FrameContextPopup(TObject *Sender, TPoint &MousePos,
          bool &Handled);
  void __fastcall CloseTab1Click(TObject *Sender);
  void __fastcall CopyPage1Click(TObject *Sender);
  void __fastcall SaveBtnClick(TObject *Sender);
  void __fastcall RecognBtnClick(TObject *Sender);
  void __fastcall CheckBox1Click(TObject *Sender);
  void __fastcall NextTab1Click(TObject *Sender);
  void __fastcall BitBtn2Click(TObject *Sender);
  void __fastcall BitBtn3Click(TObject *Sender);
  void __fastcall BitBtn4Click(TObject *Sender);
  void __fastcall BitBtn5Click(TObject *Sender);
  void __fastcall RGBButtonClick(TObject *Sender);
  void __fastcall BinButtonClick(TObject *Sender);
  void __fastcall BinBtnClick(TObject *Sender);
  void __fastcall ContBtnClick(TObject *Sender);
  void __fastcall SmpBtnClick(TObject *Sender);
  void __fastcall Med12BtnClick(TObject *Sender);
  void __fastcall Med3BtnClick(TObject *Sender);
private:	// User declarations
  std::map<String, Graphics::TBitmap*> Bitmaps;
  std::map<String, bool> Binary;
  TImage *Image;
  int BitmapCount;
  bool CreateNewWindows;
  void AddBitmap(Graphics::TBitmap* Bitmap, String Name, bool IsBinary);
  void CopyBitmap(int TabIndex);
  String GetBitmapName(int TabIndex);
  String GetBitmapName(String Name);
  void DeleteTab(int TabIndex);
  void CheckBinRGB();
  void NullBinRGB();
  void ButtonStateUpdate();
public:		// User declarations
  bool InProgress;
  void RePaint();
  __fastcall TForm1(TComponent* Owner);
};

//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
