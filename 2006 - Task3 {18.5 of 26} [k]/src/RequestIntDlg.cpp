//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "RequestIntDlg.h"
//--------------------------------------------------------------------- 
#pragma resource "*.dfm"
TOKBottomDlg *OKBottomDlg;
//---------------------------------------------------------------------
__fastcall TOKBottomDlg::TOKBottomDlg(TComponent* AOwner)
	: TForm(AOwner)
{
}
//--------------------------------------------------------------------- 

int RequestInt(String Caption, String Text, int Default)
{
  OKBottomDlg->Caption = Caption;
  OKBottomDlg->Label1->Caption = Text;
  OKBottomDlg->Edit1->Text = "";
  if (OKBottomDlg->ShowModal() == mrOk)
  {
    return StrToInt(OKBottomDlg->Edit1->Text);
  }
  else
    return Default;
}




