//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "dlg.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm2 *Form2;
//---------------------------------------------------------------------------
__fastcall TForm2::TForm2(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------


int GetR()
{
  Form2->Edit1->Text = "";
  Form2->ShowModal();
  return StrToInt(Form2->Edit1->Text);
}
