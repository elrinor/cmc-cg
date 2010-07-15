//---------------------------------------------------------------------------

#ifndef MainH
#define MainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
        TMainMenu *MainMenu1;
        TScrollBox *ScrollBox1;
        TImage *Image1;
        TOpenDialog *OpenDialog1;
        TMenuItem *File1;
        TMenuItem *Open1;
        TMenuItem *N1;
        TMenuItem *Exit1;
        TMenuItem *Image2;
        TMenuItem *Binarize1;
        TMenuItem *N2;
        TMenuItem *N3;
        TMenuItem *N4;
        TMenuItem *N5;
        TMenuItem *N6;
        TMenuItem *N7;
        TMenuItem *N8;
        TMenuItem *Simple1;
        TMenuItem *Noisy21311;
        TMenuItem *SimpleNoisy11;
        TMenuItem *Noisy1;
        void __fastcall Open1Click(TObject *Sender);
        void __fastcall Binarize1Click(TObject *Sender);
        void __fastcall N2Click(TObject *Sender);
        void __fastcall N3Click(TObject *Sender);
        void __fastcall N4Click(TObject *Sender);
        void __fastcall N5Click(TObject *Sender);
        void __fastcall N6Click(TObject *Sender);
        void __fastcall N8Click(TObject *Sender);
        void __fastcall Simple1Click(TObject *Sender);
        void __fastcall Noisy21311Click(TObject *Sender);
        void __fastcall SimpleNoisy11Click(TObject *Sender);
        void __fastcall Noisy1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
