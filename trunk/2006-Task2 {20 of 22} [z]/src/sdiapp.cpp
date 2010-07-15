#if C_GRAPHICSMEDIA_LAB_DMOROZ_C
#/*******************************************************************************
# * (C) Copyright 2003/2004 by Vladimir Vezhnevets <vvp@graphics.cs.msu.su>     *
# *******************************************************************************/
#endif

//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------
USEFORM("SDIMain.cpp", SDIAppForm);
USEFORM("Dialog1.cpp", Form1);
USEFORM("Dialog2.cpp", Form2);
USEFORM("InProgress.cpp", Form3);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
	Application->Initialize();
	Application->Title = "Simple Image Processing";
     Application->CreateForm(__classid(TSDIAppForm), &SDIAppForm);
     Application->CreateForm(__classid(TForm1), &Form1);
     Application->CreateForm(__classid(TForm2), &Form2);
     Application->CreateForm(__classid(TForm3), &Form3);
     Application->Run();

	return 0;
}
//---------------------------------------------------------------------
