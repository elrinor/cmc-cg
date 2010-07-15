//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"


TForm1 *Form1;

void TForm1::unZoom()
{
        this->zoom = 0.01;
        this->delta = complex<double>(0.0000001, 0.0000001);
}


//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{
        unZoom();
        this->mode = 0;
}
//---------------------------------------------------------------------------

void TForm1::reDraw()
{
        Graphics::TBitmap* bmp = this->Image1->Picture->Bitmap;

        int wb = (bmp->Width * 3 + 3) & -4; // кол-во байт в строке изображения
        int w = bmp->Width;
        int h = bmp->Height;

        bmp->PixelFormat = pf24bit; // 3 байта на пиксель

        unsigned char *p = (unsigned char *) bmp->ScanLine[bmp->Height - 1]; //указатели на начало массива

        if(this->mode == 0) {
                for(int iy = 0; iy < h; iy++) for(int ix = 0; ix < w; ix++) {
                        complex<double> k = toZPlane(ix, iy);
                        complex<double> z = complex<double>(0, 0);

                        int i;
                        for(i = 0; i < 31; i++) {
                                z = z * z + k;
                                if(abs(z) > 4)
                                        break;
                        }
                        int c = i * 8;

                        p[iy * wb + ix * 3 + 0] = c;
                        p[iy * wb + ix * 3 + 1] = c;
                        p[iy * wb + ix * 3 + 2] = c;
                }
        } else if(this->mode == 1) {
                for(int iy = 0; iy < h; iy++) for(int ix = 0; ix < w; ix++) {
                        complex<double> k = toZPlane(ix, iy);
                        complex<double> z = k;
                        complex<double> z1 = z;
                        complex<double> d = complex<double>(0, 0);

                        int i;
                        for(i = 0; i < 31; i++) {
                                z = z - (z * z * z - 1.0) / (3.0 * z * z);
                                d = z1 - z;
                                z1 = z;
                                if(abs(d) < 0.01)
                                        break;
                        }
                        int c = i * 8;

                        p[iy * wb + ix * 3 + 0] = c;
                        p[iy * wb + ix * 3 + 1] = c;
                        p[iy * wb + ix * 3 + 2] = c;
                }
        } else if(this->mode == 2) {
                for(int iy = 0; iy < h; iy++) for(int ix = 0; ix < w; ix++) {
                        complex<double> k = toZPlane(ix, iy);
                        complex<double> z = k;
                        complex<double> z1 = z;
                        complex<double> d = complex<double>(0.0, 0.0);

                        int i = 0;
                        for(i = 0; i < 15; i++) {
                                z = z - (pow(z, 5) - 1.0) / (3.5 * pow(z, 4));
                                d = z1 - z;
                                z1 = z;
                                if(abs(d) < 0.01)
                                        break;
                        }
                        int c = i * 16;

                        p[iy * wb + ix * 3 + 0] = c;
                        p[iy * wb + ix * 3 + 1] = c;
                        p[iy * wb + ix * 3 + 2] = c;
                }
        }

        this->Image1->Refresh();
}

//---------------------------------------------------------------------------

void __fastcall TForm1::FormResize(TObject *Sender)
{
        this->h = this->ClientHeight;
        this->w = this->ClientWidth;
        this->Image1->Picture->Bitmap->Height = this->h;
        this->Image1->Picture->Bitmap->Width = this->w;
        reDraw();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Exit1Click(TObject *Sender)
{
        Application->Terminate();        
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Unzoom1Click(TObject *Sender)
{
        unZoom();
        reDraw();
}
//---------------------------------------------------------------------------


void __fastcall TForm1::Image1MouseUp(TObject *Sender, TMouseButton Button,
      TShiftState Shift, int X, int Y)
{
        complex<double> z = toZPlane(X, this->h - Y);
        this->delta = z;
        
        if(Button == mbLeft)
                this->zoom *= 0.7;
        else if(Button == mbRight)
                this->zoom /= 0.7;

        reDraw();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Mandelbrot1Click(TObject *Sender)
{
        this->mode = 0;
        reDraw();        
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Newton1Click(TObject *Sender)
{
        this->mode = 1;
        reDraw();        
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Advanced1Click(TObject *Sender)
{
        this->mode = 2;
        reDraw();        
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Save1Click(TObject *Sender)
{
        if(this->SaveDialog1->Execute())
                this->Image1->Picture->SaveToFile(this->SaveDialog1->FileName);
}
//---------------------------------------------------------------------------


