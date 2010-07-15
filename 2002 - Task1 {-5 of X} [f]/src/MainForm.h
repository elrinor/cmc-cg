//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include <Dialogs.hpp>
#include <complex>

using namespace std;
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
        TMainMenu *MainMenu1;
        TMenuItem *File1;
        TMenuItem *Exit1;
        TMenuItem *Fractal1;
        TMenuItem *Unzoom1;
        TMenuItem *N1;
        TMenuItem *Mandelbrot1;
        TMenuItem *Newton1;
        TMenuItem *Advanced1;
        TMenuItem *N2;
        TMenuItem *Save1;
        TSaveDialog *SaveDialog1;
        TImage *Image1;
        void __fastcall FormResize(TObject *Sender);
        void __fastcall Exit1Click(TObject *Sender);
        void __fastcall Unzoom1Click(TObject *Sender);
        void __fastcall Image1MouseUp(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y);
        void __fastcall Mandelbrot1Click(TObject *Sender);
        void __fastcall Newton1Click(TObject *Sender);
        void __fastcall Advanced1Click(TObject *Sender);
        void __fastcall Save1Click(TObject *Sender);
private:
        void reDraw();
        void unZoom();

        double zoom; // зум - какой размер имеет пиксель на комплексной плоскости
        complex<double> delta; // смещение центра изображения
        int w, h; // размеры окна вывода изображения

        int mode; // 0 - мандельброт, 1 - ньютон, 2 - advanced

        // преобразования из координат окна в координаты на комплексной плоскости
        complex<double> toZPlane(int x, int y) {
                return complex<double>((x - this->w / 2), (y - this->h / 2)) * this->zoom + this->delta;
        }
        
public:		// User declarations
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
