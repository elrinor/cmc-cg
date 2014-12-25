unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Math, ComCtrls, ToolWin;

type
  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    Bevel1: TBevel;
    BitBtn2: TBitBtn;
    procedure FormResize(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
type
  TBigByteArray=Array[0..100000000]of byte;
  PBigByteArray=^TBigByteArray;

var
  Img1,Img2:TBitMap;
  Mashtab:Extended=1.0;
  State:Integer;
  PSNR:Extended;
{$R *.DFM}

Procedure ReDraw;
var Rect:TRect;
    h,w:Integer;
begin;
If State=1 then
  begin;
  h:=Max(Round(Mashtab*Img1.Height),1);
  w:=Max(Round(Mashtab*Img1.Width),1);
  Form1.Image1.Picture.Bitmap.Height:=h+2;
  Form1.Image1.Picture.Bitmap.Width:=w+2;
  Form1.Image1.Height:=h+2;
  Form1.Image1.Width:=w+2;
  Form1.Image1.Canvas.Brush.Color:=clBlack;
  Rect.Left:=0;
  Rect.Top:=0;
  Rect.Right:=w+2;
  Rect.Bottom:=h+2;
  Form1.Image1.Canvas.FrameRect(Rect);
  Rect.Left:=1;
  Rect.Top:=1;
  Rect.Right:=w+1;
  Rect.Bottom:=h+1;
  Form1.Image1.Canvas.StretchDraw(Rect,Img1);
  end;
If State=2 then
  begin;
  h:=Max(Round(Mashtab*Img2.Height),1);
  w:=Max(Round(Mashtab*Img2.Width),1);
  Form1.Image1.Picture.Bitmap.Height:=h+2;
  Form1.Image1.Picture.Bitmap.Width:=w+2;
  Form1.Image1.Height:=h+2;
  Form1.Image1.Width:=w+2;
  Form1.Image1.Canvas.Brush.Color:=clBlack;
  Rect.Left:=0;
  Rect.Top:=0;
  Rect.Right:=w+2;
  Rect.Bottom:=h+2;
  Form1.Image1.Canvas.FrameRect(Rect);
  Rect.Left:=1;
  Rect.Top:=1;
  Rect.Right:=w+1;
  Rect.Bottom:=h+1;
  Form1.Image1.Canvas.StretchDraw(Rect,Img2);
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
Form1.ScrollBox1.Width:=Form1.Width-8;
Form1.ScrollBox1.Height:=Form1.Height-60;
Form1.Bevel1.Width:=Form1.Width-8;
end;


procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
If Form1.OpenDialog1.Execute then
  If FileExists(Form1.OpenDialog1.FileName) then
    begin;
    State:=1;
    Img1.Free;
    Img1:=TBitMap.Create;
    Img1.LoadFromFile(Form1.OpenDialog1.FileName);
    ReDraw;
    end;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
try
  Mashtab:=Mashtab*1.1;
  ReDraw;
except
  on e:Exception do
    begin;
    Mashtab:=Mashtab/1.1;
    {ReDraw;}
    end;
end;
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
begin
Mashtab:=Mashtab/1.1;
ReDraw;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
State:=0;
end;

Function IsGrayScale(var Img:TBitMap;var P:PBigByteArray):Boolean;
const GrayScaleMaxDiff=1400{1200?};
var w,h,x,y:Integer;
    Delta:Int64;
begin;
w:=Img1.Width*3; w:=w+(4-w mod 4)mod 4;
h:=Img1.Height-1;
Delta:=0;
For x:=0 to (Img.Width-1) div 2 do For y:=0 to (Img.Height-1) div 2 do
  Delta:=Delta+Sqr(Max(Max(P^[(h-(y*2))*w+(x*2)*3+2],P^[(h-(y*2+1))*w+(x*2+1)*3+0]),Max(P^[(h-(y*2+1))*w+(x*2)*3+1],P^[(h-(y*2))*w+(x*2+1)*3+1]))-
                   Min(Min(P^[(h-(y*2))*w+(x*2)*3+2],P^[(h-(y*2+1))*w+(x*2+1)*3+0]),Min(P^[(h-(y*2+1))*w+(x*2)*3+1],P^[(h-(y*2))*w+(x*2+1)*3+1])));
If Delta<=(Img.Height div 2)*(Img.Width div 2)*GrayScaleMaxDiff then
  IsGrayScale:=True
else
  IsGrayScale:=False;
end;


procedure TForm1.BitBtn1Click(Sender: TObject);
var P1,P2:PBigByteArray;
    h,w:Integer;
    x,y,x1,y1,k,c:Integer;
    Sum:Extended;
begin
If State=1 then
  begin;
  Img2.Free;
  Img2:=TBitMap.Create;
  Img2.Width:=Img1.Width;
  Img2.Height:=Img1.Height;
  Img1.PixelFormat:=pf24bit;
  Img2.PixelFormat:=pf24bit;
  P1:=Img1.ScanLine[Img1.Height-1];
  P2:=Img2.ScanLine[Img2.Height-1];
  w:=Img1.Width*3; w:=w+(4-w mod 4)mod 4;
  h:=Img1.Height-1;

  Fillchar(p2^,w*(h+1),0);

  // Интерполяция картинки
  For x:=1 to Img1.Width-2 do For y:=1 to Img1.Height-2 do
    begin;
    If P1^[(h-y)*w+x*3+0]<>0 then
      begin;
      P2^[(h-y)*w+x*3+0]:=P1^[(h-y)*w+x*3+0];
      P2^[(h-(y-1))*w+(x+0)*3+0]:=P2^[(h-(y-1))*w+(x+0)*3+0]+P1^[(h-y)*w+x*3+0] shr 1;
      P2^[(h-(y+1))*w+(x+0)*3+0]:=P2^[(h-(y+1))*w+(x+0)*3+0]+P1^[(h-y)*w+x*3+0] shr 1;
      P2^[(h-(y+0))*w+(x+1)*3+0]:=P2^[(h-(y+0))*w+(x+1)*3+0]+P1^[(h-y)*w+x*3+0] shr 1;
      P2^[(h-(y+0))*w+(x-1)*3+0]:=P2^[(h-(y+0))*w+(x-1)*3+0]+P1^[(h-y)*w+x*3+0] shr 1;
      P2^[(h-(y+1))*w+(x+1)*3+0]:=P2^[(h-(y+1))*w+(x+1)*3+0]+P1^[(h-y)*w+x*3+0] shr 2;
      P2^[(h-(y+1))*w+(x-1)*3+0]:=P2^[(h-(y+1))*w+(x-1)*3+0]+P1^[(h-y)*w+x*3+0] shr 2;
      P2^[(h-(y-1))*w+(x+1)*3+0]:=P2^[(h-(y-1))*w+(x+1)*3+0]+P1^[(h-y)*w+x*3+0] shr 2;
      P2^[(h-(y-1))*w+(x-1)*3+0]:=P2^[(h-(y-1))*w+(x-1)*3+0]+P1^[(h-y)*w+x*3+0] shr 2;
      end;
     If P1^[(h-y)*w+x*3+1]<>0 then
      begin;
      P2^[(h-y)*w+x*3+1]:=P1^[(h-y)*w+x*3+1];
      P2^[(h-(y-1))*w+(x+0)*3+1]:=P2^[(h-(y-1))*w+(x+0)*3+1]+P1^[(h-y)*w+x*3+1] shr 2;
      P2^[(h-(y+1))*w+(x+0)*3+1]:=P2^[(h-(y+1))*w+(x+0)*3+1]+P1^[(h-y)*w+x*3+1] shr 2;
      P2^[(h-(y+0))*w+(x+1)*3+1]:=P2^[(h-(y+0))*w+(x+1)*3+1]+P1^[(h-y)*w+x*3+1] shr 2;
      P2^[(h-(y+0))*w+(x-1)*3+1]:=P2^[(h-(y+0))*w+(x-1)*3+1]+P1^[(h-y)*w+x*3+1] shr 2;
      end;
     If P1^[(h-y)*w+x*3+2]<>0 then
      begin;
      P2^[(h-y)*w+x*3+2]:=P1^[(h-y)*w+x*3+2];
      P2^[(h-(y-1))*w+(x+0)*3+2]:=P2^[(h-(y-1))*w+(x+0)*3+2]+P1^[(h-y)*w+x*3+2] shr 1;
      P2^[(h-(y+1))*w+(x+0)*3+2]:=P2^[(h-(y+1))*w+(x+0)*3+2]+P1^[(h-y)*w+x*3+2] shr 1;
      P2^[(h-(y+0))*w+(x+1)*3+2]:=P2^[(h-(y+0))*w+(x+1)*3+2]+P1^[(h-y)*w+x*3+2] shr 1;
      P2^[(h-(y+0))*w+(x-1)*3+2]:=P2^[(h-(y+0))*w+(x-1)*3+2]+P1^[(h-y)*w+x*3+2] shr 1;
      P2^[(h-(y+1))*w+(x+1)*3+2]:=P2^[(h-(y+1))*w+(x+1)*3+2]+P1^[(h-y)*w+x*3+2] shr 2;
      P2^[(h-(y+1))*w+(x-1)*3+2]:=P2^[(h-(y+1))*w+(x-1)*3+2]+P1^[(h-y)*w+x*3+2] shr 2;
      P2^[(h-(y-1))*w+(x+1)*3+2]:=P2^[(h-(y-1))*w+(x+1)*3+2]+P1^[(h-y)*w+x*3+2] shr 2;
      P2^[(h-(y-1))*w+(x-1)*3+2]:=P2^[(h-(y-1))*w+(x-1)*3+2]+P1^[(h-y)*w+x*3+2] shr 2;
      end;
    end;

  // Интерполяция границ
  y:=0; For x:=1 to Img1.Width-2 do
    begin;
    If P1^[(h-y)*w+x*3+1]<>0 then
      begin;
      P2^[(h-y)*w+x*3+1]:=P1^[(h-y)*w+x*3+1];
      P2^[(h-y)*w+(x+1)*3+1]:=P2^[(h-y)*w+(x+1)*3+1]+P1^[(h-y)*w+x*3+1]*3 shr 3;
      P2^[(h-y)*w+(x-1)*3+1]:=P2^[(h-y)*w+(x-1)*3+1]+P1^[(h-y)*w+x*3+1]*3 shr 3;
      P2^[(h-(y+1))*w+(x)*3+1]:=P2^[(h-(y+1))*w+(x)*3+1]+P1^[(h-y)*w+x*3+1] shr 2;
      end;
    if P1^[(h-y)*w+x*3+2]<>0 then
      begin;
      P2^[(h-y)*w+x*3+2]:=P1^[(h-y)*w+x*3+2];
      P2^[(h-y)*w+(x+1)*3+2]:=P2^[(h-y)*w+(x+1)*3+2]+P1^[(h-y)*w+x*3+2] shr 1;
      P2^[(h-y)*w+(x-1)*3+2]:=P2^[(h-y)*w+(x-1)*3+2]+P1^[(h-y)*w+x*3+2] shr 1;
      P2^[(h-(y+1))*w+(x)*3+2]:=P2^[(h-(y+1))*w+(x)*3+2]+P1^[(h-y)*w+x*3+2] shr 1;
      P2^[(h-(y+1))*w+(x+1)*3+2]:=P2^[(h-(y+1))*w+(x+1)*3+2]+P1^[(h-y)*w+x*3+2] shr 2;
      P2^[(h-(y+1))*w+(x-1)*3+2]:=P2^[(h-(y+1))*w+(x-1)*3+2]+P1^[(h-y)*w+x*3+2] shr 2;
      end;
    P2^[(h-y)*w+x*3+0]:=P2^[(h-y)*w+x*3+0]*2;
    end;
  x:=0; For y:=1 to Img1.Height-2 do
    begin;
    If P1^[(h-y)*w+x*3+1]<>0 then
      begin;
      P2^[(h-y)*w+x*3+1]:=P1^[(h-y)*w+x*3+1];
      P2^[(h-(y-1))*w+(x)*3+1]:=P2^[(h-(y-1))*w+(x)*3+1]+P1^[(h-y)*w+x*3+1]*3 shr 3;
      P2^[(h-(y+1))*w+(x)*3+1]:=P2^[(h-(y+1))*w+(x)*3+1]+P1^[(h-y)*w+x*3+1]*3 shr 3;
      P2^[(h-(y))*w+(x+1)*3+1]:=P2^[(h-(y))*w+(x+1)*3+1]+P1^[(h-y)*w+x*3+1] shr 2;
      end;
    if P1^[(h-y)*w+x*3+2]<>0 then
      begin;
      P2^[(h-y)*w+x*3+2]:=P1^[(h-y)*w+x*3+2];
      P2^[(h-(y+1))*w+(x)*3+2]:=P2^[(h-(y+1))*w+(x)*3+2]+P1^[(h-y)*w+x*3+2] shr 1;
      P2^[(h-(y-1))*w+(x)*3+2]:=P2^[(h-(y-1))*w+(x)*3+2]+P1^[(h-y)*w+x*3+2] shr 1;
      P2^[(h-(y))*w+(x+1)*3+2]:=P2^[(h-(y))*w+(x+1)*3+2]+P1^[(h-y)*w+x*3+2] shr 1;
      P2^[(h-(y+1))*w+(x+1)*3+2]:=P2^[(h-(y+1))*w+(x+1)*3+2]+P1^[(h-y)*w+x*3+2] shr 2;
      P2^[(h-(y-1))*w+(x+1)*3+2]:=P2^[(h-(y-1))*w+(x+1)*3+2]+P1^[(h-y)*w+x*3+2] shr 2;
      end;
    P2^[(h-y)*w+x*3+0]:=P2^[(h-y)*w+x*3+0]*2;
    end;
   if Img1.Height mod 2=1 then
    begin;
    y:=Img1.Height-1; For x:=1 to Img1.Width-2 do
      begin;
      If P1^[(h-y)*w+x*3+1]<>0 then
        begin;
        P2^[(h-y)*w+x*3+1]:=P1^[(h-y)*w+x*3+1];
        P2^[(h-y)*w+(x+1)*3+1]:=P2^[(h-y)*w+(x+1)*3+1]+P1^[(h-y)*w+x*3+1]*3 shr 3;
        P2^[(h-y)*w+(x-1)*3+1]:=P2^[(h-y)*w+(x-1)*3+1]+P1^[(h-y)*w+x*3+1]*3 shr 3;
        P2^[(h-(y-1))*w+(x)*3+1]:=P2^[(h-(y-1))*w+(x)*3+1]+P1^[(h-y)*w+x*3+1] shr 2;
        end;
      if P1^[(h-y)*w+x*3+2]<>0 then
        begin;
        P2^[(h-y)*w+x*3+2]:=P1^[(h-y)*w+x*3+2];
        P2^[(h-y)*w+(x+1)*3+2]:=P2^[(h-y)*w+(x+1)*3+2]+P1^[(h-y)*w+x*3+2] shr 1;
        P2^[(h-y)*w+(x-1)*3+2]:=P2^[(h-y)*w+(x-1)*3+2]+P1^[(h-y)*w+x*3+2] shr 1;
        P2^[(h-(y-1))*w+(x)*3+2]:=P2^[(h-(y-1))*w+(x)*3+2]+P1^[(h-y)*w+x*3+2] shr 1;
        P2^[(h-(y-1))*w+(x+1)*3+2]:=P2^[(h-(y-1))*w+(x+1)*3+2]+P1^[(h-y)*w+x*3+2] shr 2;
        P2^[(h-(y-1))*w+(x-1)*3+2]:=P2^[(h-(y-1))*w+(x-1)*3+2]+P1^[(h-y)*w+x*3+2] shr 2;
        end;
      P2^[(h-y)*w+x*3+0]:=P2^[(h-y)*w+x*3+0]*2;
      end;
    end;
  if Img1.Height mod 2=0 then
    begin;
    y:=Img1.Height-1; For x:=1 to Img1.Width-2 do
      begin;
      If P1^[(h-y)*w+x*3+1]<>0 then
        begin;
        P2^[(h-y)*w+x*3+1]:=P1^[(h-y)*w+x*3+1];
        P2^[(h-y)*w+(x+1)*3+1]:=P2^[(h-y)*w+(x+1)*3+1]+P1^[(h-y)*w+x*3+1]*3 shr 3;
        P2^[(h-y)*w+(x-1)*3+1]:=P2^[(h-y)*w+(x-1)*3+1]+P1^[(h-y)*w+x*3+1]*3 shr 3;
        P2^[(h-(y-1))*w+(x)*3+1]:=P2^[(h-(y-1))*w+(x)*3+1]+P1^[(h-y)*w+x*3+1] shr 2;
        end;
      if P1^[(h-y)*w+x*3+0]<>0 then
        begin;
        P2^[(h-y)*w+x*3+0]:=P1^[(h-y)*w+x*3+0];
        P2^[(h-y)*w+(x+1)*3+0]:=P2^[(h-y)*w+(x+1)*3+0]+P1^[(h-y)*w+x*3+0] shr 1;
        P2^[(h-y)*w+(x-1)*3+0]:=P2^[(h-y)*w+(x-1)*3+0]+P1^[(h-y)*w+x*3+0] shr 1;
        P2^[(h-(y-1))*w+(x)*3+0]:=P2^[(h-(y-1))*w+(x)*3+0]+P1^[(h-y)*w+x*3+0] shr 1;
        P2^[(h-(y-1))*w+(x+1)*3+0]:=P2^[(h-(y-1))*w+(x+1)*3+0]+P1^[(h-y)*w+x*3+0] shr 2;
        P2^[(h-(y-1))*w+(x-1)*3+0]:=P2^[(h-(y-1))*w+(x-1)*3+0]+P1^[(h-y)*w+x*3+0] shr 2;
        end;
      P2^[(h-y)*w+x*3+2]:=P2^[(h-y)*w+x*3+2]*2;
      end;
    end;
  if Img1.Width mod 2=0 then
    begin;
    x:=Img1.Width-1; For y:=1 to Img1.Height-2 do
      begin;
      If P1^[(h-y)*w+x*3+1]<>0 then
        begin;
        P2^[(h-y)*w+x*3+1]:=P1^[(h-y)*w+x*3+1];
        P2^[(h-(y-1))*w+(x)*3+1]:=P2^[(h-(y-1))*w+(x)*3+1]+P1^[(h-y)*w+x*3+1]*3 shr 3;
        P2^[(h-(y+1))*w+(x)*3+1]:=P2^[(h-(y+1))*w+(x)*3+1]+P1^[(h-y)*w+x*3+1]*3 shr 3;
        P2^[(h-(y))*w+(x-1)*3+1]:=P2^[(h-(y))*w+(x-1)*3+1]+P1^[(h-y)*w+x*3+1] shr 2;
        end;
      if P1^[(h-y)*w+x*3+0]<>0 then
        begin;
        P2^[(h-y)*w+x*3+0]:=P1^[(h-y)*w+x*3+0];
        P2^[(h-(y+1))*w+(x)*3+0]:=P2^[(h-(y+1))*w+(x)*3+0]+P1^[(h-y)*w+x*3+0] shr 1;
        P2^[(h-(y-1))*w+(x)*3+0]:=P2^[(h-(y-1))*w+(x)*3+0]+P1^[(h-y)*w+x*3+0] shr 1;
        P2^[(h-(y))*w+(x-1)*3+0]:=P2^[(h-(y))*w+(x-1)*3+0]+P1^[(h-y)*w+x*3+0] shr 1;
        P2^[(h-(y+1))*w+(x-1)*3+0]:=P2^[(h-(y+1))*w+(x-1)*3+0]+P1^[(h-y)*w+x*3+0] shr 2;
        P2^[(h-(y-1))*w+(x-1)*3+0]:=P2^[(h-(y-1))*w+(x-1)*3+0]+P1^[(h-y)*w+x*3+0] shr 2;
        end;
      P2^[(h-y)*w+x*3+2]:=P2^[(h-y)*w+x*3+2]*2;
      end;
    end;
  if Img1.Width mod 2=1 then
    begin;
    x:=Img1.Width-1; For y:=1 to Img1.Height-2 do
      begin;
      If P1^[(h-y)*w+x*3+1]<>0 then
        begin;
        P2^[(h-y)*w+x*3+1]:=P1^[(h-y)*w+x*3+1];
        P2^[(h-(y-1))*w+(x)*3+1]:=P2^[(h-(y-1))*w+(x)*3+1]+P1^[(h-y)*w+x*3+1]*3 shr 3;
        P2^[(h-(y+1))*w+(x)*3+1]:=P2^[(h-(y+1))*w+(x)*3+1]+P1^[(h-y)*w+x*3+1]*3 shr 3;
        P2^[(h-(y))*w+(x-1)*3+1]:=P2^[(h-(y))*w+(x-1)*3+1]+P1^[(h-y)*w+x*3+1] shr 2;
        end;
      if P1^[(h-y)*w+x*3+2]<>0 then
        begin;
        P2^[(h-y)*w+x*3+2]:=P1^[(h-y)*w+x*3+2];
        P2^[(h-(y+1))*w+(x)*3+2]:=P2^[(h-(y+1))*w+(x)*3+2]+P1^[(h-y)*w+x*3+2] shr 1;
        P2^[(h-(y-1))*w+(x)*3+2]:=P2^[(h-(y-1))*w+(x)*3+2]+P1^[(h-y)*w+x*3+2] shr 1;
        P2^[(h-(y))*w+(x-1)*3+2]:=P2^[(h-(y))*w+(x-1)*3+2]+P1^[(h-y)*w+x*3+2] shr 1;
        P2^[(h-(y+1))*w+(x-1)*3+2]:=P2^[(h-(y+1))*w+(x-1)*3+2]+P1^[(h-y)*w+x*3+2] shr 2;
        P2^[(h-(y-1))*w+(x-1)*3+2]:=P2^[(h-(y-1))*w+(x-1)*3+2]+P1^[(h-y)*w+x*3+2] shr 2;
        end;
      P2^[(h-y)*w+x*3+0]:=P2^[(h-y)*w+x*3+0]*2;
      end;
    end;

  // Интерполяция углов  
  P2^[(h-0)*w+0*3+2]:=P1^[(h-0)*w+0*3+2];
  P2^[(h-1)*w+0*3+2]:=P2^[(h-1)*w+0*3+2]+P1^[(h-0)*w+0*3+2] shr 1;
  P2^[(h-0)*w+1*3+2]:=P2^[(h-0)*w+1*3+2]+P1^[(h-0)*w+0*3+2] shr 1;
  P2^[(h-1)*w+1*3+2]:=P2^[(h-1)*w+1*3+2]+P1^[(h-0)*w+0*3+2] shr 2;
  P2^[(h-0)*w+0*3+1]:=P2^[(h-0)*w+0*3+1]*4 div 3;
  P2^[(h-0)*w+0*3+0]:=P2^[(h-0)*w+0*3+0] shl 2;
  x:=0;y:=Img1.Height-1;
  if P1^[(h-y)*w+x*3+1]<>0 then
    begin;
    P2^[(h-y)*w+x*3+1]:=P1^[(h-y)*w+x*3+1];
    P2^[(h-(y-1))*w+x*3+1]:=P2^[(h-(y-1))*w+x*3+1]+P1^[(h-y)*w+x*3+1]*3 shr 3;
    P2^[(h-(y))*w+(x+1)*3+1]:=P2^[(h-(y))*w+(x+1)*3+1]+P1^[(h-y)*w+x*3+1]*3 shr 3;
    P2^[(h-y)*w+x*3+0]:=P2^[(h-y)*w+x*3+0] shl 1;
    P2^[(h-y)*w+x*3+2]:=P2^[(h-y)*w+x*3+2] shl 1;
    end;
  if P1^[(h-y)*w+x*3+2]<>0 then
    begin;
    P2^[(h-y)*w+x*3+2]:=P1^[(h-y)*w+x*3+2];
    P2^[(h-(y-1))*w+x*3+2]:=P2^[(h-(y-1))*w+x*3+2]+P1^[(h-y)*w+x*3+2] shr 1;
    P2^[(h-(y))*w+(x+1)*3+2]:=P2^[(h-(y))*w+(x+1)*3+2]+P1^[(h-y)*w+x*3+2] shr 1;
    P2^[(h-(y-1))*w+(x+1)*3+2]:=P2^[(h-(y-1))*w+(x+1)*3+2]+P1^[(h-y)*w+x*3+2] shr 2;
    P2^[(h-y)*w+x*3+1]:=P2^[(h-y)*w+x*3+1] shl 2 div 3;
    P2^[(h-y)*w+x*3+0]:=P2^[(h-y)*w+x*3+0] shl 3;
    end;
  x:=Img1.Width-1;y:=0;
  if P1^[(h-y)*w+x*3+1]<>0 then
    begin;
    P2^[(h-y)*w+x*3+1]:=P1^[(h-y)*w+x*3+1];
    P2^[(h-(y+1))*w+x*3+1]:=P2^[(h-(y+1))*w+x*3+1]+P1^[(h-y)*w+x*3+1]*3 shr 3;
    P2^[(h-(y))*w+(x-1)*3+1]:=P2^[(h-(y))*w+(x-1)*3+1]+P1^[(h-y)*w+x*3+1]*3 shr 3;
    P2^[(h-y)*w+x*3+0]:=P2^[(h-y)*w+x*3+0] shl 1;
    P2^[(h-y)*w+x*3+2]:=P2^[(h-y)*w+x*3+2] shl 1;
    end;
  if P1^[(h-y)*w+x*3+2]<>0 then
    begin;
    P2^[(h-y)*w+x*3+2]:=P1^[(h-y)*w+x*3+2];
    P2^[(h-(y+1))*w+x*3+2]:=P2^[(h-(y+1))*w+x*3+2]+P1^[(h-y)*w+x*3+2] shr 1;
    P2^[(h-(y))*w+(x-1)*3+2]:=P2^[(h-(y))*w+(x-1)*3+2]+P1^[(h-y)*w+x*3+2] shr 1;
    P2^[(h-(y+1))*w+(x+1)*3+2]:=P2^[(h-(y+1))*w+(x+1)*3+2]+P1^[(h-y)*w+x*3+2] shr 2;
    P2^[(h-y)*w+x*3+1]:=P2^[(h-y)*w+x*3+1] shl 2 div 3;
    P2^[(h-y)*w+x*3+0]:=P2^[(h-y)*w+x*3+0] shl 3;
    end;
  x:=Img1.Width-1;y:=Img1.Height-1;
  If P1^[(h-y)*w+x*3+0]<>0 then
    begin;
    P2^[(h-y)*w+x*3+0]:=P1^[(h-y)*w+x*3+0];
    P2^[(h-y)*w+x*3+1]:=P2^[(h-y)*w+x*3+1] shl 2 div 3;
    P2^[(h-y)*w+x*3+2]:=P2^[(h-y)*w+x*3+2] shl 2;
    end;
  If P1^[(h-y)*w+x*3+1]<>0 then
    begin;
    P2^[(h-y)*w+x*3+0]:=P2^[(h-y)*w+x*3+0] shl 1;
    P2^[(h-y)*w+x*3+1]:=P1^[(h-y)*w+x*3+1];
    P2^[(h-y)*w+x*3+2]:=P2^[(h-y)*w+x*3+2] shl 1;
    end;
  If P1^[(h-y)*w+x*3+2]<>0 then
    begin;
    P2^[(h-y)*w+x*3+0]:=P2^[(h-y)*w+x*3+0] shl 2;
    P2^[(h-y)*w+x*3+1]:=P2^[(h-y)*w+x*3+1] shl 2 div 3;
    P2^[(h-y)*w+x*3+2]:=P1^[(h-y)*w+x*3+2];
    end;

  // Картинка ЧБ?

  If IsGrayScale(Img1,P2) then
    begin;
    For x:=0 to Img2.Width-1 do For y:=0 to Img2.Height-1 do
      begin;
      c:=(P2^[(h-y)*w+x*3+0]+P2^[(h-y)*w+x*3+1]+P2^[(h-y)*w+x*3+2]) div 3;
      P2^[(h-y)*w+x*3+0]:=c;
      P2^[(h-y)*w+x*3+1]:=c;
      P2^[(h-y)*w+x*3+2]:=c;
      end;
    end;

  State:=2;
  ReDraw;
  end;
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
var I1,I2:TBitmap;
    Sum:Extended;
    h,w,x,y:Integer;
    P1,P2:PBigByteArray;
    DoIt:Boolean;
begin
If State=1 then I2:=Img1;
If State=2 then I2:=Img2;
If (State=2)or(State=1) then If Form1.OpenDialog1.Execute then
  If FileExists(Form1.OpenDialog1.FileName) then
    begin;
    I1:=TBitMap.Create;
    I1.LoadFromFile(Form1.OpenDialog1.FileName);

    DoIt:=False;
    if not((I1.Width=I2.Width)and(I1.Width=I2.Width)) then
      begin;
      if (Application.MessageBox(PChar('Размеры изображений не совпадают. Хотите продолжить на свой страх и риск?'),PChar('Ошибка'),MB_YESNO)=IDYES) then
        DoIt:=True;
      end
    else
      DoIt:=True;

    if DoIt then
      begin;
      Sum:=0;
      P1:=I1.ScanLine[I1.Height-1];
      P2:=I2.ScanLine[I2.Height-1];
      w:=I1.Width*3; w:=w+(4-w mod 4)mod 4;
      h:=I1.Height-1;
      For x:=0 to I1.Width-1 do For y:=0 to I1.Height-1 do
        Sum:=Sum+Sqr(P1^[(h-y)*w+x*3+0]-P2^[(h-y)*w+x*3+0])+Sqr(P1^[(h-y)*w+x*3+1]-P2^[(h-y)*w+x*3+1])+Sqr(P1^[(h-y)*w+x*3+2]-P2^[(h-y)*w+x*3+2]);
      PSNR:=10*Log10(1/Sum*I1.Width*I1.Height*3*255*255);
      Application.MessageBox(PChar('RGB PSNR = '+IntToStr(Trunc(PSNR))+'.'+IntToStr(Trunc(100*Frac(PSNR)))),PChar('PSNR'),MB_OK);
      {Form1.Label1.Caption:='PSNR = '+IntToStr(Trunc(PSNR))+'.'+IntToStr(Trunc(100*Frac(PSNR)));}
      end;
    I1.Free;
    end;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
var s:String;
begin
If (State=2)or(State=1) then If Form1.SaveDialog1.Execute then
  begin;
  S:=Form1.SaveDialog1.FileName;
  If UpperCase(ExtractFileExt(S))<>'.BMP' then S:=S+'.bmp';
  If State=1 then
    Img1.SaveToFile(S);
  If State=2 then
    Img2.SaveToFile(S);
  end;
end;

end.

