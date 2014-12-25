unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Utils, Math;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    Image1: TImage;
    GroupBox2: TGroupBox;
    Image2: TImage;
    OpenDialog1: TOpenDialog;
    GroupBox3: TGroupBox;
    Image3: TImage;
    SaveDialog1: TSaveDialog;
    GroupBox4: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    procedure Image1Click(Sender: TObject);
    procedure Image2Clic(Sender: TObject);
    procedure Image3Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type
  TParam=array[1..6]of Extended;   {E123 D123}
  TBigByteArray=Array[0..100000000]of byte;
  PBigByteArray=^TBigByteArray;
  TMyColor=Array[1..3]of Byte;
  TExtColor=Array[1..3]of Extended;

var
  Form1: TForm1;
  Img1,Img2,Img3:TBitmap;
  Param1,Param2:TParam;


implementation

{$R *.DFM}

Procedure ChangeToRGB(const c:TExtColor; var ce:TMyColor);
var c1,c2:Array[1..3] of Extended;
begin;
c1[1]:=(0.5774*c[1]+0.4082*c[2]+0.7071*c[3]);
c1[2]:=(0.5774*c[1]+0.4082*c[2]-0.7071*c[3]);
c1[3]:=(0.5774*c[1]-0.4082*2*c[2]);
c1[1]:=Power(10,c1[1]);
c1[2]:=Power(10,c1[2]);
c1[3]:=Power(10,c1[3]);
c2[1]:=(( 4.4679*c1[1]-3.5873*c1[2]+0.1193*c1[3])*255);
c2[2]:=((-1.2186*c1[1]+2.3809*c1[2]-0.1624*c1[3])*255);
c2[3]:=(( 0.0497*c1[1]-0.2439*c1[2]+1.2045*c1[3])*255);
c2[1]:=Min(Max(c2[1],0),255);
c2[2]:=Min(Max(c2[2],0),255);
c2[3]:=Min(Max(c2[3],0),255);
ce[1]:=Round(c2[1]);
ce[2]:=Round(c2[2]);
ce[3]:=Round(c2[3]);
end;

Procedure ChangeToLab(var c:TMyColor; var ce:TExtColor);
var c1,c2:Array[1..3] of Extended;
begin;
c[1]:=max(c[1],3);
c[2]:=max(c[2],3);
c[3]:=max(c[3],3);
c1[1]:=0.3811*c[1]/256+0.5783*c[2]/256+0.0402*c[3]/256;
c1[2]:=0.1967*c[1]/256+0.7244*c[2]/256+0.0782*c[3]/256;
c1[3]:=0.0241*c[1]/256+0.1288*c[2]/256+0.8444*c[3]/256;
c1[1]:=log10(c1[1]);
c1[2]:=log10(c1[2]);
c1[3]:=log10(c1[3]);
ce[1]:=(0.5774*(c1[1]+c1[2]+c1[3]));
ce[2]:=(0.4082*(c1[1]+c1[2]-2*c1[3]));
ce[3]:=(0.7071*(c1[1]-c1[2]));
end;


Procedure RecalcResult;
var w,h,x,y:Integer;
    P,P2:PBigByteArray;
    c:TMyColor;
    c1:TExtColor;
begin;
{c[1]:=1;
c[2]:=1;
c[3]:=1;
ChangeToLab(c,c1);
ChangeToRGB(c1,c);}

Img3.Free;
Img3:=TBitMap.Create;
Img3.PixelFormat:=pf24bit;
Img3.Height:=Img1.Height;
Img3.Width:=Img1.Width;
w:=Img1.Width*3; w:=w+(4-w mod 4)mod 4;
h:=Img1.Height-1;
P:=Img1.ScanLine[Img1.Height-1];
P2:=Img3.ScanLine[Img3.Height-1];
For x:=0 to Img1.Width-1 do for y:=0 to Img1.Height-1 do
  begin;
  c[1]:=P^[(h-y)*w+x*3+0];
  c[2]:=P^[(h-y)*w+x*3+1];
  c[3]:=P^[(h-y)*w+x*3+2];
  ChangeToLab(c,c1);
  If Form1.CheckBox1.Checked then
    begin;
    If Form1.CheckBox4.Checked then c1[1]:=Param2[1]+(c1[1]-Param1[1])*Param2[4]/Param1[4]
                               else c1[1]:=Param2[1]+(c1[1]-Param1[1]);
    end;
  If Form1.CheckBox2.Checked then
    begin;
    If Form1.CheckBox5.Checked then c1[2]:=Param2[2]+(c1[2]-Param1[2])*Param2[5]/Param1[5]
                               else c1[2]:=Param2[2]+(c1[2]-Param1[2]);
    end;
  If Form1.CheckBox3.Checked then
    begin;
    If Form1.CheckBox6.Checked then c1[3]:=Param2[3]+(c1[3]-Param1[3])*Param2[6]/Param1[6]
                               else c1[3]:=Param2[3]+(c1[3]-Param1[3]);
    end;
  ChangeToRGB(c1,c);
  P2^[(h-y)*w+x*3+0]:=c[1];
  P2^[(h-y)*w+x*3+1]:=c[2];
  P2^[(h-y)*w+x*3+2]:=c[3];
  end;
h:=min(Img3.Height,Round(Img3.Height*min(Form1.Image3.Height/Img3.Height, Form1.Image3.Width/Img3.Width)));
w:=min(Img3.Width,Round(Img3.Width*min(Form1.Image3.Height/Img3.Height, Form1.Image3.Width/Img3.Width)));
Form1.Image3.Canvas.Brush.Color:=clBtnFace;
Form1.Image3.Canvas.FillRect(Rect(0,0,Form1.Image3.Width,Form1.Image3.Height));
Form1.Image3.Canvas.StretchDraw(Rect((Form1.Image3.Width-w)div 2, (Form1.Image3.Height-h)div 2, (Form1.Image3.Width-w)div 2+w, (Form1.Image3.Height-h)div 2+h),Img3);
end;

Procedure Recalculate(const Bmp:TBitmap; var Pr:TParam);
var x,y:Integer;
    P:PBigByteArray;
    w,h:Longint;
    Color:TMyColor;
    c1:TExtColor;
begin;
w:=Bmp.Width*3; w:=w+(4-w mod 4)mod 4;
h:=Bmp.Height-1;
P:=Bmp.ScanLine[Bmp.Height-1];
FillChar(Pr,sizeof(Pr),0);
for x:=0 to Bmp.Width-1 do
  for y:=0 to Bmp.Height-1 do
    begin;
    color[1]:=P^[(h-y)*w+x*3+0];
    color[2]:=P^[(h-y)*w+x*3+1];
    color[3]:=P^[(h-y)*w+x*3+2];
    ChangeToLab(Color, C1);
    Pr[1]:=Pr[1]+c1[1];
    Pr[2]:=Pr[2]+c1[2];
    Pr[3]:=Pr[3]+c1[3];
    end;
Pr[1]:=Pr[1]/(Bmp.Width*Bmp.Height);
Pr[2]:=Pr[2]/(Bmp.Width*Bmp.Height);
Pr[3]:=Pr[3]/(Bmp.Width*Bmp.Height);
for x:=0 to Bmp.Width-1 do
  for y:=0 to Bmp.Height-1 do
    begin;
    color[1]:=P^[(h-y)*w+x*3+0];
    color[2]:=P^[(h-y)*w+x*3+1];
    color[3]:=P^[(h-y)*w+x*3+2];
    ChangeToLab(Color,c1);
    Pr[4]:=Pr[4]+sqr(c1[1]-Pr[1]);
    Pr[5]:=Pr[5]+sqr(c1[2]-Pr[2]);
    Pr[6]:=Pr[6]+sqr(c1[3]-Pr[3]);
    end;
Pr[4]:=Sqrt(Pr[4]/(Bmp.Width*Bmp.Height));
Pr[5]:=Sqrt(Pr[5]/(Bmp.Width*Bmp.Height));
Pr[6]:=Sqrt(Pr[6]/(Bmp.Width*Bmp.Height));
end;

Procedure Load(var Img:TBitmap; const FileName:String; var Pr:TParam; const Imag1:TImage);
var h,w:Longint;
begin;
Img.Free;
LoadImage(Img, Form1.OpenDialog1.FileName);
h:=min(Img.Height,Round(Img.Height*min(Imag1.Height/Img.Height, Imag1.Width/Img.Width)));
w:=min(Img.Width,Round(Img.Width*min(Imag1.Height/Img.Height, Imag1.Width/Img.Width)));
Imag1.Canvas.Brush.Color:=clBtnFace;
Imag1.Canvas.FillRect(Rect(0,0,Imag1.Width,Imag1.Height));
Imag1.Canvas.StretchDraw(Rect((Imag1.Width-w)div 2, (Imag1.Height-h)div 2, (Imag1.Width-w)div 2+w, (Imag1.Height-h)div 2+h),Img);
Img.PixelFormat:=pf24bit;
Recalculate(Img,Pr);
If (Img1<>nil) and (Img2<>nil) then
  ReCalcResult;
end;

procedure TForm1.Image1Click(Sender: TObject);
begin
If Form1.OpenDialog1.Execute then
  If FileExists(Form1.OpenDialog1.FileName) then
    Load(Img1,Form1.OpenDialog1.FileName,Param1,Form1.Image1);
end;

procedure TForm1.Image2Clic(Sender: TObject);
begin
If Form1.OpenDialog1.Execute then
  If FileExists(Form1.OpenDialog1.FileName) then
    Load(Img2,Form1.OpenDialog1.FileName,Param2,Form1.Image2);
end;

procedure TForm1.Image3Click(Sender: TObject);
var FN:String;
begin
If Img3<>nil then
If Form1.SaveDialog1.Execute then
  begin;
  FN:=Form1.SaveDialog1.FileName;
  If UpperCase(ExtractFileExt(FN))<>'.BMP' then
    FN:=FN+'.bmp';
  Img3.SaveToFile(FN);
  end;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
If (Img1<>nil) and (Img2<>nil) then
  ReCalcResult;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
If (Img1<>nil) and (Img2<>nil) then
  ReCalcResult;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
If (Img1<>nil) and (Img2<>nil) then
  ReCalcResult;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
If (Img1<>nil) and (Img2<>nil) then
  ReCalcResult;
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
If (Img1<>nil) and (Img2<>nil) then
  ReCalcResult;
end;

procedure TForm1.CheckBox6Click(Sender: TObject);
begin
If (Img1<>nil) and (Img2<>nil) then
  ReCalcResult;
end;

end.
