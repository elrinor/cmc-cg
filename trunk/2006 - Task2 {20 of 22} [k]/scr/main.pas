// NAME: TUMAKOV KIRILL ALEKSANDROVICH, 203
// ASGN: N2
unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, Gauges;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    Splitter1: TSplitter;
    GroupBox2: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    SpeedButton1: TSpeedButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    Panel3: TPanel;
    Label1: TLabel;
    Gauge1: TGauge;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    procedure Image1Click(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure SpeedButton12Click(Sender: TObject);
    procedure SpeedButton13Click(Sender: TObject);
    procedure SpeedButton14Click(Sender: TObject);
    procedure SpeedButton15Click(Sender: TObject);
    procedure SpeedButton16Click(Sender: TObject);
  private
  public
    Bitmap:Array[1..2] of TBitmap;
    ActiveImage:Integer;
    procedure SetActiveImage(const n:Integer);
    procedure ReDrawImages;
    procedure SetFiltersEnabled(const Enabled:Boolean);
  end;

var
  Form1: TForm1;

implementation
uses Math, NumberDialog, Filters;
{$R *.dfm}

// ========================================================================== \\
// Main Logic
// ========================================================================== \\
procedure ReDrawImage(const Image:TImage; const Bmp:TBitmap; const Active:Boolean);
var h,w:Integer;
begin;
  Image.Picture.Bitmap.Width:=Image.Width;
  Image.Picture.Bitmap.Height:=Image.Height;
  Image.Canvas.Brush.Color:=clBtnFace;
  Image.Canvas.FillRect(Rect(0,0,Image.Width,Image.Height));
  if (Bmp<>nil) and (Bmp.Width<>0) and (Bmp.Height<>0) then
    begin;
    h:=min(Bmp.Height,Round(Bmp.Height*min(Image.Height/Bmp.Height, Image.Width/Bmp.Width)));
    w:=min(Bmp.Width, Round(Bmp.Width* min(Image.Height/Bmp.Height, Image.Width/Bmp.Width)));
    Image.Canvas.StretchDraw(Rect((Image.Width-w)div 2, (Image.Height-h)div 2, (Image.Width-w)div 2+w, (Image.Height-h)div 2+h),Bmp);
    end;
  if Active then
    begin;
    Image.Canvas.Brush.Color:=clBlack;
    Image.Canvas.FrameRect(Rect(0,0,Image.Width,Image.Height));
    end;
end;

procedure TForm1.SetActiveImage(const n:Integer);
begin;
  ActiveImage:=n;
  ReDrawImages;
end;

procedure TForm1.ReDrawImages;
begin;
  if ActiveImage=1 then
    begin;
    ReDrawImage(Image1,Bitmap[1],true);
    ReDrawImage(Image2,Bitmap[2],false);
    end
  else
    begin;
    ReDrawImage(Image1,Bitmap[1],false);
    ReDrawImage(Image2,Bitmap[2],true);
    end;
end;

procedure TForm1.SetFiltersEnabled(const Enabled:Boolean);
var i:Integer;
begin;
for i:=0 to ComponentCount-1 do if Components[i] is TSpeedButton then
  (Components[i] as TSpeedButton).Enabled:=Enabled;
end;

// ========================================================================== \\
// Event Handlers
// ========================================================================== \\
procedure TForm1.Image1Click(Sender: TObject);
begin
SetActiveImage(1);
end;

procedure TForm1.Image2Click(Sender: TObject);
begin
SetActiveImage(2);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
SetActiveImage(1);
Bitmap[1]:=TBitmap.Create;
Bitmap[2]:=TBitmap.Create;
Gauge:=Gauge1;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
if OpenDialog1.Execute then
  begin;
  Bitmap[ActiveImage].LoadFromFile(OpenDialog1.FileName);
  ReDrawImages;
  end;
end;

procedure TForm1.Splitter1Moved(Sender: TObject);
begin
  ReDrawImages;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  ReDrawImages;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
var FileName: String;
begin
if SaveDialog1.Execute then
  begin;
  FileName:=SaveDialog1.FileName;
  Delete(FileName, Length(FileName)-Length(ExtractFileExt(FileName))+1, Length(ExtractFileExt(FileName)));
  FileName:=FileName+'.bmp';
  Bitmap[ActiveImage].SaveToFile(FileName);
  end;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
  if (Bitmap[2]=nil) or (Bitmap[2].Width=0) or (Bitmap[2].Height=0) then
    Exit;
  Bitmap[1].Assign(Bitmap[2]);
  ReDrawImages;
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
begin
  if (Bitmap[1]=nil) or (Bitmap[1].Width=0) or (Bitmap[1].Height=0) then
    Exit;
  Bitmap[2].Assign(Bitmap[1]);
  ReDrawImages;  
end;

procedure TForm1.SpeedButton5Click(Sender: TObject);
var NoiseAmpl, NoiseProbability: Integer;
begin
  NoiseProbability:=RequestNumber('','Please enter Noise Probability (in percent).',0);
  NoiseAmpl:=RequestNumber('','Please enter Noise Amplitude.',0);
  SetFiltersEnabled(False);
  AddNoise(Bitmap[ActiveImage],NoiseAmpl,NoiseProbability);
  SetFiltersEnabled(True);
  ReDrawImages;
end;

procedure TForm1.SpeedButton6Click(Sender: TObject);
begin
  SetFiltersEnabled(False);
  Blur3x3(Bitmap[ActiveImage]);
  SetFiltersEnabled(True);
  ReDrawImages;
end;

procedure TForm1.SpeedButton7Click(Sender: TObject);
begin
  SetFiltersEnabled(False);
  Sharpen3x3(Bitmap[ActiveImage]);
  SetFiltersEnabled(True);
  ReDrawImages;
end;

procedure TForm1.SpeedButton8Click(Sender: TObject);
begin
  SetFiltersEnabled(False);
  FindEdges(Bitmap[ActiveImage]);
  SetFiltersEnabled(True);
  ReDrawImages;
end;

procedure TForm1.SpeedButton9Click(Sender: TObject);
var StandartDeviation:Single;
begin
  StandartDeviation:=RequestFloat('','Please enter Standart Deviation of the Gaussian Distribution.',0);
  SetFiltersEnabled(False);
  GaussianBlur(Bitmap[ActiveImage],StandartDeviation);
  SetFiltersEnabled(True);
  ReDrawImages;
end;

procedure TForm1.SpeedButton10Click(Sender: TObject);
var StandartDeviation:Single;
begin
  StandartDeviation:=RequestFloat('','Please enter Standart Deviation of the Gaussian Distribution.',0);
  SetFiltersEnabled(False);
  GaussianBlur2D(Bitmap[ActiveImage],StandartDeviation);
  SetFiltersEnabled(True);
  ReDrawImages;
end;

procedure TForm1.SpeedButton11Click(Sender: TObject);
var Radius:Integer;
begin
  Radius:=RequestNumber('','Please enter Median Radius.',0);
  SetFiltersEnabled(False);
  MedianFilter(Bitmap[ActiveImage],Radius);
  SetFiltersEnabled(True);
  ReDrawImages;
end;

procedure TForm1.SpeedButton12Click(Sender: TObject);
var Radius:Integer;
begin
  Radius:=RequestNumber('','Please enter Median Radius.',0);
  SetFiltersEnabled(False);
  VectorMedian(Bitmap[ActiveImage],Radius);
  SetFiltersEnabled(True);
  ReDrawImages;
end;

procedure TForm1.SpeedButton13Click(Sender: TObject);
var Radius:Integer;
    Strength:Single;
begin
  Radius:=RequestNumber('','Please enter KNN Radius.',0);
  Strength:=RequestFloat('','Please enter KNN Deviation (the "strength" of the filter, it is recommended to use value 30+).',0);
  SetFiltersEnabled(False);
  KNN(Bitmap[ActiveImage],Radius,Strength);
  SetFiltersEnabled(True);
  ReDrawImages;
end;

procedure TForm1.SpeedButton14Click(Sender: TObject);
var Radius:Integer;
    Strength:Single;
begin
  Radius:=RequestNumber('','Please enter NLM Radius.',0);
  Strength:=RequestFloat('','Please enter NLM Deviation (the "strength" of the filter, it is recommended to use value 10+).',0);
  SetFiltersEnabled(False);
  NLM(Bitmap[ActiveImage],Radius,Strength);
  SetFiltersEnabled(True);
  ReDrawImages;
end;

procedure TForm1.SpeedButton15Click(Sender: TObject);
begin
  SetFiltersEnabled(False);
  AutoNLM(Bitmap[ActiveImage]);
  SetFiltersEnabled(True);
  ReDrawImages;
end;

procedure TForm1.SpeedButton16Click(Sender: TObject);
begin
  SetFiltersEnabled(False);
  AutoKNN(Bitmap[ActiveImage]);
  SetFiltersEnabled(True);
  ReDrawImages;
end;

end.
