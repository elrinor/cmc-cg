// NAME: TUMAKOV KIRILL ALEKSANDROVICH, 203
// ASGN: N3
unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, Gauges, Recognition, RecognitionResult;

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
    SpeedButton11: TSpeedButton;
    Panel3: TPanel;
    Label1: TLabel;
    Gauge1: TGauge;
    SpeedButton17: TSpeedButton;
    SpeedButton18: TSpeedButton;
    SpeedButton19: TSpeedButton;
    SpeedButton20: TSpeedButton;
    SpeedButton21: TSpeedButton;
    SpeedButton22: TSpeedButton;
    SpeedButton23: TSpeedButton;
    SpeedButton24: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton25: TSpeedButton;
    procedure Image1Click(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure SpeedButton17Click(Sender: TObject);
    procedure SpeedButton19Click(Sender: TObject);
    procedure SpeedButton20Click(Sender: TObject);
    procedure SpeedButton21Click(Sender: TObject);
    procedure SpeedButton22Click(Sender: TObject);
    procedure SpeedButton23Click(Sender: TObject);
    procedure SpeedButton24Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton25Click(Sender: TObject);
  private
  public
    Source,Binary:TBitmap;
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

procedure TForm1.ReDrawImages;
begin;
  if ActiveImage=1 then
    begin;
    ReDrawImage(Image1,Source,true);
    ReDrawImage(Image2,Binary,false);
    end
  else
    begin;
    ReDrawImage(Image1,Source,false);
    ReDrawImage(Image2,Binary,true);
    end;
end;

procedure TForm1.SetActiveImage(const n:Integer);
begin;
  ActiveImage:=n;
  ReDrawImages;
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
  Source:=TBitmap.Create;
  Binary:=TBitmap.Create;
  Gauge:=Gauge1;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
if OpenDialog1.Execute then
  begin;
  SetFiltersEnabled(False);
  Source.LoadFromFile(OpenDialog1.FileName);
  ReDrawImages;
  SetFiltersEnabled(True);
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
  if ActiveImage=1 then
    Source.SaveToFile(FileName)
  else
    Binary.SaveToFile(FileName)
  end;
end;

procedure TForm1.SpeedButton11Click(Sender: TObject);
var Radius:Integer;
begin
  Radius:=RequestNumber('','Please enter Median Radius.',0);
  SetFiltersEnabled(False);
  if ActiveImage=1 then
    MedianFilter(Source,Radius)
  else
    MedianFilter(Binary,Radius);
  SetFiltersEnabled(True);
  ReDrawImages;
end;

procedure TForm1.SpeedButton17Click(Sender: TObject);
begin
  SetFiltersEnabled(False);
  Binary.Assign(Source);
  ConvertToBinary(Binary);
  SetFiltersEnabled(True);
  ReDrawImages;
end;

procedure TForm1.SpeedButton19Click(Sender: TObject);
var Radius:Integer;
begin
  Radius:=RequestNumber('','Please enter Expand Radius.',0);
  SetFiltersEnabled(False);
  BExpand(Binary,Radius);
  SetFiltersEnabled(True);
  ReDrawImages;
end;

procedure TForm1.SpeedButton20Click(Sender: TObject);
var Radius:Integer;
begin
  Radius:=RequestNumber('','Please enter Shrink Radius.',0);
  SetFiltersEnabled(False);
  BShrink(Binary,Radius);
  SetFiltersEnabled(True);
  ReDrawImages;
end;

procedure TForm1.SpeedButton21Click(Sender: TObject);
var Radius:Integer;
begin
  Radius:=RequestNumber('','Please enter Close Radius.',0);
  SetFiltersEnabled(False);
  BClose(Binary,Radius);
  SetFiltersEnabled(True);
  ReDrawImages;        
end;

procedure TForm1.SpeedButton22Click(Sender: TObject);
var Radius:Integer;
begin
  Radius:=RequestNumber('','Please enter Open Radius.',0);
  SetFiltersEnabled(False);
  BOpen(Binary,Radius);
  SetFiltersEnabled(True);
  ReDrawImages;
end;

procedure TForm1.SpeedButton23Click(Sender: TObject);
begin
  SetFiltersEnabled(False);
  AddContrast(Source,0.07);
  SetFiltersEnabled(True);
  ReDrawImages;
end;

procedure TForm1.SpeedButton24Click(Sender: TObject);
begin
  Form3.ListBox1.Clear;
  SetFiltersEnabled(False);
  Form3.ListBox1.Items:=Recognize(Binary,Source);
  SetFiltersEnabled(True);
  ReDrawImages;
  if Form3.ListBox1.Items.Count>0 then
    Form3.ShowModal;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
var Tmp:TBitmap;
begin
  Tmp:=TBitmap.Create;
  Tmp.Assign(Source);
  Form3.ListBox1.Clear;
  SetFiltersEnabled(False);
  MedianFilter(Source,1);
  Binary.Assign(Source);
  ConvertToBinary(Binary);
  BClose(Binary,4);
  Source.Assign(Tmp);
  Form3.ListBox1.Items:=Recognize(Binary,Source);
  SetFiltersEnabled(True);
  ReDrawImages;
  if Form3.ListBox1.Items.Count>0 then
    Form3.ShowModal;
  Tmp.Free;
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
var Tmp:TBitmap;
begin
  Tmp:=TBitmap.Create;
  Tmp.Assign(Source);
  Form3.ListBox1.Clear;
  SetFiltersEnabled(False);
  AddContrast(Source,0.04);
  Binary.Assign(Source);
  ConvertToBinary(Binary);
  Source.Assign(Tmp);
  Form3.ListBox1.Items:=Recognize(Binary,Source);
  SetFiltersEnabled(True);
  ReDrawImages;
  if Form3.ListBox1.Items.Count>0 then
    Form3.ShowModal;
  Tmp.Free;
end;

procedure TForm1.SpeedButton25Click(Sender: TObject);
var Tmp:TBitmap;
begin
  Tmp:=TBitmap.Create;
  Tmp.Assign(Source);
  Form3.ListBox1.Clear;
  SetFiltersEnabled(False);
  MedianFilter(Source,5);
  Binary.Assign(Source);
  ConvertToBinary(Binary);
  Source.Assign(Tmp);
  MedianFilter(Binary,1);
  MedianFilter(Binary,1);
  MedianFilter(Binary,1);
  MedianFilter(Binary,1);
  MedianFilter(Binary,1);
  BClose(Binary,5);
  MedianFilter(Binary,1);
  Form3.ListBox1.Items:=Recognize(Binary,Source);
  SetFiltersEnabled(True);
  ReDrawImages;
  if Form3.ListBox1.Items.Count>0 then
    Form3.ShowModal;
  Tmp.Free;
end;

end.
