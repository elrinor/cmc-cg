unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ExtCtrls, Utils, jpeg;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Filters1: TMenuItem;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    N5: TMenuItem;
    N7: TMenuItem;
    KNearestNeighbors1: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    N15: TMenuItem;
    Simple1: TMenuItem;
    N16: TMenuItem;
    L12: TMenuItem;
    N17: TMenuItem;
    N18: TMenuItem;
    N19: TMenuItem;
    Medium1: TMenuItem;
    Medium121: TMenuItem;
    Medium341: TMenuItem;
    Medium51: TMenuItem;
    N20: TMenuItem;
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure N5Click(Sender: TObject);
    procedure N7Click(Sender: TObject);
    procedure KNearestNeighbors1Click(Sender: TObject);
    procedure N9Click(Sender: TObject);
    procedure N10Click(Sender: TObject);
    procedure N11Click(Sender: TObject);
    procedure N12Click(Sender: TObject);
    procedure N13Click(Sender: TObject);
    procedure N14Click(Sender: TObject);
    procedure N15Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Simple1Click(Sender: TObject);
    procedure N16Click(Sender: TObject);
    procedure L12Click(Sender: TObject);
    procedure N17Click(Sender: TObject);
    procedure N18Click(Sender: TObject);
    procedure N19Click(Sender: TObject);
    procedure Medium121Click(Sender: TObject);
    procedure Medium341Click(Sender: TObject);
    procedure Medium51Click(Sender: TObject);
  private
    Unmodified:TBitmap;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses math, Unit2, Unit3;
{$R *.dfm}

procedure TForm1.Open1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    try
      Image1.Picture.Bitmap.LoadFromFile(OpenDialog1.FileName);
      Image1.Width:=Image1.Picture.Bitmap.Width;
      Image1.Height:=Image1.Picture.Bitmap.Height;
      Unmodified.Free;
      Unmodified:=TBitmap.Create;
      Unmodified.Assign(Image1.Picture.Bitmap);
    except
      on E:Exception do
        Application.MessageBox(PAnsiChar(E.Message),'Ошибка');
    end;
  end;
end;

procedure TForm1.Save1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    try
      if ExtractFileExt(SaveDialog1.FileName)<>'.bmp' then
        Image1.Picture.Bitmap.SaveToFile(SaveDialog1.FileName+'.bmp')
      else
        Image1.Picture.Bitmap.SaveToFile(SaveDialog1.FileName);
    except
      on E:Exception do
        Application.MessageBox(PAnsiChar(E.Message),'Ошибка');
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Unmodified.Free;
  Unmodified:=TBitmap.Create;
  Unmodified.Assign(Image1.Picture.Bitmap);
end;


procedure TForm1.Exit1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.N5Click(Sender: TObject);
begin
  Gauss(Image1.Picture.Bitmap);
  Image1.Repaint;
end;

procedure TForm1.N7Click(Sender: TObject);
begin
  Median(Image1.Picture.Bitmap);
  Image1.Repaint;
end;

procedure TForm1.KNearestNeighbors1Click(Sender: TObject);
begin
  KNN(Image1.Picture.Bitmap);
  Image1.Repaint;
end;

procedure TForm1.N9Click(Sender: TObject);
begin
  Binarize(Image1.Picture.Bitmap);
  Image1.Repaint;
end;

procedure TForm1.N10Click(Sender: TObject);
begin
  Dilatation(Image1.Picture.Bitmap);
  Image1.Repaint;
end;

procedure TForm1.N11Click(Sender: TObject);
begin
  Erosion(Image1.Picture.Bitmap);
  Image1.Repaint;
end;

procedure TForm1.N12Click(Sender: TObject);
begin
  DeDetalize(Image1.Picture.Bitmap);
  Image1.Repaint;
end;

procedure TForm1.N13Click(Sender: TObject);
var S:String;
begin
  FindElephants(Unmodified,Image1.Picture.Bitmap,S);
  Image1.Repaint;
  Application.MessageBox(PAnsiChar(S),'Результат',MB_OK);
end;

procedure TForm1.N14Click(Sender: TObject);
begin
  Opening(Image1.Picture.Bitmap);
  Image1.Repaint;
end;

procedure TForm1.N15Click(Sender: TObject);
begin
  Closing(Image1.Picture.Bitmap);
  Image1.Repaint;
end;


procedure TForm1.Simple1Click(Sender: TObject);
var S:String;
begin
  Binarize(Image1.Picture.Bitmap);
  DeDetalize(Image1.Picture.Bitmap);
  DoClosing(Image1.Picture.Bitmap,4);
  FindElephants(Unmodified,Image1.Picture.Bitmap,S);
  Image1.Repaint;
  Application.MessageBox(PAnsiChar(S),'Результат',MB_OK);
end;

procedure TForm1.N16Click(Sender: TObject);
begin
  LightNormalization(Image1.Picture.Bitmap);
  Image1.Repaint;
end;

procedure TForm1.L12Click(Sender: TObject);
begin
  L1LightNormalization(Image1.Picture.Bitmap);
  Image1.Repaint;
end;

procedure TForm1.N17Click(Sender: TObject);
begin
  BackGroundCompensation(Image1.Picture.Bitmap);
  Image1.Repaint;
end;

procedure TForm1.N18Click(Sender: TObject);
begin
  RemoveSmallRegions(Image1.Picture.Bitmap);
  Image1.Repaint;
end;

procedure TForm1.N19Click(Sender: TObject);
begin
  Contrast(Image1.Picture.Bitmap);
  Image1.Repaint;
end;

procedure TForm1.Medium121Click(Sender: TObject);
var S:String;
begin
  Contrast(Image1.Picture.Bitmap);
  Binarize(Image1.Picture.Bitmap);
  FindElephants(Unmodified,Image1.Picture.Bitmap,S);
  Image1.Repaint;
  Application.MessageBox(PAnsiChar(S),'Результат',MB_OK);
end;



procedure TForm1.Medium341Click(Sender: TObject);
var S:String;
begin
  BackGroundCompensation(Image1.Picture.Bitmap);
  Binarize(Image1.Picture.Bitmap);
  DoRemoveSmallRegions(Image1.Picture.Bitmap,150);
  DoClosing(Image1.Picture.Bitmap,4);
  FindElephants(Unmodified,Image1.Picture.Bitmap,S);
  Image1.Repaint;
  Application.MessageBox(PAnsiChar(S),'Результат',MB_OK);
end;

procedure TForm1.Medium51Click(Sender: TObject);
var S:String;
begin
  DoKNN(Image1.Picture.Bitmap,8,60);
  L1LightNormalization(Image1.Picture.Bitmap);
  BackGroundCompensation(Image1.Picture.Bitmap);
  Contrast(Image1.Picture.Bitmap);
  Contrast(Image1.Picture.Bitmap);
  Binarize(Image1.Picture.Bitmap);
  DoRemoveSmallRegions(Image1.Picture.Bitmap,20);
  DoClosing(Image1.Picture.Bitmap,2);
  DoRemoveSmallRegions(Image1.Picture.Bitmap,1000);
  FindElephants(Unmodified,Image1.Picture.Bitmap,S);
  Image1.Repaint;
  Application.MessageBox(PAnsiChar(S),'Результат',MB_OK);
end;

end.
