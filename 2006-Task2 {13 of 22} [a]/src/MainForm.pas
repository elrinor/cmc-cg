{ C_GRAPHICSMEDIA_LAB_DMOROZ_C
#/*******************************************************************************
# * (C) Copyright 2003/2004 by Vladimir Vezhnevets <vvp@graphics.cs.msu.su>     *
# *******************************************************************************/
}

unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtDlgs, ImgList, ActnList, Menus, ComCtrls, ToolWin, ExtCtrls, Processing,
  StdCtrls;

type
  TForm1 = class(TForm)
    DocImage: TImage;
    StatusBar: TStatusBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    FileOpenItem: TMenuItem;
    FileSaveItem: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    Process1: TMenuItem;
    Help1: TMenuItem;
    HelpAboutItem: TMenuItem;
    ActionList1: TActionList;
    FileOpen1: TAction;
    FileSave1: TAction;
    FileExit1: TAction;
    HelpAbout1: TAction;
    BoxFilter: TAction;
    Colorize: TAction;
    ImageList1: TImageList;
    OpenDialog: TOpenPictureDialog;
    SaveDialog: TSavePictureDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    Noise: TAction;
    Panel1: TPanel;
    Label1: TLabel;
    Radius: TEdit;
    Label2: TLabel;
    Might: TEdit;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    N10: TMenuItem;
    knearestneighbors1: TMenuItem;
    procedure FileOpen1Execute(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure HelpAbout1Execute(Sender: TObject);
    procedure FileExit1Execute(Sender: TObject);
    procedure BoxFilterExecute(Sender: TObject);
    procedure ColorizeExecute(Sender: TObject);
    procedure FileSave1Execute(Sender: TObject);
    procedure N2Click(Sender: TObject);
    procedure N3Click(Sender: TObject);
    procedure N5Click(Sender: TObject);
    procedure N6Click(Sender: TObject);
    procedure N4Click(Sender: TObject);
    procedure N7Click(Sender: TObject);
    procedure N8Click(Sender: TObject);
    procedure N9Click(Sender: TObject);
    procedure N10Click(Sender: TObject);
    procedure knearestneighbors1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses AboutBox;

{$R *.DFM}

{----------------------------------------------------------------------------}
procedure TForm1.FileOpen1Execute(Sender: TObject);
begin
  { Вызов диалога загрузки }
  if OpenDialog.Execute() then
  begin
    try
      DocImage.Picture.LoadFromFile(OpenDialog.FileName);
    except
      { Загрузить не удалось }
      on E: Exception do ShowMessage(E.Message);
    end;
  end;
end;

{----------------------------------------------------------------------------}
procedure TForm1.ToolButton2Click(Sender: TObject);
begin
end;

{----------------------------------------------------------------------------}
procedure TForm1.HelpAbout1Execute(Sender: TObject);
begin
  Form2.ShowModal();
end;

{----------------------------------------------------------------------------}

procedure TForm1.FileExit1Execute(Sender: TObject);
begin
  Close();
end;

{----------------------------------------------------------------------------}

procedure TForm1.BoxFilterExecute(Sender: TObject);
var
  TempImage : TBitmap;
begin
  { Создадим временное изображение }
  TempImage := TBitmap.Create;

  { Скопируем туда наше текущее изображение }
  TempImage.Assign(DocImage.Picture.Bitmap);

  { Отфильтруем }
  Processing.BoxFilter(TempImage, DocImage.Picture.Bitmap);

  { Покажем изменения}
  DocImage.Repaint();
end;

{----------------------------------------------------------------------------}

procedure TForm1.ColorizeExecute(Sender: TObject);
begin
  { Отфильтруем }
  Processing.Colorize(DocImage.Picture.Bitmap);

  { Покажем изменения}
  DocImage.Repaint();
end;

procedure TForm1.FileSave1Execute(Sender: TObject);
begin
  { Вызов диалога сохранения }
  if SaveDialog.Execute() then
  begin
    DocImage.Picture.SaveToFile(SaveDialog.FileName);
  end;
end;

procedure TForm1.N2Click(Sender: TObject);
begin
  Processing.WNoise(DocImage.Picture.Bitmap, StrToInt(Might.Text));
  DocImage.Repaint();
end;

procedure TForm1.N3Click(Sender: TObject);
begin
  Processing.INoise(DocImage.Picture.Bitmap, StrToInt(Might.Text));
  DocImage.Repaint();
end;

procedure TForm1.N5Click(Sender: TObject);
var
  TempImage : TBitmap;
begin
  TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap);
  Processing.Razmytiye(TempImage, DocImage.Picture.Bitmap);
  DocImage.Repaint();
end;

procedure TForm1.N6Click(Sender: TObject);
var
  TempImage : TBitmap;
begin
  TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap);
  Processing.NahozhdeniyeGranic(TempImage, DocImage.Picture.Bitmap);
  DocImage.Repaint();
end;

procedure TForm1.N4Click(Sender: TObject);
var
  TempImage : TBitmap;
begin
  TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap);
  Processing.PovysheniyeChetkosti(TempImage, DocImage.Picture.Bitmap);
  DocImage.Repaint();
end;

procedure TForm1.N7Click(Sender: TObject);
var
  TempImage : TBitmap;
begin
  if (DocImage.Picture.Bitmap.Width <> 0) and (DocImage.Picture.Bitmap.Height <> 0) then
  begin
    TempImage := TBitmap.Create;
    TempImage.Assign(DocImage.Picture.Bitmap);
    Processing.GaussovskoyeRazmytiyeOdnomernoye(TempImage, DocImage.Picture.Bitmap, StrToFloat(Radius.Text));
    DocImage.Repaint();
  end;
end;

procedure TForm1.N8Click(Sender: TObject);
var
  TempImage : TBitmap;
begin
  if (DocImage.Picture.Bitmap.Width <> 0) and (DocImage.Picture.Bitmap.Height <> 0) then
  begin
    TempImage := TBitmap.Create;
    TempImage.Assign(DocImage.Picture.Bitmap);
    Processing.GaussovskoyeRazmytiyeDvumernoye(TempImage, DocImage.Picture.Bitmap, StrToFloat(Radius.Text));
    DocImage.Repaint();
  end;
end;

procedure TForm1.N9Click(Sender: TObject);
var
  TempImage : TBitmap;
begin
  if (DocImage.Picture.Bitmap.Width <> 0) and (DocImage.Picture.Bitmap.Height <> 0) then
  begin
    TempImage := TBitmap.Create;
    TempImage.Assign(DocImage.Picture.Bitmap);
    Processing.MediannayaFiltraciya(TempImage, DocImage.Picture.Bitmap, StrToInt(Radius.Text));
    DocImage.Repaint();
  end;
end;

procedure TForm1.N10Click(Sender: TObject);
var
  TempImage : TBitmap;
begin
  if (DocImage.Picture.Bitmap.Width <> 0) and (DocImage.Picture.Bitmap.Height <> 0) then
  begin
    TempImage := TBitmap.Create;
    TempImage.Assign(DocImage.Picture.Bitmap);
    Processing.MediannayaFiltraciyaVectornaya(TempImage, DocImage.Picture.Bitmap, StrToInt(Radius.Text));
    DocImage.Repaint();
  end;  
end;

procedure TForm1.knearestneighbors1Click(Sender: TObject);
var
  TempImage : TBitmap;
begin
  if (DocImage.Picture.Bitmap.Width <> 0) and (DocImage.Picture.Bitmap.Height <> 0) then
  begin
    TempImage := TBitmap.Create;
    TempImage.Assign(DocImage.Picture.Bitmap);
    Processing.KNearestNeighborsFiltraciya(TempImage, DocImage.Picture.Bitmap, StrToInt(Radius.Text), StrToFloat(Might.Text));
    DocImage.Repaint();
  end;  
end;

end.
