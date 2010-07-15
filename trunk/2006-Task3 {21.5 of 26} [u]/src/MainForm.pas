unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtDlgs, ImgList, ActnList, Menus, ComCtrls, ToolWin, ExtCtrls, Processing,
  StdCtrls, MyDialogBox, MyDialogBox2;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    FileOpenItem: TMenuItem;
    FileSaveItem: TMenuItem;
    N1: TMenuItem;
    FileExitItem: TMenuItem;
    Process1: TMenuItem;
    OpenDialog: TOpenPictureDialog;
    SaveDialog: TSavePictureDialog;
    ScrollBox1: TScrollBox;
    DocImage: TImage;
    Binarize1: TMenuItem;
    BinaryMedian1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    ListBox1: TListBox;
    Label1: TLabel;
    Dilate1: TMenuItem;
    Erode1: TMenuItem;
    Close1: TMenuItem;
    Open1: TMenuItem;
    Processing1: TMenuItem;
    Contrast1: TMenuItem;
    ScanBinaryImageforElephants1: TMenuItem;
    RemoveSmallObjects1: TMenuItem;
    Simple1: TMenuItem;
    Medium1and21: TMenuItem;
    Medium3and41: TMenuItem;
    Medium51: TMenuItem;
    procedure FileOpen1Execute(Sender: TObject);
    procedure FileExit1Execute(Sender: TObject);
    procedure FileSave1Execute(Sender: TObject);
    procedure Binarize1Click(Sender: TObject);
    procedure BinaryMedian1Click(Sender: TObject);
    procedure Erode1Click(Sender: TObject);
    procedure Dilate1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Contrast1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ScanBinaryImageforElephants1Click(Sender: TObject);
    procedure RemoveSmallObjects1Click(Sender: TObject);
    procedure Simple1Click(Sender: TObject);
    procedure Medium1and21Click(Sender: TObject);
    procedure Medium3and41Click(Sender: TObject);
    procedure Medium51Click(Sender: TObject);
  private
    LastOpened: TBitmap;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

{----------------------------------------------------------------------------}
procedure TForm1.FileSave1Execute(Sender: TObject);
begin
  if SaveDialog.Execute() then
  begin
    DocImage.Picture.SaveToFile(SaveDialog.FileName);
  end;
end;

{----------------------------------------------------------------------------}
procedure TForm1.FileOpen1Execute(Sender: TObject);
begin
  if OpenDialog.Execute() then
  begin
    try
      LastOpened.Free;
      LastOpened := TBitmap.Create;
      DocImage.Picture.LoadFromFile(OpenDialog.FileName);
      LastOpened.Assign(DocImage.Picture);
      DocImage.Width := DocImage.Picture.Width;
      DocImage.Height := DocImage.Picture.Height;
    except
      on E: Exception do ShowMessage(E.Message);
    end;
  end;
end;

{----------------------------------------------------------------------------}

procedure TForm1.FileExit1Execute(Sender: TObject);
begin
  Close();
end;

{----------------------------------------------------------------------------}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Processing.List := ListBox1.Items;
end;
{----------------------------------------------------------------------------}

procedure TForm1.Binarize1Click(Sender: TObject);
var
  TempImage : TBitmap;
begin
  TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap);
  Processing.Binarize(TempImage, DocImage.Picture.Bitmap);
  DocImage.Repaint();
end;

procedure TForm1.BinaryMedian1Click(Sender: TObject);
var
  TempImage : TBitmap;
begin
  TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap);
  Processing.BinaryMedian(TempImage, DocImage.Picture.Bitmap);
  DocImage.Repaint();
end;

procedure TForm1.Erode1Click(Sender: TObject);
var
  TempImage : TBitmap;
begin
  TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap);
  Processing.Erode(TempImage, DocImage.Picture.Bitmap, GetRadius);
  DocImage.Repaint();
end;

procedure TForm1.Dilate1Click(Sender: TObject);
var
  TempImage : TBitmap;
begin
  TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap);
  Processing.Dilate(TempImage, DocImage.Picture.Bitmap, GetRadius);
  DocImage.Repaint();
end;

procedure TForm1.Close1Click(Sender: TObject);
var
  TempImage : TBitmap;
  Radius : Integer;
begin
  TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap);
  Radius := GetRadius;
  Processing.Dilate(DocImage.Picture.Bitmap, TempImage, Radius);
  DocImage.Picture.Bitmap.Assign(TempImage);
  Processing.Erode(TempImage, DocImage.Picture.Bitmap, Radius);
  DocImage.Repaint();
end;

procedure TForm1.Open1Click(Sender: TObject);
var
  TempImage : TBitmap;
  Radius : Integer;
begin
  TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap);
  Radius := GetRadius;
  Processing.Erode(DocImage.Picture.Bitmap, TempImage, Radius);
  DocImage.Picture.Bitmap.Assign(TempImage);
  Processing.Dilate(TempImage, DocImage.Picture.Bitmap, Radius);
  DocImage.Repaint();
end;

procedure TForm1.Contrast1Click(Sender: TObject);
var
  TempImage : TBitmap;
begin
  TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap);
  Processing.EnhanceContrast(TempImage, DocImage.Picture.Bitmap);
  DocImage.Repaint();
end;

procedure TForm1.ScanBinaryImageforElephants1Click(Sender: TObject);
var
  TempImage : TBitmap;
begin
  TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap);
  DocImage.Picture.Bitmap.Assign(LastOpened);
  Processing.ScanForElephants(TempImage, LastOpened, DocImage.Picture.Bitmap);
  DocImage.Repaint();
end;

procedure TForm1.RemoveSmallObjects1Click(Sender: TObject);
var
  TempImage : TBitmap;
begin
  TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap);
  Processing.RemoveSmall(TempImage, DocImage.Picture.Bitmap, GetSquare);
  DocImage.Repaint();
end;

procedure TForm1.Simple1Click(Sender: TObject);
var
  TempImage : TBitmap;
begin
  TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.Binarize    (TempImage, DocImage.Picture.Bitmap);

  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.BinaryMedian(TempImage, DocImage.Picture.Bitmap);

  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.Dilate      (TempImage, DocImage.Picture.Bitmap, 3);

  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.Erode       (TempImage, DocImage.Picture.Bitmap, 3);

  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.BinaryMedian(TempImage, DocImage.Picture.Bitmap);

  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.RemoveSmall (TempImage, DocImage.Picture.Bitmap, 1000);
  
  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap);
  DocImage.Picture.Bitmap.Assign(LastOpened);
  Processing.ScanForElephants(TempImage, LastOpened, DocImage.Picture.Bitmap);
  DocImage.Repaint();
end;

procedure TForm1.Medium1and21Click(Sender: TObject);
var
  TempImage : TBitmap;
  TempImage2 : TBitmap;
begin
  TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.EnhanceContrast(DocImage.Picture.Bitmap, TempImage);
  TempImage2 := TBitmap.Create; TempImage2.Assign(TempImage);
  DocImage.Picture.Bitmap.Assign(TempImage); Processing.Binarize       (TempImage, DocImage.Picture.Bitmap);
  TempImage.Free;
  TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap);
  DocImage.Picture.Bitmap.Assign(LastOpened);
  Processing.ScanForElephants(TempImage, TempImage2, DocImage.Picture.Bitmap);
  DocImage.Repaint();
end;

procedure TForm1.Medium3and41Click(Sender: TObject);
var
  TempImage : TBitmap;
begin
  TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.Binarize    (TempImage, DocImage.Picture.Bitmap);

  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.Erode       (TempImage, DocImage.Picture.Bitmap, 2);

  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.RemoveSmall (TempImage, DocImage.Picture.Bitmap, 200);

  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.Dilate      (TempImage, DocImage.Picture.Bitmap, 4);

  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.Erode       (TempImage, DocImage.Picture.Bitmap, 2);

  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.RemoveSmall (TempImage, DocImage.Picture.Bitmap, 1200);
  
  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap);
  DocImage.Picture.Bitmap.Assign(LastOpened);
  Processing.ScanForElephants(TempImage, LastOpened, DocImage.Picture.Bitmap);
  DocImage.Repaint();
end;

procedure TForm1.Medium51Click(Sender: TObject);
var
  TempImage : TBitmap;
  TempImage2 : TBitmap;  
begin
  TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.EnhanceContrast(TempImage, DocImage.Picture.Bitmap);

  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.EnhanceContrast(TempImage, DocImage.Picture.Bitmap);

  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.EnhanceContrast(TempImage, DocImage.Picture.Bitmap);

  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.EnhanceContrast(TempImage, DocImage.Picture.Bitmap);

  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.EnhanceContrast(TempImage, DocImage.Picture.Bitmap);

  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.EnhanceContrast(TempImage, DocImage.Picture.Bitmap);

  TempImage2 := TBitmap.Create; TempImage2.Assign(DocImage.Picture.Bitmap);

  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.BinaryMedian   (TempImage, DocImage.Picture.Bitmap);

  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.Binarize       (TempImage, DocImage.Picture.Bitmap);

  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.RemoveSmall    (TempImage, DocImage.Picture.Bitmap, 20);

  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.Dilate         (TempImage, DocImage.Picture.Bitmap, 8);

  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap); Processing.Erode          (TempImage, DocImage.Picture.Bitmap, 8);

  TempImage.Free; TempImage := TBitmap.Create;
  TempImage.Assign(DocImage.Picture.Bitmap);
  DocImage.Picture.Bitmap.Assign(TempImage2);
  Processing.ScanForElephants(TempImage, TempImage2, DocImage.Picture.Bitmap);
  DocImage.Repaint();
end;

end.
