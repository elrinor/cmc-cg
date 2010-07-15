unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ExtCtrls;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Filters1: TMenuItem;
    ImpulseNoise1: TMenuItem;
    WhiteNoise1: TMenuItem;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    L11: TMenuItem;
    KNearestNeighbors1: TMenuItem;
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure ImpulseNoise1Click(Sender: TObject);
    procedure WhiteNoise1Click(Sender: TObject);
    procedure N2Click(Sender: TObject);
    procedure N3Click(Sender: TObject);
    procedure N4Click(Sender: TObject);
    procedure N5Click(Sender: TObject);
    procedure N6Click(Sender: TObject);
    procedure N7Click(Sender: TObject);
    procedure L11Click(Sender: TObject);
    procedure KNearestNeighbors1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses math, Unit2, Unit3;
{$R *.dfm}

type TByteArray=array[0..0] of Byte;
     PByteArray=^TByteArray;
     TArray3x3=array[-1..1,-1..1] of Single;
     TCol=array[0..2] of Byte;
     TCol4=array[0..3] of Byte;
     TColArray=array[0..0] of TCol;
     PColArray=^TColArray;
     TCol4Array=array[0..0] of TCol4;
     PCol4Array=^TCol4Array;


procedure TForm1.Open1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    try
      Image1.Picture.Bitmap.LoadFromFile(OpenDialog1.FileName);
      Image1.Width:=Image1.Picture.Bitmap.Width;
      Image1.Height:=Image1.Picture.Bitmap.Height;
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

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Application.Terminate;
end;


procedure TForm1.ImpulseNoise1Click(Sender: TObject);
var BytesPerLine,x,y:Integer;
    pB:PByteArray;
    pr:Integer;
begin
  pr:=Round(Dialog('Вероятность искажения пиксела (1-100):',0)); if (pr=0) then Exit;
  Image1.Picture.Bitmap.PixelFormat:=pf24bit;
  BytesPerLine:=(Image1.Picture.Bitmap.Width*3+3) and -4;
  pB:=Image1.Picture.Bitmap.ScanLine[Image1.Picture.Bitmap.Height-1];
  for y:=0 to Image1.Picture.Bitmap.Height-1 do for x:=0 to Image1.Picture.Bitmap.Width-1 do
  begin
    if Random(100)<pr then
    begin
      pB[y*BytesPerLine+x*3  ]:=random(255);
      pB[y*BytesPerLine+x*3+1]:=random(255);
      pB[y*BytesPerLine+x*3+2]:=random(255);
    end;
  end;
  Image1.Repaint;
end;

procedure TForm1.WhiteNoise1Click(Sender: TObject);
var BytesPerLine,x,y:Integer;
    pB:PByteArray;
    isk:Integer;
    t:Integer;
begin
  isk:=Round(Dialog('Амплитуда шума (1-255):',0)); if (isk=0) then Exit;
  Image1.Picture.Bitmap.PixelFormat:=pf24bit;
  BytesPerLine:=(Image1.Picture.Bitmap.Width*3+3) and -4;
  pB:=Image1.Picture.Bitmap.ScanLine[Image1.Picture.Bitmap.Height-1];
  for y:=0 to Image1.Picture.Bitmap.Height-1 do for x:=0 to Image1.Picture.Bitmap.Width-1 do
  begin
    t:=pB[y*BytesPerLine+x*3  ]-random(isk)+random(isk);if(t<0) then t:=0; if(t>255) then t:=255;pB[y*BytesPerLine+x*3  ]:=t;
    t:=pB[y*BytesPerLine+x*3+1]-random(isk)+random(isk);if(t<0) then t:=0; if(t>255) then t:=255;pB[y*BytesPerLine+x*3+1]:=t;
    t:=pB[y*BytesPerLine+x*3+2]-random(isk)+random(isk);if(t<0) then t:=0; if(t>255) then t:=255;pB[y*BytesPerLine+x*3+2]:=t;
  end;
  Image1.Repaint;
end;

procedure Filter3(const bm2:TBitmap; const f:TArray3x3);
var BytesPerLine,x,y,dx,dy,newx,newy:Integer;
    pB,pB2:PByteArray;
    t:array[0..2] of Single;
    bm:TBitmap;
begin
  bm:=TBitmap.Create;bm.Assign(bm2);
  bm.PixelFormat:=pf24bit;
  BytesPerLine:=(bm.Width*3+3) and -4;
  pB:=bm.ScanLine[bm.Height-1];
  pB2:=bm2.ScanLine[bm2.Height-1];
  for y:=0 to bm.Height-1 do for x:=0 to bm.Width-1 do
  begin
    t[0]:=0;
    t[1]:=0;
    t[2]:=0;
    for dx:=-1 to 1 do for dy:=-1 to 1 do
    begin
      newx:=x+dx;if (newx<0) then newx:=0; if (newx>bm.Width-1) then newx:=bm.Width-1;
      newy:=y+dy;if (newy<0) then newy:=0; if (newy>bm.Height-1) then newy:=bm.Height-1;
      t[0]:=t[0]+pB[newy*BytesPerLine+newx*3  ]*f[dx][dy];
      t[1]:=t[1]+pB[newy*BytesPerLine+newx*3+1]*f[dx][dy];
      t[2]:=t[2]+pB[newy*BytesPerLine+newx*3+2]*f[dx][dy];
    end;
    if t[0]<0 then t[0]:=0;if t[0]>255 then t[0]:=255;
    if t[1]<0 then t[1]:=0;if t[1]>255 then t[1]:=255;
    if t[2]<0 then t[2]:=0;if t[2]>255 then t[2]:=255;
    pB2[y*BytesPerLine+x*3  ]:=Round(t[0]);
    pB2[y*BytesPerLine+x*3+1]:=Round(t[1]);
    pB2[y*BytesPerLine+x*3+2]:=Round(t[2]);
  end;
  bm.Free;
end;

procedure TForm1.N2Click(Sender: TObject);
const mask:TArray3x3=((-0.1,-0.2,-0.1),(-0.2,2.2,-0.2),(-0.1,-0.2,-0.1));
begin
  Filter3(Image1.Picture.Bitmap,mask);
  Image1.Repaint;
end;

procedure TForm1.N3Click(Sender: TObject);
const mask:TArray3x3=((1/15,2/15,1/15),(2/15,3/15,2/15),(1/15,2/15,1/15));
begin
  Filter3(Image1.Picture.Bitmap,mask);
  Image1.Repaint;
end;

procedure TForm1.N4Click(Sender: TObject);
const mask:TArray3x3=((0,-1,0),(-1,4,-1),(0,-1,0));
begin
  Filter3(Image1.Picture.Bitmap,mask);
  Image1.Repaint;
end;

procedure TForm1.N5Click(Sender: TObject);
var a:array[-200..200] of Single;
    power,Sum:Single;
    t:array[0..2] of Single;
    i,r,BytesPerLine,x,y,dx,dy,newx,newy:Integer;
    pB,pB2:PByteArray;
    bm:TBitmap;
begin
  power:=Dialog('Радиус размытия:',0); if (power=0) then Exit;
  Image1.Picture.Bitmap.PixelFormat:=pf24bit;
  bm:=TBitmap.Create;bm.Assign(Image1.Picture.Bitmap);
  BytesPerLine:=(Image1.Picture.Bitmap.Width*3+3) and -4;
  pB:=bm.ScanLine[bm.Height-1];
  pB2:=Image1.Picture.Bitmap.ScanLine[Image1.Picture.Bitmap.Height-1];
  r:=Round(power*3);if r<1 then r:=1; if r>200 then r:=200;
  for i:=-r to r do a[i]:=exp(-sqr(i/power)/2);
  Sum:=0; for i:=-r to r do Sum:=Sum+a[i];
  for i:=-r to r do a[i]:=a[i]/Sum;
  for y:=0 to Image1.Picture.Bitmap.Height-1 do for x:=0 to Image1.Picture.Bitmap.Width-1 do
  begin
    t[0]:=0;
    t[1]:=0;
    t[2]:=0;
    newy:=y;
    for dx:=-r to r do
    begin
      newx:=x+dx;if (newx<0) then newx:=0; if (newx>bm.Width-1) then newx:=bm.Width-1;
      t[0]:=t[0]+pB2[newy*BytesPerLine+newx*3  ]*a[dx];
      t[1]:=t[1]+pB2[newy*BytesPerLine+newx*3+1]*a[dx];
      t[2]:=t[2]+pB2[newy*BytesPerLine+newx*3+2]*a[dx];
    end;
    pB[y*BytesPerLine+x*3  ]:=Round(t[0]);
    pB[y*BytesPerLine+x*3+1]:=Round(t[1]);
    pB[y*BytesPerLine+x*3+2]:=Round(t[2]);
  end;
  for y:=0 to Image1.Picture.Bitmap.Height-1 do for x:=0 to Image1.Picture.Bitmap.Width-1 do
  begin
    t[0]:=0;
    t[1]:=0;
    t[2]:=0;
    newx:=x;
    for dy:=-r to r do
    begin
      newy:=y+dy;if (newy<0) then newy:=0; if (newy>bm.Height-1) then newy:=bm.Height-1;
      t[0]:=t[0]+pB[newy*BytesPerLine+newx*3  ]*a[dy];
      t[1]:=t[1]+pB[newy*BytesPerLine+newx*3+1]*a[dy];
      t[2]:=t[2]+pB[newy*BytesPerLine+newx*3+2]*a[dy];
    end;
    pB2[y*BytesPerLine+x*3  ]:=Round(t[0]);
    pB2[y*BytesPerLine+x*3+1]:=Round(t[1]);
    pB2[y*BytesPerLine+x*3+2]:=Round(t[2]);
  end;
  Image1.Repaint;
end;

procedure TForm1.N6Click(Sender: TObject);
var a:array[-200..200,-200..200] of Single;
    power,Sum:Single;
    t:array[0..2] of Single;
    r,BytesPerLine,x,y,dx,dy,newx,newy:Integer;
    pB,pB2:PByteArray;
    bm:TBitmap;
begin
  power:=Dialog('Радиус размытия:',0); if (power=0) then Exit;
  Image1.Picture.Bitmap.PixelFormat:=pf24bit;
  bm:=TBitmap.Create;bm.Assign(Image1.Picture.Bitmap);
  BytesPerLine:=(Image1.Picture.Bitmap.Width*3+3) and -4;
  pB:=bm.ScanLine[bm.Height-1];
  pB2:=Image1.Picture.Bitmap.ScanLine[Image1.Picture.Bitmap.Height-1];
  r:=Round(power*3);if r<1 then r:=1; if r>200 then r:=200;
  for x:=-r to r do for y:=-r to r do a[x,y]:=exp(-(sqr(x)+sqr(y))/(2*sqr(power)));
  Sum:=0; for x:=-r to r do for y:=-r to r do  Sum:=Sum+a[x,y];
  for x:=-r to r do for y:=-r to r do  a[x,y]:=a[x,y]/Sum;
  for y:=0 to Image1.Picture.Bitmap.Height-1 do for x:=0 to Image1.Picture.Bitmap.Width-1 do
  begin
    t[0]:=0;
    t[1]:=0;
    t[2]:=0;
    for dx:=-r to r do for dy:=-r to r do
    begin
      newx:=x+dx;if (newx<0) then newx:=0; if (newx>bm.Width-1) then newx:=bm.Width-1;
      newy:=y+dy;if (newy<0) then newy:=0; if (newy>bm.Height-1) then newy:=bm.Height-1;
      t[0]:=t[0]+pB[newy*BytesPerLine+newx*3  ]*a[dx,dy];
      t[1]:=t[1]+pB[newy*BytesPerLine+newx*3+1]*a[dx,dy];
      t[2]:=t[2]+pB[newy*BytesPerLine+newx*3+2]*a[dx,dy];
    end;
    pB2[y*BytesPerLine+x*3  ]:=Round(t[0]);
    pB2[y*BytesPerLine+x*3+1]:=Round(t[1]);
    pB2[y*BytesPerLine+x*3+2]:=Round(t[2]);
  end;
  Image1.Repaint;
end;

procedure TForm1.N7Click(Sender: TObject);
var t:PCol4Array;
    d,dd,c,n,i,j,r,BytesPerLine,x,y,dx,dy,newx,newy:Integer;
    pB,pB2:PByteArray;
    bm:TBitmap;
begin
  r:=Round(Dialog('Радиус медианы:',0)); if (r=0) then Exit;
  Image1.Picture.Bitmap.PixelFormat:=pf24bit;
  bm:=TBitmap.Create;bm.Assign(Image1.Picture.Bitmap);
  BytesPerLine:=(Image1.Picture.Bitmap.Width*3+3) and -4;
  pB:=bm.ScanLine[bm.Height-1];
  pB2:=Image1.Picture.Bitmap.ScanLine[Image1.Picture.Bitmap.Height-1];
  GetMem(t,4*(2*r+1)*(2*r+1));
  for y:=0 to Image1.Picture.Bitmap.Height-1 do for x:=0 to Image1.Picture.Bitmap.Width-1 do
  begin
    c:=0;
    for dx:=-r to r do for dy:=-r to r do
    begin
      newx:=x+dx;if (newx<0) then newx:=0; if (newx>bm.Width-1) then newx:=bm.Width-1;
      newy:=y+dy;if (newy<0) then newy:=0; if (newy>bm.Height-1) then newy:=bm.Height-1;
      t[c,0]:=pB[newy*BytesPerLine+newx*3  ];
      t[c,1]:=pB[newy*BytesPerLine+newx*3+1];
      t[c,2]:=pB[newy*BytesPerLine+newx*3+2];
      t[c,3]:=Round(0.114*pB^[newy*BytesPerLine+newx*3  ]+0.587*pB^[newy*BytesPerLine+newx*3+1]+0.299*pB^[newy*BytesPerLine+newx*3+2]);
      c:=c+1;
    end;
    d:=999999999;
    for i:=0 to c-1 do
    begin
      dd:=0;
      for j:=0 to c-1 do
        dd:=dd+abs(t[i,3]-t[j,3]);
      if dd<d then
      begin
        d:=dd;
        n:=i;
      end;
    end;
    pB2[y*BytesPerLine+x*3  ]:=t[n,0];
    pB2[y*BytesPerLine+x*3+1]:=t[n,1];
    pB2[y*BytesPerLine+x*3+2]:=t[n,2];
  end;
  FreeMem(t);
  Image1.Repaint;
end;

procedure TForm1.L11Click(Sender: TObject);
var t:PColArray;
    i,j,d,dd,n,c,r,BytesPerLine,x,y,dx,dy,newx,newy:Integer;
    pB,pB2:PByteArray;
    bm:TBitmap;
begin
  r:=Round(Dialog('Радиус медианы:',0)); if (r=0) then Exit;
  Image1.Picture.Bitmap.PixelFormat:=pf24bit;
  bm:=TBitmap.Create;bm.Assign(Image1.Picture.Bitmap);
  BytesPerLine:=(Image1.Picture.Bitmap.Width*3+3) and -4;
  pB:=bm.ScanLine[bm.Height-1];
  pB2:=Image1.Picture.Bitmap.ScanLine[Image1.Picture.Bitmap.Height-1];
  GetMem(t,3*(2*r+1)*(2*r+1));
  for y:=0 to Image1.Picture.Bitmap.Height-1 do for x:=0 to Image1.Picture.Bitmap.Width-1 do
  begin
    c:=0;
    for dx:=-r to r do for dy:=-r to r do
    begin
      newx:=x+dx;if (newx<0) then newx:=0; if (newx>bm.Width-1) then newx:=bm.Width-1;
      newy:=y+dy;if (newy<0) then newy:=0; if (newy>bm.Height-1) then newy:=bm.Height-1;
      t[c,0]:=pB[newy*BytesPerLine+newx*3  ];
      t[c,1]:=pB[newy*BytesPerLine+newx*3+1];
      t[c,2]:=pB[newy*BytesPerLine+newx*3+2];
      c:=c+1;
    end;
    d:=999999999;
    for i:=0 to c-1 do
    begin
      dd:=0;
      for j:=0 to c-1 do
        dd:=dd+abs(t[i,0]-t[j,0])+abs(t[i,1]-t[j,1])+abs(t[i,2]-t[j,2]);
      if dd<d then
      begin
        d:=dd;
        n:=i;
      end;
    end;
    pB2[y*BytesPerLine+x*3  ]:=t[n,0];
    pB2[y*BytesPerLine+x*3+1]:=t[n,1];
    pB2[y*BytesPerLine+x*3+2]:=t[n,2];
  end;
  FreeMem(t);
  Image1.Repaint;
end;

procedure TForm1.KNearestNeighbors1Click(Sender: TObject);
var t:array[0..2] of Single;
    r,BytesPerLine,x,y,dx,dy,newx,newy:Integer;
    Sum:Single;
    K:Single;
    pB,pB2:PByteArray;
    bm:TBitmap;
    sr,h:Real;
begin
  Dialog2('Радиус K nearest neighbors:','Уровень шума K nearest neighbors:',0,0,sr,h);
  r:=Round(sr); if(r=0) or (h=0) then exit;
  Image1.Picture.Bitmap.PixelFormat:=pf24bit;
  bm:=TBitmap.Create;bm.Assign(Image1.Picture.Bitmap);
  BytesPerLine:=(Image1.Picture.Bitmap.Width*3+3) and -4;
  pB:=bm.ScanLine[bm.Height-1];
  pB2:=Image1.Picture.Bitmap.ScanLine[Image1.Picture.Bitmap.Height-1];
  for y:=0 to Image1.Picture.Bitmap.Height-1 do for x:=0 to Image1.Picture.Bitmap.Width-1 do
  begin
    Sum:=0;
    t[0]:=0;
    t[1]:=0;
    t[2]:=0;
    for dx:=-r to r do for dy:=-r to r do
    begin
      newx:=x+dx;if (newx<0) then newx:=0; if (newx>bm.Width-1) then newx:=bm.Width-1;
      newy:=y+dy;if (newy<0) then newy:=0; if (newy>bm.Height-1) then newy:=bm.Height-1;
      K:=exp(-(sqr(pB^[newy*BytesPerLine+newx*3  ]-pB^[y*BytesPerLine+x*3  ])+sqr(pB^[newy*BytesPerLine+newx*3+1]-pB^[y*BytesPerLine+x*3+1])+sqr(pB^[newy*BytesPerLine+newx*3+2]-pB^[y*BytesPerLine+x*3+2]))/(2*sqr(h)));
      t[0]:=t[0]+pB[newy*BytesPerLine+newx*3  ]*K;
      t[1]:=t[1]+pB[newy*BytesPerLine+newx*3+1]*K;
      t[2]:=t[2]+pB[newy*BytesPerLine+newx*3+2]*K;
      Sum:=Sum+K;
    end;
    pB2[y*BytesPerLine+x*3  ]:=Round(t[0]/Sum);
    pB2[y*BytesPerLine+x*3+1]:=Round(t[1]/Sum);
    pB2[y*BytesPerLine+x*3+2]:=Round(t[2]/Sum);
  end;
  Image1.Repaint;
end;

end.
