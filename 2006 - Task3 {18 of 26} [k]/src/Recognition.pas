// NAME: TUMAKOV KIRILL ALEKSANDROVICH, 203
// ASGN: N3
unit Recognition;

interface
uses Graphics,Classes;
function Recognize(var BinBmp,Bmp:TBitmap):TStringList;


implementation
uses SysUtils,Math,Types,Filters,Forms;

const
  NoiseObjectSquare=350;
  UnmarkedObject=-666;
  DeletedObject=-37131;
  elGray=0;
  elUndefined=0;
  elRed=1;
  elYellow=2;
  elBlue=3;
  elViolet=4;
  elGreen=5;
  typeUndefined=0;
  typeElephant=1;
  typeBamboo=2;

type
  TConnectedObj=record
    Square:Integer;
    meanx,meany:Integer;
    m11,m02,m20,e,angle:Single;
    meanc:array[0..2] of Integer;
    Color:Byte;
    maxx,maxy,minx,miny:Single;
    Up:array[0..1] of Single;
    Processed:Boolean;
    ObjType:Integer;
    EatenBy:Integer;
    EatenN:Integer;
  end;
  TConnectedObjArray=array[1..32000] of TConnectedObj;
  PConnectedObjArray=^TConnectedObjArray;
  TXY=record
    x:Integer;
    y:Integer;
  end;
  TXYArray=array[0..32000] of TXY;
  PXYArray=^TXYArray;

var
  a:array[0..1000000,0..2] of Byte;

procedure PrepareBmp(var Bmp:TBitmap; var w,h: Integer; var P:PByteArray);
begin;
  Bmp.PixelFormat:=pf24bit;
  w:=Bmp.Width*3; w:=w+(4-w mod 4)mod 4;
  h:=Bmp.Height-1;
  P:=Bmp.ScanLine[Bmp.Height-1];
end;

function GetColor(r,g,b:Integer):Integer;
var mean:Integer;
begin;
  Result:=elGray;
  if max(max(abs(r-g),abs(b-g)),abs(r-b))>3 then
    begin;
    mean:=(max(max(r,g),b)+min(min(r,g),b)) div 2;
         if (r>mean) and (b>mean) then Result:=elViolet
    else if (r>mean) and (g>mean) then Result:=elYellow
    else if r>mean then                Result:=elRed
    else if b>mean then                Result:=elBlue
    else if g>mean then                Result:=elGreen
    else                               Result:=elUndefined;
    end;
end;

function GetType(e: Single):Integer;
begin;
  Result:=typeUndefined;
  if (e>1)and(e<2.7) then Result:=typeElephant
  else if e>14 then       Result:=typeBamboo;
end;

function GetElongation(m02,m20,m11:Single):Single;
begin;
Result:=(m02+m20+sqrt(sqr(m20-m02)+4*sqr(m11))) / (m02+m20-sqrt(sqr(m20-m02)+4*sqr(m11)));
end;


//============================================================================//
function FindConnectedObjects(var Bmp:TBitmap; var N:Integer):PIntegerArray;
var
  bin:PIntegerArray;
  bw,bh:Integer;
  Filler:Integer;
  Borderer:Integer;

function FillObj(const x,y:Integer):Integer;
var
  i:Integer;
  w1,w2,w3:PXYArray;
  c1,c2:Integer;
begin;
  GetMem(w1,Sizeof(TXY)*2*(bw+bh));
  GetMem(w2,Sizeof(TXY)*2*(bw+bh));
  c1:=1;c2:=0;w1[0].x:=x;w1[0].y:=y;
  Result:=1;
  bin[y*bw+x]:=Filler;
  repeat
    for i:=0 to c1-1 do
      begin;
      if (w1[i].x>0) and (bin[(w1[i].y)*bw+(w1[i].x-1)]=Borderer) then
        begin;
        bin[(w1[i].y)*bw+(w1[i].x-1)]:=Filler;
        w2[c2].x:=w1[i].x-1;
        w2[c2].y:=w1[i].y;
        c2:=c2+1;
        end;
      if (w1[i].y>0) and (bin[(w1[i].y-1)*bw+(w1[i].x)]=Borderer) then
        begin;
        bin[(w1[i].y-1)*bw+(w1[i].x)]:=Filler;
        w2[c2].x:=w1[i].x;
        w2[c2].y:=w1[i].y-1;
        c2:=c2+1;
        end;
      if (w1[i].x<bw-1) and (bin[(w1[i].y)*bw+(w1[i].x+1)]=Borderer) then
        begin;
        bin[(w1[i].y)*bw+(w1[i].x+1)]:=Filler;
        w2[c2].x:=w1[i].x+1;
        w2[c2].y:=w1[i].y;
        c2:=c2+1;
        end;
      if (w1[i].y<bh-1) and (bin[(w1[i].y+1)*bw+(w1[i].x)]=Borderer) then
        begin;
        bin[(w1[i].y+1)*bw+(w1[i].x)]:=Filler;
        w2[c2].x:=w1[i].x;
        w2[c2].y:=w1[i].y+1;
        c2:=c2+1;
        end;
      end;
    c1:=c2;c2:=0;
    Result:=Result+c1; 
    w3:=w1;w1:=w2;w2:=w3;
  until (c1=0);
  FreeMem(w1);
  FreeMem(w2);
end;

procedure MarkObj(const x,y:Integer);
begin;
  Filler:=N+1;
  Borderer:=UnmarkedObject;
  if FillObj(x,y)<NoiseObjectSquare then
    begin;
    Borderer:=Filler;
    Filler:=DeletedObject;
    FillObj(x,y);
    end
  else
    Inc(N);
end;

var
  P:PByteArray;
  w,h,x,y:Integer;
begin;
  PrepareBmp(Bmp,w,h,P);
  bw:=Bmp.Width;bh:=Bmp.Height;
  GetMem(bin,Sizeof(Integer)*bw*bh);

  for x:=0 to bw-1 do for y:=0 to bh-1 do
    begin;
    if P^[(h-y)*w+x*3+0]=0 then bin[y*bw+x]:=0
    else bin[y*bw+x]:=UnmarkedObject;
    end;

  N:=0;
  for x:=0 to bw-1 do for y:=0 to bh-1 do if bin[y*bw+x]=UnmarkedObject then
    MarkObj(x,y);

  for x:=0 to bw-1 do for y:=0 to bh-1 do if bin[y*bw+x]=DeletedObject then bin[y*bw+x]:=0;

  Result:=bin;
end;


//============================================================================//
function Recognize(var BinBmp,Bmp:TBitmap):TStringList;
var
  S:String;
  i,j,k,x,y,mindist,minj:Integer;
  w,h:Integer;
  bw,bh:Integer;
  P,P2:PByteArray;
  Obj,Elephants,Bamboos:PConnectedObjArray;
  N,eN,bN:Integer;
  bin:PIntegerArray;
  tmp:TConnectedObj;
begin;
  Result:=TStringList.Create;Result.Clear;
  if (BinBmp=nil) or (Bmp=nil) or (BinBmp.Width=0) or (BinBmp.Height=0) or (Bmp.Width=0) or (Bmp.Height=0) then
    begin;
    Result.Add('Error: either Binary or Source image does not exist.');
    exit;
    end;
  if (BinBmp.Width<>Bmp.Width) or (BinBmp.Height<>Bmp.Height) then
    begin;
    Result.Add('Error: the sizes of Binary and Source images aren''t equal.');
    exit;
    end;

  PrepareBmp(BinBmp,w,h,P);
  PrepareBmp(Bmp,w,h,P2);
  bw:=BinBmp.Width;bh:=BinBmp.Height;
  Gauge.MaxValue:=11; Gauge.Progress:=1;

  bin:=FindConnectedObjects(BinBmp,N);

  Gauge.Progress:=Gauge.Progress+1; Application.ProcessMessages;

  a[0][0]:=0;a[0][1]:=0;a[0][2]:=0;
  for i:=1 to N do
    begin;
    a[i][0]:=Random(256);
    a[i][1]:=Random(256);
    a[i][2]:=Random(256);
    end;
  for x:=0 to bw-1 do for y:=0 to bh-1 do
    begin;
    P^[(h-y)*w+x*3+0]:=a[bin[y*bw+x]][0];
    P^[(h-y)*w+x*3+1]:=a[bin[y*bw+x]][1];
    P^[(h-y)*w+x*3+2]:=a[bin[y*bw+x]][2];
    end;

  Gauge.Progress:=Gauge.Progress+1; Application.ProcessMessages;

  GetMem(Obj,Sizeof(TConnectedObj)*N);FillChar(Obj^,Sizeof(TConnectedObj)*N,0);
  for x:=0 to bw-1 do for y:=0 to bh-1 do
    begin;
    i:=bin[y*bw+x];
    if (i<>0) then
      begin;
      Inc(Obj^[i].Square);
      Obj^[i].meanx:=Obj^[i].meanx+x;
      Obj^[i].meany:=Obj^[i].meany+y;
      Obj^[i].meanc[0]:=Obj^[i].meanc[0]+P2^[(h-y)*w+x*3+0];
      Obj^[i].meanc[1]:=Obj^[i].meanc[1]+P2^[(h-y)*w+x*3+1];
      Obj^[i].meanc[2]:=Obj^[i].meanc[2]+P2^[(h-y)*w+x*3+2];
      end;
    end;

  Gauge.Progress:=Gauge.Progress+1; Application.ProcessMessages;

  for i:=1 to N do
    begin;
    Obj^[i].meanx:=Obj^[i].meanx div Obj^[i].Square;
    Obj^[i].meany:=Obj^[i].meany div Obj^[i].Square;
    Obj^[i].meanc[0]:=Obj^[i].meanc[0] div Obj^[i].Square;
    Obj^[i].meanc[1]:=Obj^[i].meanc[1] div Obj^[i].Square;
    Obj^[i].meanc[2]:=Obj^[i].meanc[2] div Obj^[i].Square;
    end;

  Gauge.Progress:=Gauge.Progress+1; Application.ProcessMessages;

  for x:=0 to bw-1 do for y:=0 to bh-1 do
    begin;
    i:=bin[y*bw+x];
    if (i<>0) then
      begin;
      Obj^[i].m02:=Obj^[i].m02+sqr(y-Obj^[i].meany);
      Obj^[i].m20:=Obj^[i].m20+sqr(x-Obj^[i].meanx);
      Obj^[i].m11:=Obj^[i].m11+(x-Obj^[i].meanx)*(y-Obj^[i].meany);
      end;
    end;

  Gauge.Progress:=Gauge.Progress+1; Application.ProcessMessages;

  eN:=0;bN:=0;
  for i:=1 to N do
    begin;
    Obj^[i].angle:=0.5*arctan2(2*Obj^[i].m11,Obj^[i].m20-Obj^[i].m02);
    Obj^[i].Up[0]:=cos(Obj^[i].angle);
    Obj^[i].Up[1]:=sin(Obj^[i].angle);
    Obj^[i].e:=GetElongation(Obj^[i].m02,Obj^[i].m20,Obj^[i].m11);
    Obj^[i].ObjType:=GetType(Obj^[i].e);
    if Obj^[i].ObjType=typeElephant then Inc(eN);
    if Obj^[i].ObjType=typeBamboo   then Inc(bN);
    Obj^[i].Color:=GetColor(Obj^[i].meanc[2],Obj^[i].meanc[1],Obj^[i].meanc[0]);
    end;

  Gauge.Progress:=Gauge.Progress+1; Application.ProcessMessages;

  for x:=0 to bw-1 do for y:=0 to bh-1 do
    begin;
    k:=bin[y*bw+x];
    if (k<>0) then
      begin;
      Obj^[k].maxy:=max(Obj^[k].maxy,(x-Obj^[k].meanx)*Obj^[k].Up[0]+(y-Obj^[k].meany)*Obj^[k].Up[1]);
      Obj^[k].maxx:=max(Obj^[k].maxx,(x-Obj^[k].meanx)*Obj^[k].Up[1]-(y-Obj^[k].meany)*Obj^[k].Up[0]);
      Obj^[k].miny:=min(Obj^[k].miny,(x-Obj^[k].meanx)*Obj^[k].Up[0]+(y-Obj^[k].meany)*Obj^[k].Up[1]);
      Obj^[k].minx:=min(Obj^[k].minx,(x-Obj^[k].meanx)*Obj^[k].Up[1]-(y-Obj^[k].meany)*Obj^[k].Up[0]);
      end;
    end;

  FreeMem(bin);

  GetMem(Elephants,Sizeof(TConnectedObj)*eN);FillChar(Elephants^,Sizeof(TConnectedObj)*eN,0);
  GetMem(Bamboos,Sizeof(TConnectedObj)*bN);FillChar(Bamboos^,Sizeof(TConnectedObj)*bN,0);
  eN:=0;bN:=0;
  for i:=1 to N do
    begin;
    if Obj^[i].ObjType=typeElephant then
      begin;
      Inc(eN);
      Elephants^[eN]:=Obj^[i];
      end;
    if Obj^[i].ObjType=typeBamboo  then
      begin;
      Inc(bN);
      Bamboos^[bN]:=Obj^[i];
      end;
    end;
  FreeMem(Obj);

  Gauge.Progress:=Gauge.Progress+1; Application.ProcessMessages;

  for i:=1 to bN do
    begin;
    mindist:=1000000000;
    for j:=1 to eN do if ((Elephants^[j].Color=elGray) or (Bamboos^[i].Color=Elephants^[j].Color))and
                         ((sqr(Bamboos^[i].meanx-Elephants^[j].meanx)+sqr(Bamboos^[i].meany-Elephants^[j].meany))<mindist) then
      begin;
      mindist:=sqr(Bamboos^[i].meanx-Elephants^[j].meanx)+sqr(Bamboos^[i].meany-Elephants^[j].meany);
      minj:=j;
      end;
    if mindist<1000000000 then
      begin;
      Bamboos^[i].EatenBy:=minj;
      Inc(Elephants^[minj].EatenN);
      end;
    end;

  Gauge.Progress:=Gauge.Progress+1; Application.ProcessMessages;

  Bmp.Canvas.Pen.Color:=255*256*256+255*256+0;Bmp.Canvas.Pen.Width:=3;
  for i:=1 to bN do
    begin;
    Bmp.Canvas.MoveTo(Bamboos^[i].meanx+Round(Bamboos^[i].Up[0]*Bamboos^[i].maxy+Bamboos^[i].Up[1]*Bamboos^[i].maxx),Bamboos^[i].meany+Round(Bamboos^[i].Up[1]*Bamboos^[i].maxy-Bamboos^[i].Up[0]*Bamboos^[i].maxx));
    Bmp.Canvas.LineTo(Bamboos^[i].meanx+Round(Bamboos^[i].Up[0]*Bamboos^[i].maxy+Bamboos^[i].Up[1]*Bamboos^[i].minx),Bamboos^[i].meany+Round(Bamboos^[i].Up[1]*Bamboos^[i].maxy-Bamboos^[i].Up[0]*Bamboos^[i].minx));
    Bmp.Canvas.LineTo(Bamboos^[i].meanx+Round(Bamboos^[i].Up[0]*Bamboos^[i].miny+Bamboos^[i].Up[1]*Bamboos^[i].minx),Bamboos^[i].meany+Round(Bamboos^[i].Up[1]*Bamboos^[i].miny-Bamboos^[i].Up[0]*Bamboos^[i].minx));
    Bmp.Canvas.LineTo(Bamboos^[i].meanx+Round(Bamboos^[i].Up[0]*Bamboos^[i].miny+Bamboos^[i].Up[1]*Bamboos^[i].maxx),Bamboos^[i].meany+Round(Bamboos^[i].Up[1]*Bamboos^[i].miny-Bamboos^[i].Up[0]*Bamboos^[i].maxx));
    Bmp.Canvas.LineTo(Bamboos^[i].meanx+Round(Bamboos^[i].Up[0]*Bamboos^[i].maxy+Bamboos^[i].Up[1]*Bamboos^[i].maxx),Bamboos^[i].meany+Round(Bamboos^[i].Up[1]*Bamboos^[i].maxy-Bamboos^[i].Up[0]*Bamboos^[i].maxx));
    end;
  Bmp.Canvas.Pen.Color:=0*256*256+255*256+255;Bmp.Canvas.Pen.Width:=3;
  for i:=1 to eN do
    begin;
    Bmp.Canvas.MoveTo(Elephants^[i].meanx+Round(Elephants^[i].Up[0]*Elephants^[i].maxy+Elephants^[i].Up[1]*Elephants^[i].maxx),Elephants^[i].meany+Round(Elephants^[i].Up[1]*Elephants^[i].maxy-Elephants^[i].Up[0]*Elephants^[i].maxx));
    Bmp.Canvas.LineTo(Elephants^[i].meanx+Round(Elephants^[i].Up[0]*Elephants^[i].maxy+Elephants^[i].Up[1]*Elephants^[i].minx),Elephants^[i].meany+Round(Elephants^[i].Up[1]*Elephants^[i].maxy-Elephants^[i].Up[0]*Elephants^[i].minx));
    Bmp.Canvas.LineTo(Elephants^[i].meanx+Round(Elephants^[i].Up[0]*Elephants^[i].miny+Elephants^[i].Up[1]*Elephants^[i].minx),Elephants^[i].meany+Round(Elephants^[i].Up[1]*Elephants^[i].miny-Elephants^[i].Up[0]*Elephants^[i].minx));
    Bmp.Canvas.LineTo(Elephants^[i].meanx+Round(Elephants^[i].Up[0]*Elephants^[i].miny+Elephants^[i].Up[1]*Elephants^[i].maxx),Elephants^[i].meany+Round(Elephants^[i].Up[1]*Elephants^[i].miny-Elephants^[i].Up[0]*Elephants^[i].maxx));
    Bmp.Canvas.LineTo(Elephants^[i].meanx+Round(Elephants^[i].Up[0]*Elephants^[i].maxy+Elephants^[i].Up[1]*Elephants^[i].maxx),Elephants^[i].meany+Round(Elephants^[i].Up[1]*Elephants^[i].maxy-Elephants^[i].Up[0]*Elephants^[i].maxx));
    end;
  
  Gauge.Progress:=Gauge.Progress+1; Application.ProcessMessages;

  for i:=1 to eN do
    begin;
    x:=Elephants^[i].meanx;
    y:=Elephants^[i].meany;
    while true do
      begin;
      mindist:=1000000000;
      for j:=1 to bN do if (Bamboos^[j].EatenBy=i) and (not Bamboos^[j].Processed) and (sqr(x-Bamboos^[j].meanx)+sqr(y-Bamboos^[j].meany)<mindist) then
        begin;
        mindist:=sqr(x-Bamboos^[j].meanx)+sqr(y-Bamboos^[j].meany);
        minj:=j;
        end;
      if mindist=1000000000 then break;
      Bmp.Canvas.Pen.Color:=0*256*256+0*256+255;
      Bmp.Canvas.Pen.Width:=3;
      Bmp.Canvas.MoveTo(x,y);
      Bmp.Canvas.LineTo(Bamboos^[minj].meanx,Bamboos^[minj].meany);
      x:=Bamboos^[minj].meanx;
      y:=Bamboos^[minj].meany;
      Bamboos^[minj].Processed:=true;
      end;
    end;

  Gauge.Progress:=Gauge.Progress+1; Application.ProcessMessages;

  for i:=1 to eN do for j:=i+1 to eN do if(Elephants^[i].meany*bw+Elephants^[i].meanx>Elephants^[j].meany*bw+Elephants^[j].meanx) then
    begin;
    tmp:=Elephants^[i];
    Elephants^[i]:=Elephants^[j];
    Elephants^[j]:=tmp;
    end;

  for i:=1 to eN do
    begin;
    S:='Elephant '+IntToStr(i)+' - '+IntToStr(Elephants^[i].EatenN);
    if (Elephants^[i].EatenN=1)then
      S:=S+' bamboo'
    else
      S:=S+' bamboos';
    Result.Add(S);
    end;

  Gauge.Progress:=0;
end;


end.
