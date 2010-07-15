// NAME: TUMAKOV KIRILL ALEKSANDROVICH, 203
// ASGN: N3
unit Filters;

interface
uses Graphics, Gauges, Recognition;

var Gauge: TGauge;

procedure MedianFilter(var Bmp:TBitmap; const R:Integer);

procedure ConvertToBinary(var Bmp:TBitmap);
procedure BExpand(var Bmp:TBitmap; R:Integer);
procedure BShrink(var Bmp:TBitmap; R:Integer);
procedure BClose(var Bmp:TBitmap; R:Integer);
procedure BOpen(var Bmp:TBitmap; R:Integer);
procedure AddContrast(var Bmp:TBitmap; IgnorePercent:Single);

implementation
uses SysUtils, Math, Forms;

const MaxKernelSize=127;

type TConvolution2D=record
  Size: Integer;
  Weights: array[-MaxKernelSize..MaxKernelSize, -MaxKernelSize..MaxKernelSize] of Single;
  end;

type TConvolution1D=record
  Size: Integer;
  Weights: array[-MaxKernelSize..MaxKernelSize] of Single;
  end;

procedure PrepareBmp(var Bmp:TBitmap; var w,h: Integer; var P:PByteArray);
begin;
  Bmp.PixelFormat:=pf24bit;
  w:=Bmp.Width*3; w:=w+(4-w mod 4)mod 4;
  h:=Bmp.Height-1;
  P:=Bmp.ScanLine[Bmp.Height-1];
end;

procedure GrayScale(var Bmp:TBitmap; var GrayScale:PByteArray); overload
var x,y:Integer;
    w,h:Integer;
    P:PByteArray;
begin;
  PrepareBmp(Bmp,w,h,P);
  GetMem(GrayScale, Bmp.Width*Bmp.Height);
  FillChar(GrayScale^, Bmp.Width*Bmp.Height, 0);
  for x:=0 to Bmp.Width-1 do for y:=0 to Bmp.Height-1 do
    GrayScale[Bmp.Width*y+x]:=Round(0.114*P^[(h-y)*w+x*3+0]+0.587*P^[(h-y)*w+x*3+1]+0.299*P^[(h-y)*w+x*3+2]);
end;

function Trim(const Min, Max, Number: Integer):Integer; overload;
begin;
  if Number>Max then
    Result:=Max
  else if Number<Min then
    Result:=Min
  else
    Result:=Number;
end;

procedure MedianFilter(var Bmp:TBitmap; const R:Integer);
var
  Bmp2:TBitmap;
  P,P2,YC:PByteArray;
  w,h,x,y,dx,dy,yy,xx:Integer;
  c:array[0..255,0..2] of Byte;
  Counts:array[0..255] of Integer;
  i, n:Integer;
begin;
  if (Bmp=nil) or (Bmp.Width=0) or (Bmp.Height=0) or (R=0) then
    exit;
  Bmp2:=TBitmap.Create;
  Bmp2.Width:=Bmp.Width;
  Bmp2.Height:=Bmp.Height;
  PrepareBmp(Bmp,w,h,P);
  PrepareBmp(Bmp2,w,h,P2);
  GrayScale(Bmp, YC);
  Gauge.MaxValue:=Bmp.Width-1; Gauge.Progress:=0;
  for x:=0 to Bmp.Width-1 do
    begin;
    Gauge.Progress:=Gauge.Progress+1; Application.ProcessMessages;
     for y:=0 to Bmp.Height-1 do
      begin;
      FillChar(Counts,sizeof(Counts),0);
      for dx:=-R to R do for dy:=-R to R do
        begin;
        xx:=Trim(0,Bmp.Width-1, x+dx);
        yy:=Trim(0,Bmp.Height-1,y+dy);
        if Counts[YC[Bmp.Width*yy+xx]]=0 then
          begin;
          c[YC[Bmp.Width*yy+xx],0]:=P^[(h-yy)*w+xx*3+0];
          c[YC[Bmp.Width*yy+xx],1]:=P^[(h-yy)*w+xx*3+1];
          c[YC[Bmp.Width*yy+xx],2]:=P^[(h-yy)*w+xx*3+2];
          Counts[YC[Bmp.Width*yy+xx]]:=1;
          end
        else
          Inc(Counts[YC[Bmp.Width*yy+xx]]);
        end;
      n:=0;
      for i:=0 to 255 do if Counts[i]<>0 then
        begin;
        n:=n+Counts[i];
        if n>sqr(2*R+1) div 2 then
          break;
        end;
      P2^[(h-y)*w+x*3+0]:=c[i,0];
      P2^[(h-y)*w+x*3+1]:=c[i,1];
      P2^[(h-y)*w+x*3+2]:=c[i,2];
      end;
    end;
  FreeMem(YC);
  Bmp.Free;
  Bmp:=Bmp2;
  Gauge.Progress:=0;
end;

procedure ConvertToBinary(var Bmp:TBitmap);
var
  w,h,n1,n2,r1,r2:Integer;
  P:PByteArray;
  C1,S1,C2,S2:array[0..2] of Integer;

procedure KStep;
var x,y,i:Integer;
begin;
  C1:=S1;C2:=S2;
  fillchar(S1,sizeof(S1),0);fillchar(S2,sizeof(S2),0);n1:=0;n2:=0;
  for x:=0 to Bmp.Width-1 do for y:=0 to Bmp.Height-1 do
    begin;
    if abs(P^[(h-y)*w+x*3+0]-C1[0])+abs(P^[(h-y)*w+x*3+1]-C1[1])+abs(P^[(h-y)*w+x*3+2]-C1[2])<
       abs(P^[(h-y)*w+x*3+0]-C2[0])+abs(P^[(h-y)*w+x*3+1]-C2[1])+abs(P^[(h-y)*w+x*3+2]-C2[2]) then
      begin;
      S1[0]:=S1[0]+P^[(h-y)*w+x*3+0];
      S1[1]:=S1[1]+P^[(h-y)*w+x*3+1];
      S1[2]:=S1[2]+P^[(h-y)*w+x*3+2];
      Inc(n1);
      end
    else
      begin;
      S2[0]:=S2[0]+P^[(h-y)*w+x*3+0];
      S2[1]:=S2[1]+P^[(h-y)*w+x*3+1];
      S2[2]:=S2[2]+P^[(h-y)*w+x*3+2];
      Inc(n2);
      end;
    end;
  for i:=0 to 2 do
    begin;
    S1[i]:=S1[i] div n1;
    S2[i]:=S2[i] div n2;
    end;
end;

var x,y:Integer;
begin;
  if (Bmp=nil) or (Bmp.Width=0) or (Bmp.Height=0) then
    exit;
  PrepareBmp(Bmp,w,h,P);
  Gauge.MaxValue:=9; Gauge.Progress:=1;

  C1[0]:=255;C1[1]:=255;C1[2]:=255;C2[0]:=0;C2[1]:=0;C2[2]:=0;
  for x:=0 to Bmp.Width-1 do for y:=0 to Bmp.Height-1 do
    begin;
    if P^[(h-y)*w+x*3+0]<C1[0] then C1[0]:=P^[(h-y)*w+x*3+0];
    if P^[(h-y)*w+x*3+1]<C1[1] then C1[1]:=P^[(h-y)*w+x*3+1];
    if P^[(h-y)*w+x*3+2]<C1[2] then C1[2]:=P^[(h-y)*w+x*3+2];
    if P^[(h-y)*w+x*3+0]>C2[0] then C2[0]:=P^[(h-y)*w+x*3+0];
    if P^[(h-y)*w+x*3+1]>C2[1] then C2[1]:=P^[(h-y)*w+x*3+1];
    if P^[(h-y)*w+x*3+2]>C2[2] then C2[2]:=P^[(h-y)*w+x*3+2];
    end;

  S1:=C1;S2:=C2;
  repeat
    Gauge.Progress:=Gauge.Progress+1; if Gauge.Progress=8 then Gauge.MaxValue:=Gauge.MaxValue+2; Application.ProcessMessages;
    KStep;
  until (abs(C1[0]-S1[0])+abs(C1[1]-S1[1])+abs(C1[2]-S1[2])+abs(C2[0]-S2[0])+abs(C2[1]-S2[1])+abs(C2[2]-S2[2])<10);
  Gauge.Progress:=Gauge.MaxValue;

  if(n1>n2) then
    begin;
    r1:=0;
    r2:=255;
    end
  else
    begin;
    r1:=255;
    r2:=0;
    end;
  C1:=S1;C2:=S2;
  for x:=0 to Bmp.Width-1 do for y:=0 to Bmp.Height-1 do
    begin;
    if abs(P^[(h-y)*w+x*3+0]-C1[0])+abs(P^[(h-y)*w+x*3+1]-C1[1])+abs(P^[(h-y)*w+x*3+2]-C1[2])<
       abs(P^[(h-y)*w+x*3+0]-C2[0])+abs(P^[(h-y)*w+x*3+1]-C2[1])+abs(P^[(h-y)*w+x*3+2]-C2[2]) then
      begin;
      P^[(h-y)*w+x*3+0]:=r1;
      P^[(h-y)*w+x*3+1]:=r1;
      P^[(h-y)*w+x*3+2]:=r1;
      end
    else
      begin;
      P^[(h-y)*w+x*3+0]:=r2;
      P^[(h-y)*w+x*3+1]:=r2;
      P^[(h-y)*w+x*3+2]:=r2;
      end;
    end;
  Gauge.Progress:=0;
end;

procedure BinaryExSh(var Bmp:TBitmap; R:Integer; C:Byte);
var
  Bmp2:TBitmap;
  P,P2:PByteArray;
  w,h,x,y,dx,dy,yy,xx:Integer;
  Convolution: TConvolution2D;
begin;
  if (Bmp=nil) or (Bmp.Width=0) or (Bmp.Height=0) or (R=0) then
    exit;
  if (R>MaxKernelSize) then
    R:=MaxKernelSize;
  Convolution.Size:=R;
  for x:=-R to R do for y:=-R to R do
    begin;
    if (sqr(x)+sqr(y)>sqr(R)) then
      Convolution.Weights[x,y]:=0
    else
      Convolution.Weights[x,y]:=1;
    end;

  Bmp2:=TBitmap.Create;
  Bmp2.Assign(Bmp);
  PrepareBmp(Bmp,w,h,P);
  PrepareBmp(Bmp2,w,h,P2);
  Gauge.MaxValue:=Bmp.Width-1; Gauge.Progress:=0;

  for x:=0 to Bmp.Width-1 do
    begin;
    Gauge.Progress:=Gauge.Progress+1; Application.ProcessMessages;
    for y:=0 to Bmp.Height-1 do
      begin;
      if (P^[(h-y)*w+x*3+0]=C) then
        begin;
        for dx:=-R to R do for dy:=-R to R do if Convolution.Weights[dx,dy]=1 then
          begin;
          xx:=Trim(0,Bmp.Width-1, x+dx);
          yy:=Trim(0,Bmp.Height-1,y+dy);
          P2^[(h-yy)*w+xx*3+0]:=C;
          P2^[(h-yy)*w+xx*3+1]:=C;
          P2^[(h-yy)*w+xx*3+2]:=C;
          end;
        end;
      end;
    end;
  Bmp.Free;
  Bmp:=Bmp2;
  Gauge.Progress:=0;
end;

procedure BExpand(var Bmp:TBitmap; R:Integer);
begin;
  BinaryExSh(Bmp,R,255);
end;

procedure BShrink(var Bmp:TBitmap; R:Integer);
begin;
  BinaryExSh(Bmp,R,0);
end;

procedure BClose(var Bmp:TBitmap; R:Integer);
begin;
  BExpand(Bmp,R);
  BShrink(Bmp,R);
end;

procedure BOpen(var Bmp:TBitmap; R:Integer);
begin;
  BShrink(Bmp,R);
  BExpand(Bmp,R);
end;

procedure AddContrast(var Bmp:TBitmap; IgnorePercent:Single);
var
  P:PByteArray;
  w,h,x,y,n,maxn,ming,maxg:Integer;
  Counts:array[0..255] of Integer;
begin;
  if IgnorePercent>0.49 then
    IgnorePercent:=0.49;
  if (Bmp=nil) or (Bmp.Width=0) or (Bmp.Height=0) then
    exit;
  PrepareBmp(Bmp,w,h,P);
  Gauge.MaxValue:=Bmp.Width*2-1; Gauge.Progress:=0;

  FillChar(Counts,sizeof(Counts),0);
  for x:=0 to Bmp.Width-1 do
    begin;
    Gauge.Progress:=Gauge.Progress+1; Application.ProcessMessages;
    for y:=0 to Bmp.Height-1 do
      Inc(Counts[Round(0.114*P^[(h-y)*w+x*3+0]+0.587*P^[(h-y)*w+x*3+1]+0.299*P^[(h-y)*w+x*3+2])]);
    end;

  maxn:=Round(Bmp.Width*Bmp.Height*IgnorePercent);
  n:=0;
  for ming:=0 to 255 do
    begin;
    n:=n+Counts[ming];
    if n>=maxn then break;
    end;
  n:=0;
  for maxg:=255 downto 0 do
    begin;
    n:=n+Counts[maxg];
    if n>=maxn then break;
    end;
  if ming>maxg then ming:=maxg;

  for x:=0 to Bmp.Width-1 do
    begin;
    Gauge.Progress:=Gauge.Progress+1; Application.ProcessMessages;
    for y:=0 to Bmp.Height-1 do
      begin;
      P^[(h-y)*w+x*3+0]:=Trim(0,255,((P^[(h-y)*w+x*3+0]-ming)*255) div (maxg-ming+1));
      P^[(h-y)*w+x*3+1]:=Trim(0,255,((P^[(h-y)*w+x*3+1]-ming)*255) div (maxg-ming+1));
      P^[(h-y)*w+x*3+2]:=Trim(0,255,((P^[(h-y)*w+x*3+2]-ming)*255) div (maxg-ming+1));
      end;
    end;
  Gauge.Progress:=0;
end;

end.

