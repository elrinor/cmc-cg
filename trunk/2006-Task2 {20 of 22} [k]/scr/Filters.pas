// NAME: TUMAKOV KIRILL ALEKSANDROVICH, 203
// ASGN: N2
unit Filters;

interface
uses Graphics, Gauges;

var Gauge: TGauge;

procedure AddNoise(var Bmp:TBitmap; const NoiseAmpl, NoiseProbability: Integer);
procedure Blur3x3(var Bmp:TBitmap);
procedure Sharpen3x3(var Bmp:TBitmap);
procedure FindEdges(var Bmp:TBitmap);
procedure GaussianBlur(var Bmp:TBitmap; const R:Single);   
procedure GaussianBlur2D(var Bmp:TBitmap; const R:Single);
procedure MedianFilter(var Bmp:TBitmap; const R:Integer);
procedure VectorMedian(var Bmp:TBitmap; const R:Integer);
procedure KNN(var Bmp:TBitmap; R:Integer; Strength:Single);
procedure NLM(var Bmp:TBitmap; R:Integer; Strength:Single);
procedure AutoNLM(var Bmp:TBitmap);
procedure AutoKNN(var Bmp:TBitmap);

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

procedure GrayScale(var Bmp:TBitmap; var GrayScale:PByteArray; const border:Integer); overload
var x,y,xx,yy:Integer;
    w,h,grayw:Integer;
    P:PByteArray;
begin;
  PrepareBmp(Bmp,w,h,P);
  GetMem(GrayScale, (Bmp.Width+2*border)*(Bmp.Height+2*border));
  FillChar(GrayScale^, Bmp.Width*Bmp.Height, 0);
  grayw:=Bmp.Width+2*border;
  for x:=0 to Bmp.Width+2*border-1 do for y:=0 to Bmp.Height+2*border-1 do
    begin;
    xx:=min(max(x-border,0),Bmp.Width-1);
    yy:=min(max(y-border,0),Bmp.Height-1);    
    GrayScale[grayw*y+x]:=Round(0.114*P^[(h-yy)*w+xx*3+0]+0.587*P^[(h-yy)*w+xx*3+1]+0.299*P^[(h-yy)*w+xx*3+2]);
    end;
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

function Trim(const Min, Max, Number: Single):Single; overload;
begin;
  if Number>Max then
    Result:=Max
  else if Number<Min then
    Result:=Min
  else
    Result:=Number;
end;

function Noise(const c:Byte; const NoiseAmpl:Integer):Byte;
begin;
  Result:=Trim(0,255,c+Random(NoiseAmpl+1)-Random(NoiseAmpl+1));
end;

procedure AddNoise(var Bmp:TBitmap; const NoiseAmpl, NoiseProbability: Integer);
var P:PByteArray;
    w,h,x,y:Integer;
begin;
  if (Bmp=nil) or (Bmp.Width=0) or (Bmp.Height=0) then
    exit;
  PrepareBmp(Bmp,w,h,P);
  for x:=0 to Bmp.Width-1 do for y:=0 to Bmp.Height-1 do
    if Random(100)<NoiseProbability then
      begin;
      P^[(h-y)*w+x*3+0]:=Noise(P^[(h-y)*w+x*3+0],NoiseAmpl);
      P^[(h-y)*w+x*3+1]:=Noise(P^[(h-y)*w+x*3+1],NoiseAmpl);
      P^[(h-y)*w+x*3+2]:=Noise(P^[(h-y)*w+x*3+2],NoiseAmpl);
      end;
end;

procedure PerformConvolution(var Bmp:TBitmap; const Convolution: TConvolution2D); overload
var
  Bmp2:TBitmap;
  P,P2:PByteArray;
  w,h,x,y,dx,dy,yy,xx:Integer;
  c:array[0..2] of Single;
begin;
  if (Bmp=nil) or (Bmp.Width=0) or (Bmp.Height=0) then
    exit;
  Bmp2:=TBitmap.Create;
  Bmp2.Width:=Bmp.Width;
  Bmp2.Height:=Bmp.Height;
  PrepareBmp(Bmp,w,h,P);
  PrepareBmp(Bmp2,w,h,P2);
  Gauge.MaxValue:=Bmp.Width-1; Gauge.Progress:=0;
  for x:=0 to Bmp.Width-1 do
    begin;
    Gauge.Progress:=Gauge.Progress+1; Application.ProcessMessages;
    for y:=0 to Bmp.Height-1 do
      begin;
      c[0]:=0;c[1]:=0;c[2]:=0;
      for dx:=-Convolution.Size to Convolution.Size do for dy:=-Convolution.Size to Convolution.Size do
        begin;
        xx:=Trim(0,Bmp.Width-1, x+dx);
        yy:=Trim(0,Bmp.Height-1,y+dy);
        c[0]:=c[0]+Convolution.Weights[dx,dy]*P^[(h-yy)*w+xx*3+0];
        c[1]:=c[1]+Convolution.Weights[dx,dy]*P^[(h-yy)*w+xx*3+1];
        c[2]:=c[2]+Convolution.Weights[dx,dy]*P^[(h-yy)*w+xx*3+2];
        end;
      P2^[(h-y)*w+x*3+0]:=Round(Trim(0,255,c[0]));
      P2^[(h-y)*w+x*3+1]:=Round(Trim(0,255,c[1]));
      P2^[(h-y)*w+x*3+2]:=Round(Trim(0,255,c[2]));
      end;
    end;
  Bmp.Free;
  Bmp:=Bmp2;
  Gauge.Progress:=0;
end;

procedure PerformConvolution(var Bmp:TBitmap; const Convolution: TConvolution1D); overload
var
  Bmp2:TBitmap;
  P,P2:PByteArray;
  w,h,x,y,dx,dy,yy,xx:Integer;
  c:array[0..2] of Single;
begin;
  if (Bmp=nil) or (Bmp.Width=0) or (Bmp.Height=0) then
    exit;
  Bmp2:=TBitmap.Create;
  Bmp2.Width:=Bmp.Width;
  Bmp2.Height:=Bmp.Height;
  PrepareBmp(Bmp,w,h,P);
  PrepareBmp(Bmp2,w,h,P2);
  Gauge.MaxValue:=Bmp.Width*2-1; Gauge.Progress:=0;
  for x:=0 to Bmp.Width-1 do
    begin;
    Gauge.Progress:=Gauge.Progress+1; Application.ProcessMessages;
    for y:=0 to Bmp.Height-1 do
      begin;
      c[0]:=0;c[1]:=0;c[2]:=0;
      for dx:=-Convolution.Size to Convolution.Size do
        begin;
        xx:=Trim(0,Bmp.Width-1, x+dx);
        c[0]:=c[0]+Convolution.Weights[dx]*P^[(h-y)*w+xx*3+0];
        c[1]:=c[1]+Convolution.Weights[dx]*P^[(h-y)*w+xx*3+1];
        c[2]:=c[2]+Convolution.Weights[dx]*P^[(h-y)*w+xx*3+2];
        end;
      P2^[(h-y)*w+x*3+0]:=Round(Trim(0,255,c[0]));
      P2^[(h-y)*w+x*3+1]:=Round(Trim(0,255,c[1]));
      P2^[(h-y)*w+x*3+2]:=Round(Trim(0,255,c[2]));
      end;
    end;
  Bmp.Assign(Bmp2);
  PrepareBmp(Bmp,w,h,P);
  for x:=0 to Bmp.Width-1 do
    begin;
    Gauge.Progress:=Gauge.Progress+1; Application.ProcessMessages;
    for y:=0 to Bmp.Height-1 do
      begin;
      c[0]:=0;c[1]:=0;c[2]:=0;
      for dy:=-Convolution.Size to Convolution.Size do
        begin;
        yy:=Trim(0,Bmp.Height-1, y+dy);
        c[0]:=c[0]+Convolution.Weights[dy]*P^[(h-yy)*w+x*3+0];
        c[1]:=c[1]+Convolution.Weights[dy]*P^[(h-yy)*w+x*3+1];
        c[2]:=c[2]+Convolution.Weights[dy]*P^[(h-yy)*w+x*3+2];
        end;
      P2^[(h-y)*w+x*3+0]:=Round(Trim(0,255,c[0]));
      P2^[(h-y)*w+x*3+1]:=Round(Trim(0,255,c[1]));
      P2^[(h-y)*w+x*3+2]:=Round(Trim(0,255,c[2]));
      end;
    end;
  Bmp.Free;
  Bmp:=Bmp2;
  Gauge.Progress:=0;
end;

procedure Blur3x3(var Bmp:TBitmap);
var Convolution:TConvolution2D;
begin;
FillChar(Convolution,SizeOf(Convolution),0);
Convolution.Size:=1;
Convolution.Weights[ 0, 0]:=3/15;
Convolution.Weights[ 1, 0]:=2/15;
Convolution.Weights[-1, 0]:=2/15;
Convolution.Weights[ 0, 1]:=2/15;
Convolution.Weights[ 0,-1]:=2/15;
Convolution.Weights[ 1, 1]:=1/15;
Convolution.Weights[-1, 1]:=1/15;
Convolution.Weights[-1,-1]:=1/15;
Convolution.Weights[ 1,-1]:=1/15;
PerformConvolution(Bmp,Convolution);
end;

procedure Sharpen3x3(var Bmp:TBitmap);
var Convolution:TConvolution2D;
begin;
FillChar(Convolution,SizeOf(Convolution),0);
Convolution.Size:=1;
Convolution.Weights[ 0, 0]:=22/10;
Convolution.Weights[ 1, 0]:=-2/10;
Convolution.Weights[-1, 0]:=-2/10;
Convolution.Weights[ 0, 1]:=-2/10;
Convolution.Weights[ 0,-1]:=-2/10;
Convolution.Weights[ 1, 1]:=-1/10;
Convolution.Weights[-1, 1]:=-1/10;
Convolution.Weights[-1,-1]:=-1/10;
Convolution.Weights[ 1,-1]:=-1/10;
PerformConvolution(Bmp,Convolution);
end;

procedure FindEdges(var Bmp:TBitmap);
var Convolution: TConvolution2D;
begin;
FillChar(Convolution,SizeOf(Convolution),0);
Convolution.Size:=1;
Convolution.Weights[ 0, 0]:=4;
Convolution.Weights[ 1, 0]:=-1;
Convolution.Weights[-1, 0]:=-1;
Convolution.Weights[ 0, 1]:=-1;
Convolution.Weights[ 0,-1]:=-1;
PerformConvolution(Bmp,Convolution);
end;

procedure GaussianBlur(var Bmp:TBitmap; const R:Single);
var
  Convolution: TConvolution1D;
  i:Integer;
  Sum:Single;
begin;
  if R=0 then exit;
  FillChar(Convolution,SizeOf(Convolution),0);
  Convolution.Size:=min(max(Trunc(3*R),1),MaxKernelSize);
  for i:=-Convolution.Size to Convolution.Size do
    Convolution.Weights[i]:=exp(-sqr(i/R)/2);
  Sum:=0;
  for i:=-Convolution.Size to Convolution.Size do
    Sum:=Sum+Convolution.Weights[i];
  for i:=-Convolution.Size to Convolution.Size do
    Convolution.Weights[i]:=Convolution.Weights[i]/Sum;
  PerformConvolution(Bmp,Convolution);
end;

procedure GaussianBlur2D(var Bmp:TBitmap; const R:Single);
var
  Convolution: TConvolution2D;
  x,y:Integer;
  Sum:Single;
begin;
  if R=0 then exit;
  FillChar(Convolution,SizeOf(Convolution),0);
  Convolution.Size:=min(max(Trunc(3*R),1),MaxKernelSize);
  for x:=-Convolution.Size to Convolution.Size do for y:=-Convolution.Size to Convolution.Size do
    Convolution.Weights[x,y]:=exp(-(sqr(x)+sqr(y))/(2*sqr(R)));
  Sum:=0;
  for x:=-Convolution.Size to Convolution.Size do for y:=-Convolution.Size to Convolution.Size do
    Sum:=Sum+Convolution.Weights[x,y];
  for x:=-Convolution.Size to Convolution.Size do for y:=-Convolution.Size to Convolution.Size do
    Convolution.Weights[x,y]:=Convolution.Weights[x,y]/Sum;
  PerformConvolution(Bmp,Convolution);
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

procedure VectorMedian(var Bmp:TBitmap; const R:Integer);
var
  Bmp2:TBitmap;
  w,h,x,y,yy,xx,i,j:Integer;
  C,P,P2:PByteArray;
  Count,Dist,MinDist,BestN:Integer;
begin;
  if (Bmp=nil) or (Bmp.Width=0) or (Bmp.Height=0) or (R=0) then
    exit;
  Bmp2:=TBitmap.Create;
  Bmp2.Width:=Bmp.Width;
  Bmp2.Height:=Bmp.Height;
  PrepareBmp(Bmp,w,h,P);
  PrepareBmp(Bmp2,w,h,P2);
  GetMem(C, 3*sqr(2*R+1));
  Gauge.MaxValue:=Bmp.Width-1; Gauge.Progress:=0;
  for x:=0 to Bmp.Width-1 do
    begin;
    Gauge.Progress:=Gauge.Progress+1; Application.ProcessMessages;
    for y:=0 to Bmp.Height-1 do
      begin;
      Count:=0;
      for xx:=max(x-R,0) to min(x+R,Bmp.Width-1) do for yy:=max(y-R,0) to min(y+R,Bmp.Height-1) do
        begin;
        C[Count*3+0]:=P^[(h-yy)*w+xx*3+0];
        C[Count*3+1]:=P^[(h-yy)*w+xx*3+1];
        C[Count*3+2]:=P^[(h-yy)*w+xx*3+2];
        Inc(Count);
        end;
      MinDist:=2000000000;
      for i:=0 to Count-1 do
        begin;
        Dist:=0;
        for j:=0 to Count-1 do
          begin;
          Dist:=Dist+abs(C[i*3+0]-C[j*3+0])+abs(C[i*3+1]-C[j*3+1])+abs(C[i*3+2]-C[j*3+2]);
          if Dist>=MinDist then
            Break;
          end;
        if Dist<MinDist then
          begin;
          MinDist:=Dist;
          BestN:=i;
          end;
        if MinDist=0 then
          Break;
        end;
      P2^[(h-y)*w+x*3+0]:=C[BestN*3+0];
      P2^[(h-y)*w+x*3+1]:=C[BestN*3+1];
      P2^[(h-y)*w+x*3+2]:=C[BestN*3+2];
      end;
    end;

  FreeMem(C);
  Bmp.Free;
  Bmp:=Bmp2;
  Gauge.Progress:=0;
end;

procedure KNN(var Bmp:TBitmap; R:Integer; Strength:Single);
var
  Bmp2:TBitmap;
  w,h,x,y,xx,yy,nearness:Integer;
  P,P2:PByteArray;
  Exps:array[0..255] of Single;
  C:array[0..2] of Single;
  Norm:Single;
begin;
  if (Bmp=nil) or (Bmp.Width=0) or (Bmp.Height=0) or (R=0) or (Strength=0) then
    exit;
  Bmp2:=TBitmap.Create;
  Bmp2.Width:=Bmp.Width;
  Bmp2.Height:=Bmp.Height;
  PrepareBmp(Bmp,w,h,P);
  PrepareBmp(Bmp2,w,h,P2);
  Gauge.MaxValue:=Bmp.Width; Gauge.Progress:=1;
  for x:=0 to 255 do
    Exps[x]:=exp(-sqr(x)/sqr(Strength));
  for x:=0 to Bmp.Width-1 do
    begin;
    Gauge.Progress:=Gauge.Progress+1; Application.ProcessMessages;
    for y:=0 to Bmp.Height-1 do
      begin;
      Norm:=0; c[0]:=0; c[1]:=0; c[2]:=0;
      for xx:=max(x-R,0) to min(x+R,Bmp.Width-1) do for yy:=max(y-R,0) to min(y+R,Bmp.Height-1) do
        begin;
        nearness:=(abs(P^[(h-yy)*w+xx*3+0]-P^[(h-y)*w+x*3+0])+abs(P^[(h-yy)*w+xx*3+1]-P^[(h-y)*w+x*3+1])+abs(P^[(h-yy)*w+xx*3+2]-P^[(h-y)*w+x*3+2])) div 3;
        Norm:=Norm+Exps[nearness];
        C[0]:=C[0]+P^[(h-yy)*w+xx*3+0]*Exps[nearness];
        C[1]:=C[1]+P^[(h-yy)*w+xx*3+1]*Exps[nearness];
        C[2]:=C[2]+P^[(h-yy)*w+xx*3+2]*Exps[nearness];
        end;
      P2^[(h-y)*w+x*3+0]:=Round(C[0]/Norm);
      P2^[(h-y)*w+x*3+1]:=Round(C[1]/Norm);
      P2^[(h-y)*w+x*3+2]:=Round(C[2]/Norm);
      end;
    end;
  Bmp.Free;
  Bmp:=Bmp2;
  Gauge.Progress:=0;
end;

function SAD8x8_SSE(const pSrc: PByteArray; const pDst: PByteArray; const uWidth: Cardinal): Cardinal;
begin;
	asm
    push esi
    push edi
    push ebx

		mov		esi, pSrc		// ds:[esi] points to the source image block
		mov		edi, pDst		// ds:[edi] points to the destination image block
		mov		ebx, uWidth	// ebx = uWidth
		mov		edx, ebx		// edx = uWidth
		shl		edx, 1			// edx = uWidth * 2

		// Load source rows in mm registers
		movq	mm0, [esi]
		movq	mm1, [esi+ebx]
		add		esi, edx
		movq	mm2, [esi]
		movq	mm3, [esi+ebx]
		add		esi, edx
		movq	mm4, [esi]
		movq	mm5, [esi+ebx]
		add		esi, edx
		movq	mm6, [esi]
		movq	mm7, [esi+ebx]

		// Calculate SADs with destination rows
		psadbw	mm0, [edi]
		psadbw	mm1, [edi+ebx]
		add		edi, edx
		psadbw	mm2, [edi]
		psadbw	mm3, [edi+ebx]
		add		edi, edx
		psadbw	mm4, [edi]
		psadbw	mm5, [edi+ebx]
		add		edi, edx
		psadbw	mm6, [edi]
		psadbw	mm7, [edi+ebx]

		// Sum all SADs
		paddusw	mm0, mm1
		paddusw	mm0, mm2
		paddusw	mm0, mm3
		paddusw	mm0, mm4
		paddusw	mm0, mm5
		paddusw	mm0, mm6
		paddusw	mm0, mm7

		movd	Result, mm0	// store sum
		emms					    // empty MMX state

    pop ebx
    pop edi
    pop esi
	end;
end;


procedure NLM(var Bmp:TBitmap; R:Integer; Strength:Single);
var
  Bmp2:TBitmap;
  w,h,x,y,xx,yy,grayw,nearness:Integer;
  P,P2,Gray:PByteArray;
  Exps:array[0..255] of Single;
  C:array[0..2] of Single;
  Norm:Single;
begin;
  if (Bmp=nil) or (Bmp.Width=0) or (Bmp.Height=0) or (R=0) or (Strength=0) then
    exit;
  Bmp2:=TBitmap.Create;
  Bmp2.Width:=Bmp.Width;
  Bmp2.Height:=Bmp.Height;
  PrepareBmp(Bmp,w,h,P);
  PrepareBmp(Bmp2,w,h,P2);
  GrayScale(Bmp,Gray,4);grayw:=Bmp.Width+2*4;
  Gauge.MaxValue:=Bmp.Width; Gauge.Progress:=1;
  for x:=0 to 255 do
    Exps[x]:=exp(-sqr(x)/sqr(Strength));
  for x:=0 to Bmp.Width-1 do
    begin;
    Gauge.Progress:=Gauge.Progress+1; Application.ProcessMessages;
    for y:=0 to Bmp.Height-1 do
      begin;
      Norm:=0; c[0]:=0; c[1]:=0; c[2]:=0;
      for xx:=max(x-R,0) to min(x+R,Bmp.Width-1) do for yy:=max(y-R,0) to min(y+R,Bmp.Height-1) do
        begin;
        nearness:=SAD8x8_SSE(@Gray[y*grayw+x],@Gray[yy*grayw+xx],grayw) div 64;
        Norm:=Norm+Exps[nearness];
        C[0]:=C[0]+P^[(h-yy)*w+xx*3+0]*Exps[nearness];
        C[1]:=C[1]+P^[(h-yy)*w+xx*3+1]*Exps[nearness];
        C[2]:=C[2]+P^[(h-yy)*w+xx*3+2]*Exps[nearness];
        end;
      P2^[(h-y)*w+x*3+0]:=Round(C[0]/Norm);
      P2^[(h-y)*w+x*3+1]:=Round(C[1]/Norm);
      P2^[(h-y)*w+x*3+2]:=Round(C[2]/Norm);
      end;
    end;
  FreeMem(Gray);
  Bmp.Free;
  Bmp:=Bmp2;
  Gauge.Progress:=0;
end;

function GetNoiseLevel(const Bmp:TBitmap):Integer;
var
  Bmp2:TBitmap;
  w,h,x,y:Integer;
  P:PByteArray;
  Counts:array[0..255] of Integer;
  i,n,m:Integer;
begin;
  Bmp2:=TBitmap.Create;
  Bmp2.Assign(Bmp);
  FindEdges(Bmp2);
  PrepareBmp(Bmp2,w,h,P);
  FillChar(Counts, SizeOf(Counts), 0);
  for x:=0 to Bmp2.Width-1 do for y:=0 to Bmp2.Height-1 do
    Inc(Counts[Round(0.114*P^[(h-y)*w+x*3+0]+0.587*P^[(h-y)*w+x*3+1]+0.299*P^[(h-y)*w+x*3+2])]);
  n:=0;
  m:=Bmp2.Width*Bmp2.Height div 2;
  for i:=0 to 255 do if Counts[i]<>0 then
    begin;
    n:=n+Counts[i];
    if n>m then
      break;
    end;
  Result:=i;
  Bmp2.Free;
end;

procedure AutoNLM(var Bmp:TBitmap);
var
  MedianNoise:Integer;
  r:Integer;
  Strength:Single;
begin;
  if (Bmp=nil) or (Bmp.Width=0) or (Bmp.Height=0) then
    exit;
  MedianNoise:=GetNoiseLevel(Bmp);
  if (MedianNoise>0) then
    begin;
    r:=min(1+MedianNoise div 4, 10);
    Strength:=8+MedianNoise*0.30;
    NLM(Bmp,R,Strength);
    end;
end;

procedure AutoKNN(var Bmp:TBitmap);
var
  MedianNoise:Integer;
  r:Integer;
  Strength:Single;
begin;
  if (Bmp=nil) or (Bmp.Width=0) or (Bmp.Height=0) then
    exit;
  MedianNoise:=GetNoiseLevel(Bmp);
  if (MedianNoise>0) then
    begin;
    r:=min(1+MedianNoise div 4, 7);
    Strength:=28+MedianNoise*0.30;
    KNN(Bmp,R,Strength);
    end;
end;



end.

