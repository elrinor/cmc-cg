unit Utils;

interface
uses Graphics;

procedure Gauss(Image:TBitmap);
procedure Median(Image:TBitmap);
procedure DoMedian(Image:TBitmap; r:Integer);
procedure KNN(Image:TBitmap);
procedure DoKNN(Image:TBitmap; r:Integer; h:Single);

procedure Binarize(Image:TBitmap);
procedure Dilatation(Image:TBitmap);
procedure Erosion(Image:TBitmap);
procedure Closing(Image:TBitmap);
procedure Opening(Image:TBitmap);
procedure DoOpening(Image:TBitmap; r:Single);
procedure DoClosing(Image:TBitmap; r:Single);
procedure DoDilatation(Image:TBitmap; R:single);
procedure DoErosion(Image:TBitmap; R:single);
procedure DeDetalize(Image:TBitmap);
procedure FindElephants(Colored:TBitmap; Binary:TBitmap; var ResultS:String);
procedure L1LightNormalization(Image:TBitmap);
procedure LightNormalization(Image:TBitmap);
procedure BackGroundCompensation(Image:TBitmap);
procedure DoRemoveSmallRegions(Binary:TBitmap; size:Integer);
procedure RemoveSmallRegions(Binary:TBitmap);
procedure Contrast(Image:TBitmap);


implementation
uses Unit1,Unit2,Unit3,Classes,Math,SysUtils;

const SLON=1;
      TROSNIK=2;
      MinSize=300;

type TByteArray=array[0..0] of Byte;
     PByteArray=^TByteArray;
     TArray3x3=array[-1..1,-1..1] of Single;
     TCol=array[0..2] of Byte;
     TCol4=array[0..3] of Byte;
     TColArray=array[0..0] of TCol;
     PColArray=^TColArray;
     TCol4Array=array[0..0] of TCol4;
     PCol4Array=^TCol4Array;
     TRegion=record
       RType:Byte;
       Compactness:Double;
       Elongation:Double;
       MySlon:Integer;
       Color:array[0..2] of Integer;
       ColorClass:Byte;
       Eaten:Boolean;
       x,y:Integer;
       S:Integer;
       P:Integer;
       mainPhi:Double;
       height,width:Double;
       cf,sf:Double;
       m11,m02,m20:Double;
       minx,miny,maxx,maxy:Integer;
     end;
     TRegionArray=array[1..10000] of TRegion;
     PRegionArray=^TRegionArray;

function RGB(r,g,b:Integer):TColor;
begin
Result:=r + g shl 8 + b shl 16;
end;

procedure DoGauss(Image:TBitmap; power:Single);
var a:array[-200..200] of Single;
    Sum:Single;
    t:array[0..2] of Single;
    i,r,BytesPerLine,x,y,dx,dy,newx,newy:Integer;
    pB,pB2:PByteArray;
    bm:TBitmap;
begin
  Image.PixelFormat:=pf24bit;
  bm:=TBitmap.Create;bm.Assign(Image);
  BytesPerLine:=(Image.Width*3+3) and -4;
  pB:=bm.ScanLine[bm.Height-1];
  pB2:=Image.ScanLine[Image.Height-1];
  r:=Round(power*3);if r<1 then r:=1; if r>200 then r:=200;
  for i:=-r to r do a[i]:=exp(-sqr(i/power)/2);
  Sum:=0; for i:=-r to r do Sum:=Sum+a[i];
  for i:=-r to r do a[i]:=a[i]/Sum;
  for y:=0 to Image.Height-1 do for x:=0 to Image.Width-1 do
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
  for y:=0 to Image.Height-1 do for x:=0 to Image.Width-1 do
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
end;

procedure Gauss(Image:TBitmap);
var power:Single;
begin
  power:=Dialog('Радиус размытия:',0); if (power=0) then Exit;
  DoGauss(Image,power);
end;

procedure DoMedian(Image:TBitmap; r:Integer);
var t:PCol4Array;
    d,dd,c,n,i,j,BytesPerLine,x,y,dx,dy,newx,newy:Integer;
    pB,pB2:PByteArray;
    bm:TBitmap;
begin
  Image.PixelFormat:=pf24bit;
  bm:=TBitmap.Create;bm.Assign(Image);
  BytesPerLine:=(Image.Width*3+3) and -4;
  pB:=bm.ScanLine[bm.Height-1];
  pB2:=Image.ScanLine[Image.Height-1];
  GetMem(t,4*(2*r+1)*(2*r+1));
  for y:=0 to Image.Height-1 do for x:=0 to Image.Width-1 do
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
end;


procedure Median(Image:TBitmap);
var r:Integer;
begin
  r:=Round(Dialog('Радиус медианы:',0)); if (r=0) then Exit;
  DoMedian(Image,r);
end;

procedure DoKNN(Image:TBitmap; r:Integer; h:Single);
var t:array[0..2] of Single;
    BytesPerLine,x,y,dx,dy,newx,newy:Integer;
    Sum:Single;
    K:Single;
    pB,pB2:PByteArray;
    bm:TBitmap;
begin
  Image.PixelFormat:=pf24bit;
  bm:=TBitmap.Create;bm.Assign(Image);
  BytesPerLine:=(Image.Width*3+3) and -4;
  pB:=bm.ScanLine[bm.Height-1];
  pB2:=Image.ScanLine[Image.Height-1];
  for y:=0 to Image.Height-1 do for x:=0 to Image.Width-1 do
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
end;

procedure KNN(Image:TBitmap);
var r:Integer;
    sr,h:Real;
begin
  Dialog2('Радиус K nearest neighbors:','Уровень шума K nearest neighbors:',0,0,sr,h);
  r:=Round(sr); if(r=0) or (h=0) then exit;
  DoKNN(Image,r,h);
end;


procedure Binarize(Image:TBitmap);
const sgs:array[0..7,0..2] of Integer=
          (( 1, 1, 1),
           ( 1, 1,-1),
           ( 1,-1, 1),
           ( 1,-1,-1),
           (-1, 1, 1),
           (-1, 1,-1),
           (-1,-1, 1),
           (-1,-1,-1));
type TMinMaxColor=record
       min,max:Integer;
       minC,maxC:TCol;
     end;
var i,BytesPerLine,x,y,val,c1,c2:Integer;
    pB,pB2:PByteArray;
    bm:TBitmap;
    m1,m2:TCol;
    newm1,newm2:array[0..2] of Integer;
    dyn:array[0..7] of TMinMaxColor;
begin
  Image.PixelFormat:=pf24bit;
  bm:=TBitmap.Create;bm.Assign(Image);
  BytesPerLine:=(Image.Width*3+3) and -4;
  pB:=bm.ScanLine[bm.Height-1];
  pB2:=Image.ScanLine[Image.Height-1];
  for i:=0 to 7 do
  begin
    dyn[i].min:=2000000000;
    dyn[i].max:=-2000000000;
  end;

  for y:=0 to Image.Height-1 do for x:=0 to Image.Width-1 do
  begin
    for i:=0 to 7 do
      begin
      val:=sgs[i][0]*pB[y*BytesPerLine+x*3  ]+sgs[i][1]*pB[y*BytesPerLine+x*3+1]+sgs[i][2]*pB[y*BytesPerLine+x*3+2];
      if val>dyn[i].max then
      begin
        dyn[i].max:=val;
        dyn[i].maxC[0]:=pB[y*BytesPerLine+x*3  ];
        dyn[i].maxC[1]:=pB[y*BytesPerLine+x*3+1];
        dyn[i].maxC[2]:=pB[y*BytesPerLine+x*3+2];
      end;
      if val<dyn[i].min then
      begin
        dyn[i].min:=val;
        dyn[i].minC[0]:=pB[y*BytesPerLine+x*3  ];
        dyn[i].minC[1]:=pB[y*BytesPerLine+x*3+1];
        dyn[i].minC[2]:=pB[y*BytesPerLine+x*3+2];
      end;
      end;
  end;

  for i:=1 to 7 do
    begin
      if dyn[i].max>dyn[0].max then
      begin
        dyn[0].max:=dyn[i].max;
        dyn[0].maxC[0]:=dyn[i].maxC[0];
        dyn[0].maxC[1]:=dyn[i].maxC[1];
        dyn[0].maxC[2]:=dyn[i].maxC[2];
      end;
      if dyn[i].min>dyn[0].min then
      begin
        dyn[0].max:=dyn[i].min;
        dyn[0].minC[0]:=dyn[i].minC[0];
        dyn[0].minC[1]:=dyn[i].minC[1];
        dyn[0].minC[2]:=dyn[i].minC[2];
      end;
    end;

  m1:=dyn[0].minC;
  m2:=dyn[0].maxC;
  while(true) do
  begin
    newm1[0]:=0;newm1[1]:=0;newm1[2]:=0;
    newm2[0]:=0;newm2[1]:=0;newm2[2]:=0;
    c1:=0;c2:=0;
    for y:=0 to Image.Height-1 do for x:=0 to Image.Width-1 do
    begin
      if abs(pB[y*BytesPerLine+x*3  ]-m1[0])+abs(pB[y*BytesPerLine+x*3+1]-m1[1])+abs(pB[y*BytesPerLine+x*3+2]-m1[2]) <
         abs(pB[y*BytesPerLine+x*3  ]-m2[0])+abs(pB[y*BytesPerLine+x*3+1]-m2[1])+abs(pB[y*BytesPerLine+x*3+2]-m2[2]) then
      begin
        Inc(c1);
        Inc(newm1[0],pB[y*BytesPerLine+x*3  ]);
        Inc(newm1[1],pB[y*BytesPerLine+x*3+1]);
        Inc(newm1[2],pB[y*BytesPerLine+x*3+2]);
      end
      else
      begin
        Inc(c2);
        Inc(newm2[0],pB[y*BytesPerLine+x*3  ]);
        Inc(newm2[1],pB[y*BytesPerLine+x*3+1]);
        Inc(newm2[2],pB[y*BytesPerLine+x*3+2]);
      end;
    end;
    newm1[0]:=newm1[0] div c1;
    newm1[1]:=newm1[1] div c1;
    newm1[2]:=newm1[2] div c1;
    newm2[0]:=newm2[0] div c2;
    newm2[1]:=newm2[1] div c2;
    newm2[2]:=newm2[2] div c2;
    if abs(m1[0]-newm1[0])+abs(m1[1]-newm1[1])+abs(m1[2]-newm1[2])+
       abs(m2[0]-newm2[0])+abs(m2[1]-newm2[1])+abs(m2[2]-newm2[2]) < 20 then
    begin
      m1[0]:=newm1[0];
      m1[1]:=newm1[1];
      m1[2]:=newm1[2];
      m2[0]:=newm2[0];
      m2[1]:=newm2[1];
      m2[2]:=newm2[2];
      break;
    end
    else
    begin
      m1[0]:=newm1[0];
      m1[1]:=newm1[1];
      m1[2]:=newm1[2];
      m2[0]:=newm2[0];
      m2[1]:=newm2[1];
      m2[2]:=newm2[2];
    end;
  end;

  c1:=0;c2:=0;
  for y:=0 to Image.Height-1 do for x:=0 to Image.Width-1 do
  begin
    if abs(pB[y*BytesPerLine+x*3  ]-m1[0])+abs(pB[y*BytesPerLine+x*3+1]-m1[1])+abs(pB[y*BytesPerLine+x*3+2]-m1[2]) >
       abs(pB[y*BytesPerLine+x*3  ]-m2[0])+abs(pB[y*BytesPerLine+x*3+1]-m2[1])+abs(pB[y*BytesPerLine+x*3+2]-m2[2]) then
    begin
      pB2[y*BytesPerLine+x*3  ]:=255;
      pB2[y*BytesPerLine+x*3+1]:=255;
      pB2[y*BytesPerLine+x*3+2]:=255;
      Inc(c1);
    end
    else
    begin
      pB2[y*BytesPerLine+x*3  ]:=0;
      pB2[y*BytesPerLine+x*3+1]:=0;
      pB2[y*BytesPerLine+x*3+2]:=0;
      Inc(c2);
    end;
  end;
  if c1>c2 then
  begin
    for y:=0 to Image.Height-1 do for x:=0 to Image.Width-1 do
    begin
      pB2[y*BytesPerLine+x*3  ]:=not pB2[y*BytesPerLine+x*3  ];
      pB2[y*BytesPerLine+x*3+1]:=not pB2[y*BytesPerLine+x*3+1];
      pB2[y*BytesPerLine+x*3+2]:=not pB2[y*BytesPerLine+x*3+2];
    end;
  end;
end;

procedure DoDilatation(Image:TBitmap; R:single);
var mask:array[-5..5,-5..5] of Byte;
    x,y,dx,dy,newx,newy:Integer;
    BytesPerLine:Integer;
    pB,pB2:PByteArray;
    bm:TBitmap;
    rr:Integer;
begin
  while r>5 do
  begin
    DoDilatation(Image, 5);
    r:=r-5;
  end;

  Image.PixelFormat:=pf24bit;
  bm:=TBitmap.Create;bm.Assign(Image);
  BytesPerLine:=(Image.Width*3+3) and -4;
  pB:=bm.ScanLine[bm.Height-1];
  pB2:=Image.ScanLine[Image.Height-1];

  for x:=-5 to 5 do for y:=-5 to 5 do
  begin
    if sqr(x)+sqr(y)<=sqr(r)+0.1 then
      mask[x,y]:=255
    else
      mask[x,y]:=0;
  end;
  rr:=Round(r)+1;if rr>5 then rr:=5;
  
  for y:=0 to Image.Height-1 do for x:=0 to Image.Width-1 do
  begin
    if pB[y*BytesPerLine+x*3  ]=255 then
    begin
      for dx:=-rr to rr do for dy:=-rr to rr do
      begin
        if mask[dx,dy]=255 then
        begin
          newx:=x+dx;if (newx<0) then newx:=0; if (newx>bm.Width-1) then newx:=bm.Width-1;
          newy:=y+dy;if (newy<0) then newy:=0; if (newy>bm.Height-1) then newy:=bm.Height-1;
          pB2[newy*BytesPerLine+newx*3  ]:=255;
          pB2[newy*BytesPerLine+newx*3+1]:=255;
          pB2[newy*BytesPerLine+newx*3+2]:=255;
        end;  
      end;
    end;
  end;
end;

procedure DoErosion(Image:TBitmap; R:single);
var mask:array[-5..5,-5..5] of Byte;
    x,y,dx,dy,newx,newy:Integer;
    BytesPerLine:Integer;
    pB,pB2:PByteArray;
    bm:TBitmap;
    rr:Integer;
begin
  while r>5 do
  begin
    DoErosion(Image, 5);
    r:=r-5;
  end;

  Image.PixelFormat:=pf24bit;
  bm:=TBitmap.Create;bm.Assign(Image);
  BytesPerLine:=(Image.Width*3+3) and -4;
  pB:=bm.ScanLine[bm.Height-1];
  pB2:=Image.ScanLine[Image.Height-1];

  for x:=-5 to 5 do for y:=-5 to 5 do
  begin
    if sqr(x)+sqr(y)<=sqr(r)+0.1 then
      mask[x,y]:=0
    else
      mask[x,y]:=255
  end;
  rr:=Round(r)+1;if rr>5 then rr:=5;
  
  for y:=0 to Image.Height-1 do for x:=0 to Image.Width-1 do
  begin
    if pB[y*BytesPerLine+x*3  ]=0 then
    begin
      for dx:=-rr to rr do for dy:=-rr to rr do
      begin
        if mask[dx,dy]=0 then
        begin
          newx:=x+dx;if (newx<0) then newx:=0; if (newx>bm.Width-1) then newx:=bm.Width-1;
          newy:=y+dy;if (newy<0) then newy:=0; if (newy>bm.Height-1) then newy:=bm.Height-1;
          pB2[newy*BytesPerLine+newx*3  ]:=0;
          pB2[newy*BytesPerLine+newx*3+1]:=0;
          pB2[newy*BytesPerLine+newx*3+2]:=0;
        end;  
      end;
    end;
  end;
end;

procedure Dilatation(Image:TBitmap);
var r:Single;
begin
  r:=Dialog('Радиус Расширения:',0); if (r=0) then Exit;
  DoDilatation(Image,r);
end;

procedure Erosion(Image:TBitmap);
var r:Single;
begin
  r:=Dialog('Радиус Сужения:',0); if (r=0) then Exit;
  DoErosion(Image,r);
end;

procedure DoOpening(Image:TBitmap; r:Single);
begin
  DoErosion(Image,r);
  DoDilatation(Image,r);
end;

procedure DoClosing(Image:TBitmap; r:Single);
begin
  DoDilatation(Image,r);
  DoErosion(Image,r);
end;

procedure Closing(Image:TBitmap);
var r:Single;
begin
  r:=Dialog('Радиус Закрытия:',0); if (r=0) then Exit;
  DoClosing(Image,r);
end;

procedure Opening(Image:TBitmap);
var r:Single;
begin
  r:=Dialog('Радиус Открытия:',0); if (r=0) then Exit;
  DoOpening(Image,r);
end;

procedure DeDetalize(Image:TBitmap);
var x,y:Integer;
    BytesPerLine:Integer;
    pB:PByteArray;
begin
  Image.PixelFormat:=pf24bit;
  BytesPerLine:=(Image.Width*3+3) and -4;
  pB:=Image.ScanLine[Image.Height-1];

  for y:=0 to Image.Height-1 do for x:=1 to Image.Width-2 do
  begin
    if(pB[y*BytesPerLine+(x-1)*3  ]=0) and (pB[y*BytesPerLine+x*3  ]=255) and (pB[y*BytesPerLine+(x+1)*3  ]=0) then
    begin
      pB[y*BytesPerLine+x*3  ]:=0;
      pB[y*BytesPerLine+x*3+1]:=0;
      pB[y*BytesPerLine+x*3+2]:=0;
    end
    else if(pB[y*BytesPerLine+(x-1)*3  ]=255) and (pB[y*BytesPerLine+x*3  ]=0) and (pB[y*BytesPerLine+(x+1)*3  ]=255) then
    begin
      pB[y*BytesPerLine+x*3  ]:=255;
      pB[y*BytesPerLine+x*3+1]:=255;
      pB[y*BytesPerLine+x*3+2]:=255;
    end;
  end;
  for y:=1 to Image.Height-2 do for x:=0 to Image.Width-1 do
  begin
    if(pB[(y-1)*BytesPerLine+x*3  ]=0) and (pB[y*BytesPerLine+x*3  ]=255) and (pB[(y+1)*BytesPerLine+x*3  ]=0) then
    begin
      pB[y*BytesPerLine+x*3  ]:=0;
      pB[y*BytesPerLine+x*3+1]:=0;
      pB[y*BytesPerLine+x*3+2]:=0;
    end
    else if(pB[(y-1)*BytesPerLine+x*3  ]=255) and (pB[y*BytesPerLine+x*3  ]=0) and (pB[(y+1)*BytesPerLine+x*3  ]=255) then
    begin
      pB[y*BytesPerLine+x*3  ]:=255;
      pB[y*BytesPerLine+x*3+1]:=255;
      pB[y*BytesPerLine+x*3+2]:=255;
    end;
  end;
end;

procedure FindElephants(Colored:TBitmap; Binary:TBitmap; var ResultS:String);
var i,j,k,d,x,y,minn,minv:Integer;
    w,h,BytesPerLine:Integer;
    pB,pBc:PByteArray;
    Regions:PRegionArray;
    RCount:Integer;
    a,sq:PIntegerArray;

procedure MarkRegion(x,y:Integer);
var i:Integer;
begin
  for i:=x downto 0 do if a[w*y+i]<0 then a[w*y+i]:=RCount else break;
  for i:=x+1 to w-1 do if a[w*y+i]<0 then a[w*y+i]:=RCount else break;

  for i:=x downto 0 do
  begin
    if a[w*y+i]=RCount then
    begin
      if (y>0) and (a[w*(y-1)+i]<0) then
        MarkRegion(i,y-1);
      if (y<h-1) and (a[w*(y+1)+i]<0) then
        MarkRegion(i,y+1);
    end
    else
      break;
  end;

  for i:=x+1 to w-1 do
  begin
    if a[w*y+i]=RCount then
    begin
      a[w*y+i]:=RCount;
      if (y>0) and (a[w*(y-1)+i]<0) then
        MarkRegion(i,y-1);
      if (y<h-1) and (a[w*(y+1)+i]<0) then
        MarkRegion(i,y+1);
    end
    else
      break;
  end;
end;


begin
  Binary.PixelFormat:=pf24bit;
  Colored.PixelFormat:=pf24bit;
  BytesPerLine:=(Binary.Width*3+3) and -4;
  pB:=Binary.ScanLine[Binary.Height-1];
  pBc:=Colored.ScanLine[Colored.Height-1];

  GetMem(a,Sizeof(Integer)*Binary.Height*Binary.Width);
  w:=Binary.Width;
  h:=Binary.Height;

  for y:=0 to Binary.Height-1 do for x:=0 to Binary.Width-1 do
  begin
    if pB[y*BytesPerLine+x*3  ]<>255 then
      a[y*w+x]:=0
    else
      a[y*w+x]:=-1;
  end;

  RCount:=0;
  for y:=0 to Binary.Height-1 do for x:=0 to Binary.Width-1 do
  begin
    if a[y*w+x]<0 then
    begin
      RCount:=RCount+1;
      MarkRegion(x,y);
    end;
  end;

  GetMem(sq,Sizeof(Integer)*(RCount+1));
  Fillchar(sq^,Sizeof(Integer)*(RCount+1),0);
  for i:=1 to RCount do sq[i]:=0;

  for y:=0 to Binary.Height-1 do for x:=0 to Binary.Width-1 do
  begin
    if a[y*w+x]>0 then
      Inc(sq[a[y*w+x]]);
  end;

  for y:=0 to Binary.Height-1 do for x:=0 to Binary.Width-1 do
  begin
    if (a[y*w+x]>0) and (sq[a[y*w+x]]<MinSize) then
      a[y*w+x]:=0;
  end;

  Fillchar(sq^,Sizeof(Integer)*(RCount+1),0);
  RCount:=0;
  for y:=0 to Binary.Height-1 do for x:=0 to Binary.Width-1 do
  begin
    if a[y*w+x]>0 then
    begin
      if sq[a[y*w+x]]=0 then
      begin
        Inc(RCount);
        sq[a[y*w+x]]:=RCount;
      end;
      a[y*w+x]:=sq[a[y*w+x]];
    end;
  end;
  FreeMem(sq);

  GetMem(Regions,sizeof(TRegion)*(RCount+1));
  fillchar(Regions^,sizeof(TRegion)*(RCount+1),0);
  for i:=1 to RCount do
  begin
    Regions[i].minx:=2000000000;
    Regions[i].miny:=2000000000;
  end;
  for y:=0 to Binary.Height-1 do for x:=0 to Binary.Width-1 do
  begin
    i:=a[y*w+x];
    if (i<>0) then
    begin
      Regions[i].S:=Regions[i].S+1;
      if (x=0) or (y=0) or (x=w-1) or (y=h-1) or (a[y*w+x+1]=0) or (a[y*w+x-1]=0) or (a[(y+1)*w+x]=0) or (a[(y-1)*w+x]=0) then
        Regions[i].P:=Regions[i].P+1;
      Regions[i].x:=Regions[i].x+x;
      Regions[i].y:=Regions[i].y+y;
      Regions[i].maxx:=max(Regions[i].maxx,x);
      Regions[i].minx:=min(Regions[i].minx,x);
      Regions[i].maxy:=max(Regions[i].maxy,y);
      Regions[i].miny:=min(Regions[i].miny,y);
      Regions[i].Color[0]:=Regions[i].Color[0]+pBc[y*BytesPerLine+x*3  ];
      Regions[i].Color[1]:=Regions[i].Color[1]+pBc[y*BytesPerLine+x*3+1];
      Regions[i].Color[2]:=Regions[i].Color[2]+pBc[y*BytesPerLine+x*3+2];
    end;
  end;
  for i:=1 to RCount do
  begin
    Regions[i].x:=Regions[i].x div Regions[i].s;
    Regions[i].y:=Regions[i].y div Regions[i].s;
    Regions[i].Color[0]:=Regions[i].Color[0] div Regions[i].s;
    Regions[i].Color[1]:=Regions[i].Color[1] div Regions[i].s;
    Regions[i].Color[2]:=Regions[i].Color[2] div Regions[i].s;
    Regions[i].Compactness:=sqr(Regions[i].P)/Regions[i].s;
  end;
  for y:=0 to Binary.Height-1 do for x:=0 to Binary.Width-1 do
  begin
    i:=a[y*w+x];
    if (i<>0) then
    begin
      Regions[i].m11:=Regions[i].m11+(x-Regions[i].x)*(y-Regions[i].y);
      Regions[i].m02:=Regions[i].m02+sqr(y-Regions[i].y);
      Regions[i].m20:=Regions[i].m20+sqr(x-Regions[i].x);
    end;
  end;
  for i:=1 to RCount do with Regions[i] do
  begin
    Elongation:=(m02+m20+sqrt(sqr(m20-m02)+4*sqr(m11))) / (m02+m20-sqrt(sqr(m20-m02)+4*sqr(m11)));
    mainPhi:=0.5*arctan2(2*m11,m20-m02);
    cf:=cos(mainPhi);
    sf:=sin(mainPhi);
    with Regions[i] do
    begin
      if (abs(Color[0]-Color[1])<=3) and (abs(Color[1]-Color[2])<=3) and (abs(Color[0]-Color[2])<=3) then
        ColorClass:=1
      else
      begin;
        d:=(Color[0]+Color[1]+Color[2]) div 3;
        if (Color[2]>d) and (Color[1]>d) then
          ColorClass:=3
        else if (Color[2]>d) and (Color[0]>d) then
          ColorClass:=5
        else if (Color[2]>=Color[0]-3) and (Color[2]>=Color[1]-3) then
          ColorClass:=2
        else if (Color[0]>=Color[2]-3) and (Color[0]>=Color[1]-3) then
          ColorClass:=4
        else if (Color[1]>=Color[0]-3) and (Color[1]>=Color[2]-3) then
          ColorClass:=6;
      end;
    end;
  end;
  for y:=0 to Binary.Height-1 do for x:=0 to Binary.Width-1 do
  begin
    i:=a[y*w+x];
    if (i<>0) then
    begin
      Regions[i].height:=max( Regions[i].height, abs((x-Regions[i].x)*Regions[i].cf+(y-Regions[i].y)*Regions[i].sf) );
      Regions[i].width:= max( Regions[i].width,  abs((x-Regions[i].x)*Regions[i].sf-(y-Regions[i].y)*Regions[i].cf) );
    end;
  end;
  for i:=1 to RCount do with Regions[i] do
  begin
    if S<20000 then
    if (Elongation>1.0)and(Elongation<2.6) then
      RType:=SLON
    else if ((Elongation>20)and(Elongation<130))or((Elongation>450)and(Elongation<650)) then
      RType:=TROSNIK
    else
      RType:=0;
  end;
  for i:=1 to RCount do if Regions[i].RType=TROSNIK then
  begin
    minv:=2000000000;
    for j:=1 to RCount do if (Regions[j].RType=SLON) and ((Regions[i].ColorClass=Regions[j].ColorClass) or (Regions[j].ColorClass=1)) then
    begin
      if (sqr(Regions[i].x-Regions[j].x)+sqr(Regions[i].y-Regions[j].y))<minv then
      begin
        minv:=sqr(Regions[i].x-Regions[j].x)+sqr(Regions[i].y-Regions[j].y);
        minn:=j;
      end;
    end;
    if minv<2000000000 then
    begin
      Regions[i].MySlon:=minn;
      Regions[minn].MySlon:=Regions[minn].MySlon+1;
    end;
  end;

  Binary.Assign(Colored);
  for i:=1 to RCount do with Regions[i] do
  begin
    if RType=TROSNIK then
    begin
      Binary.Canvas.Pen.Color:=RGB(140,214,252);
      Binary.Canvas.Pen.Width:=2;
      Binary.Canvas.MoveTo(x+Round( cf*height+sf*width),h-(y+Round( sf*height-cf*width)));
      Binary.Canvas.LineTo(x+Round(-cf*height+sf*width),h-(y+Round(-sf*height-cf*width)));
      Binary.Canvas.LineTo(x+Round(-cf*height-sf*width),h-(y+Round(-sf*height+cf*width)));
      Binary.Canvas.LineTo(x+Round( cf*height-sf*width),h-(y+Round( sf*height+cf*width)));
      Binary.Canvas.LineTo(x+Round( cf*height+sf*width),h-(y+Round( sf*height-cf*width)));
    end
    else if RType=SLON then
    begin
      Binary.Canvas.Pen.Color:=RGB(172,126,68);
      Binary.Canvas.Pen.Width:=2;
      Binary.Canvas.MoveTo(Regions[i].minx,h-Regions[i].miny);
      Binary.Canvas.LineTo(Regions[i].maxx,h-Regions[i].miny);
      Binary.Canvas.LineTo(Regions[i].maxx,h-Regions[i].maxy);
      Binary.Canvas.LineTo(Regions[i].minx,h-Regions[i].maxy);
      Binary.Canvas.LineTo(Regions[i].minx,h-Regions[i].miny);
    end;
  end;

  for i:=1 to RCount do if Regions[i].RType=SLON then
  begin
    x:=Regions[i].x;
    y:=Regions[i].y;
    for k:=1 to Regions[i].MySlon do
    begin
      minv:=2000000000;
      for j:=1 to RCount do if (Regions[j].RType=TROSNIK) and (Regions[j].MySlon=i) and (Regions[j].Eaten=false) then
      begin
        if (sqr(x-Regions[j].x)+sqr(y-Regions[j].y))<minv then
        begin
          minv:=sqr(x-Regions[j].x)+sqr(y-Regions[j].y);
          minn:=j;
        end;
      end;
      Binary.Canvas.Pen.Color:=RGB(4,116,252);
      Binary.Canvas.Pen.Width:=2;
      Binary.Canvas.Brush.Color:=RGB(4,116,252);
      Binary.Canvas.FrameRect(Rect(x-3,h-(y+3),x+3,h-(y-3)));
      Binary.Canvas.MoveTo(x,h-y);
      d:=-15+Random(31);
      x:=Regions[minn].x+Round(Regions[minn].cf*d);
      y:=Regions[minn].y+Round(Regions[minn].sf*d);
      Binary.Canvas.FrameRect(Rect(x-3,h-(y+3),x+3,h-(y-3)));
      Binary.Canvas.LineTo(x,h-y);
      Regions[minn].Eaten:=true;
    end;
  end;

  k:=0;
  for i:=1 to RCount do if (Regions[i].RType=SLON) then
  begin
    Regions[i].P:=0;
    k:=k+1;
  end;
  for j:=1 to k do
  begin
    minv:=0;
    for i:=1 to RCount do if (Regions[i].RType=SLON) and (Regions[i].P=0) and (Regions[i].y*w+Regions[i].x>minv) then
    begin
      minv:=Regions[i].y*w+Regions[i].x;
      minn:=i;
    end;
    Regions[minn].P:=j;
  end;
  ResultS:='';
  for j:=1 to k do for i:=1 to RCount do if (Regions[i].RType=SLON) and (Regions[i].P=j) then
  begin
    ResultS:=ResultS+'Слон '+IntToStr(j)+': '+IntToStr(Regions[i].MySlon);
    if (Regions[i].MySlon=0) or (Regions[i].MySlon div 10=1) or (Regions[i].MySlon mod 10>4) or (Regions[i].MySlon mod 10=0) then
      ResultS:=ResultS+' бамбуков.'#13#10
    else if (Regions[i].MySlon mod 10>=2) then
      ResultS:=ResultS+' бамбука.'#13#10
    else
      ResultS:=ResultS+' бамбук.'#13#10;
  end;

  FreeMem(Regions);
  FreeMem(a);
end;


procedure DoRemoveSmallRegions(Binary:TBitmap; size:Integer);
var i,x,y:Integer;
    w,h,BytesPerLine:Integer;
    pB:PByteArray;
    RCount:Integer;
    a,sq:PIntegerArray;

procedure MarkRegion(x,y:Integer);
var i:Integer;
begin
  for i:=x downto 0 do if a[w*y+i]<0 then a[w*y+i]:=RCount else break;
  for i:=x+1 to w-1 do if a[w*y+i]<0 then a[w*y+i]:=RCount else break;

  for i:=x downto 0 do
  begin
    if a[w*y+i]=RCount then
    begin
      if (y>0) and (a[w*(y-1)+i]<0) then
        MarkRegion(i,y-1);
      if (y<h-1) and (a[w*(y+1)+i]<0) then
        MarkRegion(i,y+1);
    end
    else
      break;
  end;

  for i:=x+1 to w-1 do
  begin
    if a[w*y+i]=RCount then
    begin
      a[w*y+i]:=RCount;
      if (y>0) and (a[w*(y-1)+i]<0) then
        MarkRegion(i,y-1);
      if (y<h-1) and (a[w*(y+1)+i]<0) then
        MarkRegion(i,y+1);
    end
    else
      break;
  end;
end;


begin
  Binary.PixelFormat:=pf24bit;
  BytesPerLine:=(Binary.Width*3+3) and -4;
  pB:=Binary.ScanLine[Binary.Height-1];

  GetMem(a,Sizeof(Integer)*Binary.Height*Binary.Width);
  w:=Binary.Width;
  h:=Binary.Height;

  for y:=0 to Binary.Height-1 do for x:=0 to Binary.Width-1 do
  begin
    if pB[y*BytesPerLine+x*3  ]<>255 then
      a[y*w+x]:=0
    else
      a[y*w+x]:=-1;
  end;

  RCount:=0;
  for y:=0 to Binary.Height-1 do for x:=0 to Binary.Width-1 do
  begin
    if a[y*w+x]<0 then
    begin
      RCount:=RCount+1;
      MarkRegion(x,y);
    end;
  end;

  GetMem(sq,Sizeof(Integer)*(RCount+1));
  Fillchar(sq^,Sizeof(Integer)*(RCount+1),0);
  for i:=1 to RCount do sq[i]:=0;

  for y:=0 to Binary.Height-1 do for x:=0 to Binary.Width-1 do
  begin
    if a[y*w+x]>0 then
      Inc(sq[a[y*w+x]]);
  end;

  for y:=0 to Binary.Height-1 do for x:=0 to Binary.Width-1 do
  begin
    if (a[y*w+x]>0) and (sq[a[y*w+x]]<size) then
      a[y*w+x]:=0;
  end;
  FreeMem(sq);

  for y:=0 to Binary.Height-1 do for x:=0 to Binary.Width-1 do
  begin
    if a[y*w+x]>0 then
    begin
      pB[y*BytesPerLine+x*3  ]:=255;
      pB[y*BytesPerLine+x*3+1]:=255;
      pB[y*BytesPerLine+x*3+2]:=255;
    end
    else
    begin
      pB[y*BytesPerLine+x*3  ]:=0;
      pB[y*BytesPerLine+x*3+1]:=0;
      pB[y*BytesPerLine+x*3+2]:=0;
    end;
  end;
  FreeMem(a);
end;

procedure RemoveSmallRegions(Binary:TBitmap);
var size:Integer;
begin
  size:=Round(Dialog('Минимальная площадь области:',0)); if (size=0) then Exit;
  DoRemoveSmallRegions(Binary,size);
end;

procedure L1LightNormalization(Image:TBitmap);
var x,y,BytesPerLine:Integer;
    pB,pB2:PByteArray;
    bm:TBitmap;
    maxB:array[0..2] of Integer;
    t:array[0..2]of Single;
begin
  Image.PixelFormat:=pf24bit;
  bm:=TBitmap.Create;bm.Assign(Image);
  BytesPerLine:=(Image.Width*3+3) and -4;
  pB:=bm.ScanLine[bm.Height-1];
  pB2:=Image.ScanLine[Image.Height-1];

  DoGauss(bm,30);
  maxB[0]:=0;
  maxB[1]:=0;
  maxB[2]:=0;
  for y:=0 to bm.Height-1 do for x:=0 to bm.Width-1 do
  begin
    if pB[y*BytesPerLine+x*3  ]>maxB[0] then
      maxB[0]:=pB[y*BytesPerLine+x*3  ];
    if pB[y*BytesPerLine+x*3+1]>maxB[1] then
      maxB[1]:=pB[y*BytesPerLine+x*3+1];
    if pB[y*BytesPerLine+x*3+2]>maxB[2] then
      maxB[2]:=pB[y*BytesPerLine+x*3+2];
  end;
  for y:=0 to Image.Height-1 do for x:=0 to Image.Width-1 do
  begin
    t[0]:=pB2[y*BytesPerLine+x*3  ]*maxB[0]/(pB[y*BytesPerLine+x*3  ]+1);
    t[1]:=pB2[y*BytesPerLine+x*3+1]*maxB[1]/(pB[y*BytesPerLine+x*3+1]+1);
    t[2]:=pB2[y*BytesPerLine+x*3+2]*maxB[2]/(pB[y*BytesPerLine+x*3+2]+1);
    if t[0]<0 then t[0]:=0;if t[0]>255 then t[0]:=255;
    if t[1]<0 then t[1]:=0;if t[1]>255 then t[1]:=255;
    if t[2]<0 then t[2]:=0;if t[2]>255 then t[2]:=255;
    pB2[y*BytesPerLine+x*3  ]:=Round(t[0]);
    pB2[y*BytesPerLine+x*3+1]:=Round(t[1]);
    pB2[y*BytesPerLine+x*3+2]:=Round(t[2]);
  end;
end;

procedure LightNormalization(Image:TBitmap);
var x,y,BytesPerLine:Integer;
    pB,pB2:PByteArray;
    bm:TBitmap;
    c:Byte;
    maxB:Integer;
    t:array[0..2]of Single;
begin
  Image.PixelFormat:=pf24bit;
  bm:=TBitmap.Create;bm.Assign(Image);
  BytesPerLine:=(Image.Width*3+3) and -4;
  pB:=bm.ScanLine[bm.Height-1];
  pB2:=Image.ScanLine[Image.Height-1];

  for y:=0 to bm.Height-1 do for x:=0 to bm.Width-1 do
  begin
    c:=(pB[y*BytesPerLine+x*3  ]+pB[y*BytesPerLine+x*3+1]+pB[y*BytesPerLine+x*3+2]) div 3;
    pB[y*BytesPerLine+x*3  ]:=c;
    pB[y*BytesPerLine+x*3+1]:=c;
    pB[y*BytesPerLine+x*3+2]:=c;
  end;
  DoGauss(bm,30);
  maxB:=0;
  for y:=0 to bm.Height-1 do for x:=0 to bm.Width-1 do
  begin
    if pB[y*BytesPerLine+x*3  ]>maxB then
      maxB:=pB[y*BytesPerLine+x*3  ];
  end;
  for y:=0 to Image.Height-1 do for x:=0 to Image.Width-1 do
  begin
    t[0]:=pB2[y*BytesPerLine+x*3  ]*maxB/(pB[y*BytesPerLine+x*3  ]+1);
    t[1]:=pB2[y*BytesPerLine+x*3+1]*maxB/(pB[y*BytesPerLine+x*3  ]+1);
    t[2]:=pB2[y*BytesPerLine+x*3+2]*maxB/(pB[y*BytesPerLine+x*3  ]+1);
    if t[0]<0 then t[0]:=0;if t[0]>255 then t[0]:=255;
    if t[1]<0 then t[1]:=0;if t[1]>255 then t[1]:=255;
    if t[2]<0 then t[2]:=0;if t[2]>255 then t[2]:=255;
    pB2[y*BytesPerLine+x*3  ]:=Round(t[0]);
    pB2[y*BytesPerLine+x*3+1]:=Round(t[1]);
    pB2[y*BytesPerLine+x*3+2]:=Round(t[2]);
  end; 
end;

procedure BackGroundCompensation(Image:TBitmap);
var x,y,BytesPerLine:Integer;
    pB,pB2:PByteArray;
    bm:TBitmap;
begin
  Image.PixelFormat:=pf24bit;
  bm:=TBitmap.Create;bm.Assign(Image);
  BytesPerLine:=(Image.Width*3+3) and -4;
  pB:=bm.ScanLine[bm.Height-1];
  pB2:=Image.ScanLine[Image.Height-1];

  bm.Assign(Image);
  DoGauss(bm,80);
  for y:=0 to Image.Height-1 do for x:=0 to Image.Width-1 do
  begin
    pB2[y*BytesPerLine+x*3  ]:=abs(pB2[y*BytesPerLine+x*3  ]-pB[y*BytesPerLine+x*3  ]);
    pB2[y*BytesPerLine+x*3+1]:=abs(pB2[y*BytesPerLine+x*3+1]-pB[y*BytesPerLine+x*3+1]);
    pB2[y*BytesPerLine+x*3+2]:=abs(pB2[y*BytesPerLine+x*3+2]-pB[y*BytesPerLine+x*3+2]);
  end;
end;

procedure Contrast(Image:TBitmap);
var x,y,i,BytesPerLine:Integer;
    pB:PByteArray;
    min,max,c,maxc:Integer;
    t:array[0..255]of Integer;
begin
  Image.PixelFormat:=pf24bit;
  BytesPerLine:=(Image.Width*3+3) and -4;
  pB:=Image.ScanLine[Image.Height-1];
  fillchar(t,sizeof(t),0);
  for y:=0 to Image.Height-1 do for x:=0 to Image.Width-1 do
    Inc(t[Round(0.114*pB^[y*BytesPerLine+x*3  ]+0.587*pB^[y*BytesPerLine+x*3+1]+0.299*pB^[y*BytesPerLine+x*3+2])]);

  maxc:=(Image.Width*Image.Height) div 20;
  c:=0;
  for i:=0 to 255 do
  begin
    c:=c+t[i];
    if c>maxc then break;
  end;
  min:=i;
  c:=0;
  for i:=255 downto 0 do
  begin
    c:=c+t[i];
    if c>maxc then break;
  end;
  max:=i;

  for y:=0 to Image.Height-1 do for x:=0 to Image.Width-1 do
  begin
    c:=(pB[y*BytesPerLine+x*3  ]-min)*256 div (max-min+1);
    if c<0 then c:=0; if c>255 then c:=255;
    pB[y*BytesPerLine+x*3  ]:=c;
    c:=(pB[y*BytesPerLine+x*3+1]-min)*256 div (max-min+1);
    if c<0 then c:=0; if c>255 then c:=255;
    pB[y*BytesPerLine+x*3+1]:=c;
    c:=(pB[y*BytesPerLine+x*3+2]-min)*256 div (max-min+1);
    if c<0 then c:=0; if c>255 then c:=255;
    pB[y*BytesPerLine+x*3+2]:=c;
  end;
end;

end.

