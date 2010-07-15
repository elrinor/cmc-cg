unit Processing;

interface
  uses graphics, sysutils, Classes;

  procedure Binarize(in_Image: TBitmap; out_Image: TBitmap);
  procedure BinaryMedian(in_Image: TBitmap; out_Image: TBitmap);
  procedure Dilate(in_Image: TBitmap; out_Image: TBitmap; Radius: Integer);
  procedure Erode(in_Image: TBitmap; out_Image: TBitmap; Radius: Integer);
  procedure EnhanceContrast(in_Image: TBitmap; out_Image: TBitmap);
  procedure RemoveSmall(in_Image: TBitmap; out_Image: TBitmap; Square: Integer);
  procedure ScanForElephants(in_Image: TBitmap; org_Image: TBitmap; out_Image: TBitmap);

  var
    List: TStrings;

implementation
uses Math;

procedure Binarize(in_Image: TBitmap; out_Image: TBitmap);
var
  pSrc, pDst: PByteArray;
  Colors: array[1..4,0..2] of Integer;
  iY, iX, iBytesPerLine, Count1, Count2, Dist1, Dist2: Integer;
begin
  if (in_Image = nil) or (in_Image.Width = 0) or (in_Image.Height = 0) then
    Exit;

  in_Image.PixelFormat := pf24bit;
  out_Image.PixelFormat := pf24bit;
  pSrc := in_Image.ScanLine[in_Image.Height - 1];
  pDst := out_Image.ScanLine[out_Image.Height - 1];
  iBytesPerLine := (out_Image.Width * 3 + 3) and -4;

  Colors[1, 0] := 0;
  Colors[1, 1] := 0;
  Colors[1, 2] := 0;
  for iY := 0 to (in_Image.Height - 1) do for iX := 0 to (in_Image.Width - 1) do
  begin
    Colors[1, 0] := Colors[1, 0] + pSrc[iY * iBytesPerLine + iX * 3];
    Colors[1, 1] := Colors[1, 1] + pSrc[iY * iBytesPerLine + iX * 3 + 1];
    Colors[1, 2] := Colors[1, 2] + pSrc[iY * iBytesPerLine + iX * 3 + 2];
  end;
  Colors[1, 0] := Colors[1, 0] div (in_Image.Height * in_Image.Width);
  Colors[1, 1] := Colors[1, 1] div (in_Image.Height * in_Image.Width);
  Colors[1, 2] := Colors[1, 2] div (in_Image.Height * in_Image.Width);
  Colors[2, 0] := Colors[1, 0] + 20;
  Colors[2, 1] := Colors[1, 1] + 20;
  Colors[2, 2] := Colors[1, 2] + 20;
  Colors[1, 0] := Colors[1, 0] - 20;
  Colors[1, 1] := Colors[1, 1] - 20;
  Colors[1, 2] := Colors[1, 2] - 20;
  if Colors[2, 0] > 255 then Colors[1, 0] := 255;
  if Colors[2, 1] > 255 then Colors[1, 0] := 255;
  if Colors[2, 2] > 255 then Colors[1, 0] := 255;
  if Colors[1, 0] < 0 then Colors[1, 0] := 0;
  if Colors[1, 1] < 0 then Colors[1, 0] := 0;
  if Colors[1, 2] < 0 then Colors[1, 0] := 0;

  repeat
    Colors[3, 0] := 0;
    Colors[3, 1] := 0;
    Colors[3, 2] := 0;
    Colors[4, 0] := 0;
    Colors[4, 1] := 0;
    Colors[4, 2] := 0;
    Count1 := 0;
    Count2 := 0;

    for iY := 0 to (in_Image.Height - 1) do for iX := 0 to (in_Image.Width - 1) do
    begin
      Dist1 := sqr(Colors[1, 0] - pSrc[iY * iBytesPerLine + iX * 3]) +
               sqr(Colors[1, 1] - pSrc[iY * iBytesPerLine + iX * 3 + 1]) +
               sqr(Colors[1, 2] - pSrc[iY * iBytesPerLine + iX * 3 + 2]);
      Dist2 := sqr(Colors[2, 0] - pSrc[iY * iBytesPerLine + iX * 3]) +
               sqr(Colors[2, 1] - pSrc[iY * iBytesPerLine + iX * 3 + 1]) +
               sqr(Colors[2, 2] - pSrc[iY * iBytesPerLine + iX * 3 + 2]);
      if Dist1 > Dist2 then
      begin
        Colors[4, 0] := Colors[4, 0] + pSrc[iY * iBytesPerLine + iX * 3];
        Colors[4, 1] := Colors[4, 1] + pSrc[iY * iBytesPerLine + iX * 3 + 1];
        Colors[4, 2] := Colors[4, 2] + pSrc[iY * iBytesPerLine + iX * 3 + 2];
        Count2 := Count2 + 1;
      end
      else
      begin
        Colors[3, 0] := Colors[3, 0] + pSrc[iY * iBytesPerLine + iX * 3];
        Colors[3, 1] := Colors[3, 1] + pSrc[iY * iBytesPerLine + iX * 3 + 1];
        Colors[3, 2] := Colors[3, 2] + pSrc[iY * iBytesPerLine + iX * 3 + 2];
        Count1 := Count1 + 1;
      end;
    end;

    Colors[3, 0] := Colors[3, 0] div Count1;
    Colors[3, 1] := Colors[3, 1] div Count1;
    Colors[3, 2] := Colors[3, 2] div Count1;
    Colors[4, 0] := Colors[4, 0] div Count2;
    Colors[4, 1] := Colors[4, 1] div Count2;
    Colors[4, 2] := Colors[4, 2] div Count2;

    Dist1 := sqr(Colors[1, 0] - Colors[3, 0]) +
             sqr(Colors[1, 1] - Colors[3, 1]) +
             sqr(Colors[1, 2] - Colors[3, 2]) +
             sqr(Colors[2, 0] - Colors[4, 0]) +
             sqr(Colors[2, 1] - Colors[4, 1]) +
             sqr(Colors[2, 2] - Colors[4, 2]);

    Colors[1, 0] := Colors[3, 0];
    Colors[1, 1] := Colors[3, 1];
    Colors[1, 2] := Colors[3, 2];
    Colors[2, 0] := Colors[4, 0];
    Colors[2, 1] := Colors[4, 1];
    Colors[2, 2] := Colors[4, 2];
  until Dist1 < 5;

  for iY := 0 to (in_Image.Height - 1) do for iX := 0 to (in_Image.Width - 1) do
  begin
    Dist1 := sqr(Colors[1, 0] - pSrc[iY * iBytesPerLine + iX * 3]) +
             sqr(Colors[1, 1] - pSrc[iY * iBytesPerLine + iX * 3 + 1]) +
             sqr(Colors[1, 2] - pSrc[iY * iBytesPerLine + iX * 3 + 2]);
    Dist2 := sqr(Colors[2, 0] - pSrc[iY * iBytesPerLine + iX * 3]) +
             sqr(Colors[2, 1] - pSrc[iY * iBytesPerLine + iX * 3 + 1]) +
             sqr(Colors[2, 2] - pSrc[iY * iBytesPerLine + iX * 3 + 2]);
    if Dist1 > Dist2 then
    begin
      pDst[iY * iBytesPerLine + iX * 3] := 0;
      pDst[iY * iBytesPerLine + iX * 3 + 1] := 0;
      pDst[iY * iBytesPerLine + iX * 3 + 2] := 0;
    end
    else
    begin
      pDst[iY * iBytesPerLine + iX * 3] := 255;
      pDst[iY * iBytesPerLine + iX * 3 + 1] := 255;
      pDst[iY * iBytesPerLine + iX * 3 + 2] := 255;
    end;
  end;

  if Count1 < Count2 then
  begin
    for iY := 0 to (in_Image.Height - 1) do for iX := 0 to (in_Image.Width - 1) do
    begin
      if pDst[iY * iBytesPerLine + iX * 3] = 0 then
      begin
        pDst[iY * iBytesPerLine + iX * 3] := 255;
        pDst[iY * iBytesPerLine + iX * 3 + 1] := 255;
        pDst[iY * iBytesPerLine + iX * 3 + 2] := 255;
      end
      else
      begin
        pDst[iY * iBytesPerLine + iX * 3] := 0;
        pDst[iY * iBytesPerLine + iX * 3 + 1] := 0;
        pDst[iY * iBytesPerLine + iX * 3 + 2] := 0;
      end;
    end;
  end;
end;

procedure BinaryMedian(in_Image: TBitmap; out_Image: TBitmap);
var
  pSrc, pDst: PByteArray;
  iY, iX, iBytesPerLine, Count: Integer;
begin
  if (in_Image = nil) or (in_Image.Width = 0) or (in_Image.Height = 0) then
    Exit;

  in_Image.PixelFormat := pf24bit;
  out_Image.PixelFormat := pf24bit;
  pSrc := in_Image.ScanLine[in_Image.Height - 1];
  pDst := out_Image.ScanLine[out_Image.Height - 1];
  iBytesPerLine := (out_Image.Width * 3 + 3) and -4;

  for iY := 1 to (in_Image.Height - 2) do for iX := 1 to (in_Image.Width - 2) do
  begin
    Count := pSrc[(iY - 1) * iBytesPerLine + (iX - 1) * 3] +
             pSrc[(iY - 1) * iBytesPerLine + (iX + 0) * 3] +
             pSrc[(iY - 1) * iBytesPerLine + (iX + 1) * 3] +
             pSrc[(iY + 0) * iBytesPerLine + (iX - 1) * 3] +
             pSrc[(iY + 0) * iBytesPerLine + (iX + 0) * 3] +
             pSrc[(iY + 0) * iBytesPerLine + (iX + 1) * 3] +
             pSrc[(iY + 1) * iBytesPerLine + (iX - 1) * 3] +
             pSrc[(iY + 1) * iBytesPerLine + (iX + 0) * 3] +
             pSrc[(iY + 1) * iBytesPerLine + (iX + 1) * 3];
    if Count > 255 * 4 then
    begin
      pDst[iY * iBytesPerLine + iX * 3] := 255;
      pDst[iY * iBytesPerLine + iX * 3 + 1] := 255;
      pDst[iY * iBytesPerLine + iX * 3 + 2] := 255;
    end
    else
    begin
      pDst[iY * iBytesPerLine + iX * 3] := 0;
      pDst[iY * iBytesPerLine + iX * 3 + 1] := 0;
      pDst[iY * iBytesPerLine + iX * 3 + 2] := 0;
    end;
  end;
end;

procedure Erode(in_Image: TBitmap; out_Image: TBitmap; Radius: Integer);
var
  pSrc, pDst: PByteArray;
  iY, iX, iBytesPerLine, iX1, iX2, iY1, iY2, i: Integer;
begin
  if (in_Image = nil) or (in_Image.Width = 0) or (in_Image.Height = 0) or (Radius = 0) then
    Exit;

  in_Image.PixelFormat := pf24bit;
  out_Image.PixelFormat := pf24bit;
  pSrc := in_Image.ScanLine[in_Image.Height - 1];
  pDst := out_Image.ScanLine[out_Image.Height - 1];
  iBytesPerLine := (out_Image.Width * 3 + 3) and -4;

  for iY := 0 to (in_Image.Height - 1) do for iX := 0 to (in_Image.Width - 1) do
    if pDst[iY * iBytesPerLine + iX * 3] = 255 then
    begin
      iX1 := iX - Radius;
      iX2 := iX + Radius;
      if iX1 < 0 then iX1 := 0;
      if iX2 > in_Image.Width - 1 then iX2 := in_Image.Width - 1;
      for i := iX1 to iX2 do
      begin
        pSrc[iY * iBytesPerLine + i * 3] := 255;
        pSrc[iY * iBytesPerLine + i * 3 + 1] := 255;
        pSrc[iY * iBytesPerLine + i * 3 + 2] := 255;
      end;
    end;

  for iY := 0 to (in_Image.Height - 1) do for iX := 0 to (in_Image.Width - 1) do
    if pSrc[iY * iBytesPerLine + iX * 3] = 255 then
    begin
      iY1 := iY - Radius;
      iY2 := iY + Radius;
      if iY1 < 0 then iY1 := 0;
      if iY2 > in_Image.Height - 1 then iY2 := in_Image.Height - 1;
      for i := iY1 to iY2 do
      begin
        pDst[i * iBytesPerLine + iX * 3] := 255;
        pDst[i * iBytesPerLine + iX * 3 + 1] := 255;
        pDst[i * iBytesPerLine + iX * 3 + 2] := 255;
      end;
    end;
end;

procedure Dilate(in_Image: TBitmap; out_Image: TBitmap; Radius: Integer);
var
  pSrc, pDst: PByteArray;
  iY, iX, iBytesPerLine, iX1, iX2, iY1, iY2, i: Integer;
begin
  if (in_Image = nil) or (in_Image.Width = 0) or (in_Image.Height = 0) or (Radius = 0) then
    Exit;

  in_Image.PixelFormat := pf24bit;
  out_Image.PixelFormat := pf24bit;
  pSrc := in_Image.ScanLine[in_Image.Height - 1];
  pDst := out_Image.ScanLine[out_Image.Height - 1];
  iBytesPerLine := (out_Image.Width * 3 + 3) and -4;

  for iY := 0 to (in_Image.Height - 1) do for iX := 0 to (in_Image.Width - 1) do
    if pDst[iY * iBytesPerLine + iX * 3] = 0 then
    begin
      iX1 := iX - Radius;
      iX2 := iX + Radius;
      if iX1 < 0 then iX1 := 0;
      if iX2 > in_Image.Width - 1 then iX2 := in_Image.Width - 1;
      for i := iX1 to iX2 do
      begin
        pSrc[iY * iBytesPerLine + i * 3] := 0;
        pSrc[iY * iBytesPerLine + i * 3 + 1] := 0;
        pSrc[iY * iBytesPerLine + i * 3 + 2] := 0;
      end;
    end;

  for iY := 0 to (in_Image.Height - 1) do for iX := 0 to (in_Image.Width - 1) do
    if pSrc[iY * iBytesPerLine + iX * 3] = 0 then
    begin
      iY1 := iY - Radius;
      iY2 := iY + Radius;
      if iY1 < 0 then iY1 := 0;
      if iY2 > in_Image.Height - 1 then iY2 := in_Image.Height - 1;
      for i := iY1 to iY2 do
      begin
        pDst[i * iBytesPerLine + iX * 3] := 0;
        pDst[i * iBytesPerLine + iX * 3 + 1] := 0;
        pDst[i * iBytesPerLine + iX * 3 + 2] := 0;
      end;
    end;
end;

procedure EnhanceContrast(in_Image: TBitmap; out_Image: TBitmap);
var
  pSrc, pDst: PByteArray;
  Color: array[0..2] of Integer;
  iY, iX, iBytesPerLine, Min, Max, Cur, i, Count, Total: Integer;
  Counts: array[0..255] of Integer;
begin
  if (in_Image = nil) or (in_Image.Width = 0) or (in_Image.Height = 0) then
    Exit;

  in_Image.PixelFormat := pf24bit;
  out_Image.PixelFormat := pf24bit;
  pSrc := in_Image.ScanLine[in_Image.Height - 1];
  pDst := out_Image.ScanLine[out_Image.Height - 1];
  iBytesPerLine := (out_Image.Width * 3 + 3) and -4;

  for i := 0 to 255 do
    Counts[i] := 0;

  for iY := 0 to (in_Image.Height - 1) do for iX := 0 to (in_Image.Width - 1) do
  begin
    Cur := (pSrc[iY * iBytesPerLine + iX * 3] + pSrc[iY * iBytesPerLine + iX * 3 + 1] + pSrc[iY * iBytesPerLine + iX * 3 + 2]) div 3;
    Counts[Cur] := Counts[Cur] + 1;
  end;

  Total := in_Image.Height * in_Image.Width;

  Count := 0;
  for Min := 0 to 255 do
  begin
    Count := Count + Counts[Min];
    if Count > Total div 30 then
      Break;
  end;

  Count := 0;
  for Max := 255 downto 0 do
  begin
    Count := Count + Counts[Max];
    if Count > Total div 30 then
      Break;
  end;

  for iY := 0 to (in_Image.Height - 1) do for iX := 0 to (in_Image.Width - 1) do
  begin
    Color[0] := 255 * (pSrc[iY * iBytesPerLine + iX * 3]     - Min) div (Max - Min + 1);
    Color[1] := 255 * (pSrc[iY * iBytesPerLine + iX * 3 + 1] - Min) div (Max - Min + 1);
    Color[2] := 255 * (pSrc[iY * iBytesPerLine + iX * 3 + 2] - Min) div (Max - Min + 1);
    if Color[0] < 0   then Color[0] := 0;
    if Color[0] > 255 then Color[0] := 255;
    if Color[1] < 0   then Color[1] := 0;
    if Color[1] > 255 then Color[1] := 255;
    if Color[2] < 0   then Color[2] := 0;
    if Color[2] > 255 then Color[2] := 255;
    pDst[iY * iBytesPerLine + iX * 3] := Color[0];
    pDst[iY * iBytesPerLine + iX * 3 + 1] := Color[1];
    pDst[iY * iBytesPerLine + iX * 3 + 2] := Color[2];
  end;
end;

procedure RemoveSmall(in_Image: TBitmap; out_Image: TBitmap; Square: Integer);
var
  pSrc, pDst: PByteArray;
  iY, iX, iBytesPerLine, i, j, Count: Integer;
  Counts: array[0..255, 0..255] of Integer;
begin
  if (in_Image = nil) or (in_Image.Width = 0) or (in_Image.Height = 0) or (Square <= 0) then
    Exit;

  in_Image.PixelFormat := pf24bit;
  out_Image.PixelFormat := pf24bit;
  pSrc := in_Image.ScanLine[in_Image.Height - 1];
  pDst := out_Image.ScanLine[out_Image.Height - 1];
  iBytesPerLine := (out_Image.Width * 3 + 3) and -4;

  Count := 0;
  for iY := 0 to (in_Image.Height - 1) do for iX := 0 to (in_Image.Width - 1) do
    if (pSrc[iY * iBytesPerLine + iX * 3] = 0) and (pSrc[iY * iBytesPerLine + iX * 3 + 1] = 0) and (pSrc[iY * iBytesPerLine + iX * 3 + 2] = 0) then
    begin
      Count := Count + 1;
      in_Image.Canvas.Brush.Color := Count;
      in_Image.Canvas.FloodFill(iX, in_Image.Height - 1 - iY, 0, fsSurface);
    end;

  for i := 0 to 255 do for j := 0 to 255 do
    Counts[i, j] := 0;

  for iY := 0 to (in_Image.Height - 1) do for iX := 0 to (in_Image.Width - 1) do
  begin
    i := pSrc[iY * iBytesPerLine + iX * 3 + 1];
    j := pSrc[iY * iBytesPerLine + iX * 3 + 2];
    Counts[i, j] := Counts[i, j] + 1;
  end;

  for i := 0 to 255 do for j := 0 to 255 do
    if Counts[i, j] > Square then
      Counts[i, j] := 0
    else
      Counts[i, j] := 255;
  Counts[255, 255] := 255;    

  for iY := 0 to (in_Image.Height - 1) do for iX := 0 to (in_Image.Width - 1) do
  begin
    i := pSrc[iY * iBytesPerLine + iX * 3 + 1];
    j := pSrc[iY * iBytesPerLine + iX * 3 + 2];
    pDst[iY * iBytesPerLine + iX * 3] := Counts[i, j];
    pDst[iY * iBytesPerLine + iX * 3 + 1] := Counts[i, j];
    pDst[iY * iBytesPerLine + iX * 3 + 2] := Counts[i, j];
  end;
end;


const
  MaxSize = 1500;
  MaxCount = 10000;
  kdBamb = 1;
  kdElph = 2;

var
  Img:array[0..MaxSize, 0..MaxSize] of Integer;
  Xmid, Ymid, Square, Xmax, Xmin, Ymax, Ymin, Kind, R, G, B, Color, Consumer, Consumed: array[0..MaxCount] of Integer;
  m11, m02, m20: array[0.. MaxCount] of Double; 

procedure ScanForElephants(in_Image: TBitmap; org_Image: TBitmap; out_Image: TBitmap);
var
  pSrc, pDst, pOrg: PByteArray;
  iY, iX, iBytesPerLine, i, j, k, Count, mean, BestDistance, BestN, Distance: Integer;
  El: Double;
begin
  if (in_Image = nil) or (in_Image.Width = 0) or (in_Image.Height = 0) then
    Exit;

  RemoveSmall(in_Image, in_Image, 400);

  in_Image.PixelFormat := pf24bit;
  org_Image.PixelFormat := pf24bit;
  out_Image.PixelFormat := pf24bit;
  pSrc := in_Image.ScanLine[in_Image.Height - 1];
  pDst := out_Image.ScanLine[out_Image.Height - 1];
  pOrg := org_Image.ScanLine[org_Image.Height - 1];
  iBytesPerLine := (out_Image.Width * 3 + 3) and -4;

  Count := 0;
  for iY := 0 to (in_Image.Height - 1) do for iX := 0 to (in_Image.Width - 1) do
    if (pSrc[iY * iBytesPerLine + iX * 3] = 0) and (pSrc[iY * iBytesPerLine + iX * 3 + 1] = 0) and (pSrc[iY * iBytesPerLine + iX * 3 + 2] = 0) then
    begin
      Count := Count + 1;
      in_Image.Canvas.Brush.Color := Count;
      in_Image.Canvas.FloodFill(iX, in_Image.Height - 1 - iY, 0, fsSurface);
    end;

  for iY := 0 to (in_Image.Height - 1) do for iX := 0 to (in_Image.Width - 1) do
  begin
    Img[iX, iY] := pSrc[iY * iBytesPerLine + iX * 3 + 1] * 256 + pSrc[iY * iBytesPerLine + iX * 3 + 2];
    if Img[iX, iY] = 65535 then
      Img[iX, iY] := 0;
  end;

  for i := 0 to Count do
  begin
    Square[i] := 0;
    Xmid[i] := 0;
    Ymid[i] := 0;
    Xmax[i] := 0;
    Xmin[i] := 99999999;
    Ymax[i] := 0;
    Ymin[i] := 99999999;
    Kind[i] := 0;
    m11[i] := 0;
    m02[i] := 0;
    m20[i] := 0;
    R[i] := 0;
    G[i] := 0;
    B[i] := 0;
    Color[i] := 0;
    Consumer[i] := 0;
    Consumed[i] := 0;
  end;

  for iY := 0 to (in_Image.Height - 1) do for iX := 0 to (in_Image.Width - 1) do
  begin
    i := Img[iX,iY];
    if i<>0 then
    begin
      Square[i] := Square[i] + 1;
      Xmid[i] := Xmid[i] + iX;
      Ymid[i] := Ymid[i] + iY;
      if iX > Xmax[i] then Xmax[i] := iX;
      if iX < Xmin[i] then Xmin[i] := iX;
      if iY > Ymax[i] then Ymax[i] := iY;
      if iY < Ymin[i] then Ymin[i] := iY;
      R[i] := R[i] + pOrg[iY * iBytesPerLine + iX * 3 + 2];
      G[i] := G[i] + pOrg[iY * iBytesPerLine + iX * 3 + 1];
      B[i] := B[i] + pOrg[iY * iBytesPerLine + iX * 3];
    end;
  end;

  for i := 1 to Count do
  begin
    XMid[i] := XMid[i] div Square[i];
    YMid[i] := YMid[i] div Square[i];
    R[i] := R[i] div Square[i];
    G[i] := G[i] div Square[i];
    B[i] := B[i] div Square[i];
    Color[i] := 0;
    if not( (abs(R[i] - G[i]) <= 3) and (abs(G[i] - B[i]) <= 3) and (abs(B[i] - R[i]) <= 3) ) then
    begin
      mean := (max(R[i], max(G[i], B[i])) + min(R[i], min(G[i], B[i]))) div 2;
           if (R[i] >= mean) and (G[i] >= mean) then Color[i] := 1
      else if (R[i] >= mean) and (B[i] >= mean) then Color[i] := 2
      else if (R[i] >= G[i]) and (R[i] >= B[i]) then Color[i] := 3
      else if (G[i] >= R[i]) and (G[i] >= B[i]) then Color[i] := 4
      else if (B[i] >= R[i]) and (B[i] >= G[i]) then Color[i] := 5;
    end;
  end;

  for iY := 0 to (in_Image.Height - 1) do for iX := 0 to (in_Image.Width - 1) do
  begin
    i := Img[iX,iY];
    if i<>0 then
    begin
      m02[i] := m02[i] + sqr(iY - Ymid[i]);
      m20[i] := m20[i] + sqr(iX - Xmid[i]);
      m11[i] := m11[i] + (iY - Ymid[i]) * (iX - Xmid[i]);
    end;
  end;

  for i := 1 to Count do
  begin
    El := (m02[i] + m20[i] + sqrt(sqr(m20[i] - m02[i]) + 4 * sqr(m11[i]))) /
          (m02[i] + m20[i] - sqrt(sqr(m20[i] - m02[i]) + 4 * sqr(m11[i])));
    if (El > 10) then
      Kind[i] := kdBamb;
    if (El > 1) and (El < 4) then
      Kind[i] := kdElph;
  end;

  for i := 1 to Count do
  begin
    if Kind[i] = kdBamb then
    begin
      out_Image.Canvas.Pen.Color := 256 * 256 * 128 + 256 * 0 + 128;
      out_Image.Canvas.Pen.Width := 2;
      out_Image.Canvas.MoveTo(Xmin[i], out_Image.Height - 1 - Ymin[i]);
      out_Image.Canvas.LineTo(Xmax[i], out_Image.Height - 1 - Ymin[i]);
      out_Image.Canvas.LineTo(Xmax[i], out_Image.Height - 1 - Ymax[i]);
      out_Image.Canvas.LineTo(Xmin[i], out_Image.Height - 1 - Ymax[i]);
      out_Image.Canvas.LineTo(Xmin[i], out_Image.Height - 1 - Ymin[i]);
    end;
    if Kind[i] = kdElph then
    begin
      out_Image.Canvas.Pen.Color := 256 * 256 * 0 + 256 * 128 + 128;
      out_Image.Canvas.Pen.Width := 2;
      out_Image.Canvas.MoveTo(Xmin[i], out_Image.Height - 1 - Ymin[i]);
      out_Image.Canvas.LineTo(Xmax[i], out_Image.Height - 1 - Ymin[i]);
      out_Image.Canvas.LineTo(Xmax[i], out_Image.Height - 1 - Ymax[i]);
      out_Image.Canvas.LineTo(Xmin[i], out_Image.Height - 1 - Ymax[i]);
      out_Image.Canvas.LineTo(Xmin[i], out_Image.Height - 1 - Ymin[i]);
    end;
  end;

  for i := 1 to Count do if Kind[i] = kdBamb then
    begin
    BestDistance := 99999999;
    BestN := 0;
    for j := 1 to Count do if Kind[j] = kdElph then
      if (Color[j] = 0) or (Color[j] = Color[i]) then
      begin
        Distance := sqr(Xmid[i] - Xmid[j]) + sqr(Ymid[i] - Ymid[j]);
        if Distance < BestDistance then
        begin
          BestDistance := Distance;
          BestN := j;
        end;
      end;
    Consumer[i] := BestN;
    Consumed[BestN] := Consumed[BestN] + 1;
    end;

  out_Image.Canvas.Pen.Color := 256 * 256 * 128 + 256 * 128 + 0;
  out_Image.Canvas.Pen.Width := 2;
  out_Image.Canvas.Brush.Color := 256 * 256 * 128 + 256 * 128 + 0;
  for i:=1 to Count do if Kind[i] = kdElph then
    begin
    iX:=Xmid[i];
    iY:=Ymid[i];
    out_Image.Canvas.Ellipse(iX - 4, out_Image.Height - 1 - (iY - 4), iX + 4, out_Image.Height - 1 - (iY + 4));
    for k := 1 to Consumed[i] do
    begin
      BestDistance := 99999999;
      BestN := 0;
      for j := 1 to Count do if (Kind[j] = kdBamb) and (Consumer[j] = i) then
      begin
        Distance := sqr(iX - Xmid[j]) + sqr(iY - Ymid[j]);
        if Distance < BestDistance then
        begin
          BestDistance := Distance;
          BestN := j;
        end;
      end;
      j := BestN;
      out_Image.Canvas.MoveTo(iX, out_Image.Height - 1 - iY);
      iX := Xmid[j];
      iY := Ymid[j];
      out_Image.Canvas.LineTo(iX, out_Image.Height - 1 - iY);
      out_Image.Canvas.Ellipse(iX - 4, out_Image.Height - 1 - (iY - 4), iX + 4, out_Image.Height - 1 - (iY + 4));
      Kind[j] := 0;
      end;
    end;

  for i:=1 to Count do for j:=i+1 to Count do if Ymid[i] < Ymid[j] then
  begin
    k := Ymid[i];
    Ymid[i] := Ymid[j];
    Ymid[j] := k;
    k := Kind[i];
    Kind[i] := Kind[j];
    Kind[j] := k;
    k := Consumed[i];
    Consumed[i] := Consumed[j];
    Consumed[j] := k;
  end;

  k := 0;
  List.Clear;
  for i := 1 to Count do if Kind[i] = kdElph then
  begin
    k := k + 1; 
    List.Add('Elephant['+IntToStr(k)+'] - '+IntToStr(Consumed[i])+' bamboos.');
  end;
end;




end.
