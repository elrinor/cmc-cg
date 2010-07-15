{ C_GRAPHICSMEDIA_LAB_DMOROZ_C
#/*******************************************************************************
# * (C) Copyright 2003/2004 by Vladimir Vezhnevets <vvp@graphics.cs.msu.su>     *
# *******************************************************************************/
}

unit Processing;

interface
  uses graphics, sysutils;

  procedure WNoise(out_Image :TBitmap; Might:Integer);
  procedure INoise(out_Image :TBitmap; Might:Integer);
  procedure Razmytiye(in_Image: TBitmap; out_Image :TBitmap);
  procedure PovysheniyeChetkosti(in_Image: TBitmap; out_Image :TBitmap);
  procedure NahozhdeniyeGranic(in_Image: TBitmap; out_Image :TBitmap);
  procedure GaussovskoyeRazmytiyeDvumernoye(in_Image: TBitmap; out_Image :TBitmap; Might:Single);
  procedure GaussovskoyeRazmytiyeOdnomernoye(in_Image: TBitmap; out_Image :TBitmap; Might:Single);
  procedure MediannayaFiltraciya(in_Image: TBitmap; out_Image :TBitmap; R: Integer);
  procedure MediannayaFiltraciyaVectornaya(in_Image: TBitmap; out_Image :TBitmap; R: Integer);
  procedure KNearestNeighborsFiltraciya(in_Image: TBitmap; out_Image :TBitmap; R: Integer; Might: Single);

  procedure BoxFilter(in_Image: TBitmap; out_Image :TBitmap);
  procedure Colorize(out_Image: TBitmap);
implementation
{---------------------------------------------------------------------------}
{ ѕростейший пример одномерной размывающей фильтрации изображени€ - усреднение
  трех соседних пикселей в строке}

procedure BoxFilter(in_Image: TBitmap; out_Image :TBitmap);
var
  iY, iX :Integer;
  pSrcLine, pDstLine : PByteArray;
begin
  {ќдномерное усреднение по строкам }
  for iY := 0 to in_Image.Height - 1 do
  begin
    { Ѕыстрый способ обработки пикселей - пр€мой доступ к пам€ти }
    pSrcLine := in_Image.ScanLine[iY];
    pDstLine := out_Image.ScanLine[iY];

    for iX := 1 to (in_Image.Width - 2) do
    begin
      { blue }
      pDstLine[iX * 3] := round((pSrcLine[iX * 3 - 3] + pSrcLine[iX * 3] + pSrcLine[iX * 3 + 3]) / 3);
      { green }
      pDstLine[iX * 3 + 1] := round((pSrcLine[iX * 3 - 3 + 1] + pSrcLine[iX * 3 + 1] + pSrcLine[iX * 3 + 3 + 1]) / 3);
      { red }
      pDstLine[iX * 3 + 2] := round((pSrcLine[iX * 3 - 3 + 2] + pSrcLine[iX * 3 + 2] + pSrcLine[iX * 3 + 3 + 2]) / 3);
    end;
  end;
end;



{---------------------------------------------------------------------------}
{  ѕростейший пример раскраски изображени€ }

procedure Colorize(out_Image :TBitmap);
var
  iY, iX :Integer;
  pDstLine : PByteArray;
begin
  for iY := 0 to out_Image.Height - 1 do
  begin
    { Ѕыстрый способ обработки пикселей - пр€мой доступ к пам€ти }
    pDstLine := out_Image.ScanLine[iY];

    for iX := 0 to (out_Image.Width - 1) do
    begin
      /// ƒелаем плавно измен€ющийс€ вдоль строк градиент
      // blue
      pDstLine[iX * 3] := iX mod 256;
      // green
      pDstLine[iX * 3 + 1] := (255 - iX) mod 256;
      // red
      pDstLine[iX * 3 + 2] := round(Abs(iX - out_Image.Width / 2)) mod 256;
    end;
  end;
end;

procedure WNoise(out_Image :TBitmap; Might:Integer);
var
  iY, iX :Integer;
  new: Integer;
  pDstLine : PByteArray;
begin
  for iY := 0 to out_Image.Height - 1 do
  begin
    pDstLine := out_Image.ScanLine[iY];
    for iX := 0 to (out_Image.Width - 1) do
    begin
      new := pDstLine[iX * 3] + Random(Might) - Random(Might);
      if (new < 0) then new := 0;
      if (new > 255) then new := 255;
      pDstLine[iX * 3] := new;
      new := pDstLine[iX * 3 + 1] + Random(Might) - Random(Might);
      if (new < 0) then new := 0;
      if (new > 255) then new := 255;
      pDstLine[iX * 3 + 1] := new;
      new := pDstLine[iX * 3 + 2] + Random(Might) - Random(Might);
      if (new < 0) then new := 0;
      if (new > 255) then new := 255;
      pDstLine[iX * 3 + 2] := new;
    end;
  end;
end;

procedure INoise(out_Image :TBitmap; Might:Integer);
var
  iY, iX :Integer;
  new: Integer;
  pDstLine : PByteArray;
begin
  for iY := 0 to out_Image.Height - 1 do
  begin
    pDstLine := out_Image.ScanLine[iY];
    for iX := 0 to (out_Image.Width - 1) do
    if Random(100) < Might then
    begin
      new := pDstLine[iX * 3] + Random(256) - Random(256);
      if (new < 0) then new := 0;
      if (new > 255) then new := 255;
      pDstLine[iX * 3] := new;
      new := pDstLine[iX * 3 + 1] + Random(256) - Random(256);
      if (new < 0) then new := 0;
      if (new > 255) then new := 255;
      pDstLine[iX * 3 + 1] := new;
      new := pDstLine[iX * 3 + 2] + Random(256) - Random(256);
      if (new < 0) then new := 0;
      if (new > 255) then new := 255;
      pDstLine[iX * 3 + 2] := new;
    end;
  end;
end;

procedure Razmytiye(in_Image: TBitmap; out_Image :TBitmap);
var
  iY, iX, Znam:Integer;
  new: array[0..2] of Integer;
  pSrcLine, pDstLine : array[-1..1] of PByteArray;
begin
  for iY := 0 to out_Image.Height - 1 do
  begin
    if iY - 1 >= 0 then
    begin
      pSrcLine[-1] := in_Image.ScanLine[iY-1];
      pDstLine[-1] := out_Image.ScanLine[iY-1];
    end;
    pSrcLine[0] := in_Image.ScanLine[iY];
    pDstLine[0] := out_Image.ScanLine[iY];
    if iY + 1 <= out_Image.Height - 1 then
    begin
      pSrcLine[1] := in_Image.ScanLine[iY+1];
      pDstLine[1] := out_Image.ScanLine[iY+1];
    end;
    for iX := 0 to (out_Image.Width - 1) do
    begin
      new[0] := 0;
      new[1] := 0;
      new[2] := 0;
      Znam := 2;
      if iX - 1 >= 0 then
      begin
        new[0] := new[0] + pSrcLine[0][(iX - 1) * 3];
        new[1] := new[1] + pSrcLine[0][(iX - 1) * 3 + 1];
        new[2] := new[2] + pSrcLine[0][(iX - 1) * 3 + 2];
        Znam := Znam + 1;
      end;
      if iX + 1 <= out_Image.Width - 1 then
      begin
        new[0] := new[0] + pSrcLine[0][(iX + 1) * 3];
        new[1] := new[1] + pSrcLine[0][(iX + 1) * 3 + 1];
        new[2] := new[2] + pSrcLine[0][(iX + 1) * 3 + 2];
        Znam := Znam + 1;
      end;
      if iY - 1 >= 0 then
      begin
        new[0] := new[0] + pSrcLine[-1][iX * 3];
        new[1] := new[1] + pSrcLine[-1][iX * 3 + 1];
        new[2] := new[2] + pSrcLine[-1][iX * 3 + 2];
        Znam := Znam + 1;
      end;
      if iY + 1 <= out_Image.Height - 1 then
      begin
        new[0] := new[0] + pSrcLine[1][iX * 3];
        new[1] := new[1] + pSrcLine[1][iX * 3 + 1];
        new[2] := new[2] + pSrcLine[1][iX * 3 + 2];
        Znam := Znam + 1;
      end;
      new[0] := new[0] + 2 * pSrcLine[0][iX * 3];
      new[1] := new[1] + 2 * pSrcLine[0][iX * 3 + 1];
      new[2] := new[2] + 2 * pSrcLine[0][iX * 3 + 2];

      new[0] := new[0] div Znam;
      new[1] := new[1] div Znam;
      new[2] := new[2] div Znam;

      pDstLine[0][iX * 3] := new[0];
      pDstLine[0][iX * 3 + 1] := new[1];
      pDstLine[0][iX * 3 + 2] := new[2];
    end;
  end;
end;

procedure PovysheniyeChetkosti(in_Image: TBitmap; out_Image :TBitmap);
var
  iY, iX, Znam:Integer;
  new: array[0..2] of Integer;
  pSrcLine, pDstLine : array[-1..1] of PByteArray;
begin
  for iY := 0 to out_Image.Height - 1 do
  begin
    if iY - 1 >= 0 then
    begin
      pSrcLine[-1] := in_Image.ScanLine[iY-1];
      pDstLine[-1] := out_Image.ScanLine[iY-1];
    end;
    pSrcLine[0] := in_Image.ScanLine[iY];
    pDstLine[0] := out_Image.ScanLine[iY];
    if iY + 1 <= out_Image.Height - 1 then
    begin
      pSrcLine[1] := in_Image.ScanLine[iY+1];
      pDstLine[1] := out_Image.ScanLine[iY+1];
    end;
    for iX := 0 to (out_Image.Width - 1) do
    begin
      new[0] := 0;
      new[1] := 0;
      new[2] := 0;
      Znam := 22;
      if iX - 1 >= 0 then
      begin
        new[0] := new[0] - 2 * pSrcLine[0][(iX - 1) * 3];
        new[1] := new[1] - 2 * pSrcLine[0][(iX - 1) * 3 + 1];
        new[2] := new[2] - 2 * pSrcLine[0][(iX - 1) * 3 + 2];
        Znam := Znam - 2;
      end;
      if iX + 1 <= out_Image.Width - 1 then
      begin
        new[0] := new[0] - 2 * pSrcLine[0][(iX + 1) * 3];
        new[1] := new[1] - 2 * pSrcLine[0][(iX + 1) * 3 + 1];
        new[2] := new[2] - 2 * pSrcLine[0][(iX + 1) * 3 + 2];
        Znam := Znam - 2;
      end;
      if iY - 1 >= 0 then
      begin
        new[0] := new[0] - 2 * pSrcLine[-1][iX * 3];
        new[1] := new[1] - 2 * pSrcLine[-1][iX * 3 + 1];
        new[2] := new[2] - 2 * pSrcLine[-1][iX * 3 + 2];
        Znam := Znam - 2;
      end;
      if iY + 1 <= out_Image.Height - 1 then
      begin
        new[0] := new[0] - 2 * pSrcLine[1][iX * 3];
        new[1] := new[1] - 2 * pSrcLine[1][iX * 3 + 1];
        new[2] := new[2] - 2 * pSrcLine[1][iX * 3 + 2];
        Znam := Znam - 2;
      end;
      if (iX - 1 >= 0) and (iY - 1 >= 0) then
      begin
        new[0] := new[0] - 1 * pSrcLine[-1][(iX - 1) * 3];
        new[1] := new[1] - 1 * pSrcLine[-1][(iX - 1) * 3 + 1];
        new[2] := new[2] - 1 * pSrcLine[-1][(iX - 1) * 3 + 2];
        Znam := Znam - 1;
      end;
      if (iX - 1 >= 0) and (iY + 1 <= out_Image.Height - 1) then
      begin
        new[0] := new[0] - 1 * pSrcLine[1][(iX - 1) * 3];
        new[1] := new[1] - 1 * pSrcLine[1][(iX - 1) * 3 + 1];
        new[2] := new[2] - 1 * pSrcLine[1][(iX - 1) * 3 + 2];
        Znam := Znam - 1;
      end;
      if (iX + 1 <= out_Image.Width - 1) and (iY - 1 >= 0) then
      begin
        new[0] := new[0] - 1 * pSrcLine[-1][(iX + 1) * 3];
        new[1] := new[1] - 1 * pSrcLine[-1][(iX + 1) * 3 + 1];
        new[2] := new[2] - 1 * pSrcLine[-1][(iX + 1) * 3 + 2];
        Znam := Znam - 1;
      end;
      if (iX + 1 <= out_Image.Width - 1) and (iY + 1 <= out_Image.Height - 1) then
      begin
        new[0] := new[0] - 1 * pSrcLine[1][(iX + 1) * 3];
        new[1] := new[1] - 1 * pSrcLine[1][(iX + 1) * 3 + 1];
        new[2] := new[2] - 1 * pSrcLine[1][(iX + 1) * 3 + 2];
        Znam := Znam - 1;
      end;
      new[0] := new[0] + 22 * pSrcLine[0][iX * 3];
      new[1] := new[1] + 22 * pSrcLine[0][iX * 3 + 1];
      new[2] := new[2] + 22 * pSrcLine[0][iX * 3 + 2];

      new[0] := new[0] div Znam;
      new[1] := new[1] div Znam;
      new[2] := new[2] div Znam;

      if (new[0] < 0) then new[0] := 0;
      if (new[0] > 255) then new[0] := 255;
      if (new[1] < 0) then new[1] := 0;
      if (new[1] > 255) then new[1] := 255;
      if (new[2] < 0) then new[2] := 0;
      if (new[2] > 255) then new[2] := 255;
      
      pDstLine[0][iX * 3] := new[0];
      pDstLine[0][iX * 3 + 1] := new[1];
      pDstLine[0][iX * 3 + 2] := new[2];
    end;
  end;
end;

procedure NahozhdeniyeGranic(in_Image: TBitmap; out_Image :TBitmap);
var
  iY, iX, Mnozh:Integer;
  new: array[0..2] of Integer;
  pSrcLine, pDstLine : array[-1..1] of PByteArray;
begin
  for iY := 0 to out_Image.Height - 1 do
  begin
    if iY - 1 >= 0 then
    begin
      pSrcLine[-1] := in_Image.ScanLine[iY-1];
      pDstLine[-1] := out_Image.ScanLine[iY-1];
    end;
    pSrcLine[0] := in_Image.ScanLine[iY];
    pDstLine[0] := out_Image.ScanLine[iY];
    if iY + 1 <= out_Image.Height - 1 then
    begin
      pSrcLine[1] := in_Image.ScanLine[iY+1];
      pDstLine[1] := out_Image.ScanLine[iY+1];
    end;
    for iX := 0 to (out_Image.Width - 1) do
    begin
      new[0] := 0;
      new[1] := 0;
      new[2] := 0;
      Mnozh := 0;
      if iX - 1 >= 0 then
      begin
        new[0] := new[0] - pSrcLine[0][(iX - 1) * 3];
        new[1] := new[1] - pSrcLine[0][(iX - 1) * 3 + 1];
        new[2] := new[2] - pSrcLine[0][(iX - 1) * 3 + 2];
        Mnozh := Mnozh + 1;
      end;
      if iX + 1 <= out_Image.Width - 1 then
      begin
        new[0] := new[0] - pSrcLine[0][(iX + 1) * 3];
        new[1] := new[1] - pSrcLine[0][(iX + 1) * 3 + 1];
        new[2] := new[2] - pSrcLine[0][(iX + 1) * 3 + 2];
        Mnozh := Mnozh + 1;
      end;
      if iY - 1 >= 0 then
      begin
        new[0] := new[0] - pSrcLine[-1][iX * 3];
        new[1] := new[1] - pSrcLine[-1][iX * 3 + 1];
        new[2] := new[2] - pSrcLine[-1][iX * 3 + 2];
        Mnozh := Mnozh + 1;
      end;
      if iY + 1 <= out_Image.Height - 1 then
      begin
        new[0] := new[0] - pSrcLine[1][iX * 3];
        new[1] := new[1] - pSrcLine[1][iX * 3 + 1];
        new[2] := new[2] - pSrcLine[1][iX * 3 + 2];
        Mnozh := Mnozh + 1;
      end;
      new[0] := new[0] + Mnozh * pSrcLine[0][iX * 3];
      new[1] := new[1] + Mnozh * pSrcLine[0][iX * 3 + 1];
      new[2] := new[2] + Mnozh * pSrcLine[0][iX * 3 + 2];

      if (new[0] < 0) then new[0] := 0;
      if (new[0] > 255) then new[0] := 255;
      if (new[1] < 0) then new[1] := 0;
      if (new[1] > 255) then new[1] := 255;
      if (new[2] < 0) then new[2] := 0;
      if (new[2] > 255) then new[2] := 255;

      pDstLine[0][iX * 3] := new[0];
      pDstLine[0][iX * 3 + 1] := new[1];
      pDstLine[0][iX * 3 + 2] := new[2];
    end;
  end;
end;

procedure GaussovskoyeRazmytiyeDvumernoye(in_Image: TBitmap; out_Image :TBitmap; Might:Single);
var
  Koefs : array[-100..100,-100..100] of Single;
  Sum : Single;
  iY, iX, dX, dY, R:Integer;
  new: array[0..2] of Single;
  pSrc, pDst : PByteArray;
  iBytesPerLine : Integer;
begin
  if Might = 0 then
    Exit;
  for iX := -100 to 100 do
  for iY := -100 to 100 do
    Koefs[iX][iY] := exp(-(iX * iX + iY * iY) / (2 * Might * Might));
  for R := 1 to 100 do
    if Koefs[R][0] < 0.01 then
      Break;
  Sum := 0;
  for iX := -R to R do
  for iY := -R to R do
    Sum := Sum + Koefs[iX][iY];
  for iX := -R to R do
  for iY := -R to R do
    Koefs[iX][iY] := Koefs[iX][iY] / Sum;

  iBytesPerLine := (out_Image.Width * 3 + 3) and -4;
  pSrc := in_Image.ScanLine[in_Image.Height - 1];
  pDst := out_Image.ScanLine[out_Image.Height - 1];
  for iY := 0 to out_Image.Height - 1 do
  begin
    for iX := 0 to (out_Image.Width - 1) do
    begin
      new[0] := 0;
      new[1] := 0;
      new[2] := 0;
      Sum := 0;
      for dX := -R to R do
      for dY := -R to R do
      begin
        if (iX + dX >= 0) and (iX + dX <= out_Image.Width - 1) and (iY + dY >= 0) and (iY + dY <= out_Image.Height - 1) then
        begin
          Sum := Sum + Koefs[dX][dY];
          new[0] := new[0] + Koefs[dX][dY] * pSrc[(iY + dY) * iBytesPerLine + (iX + dX) * 3];
          new[1] := new[1] + Koefs[dX][dY] * pSrc[(iY + dY) * iBytesPerLine + (iX + dX) * 3 + 1];
          new[2] := new[2] + Koefs[dX][dY] * pSrc[(iY + dY) * iBytesPerLine + (iX + dX) * 3 + 2];
        end;
      end;
      new[0] := new[0] / Sum;
      new[1] := new[1] / Sum;
      new[2] := new[2] / Sum;
      pDst[iY * iBytesPerLine + iX * 3] := Round(new[0]);
      pDst[iY * iBytesPerLine + iX * 3 + 1] := Round(new[1]);
      pDst[iY * iBytesPerLine + iX * 3 + 2] := Round(new[2]);
    end;
  end;
end;

procedure GaussovskoyeRazmytiyeOdnomernoye(in_Image: TBitmap; out_Image :TBitmap; Might:Single);
var
  Koefs : array[-100..100] of Single;
  Sum : Single;
  iY, iX, dX, dY, R:Integer;
  new: array[0..2] of Single;
  pSrc, pDst : PByteArray;
  iBytesPerLine : Integer;
begin
  if Might = 0 then
    Exit;
  for iX := -100 to 100 do
    Koefs[iX] := exp(-(iX * iX) / (2 * Might * Might));
  for R := 1 to 100 do
    if Koefs[R] < 0.01 then
      Break;
  Sum := 0;
  for iX := -R to R do
    Sum := Sum + Koefs[iX];
  for iX := -R to R do
    Koefs[iX] := Koefs[iX] / Sum;

  iBytesPerLine := (out_Image.Width * 3 + 3) and -4;
  pSrc := in_Image.ScanLine[in_Image.Height - 1];
  pDst := out_Image.ScanLine[out_Image.Height - 1];
  for iY := 0 to out_Image.Height - 1 do
  begin
    for iX := 0 to (out_Image.Width - 1) do
    begin
      new[0] := 0;
      new[1] := 0;
      new[2] := 0;
      Sum := 0;
      for dX := -R to R do
      begin
        if (iX + dX >= 0) and (iX + dX <= out_Image.Width - 1) then
        begin
          Sum := Sum + Koefs[dX];
          new[0] := new[0] + Koefs[dX] * pDst[iY * iBytesPerLine + (iX + dX) * 3];
          new[1] := new[1] + Koefs[dX] * pDst[iY * iBytesPerLine + (iX + dX) * 3 + 1];
          new[2] := new[2] + Koefs[dX] * pDst[iY * iBytesPerLine + (iX + dX) * 3 + 2];
        end;
      end;
      new[0] := new[0] / Sum;
      new[1] := new[1] / Sum;
      new[2] := new[2] / Sum;
      pSrc[iY * iBytesPerLine + iX * 3] := Round(new[0]);
      pSrc[iY * iBytesPerLine + iX * 3 + 1] := Round(new[1]);
      pSrc[iY * iBytesPerLine + iX * 3 + 2] := Round(new[2]);
    end;
  end;
  for iY := 0 to out_Image.Height - 1 do
  begin
    for iX := 0 to (out_Image.Width - 1) do
    begin
      new[0] := 0;
      new[1] := 0;
      new[2] := 0;
      Sum := 0;
      for dY := -R to R do
      begin
        if (iY + dY >= 0) and (iY + dY <= out_Image.Height - 1) then
        begin
          Sum := Sum + Koefs[dY];
          new[0] := new[0] + Koefs[dY] * pSrc[(iY + dY) * iBytesPerLine + iX * 3];
          new[1] := new[1] + Koefs[dY] * pSrc[(iY + dY) * iBytesPerLine + iX * 3 + 1];
          new[2] := new[2] + Koefs[dY] * pSrc[(iY + dY) * iBytesPerLine + iX * 3 + 2];
        end;
      end;
      new[0] := new[0] / Sum;
      new[1] := new[1] / Sum;
      new[2] := new[2] / Sum;
      pDst[iY * iBytesPerLine + iX * 3] := Round(new[0]);
      pDst[iY * iBytesPerLine + iX * 3 + 1] := Round(new[1]);
      pDst[iY * iBytesPerLine + iX * 3 + 2] := Round(new[2]);
    end;
  end;
end;

var
  Gray : array[0..40000] of Byte;
  Color : array[0..40000,0..2] of Byte;
  Count : Integer;

procedure sort(l, r: Integer);
var
  i, j: integer;
  x, y: Byte;
begin
  i := l; j := r; x := Gray[(l + r) div 2];
  repeat
    while Gray[i] < x do i := i + 1;
    while x < Gray[j] do j := j - 1;
    if i <= j then
    begin
      y := Gray[i]; Gray[i] := Gray[j]; Gray[j] := y;
      y := Color[i][0]; Color[i][0] := Color[j][0]; Color[j][0] := y;
      y := Color[i][1]; Color[i][1] := Color[j][1]; Color[j][1] := y;
      y := Color[i][2]; Color[i][2] := Color[j][2]; Color[j][2] := y;
      i := i + 1; j := j - 1;
    end;
  until i > j;
  if l < j then sort(l, j);
  if i < r then sort(i, r);
end;
  

procedure MediannayaFiltraciya(in_Image: TBitmap; out_Image :TBitmap; R: Integer);
var
  iY, iX, dX, dY:Integer;
  pSrc, pDst : PByteArray;
  iBytesPerLine : Integer;
begin
  if R = 0 then
    Exit;
  if R > 99 then
    R := 99;
  iBytesPerLine := (out_Image.Width * 3 + 3) and -4;
  pSrc := in_Image.ScanLine[in_Image.Height - 1];
  pDst := out_Image.ScanLine[out_Image.Height - 1];
  for iY := 0 to out_Image.Height - 1 do
  begin
    for iX := 0 to (out_Image.Width - 1) do
    begin
      Count := 0;
      for dX := -R to R do
      for dY := -R to R do
      begin
        if (iX + dX >= 0) and (iX + dX <= out_Image.Width - 1) and (iY + dY >= 0) and (iY + dY <= out_Image.Height - 1) then
        begin
          Gray[Count] := Round( 0.114 * pSrc[(iY + dY) * iBytesPerLine + (iX + dX) * 3] +
                                0.587 * pSrc[(iY + dY) * iBytesPerLine + (iX + dX) * 3 + 1] +
                                0.299 * pSrc[(iY + dY) * iBytesPerLine + (iX + dX) * 3 + 2] );
          Color[Count][0] := pSrc[(iY + dY) * iBytesPerLine + (iX + dX) * 3];
          Color[Count][1] := pSrc[(iY + dY) * iBytesPerLine + (iX + dX) * 3 + 1];
          Color[Count][2] := pSrc[(iY + dY) * iBytesPerLine + (iX + dX) * 3 + 2];
          Inc(Count);
        end;
      end;
      sort(0, Count-1);
      pDst[iY * iBytesPerLine + iX * 3] :=     Color[Count div 2][0];
      pDst[iY * iBytesPerLine + iX * 3 + 1] := Color[Count div 2][1];
      pDst[iY * iBytesPerLine + iX * 3 + 2] := Color[Count div 2][2];
    end;
  end;
end;

procedure MediannayaFiltraciyaVectornaya(in_Image: TBitmap; out_Image :TBitmap; R: Integer);
var
  Distance, BestDistance, Besti, i, j, iY, iX, dX, dY:Integer;
  pSrc, pDst : PByteArray;
  iBytesPerLine : Integer;
begin
  if R = 0 then
    Exit;
  if R > 99 then
    R := 99;
  iBytesPerLine := (out_Image.Width * 3 + 3) and -4;
  pSrc := in_Image.ScanLine[in_Image.Height - 1];
  pDst := out_Image.ScanLine[out_Image.Height - 1];
  for iY := 0 to out_Image.Height - 1 do
  begin
    for iX := 0 to (out_Image.Width - 1) do
    begin
      Count := 0;
      for dX := -R to R do
      for dY := -R to R do
      begin
        if (iX + dX >= 0) and (iX + dX <= out_Image.Width - 1) and (iY + dY >= 0) and (iY + dY <= out_Image.Height - 1) then
        begin
          Color[Count][0] := pSrc[(iY + dY) * iBytesPerLine + (iX + dX) * 3];
          Color[Count][1] := pSrc[(iY + dY) * iBytesPerLine + (iX + dX) * 3 + 1];
          Color[Count][2] := pSrc[(iY + dY) * iBytesPerLine + (iX + dX) * 3 + 2];
          Inc(Count);
        end;
      end;
      BestDistance := 2000000000;
      for i := 0 to Count - 1 do
      begin
        Distance := 0;
        for j := 0 to Count - 1 do
          Distance := Distance + abs(Color[i][0] - Color[j][0]) + abs(Color[i][1] - Color[j][1]) + abs(Color[i][2] - Color[j][2]);
        if Distance < BestDistance then
        begin
          BestDistance := Distance;
          Besti := i;
        end;
      end;
      pDst[iY * iBytesPerLine + iX * 3] :=     Color[Besti][0];
      pDst[iY * iBytesPerLine + iX * 3 + 1] := Color[Besti][1];
      pDst[iY * iBytesPerLine + iX * 3 + 2] := Color[Besti][2];
    end;
  end;
end;

procedure KNearestNeighborsFiltraciya(in_Image: TBitmap; out_Image :TBitmap; R: Integer; Might: Single);
var
  iY, iX, dX, dY: Integer;
  pSrc, pDst : PByteArray;
  iBytesPerLine : Integer;
  new : array[0..2] of Single;
  Sum : Single;
  Distance: Single;
  K: Single;
begin
  if (R = 0) or (Might = 0) then
    Exit;
  iBytesPerLine := (out_Image.Width * 3 + 3) and -4;
  pSrc := in_Image.ScanLine[in_Image.Height - 1];
  pDst := out_Image.ScanLine[out_Image.Height - 1];
  for iY := 0 to out_Image.Height - 1 do
  begin
    for iX := 0 to (out_Image.Width - 1) do
    begin
      new[0] := 0;
      new[1] := 0;
      new[2] := 0;
      Sum := 0;
      for dX := -R to R do
      for dY := -R to R do
      begin
        if (iX + dX >= 0) and (iX + dX <= out_Image.Width - 1) and (iY + dY >= 0) and (iY + dY <= out_Image.Height - 1) then
        begin
          Distance := sqr(pSrc[(iY + dY) * iBytesPerLine + (iX + dX) * 3]     - pSrc[iY * iBytesPerLine + iX * 3]) +
                      sqr(pSrc[(iY + dY) * iBytesPerLine + (iX + dX) * 3 + 1] - pSrc[iY * iBytesPerLine + iX * 3 + 1]) +
                      sqr(pSrc[(iY + dY) * iBytesPerLine + (iX + dX) * 3 + 2] - pSrc[iY * iBytesPerLine + iX * 3 + 2]);
          K := exp(-Distance / (2 * Might * Might));
          Sum := Sum + K;
          new[0] := new[0] + K * pSrc[(iY + dY) * iBytesPerLine + (iX + dX) * 3];
          new[1] := new[1] + K * pSrc[(iY + dY) * iBytesPerLine + (iX + dX) * 3 + 1];
          new[2] := new[2] + K * pSrc[(iY + dY) * iBytesPerLine + (iX + dX) * 3 + 2];
        end;
      end;
      new[0] := new[0] / Sum;
      new[1] := new[1] / Sum;
      new[2] := new[2] / Sum;
      pDst[iY * iBytesPerLine + iX * 3] := Round(new[0]);
      pDst[iY * iBytesPerLine + iX * 3 + 1] := Round(new[1]);
      pDst[iY * iBytesPerLine + iX * 3 + 2] := Round(new[2]);
    end;
  end;
end;

end.
