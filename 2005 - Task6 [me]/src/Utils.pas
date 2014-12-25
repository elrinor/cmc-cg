unit Utils;

interface
uses Globals, Windows, ShellAPI, Forms, SysUtils, Graphics, CheckLst, Controls,
     Classes, Math;

procedure LoadImage(var Img:TBitmap; FileName:String);
function ExecuteFile(const FileName, Params, DefaultDir: string; ShowCmd: Integer): THandle;
function FileSizeToStr(n:Int64):String;


implementation
uses RxGIF, Jpeg;

//============================================================================\\
// File Size to String
//============================================================================\\
function FileSizeToStr(n:Int64):String;
begin;
if n<1024 then Result:=IntToStr(n)+' bytes'
else if n<1024*1024 then Result:=IntToStr(n div 1024)+','+IntToStr(Round((n mod 1024)/1024*10))+' KB'+' ('+IntToStr(n)+' bytes)'
else Result:=IntToStr(n div (1024*1024))+','+IntToStr(Round((n mod 1024*1024)/(1024*1024)*10))+' MB'+' ('+IntToStr(n)+' bytes)';
end;


//============================================================================\\
// Execute File
//============================================================================\\
function ExecuteFile(const FileName, Params, DefaultDir: string; ShowCmd: Integer): THandle;
var zFileName, zParams, zDir: array[0..79] of Char;
begin
Result := ShellExecute(Application.MainForm.Handle, nil, StrPCopy(zFileName, FileName), StrPCopy(zParams, Params), StrPCopy(zDir, DefaultDir), ShowCmd);
end;


//============================================================================\\
// Load Image
//============================================================================\\
procedure LoadImage(var Img:TBitmap; FileName:String);
var ext:String;
    GifImage:TGIFImage;
    JpgImage:TJPEGImage;
    IcoImage:TIcon;
begin;
{Img.Free;}
Img:=TBitmap.Create;
ext:=ExtractFileExt(FileName);
ext:=UpperCase(Copy(ext,2,Length(ext)-1));
if (ext='BMP') then
  begin;
  Img.LoadFromFile(FileName);
  end;
if (ext='GIF') then
  begin;
  GifImage:=TGifImage.Create;
  GifImage.LoadFromFile(FileName);
  Img.Height:=GifImage.Frames[0].Bitmap.Height;
  Img.Width:=GifImage.Frames[0].Bitmap.Width;
  Img.Canvas.Draw(0,0,GifImage.Frames[0].Bitmap);
  GifImage.Free;
  end;
if (ext='JPG')or(ext='JPEG') then
  begin;
  JpgImage:=TJPEGImage.Create;
  JpgImage.LoadFromFile(FileName);
  Img.Height:=JpgImage.Height;
  Img.Width:=JpgImage.Width;
  Img.Canvas.Draw(0,0,JpgImage);
  JpgImage.Free;
  end;
if (ext='ICO') then
  begin;
  IcoImage:=TIcon.Create;
  IcoImage.LoadFromFile(FileName);
  Img.Height:=IcoImage.Height;
  Img.Width:=IcoImage.Width;
  Img.Canvas.Draw(0,0,IcoImage);
  IcoImage.Free;
  end;
end;



end.
