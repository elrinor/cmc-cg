// NAME: TUMAKOV KIRILL ALEKSANDROVICH, 203
// ASGN: N2
unit NumberDialog;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TForm2 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    BitBtn1: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

function RequestNumber(Caption, Request:string; DefaultValue:Integer):Integer;
function RequestFloat(Caption, Request:string; DefaultValue:Single):Single;


implementation
{$R *.dfm}

function RequestNumber(Caption, Request:string; DefaultValue:Integer):Integer;
begin;
Form2.Label1.Caption:=Request;
Form2.Edit1.Text:='';
Form2.Caption:=Caption;
Form2.ShowModal;
try
  Result:=StrToInt(Form2.Edit1.Text);
except
  on E:Exception do
    Result:=DefaultValue;
end;
end;

function RequestFloat(Caption, Request:string; DefaultValue:Single):Single;
var s:String;
begin;
Form2.Label1.Caption:=Request;
Form2.Edit1.Text:='';
Form2.Caption:=Caption;
Form2.ShowModal;
try
  s:=Form2.Edit1.Text;
  while pos('.',s)<>0 do s[pos('.',s)]:=',';
  Result:=StrToFloat(s);
except
  on E:Exception do
    Result:=DefaultValue;
end;
end;

end.

