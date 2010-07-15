unit Unit2;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons;

type
  TPasswordDlg = class(TForm)
    Label1: TLabel;
    Password: TEdit;
    OKBtn: TButton;
    CancelBtn: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PasswordDlg: TPasswordDlg;

function Dialog(s:String; def:Real):Real;

implementation
{$R *.dfm}

function Dialog(s:String; def:Real):Real;
begin
  PasswordDlg.Label1.Caption:=s;
  PasswordDlg.Password.Text:='';
  if PasswordDlg.ShowModal=mrOk then
  begin;
    try
      Result:=StrToFloat(PasswordDlg.Password.Text);
    except
      on E:Exception do
      begin;
        Application.MessageBox(PAnsiChar(E.Message),'Ошибка');
        Result:=def;
      end;
    end;
  end
  else
    Result:=def;
end;


end.

