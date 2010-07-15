unit Unit3;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons;

type
  TPasswordDlg1 = class(TForm)
    Label1: TLabel;
    Password: TEdit;
    OKBtn: TButton;
    CancelBtn: TButton;
    Edit1: TEdit;
    Label2: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure Dialog2(s,s1:String; def,def1:Real; var val,val1:Real);

var
  PasswordDlg1: TPasswordDlg1;

implementation

{$R *.dfm}

procedure Dialog2(s,s1:String; def,def1:Real; var val,val1:Real);
begin
  PasswordDlg1.Show;
  PasswordDlg1.Password.SetFocus;
  PasswordDlg1.Hide;
  PasswordDlg1.Label1.Caption:=s;
  PasswordDlg1.Password.Text:='';
  PasswordDlg1.Label2.Caption:=s1;
  PasswordDlg1.Edit1.Text:='';
  if PasswordDlg1.ShowModal=mrOk then
  begin;
    try
      val:=StrToFloat(PasswordDlg1.Password.Text);
      val1:=StrToFloat(PasswordDlg1.Edit1.Text);
    except
      on E:Exception do
      begin;
        Application.MessageBox(PAnsiChar(E.Message),'Ошибка');
        val:=def;
        val1:=def1;
      end;
    end;
  end
  else
  begin;
    val:=def;
    val1:=def1;
  end;
end;


end.
 
