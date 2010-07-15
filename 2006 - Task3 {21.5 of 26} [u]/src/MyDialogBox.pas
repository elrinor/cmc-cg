unit MyDialogBox;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm2 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

function GetRadius: Integer;

implementation

{$R *.dfm}

procedure TForm2.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    Form2.ModalResult := mrOk;
  if Key = #27 then
    Form2.ModalResult := mrCancel;
end;

function GetRadius: Integer;
begin
  Form2.Edit1.Text := '';
  if Form2.ShowModal = mrOk then
    Result := StrToInt(Form2.Edit1.Text)
  else
    Result := 0;
end;

end.
