program Noise;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas' {PasswordDlg},
  Unit3 in 'Unit3.pas' {PasswordDlg1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TPasswordDlg, PasswordDlg);
  Application.CreateForm(TPasswordDlg1, PasswordDlg1);
  Application.Run;
end.
