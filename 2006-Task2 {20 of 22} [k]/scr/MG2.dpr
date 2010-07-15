// NAME: TUMAKOV KIRILL ALEKSANDROVICH, 203
// ASGN: N2
program MG2;

uses
  Forms,
  main in 'main.pas' {Form1},
  Filters in 'Filters.pas',
  NumberDialog in 'NumberDialog.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
