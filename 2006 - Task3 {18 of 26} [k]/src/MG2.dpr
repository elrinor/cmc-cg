// NAME: TUMAKOV KIRILL ALEKSANDROVICH, 203
// ASGN: N3
program MG2;

uses
  Forms,
  main in 'main.pas' {Form1},
  Filters in 'Filters.pas',
  NumberDialog in 'NumberDialog.pas' {Form2},
  Recognition in 'Recognition.pas',
  RecognitionResult in 'RecognitionResult.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
