program DeplpiBasis;

uses
  Forms,
  MainForm in 'MainForm.pas' {Form1},
  AboutBox in 'AboutBox.pas' {Form2},
  Processing in 'Processing.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Simple Image Processing';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
