program DeplpiBasis;

uses
  Forms,
  MainForm in 'MainForm.pas',
  MyDialogBox in 'MyDialogBox.pas' {Form1},
  MyDialogBox2 in 'MyDialogBox2.pas' {Form2};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Simple Image Processing';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
