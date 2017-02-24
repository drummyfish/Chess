program Sachy;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  FNastaveni in 'FNastaveni.pas' {Form2},
  CVykreslovac in 'CVykreslovac.pas',
  FOProgramu in 'FOProgramu.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Šachy';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
