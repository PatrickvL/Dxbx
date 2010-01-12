program TranslationChecker;

uses
  Forms,
  uTranslationChecker in '..\..\src\Tools\TranslationChecker\uTranslationChecker.pas' {Form1},
  uTypes in '..\..\src\uTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
