program TranslationChecker;

uses
  Forms,
  uTypes in '..\..\src2\uTypes.pas',
  uTranslationChecker in '..\..\src2\Tools\TranslationChecker\uTranslationChecker.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
