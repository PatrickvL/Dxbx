program TranslationChecker;

uses
  Forms,
  uTypes in '..\..\..\Source\Delphi\src\uTypes.pas',
  uTranslationChecker in '..\..\..\Source\Delphi\src\Tools\TranslationChecker\uTranslationChecker.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
