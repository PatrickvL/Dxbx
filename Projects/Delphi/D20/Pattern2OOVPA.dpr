program Pattern2OOVPA;

uses
  Vcl.Forms,
  Unit2 in '..\..\..\Source\Delphi\src\Tools\Pattern2OOVPA\Unit2.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
