program ChartMaker;

uses
  Forms,
  ufrmMAin in 'ufrmMAin.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
