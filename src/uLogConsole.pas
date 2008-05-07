unit uLogConsole;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  // AlphaSkins
  sSkinProvider, sSkinManager;

type
  Tfrm_LogConsole = class(TForm)
    Log: TMemo;
    procedure FormCreate(Sender: TObject);
  end;

var
  frm_LogConsole: Tfrm_LogConsole;

implementation

{$R *.dfm}

procedure Tfrm_LogConsole.FormCreate(Sender: TObject);
begin
  Log.DoubleBuffered := True;
end;

end.
