unit uLogConsole;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, sSkinProvider, sSkinManager;

type
  Tfrm_LogConsole = class(TForm)
    Log: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    procedure WMCopyData(var Msg : TWMCopyData); message WM_COPYDATA;
  public
    { Public declarations }
  end;

var
  frm_LogConsole: Tfrm_LogConsole;

implementation

{$R *.dfm}
type
  TCopyDataType = (cdtString = 0, cdtImage = 1, cdtRecord = 2);

// **********************************************
// Receive and show messages sent via WM_CopyData
// **********************************************
procedure Tfrm_LogConsole.WMCopyData(var Msg: TWMCopyData);
var
  copyDataType : TCopyDataType;
begin
  copyDataType := TCopyDataType(Msg.CopyDataStruct.dwData);
  // We only care for string messages
  if copyDataType = cdtString then begin
    Log.Lines.Add(PChar(Msg.CopyDataStruct.lpData));
  end;
end;

procedure Tfrm_LogConsole.FormCreate(Sender: TObject);
begin
  Log.DoubleBuffered := True;
end;

end.
