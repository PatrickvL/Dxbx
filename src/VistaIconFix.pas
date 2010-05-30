unit VistaIconFix;
// By Eric Thorniley
// Source : https://forums.codegear.com/thread.jspa?threadID=8769

interface

implementation

uses
  Forms, Windows, ShellAPI;

var
  IconHandle: THandle;

initialization
  IconHandle := ExtractIcon(MainInstance, PChar(Application.ExeName), 0);
  if IconHandle > 0 then
    Application.Icon.Handle := IconHandle;
finalization
  Application.Icon.Handle := 0;
  DestroyIcon(IconHandle);
end.
