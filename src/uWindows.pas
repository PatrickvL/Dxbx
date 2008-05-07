unit uWindows;

interface

function IsWindowsVista: Boolean;
function GetTempDirectory: string;

implementation

Uses
  Windows;

function IsWindowsVista: Boolean;
var VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(VerInfo);
  Result := VerInfo.dwMajorVersion >= 6;
end;

function GetTempDirectory: string;
var
  Buffer: array[0..Max_path] of char;
begin
  FillChar(Buffer, Max_Path + 1, 0);
  GetTempPath(Max_path, Buffer);
  Result := string(Buffer);
  if Result[Length(Result)] <> '\' then Result := Result + '\';
end;



end.
