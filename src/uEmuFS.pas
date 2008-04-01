unit uEmuFS;

interface

procedure EmuSwapFS;
function EmuIsXboxFS : Boolean;

var
  EmuAutoSleepRate : Integer = -1;

implementation

uses
  SysUtils;


function EmuIsXboxFS : Boolean;
var
  chk : char;
begin
  asm
    mov ah, fs:16h
    mov chk, ah
  end;
  Result := (chk = '1');
end;

procedure EmuSwapFS;
const
{$J+}
  dwInterceptionCount : Integer = 0;
{$J-}
begin
  // Note that this is only the *approximate* interception count,
  // because not all interceptions swap the FS register, and some
  // non-interception code uses it

  asm
    mov ax, fs:14h
    mov fs, ax
  end;

  // Every "N" interceptions, perform various periodic services
  if((dwInterceptionCount + 1) >= EmuAutoSleepRate) then
  begin
    // If we're in the Xbox FS, wait until the next swap
    if EmuIsXboxFS then
    begin
      Dec (dwInterceptionCount );
    end
    else
    begin
      // Yield!
      Sleep(1);

      // Back to Zero!
      dwInterceptionCount := 0;
    end;
  end;
end; // EmuSwapFS

end.
