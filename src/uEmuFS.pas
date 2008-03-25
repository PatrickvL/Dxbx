unit uEmuFS;

interface

procedure EmuSwapFS;

var
  EmuAutoSleepRate : Integer = -1;

implementation


uses
  SysUtils;


Function EmuIsXboxFS : Boolean;
var
  chk : char;
begin
  //  __asm
    {
        mov ah, fs:[0x16]
        mov chk, ah
    }

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

{    mov ax, fs:[0x14]
    mov fs, ax}

    // ******************************************************************
    // * Every "N" interceptions, perform various periodic services
    // ******************************************************************
    if((dwInterceptionCount + 1) >= EmuAutoSleepRate) then
    begin
        // If we're in the Xbox FS, wait until the next swap
        if EmuIsXboxFS then
        begin
          Dec (dwInterceptionCount );
        end;

        // Yield!
        Sleep(1);

        // Back to Zero!
        dwInterceptionCount := 0;
    end;
end;

end.
