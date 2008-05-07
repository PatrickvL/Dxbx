unit uEmuFS;

interface

procedure EmuSwapFS;
procedure EmuCleanupFS;
function EmuIsXboxFS: Boolean;

var
  EmuAutoSleepRate: Integer = -1;

implementation

uses
  // Delphi
  SysUtils;

function EmuIsXboxFS: Boolean;
var
  chk: Char;
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
  dwInterceptionCount: Integer = 0;
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
  if (dwInterceptionCount + 1) >= EmuAutoSleepRate then
  begin
    // If we're in the Xbox FS, wait until the next swap
    if EmuIsXboxFS then
      Dec(dwInterceptionCount)
    else
    begin
      // Yield!
      Sleep(1);

      // Back to Zero!
      dwInterceptionCount := 0;
    end;
  end;
end; // EmuSwapFS

procedure EmuCleanupFS;
//var
//  wSwapFS: Integer;
begin
(*{
    uint16 wSwapFS = 0;

    __asm
    {
        mov ax, fs:[0x14]   // FS.ArbitraryUserPointer
        mov wSwapFS, ax
    }

    if(wSwapFS == 0)
        return;

    if(!EmuIsXboxFS())
        EmuSwapFS();    // Xbox FS

    uint08 *pTLSData = NULL;

    __asm
    {
        mov eax, fs:[0x04]
        mov pTLSData, eax
    }

    EmuSwapFS(); // Win2k/XP FS

    if(pTLSData != 0)
        delete[] pTLSData;

    EmuDeallocateLDT(wSwapFS);
}          *)
end;

end.
