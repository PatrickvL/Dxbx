unit uEmu;

interface

uses
  uEmuFS, uLogging;

  procedure EmuNoFunc; export;

implementation                

procedure EmuNoFunc; export;
begin
  //EmuSwapFS();   // Win2k/XP FS

  WriteLog( 'EmuNoFunc');
  //printf("Emu (0x%X): EmuNoFunc()\n", GetCurrentThreadId());

  //EmuSwapFS();   // XBox FS*)
end;



end.
