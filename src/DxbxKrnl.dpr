library DxbxKrnl;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  Windows,
  SysUtils,
  Dialogs,
  Classes,
  uEmuShared in 'uEmuShared.pas',
  uEmu in 'uEmu.pas',
  uEnums in 'uEnums.pas',
  uLog in 'uLog.pas',
  uLogConsole in 'uLogConsole.pas' {frm_LogConsole},
  uEmuFS in 'uEmuFS.pas',
  uXbe in 'uXbe.pas',
  uConsts in 'uConsts.pas',
  uMutex in 'uMutex.pas',
  uKernelThunk in 'uKernelThunk.pas';

{$R *.res}   

  Exports
    EmuInit name '_EmuInit@36',
    SetXbePath name '?SetXbePath@EmuShared@@QAEXPBD@Z';


(*  Exports EmuVerifyVersion name '_EmuVerifyVersion@4';
  Exports EmuPanic name '_EmuPanic@0';
  Exports EmuNoFunc name '_EmuNoFunc@0';
  Exports EmuCleanup;
  Exports EmuCleanThread name '_EmuCleanThread@0';
  { TODO : name need to be set }
  (*Exports Init; // name must be "void EmuShared::Init (void)
  //  Exports KernelThunkTable;*)

procedure DllMain ( Reason : Integer );
begin
  if Reason = DLL_PROCESS_ATTACH then begin
     Init;
  end
  else begin
    if Reason = DLL_PROCESS_DETACH then
      Cleanup;
  end;
end;


begin
  CreateLogs ( ltKernel );
  DllProc := DllMain;
  DllProc( DLL_PROCESS_ATTACH );
end.
