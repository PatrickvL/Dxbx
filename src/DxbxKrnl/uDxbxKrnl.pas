(*
    This file is part of Dxbx - a XBox emulator written in Delphi (ported over from cxbx)
    Copyright (C) 2007 Shadow_tj and other members of the development team.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)
unit uDxbxKrnl;

{$INCLUDE Dxbx.inc}

{.$DEFINE LOG_STRUCT_SIZES}

interface

uses
  // Delphi
  Windows, // DWord
  Messages, // WM_DESTROY
  SysUtils, // Format
  Math, // IfThen
  // Jedi Win32API
  JwaWinType,
  // Dxbx
  uConsts,
  uTypes,
  uDxbxUtils,
  uLog,
  uXbe,
  uDxbxKrnlUtils,
  uDxbxDebugUtils,
{$IFDEF LOG_STRUCT_SIZES}
  uError,
  uMutex,
  uXBController,
  uXBVideo,
  uVertexBuffer,
  uVertexShader,
  uEmuLDT,
  uEmuDSound,
  uEmuD3D8Types,
{$ENDIF LOG_STRUCT_SIZES}
  uEmuShared,
  uEmu,
  uEmuFS,
  uEmuFile,
  uEmuD3D8,
  uHLEIntercept;

type
  PHYSICAL_ADDRESS = ULONG;

  TEntryProc = procedure();
//  PEntryProc = ^TEntryProc;

procedure DxbxKrnlInit(
  hwndParent: HWND;
  pTLSData: PVOID;
  pTLS: PXBE_TLS;
  pLibraryVersion: PXBE_LIBRARYVERSION;
  DbgMode: TDebugMode;
  szDebugFileName: P_char;
  pXbeHeader: PXBEIMAGE_HEADER;
  dwXbeHeaderSize: DWord;
  Entry: TEntryProc); stdcall;

procedure _DxbxKrnlCleanup(const szErrorMessage: string); overload;
procedure _DxbxKrnlCleanup(const szErrorMessage: string; const Args: array of const); overload;

procedure DxbxKrnlRegisterThread(const hThread: Handle);
procedure DxbxKrnlTerminateThread(); // EmuCleanThread(); // export;
procedure DxbxKrnlResume();
procedure DxbxKrnlSuspend();

exports
  DxbxKrnlInit;

  (*Exports EmuCleanThread name '_EmuCleanThread@0';
  { TODO -oDXBX: name need to be set }
  (*Exports Init; // name must be "void EmuShared::Init (void)
  *)

var
  g_Xbe_XbePath: string; // The path of the running Xbe, as seen from Windows
  g_EmuXbePath: AnsiString; // The path of the running Xbe, as seen from Xbox1 (including \Device\Harddisk0\Partition1\)

  g_CPUXbox: DWORD_PTR;
  g_CPUOthers: DWORD_PTR;

implementation

procedure DxbxKrnlInit(
  hwndParent: HWND;
  pTLSData: PVOID;
  pTLS: PXBE_TLS;
  pLibraryVersion: PXBE_LIBRARYVERSION;
  DbgMode: TDebugMode;
  szDebugFileName: P_char;
  pXbeHeader: PXBEIMAGE_HEADER;
  dwXbeHeaderSize: DWord;
  Entry: TEntryProc);
// Branch:shogun  Revision:162  Translator:Patrick  Done:100
{$IFDEF LOG_STRUCT_SIZES}
var
  LogOnlyDifferences: Boolean;

  procedure _TypeSize(const aTypeName: string; const aActualSize, aExpectedSize: Integer);
  begin
    if (aActualSize <> aExpectedSize) or (LogOnlyDifferences = False) then
      Dbgprintf('sizeof(%s) = %d  (Cxbx has %d)', [aTypeName, aActualSize, aExpectedSize]);
  end;
{$ENDIF LOG_STRUCT_SIZES}

var
  szBuffer: string;
  pCertificate: PXBE_CERTIFICATE;
  TitleStr: AnsiString;
  hDupHandle: Handle;
  OldExceptionFilter: TFNTopLevelExceptionFilter;

begin
  // debug console allocation (if configured)
  CreateLogs(DbgMode, string(szDebugFileName)); // Initialize logging interface

{$IFDEF _DEBUG_TRACE}
  DbgPrintf('EmuInit : Dxbx Version ' + _DXBX_VERSION);
{$ENDIF}

  // update caches
  DxbxKrnl_TLS := pTLS;
  DxbxKrnl_TLSData := pTLSData;
  DxbxKrnl_XbeHeader := pXbeHeader;
  DxbxKrnl_hEmuParent := iif(IsWindow(hwndParent), hwndParent, 0);
  pCertificate := PXBE_CERTIFICATE(pXbeHeader.dwCertificateAddr);

  // For Unicode Conversions
  // SetLocaleInfo(LC_ALL, 'English'); // Not neccesary, Delphi has this by default
{$IFDEF LOG_STRUCT_SIZES}
  LogOnlyDifferences := False;
  _TypeSize('X_CMcpxStream_pParentStream_Offset', FIELD_OFFSET(X_CMcpxStream(nil).pParentStream), 1028);
  _TypeSize('X_CDirectSoundStream_pMcpxStream_Offset', FIELD_OFFSET(X_CDirectSoundStream(nil).pMcpxStream), 36);
  _TypeSize('X_CDirectSoundStream_EmuPlayFlags_Offset', FIELD_OFFSET(X_CDirectSoundStream(nil).EmuPlayFlags), 1092);

  _TypeSize('EmuShared', sizeof(EmuShared), 7164);
  //_TypeSize('EmuThis', sizeof(EmuThis), 1);
  _TypeSize('Error', sizeof(Error), 8);
  _TypeSize('Mutex', sizeof(Mutex), 16);
  _TypeSize('PATCHEDSTREAM', sizeof(PATCHEDSTREAM), 20);
  _TypeSize('VertexPatcher', sizeof(VertexPatcher), 336);
  //_TypeSize('Wnd', sizeof(Wnd), ?);
  _TypeSize('XBCtrlObjectCfg', sizeof(XBCtrlObjectCfg), 12);
  _TypeSize('XBController', sizeof(XBController), 6760);
  //_TypeSize('Xbe', Xbe.InstanceSize, 1180);
  _TypeSize('XBVideo', sizeof(XBVideo), 128);
  _TypeSize('XINPUT_GAMEPAD', sizeof(_XINPUT_GAMEPAD), 18);
  _TypeSize('XINPUT_STATE', sizeof(_XINPUT_STATE), 24);
  //_TypeSize('X_CDirectSoundStream', X_CDirectSoundStream.InstanceSize, 0);
  //_TypeSize('X_CMcpxStream', X_CMcpxStream.InstanceSize, 0);
  _TypeSize('X_D3DBaseTexture', sizeof(X_D3DBaseTexture), 20);
  _TypeSize('X_D3DCubeTexture', sizeof(X_D3DCubeTexture), 20);
  _TypeSize('X_D3DFixup', sizeof(X_D3DFixup), 24);
  _TypeSize('X_D3DPixelContainer', sizeof(X_D3DPixelContainer), 20);
  _TypeSize('X_D3DPushBuffer', sizeof(X_D3DPushBuffer), 20);
  _TypeSize('X_D3DResource', sizeof(X_D3DResource), 12);
  _TypeSize('X_D3DSurface', sizeof(X_D3DSurface), 20);
  _TypeSize('X_D3DTILE', sizeof(X_D3DTILE), 24);
  _TypeSize('X_D3DVertexShader', sizeof(X_D3DVertexShader), 368);
  _TypeSize('_D3DSWAPDATA', sizeof(_D3DSWAPDATA), 20);
  _TypeSize('_D3DVBLANKDATA', sizeof(_D3DVBLANKDATA), 12);
  _TypeSize('_PATCHEDSTREAM', sizeof(_PATCHEDSTREAM), 20);
  _TypeSize('_CACHEDSTREAM', sizeof(_CACHEDSTREAM), 56);
  _TypeSize('_D3DIVB', sizeof(_D3DIVB), 72);
  _TypeSize('_STREAM_DYNAMIC_PATCH_', sizeof(_STREAM_DYNAMIC_PATCH_), 16);
  _TypeSize('_VERTEX_DYNAMIC_PATCH_', sizeof(_VERTEX_DYNAMIC_PATCH_), 8);
  _TypeSize('_VERTEX_SHADER', sizeof(_VERTEX_SHADER), 40);
  _TypeSize('_X_D3DDISPLAYMODE', sizeof(_X_D3DDISPLAYMODE), 20);
  _TypeSize('_X_D3DFIELD_STATUS', sizeof(_X_D3DFIELD_STATUS), 8);
  _TypeSize('_X_D3DGAMMARAMP', sizeof(_X_D3DGAMMARAMP), 768);
  _TypeSize('_X_D3DPIXELSHADERDEF', sizeof(_X_D3DPIXELSHADERDEF), 240);
  _TypeSize('_X_D3DPRESENT_PARAMETERS', sizeof(_X_D3DPRESENT_PARAMETERS), 68);
  _TypeSize('_X_D3DSURFACE_DESC', sizeof(_X_D3DSURFACE_DESC), 28);
  _TypeSize('_X_STREAMINPUT', sizeof(_X_STREAMINPUT), 12);
  _TypeSize('_X_VERTEXATTRIBUTEFORMAT', sizeof(_X_VERTEXATTRIBUTEFORMAT), 256);
  _TypeSize('_X_VERTEXSHADERINPUT', sizeof(_X_VERTEXSHADERINPUT), 16);
  _TypeSize('X_D3DPalette', sizeof(X_D3DPalette), 12);
  _TypeSize('X_D3DVertexBuffer', sizeof(X_D3DVertexBuffer), 12);
  _TypeSize('X_D3DIndexBuffer', sizeof(X_D3DIndexBuffer), 12);

  _TypeSize('_VSH_OPCODE_PARAMS', sizeof(_VSH_OPCODE_PARAMS), 12);
  _TypeSize('_VSH_PARAMETER', sizeof(_VSH_PARAMETER), 28);
  _TypeSize('_VSH_OUTPUT', sizeof(_VSH_OUTPUT), 24);
  _TypeSize('_VSH_SHADER_INSTRUCTION', sizeof(_VSH_SHADER_INSTRUCTION), 120);
  _TypeSize('_VSH_IMD_OUTPUT', sizeof(_VSH_IMD_OUTPUT), 12);
  _TypeSize('_VSH_IMD_PARAMETER', sizeof(_VSH_IMD_PARAMETER), 36);
  _TypeSize('_VSH_INTERMEDIATE_FORMAT', sizeof(_VSH_INTERMEDIATE_FORMAT), 136);
  _TypeSize('_VSH_FIELDMAPPING', sizeof(_VSH_FIELDMAPPING), 8);
  _TypeSize('_VSH_SHADER_HEADER', sizeof(_VSH_SHADER_HEADER), 4);
  _TypeSize('_VSH_XBOX_SHADER', sizeof(_VSH_XBOX_SHADER), 139272);
  //_TypeSize('_VSH_TYPE_PATCH_DATA', sizeof(VSH_TYPE_PATCH_DATA), 1028);
  //_TypeSize('_VSH_STREAM_PATCH_DATA', sizeof(VSH_STREAM_PATCH_DATA), 4100);
  //_TypeSize('_VSH_PATCH_DATA', sizeof(VSH_PATCH_DATA), 5136);

{$ENDIF LOG_STRUCT_SIZES}

{$IFDEF _DEBUG_TRACE}
  DbgPrintf('EmuMain : Debug Trace Enabled.');

  DbgPrintf('EmuMain : 0x%.8x : DxbxKrnlInit' +
    #13#10'(' +
    #13#10'   hwndParent          : 0x%.8x' +
    #13#10'   pTLSData            : 0x%.8x' +
    #13#10'   pTLS                : 0x%.8x' +
    #13#10'   pLibraryVersion     : 0x%.8x' +
    #13#10'   DebugConsole        : 0x%.8x' +
    #13#10'   DebugFileName       : 0x%.8x (%s)' +
    #13#10'   pXBEHeader          : 0x%.8x' +
    #13#10'   dwXBEHeaderSize     : 0x%.8x' +
    #13#10'   Entry               : 0x%.8x' +
    #13#10')', [
      @DxbxKrnlInit,
      hwndParent,
      pTLSData,
      pTLS,
      pLibraryVersion,
      Ord(DbgMode),
      Pointer(szDebugFileName), AnsiString(szDebugFileName),
      pXbeHeader,
      dwXbeHeaderSize,
      Addr(Entry)
    ]);
{$ELSE}
 {$IFDEF DEBUG}
  DbgPrintf('EmuMain : Debug Trace Disabled.');
 {$ENDIF}
{$ENDIF}

  // Determine Xbe Path :
  begin
    g_EmuShared.GetXbePath({var}szBuffer);
    if (szBuffer = '') and (g_Xbe_XbePath <> '') then
      szBuffer := ExtractFilePath(g_Xbe_XbePath);

    if szBuffer <> '' then
    begin
      DbgPrintf('EmuMain : XBEPath := ' + szBuffer);
      SetCurrentDirectory(PChar(szBuffer));
    end
    else
    begin
      // When no path is registered in EmuShared, fall back on current directory :
      SetLength(szBuffer, MAX_PATH);
      SetLength(szBuffer, GetCurrentDirectory(MAX_PATH, @(szBuffer[1])));
      // Make sure the CurrentDir ends with a trailing backslash :
      if szBuffer[Length(szBuffer)] <> '\' then
        szBuffer := szBuffer + '\';
    end;
  end;

  // Initialize devices :
  begin
    DxbxBasePath := GetDxbxBasePath + '\EmuDisk\';
    DxbxBasePathHandle := CreateFile(PChar(DxbxBasePath),
          GENERIC_READ,
          FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
          NULL,
          OPEN_EXISTING,
          FILE_FLAG_BACKUP_SEMANTICS,
          HNULL);

    TitleStr := AnsiString(IntToHex(pCertificate.dwTitleId, 8));

    // Games may assume they are running from CdRom :
    DxbxRegisterDeviceNativePath(DeviceCdrom0, szBuffer);

    // Partition 0 contains configuration data, and is accessed as a native file, instead as a folder :
    DxbxRegisterDeviceNativePath(DeviceHarddisk0Partition0, DxbxBasePath + 'Partition0_ConfigData.bin');
    // The first two partitions are for Data and Shell files, respectively :
    DxbxRegisterDeviceNativePath(DeviceHarddisk0Partition1, DxbxBasePath + 'Partition1\');
    DxbxRegisterDeviceNativePath(DeviceHarddisk0Partition2, DxbxBasePath + 'Partition2\');
    // The following partitions are for caching purposes - for now we allocate up to 7 (as xbmp needs that many) :
    DxbxRegisterDeviceNativePath(DeviceHarddisk0Partition3, DxbxBasePath + 'Partition3\');
    DxbxRegisterDeviceNativePath(DeviceHarddisk0Partition4, DxbxBasePath + 'Partition4\');
    DxbxRegisterDeviceNativePath(DeviceHarddisk0Partition5, DxbxBasePath + 'Partition5\');
    DxbxRegisterDeviceNativePath(DeviceHarddisk0Partition6, DxbxBasePath + 'Partition6\');
    DxbxRegisterDeviceNativePath(DeviceHarddisk0Partition7, DxbxBasePath + 'Partition7\');
  end;

  // Create default symbolic links :
  begin
    DxbxCreateSymbolicLink(DriveD, DeviceCdrom0); // CdRom goes to D:
    DxbxCreateSymbolicLink(DriveT, DeviceHarddisk0Partition1 + 'TDATA\' + TitleStr + '\'); // Title data to T:
    DxbxCreateSymbolicLink(DriveU, DeviceHarddisk0Partition1 + 'UDATA\' + TitleStr + '\'); // User data to U:
    DxbxCreateSymbolicLink(DriveY, DeviceHarddisk0Partition2); // Officially unused, but contains Dashboard files
    DxbxCreateSymbolicLink(DriveZ, DeviceHarddisk0Partition6 + 'ZDATA\' + TitleStr + '\'); // Utility data to Z:

    // Arrange that the Xbe path can reside outside the partitions, and put it to g_hCurDir :
    DxbxCreateSymbolicLink(DriveC, AnsiString(szBuffer));
    g_hCurDir := FindNtSymbolicLinkObjectByVolumeLetter('C').RootDirectoryHandle;
    // TODO -oDxbx: Make sure this path is set in g_EmuXbePath (xboxkrnl_XeImageFileName) too.
  end;

  // initialize FS segment selector
  begin
    EmuInitFS();

    EmuGenerateFS(DxbxKrnl_TLS, DxbxKrnl_TLSData);
  end;

  // duplicate handle in order to retain Suspend/Resume thread rights from a remote thread
  begin
    hDupHandle := 0;

    if not DuplicateHandle(GetCurrentProcess(), GetCurrentThread(), GetCurrentProcess(), @hDupHandle, 0, False, DUPLICATE_SAME_ACCESS) then
      DbgPrintf('EmuMain : Couldn''t duplicate handle!');

    DxbxKrnlRegisterThread(hDupHandle);
  end;

  
{$IFDEF DEBUG}
  DbgPrintf('EmuMain : Initializing Direct3D.');
{$ENDIF}

  XTL_EmuD3DInit(pXbeHeader, dwXbeHeaderSize);

  EmuHLEIntercept(pLibraryVersion, pXbeHeader);

{$IFDEF DEBUG}
  DbgPrintf('EmuMain : Initial thread starting.');
{$ENDIF}

  // Re-route unhandled exceptions to our emulation-execption handler :
  OldExceptionFilter := SetUnhandledExceptionFilter(TFNTopLevelExceptionFilter(@EmuException));

  // Make sure the Xbox1 code runs on one core (as the box itself has only 1 CPU,
  // this will better aproximate the environment with regard to multi-threading) :
  begin
    GetProcessAffinityMask(GetCurrentProcess(), {var lpProcessAffinityMask=}g_CPUXbox, {var lpSystemAffinityMask=}g_CPUOthers{ignored});
    // For the other threads, remove one bit from the processor mask :
    g_CPUOthers := ((g_CPUXbox - 1) and g_CPUXbox);
    // Test if there are any other cores available :
    if g_CPUOthers > 0 then
      // If so, make sure the Xbox threads run on the core NOT running Xbox code :
      g_CPUXbox := g_CPUXbox and (not g_CPUOthers)
    else
      // Else the other threads must run on the same core as the Xbox code :
      g_CPUOthers := g_CPUXbox;

    // Make sure Xbox1 code runs on one core :
    SetThreadAffinityMask(GetCurrentThreadID(), g_CPUXbox);
  end;

  // Xbe entry point
  try
    EmuSwapFS(fsXbox);

    // _USE_XGMATH Disabled in mesh :[
    // halo : dword_0_2E2D18
    // halo : 1744F0 (bink)
    // asm int 3 end;

    (* Marked out by cxbx
    for v := 0 to (SizeOf(FuncAddr / SizeOf(UInt32)) - 1 do
    begin
        _bool bExclude = False;
        for r := 0 to (SizeOf(funcExclude / SizeOf(UInt32)) - 1 do
        begin
            if funcAddr[v] = funcExclude[r] then
            begin
                bExclude := True;
                break;
            end;
        end;

        if not bExclude then
            *(uint08* )(funcAddr[v]) := 0xCC;
    end
    *)

    Entry();

    EmuSwapFS(fsWindows);
  except
    on E: Exception do
    begin
      EmuSwapFS(fsWindows);
{$IFDEF DEBUG}
      DbgPrintf('EmuMain : Catched an exception : ' + E.Message);
{$IFDEF DXBX_USE_JCLDEBUG}
      DbgPrintf(JclLastExceptStackListToString);
{$ENDIF}
//    on(EmuException(GetExceptionInformation())) :
//      printf('Emu: WARNING!! Problem with ExceptionFilter');
{$ENDIF}
    end;
  end;

  // Restore original exception filter :
  SetUnhandledExceptionFilter(OldExceptionFilter);

{$IFDEF DEBUG}
  DbgPrintf('EmuMain : Initial thread ended.');
{$ENDIF}

//  fflush(stdout);

  DxbxKrnlTerminateThread();
end;

procedure _DxbxKrnlCleanup(const szErrorMessage: string; const Args: array of const);
begin
  _DxbxKrnlCleanup(DxbxFormat(szErrorMessage, Args, {MayRenderArguments=}True));
end;

procedure _DxbxKrnlCleanup(const szErrorMessage: string);
var
  szBuffer1: string;
//  buffer: array [0..15] of char;
begin
  g_bEmuException := true;

  DxbxKrnlResume();
    
  // Print out ErrorMessage (if exists)
  if szErrorMessage <> '' then
  begin
    szBuffer1 := {Format} 'DxbxKrnlCleanup : Received Fatal Message ->'#13#10#13#10 + szErrorMessage;
{$IFDEF DEBUG}
    DbgPrintf(szBuffer1);
{$ENDIF}
    MessageBox(0, @(szBuffer1[1]), 'DxbxKrnl', MB_OK or MB_ICONSTOP);
  end;

{$IFDEF DEBUG}
  DbgPrintf('DxbxKrnl: Terminating Process');
{$ENDIF}
  fflush(stdout);

  // Cleanup debug output
  begin
    CloseLogs(); // FreeConsole();

    (* if (GetConsoleTitle(buffer, 16) <> '') then
        freopen('nul', 'w', stdout); *)
  end;

  if(DxbxKrnl_hEmuParent <> HNULL) then
    SendMessage(DxbxKrnl_hEmuParent, WM_USER_PARENTNOTIFY, WM_DESTROY, 0);

  TerminateProcess(GetCurrentProcess(), 0);
end;

procedure DxbxKrnlRegisterThread(const hThread: HANDLE);
// Branch:shogun  Revision:162  Translator:Shadow_tj  Done:100
var
  v: int;
begin
  v := 0;
  while v < MAXIMUM_XBOX_THREADS do
  begin
    if g_hThreads[v] = 0 then
    begin
      g_hThreads[v] := hThread;
      Exit;
    end;

    Inc(v);
  end;

  _DxbxKrnlCleanup('There are too many active threads!');
end;

// alert for the situation where an Xref function body is hit

procedure DxbxKrnlSuspend();
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  v: int;
  dwExitCode: DWORD;
  szBuffer: array [0..256-1] of Char;
  _hWnd: HANDLE;
begin
  if (g_bEmuSuspended or g_bEmuException) then
    Exit;

  for v := 0 to MAXIMUM_XBOX_THREADS-1 do
  begin
    if (g_hThreads[v] <> 0) then
    begin
      if GetExitCodeThread(g_hThreads[v], {var}dwExitCode) and (dwExitCode = STILL_ACTIVE) then
      begin
        // suspend thread if it is active
        SuspendThread(g_hThreads[v]);
      end
      else
      begin
        // remove thread from thread list if it is dead
        g_hThreads[v] := 0;
      end;
    end;
  end;

  // append 'paused' to rendering window caption text
  begin
    if DxbxKrnl_hEmuParent <> 0 then
      _hWnd := DxbxKrnl_hEmuParent
    else
      _hWnd := g_hEmuWindow;

    GetWindowText(_hWnd, szBuffer, 255 - 10);

    strcat(szBuffer, ' (paused)');
    SetWindowText(_hWnd, szBuffer);
  end;

  g_bEmuSuspended := true;
end;

procedure DxbxKrnlResume();
// Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
var
  v: int;
  dwExitCode: DWORD;
  szBuffer: array [0..256-1] of Char;
  _hWnd: Handle;
begin
  if (not g_bEmuSuspended) then
    Exit;

  // remove 'paused' from rendering window caption text
  begin
    if DxbxKrnl_hEmuParent <> 0 then
      _hWnd := DxbxKrnl_hEmuParent
    else
      _hWnd := g_hEmuWindow;

    GetWindowText(_hWnd, szBuffer, 255);
    szBuffer[strlen(szBuffer)-9] := #0;
    SetWindowText(_hWnd, szBuffer);
  end;

  for v := 0 to MAXIMUM_XBOX_THREADS-1 do
  begin
    if (g_hThreads[v] <> 0) then
    begin
      if GetExitCodeThread(g_hThreads[v], {var}dwExitCode) and (dwExitCode = STILL_ACTIVE) then
      begin
        // resume thread if it is active
        ResumeThread(g_hThreads[v]);
      end
      else
      begin
        // remove thread from thread list if it is dead
        g_hThreads[v] := 0;
      end;
    end;
  end;

  g_bEmuSuspended := false;
end;


procedure DxbxKrnlTerminateThread();
// Branch:shogun  Revision:162  Translator:Shadow_tj  Done:100
begin
  EmuSwapFS(fsWindows);

  EmuCleanupFS;

  TerminateThread(GetCurrentThread(), 0);
end;

end.

