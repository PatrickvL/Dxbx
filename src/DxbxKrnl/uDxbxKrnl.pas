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
  Messages, // WM_PARENTNOTIFY
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
  uEmuD3D8Types,
{$ENDIF LOG_STRUCT_SIZES}
  uEmuShared,
  uEmu,
  uEmuFS,
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
  pXbeHeader: PXBE_HEADER;
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
  g_Xbe: TXbe;

implementation

procedure DxbxKrnlInit(
  hwndParent: HWND;
  pTLSData: PVOID;
  pTLS: PXBE_TLS;
  pLibraryVersion: PXBE_LIBRARYVERSION;
  DbgMode: TDebugMode;
  szDebugFileName: P_char;
  pXbeHeader: PXBE_HEADER;
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
//  MemXbeHeader: PXBE_HEADER;
//  old_protection: DWord;
  szBuffer: string;
  pCertificate: PXBE_CERTIFICATE;
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

  // For Unicode Conversions
  // SetLocaleInfo(LC_ALL, 'English'); // Not neccesary, Delphi has this by default
{$IFDEF LOG_STRUCT_SIZES}
  LogOnlyDifferences := False;
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
    #13#10'   DebugFileName       : 0x%.8x' +
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
      Pointer(szDebugFileName), // Print as pointer, not as string! (will be added automatically)
      pXbeHeader,
      dwXbeHeaderSize,
      Addr(Entry)
    ]);
{$ELSE}
 {$IFDEF DEBUG}
  DbgPrintf('EmuMain : Debug Trace Disabled.');
 {$ENDIF}
{$ENDIF}

(* Dxbx note : We don't need to do this anymore, since we load XBE's at $10000 already:
  // Load the necessary pieces of XBEHeader
  begin
    MemXbeHeader := PXBE_HEADER($00010000);

    VirtualProtect(MemXbeHeader, $1000, PAGE_READWRITE, {var} old_protection);

    // we sure hope we aren't corrupting anything necessary for an .exe to survive :]
    MemXbeHeader.dwSizeofHeaders := pXbeHeader.dwSizeofHeaders;
    MemXbeHeader.dwCertificateAddr := pXbeHeader.dwCertificateAddr;
    MemXbeHeader.dwPeHeapReserve := pXbeHeader.dwPeHeapReserve;
    MemXbeHeader.dwPeHeapCommit := pXbeHeader.dwPeHeapCommit;

    CopyMemory(@MemXbeHeader.dwInitFlags, @pXbeHeader.dwInitFlags, SizeOf(pXbeHeader.dwInitFlags));
    CopyMemory(Pointer(pXbeHeader.dwCertificateAddr), MathPtr(pXbeHeader) + pXbeHeader.dwCertificateAddr - $00010000, SizeOf(XBE_CERTIFICATE));
  end;
*)
  // Initialize current directory
  g_EmuShared.GetXbePath({var}szBuffer);
  if (szBuffer = '') and Assigned(g_Xbe) then
    szBuffer := ExtractFilePath(g_Xbe.XbePath);

  if szBuffer <> '' then
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuMain : XBEPath := ' + szBuffer);
{$ENDIF}
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

  g_strCurDrive := szBuffer;

  g_hCurDir := CreateFile(PChar(szBuffer), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  if g_hCurDir = INVALID_HANDLE_VALUE then
    _DxbxKrnlCleanup('Could not map D:\');

{$IFDEF DEBUG}
  DbgPrintf('EmuMain : CurDir := ' + szBuffer);
{$ENDIF}

  // initialize EmuDisk
  begin
    DxbxBasePath := GetDxbxBasePath;
    CreateDirectory(PChar(DxbxBasePath), nil);

    // create EmuDisk directory
    szBuffer := DxbxBasePath + '\EmuDisk';
    CreateDirectory(PChar(szBuffer), nil);

    // create T:\ directory
    begin
      szBuffer := DxbxBasePath + '\EmuDisk\T';
      CreateDirectory(PChar(szBuffer), nil);

      pCertificate := PXBE_CERTIFICATE(pXbeHeader.dwCertificateAddr);
      szBuffer := szBuffer + '\' + IntToHex(pCertificate.dwTitleId, 8);
      CreateDirectory(PChar(szBuffer), nil);

      g_strTDrive := szBuffer;
      g_hTDrive := CreateFile(PChar(szBuffer), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);

      if g_hTDrive = INVALID_HANDLE_VALUE then
        _DxbxKrnlCleanup('Could not map T:\');

{$IFDEF DEBUG}
      DbgPrintf('EmuMain : T Data := ' + g_strTDrive);
{$ENDIF}
    end;

    // create U:\ directory
    begin
      szBuffer := DxbxBasePath + '\EmuDisk\U';
      CreateDirectory(PChar(szBuffer), nil);

      szBuffer := szBuffer + '\' + IntToHex(pCertificate.dwTitleId, 8);
      CreateDirectory(PChar(szBuffer), nil);

      g_strUDrive := szBuffer;
      g_hUDrive := CreateFile(PChar(szBuffer), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);

      if g_hUDrive = INVALID_HANDLE_VALUE then
        _DxbxKrnlCleanup('Could not map U:\');

{$IFDEF DEBUG}
      DbgPrintf('EmuMain : U Data := ' + g_strUDrive);
{$ENDIF}
    end;

    // create Z:\ directory
    begin
      szBuffer := DxbxBasePath + '\EmuDisk\Z';
      CreateDirectory(PChar(szBuffer), nil);

      (* marked out by cxbx
      //is it necessary to make this directory title unique?
      szBuffer := szBuffer + '\' + IntToHex(pCertificate.dwTitleId, 8);
      CreateDirectory(PChar(szBuffer), nil);
      *)

      g_strZDrive := szBuffer;
      g_hZDrive := CreateFile(PChar(szBuffer), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);

      if g_hUDrive = INVALID_HANDLE_VALUE then
        _DxbxKrnlCleanup('Could not map Z:\');

{$IFDEF DEBUG}
      DbgPrintf('EmuMain : Z Data := ' + g_strZDrive);
{$ENDIF}
    end;

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

  // Xbe entry point
  try
    EmuSwapFS(fsXbox);

    // _USE_XGMATH Disabled in mesh :[
    // halo : dword_0_2E2D18
    // halo : 1744F0 (bink)
    //_asm int 3;

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

  fflush(stdout);

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
    szBuffer1 := {Format} 'DxbxKrnlCleanup : Received Fatal Message ->'#13#13 + szErrorMessage;
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
    SendMessage(DxbxKrnl_hEmuParent, WM_PARENTNOTIFY, WM_DESTROY, 0);

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

