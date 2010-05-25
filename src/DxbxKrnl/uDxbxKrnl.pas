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

interface

uses
  // Delphi
  Windows, // DWord
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
  uVertexBuffer,
  uXBController,
  uXBVideo,
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

procedure CxbxKrnlInit(
  hwndParent: HWND;
  pTLSData: PVOID;
  pTLS: PXBE_TLS;
  pLibraryVersion: PXBE_LIBRARYVERSION;
  DbgMode: TDebugMode;
  szDebugFileName: PAnsiChar;
  pXbeHeader: PXBE_HEADER;
  dwXbeHeaderSize: DWord;
  Entry: TEntryProc); stdcall;

procedure CxbxKrnlRegisterThread(const hThread: Handle);
procedure CxbxKrnlTerminateThread(); // EmuCleanThread(); // export;
procedure EmuXRefFailure;
procedure CxbxKrnlResume();
procedure EmuPanic(); stdcall; //
procedure CxbxKrnlNoFunc; cdecl;
procedure CxbxKrnlSuspend();

exports
  CxbxKrnlInit,
  CxbxKrnlNoFunc,
  EmuPanic name '_EmuPanic@0';

  (*Exports EmuCleanThread name '_EmuCleanThread@0';
  { TODO -oDXBX: name need to be set }
  (*Exports Init; // name must be "void EmuShared::Init (void)
  *)

var
  Xbe: TXbe;

implementation

procedure CxbxKrnlInit(
  hwndParent: HWND;
  pTLSData: PVOID;
  pTLS: PXBE_TLS;
  pLibraryVersion: PXBE_LIBRARYVERSION;
  DbgMode: TDebugMode;
  szDebugFileName: PAnsiChar;
  pXbeHeader: PXBE_HEADER;
  dwXbeHeaderSize: DWord;
  Entry: TEntryProc);
// Branch:martin  Revision:39  Translator:Patrick  Done:100
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
  Dbgprintf('sizeof(EmuShared) = %d', [sizeof(EmuShared)]);
  //Dbgprintf('sizeof(EmuThis) = %d', [sizeof(EmuThis)]);
  Dbgprintf('sizeof(Error) = %d', [sizeof(Error)]);
  Dbgprintf('sizeof(Mutex) = %d', [sizeof(Mutex)]);
  Dbgprintf('sizeof(PATCHEDSTREAM) = %d', [sizeof(PATCHEDSTREAM)]);
  Dbgprintf('sizeof(VertexPatcher) = %d', [sizeof(VertexPatcher)]);
  //Dbgprintf('sizeof(Wnd) = %d', [sizeof(Wnd)]);
  Dbgprintf('sizeof(XBController) = %d', [sizeof(XBController)]);
  Dbgprintf('sizeof(Xbe) = %d', [Xbe.InstanceSize]);
  Dbgprintf('sizeof(XBVideo) = %d', [sizeof(XBVideo)]);
  //Dbgprintf('sizeof(X_CDirectSoundStream) = %d', [sizeof(X_CDirectSoundStream)]);
  //Dbgprintf('sizeof(X_CMcpxStream) = %d', [sizeof(X_CMcpxStream)]);
  Dbgprintf('sizeof(X_D3DFixup) = %d', [sizeof(X_D3DFixup)]);
  Dbgprintf('sizeof(X_D3DPixelContainer) = %d', [sizeof(X_D3DPixelContainer)]);
  Dbgprintf('sizeof(X_D3DPushBuffer) = %d', [sizeof(X_D3DPushBuffer)]);
  Dbgprintf('sizeof(X_D3DResource) = %d', [sizeof(X_D3DResource)]);
  Dbgprintf('sizeof(X_D3DTILE) = %d', [sizeof(X_D3DTILE)]);
  Dbgprintf('sizeof(X_D3DVertexShader) = %d', [sizeof(X_D3DVertexShader)]);
  Dbgprintf('sizeof(_D3DSWAPDATA) = %d', [sizeof(_D3DSWAPDATA)]);
  Dbgprintf('sizeof(_D3DVBLANKDATA) = %d', [sizeof(_D3DVBLANKDATA)]);
  Dbgprintf('sizeof(_STREAM_DYNAMIC_PATCH_) = %d', [sizeof(_STREAM_DYNAMIC_PATCH_)]);
  Dbgprintf('sizeof(_VERTEX_DYNAMIC_PATCH_) = %d', [sizeof(_VERTEX_DYNAMIC_PATCH_)]);
  Dbgprintf('sizeof(_VERTEX_SHADER) = %d', [sizeof(_VERTEX_SHADER)]);
  Dbgprintf('sizeof(_X_D3DDISPLAYMODE) = %d', [sizeof(_X_D3DDISPLAYMODE)]);
  Dbgprintf('sizeof(_X_D3DFIELD_STATUS) = %d', [sizeof(_X_D3DFIELD_STATUS)]);
  Dbgprintf('sizeof(_X_D3DGAMMARAMP) = %d', [sizeof(_X_D3DGAMMARAMP)]);
  Dbgprintf('sizeof(_X_D3DPIXELSHADERDEF) = %d', [sizeof(_X_D3DPIXELSHADERDEF)]);
  Dbgprintf('sizeof(_X_D3DPRESENT_PARAMETERS) = %d', [sizeof(_X_D3DPRESENT_PARAMETERS)]);
  Dbgprintf('sizeof(_X_D3DSURFACE_DESC) = %d', [sizeof(_X_D3DSURFACE_DESC)]);
  Dbgprintf('sizeof(_X_STREAMINPUT) = %d', [sizeof(_X_STREAMINPUT)]);
  Dbgprintf('sizeof(_X_VERTEXATTRIBUTEFORMAT) = %d', [sizeof(_X_VERTEXATTRIBUTEFORMAT)]);
  Dbgprintf('sizeof(_X_VERTEXSHADERINPUT) = %d', [sizeof(_X_VERTEXSHADERINPUT)]);
{$ENDIF LOG_STRUCT_SIZES}

{$IFDEF _DEBUG_TRACE}
  DbgPrintf('EmuMain : Debug Trace Enabled.');

  DbgPrintf('EmuMain : 0x%.8x : CxbxKrnlInit' +
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
      @CxbxKrnlInit,
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
  if (szBuffer = '') and Assigned(Xbe) then
    szBuffer := ExtractFilePath(Xbe.XbePath);

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
    CxbxKrnlCleanup('Could not map D:\');

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
        CxbxKrnlCleanup('Could not map T:\');

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
        CxbxKrnlCleanup('Could not map U:\');

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
        CxbxKrnlCleanup('Could not map Z:\');

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

    CxbxKrnlRegisterThread(hDupHandle);
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

    { Marked out by cxbx
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
    }

    Entry();

    EmuSwapFS(fsWindows);
  except
    on E: Exception do
    begin
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

  CxbxKrnlTerminateThread();
end;

procedure CxbxKrnlRegisterThread(const hThread: Handle);
// Branch:martin  Revision:39  Translator:Shadow_tj  Done:100
var
  v: Integer;
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

  CxbxKrnlCleanup('There are too many active threads!');
end;

procedure CxbxKrnlTerminateThread();
// Branch:martin  Revision:39  Translator:Shadow_tj  Done:100
begin
  EmuSwapFS(fsWindows);

  EmuCleanupFS;

  TerminateThread(GetCurrentThread(), 0);
end;

// alert for the situation where an Xref function body is hit

procedure EmuXRefFailure();
// Branch:martin  Revision:39  Translator:Shadow_tj  Done:100
begin
  EmuSwapFS(fsWindows);

  CxbxKrnlCleanup('XRef-only function body reached. Fatal Error.');
end;

procedure CxbxKrnlResume();
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  v: Integer;
  dwExitCode: DWORD;
  szBuffer: array [0..256-1] of Char;
  MyhWnd: Handle;
begin
  if (not g_bEmuSuspended) then
    Exit;

  // remove 'paused' from rendering window caption text
  begin
    if DxbxKrnl_hEmuParent <> 0 then
      MyhWnd := DxbxKrnl_hEmuParent
    else
      MyhWnd := g_hEmuWindow;

    GetWindowText(MyhWnd, szBuffer, 255);
    szBuffer[strlen(szBuffer)-9] := #0;
    SetWindowText(MyhWnd, szBuffer);
  end;

  for v := 0 to MAXIMUM_XBOX_THREADS - 1 do
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

  g_bEmuSuspended := False;
end;

procedure CxbxKrnlSuspend();
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  v: Integer;
  dwExitCode: DWORD;
  szBuffer: array [0..256-1] of Char;
  MyhWnd: Handle;
begin
  if (g_bEmuSuspended or g_bEmuException) then
    Exit;

  for v := 0 to MAXIMUM_XBOX_THREADS - 1 do
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
      MyhWnd := DxbxKrnl_hEmuParent
    else
      MyhWnd := g_hEmuWindow;

    GetWindowText(MyhWnd, szBuffer, 255 - 10);

    strcat(szBuffer, ' (paused)');
    SetWindowText(MyhWnd, szBuffer);
  end;

  g_bEmuSuspended := True;
end;


procedure EmuPanic(); stdcall;
// Branch:martin  Revision:39  Translator:Shadow_tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuMain : EmuPanic');
{$ENDIF}

  CxbxKrnlCleanup('Kernel Panic!');

  EmuSwapFS(fsXbox);
end;

procedure CxbxKrnlNoFunc;
// Branch:martin  Revision:39  Translator:Shadow_tj  Done:100
begin
{$IFDEF DEBUG}
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuMain : CxbxKrnlNoFunc();');
  EmuSwapFS(fsXbox);
{$ENDIF}
end;

end.

