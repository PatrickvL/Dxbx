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

{$INCLUDE ..\Dxbx.inc}

interface

uses
  // Delphi
  Windows, // DWord
  Math, // IfThen
  SysUtils, // Format
  ShlObj, // SHGetSpecialFolderPath
  // Dxbx
  uConsts,
  uTypes,
  uLog,
  uXbe,
  uDxbxKrnlUtils,
  uEmuShared,
  uEmu,
  uEmuFS,
  uEmuD3D8,
  uHLEIntercept;

type
  TEntryProc = procedure();
//  PEntryProc = ^TEntryProc;

function CxbxKrnlVerifyVersion(const szVersion: string): Boolean; // export;

procedure CxbxKrnlInit(
  hwndParent: THandle;
  pTLSData: Pointer;
  pTLS: PXBE_TLS;
  pLibraryVersion: PXBE_LIBRARYVERSION;
  DbgMode: DebugMode;
  szDebugFilename: PChar;
  pXbeHeader: PXBE_HEADER;
  dwXbeHeaderSize: DWord;
  Entry: TEntryProc); cdecl;

procedure CxbxKrnlRegisterThread(const hThread: THandle);
procedure CxbxKrnlTerminateThread(); // EmuCleanThread(); // export;
procedure EmuXRefFailure;
procedure EmuPanic(); // export;
procedure CxbxKrnlNoFunc; cdecl;

var
  // ! thread local storage
  CxbxKrnl_TLS: PXBE_TLS;
  // thread local storage data
  CxbxKrnl_TLSData: Pointer;
  // xbe header structure
  CxbxKrnl_XbeHeader: PXBE_HEADER;
  // parent window handle
  CxbxKrnl_hEmuParent: THandle;

  // thread handles
  g_hThreads: array [0..MAXIMUM_XBOX_THREADS - 1] of THandle;

implementation

function CxbxKrnlVerifyVersion(const szVersion: string): Boolean;
begin
  Result := (szVersion = _DXBX_VERSION);
end;

procedure CxbxKrnlInit(
  hwndParent: THandle;
  pTLSData: Pointer;
  pTLS: PXBE_TLS;
  pLibraryVersion: PXBE_LIBRARYVERSION;
  DbgMode: DebugMode;
  szDebugFilename: PChar;
  pXbeHeader: PXBE_HEADER;
  dwXbeHeaderSize: DWord;
  Entry: TEntryProc);
var
  MemXbeHeader: PXBE_HEADER;
  old_protection: DWord;
  szBuffer, BasePath: string;
  pCertificate: PXBE_CERTIFICATE;
  hDupHandle: THandle;
//  v, r: Integer;
begin
  // debug console allocation (if configured)
  SetLogMode(DbgMode);
  CreateLogs(ltKernel); // Initialize logging interface

  DbgPrintf('EmuInit : Dxbx Version ' + _DXBX_VERSION);

  // update caches
  CxbxKrnl_TLS := pTLS;
  CxbxKrnl_TLSData := pTLSData;
  CxbxKrnl_XbeHeader := pXbeHeader;
  CxbxKrnl_hEmuParent := IfThen(IsWindow(hwndParent), hwndParent, 0);

  // For Unicode Conversions
// TODO SetLocale(LC_ALL, 'English');

  // debug trace
  begin
{$IFDEF _DEBUG_TRACE}
    DbgPrintf('EmuMain : Debug Trace Enabled.');
    
    DbgPrintf('EmuMain : 0x' + IntToHex(Integer(@CxbxKrnlInit), 8) + ' : CxbxKrnlInit(');
    // TODO : For some reason, using Format() fails here?
    DbgPrintf('  hwndParent       : 0x' + IntToHex(hwndParent, 8));
    DbgPrintf('  pTLSData         : 0x' + IntToHex(Integer(pTLSData), 8));
    DbgPrintf('  pTLS             : 0x' + IntToHex(Integer(pTLS), 8));
    DbgPrintf('  pLibraryVersion  : 0x' + IntToHex(Integer(pLibraryVersion), 8) + ' ("' + PChar(pLibraryVersion) + '")');
    DbgPrintf('  DebugConsole     : 0x' + IntToHex(Ord(DbgMode), 8));
    DbgPrintf('  DebugFilename    : 0x' + IntToHex(Integer(szDebugFilename), 8) + ' ("' + szDebugFilename + '")');
    DbgPrintf('  pXBEHeader       : 0x' + IntToHex(Integer(pXbeHeader), 8));
    DbgPrintf('  dwXBEHeaderSize  : 0x' + IntToHex(dwXbeHeaderSize, 8));
    DbgPrintf('  Entry            : 0x' + IntToHex(Integer(Addr(Entry)), 8));
    DbgPrintf(')');

{$ELSE}
    DbgPrintf('EmuMain : Debug Trace Disabled.');
{$ENDIF}
  end;

  // Load the necessary pieces of XBEHeader
  begin
    MemXbeHeader := PXBE_HEADER($00010000);

    VirtualProtect(MemXbeHeader, $1000, PAGE_READWRITE, {var}old_protection);

    // we sure hope we aren't corrupting anything necessary for an .exe to survive :]
    MemXbeHeader.dwSizeofHeaders   := pXbeHeader.dwSizeofHeaders;
    MemXbeHeader.dwCertificateAddr := pXbeHeader.dwCertificateAddr;
    MemXbeHeader.dwPeHeapReserve   := pXbeHeader.dwPeHeapReserve;
    MemXbeHeader.dwPeHeapCommit    := pXbeHeader.dwPeHeapCommit;

    CopyMemory(@MemXbeHeader.dwInitFlags, @pXbeHeader.dwInitFlags, SizeOf(pXbeHeader.dwInitFlags));
    CopyMemory(Pointer(pXbeHeader.dwCertificateAddr), PChar(pXbeHeader) + pXbeHeader.dwCertificateAddr - $00010000, SizeOf(XBE_CERTIFICATE));
  end;

  // Initialize current directory
  g_EmuShared.GetXbePath({var}szBuffer);
  if szBuffer <> '' then
    SetCurrentDirectory(PChar(szBuffer))
  else
  begin
    // When no path is registered in EmuShared, fall back on current directory :
    SetLength(szBuffer, MAX_PATH);
    SetLength(szBuffer, GetCurrentDirectory(MAX_PATH, @(szBuffer[1])));
  end;

  g_strCurDrive := szBuffer;

  g_hCurDir := CreateFile(PChar(szBuffer), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  if g_hCurDir = INVALID_HANDLE_VALUE then
    CxbxKrnlCleanup('Could not map D:\');

  DbgPrintf('EmuMain : CurDir := ' + szBuffer);

  // initialize EmuDisk
  begin
    SetLength(szBuffer, MAX_PATH);
    SHGetSpecialFolderPath(0, @(szBuffer[1]), CSIDL_APPDATA, True);
    SetLength(szBuffer, StrLen(@(szBuffer[1])));

    BasePath := szBuffer + '\Dxbx';
    CreateDirectory(PAnsiChar(BasePath), nil);

    // create EmuDisk directory
    szBuffer := BasePath + '\EmuDisk';
    CreateDirectory(PAnsiChar(szBuffer), nil);

    // create T:\ directory
    begin
      szBuffer := BasePath + '\EmuDisk\T';
      CreateDirectory(PAnsiChar(szBuffer), nil);

      pCertificate := PXBE_CERTIFICATE(pXbeHeader.dwCertificateAddr);
      szBuffer := szBuffer + '\' + IntToHex(pCertificate.dwTitleId, 8);
      CreateDirectory(PAnsiChar(szBuffer), nil);

      g_strTDrive := szBuffer;
      g_hTDrive := CreateFile(PChar(szBuffer), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);

      if g_hTDrive = INVALID_HANDLE_VALUE then
        CxbxKrnlCleanup('Could not map T:\\\n');

      DbgPrintf('EmuMain : T Data := ' + g_strTDrive);
    end;

    // create U:\ directory
    begin
      szBuffer := BasePath + '\EmuDisk\U';
      CreateDirectory(PAnsiChar(szBuffer), nil);

      szBuffer := szBuffer + '\' + IntToHex(pCertificate.dwTitleId, 8);
      CreateDirectory(PAnsiChar(szBuffer), nil);

      g_strUDrive := szBuffer;
      g_hUDrive := CreateFile(PChar(szBuffer), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);

      if g_hUDrive = INVALID_HANDLE_VALUE then
        CxbxKrnlCleanup('Could not map U:\');

      DbgPrintf('EmuMain : U Data := ' + g_strUDrive);
    end;

    // create Z:\ directory
    begin
      szBuffer := BasePath + '\EmuDisk\Z';
      CreateDirectory(PAnsiChar(szBuffer), nil);

      //(* is it necessary to make this directory title unique?
      szBuffer := szBuffer + '\' + IntToHex(pCertificate.dwTitleId, 8);
      CreateDirectory(PAnsiChar(szBuffer), nil);
      //*)

      g_strZDrive := szBuffer;
      g_hZDrive := CreateFile(PChar(szBuffer), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);

      if g_hUDrive = INVALID_HANDLE_VALUE then
        CxbxKrnlCleanup('Could not map Z:\');

      DbgPrintf('EmuMain : Z Data := ' + g_strZDrive);
    end;

  end;

  // initialize FS segment selector
  begin
    EmuInitFS();

    EmuGenerateFS(pTLS, pTLSData);
  end;

  // duplicate handle in order to retain Suspend/Resume thread rights from a remote thread
  begin
    hDupHandle := 0;

    DuplicateHandle(GetCurrentProcess(), GetCurrentThread(), GetCurrentProcess(), @hDupHandle, 0, FALSE, DUPLICATE_SAME_ACCESS);

    CxbxKrnlRegisterThread(hDupHandle);
  end;

  DbgPrintf('EmuMain : Initializing Direct3D.');

  XTL__EmuD3DInit(pXbeHeader, dwXbeHeaderSize);

  EmuHLEIntercept(pLibraryVersion, pXbeHeader);

  DbgPrintf('EmuMain : Initial thread starting.');

  // Xbe entry point
  try
    EmuSwapFS();   // XBox FS

    // _USE_XGMATH Disabled in mesh :[
    // halo : dword_0_2E2D18
    // halo : 1744F0 (bink)
    //_asm int 3;

    (*
    for v := 0 to (SizeOf(FuncAddr / SizeOf(UInt32)) - 1 do
    begin
        bool bExclude = False;
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
    //*)

    Entry();

    EmuSwapFS();   // Win2k/XP FS
  except
    on E: Exception do
      DbgPrintf('EmuMain : Catched an exception : ' + E.Message);
//    on(EmuException(GetExceptionInformation())) :
//      printf('Emu: WARNING!! Problem with ExceptionFilter');
  end;

  DbgPrintf('EmuMain : Initial thread ended.');

//  FFlush(stdout);

  CxbxKrnlTerminateThread();
end;

procedure CxbxKrnlRegisterThread(const hThread: THandle);
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
begin
  if EmuIsXboxFS then
    EmuSwapFS();    // Win2k/XP FS

  EmuCleanupFS;

if m_DxbxDebug = DM_CONSOLE then Sleep(10 * 1000); // TODO : Remove this!

  TerminateThread(GetCurrentThread(), 0);
end;

// alert for the situation where an Xref function body is hit
procedure EmuXRefFailure();
begin
  EmuSwapFS();    // Win2k/XP FS

  CxbxKrnlCleanup('XRef-only function body reached. Fatal Error.');
end;

procedure EmuPanic();
begin
  if EmuIsXboxFS then
    EmuSwapFS(); // Win2k/XP FS

  DbgPrintf('Emu: EmuPanic');

  CxbxKrnlCleanup('Kernel Panic!');

  EmuSwapFS(); // XBox FS
end;

procedure CxbxKrnlNoFunc;
begin
  EmuSwapFS();   // Win2k/XP FS

  DbgPrintf('EmuMain : CxbxKrnlNoFunc()');

  EmuSwapFS();   // XBox FS
end;

exports

  EmuPanic;

end.

