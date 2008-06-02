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
  uEmuFS;

type
  TEntryProc = procedure();
  PEntryProc = ^TEntryProc;

procedure CxbxKrnlInit(
  hwndParent: THandle;
  pTLSData: Pointer;
  pTLS: P_XBE_TLS;
  pLibraryVersion: P_XBE_LIBRARYVERSION;
  DbgMode: DebugMode;
  szDebugFilename: PChar;
  pXbeHeader: P_XBE_HEADER;
  dwXbeHeaderSize: DWord;
  Entry: PEntryProc); cdecl;

procedure CxbxKrnlNoFunc; cdecl;

procedure EmuPanic(); // export;
function EmuVerifyVersion(const szVersion: string): Boolean; // export;
procedure EmuCleanThread(); // export;

implementation

var
  DxbxKrnl_hEmuParent: THandle;
  DxbxKrnl_TLSData: Pointer;
  DxbxKrnl_TLS: P_XBE_TLS;
//  g_pLibraryVersion: P_XBE_LIBRARYVERSION;
//  g_szDebugFilename: PChar;
  DxbxKrnl_XbeHeader: P_XBE_HEADER;
//  g_dwXbeHeaderSize: DWord;
//  g_Entry: PEntryProc;

procedure CxbxKrnlInit(
  hwndParent: THandle;
  pTLSData: Pointer;
  pTLS: P_XBE_TLS;
  pLibraryVersion: P_XBE_LIBRARYVERSION;
  DbgMode: DebugMode;
  szDebugFilename: PChar;
  pXbeHeader: P_XBE_HEADER;
  dwXbeHeaderSize: DWord;
  Entry: PEntryProc);
var
  MemXbeHeader: P_XBE_HEADER;
  old_protection: DWord;
  szBuffer, BasePath: string;
  g_hCurDir: THandle;
//  spot, v: Integer;
begin
  // debug console allocation (if configured)
  SetLogMode(DbgMode);
  CreateLogs(ltKernel); // Initialize logging interface

  WriteLog('EmuInit : Dxbx Version ' + _DXBX_VERSION);

  // update caches
  DxbxKrnl_TLS := pTLS;
  DxbxKrnl_TLSData := pTLSData;
  DxbxKrnl_XbeHeader := pXbeHeader;
  DxbxKrnl_hEmuParent := IfThen(IsWindow(hwndParent), hwndParent, 0);

  // For Unicode Conversions
// TODO SetLocale(LC_ALL, 'English');

  // debug trace
  begin
{$IFDEF _DEBUG_TRACE}
    WriteLog('EmuMain : Debug Trace Enabled.');
    
    WriteLog('EmuMain : 0x' + IntToHex(Integer(@CxbxKrnlInit), 8) + ' : CxbxKrnlInit(');
    // TODO : For some reason, using Format() fails here?
    WriteLog('  hwndParent       : 0x' + IntToHex(hwndParent, 8));
    WriteLog('  pTLSData         : 0x' + IntToHex(Integer(pTLSData), 8));
    WriteLog('  pTLS             : 0x' + IntToHex(Integer(pTLS), 8));
    WriteLog('  pLibraryVersion  : 0x' + IntToHex(Integer(pLibraryVersion), 8) + ' ("' + PChar(pLibraryVersion) + '")');
    WriteLog('  DebugConsole     : 0x' + IntToHex(Ord(DbgMode), 8));
    WriteLog('  DebugFilename    : 0x' + IntToHex(Integer(szDebugFilename), 8) + ' ("' + szDebugFilename + '")');
    WriteLog('  pXBEHeader       : 0x' + IntToHex(Integer(pXbeHeader), 8));
    WriteLog('  dwXBEHeaderSize  : 0x' + IntToHex(dwXbeHeaderSize, 8));
    WriteLog('  Entry            : 0x' + IntToHex(Integer(Entry), 8));
    WriteLog(')');

{$ELSE}
    WriteLog('EmuMain : Debug Trace Disabled.');
{$ENDIF}
  end;

  // Load the necessary pieces of XBEHeader
  begin
    MemXbeHeader := P_XBE_HEADER($00010000);

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

  g_hCurDir := CreateFile(PChar(szBuffer), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  if g_hCurDir = INVALID_HANDLE_VALUE then
    CxbxKrnlCleanup('Could not map D:\');

  WriteLog('EmuMain : CurDir = ' + szBuffer);

  // initialize EmuDisk
  begin
    SetLength(szBuffer, MAX_PATH);
    SHGetSpecialFolderPath(0, @(szBuffer[1]), CSIDL_APPDATA, True);
    SetLength(szBuffer, StrLen(@(szBuffer[1])));

    BasePath := szBuffer + '\Dxbx';
    CreateDirectory(PAnsiChar(BasePath), nil);

    // create EmuDisk directory
    szBuffer := BasePath +'\EmuDisk';
    CreateDirectory(PAnsiChar(szBuffer), nil);

    // create T:\ directory
    begin
      szBuffer := BasePath +'\EmuDisk\T';
      CreateDirectory(PAnsiChar(szBuffer), nil);
(*
      Xbe::Certificate *pCertificate = (Xbe::Certificate* )pXbeHeader->dwCertificateAddr;
      sprintf(&szBuffer[spot+10], '\\%08x', pCertificate->dwTitleId);

      CreateDirectory(PAnsiChar(szBuffer), nil);

      g_strTDrive = strdup(szBuffer);

      g_hTDrive = CreateFile(szBuffer, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);

      if g_hTDrive = INVALID_HANDLE_VALUE then
        CxbxKrnlCleanup('Could not map T:\\\n');

      DbgPrintf('EmuMain (0x%X): T Data := %s\n', GetCurrentThreadId(), szBuffer);
*)
    end;

    // create U:\ directory
    begin
      szBuffer := BasePath +'\EmuDisk\U';
      CreateDirectory(PAnsiChar(szBuffer), nil);
(*
      sprintf(&szBuffer[spot+10], "\\%08x", pCertificate->dwTitleId);

      CreateDirectory(PAnsiChar(szBuffer), nil);

      g_strUDrive = strdup(szBuffer);

      g_hUDrive = CreateFile(szBuffer, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);

      if g_hUDrive = INVALID_HANDLE_VALUE then
        CxbxKrnlCleanup('Could not map U:\\\n');

      DbgPrintf('EmuMain (0x%X): U Data := %s\n', GetCurrentThreadId(), szBuffer);
*)
    end;

    // create Z:\ directory
    begin
      szBuffer := BasePath +'\EmuDisk\Z';
      CreateDirectory(PAnsiChar(szBuffer), nil);
(*
      //* is it necessary to make this directory title unique?
      sprintf(&szBuffer[spot+10], '\\%08x', pCertificate->dwTitleId);

      CreateDirectory(PAnsiChar(szBuffer), nil);
      //*/

      g_strZDrive = strdup(szBuffer);

      g_hZDrive = CreateFile(szBuffer, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);

      if g_hUDrive = INVALID_HANDLE_VALUE then
        CxbxKrnlCleanup("Could not map Z:\\\n");

      DbgPrintf('EmuMain (0x%X): Z Data := %s\n', GetCurrentThreadId(), szBuffer);
*)
    end;

  end;

  // initialize FS segment selector
  begin
    EmuInitFS();

    EmuGenerateFS(pTLS, pTLSData);
  end;

(*
  // duplicate handle in order to retain Suspend/Resume thread rights from a remote thread
  begin
    HANDLE hDupHandle = NULL;

    DuplicateHandle(GetCurrentProcess(), GetCurrentThread(), GetCurrentProcess(), &hDupHandle, 0, FALSE, DUPLICATE_SAME_ACCESS);

    CxbxKrnlRegisterThread(hDupHandle);
  end;

  DbgPrintf('EmuMain (0x%X): Initializing Direct3D.', GetCurrentThreadId());

  XTL::EmuD3DInit(pXbeHeader, dwXbeHeaderSize);

  EmuHLEIntercept(pLibraryVersion, pXbeHeader);

  DbgPrintf('EmuMain (0x%X): Initial thread starting.\n', GetCurrentThreadId());
*)

  // Xbe entry point
  try
    EmuSwapFS();   // XBox FS

    // _USE_XGMATH Disabled in mesh :[
    // halo : dword_0_2E2D18
    // halo : 1744F0 (bink)
    //_asm int 3;

    (*
    for(int v=0;v<sizeof(funcAddr)/sizeof(uint32);v++)
    {
        bool bExclude = false;
        for(int r=0;r<sizeof(funcExclude)/sizeof(uint32);r++)
        {
            if(funcAddr[v] == funcExclude[r])
            {
                bExclude = true;
                break;
            }
        }

        if(!bExclude)
        {
            *(uint08* )(funcAddr[v]) = 0xCC;
        }
    }
    //*)

    Entry^();

    EmuSwapFS();   // Win2k/XP FS
  except
    on E: Exception do
      WriteLog('EmuMain : Catched an exception : ' + E.Message); 
//    on(EmuException(GetExceptionInformation())) :
//      printf('Emu: WARNING!! Problem with ExceptionFilter');
  end;

  WriteLog('EmuMain : Initial thread ended.');

//  FFlush(stdout);
Sleep(10 * 1000); // TODO : Remove this!
  EmuCleanThread();

WriteLog('CxbxKrnlInit<');
end;

procedure EmuCleanThread(); // CxbxKrnlTerminateThread
begin
  if EmuIsXboxFS then
    EmuSwapFS();    // Win2k/XP FS

  EmuCleanupFS;

  TerminateThread(GetCurrentThread(), 0);
end;

procedure EmuXRefFailure;
begin
  EmuSwapFS();    // Win2k/XP FS
  CxbxKrnlCleanup('XRef-only function body reached. Fatal Error.');
end;


procedure CxbxKrnlNoFunc;
begin
  EmuSwapFS();   // Win2k/XP FS

  WriteLog('Emu: EmuNoFunc');

  EmuSwapFS();   // XBox FS
end;

function EmuVerifyVersion(const szVersion: string): Boolean;
begin
  Result := (szVersion = _DXBX_VERSION);
end;

procedure EmuPanic();
begin
  if EmuIsXboxFS then
    EmuSwapFS(); // Win2k/XP FS

  WriteLog('Emu: EmuPanic');

  CxbxKrnlCleanup('Kernel Panic!');

  EmuSwapFS(); // XBox FS
end;

exports

  EmuPanic;

end.

