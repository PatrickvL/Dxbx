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
unit uEmu;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  // Dxbx
  uConsts, uTypes, uLog, uXbe, uEmuFS;

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

(*
procedure EmuPanic; export;
function EmuVerifyVersion( const szVersion : string ) : boolean; export;
procedure EmuCleanup ( szErrorMessage : String ); export;
procedure EmuCleanThread; export;   *)

implementation

uses
  // Delphi
  SysUtils, Dialogs;

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
begin
  WriteLog('EmuInit');

   (*g_pTLS       = pTLS;
   g_pTLSData   = pTLSData;
   g_pXbeHeader = pXbeHeader;

 // For Unicode Conversions
 setlocale(LC_ALL, "English");

    // debug console allocation (if configured)
    case DbgMode of
       DM_CONSOLE : begin
         if AllocConsole() then begin
            freopen("CONOUT$", "wt", stdout);
            SetConsoleTitle("Cxbx : Kernel Debug Console");
            SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FOREGROUND_GREEN | FOREGROUND_BLUE | FOREGROUND_RED);
            WriteLog( Format ('Emu (0x%X): Debug console allocated (DM_CONSOLE).', GetCurrentThreadId );
         end;
       end;

       DM_FILE : begin
         FreeConsole();
         freopen(szDebugFilename, "wt", stdout);
         WriteLog( Format ( 'Emu (0x%X): Debug console allocated (DM_FILE).', GetCurrentThreadId );
       end;
    else
      FreeConsole();
      char buffer[16];
      if GetConsoleTitle(buffer, 16)) <> '' then
        freopen("nul", "w", stdout);
    end;


    // debug trace

    {$IFDEF _DEBUG_TRACE}
        WriteLog ( Format ( 'Emu (0x%X): Debug Trace Enabled.', GetCurrentThreadId );

        WriteLog('(');
        WriteLog(Format('  pTLSData         : 0x%.8X', [pTLSData]));
        WriteLog(Format('  pTLS             : 0x%.8X', [pTLS]));
        WriteLog(Format('  pLibraryVersion  : 0x%.8X', [pLibraryVersion]));
        WriteLog(Format('  DebugConsole     : 0x%.8X', [Ord(DbgMode)]));
        WriteLog(Format('  DebugFilename    : "%s"', [szDebugFilename]));
        WriteLog(Format('  pXBEHeader       : 0x%.8X', [pXbeHeader]));
        WriteLog(Format('  dwXBEHeaderSize  : 0x%.8X', [dwXbeHeaderSize]));
        WriteLog(Format('  Entry            : 0x%.8X', [Entry]));
        WriteLog(')');
               GetCurrentThreadId(), pTLSData, pTLS, pLibraryVersion, DbgMode, szDebugFilename, pXbeHeader, dwXbeHeaderSize, Entry);

    {$ELSE}
        WriteLog ( Format ( 'Emu (0x%X): Debug Trace Disabled.', GetCurrentThreadId );
    {$ENDIF}



    // Load the necessary pieces of XBEHeader
        Xbe::Header *MemXbeHeader = (Xbe::Header*)(*0x00010000;
(*
        uint32 old_protection = 0;

        VirtualProtect(MemXbeHeader, 0x1000, PAGE_READWRITE, &old_protection);

        // we sure hope we aren't corrupting anything necessary for an .exe to survive :]
        MemXbeHeader->dwSizeofHeaders   = pXbeHeader->dwSizeofHeaders;
        MemXbeHeader->dwCertificateAddr = pXbeHeader->dwCertificateAddr;
        MemXbeHeader->dwPeHeapReserve   = pXbeHeader->dwPeHeapReserve;
        MemXbeHeader->dwPeHeapCommit    = pXbeHeader->dwPeHeapCommit;

        memcpy(&MemXbeHeader->dwInitFlags, &pXbeHeader->dwInitFlags, sizeof(pXbeHeader->dwInitFlags));

        memcpy((void*)(*pXbeHeader->dwCertificateAddr, &((uint08*)(*pXbeHeader)[pXbeHeader->dwCertificateAddr - 0x00010000], sizeof(Xbe::Certificate));

 // Initialize current directory

(*  char szBuffer[260];

        g_EmuShared->GetXbePath(szBuffer);

        SetCurrentDirectory(szBuffer);

  g_hCurDir = CreateFile(szBuffer, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);

        if(g_hCurDir == INVALID_HANDLE_VALUE)
   EmuCleanup("Could not map D:\\\n");


 // Initialize T:\ and U:\ directories

  char szBuffer[260];

  {$IFDEF _DEBUG}
    GetModuleFileName(GetModuleHandle("CxbxKrnl.dll"), szBuffer, 260);
  {$ELSE}
    GetModuleFileName(GetModuleHandle("Cxbx.dll"), szBuffer, 260);
  {$ENDIF}





        sint32 spot=-1;
        for v := 0 to 260 do begin
          if(szBuffer[v] == '\\')
              spot = v;
          else if(szBuffer[v] == '\0')
              break;
        end;

        if(spot != -1)
            szBuffer[spot] = '\0';

        Xbe::Certificate *pCertificate = (Xbe::Certificate*)(*pXbeHeader->dwCertificateAddr;

        // Create TData Directory

            strcpy(&szBuffer[spot], "\\TDATA");

            CreateDirectory(szBuffer, NULL);

            sprintf(&szBuffer[spot+6], "\\%08x", pCertificate->dwTitleId);

            CreateDirectory(szBuffer, NULL);

            g_hTDrive = CreateFile(szBuffer, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);

            if(g_hTDrive == INVALID_HANDLE_VALUE)
                EmuCleanup("Could not map T:\\\n");


        // Create UData Directory

            strcpy(&szBuffer[spot], "\\UDATA");

            CreateDirectory(szBuffer, NULL);

            sprintf(&szBuffer[spot+6], "\\%08x", pCertificate->dwTitleId);

            CreateDirectory(szBuffer, NULL);

            g_hUDrive = CreateFile(szBuffer, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);

            if(g_hUDrive == INVALID_HANDLE_VALUE)
                EmuCleanup("Could not map U:\\\n");


        // Create ZData Directory

            strcpy(&szBuffer[spot], "\\CxbxCache");

            CreateDirectory(szBuffer, NULL);

            //* is it necessary to make this directory title unique?
            sprintf(&szBuffer[spot+10], "\\%08x", pCertificate->dwTitleId);

            CreateDirectory(szBuffer, NULL);
            //*/

            g_hZDrive = CreateFile(szBuffer, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);

            if(g_hUDrive == INVALID_HANDLE_VALUE)
                EmuCleanup("Could not map Z:\\\n");



    // Initialize OpenXDK emulation
    if (pLibraryVersion = 0) then begin
      WriteLog ( Format ( 'Emu (0x%X): Detected OpenXDK application...', GetCurrentThreadId );
    end;

    // Initialize Microsoft XDK emulation
    if (pLibraryVersion <> 0) then begin

        WriteLog( Format ( 'Emu (0x%X): Detected Microsoft XDK application...', GetCurrentThreadId );

        uint32 dwLibraryVersions = pXbeHeader->dwLibraryVersions;
        uint32 dwHLEEntries      = HLEDataBaseSize/sizeof(HLEData);

        uint32 LastUnResolvedXRefs = UnResolvedXRefs+1;
        uint32 OrigUnResolvedXRefs = UnResolvedXRefs;

        for(int p=0;UnResolvedXRefs < LastUnResolvedXRefs;p++)
        {
            printf("Emu (0x%X): Beginning HLE Pass %d...\n", GetCurrentThreadId(), p);

            LastUnResolvedXRefs = UnResolvedXRefs;

            bool bFoundD3D = false;
            for(uint32 v=0;v<dwLibraryVersions;v++)
            {
                uint16 MajorVersion = pLibraryVersion[v].wMajorVersion;
                uint16 MinorVersion = pLibraryVersion[v].wMinorVersion;
                uint16 BuildVersion = pLibraryVersion[v].wBuildVersion;

                char szLibraryName[9] = {0};

 {               for(uint32 c=0;c<8;c++)
                    szLibraryName[c] = pLibraryVersion[v].szName[c];

                printf("Emu (0x%X): Locating HLE Information for %s %d.%d.%d...", GetCurrentThreadId(), szLibraryName, MajorVersion, MinorVersion, BuildVersion);

                // TODO: HACK: These libraries are packed into one database
                if(strcmp(szLibraryName, "D3DX8") == 0)
                    strcpy(szLibraryName, "D3D8");

                if(strcmp(szLibraryName, "D3D8") == 0)
                {
                    if(bFoundD3D)
                    {
                        printf("Redundant\n");
                        continue;
                    }

{                    bFoundD3D = true;
                }

{                bool found=false;

                for(uint32 d=0;d<dwHLEEntries;d++)
                {
                    if(BuildVersion != HLEDataBase[d].BuildVersion || MinorVersion != HLEDataBase[d].MinorVersion || MajorVersion != HLEDataBase[d].MajorVersion || strcmp(szLibraryName, HLEDataBase[d].Library) != 0)
                        continue;

                    found = true;

                    printf("Found\n");

                    EmuInstallWrappers(HLEDataBase[d].OovpaTable, HLEDataBase[d].OovpaTableSize, Entry, pXbeHeader);
                }

{                if(!found)
                    printf("Skipped\n");

                if(bXRefFirstPass)
                {
                    if(strcmp("XAPILIB", szLibraryName) == 0 && MajorVersion == 1 && MinorVersion == 0 && (BuildVersion == 3911 || BuildVersion == 4034 || BuildVersion == 4134 || BuildVersion == 4361 || BuildVersion == 4627))
                    {
                        uint32 lower = pXbeHeader->dwBaseAddr;
                        uint32 upper = pXbeHeader->dwBaseAddr + pXbeHeader->dwSizeofImage;

            // ******************************************************************
            // * Locate XapiProcessHeap
            // ******************************************************************
                        {
                            void *pFunc = 0;

                            if(BuildVersion >= 4361)
                 pFunc = EmuLocateFunction((OOVPA*)(*&XapiInitProcess_1_0_4361, lower, upper);
                            else // 3911, 4034, 4134
                                pFunc = EmuLocateFunction((OOVPA*)(*&XapiInitProcess_1_0_3911, lower, upper);

             if(pFunc != 0)
             {
              XTL::EmuXapiProcessHeap = *(PVOID**)(*((uint32)pFunc + 0x3E);

              XTL::g_pRtlCreateHeap = *(XTL::pfRtlCreateHeap*)(*((uint32)pFunc + 0x37);
              XTL::g_pRtlCreateHeap = (XTL::pfRtlCreateHeap)((uint32)pFunc + (uint32)XTL::g_pRtlCreateHeap + 0x37 + 0x04);

              printf("Emu (0x%X): 0x%.8X -> EmuXapiProcessHeap\n", GetCurrentThreadId(), XTL::EmuXapiProcessHeap);
              printf("Emu (0x%X): 0x%.8X -> RtlCreateHeap\n", GetCurrentThreadId(), XTL::g_pRtlCreateHeap);
             }
{				        }
{                    }
{			        else if(strcmp("D3D8", szLibraryName) == 0 && MajorVersion == 1 && MinorVersion == 0 && (BuildVersion == 4134 || BuildVersion == 4361 || BuildVersion == 4627))
           {
                        uint32 lower = pXbeHeader->dwBaseAddr;
                        uint32 upper = pXbeHeader->dwBaseAddr + pXbeHeader->dwSizeofImage;

            void *pFunc = EmuLocateFunction((OOVPA*)(*&IDirect3DDevice8_SetRenderState_CullMode_1_0_4134, lower, upper);

                        // ******************************************************************
            // * Locate D3DDeferredRenderState
            // ******************************************************************
                        if(pFunc != 0 && (BuildVersion == 4134 || BuildVersion == 4361 || BuildVersion == 4627))
                        {
                            if(BuildVersion == 4134)
                                XTL::EmuD3DDeferredRenderState = (DWORD*)(*(DWORD*)(*((uint32)pFunc + 0x2B) - 0x248 + 82*4);  // TODO: Verify
                            else if(BuildVersion == 4361)
              XTL::EmuD3DDeferredRenderState = (DWORD*)(*(DWORD*)(*((uint32)pFunc + 0x2B) - 0x200 + 82*4);
                            else if(BuildVersion == 4627)
              XTL::EmuD3DDeferredRenderState = (DWORD*)(*(DWORD*)(*((uint32)pFunc + 0x2B) - 0x24C + 92*4);

                            for(int v=0;v<146;v++)
                                XTL::EmuD3DDeferredRenderState[v] = X_D3DRS_UNK;

                            printf("Emu (0x%X): 0x%.8X -> EmuD3DDeferredRenderState\n", GetCurrentThreadId(), XTL::EmuD3DDeferredRenderState);
                        }
{                        else
                        {
                            XTL::EmuD3DDeferredRenderState = 0;
                            EmuWarning("EmuD3DDeferredRenderState was not found!");
                        }

                        // ******************************************************************
            // * Locate D3DDeferredTextureState
            // ******************************************************************
                        {
                            if(BuildVersion == 4134)
                                pFunc = EmuLocateFunction((OOVPA*)(*&IDirect3DDevice8_SetTextureState_TexCoordIndex_1_0_4134, lower, upper);
                            else if(BuildVersion == 4361)
                                pFunc = EmuLocateFunction((OOVPA*)(*&IDirect3DDevice8_SetTextureState_TexCoordIndex_1_0_4361, lower, upper);
                            else if(BuildVersion == 4627)
                                pFunc = EmuLocateFunction((OOVPA*)(*&IDirect3DDevice8_SetTextureState_TexCoordIndex_1_0_4627, lower, upper);

                            if(pFunc != 0)
                            {
                                if(BuildVersion == 4134)
                     XTL::EmuD3DDeferredTextureState = (DWORD*)(*(DWORD*)(*((uint32)pFunc + 0x18) - 0x70);
                                else
                     XTL::EmuD3DDeferredTextureState = (DWORD*)(*(DWORD*)(*((uint32)pFunc + 0x19) - 0x70);

                                for(int v=0;v<32*4;v++)
                                    XTL::EmuD3DDeferredTextureState[v] = X_D3DTSS_UNK;

                                printf("Emu (0x%X): 0x%.8X -> EmuD3DDeferredTextureState\n", GetCurrentThreadId(), XTL::EmuD3DDeferredTextureState);
                            }
{                            else
                            {
                                XTL::EmuD3DDeferredTextureState = 0;
                                EmuWarning("EmuD3DDeferredTextureState was not found!");
                            }
{                        }
{			        }
{                }
{            }

{            bXRefFirstPass = false;
        }

        // ******************************************************************
        // * Display XRef Summary
        // ******************************************************************
{        printf("Emu (0x%X): Resolved %d cross reference(s)\n", GetCurrentThreadId(), OrigUnResolvedXRefs - UnResolvedXRefs);
    }

 // ******************************************************************
    // * Initialize FS Emulation
    // ******************************************************************
    {
        EmuInitFS();

        EmuGenerateFS(pTLS, pTLSData);
    }

{    printf("Emu (0x%X): Initializing Direct3D.\n", GetCurrentThreadId());

    XTL::EmuD3DInit(pXbeHeader, dwXbeHeaderSize);

    printf("Emu (0x%X): Initial thread starting.\n", GetCurrentThreadId());

    // ******************************************************************
    // * Entry Point
    // ******************************************************************
    __try
    {
        EmuSwapFS();   // XBox FS

        // _USE_XGMATH Disabled in mesh :[
        // halo : dword_0_2E2D18
        //_asm int 3

        Entry();

        EmuSwapFS();   // Win2k/XP FS
    }
{    __except(EmuException(GetExceptionInformation()))
    {
        printf("Emu: WARNING!! Problem with ExceptionFilter\n");
    }

{    printf("Emu (0x%X): Initial thread ended.\n", GetCurrentThreadId());

    fflush(stdout);

    EmuCleanThread();

    return;
}               *)
end;

procedure EmuCleanThread;
begin
  if EmuIsXboxFS then
    EmuSwapFS();    // Win2k/XP FS

  EmuCleanupFS;

  TerminateThread(GetCurrentThread(), 0);
end;

procedure EmuCleanup(szErrorMessage: string);
begin
  CreateLogs(ltKernel);
  WriteLog('EmuInit');

  // Print out ErrorMessage (if exists)
(*    if(szErrorMessage != NULL)
    {
        char szBuffer1[255];
        char szBuffer2[255];

        va_list argp;

        sprintf(szBuffer1, "Emu (0x%X): Recieved Fatal Message -> \n\n", GetCurrentThreadId());

        va_start(argp, szErrorMessage);

        vsprintf(szBuffer2, szErrorMessage, argp);

        va_end(argp);

        strcat(szBuffer1, szBuffer2);

        printf("%s\n", szBuffer1);

        MessageBox(NULL, szBuffer1, "CxbxKrnl", MB_OK | MB_ICONEXCLAMATION);
    }

    printf("CxbxKrnl: Terminating Process\n");
    fflush(stdout);

    // ******************************************************************
    // * Cleanup debug output
    // ******************************************************************
    {
        FreeConsole();

        char buffer[16];

        if(GetConsoleTitle(buffer, 16) != NULL)
            freopen("nul", "w", stdout);
    }

    TerminateProcess(GetCurrentProcess(), 0);

    return;  *)
end;

procedure EmuXRefFailure;
begin
  EmuSwapFS();    // Win2k/XP FS
  EmuCleanup('XRef-only function body reached. Fatal Error.');
end;


procedure CxbxKrnlNoFunc; cdecl;
begin
  EmuSwapFS();   // Win2k/XP FS

  WriteLog('Emu: EmuNoFunc' + IntToStr(GetCurrentThreadId));

  EmuSwapFS();   // XBox FS*)
end;

function EmuVerifyVersion(const szVersion: string): Boolean;
begin
  Result := (szVersion = _DXBX_VERSION);
end;

procedure EmuPanic; stdcall;
begin
  if EmuIsXboxFS then
    EmuSwapFS; // Win2k/XP FS

  WriteLog('Emu: EmuPanic' + IntToStr(GetCurrentThreadId));

  EmuCleanup('Kernel Panic!');

  EmuSwapFS(); // XBox FS
end;

exports
  EmuPanic;

end.

