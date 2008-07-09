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
unit uHLEIntercept;

{$INCLUDE ..\Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  SysUtils,
  // Jedi
  JwaWinType,
  // Cxbx
  uTypes,
  uLog,
  uDxbxKrnlUtils,
  uXbe,
  uEmuShared,
  uEmuFS,
  uHLEDatabase,
  // Dxbx
  DxLibraryAPIScanning;

procedure EmuHLEIntercept(pLibraryVersion: PXBE_LIBRARYVERSION; pXbeHeader: PXBE_HEADER);
procedure EmuInstallWrapper(FunctionAddr: PByte; WrapperAddr: PVOID); inline;

implementation

(*
//include 'CxbxKrnl.h'
//include 'Emu.h'
//include 'EmuFS.h'
//include 'EmuXTL.h'
//include 'EmuShared.h'
//include 'HLEDataBase.h'

function EmuLocateFunction(var Oovpa: OOVPA; lower: uint32; upper: uint32): PVOID;
procedure  EmuInstallWrappers(var OovpaTable: OOVPATable; OovpaTableSize: uint32; var pXbeHeader: Xbe.Header);

//include <shlobj.h>
//include <vector>

 std.vector<Pointer > vCacheOut;

 bool bCacheInp := False;
 std.vector<Pointer > vCacheInp;
 std.vector<Pointer >.const_iterator vCacheInpIter;
*)

procedure EmuHLEIntercept(pLibraryVersion: PXBE_LIBRARYVERSION; pXbeHeader: PXBE_HEADER);
var
  pCertificate: PXBE_CERTIFICATE;
//     szCacheFileName: array[0..260-1] of Char;
  dwLibraryVersions: uint32;
  dwHLEEntries: uint32;

  LastUnResolvedXRefs: uint32;
  OrigUnResolvedXRefs: uint32;

  p: Integer;
begin
  pCertificate := PXBE_CERTIFICATE(pXbeHeader.dwCertificateAddr);

  DbgPrintf('');
  DbgPrintf('*******************************************************************************');
  DbgPrintf('* Dxbx High Level Emulation database last modified ' + szHLELastCompileTime);
  DbgPrintf('*******************************************************************************');
  DbgPrintf('');

  { TODO : need to be translated to delphi }
(*
  //
  // initialize HLE cache file
  //

  begin
    SHGetSpecialFolderPath(0, szCacheFileName, CSIDL_APPDATA, TRUE);

    StrCat(szCacheFileName, '\Cxbx');

    CreateDirectory(szCacheFileName, 0);

    sint32 spot := -1;

    for (integer v:=0;v<260;v++)
    begin
        if (szCacheFileName[v] := '') then  begin  spot = v;  end;
        else if (szCacheFileName[v] := #0) then  begin  break;  end;
     end;

    if (spot <> -1) then  begin  szCacheFileName[spot] := #0;  end;

    //
    // create HLECache directory
    //

    StrCopy(@szCacheFileName[spot], '\HLECache');

    CreateDirectory(szCacheFileName, 0);

    //
    // open title's cache file
    //

    StrFmt(@szCacheFileName[spot+9], '\%08x.dat', pCertificate.dwTitleId);

    FILE *pCacheFile := FileOpen(szCacheFileName, fmOpenRead);

    if (pCacheFile <> 0) then
    begin
        bool bVerified := False;

        //
        // verify last compiled timestamp
        //

         szCacheLastCompileTime: array[0..64-1] of Char;

        FillChar(szCacheLastCompileTime, 0, 64);

        if (FileRead(szCacheLastCompileTime, 64, 1, pCacheFile) = 1) then
        begin
            if (StrComp(szCacheLastCompileTime, szHLELastCompileTime) = 0) then
            begin
                bVerified := true;
             end;
         end;

        //
        // load function addresses
        //

        if (bVerified) then
        begin
            while(true)
            begin
                Pointer cur;

                if (FileRead(@cur, 4, 1, pCacheFile) <> 1) then
                    break;

                vCacheInp.push_back(cur);
             end;

            bCacheInp := true;

            vCacheInpIter := vCacheInp.begin();

            DbgPrintf('HLE: Loaded HLE Cache for $%.08X', pCertificate.dwTitleId);
         end;

        FileClose(pCacheFile);
     end;
  end;
*)
    //
    // initialize openxdk emulation (TODO)
    //

  if pLibraryVersion = nil then
  begin
    DbgPrintf('HLE: Detected OpenXDK application...');
  end;

    //
    // initialize Microsoft XDK emulation
    //

  if pLibraryVersion <> nil then
  begin
    DbgPrintf('HLE: Detected Microsoft XDK application...');
    dwLibraryVersions := pXbeHeader.dwLibraryVersions;
    dwHLEEntries := HLEDataBaseSize div SizeOf(HLEData);

    LastUnResolvedXRefs := UnResolvedXRefs + 1;
    OrigUnResolvedXRefs := UnResolvedXRefs;

    p := 0;
    while UnResolvedXRefs < LastUnResolvedXRefs do
    begin
      Inc(p);
      DbgPrintf('HLE: Starting pass #' + IntToStr(p) + '...');

      LastUnResolvedXRefs := UnResolvedXRefs;

      DxbxScanForLibraryAPIs(pXbeHeader);
(*
            bool bFoundD3D := False;
            for (uint32 v:=0;v<dwLibraryVersions;v++)
            begin
                uint16 MajorVersion := pLibraryVersion[v].wMajorVersion;
                uint16 MinorVersion := pLibraryVersion[v].wMinorVersion;
                uint16 BuildVersion := pLibraryVersion[v].wBuildVersion;
                uint16 OrigBuildVersion := BuildVersion;

                // aliases
                if (BuildVersion := 4928) then  begin  BuildVersion = 4627;  end;
                if (BuildVersion := 5659) then  begin  BuildVersion = 5558;  end;

                Char szLibraryName[9] := (0);

                for (uint32 c:=0;c<8;c++)
                begin
                    szLibraryName[c] := pLibraryVersion[v].szName[c];
                 end;

                // TODO: HACK: These libraries are packed into one database
                if (StrComp(szLibraryName, 'D3DX8') = 0) then
                begin
                    StrCopy(szLibraryName, 'D3D8');
                 end;

                if (StrComp(szLibraryName, 'D3D8') = 0) then
                begin
                    if (bFoundD3D) then
                    begin
                        //DbgPrintf("Redundant\n");
                        continue;
                     end;

                    bFoundD3D := true;
                 end;

                if (bXRefFirstPass) then
                begin
                    if (StrComp('XAPILIB', szLibraryName) then  = 0 and MajorVersion = 1 and MinorVersion = 0 and
                        (BuildVersion = 3911 or BuildVersion = 4034 or BuildVersion = 4134 or BuildVersion = 4361
                      or BuildVersion = 4432 or BuildVersion = 4627 or BuildVersion = 5558 or BuildVersion = 5849))
                    begin
                        uint32 lower := pXbeHeader.dwBaseAddr;
                        uint32 upper := pXbeHeader.dwBaseAddr + pXbeHeader.dwSizeofImage;

                        // locate XapiProcessHeap
                        begin
                            Pointer pFunc := 0;
                            uint ProcessHeapOffs;
                            uint RtlCreateHeapOffs;

                            if (BuildVersion >= 5849) then
                            begin
                                pFunc := EmuLocateFunction((OOVPA)@XapiInitProcess_1_0_5849, lower, upper);
                                ProcessHeapOffs := $51;
                                RtlCreateHeapOffs := $4A;
                             end;
                            else if (BuildVersion >= 5558) then
                            begin
                                pFunc := EmuLocateFunction((OOVPA)@XapiInitProcess_1_0_5558, lower, upper);

                                // 5659 has an updated function
                                if (pFunc = 0) then
                                begin
                                    pFunc := EmuLocateFunction((OOVPA)@XapiInitProcess_1_0_5659, lower, upper);
                                 end;

                                ProcessHeapOffs := $51;
                                RtlCreateHeapOffs := $4A;
                             end;
                            else if (BuildVersion >= 4361) then
                            begin
                                if (OrigBuildVersion = 4928) then
                                begin
                          pFunc := EmuLocateFunction((OOVPA)@XapiInitProcess_1_0_4928, lower, upper);
                                    ProcessHeapOffs := $44;
                                    RtlCreateHeapOffs := $3B;
                                 end;
                                else
                                begin
                          pFunc := EmuLocateFunction((OOVPA)@XapiInitProcess_1_0_4361, lower, upper);
                                    ProcessHeapOffs := $3E;
                                    RtlCreateHeapOffs := $37;
                                 end;
                             end;
                            else // 3911, 4034, 4134
                            begin
                                pFunc := EmuLocateFunction((OOVPA)@XapiInitProcess_1_0_3911, lower, upper);
                                ProcessHeapOffs := $3E;
                                RtlCreateHeapOffs := $37;
                             end;

                            if (pFunc <> 0) then
                            begin
                                XTL.EmuXapiProcessHeap := *(PVOID)((uint32)pFunc + ProcessHeapOffs);

                                XTL.g_pRtlCreateHeap := *(XTL.pfRtlCreateHeap)((uint32)pFunc + RtlCreateHeapOffs);
                                XTL.g_pRtlCreateHeap := (XTL.pfRtlCreateHeap)((uint32)pFunc + (uint32)XTL.g_pRtlCreateHeap + RtlCreateHeapOffs + $04);

                                DbgPrintf('HLE: $%.08X . EmuXapiProcessHeap', XTL.EmuXapiProcessHeap);
                                DbgPrintf('HLE: $%.08X . g_pRtlCreateHeap', XTL.g_pRtlCreateHeap);
                             end;
                         end;
                     end;
                    else if (StrComp('D3D8', szLibraryName) then  = 0 and MajorVersion = 1 and MinorVersion = 0  and
                        (BuildVersion = 3925 or BuildVersion = 4134 or BuildVersion = 4361 or BuildVersion = 4432
                      or BuildVersion = 4627 or BuildVersion = 5558 or BuildVersion = 5849))
                    begin
                        uint32 lower := pXbeHeader.dwBaseAddr;
                        uint32 upper := pXbeHeader.dwBaseAddr + pXbeHeader.dwSizeofImage;

                        Pointer pFunc := 0;

                        if (BuildVersion = 3925) then
                            pFunc := EmuLocateFunction((OOVPA)@IDirect3DDevice8_SetRenderState_CullMode_1_0_3925, lower, upper);
                        else if (BuildVersion < 5558) then
                            pFunc := EmuLocateFunction((OOVPA)@IDirect3DDevice8_SetRenderState_CullMode_1_0_4134, lower, upper);
                        else
                            pFunc := EmuLocateFunction((OOVPA)@IDirect3DDevice8_SetRenderState_CullMode_1_0_5558, lower, upper);

                        // locate D3DDeferredRenderState
                        if (pFunc <> 0) then
                        begin
                            // offset for stencil cull enable render state in the deferred render state buffer
                            integer patchOffset := 0;

                            if (BuildVersion = 3925) then
                            begin
                                XTL.EmuD3DDeferredRenderState := (DWORD)((DWORD)((uint32)pFunc + $25) - $19F + 72*4);  // TODO: Clean up (?)
                                patchOffset := 142*4 - 72*4; // TODO: Verify
                             end;
                            else if (BuildVersion = 4134) then
                            begin
                                XTL.EmuD3DDeferredRenderState := (DWORD)((DWORD)((uint32)pFunc + $2B) - $248 + 82*4);  // TODO: Verify
                                patchOffset := 142*4 - 82*4;
                             end;
                            else if (BuildVersion = 4361) then
                            begin
                                XTL.EmuD3DDeferredRenderState := (DWORD)((DWORD)((uint32)pFunc + $2B) - $200 + 82*4);
                                patchOffset := 142*4 - 82*4;
                             end;
                            else if (BuildVersion = 4432) then
                            begin
                                XTL.EmuD3DDeferredRenderState := (DWORD)((DWORD)((uint32)pFunc + $2B) - $204 + 83*4);
                                patchOffset := 143*4 - 83*4;
                             end;
                            else if (BuildVersion = 4627) then
                            begin
                                XTL.EmuD3DDeferredRenderState := (DWORD)((DWORD)((uint32)pFunc + $2B) - $24C + 92*4);
                                patchOffset := 162*4 - 92*4;
                             end;
                            else if (BuildVersion = 5558 or BuildVersion = 5849) then
                            begin
                                // WARNING: Not thoroughly tested (just seemed very correct right away)
                                XTL.EmuD3DDeferredRenderState := (DWORD)((DWORD)((uint32)pFunc + $2B) - $24C + 92*4);
                                patchOffset := 162*4 - 92*4;
                             end;

                            XRefDataBase[XREF_D3DDEVICE]                   := *(DWORD)((DWORD)pFunc + $03);
                            XRefDataBase[XREF_D3DRS_STENCILCULLENABLE]     := (uint32)XTL.EmuD3DDeferredRenderState + patchOffset + 0*4;
                            XRefDataBase[XREF_D3DRS_ROPZCMPALWAYSREAD]     := (uint32)XTL.EmuD3DDeferredRenderState + patchOffset + 1*4;
                            XRefDataBase[XREF_D3DRS_ROPZREAD]              := (uint32)XTL.EmuD3DDeferredRenderState + patchOffset + 2*4;
                            XRefDataBase[XREF_D3DRS_DONOTCULLUNCOMPRESSED] := (uint32)XTL.EmuD3DDeferredRenderState + patchOffset + 3*4;

                            for (integer v:=0;v<44;v++)
                            begin
                                XTL.EmuD3DDeferredRenderState[v] := X_D3DRS_UNK;
                             end;

                            DbgPrintf('HLE: $%.08X . EmuD3DDeferredRenderState', XTL.EmuD3DDeferredRenderState);
                         end;
                        else
                        begin
                            XTL.EmuD3DDeferredRenderState := 0;
                            EmuWarning('EmuD3DDeferredRenderState was not found!');
                         end;

                        // locate D3DDeferredTextureState
                        begin
                            pFunc := 0;

                            if (BuildVersion = 3925) then
                                pFunc := EmuLocateFunction((OOVPA)@IDirect3DDevice8_SetTextureState_TexCoordIndex_1_0_3925, lower, upper);
                            else if (BuildVersion = 4134) then
                                pFunc := EmuLocateFunction((OOVPA)@IDirect3DDevice8_SetTextureState_TexCoordIndex_1_0_4134, lower, upper);
                            else if (BuildVersion = 4361 or BuildVersion = 4432) then
                                pFunc := EmuLocateFunction((OOVPA)@IDirect3DDevice8_SetTextureState_TexCoordIndex_1_0_4361, lower, upper);
                            else if (BuildVersion = 4627 or BuildVersion = 5558 or BuildVersion = 5849) then
                                pFunc := EmuLocateFunction((OOVPA)@IDirect3DDevice8_SetTextureState_TexCoordIndex_1_0_4627, lower, upper);

                            if (pFunc <> 0) then
                            begin
                                if (BuildVersion = 3925) then  // 0x18F180
                                    XTL.EmuD3DDeferredTextureState := (DWORD)((DWORD)((uint32)pFunc + $11) - $70); // TODO: Verify
                                else if (BuildVersion = 4134) then
                                    XTL.EmuD3DDeferredTextureState := (DWORD)((DWORD)((uint32)pFunc + $18) - $70); // TODO: Verify
                                else
                                    XTL.EmuD3DDeferredTextureState := (DWORD)((DWORD)((uint32)pFunc + $19) - $70);

                                for (integer s:=0;s<4;s++)
                                begin
                                    for (integer v:=0;v<32;v++)
                                        XTL.EmuD3DDeferredTextureState[v+s*32] := X_D3DTSS_UNK;
                                 end;

                                DbgPrintf('HLE: $%.08X . EmuD3DDeferredTextureState', XTL.EmuD3DDeferredTextureState);
                             end;
                            else
                            begin
                                XTL.EmuD3DDeferredTextureState := 0;
                                CxbxKrnlCleanup('EmuD3DDeferredTextureState was not found!');
                             end;
                         end;
                     end;
                 end;

                DbgPrintf('HLE: * Searching HLE database for %s %d.%d.%d ...', pLibraryVersion[v].szName, MajorVersion, MinorVersion, BuildVersion);

                bool found:=False;

                for (uint32 d:=0;d<dwHLEEntries;d++)
                begin
                    if (BuildVersion <> HLEDataBase[d].BuildVersion
                    or MinorVersion <> HLEDataBase[d].MinorVersion
                    or MajorVersion <> HLEDataBase[d].MajorVersion
                    or StrComp(szLibraryName, HLEDataBase[d].Library) <> 0) then
                      Continue;

                    found := true;

                    DbgPrintf('Found');

                    EmuInstallWrappers(HLEDataBase[d].OovpaTable, HLEDataBase[d].OovpaTableSize, pXbeHeader);
                 end;

                if (not found) then DbgPrintf('Skipped');
             end;
*)

      bXRefFirstPass := False;
    end;

        // display Xref summary
    DbgPrintf('HLE: Resolved ' + IntToStr(OrigUnResolvedXRefs - UnResolvedXRefs) + ' cross reference(s)');
  end;
(*
    vCacheInp.empty();

    //
    // update cache file
    //

    if (vCacheOut.size() > 0) then
    begin
        FILE *pCacheFile := FileOpen(szCacheFileName, fmOpenWrite);

        if (pCacheFile <> 0) then
        begin
            DbgPrintf('HLE: Saving HLE Cache for $%.08XArgs: array of const', pCertificate.dwTitleId);

            //
            // write last compiled timestamp
            //

             szCacheLastCompileTime: array[0..64-1] of Char;

            FillChar(szCacheLastCompileTime, 0, 64);

            StrCopy(szCacheLastCompileTime, szHLELastCompileTime);

            FileWrite(szCacheLastCompileTime, 64, 1, pCacheFile);

            //
            // write function addresses
            //

            std.vector<Pointer >.const_iterator cur;

            for (cur := vCacheOut.begin();cur <> vCacheOut.end(); ++cur)
            begin
                FileWrite( and (cur), 4, 1, pCacheFile);
             end;
         end;

        FileClose(pCacheFile);
     end;

    vCacheOut.empty();

    DbgPrintf('');

    Exit;
  end;
*)
end;

// install function interception wrapper

procedure EmuInstallWrapper(FunctionAddr: PByte; WrapperAddr: PVOID); inline;
var
  RelativeJMPAddress: UInt32;
begin
  // Write JMP rel16 opcode (Jump near, displacement relative to next instruction) :
  FunctionAddr^ := $E9;
  // Calculate relative address :
  RelativeJMPAddress := (uint32(WrapperAddr) - uint32(FunctionAddr) - 5);
  // Write that after the JMP :
  CopyMemory(Pointer(Integer(FunctionAddr) + 1), @RelativeJMPAddress, 4);
end;

(*
// locate the given function, searching within lower and upper bounds
function EmuLocateFunction(var Oovpa: OOVPA; lower: uint32; upper: uint32): Pointer;
begin
    uint32 count := Oovpa.Count;

    // Skip out if this is an unnecessary search
    if ( not bXRefFirstPass and Oovpa.XRefCount = 0 and Oovpa.XRefSaveIndex = (uint08)-1) then
        result:= 0;

    // large
    if (Oovpa.Large = 1) then
    begin
        LOOVPA<1> *Loovpa := (LOOVPA<1>)Oovpa;

        upper:= upper - Loovpa.Lovp[count-1].Offset;

        // search all of the image memory
        for (uint32 cur:=lower;cur<upper;cur++)
        begin
            uint32 v;

            // check all cross references
            for (v:=0;v<Loovpa.XRefCount;v++)
            begin
                uint32 Offset := Loovpa.Lovp[v].Offset;
                uint32 Value  := Loovpa.Lovp[v].Value;

                uint32 RealValue := *(uint32)(cur + Offset);

                if (XRefDataBase[Value] = -1) then
                    goto skipout_L;   // unsatisfied Xref is not acceptable

                if ((RealValue + cur + Offset+4 <> XRefDataBase[Value]) and (RealValue <> XRefDataBase[Value])) then
                    break;
             end;

            // check all pairs, moving on if any do not match
            for (v:=0;v<count;v++)
            begin
                uint32 Offset := Loovpa.Lovp[v].Offset;
                uint32 Value  := Loovpa.Lovp[v].Value;

                uint08 RealValue := *(uint08)(cur + Offset);

                if (RealValue <> Value) then
                    break;
             end;

            // success if we found all pairs
            if (v = count) then
            begin
                if (Loovpa.XRefSaveIndex <> (uint08)-1) then
                begin
                    if (XRefDataBase[Loovpa.XRefSaveIndex] = -1) then
                    begin
                        UnResolvedXRefs:= UnResolvedXRefs - 1;
                        XRefDataBase[Loovpa.XRefSaveIndex] := cur;

                        result:= cur;
                     end;
                    else
                    begin
                        result:= XRefDataBase[Loovpa.XRefSaveIndex];   // already Found, no bother patching again
                     end;
                 end;

                result:= cur;
             end;

            skipout_L:;
         end;
     end;
    // small
    else
    begin
        SOOVPA<1> *Soovpa := (SOOVPA<1>)Oovpa;

        upper:= upper - Soovpa.Sovp[count-1].Offset;

        // search all of the image memory
        for (uint32 cur:=lower;cur<upper;cur++)
        begin
            uint32 v;

            // check all cross references
            for (v:=0;v<Soovpa.XRefCount;v++)
            begin
                uint32 Offset := Soovpa.Sovp[v].Offset;
                uint32 Value  := Soovpa.Sovp[v].Value;

                uint32 RealValue := *(uint32)(cur + Offset);

                if (XRefDataBase[Value] = -1) then
                    goto skipout_S;   // Unsatisfied XRef is not acceptable

                if ( (RealValue + cur + Offset + 4 <> XRefDataBase[Value]) and (RealValue <> XRefDataBase[Value])) then
                    break;
             end;

            // check OV pairs if all xrefs matched
            if (v = Soovpa.XRefCount) then
            begin
                // check all pairs, moving on if any do not match
                for (;v<count;v++)
                begin
                    uint32 Offset := Soovpa.Sovp[v].Offset;
                    uint32 Value  := Soovpa.Sovp[v].Value;

                    uint08 RealValue := *(uint08)(cur + Offset);

                    if (RealValue <> Value) then
                        break;
                 end;
             end;

            // success if we found all pairs
            if (v = count) then
            begin
                if (Soovpa.XRefSaveIndex <> (uint08)-1) then
                begin
                    if (XRefDataBase[Soovpa.XRefSaveIndex] = -1) then
                    begin
                        UnResolvedXRefs:= UnResolvedXRefs - 1;
                        XRefDataBase[Soovpa.XRefSaveIndex] := cur;

                        result:= cur;
                     end;
                    else
                    begin
                        result:= XRefDataBase[Soovpa.XRefSaveIndex];   // already Found, no bother patching again
                     end;
                 end;

                result:= cur;
             end;

            skipout_S:;
         end;
     end;

    result:= 0;
 end;

// install function interception wrappers
  procedure EmuInstallWrappers(var OovpaTable: OOVPATable; OovpaTableSize: uint32; var pXbeHeader: Xbe.Header);
    uint32 lower := pXbeHeader.dwBaseAddr;
    uint32 upper := pXbeHeader.dwBaseAddr + pXbeHeader.dwSizeofImage;

    // traverse the full OOVPA table
    for (uint32 a:=0;a<OovpaTableSize/SizeOf(OOVPATable);a++)
    begin
        OOVPA *Oovpa := OovpaTable[a].Oovpa;

        Pointer pFunc := 0;

        if (bCacheInp and (vCacheInpIter <> vCacheInp.end())) then
        begin
            pFunc := (vCacheInpIter);

            ++vCacheInpIter;
         end;
        else
        begin
            pFunc := EmuLocateFunction(Oovpa, lower, upper);
            vCacheOut.push_back(pFunc);
         end;

        if (pFunc <> 0) then
        begin
            #ifdef _DEBUG_TRACE
            DbgPrintf('HLE: $%.08X . %s', pFunc, OovpaTable[a].szFuncName);
            //endif

            if (OovpaTable[a].lpRedirect = 0) then
            begin
                EmuInstallWrapper(pFunc, EmuXRefFailure);
             end;
            else
            begin
                EmuInstallWrapper(pFunc, OovpaTable[a].lpRedirect);
             end;
         end;
     end;
end;
*)

end.
