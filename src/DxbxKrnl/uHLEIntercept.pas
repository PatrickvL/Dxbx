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
  Classes, // TBits
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
  uXboxLibraryUtils,
  uStoredTrieTypes,
  DxLibraryAPIScanning;

procedure EmuHLEIntercept(pLibraryVersion: PXBE_LIBRARYVERSION; pXbeHeader: PXBE_HEADER);
procedure EmuInstallWrapper(FunctionAddr: PByte; WrapperAddr: PVOID); inline;
procedure EmuInstallWrappers(const pXbeHeader: PXBE_HEADER);

implementation

procedure EmuHLEIntercept(pLibraryVersion: PXBE_LIBRARYVERSION; pXbeHeader: PXBE_HEADER);
(*
var
//  pCertificate: PXBE_CERTIFICATE;
  dwLibraryVersions: uint32;
  dwHLEEntries: uint32;

  LastUnResolvedXRefs: uint32;
  OrigUnResolvedXRefs: uint32;

  p: Integer;
*)
begin
//  pCertificate := PXBE_CERTIFICATE(pXbeHeader.dwCertificateAddr);

  // initialize openxdk emulation (TODO)
  if pLibraryVersion = nil then
  begin
    DbgPrintf('DxbxHLE: Detected OpenXDK application... cannot patch!');
    Exit;
  end;

  // initialize Microsoft XDK emulation
  DbgPrintf('DxbxHLE: Detected Microsoft XDK application...');

  DxbxScanForLibraryAPIs(pLibraryVersion, pXbeHeader);

  EmuInstallWrappers(pXbeHeader);

(*
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

      bool bFoundD3D := False;
      for (uint32 v:=0;v<dwLibraryVersions;v++)
      begin
        uint16 MajorVersion := pLibraryVersion[v].wMajorVersion;
        uint16 MinorVersion := pLibraryVersion[v].wMinorVersion;
        uint16 BuildVersion := pLibraryVersion[v].wBuildVersion;
        uint16 OrigBuildVersion := BuildVersion;

        // aliases
        if BuildVersion = 4928 then
          BuildVersion := 4627;

        if BuildVersion = 5659 then
          BuildVersion := 5558;

        Char szLibraryName[9] := (0);
        for c := 0 to 7 do
          szLibraryName[c] := pLibraryVersion[v].szName[c];

        // TODO: HACK: These libraries are packed into one database
        if StrComp(szLibraryName, 'D3DX8') = 0 then
          StrCopy(szLibraryName, 'D3D8');

        if StrComp(szLibraryName, 'D3D8') = 0 then
        begin
          if (bFoundD3D) then
          begin
              //DbgPrintf('Redundant');
              Continue;
          end;

          bFoundD3D := True;
        end;

        if bXRefFirstPass then
        begin
          if (StrComp('XAPILIB', szLibraryName) = 0) and (MajorVersion = 1) and (MinorVersion = 0)
          and ((BuildVersion = 3911) or (BuildVersion = 4034) or (BuildVersion = 4134) or (BuildVersion = 4361) or
               (BuildVersion = 4432) or (BuildVersion = 4627) or (BuildVersion = 5558) or (BuildVersion = 5849)) then
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
              end
              else if (BuildVersion >= 5558) then
              begin
                pFunc := EmuLocateFunction((OOVPA)@XapiInitProcess_1_0_5558, lower, upper);

                // 5659 has an updated function
                if (pFunc = 0) then
                  pFunc := EmuLocateFunction((OOVPA)@XapiInitProcess_1_0_5659, lower, upper);

                ProcessHeapOffs := $51;
                RtlCreateHeapOffs := $4A;
              end
              else if (BuildVersion >= 4361) then
              begin
                if (OrigBuildVersion = 4928) then
                begin
                  pFunc := EmuLocateFunction((OOVPA)@XapiInitProcess_1_0_4928, lower, upper);
                  ProcessHeapOffs := $44;
                  RtlCreateHeapOffs := $3B;
                end
                else
                begin
                  pFunc := EmuLocateFunction((OOVPA)@XapiInitProcess_1_0_4361, lower, upper);
                  ProcessHeapOffs := $3E;
                  RtlCreateHeapOffs := $37;
                end;
              end
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
          end // not XAPILIB
          else
            if (StrComp('D3D8', szLibraryName) = 0) and (MajorVersion = 1) and (MinorVersion = 0)
            and ((BuildVersion = 3925) or (BuildVersion = 4134) or (BuildVersion = 4361) or (BuildVersion = 4432) or
                 (BuildVersion = 4627) or (BuildVersion = 5558) or (BuildVersion = 5849)) then
            begin
              uint32 lower := pXbeHeader.dwBaseAddr;
              uint32 upper := pXbeHeader.dwBaseAddr + pXbeHeader.dwSizeofImage;

              Pointer pFunc := nil;

              if (BuildVersion = 3925) then
                pFunc := EmuLocateFunction((OOVPA)@IDirect3DDevice8_SetRenderState_CullMode_1_0_3925, lower, upper);
              else if (BuildVersion < 5558) then
                pFunc := EmuLocateFunction((OOVPA)@IDirect3DDevice8_SetRenderState_CullMode_1_0_4134, lower, upper);
              else
                pFunc := EmuLocateFunction((OOVPA)@IDirect3DDevice8_SetRenderState_CullMode_1_0_5558, lower, upper);

              // locate D3DDeferredRenderState
              if Assigned(pFunc) then
              begin
                // offset for stencil cull enable render state in the deferred render state buffer
                Integer patchOffset := 0;

                if (BuildVersion = 3925) then
                begin
                  XTL.EmuD3DDeferredRenderState := (DWORD)((DWORD)((uint32)pFunc + $25) - $19F + 72*4);  // TODO: Clean up (?)
                  patchOffset := 142*4 - 72*4; // TODO: Verify
                end
                else if (BuildVersion = 4134) then
                begin
                  XTL.EmuD3DDeferredRenderState := (DWORD)((DWORD)((uint32)pFunc + $2B) - $248 + 82*4);  // TODO: Verify
                  patchOffset := 142*4 - 82*4;
                end
                else if (BuildVersion = 4361) then
                begin
                  XTL.EmuD3DDeferredRenderState := (DWORD)((DWORD)((uint32)pFunc + $2B) - $200 + 82*4);
                  patchOffset := 142*4 - 82*4;
                end
                else if (BuildVersion = 4432) then
                begin
                  XTL.EmuD3DDeferredRenderState := (DWORD)((DWORD)((uint32)pFunc + $2B) - $204 + 83*4);
                  patchOffset := 143*4 - 83*4;
                end
                else if (BuildVersion = 4627) then
                begin
                  XTL.EmuD3DDeferredRenderState := (DWORD)((DWORD)((uint32)pFunc + $2B) - $24C + 92*4);
                  patchOffset := 162*4 - 92*4;
                end
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

                for (Integer v:=0;v<44;v++)
                  XTL.EmuD3DDeferredRenderState[v] := X_D3DRS_UNK;

                DbgPrintf('HLE: $%.08X . EmuD3DDeferredRenderState', XTL.EmuD3DDeferredRenderState);
              end
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

                  for (Integer s:=0;s<4;s++)
                  begin
                    for (Integer v:=0;v<32;v++)
                      XTL.EmuD3DDeferredTextureState[v+s*32] := X_D3DTSS_UNK;
                  end;

                  DbgPrintf('HLE: $%.08X . EmuD3DDeferredTextureState', XTL.EmuD3DDeferredTextureState);
                end
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

          found := True;

          DbgPrintf('Found');

          EmuInstallWrappers(HLEDataBase[d].OovpaTable, HLEDataBase[d].OovpaTableSize, pXbeHeader);
        end;

        if (not found) then
          DbgPrintf('Skipped');
      end;

      bXRefFirstPass := False;
    end;

    // display Xref summary
    DbgPrintf('HLE: Resolved ' + IntToStr(OrigUnResolvedXRefs - UnResolvedXRefs) + ' cross reference(s)');
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
  RelativeJMPAddress := (IntPtr(WrapperAddr) - IntPtr(FunctionAddr) - 5);
  // Write that after the JMP :
  PCardinal(IntPtr(FunctionAddr) + 1)^ := RelativeJMPAddress;
//  CopyMemory(Pointer(IntPtr(FunctionAddr) + 1), @RelativeJMPAddress, 4);
end;

// install function interception wrappers
procedure EmuInstallWrappers(const pXbeHeader: PXBE_HEADER);
var
  i: Integer;
  DetectedSymbol: TDetectedVersionedXboxLibrarySymbol;
  OrgCode: TCodePointer;
  NewCode: TCodePointer;
  NrPatches: Integer;
{$IFDEF DXBX_DEBUG}
  UsedPatches: TBits;
{$ENDIF}
begin
  DbgPrintf('DxbxHLE : Installing registered patches :');

  NrPatches := 0;
{$IFDEF DXBX_DEBUG}
  UsedPatches := TBits.Create;
  try
    UsedPatches.Size := AvailablePatches.Count + 1;
{$ENDIF}
    for i := 0 to DetectedSymbols.Count - 1 do
    begin
      DetectedSymbol := DetectedSymbols[i];
      if  Assigned(DetectedSymbol.SymbolLocation)
      and (DetectedSymbol.XboxLibraryPatch <> xlp_Unknown) then
      begin
        OrgCode := DetectedSymbol.SymbolLocation;
        NewCode := XboxLibraryPatchToPatch(DetectedSymbol.XboxLibraryPatch);
        Assert(Assigned(NewCode));

  {$IFDEF DXBX_DEBUG}
        DbgPrintf('DxbxHLE : Installed patch from $%.08X (%s) to $%.08X', [
          OrgCode, DetectedSymbol.SymbolName,
          NewCode
          ]);
        UsedPatches[DetectedSymbol.XboxLibraryPatch] := True;
  {$ENDIF}

        EmuInstallWrapper(OrgCode, NewCode);
        Inc(NrPatches);
      end;
    end;

    DbgPrintf('DxbxHLE : Installed patches : %d.', [NrPatches]);

{$IFDEF DXBX_DEBUG}
    DbgPrintf('DxbxHLE : Unused patches : ');
    NrPatches := 0;
    for i := 0 to AvailablePatches.Count - 1 do
    begin
      if not UsedPatches[{XboxLibraryPatch=}(i + 1)] then
      begin
        Inc(NrPatches);
        DbgPrintf('DxbxHLE : Unused patch %.3d : $%.08x (%s{Emu}%s)', [i, Integer(AvailablePatches.Objects[i]), PatchPrefix, AvailablePatches[i]]);
      end;
    end;

    DbgPrintf('DxbxHLE : Unused patches : %d.', [NrPatches]);

  finally
    FreeAndNil(UsedPatches);
  end;
{$ENDIF}
end;

end.
