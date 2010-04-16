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

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  SysUtils,
  Classes, // TBits
  // Jedi Win32API
  JwaWinType,
  // Cxbx
  uTypes,
  uLog,
  uDxbxKrnlUtils,
  uXbe,
  uEmuShared,
  uEmu,
  uEmuFS,
  uHLEDatabase,
  // Dxbx
  uXboxLibraryUtils,
  uStoredTrieTypes,
  DxLibraryAPIScanning,
  uState, // XTL_EmuD3DDeferredRenderState and XTL_EmuD3DDeferredTextureState
  uEmuD3D8Types, // X_D3DTSS_UNK
  uEmuXapi; // XTL_EmuXapiProcessHeap

procedure EmuHLEIntercept(pLibraryVersion: PXBE_LIBRARYVERSION; pXbeHeader: PXBE_HEADER);
procedure EmuInstallWrapper(FunctionAddr: PByte; WrapperAddr: PVOID); inline;
procedure EmuInstallWrappers(const pXbeHeader: PXBE_HEADER);

implementation

procedure EmuHLEIntercept(pLibraryVersion: PXBE_LIBRARYVERSION; pXbeHeader: PXBE_HEADER);
var
//  pCertificate: PXBE_CERTIFICATE;
  CacheFileNameStr: string;
  Symbol: TSymbolInformation;
  s, v: int;
begin
//  pCertificate := PXBE_CERTIFICATE(pXbeHeader.dwCertificateAddr);

  // initialize openxdk emulation (TODO)
  if pLibraryVersion = nil then
  begin
{$IFDEF DXBX_DEBUG}
    DbgPrintf('DxbxHLE: Detected OpenXDK application... cannot patch!');
{$ENDIF}
    Exit;
  end;

  // initialize Microsoft XDK emulation
{$IFDEF DEBUG}
  DbgPrintf('DxbxHLE: Detected Microsoft XDK application...');
{$ENDIF}

  SymbolManager.Clear;

  // Note : Somehow, the output of CacheFileName() changes because of the
  // following code, that's why we put the initial filename in a variable :
  CacheFileNameStr := SymbolManager.CacheFileName(pXbeHeader);

  // Try to load the symbols from the cache :
  if SymbolManager.LoadSymbolsFromCache(CacheFileNameStr) then
    // If that succeeded, we don't have to scan and save anymore :
    CacheFileNameStr := ''
  else
    // No cache found, start out symbol-scanning engine :
    SymbolManager.DxbxScanForLibraryAPIs(pLibraryVersion, pXbeHeader);

  // locate XapiProcessHeap
  begin
    // Resolve the address of the _XapiProcessHeap symbol (at least cross-referenced once, from XapiInitProcess) :
    Symbol := SymbolManager.FindSymbol('_XapiProcessHeap');
    if Assigned(Symbol) then
      // and remember that in a global :
      XTL_EmuXapiProcessHeap := Symbol.Address;

{$IFDEF DXBX_DEBUG}
    if Assigned(XTL_EmuXapiProcessHeap) then
      DbgPrintf('HLE: $%.08X . XapiProcessHeap',
        [XTL_EmuXapiProcessHeap],
        {MayRenderArguments=}False)
    else
      DbgPrintf('HLE : Can''t find XapiProcessHeap!',
        [],
        {MayRenderArguments=}False);
{$ENDIF}
  end;

  // locate D3DDeferredRenderState
  begin
    XTL_EmuD3DDeferredRenderState := nil;
    // First option; Just search for _D3D__RenderState itself !
    Symbol := SymbolManager.FindSymbol('_D3D__RenderState');
    if Assigned(Symbol) then
      XTL_EmuD3DDeferredRenderState := Symbol.Address
    else
    begin
      // Second option (Cxbx does this); Search for 'D3DDevice_SetRenderState_CullMode', and offset from that :
      Symbol := SymbolManager.FindSymbol('D3DDevice_SetRenderState_CullMode');
//      Pointer pFunc := nil;
//      if (BuildVersion = 3925) then
//        pFunc := EmuLocateFunction((OOVPA)@IDirect3DDevice8_SetRenderState_CullMode_1_0_3925, lower, upper);
//      else if (BuildVersion < 5558) then
//        pFunc := EmuLocateFunction((OOVPA)@IDirect3DDevice8_SetRenderState_CullMode_1_0_4134, lower, upper);
//      else
//        pFunc := EmuLocateFunction((OOVPA)@IDirect3DDevice8_SetRenderState_CullMode_1_0_5558, lower, upper);
    end;

    if Assigned(XTL_EmuD3DDeferredRenderState) then
    begin
(*
      // offset for stencil cull enable render state in the deferred render state buffer
      Integer patchOffset := 0;

      if (BuildVersion = 3925) then
      begin
        XTL_EmuD3DDeferredRenderState := (DWORD)((DWORD)((uint32)pFunc + $25) - $19F + 72*4);  // TODO: Clean up (?)
        patchOffset := 142*4 - 72*4; // TODO: Verify
      end
      else if (BuildVersion = 4134) then
      begin
        XTL_EmuD3DDeferredRenderState := (DWORD)((DWORD)((uint32)pFunc + $2B) - $248 + 82*4);  // TODO: Verify
        patchOffset := 142*4 - 82*4;
      end
      else if (BuildVersion = 4361) then
      begin
        XTL_EmuD3DDeferredRenderState := (DWORD)((DWORD)((uint32)pFunc + $2B) - $200 + 82*4);
        patchOffset := 142*4 - 82*4;
      end
      else if (BuildVersion = 4432) then
      begin
        XTL_EmuD3DDeferredRenderState := (DWORD)((DWORD)((uint32)pFunc + $2B) - $204 + 83*4);
        patchOffset := 143*4 - 83*4;
      end
      else if (BuildVersion = 4627) then
      begin
        XTL_EmuD3DDeferredRenderState := (DWORD)((DWORD)((uint32)pFunc + $2B) - $24C + 92*4);
        patchOffset := 162*4 - 92*4;
      end
      else if (BuildVersion = 5558 or BuildVersion = 5849) then
      begin
        // WARNING: Not thoroughly tested (just seemed very correct right away)
        XTL_EmuD3DDeferredRenderState := (DWORD)((DWORD)((uint32)pFunc + $2B) - $24C + 92*4);
        patchOffset := 162*4 - 92*4;
      end;

      XRefDataBase[XREF_D3DDEVICE]                   := *(DWORD)((DWORD)pFunc + $03);
      XRefDataBase[XREF_D3DRS_STENCILCULLENABLE]     := (uint32)XTL_EmuD3DDeferredRenderState + patchOffset + 0*4;
      XRefDataBase[XREF_D3DRS_ROPZCMPALWAYSREAD]     := (uint32)XTL_EmuD3DDeferredRenderState + patchOffset + 1*4;
      XRefDataBase[XREF_D3DRS_ROPZREAD]              := (uint32)XTL_EmuD3DDeferredRenderState + patchOffset + 2*4;
      XRefDataBase[XREF_D3DRS_DONOTCULLUNCOMPRESSED] := (uint32)XTL_EmuD3DDeferredRenderState + patchOffset + 3*4;
*)

      for v :=0 to 44 -1 do
        XTL_EmuD3DDeferredRenderState[v] := X_D3DRS_UNK;

{$IFDEF DEBUG}
      DbgPrintf('HLE: $%.08X . EmuD3DDeferredRenderState', [XTL_EmuD3DDeferredRenderState]);
{$ENDIF}
    end
    else
      EmuWarning('EmuD3DDeferredRenderState was not found!');
  end;

  // locate D3DDeferredTextureState
  begin
    XTL_EmuD3DDeferredTextureState := nil;
    // First option; Just search for _D3D__TextureState itself !
    Symbol := SymbolManager.FindSymbol('_D3D__TextureState');
    if Assigned(Symbol) then
      XTL_EmuD3DDeferredTextureState := Symbol.Address
    else
    begin
      // Second option (Cxbx does this); Search for 'TexCoordIndex', and offset that :
      Symbol := SymbolManager.FindSymbol('D3DDevice_SetTextureState_TexCoordIndex');
//    pFunc := 0;
//    if (BuildVersion = 3925) then
//      pFunc := EmuLocateFunction((OOVPA)@IDirect3DDevice8_SetTextureState_TexCoordIndex_1_0_3925, lower, upper);
//    else if (BuildVersion = 4134) then
//      pFunc := EmuLocateFunction((OOVPA)@IDirect3DDevice8_SetTextureState_TexCoordIndex_1_0_4134, lower, upper);
//    else if (BuildVersion = 4361 or BuildVersion = 4432) then
//      pFunc := EmuLocateFunction((OOVPA)@IDirect3DDevice8_SetTextureState_TexCoordIndex_1_0_4361, lower, upper);
//    else if (BuildVersion = 4627 or BuildVersion = 5558 or BuildVersion = 5849) then
//      pFunc := EmuLocateFunction((OOVPA)@IDirect3DDevice8_SetTextureState_TexCoordIndex_1_0_4627, lower, upper);
      if Assigned(Symbol) then
      begin
//      if (BuildVersion = 3925) then  // 0x18F180
//        XTL_EmuD3DDeferredTextureState := (DWORD)((DWORD)((uint32)pFunc + $11) - $70); // TODO: Verify
//      else if (BuildVersion = 4134) then
//        XTL_EmuD3DDeferredTextureState := (DWORD)((DWORD)((uint32)pFunc + $18) - $70); // TODO: Verify
//      else
//        XTL_EmuD3DDeferredTextureState := (DWORD)((DWORD)((uint32)pFunc + $19) - $70);
      end;
    end;

    if Assigned(XTL_EmuD3DDeferredTextureState) then
    begin
      for s := 0 to 4-1 do
      begin
        for v := 0 to 32-1 do
          XTL_EmuD3DDeferredTextureState[v+s*32] := X_D3DTSS_UNK;
      end;

      DbgPrintf('HLE: $%.08X . EmuD3DDeferredTextureState',
        [XTL_EmuD3DDeferredTextureState],
        {MayRenderArguments=}False);
    end
    else
      EmuWarning('EmuD3DDeferredTextureState was not found!');
  end;

  // After detection of all symbols, see if we need to save that to cache :
  if CacheFileNameStr <> '' then
    SymbolManager.SaveSymbolsToCache(CacheFileNameStr);

  // Now that the symbols are known, patch them up where needed :
  EmuInstallWrappers(pXbeHeader);
end;

// install function interception wrapper
procedure EmuInstallWrapper(FunctionAddr: PByte; WrapperAddr: PVOID); inline;
var
  RelativeJMPAddress: UInt32;
begin
  // Write JMP rel16 opcode (Jump near, displacement relative to next instruction) :
  FunctionAddr^ := $E9;
  // Calculate relative address :
  RelativeJMPAddress := (UIntPtr(WrapperAddr) - UIntPtr(FunctionAddr) - 5);
  // Write that after the JMP :
  PCardinal(UIntPtr(FunctionAddr) + 1)^ := RelativeJMPAddress;
//  CopyMemory(Pointer(IntPtr(FunctionAddr) + 1), @RelativeJMPAddress, 4);
end;

// install function interception wrappers
procedure EmuInstallWrappers(const pXbeHeader: PXBE_HEADER);
var
  i: Integer;
  DetectedSymbol: TSymbolInformation;
  OrgCode: TCodePointer;
  NewCode: TCodePointer;
  NrPatches: Integer;
  XboxLibraryPatch: TXboxLibraryPatch;
{$IFDEF DXBX_DEBUG}
  UsedPatches: TBits;
{$ENDIF}
begin
  NrPatches := 0;

{$IFDEF DXBX_DEBUG}
  DbgPrintf('DxbxHLE : Installing registered patches :');
  UsedPatches := TBits.Create;
  try
    UsedPatches.Size := AvailablePatches.Count + 1;
{$ENDIF}

    for i := 0 to SymbolManager.Count - 1 do
    begin
      DetectedSymbol := SymbolManager.Locations[i];
      OrgCode := DetectedSymbol.Address;
      if not Assigned(OrgCode) then
        continue;

      XboxLibraryPatch := XboxFunctionNameToLibraryPatch(DetectedSymbol.Name);
      if XboxLibraryPatch = xlp_Unknown then
        continue;

      NewCode := XboxLibraryPatchToPatch(XboxLibraryPatch);
      Assert(Assigned(NewCode));

{$IFDEF DXBX_DEBUG}
      DbgPrintf('DxbxHLE : Installed patch over $%.08X (to %s)', [
        OrgCode, DetectedSymbol.Name], {MayRenderArguments=}False);
      UsedPatches[XboxLibraryPatch] := True;
{$ENDIF}

      EmuInstallWrapper(OrgCode, NewCode);
      Inc(NrPatches);
    end;

{$IFDEF DXBX_DEBUG}
    DbgPrintf('DxbxHLE : Installed patches : %d.', [NrPatches]);
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
