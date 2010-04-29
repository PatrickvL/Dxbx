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
  Inc(FunctionAddr);
  // Calculate relative address :
  RelativeJMPAddress := (UIntPtr(WrapperAddr) - UIntPtr(FunctionAddr) - 4);
  // Write that after the JMP :
  PUInt32(FunctionAddr)^ := RelativeJMPAddress;
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
      DbgPrintf('HLE: 0x%.08X -> %s (patching %s)', [
        OrgCode, AvailablePatches[XboxLibraryPatch - 1],
        DetectedSymbol.Name], {MayRenderArguments=}False);
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
        DbgPrintf('DxbxHLE : Unused patch %.3d : %s{Emu}%s', [i, PatchPrefix, AvailablePatches[i]]);
      end;
    end;

    DbgPrintf('DxbxHLE : Unused patches : %d.', [NrPatches]);

  finally
    FreeAndNil(UsedPatches);
  end;
{$ENDIF}
end;

end.
