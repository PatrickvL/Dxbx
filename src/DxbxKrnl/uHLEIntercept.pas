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
  uXboxLibraryUtils;

procedure EmuHLEIntercept(pLibraryVersion: PXBE_LIBRARYVERSION; pXbeHeader: PXBEIMAGE_HEADER);
procedure EmuInstallWrapper(FunctionAddr: PByte; WrapperAddr: PVOID); inline;
procedure EmuInstallWrappers(const pXbeHeader: PXBEIMAGE_HEADER);

implementation

uses
  DxLibraryAPIScanning;

procedure EmuHLEIntercept(pLibraryVersion: PXBE_LIBRARYVERSION; pXbeHeader: PXBEIMAGE_HEADER);
 begin
  // initialize openxdk emulation (TODO)
  if pLibraryVersion = nil then
  begin
{$IFDEF DXBX_DEBUG}
    DbgPrintf('DxbxHLE: Detected no libraries at all... cannot patch!');
{$ENDIF}
    Exit;
  end;

  // initialize Microsoft XDK emulation
{$IFDEF DEBUG}
  DbgPrintf('DxbxHLE: Detected Microsoft XDK application...');
{$ENDIF}

  SymbolManager.DxbxScanForLibraryAPIs(pLibraryVersion, pXbeHeader);

  // Now that the symbols are known, patch them up where needed :
  EmuInstallWrappers(pXbeHeader);
end;

// install function interception wrapper
procedure EmuInstallWrapper(FunctionAddr: PByte; WrapperAddr: PVOID); inline;
var
  RelativeJMPAddress: UInt32;
begin
  // Write JMP rel16 opcode (Jump near, displacement relative to next instruction) :
  FunctionAddr^ := OPCODE_JMP;
  Inc(FunctionAddr);
  // Calculate relative address :
  RelativeJMPAddress := (UIntPtr(WrapperAddr) - UIntPtr(FunctionAddr) - 4);
  // Write that after the JMP :
  PUInt32(FunctionAddr)^ := RelativeJMPAddress;
end;

// install function interception wrappers
procedure EmuInstallWrappers(const pXbeHeader: PXBEIMAGE_HEADER);
var
  i, j: Integer;
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
        DetectedSymbol.Name]);
      UsedPatches[XboxLibraryPatch] := True;
{$ENDIF}

      EmuInstallWrapper(OrgCode, NewCode);

      // Fill the remainder of the patched function with "int 3" opcodes,
      // so that it breaks early when patches are placed incorrectly.
      // (Ofcourse, we'll have to fix the symbol-detection too than).
      if Assigned(DetectedSymbol.StoredLibraryFunction) then
      begin
        j :=  DetectedSymbol.StoredLibraryFunction.FunctionLength - 5;
        Inc(PByte(OrgCode), 5);
        while j > 0 do
        begin
          Dec(j);
          PByte(OrgCode)^ := OPCODE_INT3;
          Inc(PByte(OrgCode));
        end;
      end;

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
