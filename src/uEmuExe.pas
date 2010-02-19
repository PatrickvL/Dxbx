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
unit uEmuExe;

{$INCLUDE Dxbx.inc}

{$OVERFLOWCHECKS OFF}

interface

uses
  // Delphi
  Windows,
  ShellAPI,
  SysUtils, // SafeLoadLibrary
  Math, // Min
  // Jedi
  JclWin32,
  // Dxbx
  uConsts,
  uTypes,
  uDxbxUtils,
  uLog,
  uXbe,
  uKernelThunk,
  uDxbxKrnl;

type
  PImageTlsDirectory = ^TImageTlsDirectory;
  _IMAGE_TLS_DIRECTORY = record
    StartAddressOfRawData: DWord;
    EndAddressOfRawData: DWord;
    AddressOfIndex: DWord;
    AddressOfCallbacks: DWord;
    SizeOfZeroFill: DWord;
    Characteristics: DWord;
  end;
  TImageTlsDirectory = _IMAGE_TLS_DIRECTORY;
  IMAGE_TLS_DIRECTORY = _IMAGE_TLS_DIRECTORY;

procedure PrepareXBoxMemoryMap;

procedure ReinitExeImageHeader;
procedure ReinitXbeImageHeader;

function MapAndRunXBE(const aXbe: TXbe; const aHandle: THandle): Boolean;

procedure DxbxMain(const aData: MathPtr; const aSize: DWord); stdcall;

exports
  DxbxMain;
  
var
  pwCommandLine: PWideChar;
  CommandLine: WideString;

  KernelDebugMode: TDebugMode = dmFile;
  KernelDebugFileName: WideString = '';

implementation

var
  ExeDosHeader: PImageDosHeader;
  ExeNtHeader: PImageNtHeaders32;
  ExeOptionalHeader: PImageOptionalHeader;
  ExeHeaderSize: DWord;

  NewDosHeader: PImageDosHeader;
  NewNtHeader: PImageNtHeaders32;
  NewOptionalHeader: PImageOptionalHeader;
  NewTLSDirectory: PImageTlsDirectory;
  TLSIndex: DWord;

procedure PrepareXBoxMemoryMap;
var
  ExeSectionHeader: PImageSectionHeader;
  NewSectionHeader: PImageSectionHeader;
  i: Integer;
  NewSection: Pointer;
  Protection: DWord;
begin
  // Determine EXE's header locations & size :
  ExeDosHeader := PImageDosHeader(XBE_IMAGE_BASE); // = $10000
  ExeNtHeader := PImageNtHeaders32(IntPtr(ExeDosHeader) + ExeDosHeader._lfanew); // = + $100
  ExeOptionalHeader := @(ExeNtHeader.OptionalHeader);
  ExeHeaderSize := ExeOptionalHeader.SizeOfHeaders; // Should end up as $400

  // Create a safe copy of the complete EXE header :
  NewDosHeader := PImageDosHeader(AllocMem(ExeHeaderSize));
  NewNtHeader := PImageNtHeaders32(IntPtr(NewDosHeader) + ExeDosHeader._lfanew);
  NewOptionalHeader := @(NewNtHeader.OptionalHeader);
  Move(ExeDosHeader^, NewDosHeader^, ExeHeaderSize);

  // Make sure the new DOS header points to the new relative NtHeader location :
  NewDosHeader._lfanew := UIntPtr(NewNtHeader) - UIntPtr(XBE_IMAGE_BASE);

  // Create safe copies of all EXE sections that could get overwritten :
  ExeSectionHeader := PImageSectionHeader(UIntPtr(ExeOptionalHeader) + ExeNtHeader.FileHeader.SizeOfOptionalHeader);
  NewSectionHeader := PImageSectionHeader(UIntPtr(NewOptionalHeader) + NewNtHeader.FileHeader.SizeOfOptionalHeader);
  for i := 0 to ExeNtHeader.FileHeader.NumberOfSections - 1 do
  begin
    // Check if this section will be overwritten :
    if ExeSectionHeader.VirtualAddress + ExeSectionHeader.Misc.VirtualSize < XBOX_MEMORY_SIZE then
    begin
      // Allocate a new copy of this section :
      NewSection := AllocMem(ExeSectionHeader.Misc.VirtualSize);
      Move(Pointer(ExeSectionHeader.VirtualAddress + UIntPtr(ExeDosHeader))^, NewSection^, ExeSectionHeader.Misc.VirtualSize);
      NewSectionHeader.VirtualAddress := UIntPtr(NewSection) - UIntPtr(ExeDosHeader);

      if NewOptionalHeader.BaseOfData = ExeSectionHeader.VirtualAddress then
        NewOptionalHeader.BaseOfData := NewSectionHeader.VirtualAddress;
    end;

    Inc(ExeSectionHeader);
    Inc(NewSectionHeader);
  end;

  NewTLSDirectory := PImageTlsDirectory(UIntPtr(ExeDosHeader) + NewNtHeader.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_TLS].VirtualAddress);

  if not VirtualProtect(NewTLSDirectory, SizeOf(TImageTlsDirectory), PAGE_READWRITE, {var}Protection) then
    RaiseLastOSError;

  TLSIndex := PDWord(UIntPtr(ExeDosHeader) + NewTLSDirectory.AddressOfIndex)^;
  NewTLSDirectory.AddressOfIndex := UIntPtr(@TLSIndex) - UIntPtr(ExeDosHeader);

  // Make sure we can write to the complete EXE's memory-range :
  if not VirtualProtect(ExeDosHeader, XBOX_MEMORY_SIZE, PAGE_READWRITE, {var}Protection) then
    RaiseLastOSError;

  // Clear out the whole range, overwriting the complete launcher EXE :
//  ZeroMemory(ExeDosHeader, XBOX_MEMORY_SIZE);

  // Restore just as much of the headers to keep WinAPI's working :
  ReinitExeImageHeader;
end;

procedure ReinitExeImageHeader;
begin
  ExeDosHeader.e_magic := $5A4D; // 'MZ'; Overwrites XbeHeader.dwMagic
  ExeDosHeader._lfanew := NewDosHeader._lfanew; // Overwrites XbeHeader.pbDigitalSignature
end;

procedure ReinitXbeImageHeader;
begin
  // Dxbx TODO
(*
  ExeDosHeader.e_magic := $....; // 'XB'; (...'EH') Overwrites XbeHeader.dwMagic
  ExeDosHeader._lfanew := NewDosHeader._lfanew; // Overwrites XbeHeader.pbDigitalSignature
*)
end;

// Load XBE sections in Virtual Memory, and call CxbxKrnlInit (from a new thread?)
function MapAndRunXBE(const aXbe: TXbe; const aHandle: THandle): Boolean;
var
  Protection: DWord;
  XbeHeader: PXbeHeader;
  XbeSectionHeader: PXbeSectionHeader;
  i: Integer;
  kt, t: DWord;
  kt_tbl: PDWordArray;
  EntryPoint: DWord;
  XbeTLS: PXbeTLS;

  procedure _CopyBlock(const aRawOffset, aRawSize, aVirtualAddr, aProtection: DWord);
  var
    old_protection: DWord;
  begin
    if not VirtualProtect(Pointer(aVirtualAddr), aRawSize, PAGE_READWRITE, {var}old_protection) then
      RaiseLastOSError;

    Move(aXbe.RawData[aRawOffset], Pointer(aVirtualAddr)^, aRawSize);
    if not VirtualProtect(Pointer(aVirtualAddr), aRawSize, aProtection, {var}old_protection) then
      RaiseLastOSError;
  end;

begin
  WriteLog('EmuExe: Loading Sections...');

  if not VirtualProtect(Pointer(XBE_IMAGE_BASE), XBOX_MEMORY_SIZE, PAGE_READWRITE, {var}Protection) then
    RaiseLastOSError;

  // Clear out the whole EXE range :
  ZeroMemory(Pointer(XBE_IMAGE_BASE), XBOX_MEMORY_SIZE);

  // Copy the complete XBEHeader over to the ImageBase :
  _CopyBlock(
    {RawOffset=}0,
    {RawSize=}XBE_HEADER_SIZE,
    {VirtualAddr=}XBE_IMAGE_BASE,
    {NewProtect}PAGE_READWRITE);

  // Restore just enough of the EXE header to make Windows API's like BeginThread work again :
  ReinitExeImageHeader;

  // Copy all sections to their requested Virtual Address :
  XbeHeader := PXbeHeader(@aXbe.m_Header);
  XbeSectionHeader := PXbeSectionHeader(@(aXbe.m_SectionHeader[0]));
  for i := 0 to XbeHeader.dwSections - 1 do
  begin
    // Determine protection, based on actual section flags :
    Protection := PAGE_READONLY;
    if (XbeSectionHeader.dwFlags[0] and XBE_SECTIONHEADER_FLAG_Writable) > 0 then
      Protection := PAGE_READWRITE;
    if (XbeSectionHeader.dwFlags[0] and XBE_SECTIONHEADER_FLAG_Executable) > 0 then
      Protection := PAGE_EXECUTE_READWRITE;

    _CopyBlock(
      {RawOffset=}XbeSectionHeader.dwRawAddr,
      {RawSize=}XbeSectionHeader.dwSizeofRaw,
      {VirtualAddr=}XbeSectionHeader.dwVirtualAddr,
      {NewProtect}Protection);

    WriteLog(Format('EmuExe: Loading Section 0x%.4x... OK', [i]));
    Inc(XbeSectionHeader);
  end;

  // Decode kernel thunk table address :
  kt := XbeHeader.dwKernelImageThunkAddr xor XOR_KT_RETAIL;
  if kt > XOR_MAX_VIRTUAL_ADDRESS then
    kt := XbeHeader.dwKernelImageThunkAddr xor XOR_KT_DEBUG;

  // Patch the Kernel Thunk Table, to use the actual functions :
  WriteLog('EmuExe: Hijacking Kernel Imports...');
  kt_tbl := Pointer(kt);
  i := 0;
  while kt_tbl[i] <> 0 do
  begin
    t := kt_tbl[i] and $7FFFFFFF;
    if  (t < NUMBER_OF_THUNKS)
    and (DWord(KernelThunkTable[t]) > NUMBER_OF_THUNKS) then
    begin
      kt_tbl[i] := DWord(KernelThunkTable[t]);
      WriteLog(Format('EmuExe: Thunk %.3d : *0x%.8X := 0x%.8X', [t, kt + DWord(i * 4), kt_tbl[i]]));
    end
    else
      WriteLog(Format('EmuExe: Out-of-range thunk %.3d : *0x%.8X := 0x%.8X', [t, kt + DWord(i * 4), kt_tbl[i]]));

    Inc(i);
  end; // while

  // Decode entry point address :
  EntryPoint := XbeHeader.dwEntryAddr xor XOR_EP_RETAIL;
  if EntryPoint > XOR_MAX_VIRTUAL_ADDRESS then
    EntryPoint := XbeHeader.dwEntryAddr xor XOR_EP_DEBUG;

  Result := True;

  XbeTLS := PXbeTls(XbeHeader.dwTLSAddr);
//  XbeTLS := aXbe.m_TLS;

  // Launch the XBE :
  CxbxKrnlInit(
    aHandle,
    Pointer(XbeTLS.dwDataStartAddr),
    XbeTls,
    PXbeLibraryVersion(@(aXbe.m_LibraryVersion[0])),
    KernelDebugMode,
    PAnsiChar(AnsiString(KernelDebugFileName)),
    XbeHeader,
    XbeHeader.dwSizeofHeaders,
    TEntryProc(EntryPoint));
end;

var
  Params: PPWideChar;
  ParamCount: Integer;
  
procedure DxbxMain(const aData: MathPtr; const aSize: DWord); stdcall;
var
  XBEPath: WideString;
  DCHandle: THandle;
begin
  Assert(GetModuleHandle(nil) = XBE_IMAGE_BASE);
  Assert(aSize >= XBOX_MEMORY_SIZE);

  pwCommandLine := GetCommandLineW();
  CommandLine := WideString(pwCommandLine);

  // Split CommandLine up, just like ParamStr()
  Params := CommandLineToArgvW(pwCommandLine, {var}ParamCount);
  if ParamCount >= 3 then
  begin
    // Skip exename :
    Inc(Params);
    // Skip '/load' switch :
    Inc(Params);
    // Get XBE Name :
    XBEPath := WideString(Params^);
    Inc(Params);
    // Get DCHandle ;
    DCHandle := StrToIntDef(WideString(Params^), 0);
    Inc(Params);
    // Get KernelDebugMode :
    KernelDebugMode := TDebugMode(StrToIntDef(WideString(Params^), Ord(dmFile)));
    Inc(Params);
    // Get KernelDebugFileName :
    KernelDebugFileName := WideString(Params^);
    Inc(Params);

    // Now we got the arguments, start by initializing the Xbox memory map :
    PrepareXBoxMemoryMap;

    // Now we can load and run the XBE :
    MapAndRunXBE(TXbe.Create(XbePath), DCHandle);
  end;
  
  // Prevent the process from returning to the overwritten EXE location
  // we started out from :
  Halt(0);
end;

end.
