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
  Classes,
  ShellAPI,
  SysUtils, // SafeLoadLibrary
  Math, // Min
  // Jedi
  JclWin32,
  JwaNative,
  // Dxbx
  uConsts,
  uTypes,
  uFileSystem, // Drives
  uDxbxUtils,
  uLog,
  uXbe, // PXbeHeader
  uKernelThunk,
  uEmuFile,
  uEmuKrnlXe, // xboxkrnl_XeImageFileName
  uDxbxKrnl;

type _IMAGE_TLS_DIRECTORY = record
    StartAddressOfRawData: DWord;
    EndAddressOfRawData: DWord;
    AddressOfIndex: DWord;
    AddressOfCallbacks: DWord;
    SizeOfZeroFill: DWord;
    Characteristics: DWord;
  end; // size = 24
  TImageTlsDirectory = _IMAGE_TLS_DIRECTORY;
  IMAGE_TLS_DIRECTORY = _IMAGE_TLS_DIRECTORY;
  PImageTlsDirectory = ^TImageTlsDirectory;

procedure PrepareXBoxMemoryMap;

procedure ReinitExeImageHeader;
procedure ReinitXbeImageHeader;

function GetEntryPoint(const aXbeHeader: PXbeHeader): UIntPtr;
function MapAndRunXBE(const aFilePath: string; const aHandle: THandle): Boolean;

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
  NewDosHeader: PImageDosHeader;
  Xbe_lfanew: LongInt;

procedure PrepareXBoxMemoryMap;
var
  ExeNtHeader: PImageNtHeaders32;
  ExeOptionalHeader: PImageOptionalHeader;
  ExeHeaderSize: DWord;

  NewNtHeader: PImageNtHeaders32;
  NewOptionalHeader: PImageOptionalHeader;

  ExeSectionHeader: PImageSectionHeader;
  NewSectionHeader: PImageSectionHeader;
  i: Integer;
  NewSection: Pvoid;

  NewTLSDirectory: PImageTlsDirectory;

  Protection: DWord;
  TLSIndex: DWord;
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
  memcpy(NewDosHeader, ExeDosHeader, ExeHeaderSize);

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
      memcpy(NewSection, Pvoid(ExeSectionHeader.VirtualAddress + UIntPtr(ExeDosHeader)), ExeSectionHeader.Misc.VirtualSize);
      NewSectionHeader.VirtualAddress := UIntPtr(NewSection) - UIntPtr(ExeDosHeader);

      if NewOptionalHeader.BaseOfData = ExeSectionHeader.VirtualAddress then
        NewOptionalHeader.BaseOfData := NewSectionHeader.VirtualAddress;
    end;

    Inc(ExeSectionHeader);
    Inc(NewSectionHeader);
  end;

  NewTLSDirectory := PImageTlsDirectory(UIntPtr(ExeDosHeader) + NewOptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_TLS].VirtualAddress);

  if not VirtualProtect(NewTLSDirectory, SizeOf(TImageTlsDirectory), PAGE_READWRITE, {var}Protection) then
    RaiseLastOSError;

  TLSIndex := PDWord(UIntPtr(ExeDosHeader) + NewTLSDirectory.AddressOfIndex)^;
  NewTLSDirectory.AddressOfIndex := UIntPtr(@TLSIndex) - UIntPtr(ExeDosHeader);

  // Make sure we can write to the complete EXE's memory-range :
  if not VirtualProtect(Pvoid(XBE_IMAGE_BASE), XBOX_MEMORY_SIZE, PAGE_READWRITE, {var}Protection) then
    RaiseLastOSError;

  // Clear out the whole range, overwriting the complete launcher EXE :
  ZeroMemory(ExeDosHeader, XBOX_MEMORY_SIZE);

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
  ExeDosHeader.e_magic := $5842; // 'XB'; (...'EH') Overwrites XbeHeader.dwMagic
  ExeDosHeader._lfanew := Xbe_lfanew;
end;

// Decode entry point address :
function GetEntryPoint(const aXbeHeader: PXbeHeader): UIntPtr;
begin
  Result := aXbeHeader.dwEntryAddr xor XOR_EP_RETAIL;
  if Result > XOR_MAX_VIRTUAL_ADDRESS then
    Result := aXbeHeader.dwEntryAddr xor XOR_EP_DEBUG;
end;

// Load XBE sections in Virtual Memory, and call DxbxKrnlInit (TODO : from a new thread?)
function MapAndRunXBE(const aFilePath: string; const aHandle: THandle): Boolean;
var
  Drive: PLogicalVolume;
  XbeFilePath: string;
  FileHandle: TFileHandle;

  Protection: DWord;
  XbeHeader: PXbeHeader;
  XbeSectionHeader: PXbeSectionHeader;
  i: Integer;
  kt, t: DWord;
  kt_tbl: PDWORDs;
  EntryPoint: UIntPtr;
  XbeTLS: PXbeTLS;
  XbeTlsData: Pvoid;
  XbeLibraryVersion: PXbeLibraryVersion;

  procedure _ReadXbeBlock(aRawOffset, aRawSize, aVirtualAddr, aVirtualSize, aProtection: DWord);
  (*var
    old_protection: DWord; *)
  begin
    Assert(aVirtualSize >= aRawSize);
    if aVirtualSize = 0 then
      Exit;

//    if not VirtualProtect(Pvoid(aVirtualAddr), aVirtualSize, PAGE_READWRITE, {var}old_protection) then
//      RaiseLastOSError;

    if aRawSize > 0 then
    begin
      Drive.FileSystem.Seek(FileHandle, aRawOffset, soFromBeginning);
      Drive.FileSystem.Read(FileHandle, Pvoid(aVirtualAddr)^, aRawSize);
    end;

// Dxbx Note : Restoring the page-protection crashes some xbe's writing to TLS, so skip it for now!
//    if not VirtualProtect(Pvoid(aVirtualAddr), aRawSize, aProtection, {var}old_protection) then
//      RaiseLastOSError;
  end;

begin
  Result := False;
  WriteLog('EmuExe: Loading Sections...');

  // Make sure we can write to the original range of Xbox memory (128 MB, starting at $10000) :
  if not VirtualProtect(Pvoid(XBE_IMAGE_BASE), XBOX_MEMORY_SIZE, PAGE_READWRITE, {var}Protection) then
    RaiseLastOSError;

  // Clear out the whole EXE range :
  ZeroMemory(Pvoid(XBE_IMAGE_BASE), XBOX_MEMORY_SIZE);

  // Read the Xbe using our generic FileSystem implementation, allowing loading from xISO's :
  Drive := Drives.D;
  if not Drive.OpenImage(aFilePath, {out}XbeFilePath) then
  begin
    WriteLog(Format('EmuExe: Could not open path : %s', [aFilePath]));
    Exit;
  end;

  g_Xbe_XbePath := aFilePath;
  FileHandle := Drive.FileSystem.Open(XbeFilePath);
  try
    // Copy the complete XBEHeader over to the ImageBase :
    _ReadXbeBlock(
      {RawOffset=}0,
      {RawSize=}XBE_HEADER_SIZE, // =$1000
      {VirtualAddr=}XBE_IMAGE_BASE,
      {VirtualSize=}XBE_HEADER_SIZE,
      {NewProtect}PAGE_READWRITE);

    // Some XBE's have a header bigger than $1000 (like 'BLiNX:the time sweeper'), read the real size :
    XbeHeader := PXbeHeader(XBE_IMAGE_BASE);
    if XbeHeader.dwSizeofHeaders > XBE_HEADER_SIZE then
      _ReadXbeBlock(
        {RawOffset=}0,
        {RawSize=}XbeHeader.dwSizeofHeaders,
        {VirtualAddr=}XBE_IMAGE_BASE,
        {VirtualSize=}XbeHeader.dwSizeofHeaders,
        {NewProtect}PAGE_READWRITE);

    // Remember the ExeDosHeader._lfanew value, as we're about to overwrite it :
    // (This field can be restored by calling ReinitXbeImageHeader)
    Xbe_lfanew := ExeDosHeader._lfanew;

    // Restore just enough of the EXE header to make Windows API's like CreateThread work again :
    ReinitExeImageHeader;

    // Load all sections to their requested Virtual Address :
    XbeSectionHeader := PXbeSectionHeader(XbeHeader.dwSectionHeadersAddr);
    for i := 0 to XbeHeader.dwSections - 1 do
    begin
      // Determine protection, based on actual section flags :
      Protection := PAGE_READONLY;
      if (XbeSectionHeader.dwFlags[0] and XBE_SECTIONHEADER_FLAG_Writable) > 0 then
        Protection := PAGE_READWRITE;
      if (XbeSectionHeader.dwFlags[0] and XBE_SECTIONHEADER_FLAG_Executable) > 0 then
        Protection := PAGE_EXECUTE_READWRITE;

      _ReadXbeBlock(
        {RawOffset=}XbeSectionHeader.dwRawAddr,
        {RawSize=}XbeSectionHeader.dwSizeofRaw,
        {VirtualAddr=}XbeSectionHeader.dwVirtualAddr,
        {VirtualSize=}XbeSectionHeader.dwVirtualSize,
        {NewProtect}Protection);

      WriteLog(Format('EmuExe: Loading Section 0x%.4x... OK', [i]));
      Inc(XbeSectionHeader);
    end;

  finally
    Drive.FileSystem.Close(FileHandle);
  end;

  // Decode kernel thunk table address :
  begin
    kt := XbeHeader.dwKernelImageThunkAddr xor XOR_KT_RETAIL;
    if kt > XOR_MAX_VIRTUAL_ADDRESS then
      kt := XbeHeader.dwKernelImageThunkAddr xor XOR_KT_DEBUG;
  end;

  // Process the Kernel thunk table to map Kernel function calls to their actual address :
  begin
    WriteLog('EmuExe: Hijacking Kernel Imports...');
    DWord(kt_tbl) := kt;
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
    DWord(kt_tbl) := 0; // Make sure Delphi doesn't automatically deallocate this variable
  end;

  EntryPoint := GetEntryPoint(XbeHeader);

  Result := True;

  // Launch the XBE :
  begin
    XbeTLS := PXbeTls(XbeHeader.dwTLSAddr);
    if Assigned(XbeTLS) then
      XbeTlsData := Pvoid(XbeTLS.dwDataStartAddr)
    else
      XbeTlsData := nil;

    XbeLibraryVersion := PXbeLibraryVersion(XbeHeader.dwLibraryVersionsAddr);

    // Assign the running Xbe path, so it can be accessed via the kernel thunk 'XeImageFileName' :
    g_EmuXbePath := DeviceHarddisk0Partition1 + AnsiString(XbeFilePath);
    RtlInitAnsiString(@xboxkrnl_XeImageFileName, PCSZ(g_EmuXbePath));
    // TODO -oDxbx : Make sure this matches g_hCurDir

    DxbxKrnlInit(
      aHandle,
      XbeTlsData,
      XbeTls,
      XbeLibraryVersion,
      KernelDebugMode,
      PAnsiChar(AnsiString(KernelDebugFileName)),
      XbeHeader,
      XbeHeader.dwSizeofHeaders,
      TEntryProc(EntryPoint));
  end;
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
    // Get DCHandle :
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

    // TODO : Setup LaunchData and Initialize kernel
    // (which will launch the Xbe on itself), instead here.

    // Now we can load and run the XBE :
    MapAndRunXBE(XbePath, DCHandle);
  end;
  
  // Prevent the process from returning to the overwritten EXE location
  // we started out from :
  Halt(0);
end;

end.
