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

interface

uses
  // Delphi
  Windows, SysUtils,
  // Dxbx
  uConsts, uTypes, uBitsOps, uProlog, uXbe, uExe;

type
  TDWordArray = array[0..3] of Byte;
  TWordArray = array[0..1] of Byte;

  TEmuExe = class(TExe)
  protected
    KrnlHandle: THandle;
  public
    constructor Create(m_Xbe: TXbe; m_KrnlDebug: DebugMode; m_KrnlDebugFilename: string; hwndParent: THandle);
    destructor Destroy; override;
  end;


implementation

uses
  // Dxbx
  uLog, uExternals;

const
{$IFDEF DEBUG}
  UseDebugDLL = True;
{$ELSE}
  UseDebugDLL = False;
{$ENDIF}

  StrCxbxKrnlNoFunc00CxbxKrnl_dll = 'CxbxKrnlNoFunc'#0#0'CxbxKrnl.dll';
  StrCxbxKrnlNoFunc00Cxbx_dll = 'CxbxKrnlNoFunc'#0#0'Cxbx.dll';

{ TEmuExe }

//------------------------------------------------------------------------------

constructor TEmuExe.Create(m_Xbe: TXbe; m_KrnlDebug: DebugMode;
  m_KrnlDebugFilename: string; hwndParent: THandle);

  procedure _WriteDWordToAddr(const aAddr: Pointer; aDWord: DWord);
  begin
    CopyMemory(aAddr, @aDWord, SizeOf(DWord));
  end;

  procedure _WriteDWordToSectionPos(SectionIdx, iPos: Integer; aDWord: DWord);
//  var
//    i: Integer;
//    DWordArray: TDWordArray;
  begin
//    DWordArray := TDWordArray(aDWord);
    CopyMemory(@(m_bzSection[SectionIdx][iPos]), @aDWord, SizeOf(DWord));
//    for i := 0 to Length(DWordArray) - 1 do
//    begin
//      m_bzSection[SectionIdx][iPos] := DWordArray[i];
//      Inc(iPos);
//    end;
  end;

(*
  procedure _AppendProlog(SectionIdx: Integer);
  var
    iPos: Integer;
  begin
    // Append prolog section
    iPos := Length(m_bzSection[SectionIdx]);
    SetLength(m_bzSection[SectionIdx], iPos + Length(Prolog));
    CopyMemory(@(m_bzSection[SectionIdx][iPos]), @(Prolog[0]), Length(Prolog));
  end;

  procedure _AppenddwMagic(SectionIdx: Integer);
  var
    i: Integer;
    iPos: Integer;
  begin
    // Append dwMagic to section
    iPos := Length(m_bzSection[SectionIdx]);
    SetLength(m_bzSection[SectionIdx], Length(m_bzSection[SectionIdx]) + Length(m_xbe.m_Header.dwMagic));
    for i := 0 to Length(m_xbe.m_Header.dwMagic) - 1 do
    begin
      m_bzSection[SectionIdx][iPos] := Byte(m_xbe.m_Header.dwMagic[i]);
      Inc(iPos);
    end;
  end;

  procedure _AppenddwMagicSubI(SectionIdx: Integer; iPos: Dword);
  var
    i: Integer;
  begin
    // Append dwMagic to section
    for i := 0 to Length(m_xbe.m_Header.dwMagic) - 1 do
    begin
      m_bzSection[SectionIdx][iPos] := Byte(m_xbe.m_Header.dwMagic[i]);
      Inc(iPos);
    end;
  end;

  procedure _AppendpbDigitalSignature(SectionIdx: Integer);
  var
    i: Integer;
    iPos: Integer;
  begin
    // Append pbDigitalSignature to section
    iPos := Length(m_bzSection[SectionIdx]);
    SetLength(m_bzSection[SectionIdx], Length(m_bzSection[SectionIdx]) + Length(m_Xbe.m_Header.pbDigitalSignature));
    for i := 0 to Length(m_Xbe.m_Header.pbDigitalSignature) - 1 do
    begin
      m_bzSection[SectionIdx][iPos] := m_Xbe.m_Header.pbDigitalSignature[i];
      Inc(iPos);
    end;
  end;

  procedure _AppendpbDigitalSignatureSubI(SectionIdx: Integer; iPos: Dword);
  var
    i: Integer;
  begin
    // Append pbDigitalSignature to section
    for i := 0 to Length(m_Xbe.m_Header.pbDigitalSignature) - 1 do
    begin
      m_bzSection[SectionIdx][iPos] := m_Xbe.m_Header.pbDigitalSignature[i];
      Inc(iPos);
    end;
  end;

  procedure _AppenddwInitFlags(SectionIdx: Integer);
  var
    iPos: Integer;
  begin
    // Append dwInitFlags to section
    iPos := Length(m_bzSection[SectionIdx]);
    SetLength(m_bzSection[SectionIdx], Length(m_bzSection[SectionIdx]) + SizeOf(m_XBe.m_Header.dwInitFlags));
    Move(m_XBe.m_Header.dwInitFlags[0], m_bzSection[SectionIdx][iPos], SizeOf(m_XBe.m_Header.dwInitFlags));
  end;
*)

  procedure _CopySections(Index: Integer);
  begin
    SetLength(m_bzSection[Index], m_SectionHeader[Index].m_sizeof_raw);
    CopyMemory(m_bzSection[Index], m_Xbe.m_bzSection[Index], m_Xbe.m_SectionHeader[Index].dwSizeofRaw);
  end;

(*
  procedure _AppenddwInitFlagsSubI(SectionIdx: Integer; iPos: Dword);
  begin
    CopyMemory(@(m_bzSection[SectionIdx][iPos]), @(m_XBe.m_Header.dwInitFlags[0]), SizeOf(m_XBe.m_Header.dwInitFlags));
  end;

  procedure _AppendXbeHeader(SectionIdx: Integer; SubIndex: Dword);
  begin
    _AppenddwMagicSubI(SectionIdx, $100); // Append dwMagic
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwMagic);
    _AppendpbDigitalSignatureSubI(SectionIdx, SubIndex); // Append Digital signature
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.pbDigitalSignature);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwBaseAddr); // Append DwBaseAddr
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwBaseAddr);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwSizeofHeaders); // Append dwSizeofHeaders
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwSizeofHeaders);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwSizeofImage); // Append dwSizeofImage
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwSizeofImage);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwSizeofImageHeader); // Append dwSizeofImageHeader
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwSizeofImageHeader);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwTimeDate); // Append dwTimeDate
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwTimeDate);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwCertificateAddr); // Append dwCertificateAddr
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwCertificateAddr);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwSections); // Append dwSections
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwSections);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwSectionHeadersAddr); // Append dwSectionHeadersAddr
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwSectionHeadersAddr);
    _AppenddwInitFlagsSubI(SectionIdx, SubIndex); // Append dwInitFlags
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwInitFlags);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwEntryAddr); // Append dwEntryAddr
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwEntryAddr);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwTLSAddr); // Append dwTLSAddr
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwTLSAddr);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwPeStackCommit); // Append dwPeStackCommit
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwPeStackCommit);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwPeHeapReserve); // Append dwPeHeapReserve
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwPeHeapReserve);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwPeHeapCommit); // Append dwPeHeapCommit
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwPeHeapReserve);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwPeBaseAddr); // Append dwPeBaseAddr
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwPeBaseAddr);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwPeSizeofImage); // Append dwPeSizeofImage
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwPeSizeofImage);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwPeChecksum); // Append dwPeChecksum
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwPeChecksum);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwPeTimeDate); // Append dwPeTimeDate
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwPeTimeDate);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwDebugPathnameAddr); // Append dwDebugPathNameAddr
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwDebugPathnameAddr);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwDebugFilenameAddr); // Append dwDebugFileNameAddr
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwDebugFilenameAddr);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwDebugUnicodeFilenameAddr); // Append dwDebugUnicodeFileNameAddr
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwDebugUnicodeFilenameAddr);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwKernelImageThunkAddr); // Append deKernelImageThunkAddr
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwKernelImageThunkAddr);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwNonKernelImportDirAddr); // Append dwNonKernelImportDirAddr
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwNonKernelImportDirAddr);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwLibraryVersions); // Append dwLibraryVersions
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwLibraryVersions);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwLibraryVersionsAddr); // Append dwLibraryVersionsAddr
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwLibraryVersionsAddr);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwKernelLibraryVersionAddr); // Append dwKernelLibraryVersionAddr
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwKernelLibraryVersionAddr);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwXAPILibraryVersionAddr); // Append dwXapiLibraryVersionAddr
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwXAPILibraryVersionAddr);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwLogoBitmapAddr); // Append dwLogoBitmapAddr
    SubIndex := SubIndex + SizeOf(m_XBe.m_Header.dwLogoBitmapAddr);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_XBe.m_Header.dwSizeofLogoBitmap); // Append dwSizeofLogoBitmap
  end;
*)

(*
  procedure _AppendXbeLibVersionSzName(SectionIdx: Integer; iPos: Dword; LibVersioNbr: Integer);
  var
    i: Integer;
  begin
    // Append pbDigitalSignature to section
    for i := 0 to 7 do
    begin
      m_bzSection[SectionIdx][iPos] := Byte(m_Xbe.m_LibraryVersion[LibVersioNbr].szName[i]);
      Inc(iPos);
    end;
  end;

  procedure _AppenddwLibVerFlagsSubI(SectionIdx: Integer; iPos: Dword; LibVersioNbr: Integer);
  var
    i: Integer;
  begin
    for i := 0 to Length(m_Xbe.m_LibraryVersion[LibVersioNbr].dwFlags) - 1 do
    begin
      m_bzSection[SectionIdx][iPos] := m_Xbe.m_LibraryVersion[LibVersioNbr].dwFlags[i];
      Inc(iPos);
    end;
  end;

  procedure _AppendWordToSubSection(SectionIdx, iPos: Integer; aWord: Word);
  var
    i: Integer;
    WordArray: TWordArray;
  begin
    WordArray := TWordArray(aWord);
    for i := 0 to Length(WordArray) - 1 do
    begin
      m_bzSection[SectionIdx][iPos] := WordArray[i];
      Inc(iPos);
    end;
  end;

  procedure _AppendXbeLibVersion(SectionIdx: Integer; SubIndex: Dword; LibVersioNbr: Integer);
  begin
    _AppendXbeLibVersionSzName(SectionIdx, SubIndex, LibVersioNbr);
    SubIndex := SubIndex + SizeOf(m_Xbe.m_LibraryVersion[LibVersioNbr].szName);
    _AppendWordToSubSection(SectionIdx, SubIndex, m_Xbe.m_LibraryVersion[LibVersioNbr].wMajorVersion);
    SubIndex := SubIndex + SizeOf(m_Xbe.m_LibraryVersion[LibVersioNbr].wMajorVersion);
    _AppendWordToSubSection(SectionIdx, SubIndex, m_Xbe.m_LibraryVersion[LibVersioNbr].wMinorVersion);
    SubIndex := SubIndex + SizeOf(m_Xbe.m_LibraryVersion[LibVersioNbr].wMinorVersion);
    _AppendWordToSubSection(SectionIdx, SubIndex, m_Xbe.m_LibraryVersion[LibVersioNbr].wBuildVersion);
    SubIndex := SubIndex + SizeOf(m_Xbe.m_LibraryVersion[LibVersioNbr].wBuildVersion);
    _AppenddwLibVerFlagsSubI(SectionIdx, SubIndex, LibVersioNbr);
  end;
*)

(*
  procedure _AppendTLS(SectionIdx: Integer; SubIndex: Dword);
  begin
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_Xbe.m_TLS.dwDataStartAddr);
    SubIndex := SubIndex + SizeOf(m_Xbe.m_TLS.dwDataStartAddr);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_Xbe.m_TLS.dwDataEndAddr);
    SubIndex := SubIndex + SizeOf(m_Xbe.m_TLS.dwDataEndAddr);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_Xbe.m_TLS.dwTLSIndexAddr);
    SubIndex := SubIndex + SizeOf(m_Xbe.m_TLS.dwTLSIndexAddr);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_Xbe.m_TLS.dwTLSCallbackAddr);
    SubIndex := SubIndex + SizeOf(m_Xbe.m_TLS.dwTLSCallbackAddr);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_Xbe.m_TLS.dwSizeofZeroFill);
    SubIndex := SubIndex + SizeOf(m_Xbe.m_TLS.dwSizeofZeroFill);
    _WriteDWordToSectionPos(SectionIdx, SubIndex, m_Xbe.m_TLS.dwCharacteristics);
  end;
*)

(*
  procedure _AppendTLSData(SectionIdx: Integer; SubIndex: Dword);
  var
    i: DWord;
    pTLSData: ^TVarByteArray;
  begin
    if m_Xbe.GetTLSData <> 0 then
    begin
      pTLSData := Pointer(m_Xbe.GetTLSData);
      for i := 0 to m_Xbe.m_TLS.dwDataEndAddr - m_Xbe.m_TLS.dwDataStartAddr - 1 do
        m_bzSection[SectionIdx][SubIndex + i] := pTLSData^[i];
    end;
  end;
*)

var
  i, d, v, c: Integer;
  k, t: DWord;

  dwSectionCursor: LongInt;
  RawAddr: LongInt;
  RawSize: LongInt;
  dwVirtAddr: LongInt;
  dwRawSize: LongInt;
  VirtSize: LongInt;
  VirtAddr: LongInt;
  Flags: DWord;
  SectionSize: LongInt;
  Characteristics: DWord;

  raw_size: LongInt;
  virt_size: DWord;
  virt_addr: DWord;
  ep: DWord;
  kt: DWord;
  SizeOf_Code: DWord;
  SizeOf_Data: DWord;
  SizeOf_Undata: DWord;
  SizeOf_Image: DWord;
  Imag_Base: DWord;


  TLS_DATA: DWord;
  kt_tbl: PDWordArray;
  ThunkTable: PThunkTable;

  pWriteCursor: PByte; // DWord ?
  WriteCursor: DWord;
  Flag: Byte;

  pEmuInit: Pointer;

begin
  pEmuInit := @CxbxKrnlInit; // We need to access the procedure once so it's in memory

  KrnlHandle := GetModuleHandle(cDLLNAME);
  Assert(KrnlHandle >= 32);

  ConstructorInit();

  WriteLog('EmuExe: Generating Exe file...');

  // generate pe header
  m_Header.m_magic := IMAGE_NT_SIGNATURE;
  m_Header.m_machine := IMAGE_FILE_MACHINE_I386; // machine type : i386
  m_Header.m_sections := m_Xbe.m_Header.dwSections + 2; // xbe sections + .cxbximp + .cxbxplg
  m_Header.m_timedate := m_Xbe.m_Header.dwTimeDate; // time/date stamp
  m_Header.m_symbol_table_addr := 0; // unused
  m_Header.m_symbols := 0; // unused
  m_Header.m_sizeof_optional_header := SizeOf(OptionalHeader); // size of optional header
  m_Header.m_characteristics := $010F; // should be fine..
  WriteLog('EmuExe: Generating PE header...OK');

  // generate optional header
  m_OptionalHeader.m_magic := $010B;

  // abitrary linker version : 6.0
  m_OptionalHeader.m_linker_version_major := $06;
  m_OptionalHeader.m_linker_version_minor := $00;

  // size of headers
  m_OptionalHeader.m_sizeof_headers := SizeOf(bzDOSStub) + SizeOf(m_Header);
  m_OptionalHeader.m_sizeof_headers := m_OptionalHeader.m_sizeof_headers + SizeOf(m_OptionalHeader) + SizeOf(SectionHeader) * m_Header.m_sections;
  m_OptionalHeader.m_sizeof_headers := RoundUp(m_OptionalHeader.m_sizeof_headers, PE_FILE_ALIGN);

  m_OptionalHeader.m_image_base := m_Xbe.m_Header.dwBaseAddr;
  m_OptionalHeader.m_section_alignment := PE_SEGM_ALIGN;
  m_OptionalHeader.m_file_alignment := PE_FILE_ALIGN;

  // OS version : 4.0
  m_OptionalHeader.m_os_version_major := $0004;
  m_OptionalHeader.m_os_version_minor := $0000;

  // image version : 0.0
  m_OptionalHeader.m_image_version_major := $0000;
  m_OptionalHeader.m_image_version_minor := $0000;

  // subsystem version : 4.0
  m_OptionalHeader.m_subsystem_version_major := $0004;
  m_OptionalHeader.m_subsystem_version_minor := $0000;

  m_OptionalHeader.m_win32_version := $0000;
  m_OptionalHeader.m_checksum := $0000;
  m_OptionalHeader.m_subsystem := IMAGE_SUBSYSTEM_WINDOWS_GUI;

  // no special dll Characteristics are necessary
  m_OptionalHeader.m_dll_characteristics := $0000;

  m_OptionalHeader.m_sizeof_stack_reserve := $00100000;
  m_OptionalHeader.m_sizeof_stack_commit := m_Xbe.m_Header.dwPeStackCommit;
  m_OptionalHeader.m_sizeof_heap_reserve := m_Xbe.m_Header.dwPeHeapReserve;
  m_OptionalHeader.m_sizeof_heap_commit := m_Xbe.m_Header.dwPeHeapCommit;

  // this member is obsolete, so we'll just set it to zero
  m_OptionalHeader.m_loader_flags := $00000000;

  // we'll set this to the typical 0x10 (16)
  m_OptionalHeader.m_data_directories := $10;

  // clear all data directories (we'll setup some later)
  for d := 1 to m_OptionalHeader.m_data_directories - 1 do
  begin
    m_OptionalHeader.m_image_data_directory[d].m_virtual_addr := 0;
    m_OptionalHeader.m_image_data_directory[d].m_size := 0;
  end;
  WriteLog('EmuExe: Generating Optional Header...OK');


  // generate section headers
  WriteLog('EmuExe: Generating Section Headers...');

  SetLength(m_SectionHeader, m_Header.m_sections);

  // start appending section headers at this point
  dwSectionCursor := RoundUp(m_OptionalHeader.m_sizeof_headers, $1000);

  // generate xbe section headers
  for v := 0 to m_Xbe.m_Header.dwSections - 1 do
  begin
    // generate xbe section name
    FillChar(m_SectionHeader[v].m_name, 8, #0);
    for c := 0 to 7 do
    begin
      m_SectionHeader[v].m_name[c] := m_Xbe.m_szSectionName[v][c];
      if m_SectionHeader[v].m_name[c] = #0 then
        Break;
    end;

    // generate xbe section virtual size / addr
    VirtSize := m_Xbe.m_SectionHeader[v].dwVirtualSize;
    VirtAddr := m_Xbe.m_SectionHeader[v].dwVirtualAddr - m_Xbe.m_Header.dwBaseAddr;

    m_SectionHeader[v].m_virtual_size := VirtSize;
    m_SectionHeader[v].m_virtual_addr := VirtAddr;

    // generate xbe section raw size / addr
    // CXBX TODO: get this working such that m_sizeof_raw can be the actual raw size, not virtual size
    RawSize := RoundUp(m_Xbe.m_SectionHeader[v].dwVirtualSize, PE_FILE_ALIGN);
    RawAddr := dwSectionCursor;

    m_SectionHeader[v].m_sizeof_raw := RawSize;
    m_SectionHeader[v].m_raw_addr := RawAddr;

    dwSectionCursor := dwSectionCursor + RawSize;

    // relocation / line numbers will not exist
    m_SectionHeader[v].m_relocations_addr := 0;
    m_SectionHeader[v].m_linenumbers_addr := 0;

    m_SectionHeader[v].m_relocations := 0;
    m_SectionHeader[v].m_linenumbers := 0;

    // generate Flags for this xbe section
    Flags := IMAGE_SCN_MEM_READ;
    if GetBitEn(Ord(m_Xbe.m_SectionHeader[v].dwFlags[0]), 2) > 0 then // Executable
      Flags := Flags or IMAGE_SCN_MEM_EXECUTE or IMAGE_SCN_CNT_CODE
    else
      Flags := Flags or IMAGE_SCN_CNT_INITIALIZED_DATA;

    if GetBitEn(Ord(m_Xbe.m_SectionHeader[v].dwFlags[0]), 0) > 0 then // Writable
      Flags := Flags or IMAGE_SCN_MEM_WRITE;

    m_SectionHeader[v].m_characteristics := Flags;

    WriteLog(Format('EmuExe: Generating Section Header 0x%.4x... OK', [v]));
  end;

  // generate .cxbximp section header
  i := m_Header.m_sections - 2;
  Move('.cxbximp', m_SectionHeader[i].m_name, 8);

  // generate .cxbximp section virtual size / addr
  virt_size := RoundUp($6E, PE_SEGM_ALIGN);
  virt_addr := RoundUp(m_SectionHeader[i - 1].m_virtual_addr + m_SectionHeader[i - 1].m_virtual_size, PE_SEGM_ALIGN);

  m_SectionHeader[i].m_virtual_size := virt_size;
  m_SectionHeader[i].m_virtual_addr := virt_addr;

  // generate .cxbximp section raw size / addr
  raw_size := RoundUp(m_SectionHeader[i].m_virtual_size, PE_FILE_ALIGN);

  m_SectionHeader[i].m_sizeof_raw := raw_size;
  m_SectionHeader[i].m_raw_addr := dwSectionCursor;

  dwSectionCursor := dwSectionCursor + raw_size;

  // relocation / line numbers will not exist
  m_SectionHeader[i].m_relocations_addr := 0;
  m_SectionHeader[i].m_linenumbers_addr := 0;

  m_SectionHeader[i].m_relocations := 0;
  m_SectionHeader[i].m_linenumbers := 0;

  // make this section readable initialized data
  m_SectionHeader[i].m_characteristics := IMAGE_SCN_MEM_READ xor IMAGE_SCN_CNT_INITIALIZED_DATA;

  // update import table directory entry
  m_OptionalHeader.m_image_data_directory[IMAGE_DIRECTORY_ENTRY_IMPORT].m_virtual_addr := m_SectionHeader[i].m_virtual_addr + $08;
  m_OptionalHeader.m_image_data_directory[IMAGE_DIRECTORY_ENTRY_IMPORT].m_size := $28;

  //  update import address table directory entry
  m_OptionalHeader.m_image_data_directory[IMAGE_DIRECTORY_ENTRY_IAT].m_virtual_addr := m_SectionHeader[i].m_virtual_addr;
  m_OptionalHeader.m_image_data_directory[IMAGE_DIRECTORY_ENTRY_IAT].m_size := $08;
  WriteLog(Format('EmuExe: Generating Section Header 0x%.4x(.cxbximp)... OK', [i]));

  //  generate .cxbxplg section header
  i := m_Header.m_sections - 1;

  m_SectionHeader[i].m_name := '.cxbxplg';

  // generate .cxbxplg section virtual size / addr
  begin
    virt_size := RoundUp(
      m_OptionalHeader.m_image_base + $100 +
      m_Xbe.m_Header.dwSizeofHeaders + 260 +
      SizeOf(m_Xbe.m_LibraryVersion) * m_Xbe.m_Header.dwLibraryVersions +
      SizeOf(m_Xbe.m_TLS) +
      (m_Xbe.m_TLS.dwDataEndAddr - m_Xbe.m_TLS.dwDataStartAddr), $1000);
    virt_addr := RoundUp(
      m_SectionHeader[i - 1].m_virtual_addr +
      m_SectionHeader[i - 1].m_virtual_size, PE_SEGM_ALIGN);

    m_SectionHeader[i].m_virtual_size := virt_size;
    m_SectionHeader[i].m_virtual_addr := virt_addr;

    // our entry point should be the first bytes in this section
    m_OptionalHeader.m_entry := virt_addr;
  end;

  // generate .cxbxplg section raw size / addr
  begin
    raw_size := RoundUp(m_SectionHeader[i].m_virtual_size, PE_FILE_ALIGN);

    m_SectionHeader[i].m_sizeof_raw := raw_size;
    m_SectionHeader[i].m_raw_addr := dwSectionCursor;
//    Inc(dwSectionCursor, raw_size);
  end;

  // relocation / line numbers will not exist
  begin
    m_SectionHeader[i].m_relocations_addr := 0;
    m_SectionHeader[i].m_linenumbers_addr := 0;

    m_SectionHeader[i].m_relocations := 0;
    m_SectionHeader[i].m_linenumbers := 0;
  end;

  // make this section readable and executable
  m_SectionHeader[i].m_characteristics := IMAGE_SCN_MEM_READ xor IMAGE_SCN_MEM_EXECUTE xor IMAGE_SCN_CNT_CODE;

  WriteLog(Format('EmuExe: Generating Section Header 0x%.4x(.cxbxplg)... OK', [i]));


  // GENERATE SECTIONS  ------ PART WE STUCK

  // generate sections
  WriteLog('EmuExe: Generating Sections...');

  SetLength(m_bzSection, m_Header.m_sections);


  // generate xbe sections
  for v := 0 to m_xbe.m_Header.dwSections - 1 do
  begin
    _CopySections(v);
    WriteLog(Format('EmuExe: Generating Section 0x%.4x... OK', [v]));
  end;

  // generate .cxbximp section
  begin
    i := m_Header.m_sections - 2;
    WriteLog(Format('EmuExe: Generating Section 0x%.4x (.cxbximp)... OK', [i]));

    dwVirtAddr := m_SectionHeader[i].m_virtual_addr;
    dwRawSize := m_SectionHeader[i].m_sizeof_raw;
    SetLength(m_bzSection[i], dwRawSize);
    ZeroMemory(m_bzSection[i], dwRawSize);

    _WriteDWordToSectionPos(i, $00, dwVirtAddr + $38);
    _WriteDWordToSectionPos(i, $04, 0);
    _WriteDWordToSectionPos(i, $08, dwVirtAddr + $30);
    _WriteDWordToSectionPos(i, $0C, 0);

    _WriteDWordToSectionPos(i, $10, 0);
    _WriteDWordToSectionPos(i, $14, dwVirtAddr + $4A);
    _WriteDWordToSectionPos(i, $18, dwVirtAddr + $00);
    _WriteDWordToSectionPos(i, $1C, 0);

    _WriteDWordToSectionPos(i, $20, 0);
    _WriteDWordToSectionPos(i, $24, 0);
    _WriteDWordToSectionPos(i, $28, 0);
    _WriteDWordToSectionPos(i, $2C, 0);

    _WriteDWordToSectionPos(i, $30, dwVirtAddr + $38);
    _WriteDWordToSectionPos(i, $34, 0);
    _WriteDWordToSectionPos(i, $38, $0001);

    if UseDebugDLL then
      CopyMemory(@(m_bzSection[i][$3A]), PChar(StrCxbxKrnlNoFunc00CxbxKrnl_dll), Length(StrCxbxKrnlNoFunc00CxbxKrnl_dll))
    else
      CopyMemory(@(m_bzSection[i][$3A]), PChar(StrCxbxKrnlNoFunc00Cxbx_dll), Length(StrCxbxKrnlNoFunc00Cxbx_dll));
  end;

  // generate .cxbxplg section
  begin
    ep := m_Xbe.m_Header.dwEntryAddr;
    i := m_Header.m_sections - 1;

    WriteLog(Format('EmuExe: Generating Section Header 0x%.4x (.cxbxplg)... OK', [i]));

    // decode entry point
    if ((ep xor XOR_EP_RETAIL) > $01000000) then
      ep := ep xor XOR_EP_DEBUG
    else
      ep := ep xor XOR_EP_RETAIL;

    SetLength(m_bzSection[i], m_SectionHeader[i].m_sizeof_raw);

    pWriteCursor := @(m_bzSection[i][0]);

    // Append prolog section
(*
    _AppendProlog(i);
*)
    CopyMemory(pWriteCursor, @(Prolog[0]), Length(Prolog));
    Inc(pWriteCursor, $100);

    // Append xbe header
(*
    _AppendXbeHeader(i, pWriteCursor);
*)
    CopyMemory(pWriteCursor, @(m_Xbe.m_Header), SizeOf(m_Xbe.m_Header));
    Inc(pWriteCursor, SizeOf(m_Xbe.m_Header));

    // Append xbe extra header bytes
(*
    for c := 0 to (m_Xbe.m_Header.dwSizeofHeaders - SizeOf(m_Xbe.m_Header)) - 1 do
    begin
      m_bzSection[i][pWriteCursor] := m_Xbe.m_HeaderEx[c];
      Inc(pWriteCursor);
    end;
*)
    CopyMemory(pWriteCursor, @(m_Xbe.m_HeaderEx), m_Xbe.m_Header.dwSizeofHeaders - SizeOf(m_Xbe.m_Header));
    Dec(pWriteCursor, SizeOf(m_Xbe.m_Header));
    Inc(pWriteCursor, m_Xbe.m_Header.dwSizeofHeaders);

    // Append x_debug_filename
(*
    for c := Length(m_KrnlDebugFilename) to 259 do
    begin
      m_bzSection[i][pWriteCursor] := 0;
      Inc(pWriteCursor);
    end;
*)
    SetLength(m_KrnlDebugFilename, 260);
    CopyMemory(pWriteCursor, @(m_Xbe.m_HeaderEx), 260);
    Inc(pWriteCursor, 260);

    // Append library versions
    for c := 0 to m_xbe.m_Header.dwLibraryVersions - 1 do
    begin
(*
      _AppendXbeLibVersion(i, pWriteCursor, c);
*)
      CopyMemory(pWriteCursor, @(m_Xbe.m_LibraryVersion[c]), Sizeof(m_Xbe.m_LibraryVersion[c]));
      Inc(pWriteCursor, SizeOf(m_xbe.m_LibraryVersion[c]));
    end;

    // Append TLS data
    if SizeOf(m_Xbe.m_TLS) <> 0 then // TODO : m_TLS should be a pointer, tested with Assigned()
    begin
(*
      _AppendTLS(i, pWriteCursor);
*)
      CopyMemory(pWriteCursor, @(m_Xbe.m_TLS), SizeOf(m_Xbe.m_TLS));
      Inc(pWriteCursor, SizeOf(m_Xbe.m_TLS));

(*
      _AppendTLSData(i, pWriteCursor);
*)
      CopyMemory(pWriteCursor, Pointer(m_Xbe.GetTLSData()), m_Xbe.m_TLS.dwDataEndAddr - m_Xbe.m_TLS.dwDataStartAddr);
      Inc(pWriteCursor, m_Xbe.m_TLS.dwDataEndAddr - m_Xbe.m_TLS.dwDataStartAddr);
    end;

    // patch prolog function parameters
    WriteCursor := m_SectionHeader[i].m_virtual_addr + m_optionalHeader.m_image_base + $100;

    // Function Pointer
    pEmuInit := GetProcAddress(KrnlHandle, 'CxbxKrnlInit');
    _WriteDWordToSectionPos(i, 1, DWord(pEmuInit));
// test
pEmuInit := @CxbxKrnlInit;
_WriteDWordToSectionPos(i, $81, DWord(pEmuInit));

    // Param 8 : Entry
    _WriteDWordToSectionPos(i, 6, ep);

    // Param 7 : dwXbeHeaderSize
    _WriteDWordToSectionPos(i, 11, m_Xbe.m_Header.dwSizeofHeaders);

    // Param 6 : pXbeHeader
    _WriteDWordToSectionPos(i, 16, WriteCursor);
    Inc(WriteCursor, m_Xbe.m_Header.dwSizeofHeaders);

    // Param 5 : szDebugFilename
    _WriteDWordToSectionPos(i, 21, WriteCursor);
    Inc(WriteCursor, 260);

    // Param 4 : DbgMode
    _WriteDWordToSectionPos(i, 26, DWord(m_KrnlDebug));

    // Param 3 : pLibraryVersion
    if Length(m_Xbe.m_LibraryVersion) <> 0 then
    begin
      _WriteDWordToSectionPos(i, 31, WriteCursor);
      Inc(WriteCursor, SizeOf(m_Xbe.m_LibraryVersion[0]) * m_xbe.m_Header.dwLibraryVersions);
    end
    else
      _WriteDWordToSectionPos(i, 31, 0);

    // Param 2 : pTLS
    if SizeOf(m_Xbe.m_TLS) <> 0 then
    begin
      _WriteDWordToSectionPos(i, 36, WriteCursor);
      Inc(WriteCursor, SizeOf(m_Xbe.m_TLS));
    end
    else
      _WriteDWordToSectionPos(i, 36, 0);

    // Param 1 : pTLSData
    if SizeOf(m_Xbe.m_TLS) <> 0 then
    begin
      _WriteDWordToSectionPos(i, 41, WriteCursor);
      Inc(WriteCursor, m_Xbe.m_TLS.dwDataEndAddr - m_Xbe.m_TLS.dwDataStartAddr);
    end
    else
      _WriteDWordToSectionPos(i, 41, 0);

    // Param 0 : hwndParent
    _WriteDWordToSectionPos(i, 46, hwndParent);
  end;

  // END GENERATE SECTIONS  ------ WE STUCK HERE
  // ******************************************************************
  // * patch kernel thunk table
  // ******************************************************************
  WriteLog('EmuExe: Hijacking Kernel Imports...');
  // generate xbe sections
  kt := m_Xbe.m_Header.dwKernelImageThunkAddr;

  // decode kernel thunk address
  if (kt xor XOR_KT_DEBUG) > $01000000 then
    kt := kt xor XOR_KT_RETAIL
  else
    kt := kt xor XOR_KT_DEBUG;

  // locate section containing kernel thunk table
  ThunkTable := GetProcAddress(KrnlHandle, 'CxbxKrnl_KernelThunkTable');
  for v := 0 to m_Xbe.m_Header.dwSections - 1 do
  begin
    imag_base := m_OptionalHeader.m_image_base;
    virt_addr := m_SectionHeader[v].m_virtual_addr;
    virt_size := m_SectionHeader[v].m_virtual_size;

    // modify kernel thunk table, if found
    if ((kt >= virt_addr + imag_base) and (kt < virt_addr + virt_size + imag_base)) then
    begin
      WriteLog(Format('EmuExe: Located Thunk Table in Section 0x%.4x (0x%.8X)...', [v, kt]));
      kt_tbl := @(m_bzSection[v][kt - virt_addr - imag_base]);
      k := 0;
      while kt_tbl[k] <> 0 do
      begin
        t := kt_tbl[k] and $7FFFFFFF;
        // TODO : This method works with cxbx's dll
        //        Once exe creation is complete, we'll get the thunk table from our dll
        _WriteDWordToAddr(@(kt_tbl[k]), ThunkTable[t]);
        if t <> $FFFFFFFF then
          WriteLog(Format('EmuExe: Thunk %.3d : *0x%.8X := 0x%.8X', [t, kt + (k * 4), kt_tbl[k]]));

        Inc(k);
      end; // while
    end; // if

  end; // for


  // update imcomplete header fields
  // calculate size of code / data / image
  SizeOf_Code := 0;
  SizeOf_Data := 0;
  SizeOf_Undata := 0;

  for v := 0 to m_Header.m_sections - 1 do
  begin
    Characteristics := m_SectionHeader[v].m_characteristics;
    if ((Characteristics and IMAGE_SCN_MEM_EXECUTE) <> 0) or ((Characteristics and IMAGE_SCN_CNT_CODE) <> 0) then
      SizeOf_Code := SizeOf_Code + m_SectionHeader[v].m_SizeOf_Raw;

    if (Characteristics and IMAGE_SCN_CNT_INITIALIZED_DATA) <> 0 then
      Sizeof_data := Sizeof_Data + m_SectionHeader[v].m_sizeof_raw;
  end;

  // calculate size of image
  // The RoundUp is necessary to allow the generated exe to run under Vista
  // This is actually a bug that needs to be corrected in CXBX too
  SizeOf_Image := SizeOf_Undata + SizeOf_Data + SizeOf_Code + RoundUp(m_OptionalHeader.m_sizeof_headers, $1000);
  SizeOf_Image := RoundUp(SizeOf_Image, PE_SEGM_ALIGN);

  // update optional header as necessary
  m_OptionalHeader.m_sizeof_code := sizeof_code;
  m_OptionalHeader.m_sizeof_initialized_data := sizeof_data;
  m_OptionalHeader.m_sizeof_uninitialized_data := sizeof_undata;
  m_OptionalHeader.m_sizeof_image := sizeof_image;

  // we'll set code base as the virtual address of the first section
  m_OptionalHeader.m_code_base := m_SectionHeader[0].m_virtual_addr;

  // we'll set data base as the virtual address of the first section
  // that is not marked as containing code or being executable

  for v := 0 to m_Header.m_sections - 1 do
  begin
    Characteristics := m_SectionHeader[v].m_characteristics;
    if (   not ((Characteristics and IMAGE_SCN_MEM_EXECUTE) <> 0)
        or not ((Characteristics and IMAGE_SCN_CNT_CODE) <> 0)) then
    begin
      m_optionalHeader.m_data_base := m_SectionHeader[v].m_virtual_addr;
      Break;
    end;
  end;

  WriteLog('EmuExe: Finalizing Exe Files...OK');
end; // TEmuExe.Create

//------------------------------------------------------------------------------

destructor TEmuExe.Destroy;
begin
  FreeLibrary(KrnlHandle);

  inherited Destroy;
end;

end.
