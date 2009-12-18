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
  SysUtils, // SafeLoadLibrary
  // Dxbx
  uConsts,
  uTypes,
  uLog,
  uDxbxUtils,
  uProlog,
  uXbe,
  uExe;

type
  TEmuExe = class(TExe)
  protected
    KrnlHandle: HMODULE;
  public
    constructor Create(x_Xbe: TXbe; aKernelDebugMode: TDebugMode; aKernelDebugFileName: string; hwndParent: THandle);
    destructor Destroy; override;
  end;

const
  bzDOSStub: array [0..120-1] of Byte = (
    // Start of DOS code block :
    $0E,           // push cs
    $1F,           // pop ds

    $BA, $0E, $00, // mov dx, $0e // DX=$0e is offset to the string below
    $B4, $09,      // mov ah, $09 // AH=$09 lets int 21 do 'Display String'
    $CD, $21,      // int 21

    $B8, $01, $4C, // mov ax, $4c01 // AH=$4c AL=$01 lets int 21 do 'Terminate' with errorcode 1
    $CD, $21,      // int 21

    // Offset $0E - 'This program cannot be run in DOS mode.'#13#13#10'$'#0 :
    $54, $68, $69, $73, $20, $70, $72, $6F,
    $67, $72, $61, $6D, $20, $63, $61, $6E,
    $6E, $6F, $74, $20, $62, $65, $20, $72,
    $75, $6E, $20, $69, $6E, $20, $44, $4F,
    $53, $20, $6D, $6F, $64, $65, $2E, $0D,
    $0D, $0A, $24, $00,

    // Offset $3A - Unidentified bytes :
    $00, $00, $00, $00, $00, $00,
    $85, $E3, $B8, $DB, $C1, $82, $D6, $88,
    $C1, $82, $D6, $88, $C1, $82, $D6, $88,
    $C1, $82, $D7, $88, $C3, $82, $D6, $88,
    $3E, $A2, $D2, $88, $C2, $82, $D6, $88,
    $95, $A1, $E7, $88, $C0, $82, $D6, $88,
    $52, $69, $63, $68, $C1, $82, $D6, $88,
    $00, $00, $00, $00, $00, $00, $00, $00);

type
  // TODO : Remove Cxbx versions and add non-debug Dxbx version when our DLL starts working :
  TUseDll = (udCxbxKrnl, udCxbx, udDxbxKrnl {, udDxbx});

var
  DllToUse: TUseDLL = udDxbxKrnl;

function GetDllDescription(const aDllToUse: TUseDll): string;
function GetDllName(const aDllToUse: TUseDll): string;
function GetNoFuncImport(const aDllToUse: TUseDll): AnsiString;

implementation

function GetDllDescription(const aDllToUse: TUseDll): string;
begin
  case aDllToUse of
    udCxbxKrnl:
      Result := 'Cxbx debug';
    udCxbx:
      Result := 'Cxbx';
    udDxbxKrnl:
      Result := 'Dxbx debug';
{    udDxbx:
      Result := 'Dxbx';}
  else
    Assert(False);
    Result := '';
  end;
end;

function GetDllName(const aDllToUse: TUseDll): string;
begin
  case aDllToUse of
    udCxbxKrnl:
      Result := CCXBXKRNLDLLNAME;
    udCxbx:
      Result := CCXBXDLLNAME;
    udDxbxKrnl:
      Result := CDXBXKRNLDLLNAME;
{    udDxbx:
      Result := CDXBXDLLNAME;}
  else
    Assert(False);
    Result := '';
  end;
end;

function GetNoFuncImport(const aDllToUse: TUseDll): AnsiString;
begin
  Result := 'CxbxKrnlNoFunc'#0#0 + AnsiString(GetDllName(aDllToUse));
end;

{ TEmuExe }

//------------------------------------------------------------------------------

constructor TEmuExe.Create(x_Xbe: TXbe; aKernelDebugMode: TDebugMode;
  aKernelDebugFileName: string; hwndParent: THandle);

  procedure _WriteDWordToAddr(const aAddr: Pointer; aDWord: DWord);
  begin
    CopyMemory(aAddr, @aDWord, SizeOf(DWord));
  end;

  procedure _WriteDWordToSectionPos(SectionIdx, iPos: Integer; aDWord: DWord);
  begin
    _WriteDWordToAddr(@(SectionContents[SectionIdx][iPos]), aDWord);
  end;

var
  DOSSize: Integer;
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
  SectionSize: uint32;
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
  DLLBase: DWord;

  TLS_DATA: DWord;
  kt_tbl: PDWordArray;
  GetKernelThunkTable: TGetKernelThunkTable;
  KernelThunkTable: PKernelThunkTable;

  pWriteCursor: PByte; // DWord ?
  WriteCursor: DWord;
  Flag: Byte;

  pEmuInit: Pointer;
  NoFuncImport: AnsiString;
  TmpStr: string;

begin
  TmpStr := GetDllName(DllToUse);
  KrnlHandle := SafeLoadLibrary(TmpStr);
  if not IsValidLibraryHandle(KrnlHandle) then
  begin
    TmpStr := 'EmuExe: Cannot open DLL ' + TmpStr + '. Reason :'#13#10 + GetLastErrorString();
    WriteLog(TmpStr);
    RaiseLastOSError;
  end;

  ConstructorInit();

  WriteLog('EmuExe: Generating Exe file...');

  // Fill DOS code block with a stub :
  SetLength(DOSCode, SizeOf(bzDOSStub));
  CopyMemory(@(DOSCode[0]), @(bzDOSStub[0]), SizeOf(bzDOSStub));

  DOSSize := SizeOf(TImageDosHeader) + Length(DOSCode); // Cxbx used 1680 for this for some reason.

  // generate dos header
  DOSHeader.e_magic := IMAGE_DOS_SIGNATURE;     { Magic number                     } 
  DOSHeader.e_cblp := DOSSize mod DOS_PAGESIZE; { Bytes on last page of file       }
  DOSHeader.e_cp := DOSSize div DOS_PAGESIZE;   { Pages in file                    }
  DOSHeader.e_crlc := 0;                        { Relocations                      }
  DOSHeader.e_cparhdr := 4;                     { Size of header in paragraphs     }
  DOSHeader.e_minalloc := 0;                    { Minimum extra paragraphs needed  }
  DOSHeader.e_maxalloc := $FFFF;                { Maximum extra paragraphs needed  }
  DOSHeader.e_ss := 0;                          { Initial (relative) SS value      }
  DOSHeader.e_sp := DOSSize;                    { Initial SP value                 }
  DOSHeader.e_csum := 0;                        { Checksum                         }
  DOSHeader.e_ip := 0;                          { Initial IP value                 }
  DOSHeader.e_cs := 0;                          { Initial (relative) CS value      }
  DOSHeader.e_lfarlc := SizeOf(DOSHeader);      { File address of relocation table }
  DOSHeader.e_ovno := 0;                        { Overlay number                   }
  DOSHeader.e_res[0] := 0;                      { Reserved words                   }
  DOSHeader.e_res[1] := 0;
  DOSHeader.e_res[2] := 0;
  DOSHeader.e_res[3] := 0;
  DOSHeader.e_oemid := 0;                       { OEM identifier (for DOSHeader.e_oeminfo)   }
  DOSHeader.e_oeminfo := 0;                     { OEM information; DOSHeader.e_oemid specific}
  DOSHeader.e_res2[0] := 0;                     { Reserved words                   }
  DOSHeader.e_res2[1] := 0;
  DOSHeader.e_res2[2] := 0;
  DOSHeader.e_res2[3] := 0;
  DOSHeader.e_res2[4] := 0;
  DOSHeader.e_res2[5] := 0;
  DOSHeader.e_res2[6] := 0;
  DOSHeader.e_res2[7] := 0;
  DOSHeader.e_res2[8] := 0;
  DOSHeader.e_res2[9] := 0;
  DOSHeader._lfanew := DOSSize;                 { File address of new exe header   }

  // generate pe header
  NtHeaders.Signature := IMAGE_NT_SIGNATURE;
  NtHeaders.FileHeader.Machine := IMAGE_FILE_MACHINE_I386; // machine type : i386
  NtHeaders.FileHeader.NumberOfSections := x_Xbe.m_Header.dwSections + 2; // xbe sections + .cxbximp + .cxbxplg
  NtHeaders.FileHeader.TimeDateStamp := x_Xbe.m_Header.dwTimeDate; // time/date stamp
  NtHeaders.FileHeader.PointerToSymbolTable := 0; // unused
  NtHeaders.FileHeader.NumberOfSymbols := 0; // unused
  NtHeaders.FileHeader.SizeOfOptionalHeader := SizeOf(NtHeaders.OptionalHeader); // size of optional header
  NtHeaders.FileHeader.Characteristics := IMAGE_FILE_RELOCS_STRIPPED     // Relocation info stripped from file.
                                       or IMAGE_FILE_EXECUTABLE_IMAGE    // File is executable  (i.e. no unresolved externel references).
                                       or IMAGE_FILE_LINE_NUMS_STRIPPED  // Line nunbers stripped from file.
                                       or IMAGE_FILE_LOCAL_SYMS_STRIPPED // Local symbols stripped from file.
                                 {new} or IMAGE_FILE_BYTES_REVERSED_LO   // Bytes of machine word are reversed.
                                       or IMAGE_FILE_32BIT_MACHINE       // 32 bit word machine.
                                 {new} or IMAGE_FILE_BYTES_REVERSED_HI;  // Bytes of machine word are reversed.

  WriteLog('EmuExe: Generating PE header...OK');

  // generate optional header
  NtHeaders.OptionalHeader.Magic := IMAGE_NT_OPTIONAL_HDR_MAGIC;

  // abitrary linker version : 6.0
  NtHeaders.OptionalHeader.MajorLinkerVersion := $06;
  NtHeaders.OptionalHeader.MinorLinkerVersion := $00;

  // size of headers
  NtHeaders.OptionalHeader.SizeOfHeaders := SizeOf(bzDOSStub) + SizeOf(NtHeaders)
                                          + (SizeOf(TImageSectionHeader) * NtHeaders.FileHeader.NumberOfSections);
  NtHeaders.OptionalHeader.SizeOfHeaders := RoundUp(NtHeaders.OptionalHeader.SizeOfHeaders, PE_FILE_ALIGNMENT);

  NtHeaders.OptionalHeader.ImageBase := x_Xbe.m_Header.dwBaseAddr;
  NtHeaders.OptionalHeader.SectionAlignment := PE_SECTION_ALIGNMENT;
  NtHeaders.OptionalHeader.FileAlignment := PE_FILE_ALIGNMENT;

  // OS version : 4.0
  NtHeaders.OptionalHeader.MajorOperatingSystemVersion := $0004;
  NtHeaders.OptionalHeader.MinorOperatingSystemVersion := $0000;

  // image version : 0.0
  NtHeaders.OptionalHeader.MajorImageVersion := $0000;
  NtHeaders.OptionalHeader.MinorImageVersion := $0000;

  // subsystem version : 4.0
  NtHeaders.OptionalHeader.MajorSubsystemVersion := $0004;
  NtHeaders.OptionalHeader.MinorSubsystemVersion := $0000;

  NtHeaders.OptionalHeader.Win32VersionValue := $0000;
  NtHeaders.OptionalHeader.CheckSum := $0000;
  NtHeaders.OptionalHeader.Subsystem := IMAGE_SUBSYSTEM_WINDOWS_GUI;

  // no special dll Characteristics are necessary
  NtHeaders.OptionalHeader.DllCharacteristics := $0000;

  NtHeaders.OptionalHeader.SizeOfStackReserve := $00100000;
  NtHeaders.OptionalHeader.SizeOfStackCommit := x_Xbe.m_Header.dwPeStackCommit;
  NtHeaders.OptionalHeader.SizeOfHeapReserve := x_Xbe.m_Header.dwPeHeapReserve;
  NtHeaders.OptionalHeader.SizeOfHeapCommit := x_Xbe.m_Header.dwPeHeapCommit;

  // this member is obsolete, so we'll just set it to zero
  NtHeaders.OptionalHeader.LoaderFlags := $00000000;

  // we'll set this to the typical 0x10 (16)
  NtHeaders.OptionalHeader.NumberOfRvaAndSizes := $10;

  // clear all data directories (we'll setup some later)
  for d := 1 to NtHeaders.OptionalHeader.NumberOfRvaAndSizes - 1 do
  begin
    NtHeaders.OptionalHeader.DataDirectory[d].VirtualAddress := 0;
    NtHeaders.OptionalHeader.DataDirectory[d].Size := 0;
  end;
  WriteLog('EmuExe: Generating Optional Header...OK');


  // generate section headers
  WriteLog('EmuExe: Generating Section Headers...');

  SetLength(SectionHeaders, NtHeaders.FileHeader.NumberOfSections);

  // start appending section headers at this point
  dwSectionCursor := RoundUp(NtHeaders.OptionalHeader.SizeOfHeaders, PE_HEADER_ALIGNMENT);

  // generate xbe section headers
  for v := 0 to x_Xbe.m_Header.dwSections - 1 do
  begin
    // generate xbe section name
    ZeroMemory(@(SectionHeaders[v].Name[0]), IMAGE_SIZEOF_SHORT_NAME);
    for c := 0 to 7 do
    begin
      SectionHeaders[v].Name[c] := Ord(x_Xbe.m_szSectionName[v][c]);
      if SectionHeaders[v].Name[c] = 0 then
        Break;
    end;

    // generate xbe section virtual size / addr
    VirtSize := x_Xbe.m_SectionHeader[v].dwVirtualSize;
    VirtAddr := x_Xbe.m_SectionHeader[v].dwVirtualAddr - x_Xbe.m_Header.dwBaseAddr;

    SectionHeaders[v].Misc.VirtualSize := VirtSize;
    SectionHeaders[v].VirtualAddress := VirtAddr;

    // generate xbe section raw size / addr
    // CXBX TODO: get this working such that m_sizeof_raw can be the actual raw size, not virtual size
    RawSize := RoundUp(x_Xbe.m_SectionHeader[v].dwVirtualSize, PE_FILE_ALIGNMENT);
    RawAddr := dwSectionCursor;

    SectionHeaders[v].SizeOfRawData := RawSize;
    SectionHeaders[v].PointerToRawData := RawAddr;

    dwSectionCursor := dwSectionCursor + RawSize;

    // relocation / line numbers will not exist
    SectionHeaders[v].PointerToRelocations := 0;
    SectionHeaders[v].PointerToLinenumbers := 0;

    SectionHeaders[v].NumberOfRelocations := 0;
    SectionHeaders[v].NumberOfLinenumbers := 0;

    // generate Flags for this xbe section
    Flags := IMAGE_SCN_MEM_READ;
    if (x_Xbe.m_SectionHeader[v].dwFlags[0] and XBE_SECTIONHEADER_FLAG_Executable) > 0 then
      Flags := Flags or IMAGE_SCN_MEM_EXECUTE or IMAGE_SCN_CNT_CODE
    else
      Flags := Flags or IMAGE_SCN_CNT_INITIALIZED_DATA;

    if (x_Xbe.m_SectionHeader[v].dwFlags[0] and XBE_SECTIONHEADER_FLAG_Writable) > 0 then
      Flags := Flags or IMAGE_SCN_MEM_WRITE;

    SectionHeaders[v].Characteristics := Flags;

{$IFDEF DEBUG}
    DbgPrintf('EmuExe: Generating Section Header 0x%.4x... OK', [v]);
{$ENDIF}
  end;

  // generate .cxbximp section header
  i := NtHeaders.FileHeader.NumberOfSections - 2;
  Move(AnsiString('.cxbximp'), SectionHeaders[i].Name, IMAGE_SIZEOF_SHORT_NAME);

  // generate .cxbximp section virtual size / addr
  virt_size := RoundUp($6E, PE_SECTION_ALIGNMENT);
  virt_addr := RoundUp(SectionHeaders[i - 1].VirtualAddress + SectionHeaders[i - 1].Misc.VirtualSize, PE_SECTION_ALIGNMENT);

  SectionHeaders[i].Misc.VirtualSize := virt_size;
  SectionHeaders[i].VirtualAddress := virt_addr;

  // generate .cxbximp section raw size / addr
  raw_size := RoundUp(SectionHeaders[i].Misc.VirtualSize, PE_FILE_ALIGNMENT);

  SectionHeaders[i].SizeOfRawData := raw_size;
  SectionHeaders[i].PointerToRawData := dwSectionCursor;

  dwSectionCursor := dwSectionCursor + raw_size;

  // relocation / line numbers will not exist
  SectionHeaders[i].PointerToRelocations := 0;
  SectionHeaders[i].PointerToLinenumbers := 0;

  SectionHeaders[i].NumberOfRelocations := 0;
  SectionHeaders[i].NumberOfLinenumbers := 0;

  // make this section readable initialized data
  SectionHeaders[i].Characteristics := IMAGE_SCN_MEM_READ xor IMAGE_SCN_CNT_INITIALIZED_DATA;

  // update import table directory entry
  NtHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress := SectionHeaders[i].VirtualAddress + $08;
  NtHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].Size := $28;

  //  update import address table directory entry
  NtHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IAT].VirtualAddress := SectionHeaders[i].VirtualAddress;
  NtHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IAT].Size := $08;

{$IFDEF DEBUG}
  DbgPrintf('EmuExe: Generating Section Header 0x%.4x(.cxbximp)... OK', [i]);
{$ENDIF}

  //  generate .cxbxplg section header
  i := NtHeaders.FileHeader.NumberOfSections - 1;

  Move(AnsiString('.cxbxplg'), SectionHeaders[i].Name, IMAGE_SIZEOF_SHORT_NAME);

  // generate .cxbxplg section virtual size / addr
  begin
    if Assigned(x_Xbe.m_TLS) then
      virt_size := SizeOF(XBE_TLS) + (x_Xbe.m_TLS.dwDataEndAddr - x_Xbe.m_TLS.dwDataStartAddr)
    else
      virt_size := 0;

    virt_size := RoundUp(
      NtHeaders.OptionalHeader.ImageBase + SizeOf(Prolog) +
      x_Xbe.m_Header.dwSizeofHeaders + MAX_PATH +
      DWord(Length(x_Xbe.m_LibraryVersion) * Integer(x_Xbe.m_Header.dwLibraryVersions)) +
      virt_size
      , PE_HEADER_ALIGNMENT);
    virt_addr := RoundUp(
      SectionHeaders[i - 1].VirtualAddress +
      SectionHeaders[i - 1].Misc.VirtualSize, PE_SECTION_ALIGNMENT);

    SectionHeaders[i].Misc.VirtualSize := virt_size;
    SectionHeaders[i].VirtualAddress := virt_addr;

    // our entry point should be the first bytes in this section
    NtHeaders.OptionalHeader.AddressOfEntryPoint := virt_addr;
  end;

  // generate .cxbxplg section raw size / addr
  begin
    raw_size := RoundUp(SectionHeaders[i].Misc.VirtualSize, PE_FILE_ALIGNMENT);

    SectionHeaders[i].SizeOfRawData := raw_size;
    SectionHeaders[i].PointerToRawData := dwSectionCursor;
{$IFDEF KEEP_UNNECCESARY_CODE}
    Inc(dwSectionCursor, raw_size);
{$ENDIF}
  end;

  // relocation / line numbers will not exist
  begin
    SectionHeaders[i].PointerToRelocations := 0;
    SectionHeaders[i].PointerToLinenumbers := 0;

    SectionHeaders[i].NumberOfRelocations := 0;
    SectionHeaders[i].NumberOfLinenumbers := 0;
  end;

  // make this section readable and executable
  SectionHeaders[i].Characteristics := IMAGE_SCN_MEM_READ xor IMAGE_SCN_MEM_EXECUTE xor IMAGE_SCN_CNT_CODE;

{$IFDEF DEBUG}
  DbgPrintf('EmuExe: Generating Section Header 0x%.4x(.cxbxplg)... OK', [i]);
{$ENDIF}

  // generate sections
  WriteLog('EmuExe: Generating Sections...');

  SetLength(SectionContents, NtHeaders.FileHeader.NumberOfSections);


  // generate xbe sections
  for v := 0 to x_Xbe.m_Header.dwSections - 1 do
  begin
    SectionSize := SectionHeaders[v].SizeOfRawData;
    // Allocate needed section size + 4 bytes (needed to assure zero-termination of this section) :
    SetLength(SectionContents[v], SectionSize + 4);

    // Copy over all contents from the Xbe section :
    memcpy(SectionContents[v], x_Xbe.m_bzSection[v], x_Xbe.m_SectionHeader[v].dwSizeofRaw);
    WriteLog(Format('EmuExe: Generating Section 0x%.4x... OK', [v]));
  end;

  // generate .cxbximp section
  begin
    i := NtHeaders.FileHeader.NumberOfSections - 2;
    WriteLog(Format('EmuExe: Generating Section 0x%.4x (.cxbximp)... OK', [i]));

    dwVirtAddr := SectionHeaders[i].VirtualAddress;
    dwRawSize := SectionHeaders[i].Misc.VirtualSize;
    SetLength(SectionContents[i], dwRawSize);
    ZeroMemory(SectionContents[i], dwRawSize);

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

    NoFuncImport := GetNoFuncImport(DllToUse);
    CopyMemory(@(SectionContents[i][$3A]), PAnsiChar(NoFuncImport), Length(NoFuncImport));
  end;

  // generate .cxbxplg section
  begin
    ep := x_Xbe.m_Header.dwEntryAddr;
    i := NtHeaders.FileHeader.NumberOfSections - 1;

{$IFDEF DEBUG}
    DbgPrintf('EmuExe: Generating Section Header 0x%.4x (.cxbxplg)... OK', [i]);
{$ENDIF}

    // decode entry point
    if ((ep xor XOR_EP_RETAIL) > $01000000) then
      ep := ep xor XOR_EP_DEBUG
    else
      ep := ep xor XOR_EP_RETAIL;

    SetLength(SectionContents[i], SectionHeaders[i].SizeOfRawData);

    pWriteCursor := @(SectionContents[i][0]);

    // Append prolog section
    CopyMemory(pWriteCursor, @(Prolog[0]), Length(Prolog));
    Inc(pWriteCursor, Length(Prolog));

    // Append xbe header
    CopyMemory(pWriteCursor, @(x_Xbe.m_Header), SizeOf(x_Xbe.m_Header));
    Inc(pWriteCursor, SizeOf(x_Xbe.m_Header));

    // Append xbe extra header bytes
    CopyMemory(pWriteCursor, x_Xbe.m_HeaderEx, x_Xbe.m_Header.dwSizeofHeaders - SizeOf(x_Xbe.m_Header));
    Dec(pWriteCursor, SizeOf(x_Xbe.m_Header));
    Inc(pWriteCursor, x_Xbe.m_Header.dwSizeofHeaders);

    // Append x_debug_FileName
    ZeroMemory(pWriteCursor, MAX_PATH);
    if aKernelDebugFileName <> '' then
      CopyMemory(pWriteCursor, @(AnsiString(aKernelDebugFileName)[1]), Length(AnsiString(aKernelDebugFileName)));
    Inc(pWriteCursor, MAX_PATH);

    // Append library versions
    for c := 1 to x_Xbe.m_Header.dwLibraryVersions do
    begin
      CopyMemory(pWriteCursor, @(x_Xbe.m_LibraryVersion[c - 1]), Sizeof(x_Xbe.m_LibraryVersion[c - 1]));
      Inc(pWriteCursor, SizeOf(x_xbe.m_LibraryVersion[c - 1]));
    end;

    // Append TLS data
    if Assigned(x_Xbe.m_TLS) then
    begin
      CopyMemory(pWriteCursor, x_Xbe.m_TLS, SizeOf(x_Xbe.m_TLS^));
      Inc(pWriteCursor, SizeOf(x_Xbe.m_TLS^));

      CopyMemory(pWriteCursor, Pointer(x_Xbe.GetTLSData()), x_Xbe.m_TLS.dwDataEndAddr - x_Xbe.m_TLS.dwDataStartAddr);
{$IFDEF KEEP_UNNECCESARY_CODE}
      Inc(pWriteCursor, x_Xbe.m_TLS.dwDataEndAddr - x_Xbe.m_TLS.dwDataStartAddr);
{$ENDIF}
    end;

    // patch prolog function parameters
    WriteCursor := SectionHeaders[i].VirtualAddress + NtHeaders.OptionalHeader.ImageBase + $100;

    // TODO : Determine the actual ImageBase of the used DLL :
    DLLBase := {CurrentDLLBase=}DWord(KrnlHandle) - {LoadTimeDLLBase=DLL.ImageBase=}DLL_IMAGE_BASE;

    // Function Pointer
    pEmuInit := GetProcAddress(KrnlHandle, CCXBXKRNLINIT);
    _WriteDWordToSectionPos(i, 1, DWord(pEmuInit) -  DLLBase);

    // Param 8 : Entry
    _WriteDWordToSectionPos(i, 6, ep);

    // Param 7 : dwXbeHeaderSize
    _WriteDWordToSectionPos(i, 11, x_Xbe.m_Header.dwSizeofHeaders);

    // Param 6 : pXbeHeader
    _WriteDWordToSectionPos(i, 16, WriteCursor);
    Inc(WriteCursor, x_Xbe.m_Header.dwSizeofHeaders);

    // Param 5 : szDebugFileName
    _WriteDWordToSectionPos(i, 21, WriteCursor);
    Inc(WriteCursor, MAX_PATH);

    // Param 4 : DbgMode
    _WriteDWordToSectionPos(i, 26, DWord(Ord(aKernelDebugMode)));

    // Param 3 : pLibraryVersion
    if Length(x_Xbe.m_LibraryVersion) <> 0 then
    begin
      _WriteDWordToSectionPos(i, 31, WriteCursor);
      Inc(WriteCursor, SizeOf(x_Xbe.m_LibraryVersion[0]) * x_Xbe.m_Header.dwLibraryVersions);
    end
    else
      _WriteDWordToSectionPos(i, 31, 0);

    // Param 2 : pTLS
    if Assigned(x_Xbe.m_TLS) then
    begin
      _WriteDWordToSectionPos(i, 36, WriteCursor);
      Inc(WriteCursor, SizeOf(x_Xbe.m_TLS^));
    end
    else
      _WriteDWordToSectionPos(i, 36, 0);

    // Param 1 : pTLSData
    if Assigned(x_Xbe.m_TLS) then
    begin
      _WriteDWordToSectionPos(i, 41, WriteCursor);
{$IFDEF KEEP_UNNECCESARY_CODE}
      Inc(WriteCursor, x_Xbe.m_TLS.dwDataEndAddr - x_Xbe.m_TLS.dwDataStartAddr);
{$ENDIF}
    end
    else
      _WriteDWordToSectionPos(i, 41, 0);

    // Param 0 : hwndParent
    _WriteDWordToSectionPos(i, 46, hwndParent);
  end;

  // ******************************************************************
  // * patch kernel thunk table
  // ******************************************************************
  WriteLog('EmuExe: Hijacking Kernel Imports...');
  // generate xbe sections
  kt := x_Xbe.m_Header.dwKernelImageThunkAddr;

  // decode kernel thunk address
  if (kt xor XOR_KT_DEBUG) > $01000000 then
    kt := kt xor XOR_KT_RETAIL
  else
    kt := kt xor XOR_KT_DEBUG;

  // locate section containing kernel thunk table
  if DLLToUse in [udCxbxKrnl, udCxbx] then
    KernelThunkTable := GetProcAddress(KrnlHandle, CXBXKRNL_KERNELTHUNKTABLE)
  else
  begin
    // Delphi doesn't support DLL's declaring an exported record,
    // so our thunk is actually a method returning the thunk table :
    GetKernelThunkTable := GetProcAddress(KrnlHandle, CXBXKRNL_KERNELTHUNKTABLE);
    Assert(Assigned(GetKernelThunkTable));
    // Call the method to get to the thunk table :
    KernelThunkTable := GetKernelThunkTable();
  end;

  Assert(Assigned(KernelThunkTable));

  imag_base := NtHeaders.OptionalHeader.ImageBase;
  for v := 0 to x_Xbe.m_Header.dwSections - 1 do
  begin
    virt_addr := SectionHeaders[v].VirtualAddress;
    virt_size := SectionHeaders[v].Misc.VirtualSize;

    // modify kernel thunk table, if found
    if ((kt >= imag_base + virt_addr) and (kt < imag_base + virt_addr + virt_size)) then
    begin
      WriteLog(Format('EmuExe: Located Thunk Table in Section 0x%.4x (0x%.8X)...', [v, kt]));
      kt_tbl := @(SectionContents[v][kt - virt_addr - imag_base]);
      k := 0;
      while kt_tbl[k] <> 0 do
      begin
        t := kt_tbl[k] and $7FFFFFFF;
        if  (t < NUMBER_OF_THUNKS)
        and (DWord(KernelThunkTable[t]) > NUMBER_OF_THUNKS) then
        begin
          _WriteDWordToAddr(@(kt_tbl[k]), DWord(KernelThunkTable[t]) - DLLBase);
          WriteLog(Format('EmuExe: Thunk %.3d : *0x%.8X := 0x%.8X', [t, kt + (k * 4), kt_tbl[k]]));
        end
        else
        begin
          _WriteDWordToAddr(@(kt_tbl[k]), t);
          WriteLog(Format('EmuExe: Out-of-range thunk %.3d : *0x%.8X := 0x%.8X', [t, kt + (k * 4), kt_tbl[k]]));
        end;
        
        Inc(k);
      end; // while
    end; // if

  end; // for

  // update imcomplete header fields
  WriteLog('EmuExe: Finalizing Exe file...');

  // calculate size of code / data / image
  begin
    SizeOf_Code := 0;
    SizeOf_Data := 0;
    SizeOf_Undata := 0;

    for v := 0 to NtHeaders.FileHeader.NumberOfSections - 1 do
    begin
      Characteristics := SectionHeaders[v].Characteristics;
      if ((Characteristics and IMAGE_SCN_MEM_EXECUTE) <> 0)
      or ((Characteristics and IMAGE_SCN_CNT_CODE) <> 0) then
        Inc(SizeOf_Code, SectionHeaders[v].SizeOfRawData);

      if (Characteristics and IMAGE_SCN_CNT_INITIALIZED_DATA) <> 0 then
        Inc(Sizeof_Data, SectionHeaders[v].SizeOfRawData);
    end;

    // calculate size of image
    // The RoundUp is necessary to allow the generated exe to run under Vista
    // This is actually a bug that needs to be corrected in CXBX too
    SizeOf_Image := SizeOf_Undata + SizeOf_Data + SizeOf_Code + RoundUp(NtHeaders.OptionalHeader.SizeOfHeaders, PE_HEADER_ALIGNMENT);
    SizeOf_Image := RoundUp(SizeOf_Image, PE_SECTION_ALIGNMENT);

    // update optional header as necessary
    NtHeaders.OptionalHeader.SizeOfCode := sizeof_code;
    NtHeaders.OptionalHeader.SizeOfInitializedData := sizeof_data;
    NtHeaders.OptionalHeader.SizeOfUninitializedData := sizeof_undata;
    NtHeaders.OptionalHeader.SizeOfImage := sizeof_image;
  end;

  // we'll set code base as the virtual address of the first section
  NtHeaders.OptionalHeader.BaseOfCode := SectionHeaders[0].VirtualAddress;

  // we'll set data base as the virtual address of the first section
  // that is not marked as containing code or being executable

  for v := 0 to NtHeaders.FileHeader.NumberOfSections - 1 do
  begin
    Characteristics := SectionHeaders[v].Characteristics;
    if ((Characteristics and IMAGE_SCN_MEM_EXECUTE) = 0)
    or ((Characteristics and IMAGE_SCN_CNT_CODE) = 0) then
    begin
      NtHeaders.OptionalHeader.BaseOfData := SectionHeaders[v].VirtualAddress;
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
