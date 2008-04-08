unit uEmuExe;

interface

uses uXbe, uExe, uEnums, Windows, SysUtils, BitsOps, uConsts, uProlog;

type
  TVarByteArray = array of Byte;
  TDWordArray = array[0..3] of char;
  TWordArray = array[0..1] of char;

  TEmuExe = class(TExe)
  private
  public
    constructor Create(m_Xbe: TXbe; m_KrnlDebug: DebugMode; m_KrnlDebugFilename: string; hwndParent: THandle);
  end;


implementation

uses
  uLog, uExternals, Dialogs;


{ TEmuExe }

//------------------------------------------------------------------------------

constructor TEmuExe.Create(m_Xbe: TXbe; m_KrnlDebug: DebugMode;
  m_KrnlDebugFilename: string; hwndParent: THandle);

  procedure AppendDWordToSection(SectionIdx: Integer; aDWord: DWord);
  var i: Integer;
    iPos: Integer;
    DWordArray: TDWordArray;
  begin
    DWordArray := TDWordArray(aDWord);
    iPos := Length(m_bzSection[SectionIdx]);
    SetLength(m_bzSection[SectionIdx], Length(m_bzSection[SectionIdx]) + Length(DWordArray));
    for i := 0 to Length(DWordArray) - 1 do begin
      m_bzSection[SectionIdx][iPos] := DWordArray[i];
      inc(iPos);
    end;
  end;

  procedure AppendDWordToSubSection(SectionIdx, iPos: Integer; aDWord: DWord);
  var i: Integer;
    DWordArray: TDWordArray;
  begin
    DWordArray := TDWordArray(aDWord);
    for i := 0 to Length(DWordArray) - 1 do begin
      m_bzSection[SectionIdx][iPos] := DWordArray[i];
      inc(iPos);
    end;
  end;

  procedure AppendProlog(SectionIdx: Integer);
  var i: Integer;
  begin
    // append prolog section
    SetLength(m_bzSection[SectionIdx], Length(m_bzSection[SectionIdx]) + Length(Prolog));
    for i := 0 to Length(Prolog) - 1 do begin
      m_bzSection[SectionIdx][i] := Char(Prolog[i]);
    end;
  end;

  procedure AppenddwMagic(SectionIdx: Integer);
  var i: Integer;
    iPos: Integer;
  begin
    // Append dwMagic to section
    iPos := Length(m_bzSection[SectionIdx]);
    SetLength(m_bzSection[SectionIdx], Length(m_bzSection[SectionIdx]) + Length(m_xbe.m_Header.dwMagic));
    for i := 0 to Length(m_xbe.m_Header.dwMagic) - 1 do begin
      m_bzSection[SectionIdx][iPos] := m_xbe.m_Header.dwMagic[i];
      Inc(iPos);
    end;
  end;

  procedure AppenddwMagicSubI(SectionIdx: Integer; iPos: Dword);
  var
    i: Integer;
  begin
    // Append dwMagic to section
    for i := 0 to Length(m_xbe.m_Header.dwMagic) - 1 do begin
      m_bzSection[SectionIdx][iPos] := m_xbe.m_Header.dwMagic[i];
      Inc(iPos);
    end;
  end;

  procedure AppendpbDigitalSignature(SectionIdx: Integer);
  var i: Integer;
    iPos: Integer;
  begin
    // Append pbDigitalSignature to section
    iPos := Length(m_bzSection[SectionIdx]);
    SetLength(m_bzSection[SectionIdx], Length(m_bzSection[SectionIdx]) + Length(m_Xbe.m_Header.pbDigitalSignature));
    for i := 0 to Length(m_Xbe.m_Header.pbDigitalSignature) - 1 do begin
      m_bzSection[SectionIdx][iPos] := m_Xbe.m_Header.pbDigitalSignature[i];
      Inc(iPos);
    end;
  end;

  procedure AppendpbDigitalSignatureSubI(SectionIdx: Integer; iPos: Dword);
  var
    i: Integer;
  begin
    // Append pbDigitalSignature to section
    for i := 0 to Length(m_Xbe.m_Header.pbDigitalSignature) - 1 do begin
      m_bzSection[SectionIdx][iPos] := m_Xbe.m_Header.pbDigitalSignature[i];
      Inc(iPos);
    end;
  end;



  procedure AppenddwInitFlags(SectionIdx: Integer);
  var i: Integer;
    iPos: Integer;
  begin
    // Append dwInitFlags to section
    iPos := Length(m_bzSection[SectionIdx]);
    SetLength(m_bzSection[SectionIdx], Length(m_bzSection[SectionIdx]) + Length(m_XBe.m_Header.dwInitFlags));
    for i := 0 to Length(m_XBe.m_Header.dwInitFlags) - 1 do begin
      m_bzSection[SectionIdx][iPos] := m_XBe.m_Header.dwInitFlags[i];
      Inc(iPos);
    end;
  end;

  procedure AppenddwInitFlagsSubI(SectionIdx: Integer; iPos: Dword);
  var
    i: Integer;
  begin
    for i := 0 to Length(m_XBe.m_Header.dwInitFlags) - 1 do begin
      m_bzSection[SectionIdx][iPos] := m_XBe.m_Header.dwInitFlags[i];
      Inc(iPos);
    end;
  end;

  procedure CopySections(Index: Integer);
  begin
    m_bzSection[Index] := copy(m_Xbe.m_bzSection[Index], 0, m_Xbe.m_SectionHeader[Index].dwSizeofRaw);
    SetLength(m_bzSection[Index], m_SectionHeader[Index].m_sizeof_raw);
  end;

  procedure AppendXbeHeader(SectionIdx: Integer; SubIndex: Dword);
  begin
    AppenddwMagicSubI(SectionIdx, $100); // Append dwMagic
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwMagic);
    AppendpbDigitalSignatureSubI(SectionIdx, SubIndex); // Append Digital signature
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.pbDigitalSignature);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwBaseAddr); // Append DwBaseAddr
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwBaseAddr);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwSizeofHeaders); // Append dwSizeofHeaders
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwSizeofHeaders);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwSizeofImage); // Append dwSizeofImage
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwSizeofImage);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwSizeofImageHeader); // Append dwSizeofImageHeader
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwSizeofImageHeader);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwTimeDate); // Append dwTimeDate
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwTimeDate);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwCertificateAddr); // Append dwCertificateAddr
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwCertificateAddr);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwSections); // Append dwSections
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwSections);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwSectionHeadersAddr); // Append dwSectionHeadersAddr
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwSectionHeadersAddr);
    AppenddwInitFlagsSubI(SectionIdx, SubIndex); // Append dwInitFlags
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwInitFlags);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwEntryAddr); // Append dwEntryAddr
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwEntryAddr);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwTLSAddr); // Append dwTLSAddr
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwTLSAddr);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwPeStackCommit); // Append dwPeStackCommit
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwPeStackCommit);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwPeHeapReserve); // Append dwPeHeapReserve
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwPeHeapReserve);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwPeHeapCommit); // Append dwPeHeapCommit
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwPeHeapReserve);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwPeBaseAddr); // Append dwPeBaseAddr
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwPeBaseAddr);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwPeSizeofImage); // Append dwPeSizeofImage
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwPeSizeofImage);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwPeChecksum); // Append dwPeChecksum
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwPeChecksum);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwPeTimeDate); // Append dwPeTimeDate
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwPeTimeDate);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwDebugPathnameAddr); // Append dwDebugPathNameAddr
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwDebugPathnameAddr);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwDebugFilenameAddr); // Append dwDebugFileNameAddr
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwDebugFilenameAddr);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwDebugUnicodeFilenameAddr); // Append dwDebugUnicodeFileNameAddr
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwDebugUnicodeFilenameAddr);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwKernelImageThunkAddr); // Append deKernelImageThunkAddr
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwKernelImageThunkAddr);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwNonKernelImportDirAddr); // Append dwNonKernelImportDirAddr
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwNonKernelImportDirAddr);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwLibraryVersions); // Append dwLibraryVersions
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwLibraryVersions);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwLibraryVersionsAddr); // Append dwLibraryVersionsAddr
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwLibraryVersionsAddr);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwKernelLibraryVersionAddr); // Append dwKernelLibraryVersionAddr
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwKernelLibraryVersionAddr);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwXAPILibraryVersionAddr); // Append dwXapiLibraryVersionAddr
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwXAPILibraryVersionAddr);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwLogoBitmapAddr); // Append dwLogoBitmapAddr
    SubIndex := SubIndex + sizeof(m_XBe.m_Header.dwLogoBitmapAddr);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_XBe.m_Header.dwSizeofLogoBitmap); // Append dwSizeofLogoBitmap
  end;

  procedure AppendXbeLibVersionSzName(SectionIdx: Integer; iPos: Dword; LibVersioNbr: Integer);
  var
    i: Integer;
  begin
    // Append pbDigitalSignature to section
    for i := 0 to 7 do begin
      m_bzSection[SectionIdx][iPos] := m_Xbe.m_LibraryVersion[LibVersioNbr].szName[i];
      Inc(iPos);
    end;
  end;

  procedure AppenddwLibVerFlagsSubI(SectionIdx: Integer; iPos: Dword; LibVersioNbr: Integer);
  var
    i: Integer;
  begin
    for i := 0 to Length(m_Xbe.m_LibraryVersion[LibVersioNbr].dwFlags) - 1 do begin
      m_bzSection[SectionIdx][iPos] := m_Xbe.m_LibraryVersion[LibVersioNbr].dwFlags[i];
      Inc(iPos);
    end;
  end;

  procedure AppendWordToSubSection(SectionIdx, iPos: Integer; aWord: Word);
  var i: Integer;
    WordArray: TWordArray;
  begin
    WordArray := TWordArray(aWord);
    for i := 0 to Length(WordArray) - 1 do begin
      m_bzSection[SectionIdx][iPos] := WordArray[i];
      inc(iPos);
    end;
  end;

  procedure AppendXbeLibVersion(SectionIdx: Integer; SubIndex: Dword; LibVersioNbr: Integer);
  begin
    AppendXbeLibVersionSzName(SectionIdx, SubIndex, LibVersioNbr);
    SubIndex := SubIndex + sizeOf(m_Xbe.m_LibraryVersion[LibVersioNbr].szName);
    AppendWordToSubSection(SectionIdx, SubIndex, m_Xbe.m_LibraryVersion[LibVersioNbr].wMajorVersion);
    SubIndex := SubIndex + sizeOf(m_Xbe.m_LibraryVersion[LibVersioNbr].wMajorVersion);
    AppendWordToSubSection(SectionIdx, SubIndex, m_Xbe.m_LibraryVersion[LibVersioNbr].wMinorVersion);
    SubIndex := SubIndex + sizeOf(m_Xbe.m_LibraryVersion[LibVersioNbr].wMinorVersion);
    AppendWordToSubSection(SectionIdx, SubIndex, m_Xbe.m_LibraryVersion[LibVersioNbr].wBuildVersion);
    SubIndex := SubIndex + sizeOf(m_Xbe.m_LibraryVersion[LibVersioNbr].wBuildVersion);
    AppenddwLibVerFlagsSubI(SectionIdx, SubIndex, LibVersioNbr);
  end;

  procedure AppendTLS(SectionIdx: Integer; SubIndex: Dword);
  begin
    AppendDWordToSubSection(SectionIdx, SubIndex, m_Xbe.m_TLS.dwDataStartAddr);
    SubIndex := SubIndex + sizeOf(m_Xbe.m_TLS.dwDataStartAddr);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_Xbe.m_TLS.dwDataEndAddr);
    SubIndex := SubIndex + sizeOf(m_Xbe.m_TLS.dwDataEndAddr);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_Xbe.m_TLS.dwTLSIndexAddr);
    SubIndex := SubIndex + sizeOf(m_Xbe.m_TLS.dwTLSIndexAddr);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_Xbe.m_TLS.dwTLSCallbackAddr);
    SubIndex := SubIndex + sizeOf(m_Xbe.m_TLS.dwTLSCallbackAddr);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_Xbe.m_TLS.dwSizeofZeroFill);
    SubIndex := SubIndex + sizeOf(m_Xbe.m_TLS.dwSizeofZeroFill);
    AppendDWordToSubSection(SectionIdx, SubIndex, m_Xbe.m_TLS.dwCharacteristics);
  end;

  procedure AppendTLSData(SectionIdx: Integer; SubIndex: Dword);
  var
    i: DWord;
    pTLSData: ^TVarCharArray;
  begin
    if m_Xbe.GetTLSData <> 0 then begin
      pTLSData := Pointer(m_Xbe.GetTLSData);
      for i := 0 to m_Xbe.m_TLS.dwDataEndAddr - m_Xbe.m_TLS.dwDataStartAddr - 1 do begin
        m_bzSection[SectionIdx][SubIndex + i] := pTLSData^[i];
      end;
    end;
  end;

var
  i, d, v, c: integer;

  dwSectionCursor: LongInt;
  RawAddr: LongInt;
  RawSize: LongInt;
  dwVirtAddr: LongInt;
  dwRawSize: LongInt;
  VirtSize: LongInt;
  VirtAddr: LongInt;
  Flags: DWord;
  SectionSize: LongInt;
  characteristics: DWord;

  raw_size: longint;
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
  kt_tbl: Word;

  pWriteCursor: DWord;
  WriteCursor: DWord;
  Flag: Byte;

  KrnlHandle: Thandle;
  pEmuInit: Pointer;

begin
  ConstructorInit();

  WriteLog('EmuExe: Generating Exe file...');

  // generate pe header
  m_Header.m_magic := IMAGE_NT_SIGNATURE;
  m_Header.m_machine := IMAGE_FILE_MACHINE_I386; // machine type : i386
  m_Header.m_sections := m_Xbe.m_Header.dwSections + 2; // xbe sections + .cxbximp + .cxbxplg
  m_Header.m_timedate := m_Xbe.m_Header.dwTimeDate; // time/date stamp
  m_Header.m_symbol_table_addr := 0; // unused
  m_Header.m_symbols := 0; // unused
  m_Header.m_sizeof_optional_header := sizeof(OptionalHeader); // size of optional header
  m_Header.m_characteristics := $010F; // should be fine..
  WriteLog('EmuExe: Generating PE header...OK');

  // generate optional header
  m_OptionalHeader.m_magic := $010B;

  // abitrary linker version : 6.0
  m_OptionalHeader.m_linker_version_major := $06;
  m_OptionalHeader.m_linker_version_minor := $00;

  // size of headers
  m_OptionalHeader.m_sizeof_headers := sizeof(bzDOSStub) + sizeof(m_Header);
  m_OptionalHeader.m_sizeof_headers := m_OptionalHeader.m_sizeof_headers + sizeof(m_OptionalHeader) + sizeof(SectionHeader) * m_Header.m_sections;
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

  // no special dll characteristics are necessary
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
  for v := 0 to m_Xbe.m_Header.dwSections - 1 do begin
    FillChar(m_SectionHeader[v].m_name, 8, Char(0));
    // generate xbe section name
    for c := 0 to 7 do begin
      m_SectionHeader[v].m_name[c] := m_Xbe.m_szSectionName[v][c];
      if Ord(m_SectionHeader[v].m_name[c]) = 0 then begin
        break;
      end;
    end;

    // generate xbe section virtual size / addr
    VirtSize := m_Xbe.m_SectionHeader[v].dwVirtualSize;
    VirtAddr := m_Xbe.m_SectionHeader[v].dwVirtualAddr - m_Xbe.m_Header.dwBaseAddr;

    m_SectionHeader[v].m_virtual_size := VirtSize;
    m_SectionHeader[v].m_virtual_addr := VirtAddr;

    // generate xbe section raw size / addr
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

    Flags := IMAGE_SCN_MEM_READ;
    // generate flags for this xbe section
    if GetBitEn(Ord(m_Xbe.m_SectionHeader[v].dwFlags[0]), 2) = 1 then begin // Executable
      Flags := Flags or IMAGE_SCN_MEM_EXECUTE or IMAGE_SCN_CNT_CODE;
    end
    else begin
      Flags := Flags or IMAGE_SCN_CNT_INITIALIZED_DATA;
    end;

    if GetBitEn(Ord(m_Xbe.m_SectionHeader[v].dwFlags[0]), 0) = 1 then begin // Writable
      Flags := Flags or IMAGE_SCN_MEM_WRITE;
    end;

    m_SectionHeader[v].m_characteristics := flags;

    WriteLog('EmuExe: Generating Section Header 0x%...' + IntToHex(v, 4) + ' OK');
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
  WriteLog('EmuExe: Generating Section Header 0x%..' + IntToHex(i, 4) + '(.cxbximp)... OK');

  //  generate .cxbxplg section header
  i := m_Header.m_sections - 1;

  m_SectionHeader[i].m_name := '.cxbxplg';

  // generate .cxbxplg section virtual size / addr
  virt_size := RoundUp(m_OptionalHeader.m_image_base + $100 + m_Xbe.m_Header.dwSizeofHeaders + 260
    + sizeof(m_Xbe.m_LibraryVersion) * m_Xbe.m_Header.dwLibraryVersions + sizeof(m_Xbe.m_TLS)
    + (m_Xbe.m_TLS.dwDataEndAddr - m_Xbe.m_TLS.dwDataStartAddr), $1000);
  virt_addr := RoundUp(m_SectionHeader[i - 1].m_virtual_addr + m_SectionHeader[i - 1].m_virtual_size, PE_SEGM_ALIGN);

  m_SectionHeader[i].m_virtual_size := virt_size;
  m_SectionHeader[i].m_virtual_addr := virt_addr;

  // our entry point should be the first bytes in this section
  m_OptionalHeader.m_entry := virt_addr;

  // generate .cxbxplg section raw size / addr
  raw_size := RoundUp(m_SectionHeader[i].m_virtual_size, PE_FILE_ALIGN);

  m_SectionHeader[i].m_sizeof_raw := raw_size;
  m_SectionHeader[i].m_raw_addr := dwSectionCursor;

  // relocation / line numbers will not exist
  m_SectionHeader[i].m_relocations_addr := 0;
  m_SectionHeader[i].m_linenumbers_addr := 0;

  m_SectionHeader[i].m_relocations := 0;
  m_SectionHeader[i].m_linenumbers := 0;

  // make this section readable and executable
  m_SectionHeader[i].m_characteristics := IMAGE_SCN_MEM_READ xor IMAGE_SCN_MEM_EXECUTE xor IMAGE_SCN_CNT_CODE;

  WriteLog('EmuExe: Generating Section Header 0x%..' + IntToHex(i, 4) + '(.cxbxplg)... OK');


  // GENERATE SECTIONS  ------ PART WE STUCK

  // generate sections
  WriteLog('EmuExe: Generating Sections...');

  SetLength(m_bzSection, m_Header.m_sections);


  // generate xbe sections
  for v := 0 to m_xbe.m_Header.dwSections - 1 do begin
    CopySections(v);
    WriteLog('EmuExe: Generating Section 0x' + IntToHex(v, 4) + '... OK');
  end;

  i := m_Header.m_sections - 2;

  dwVirtAddr := m_SectionHeader[i].m_virtual_addr;
  dwRawSize := m_SectionHeader[i].m_sizeof_raw;
  setlength(m_bzSection[i], dwRawSize);

  AppendDWordToSubSection(i, $00, dwVirtAddr + $38);
  m_bzSection[i][$04] := chr(0);
  AppendDWordToSubSection(i, $08, dwVirtAddr + $30);
  m_bzSection[i][$0C] := chr(0);

  m_bzSection[i][$10] := chr(0);
  AppendDWordToSubSection(i, $14, dwVirtAddr + $48);
  AppendDWordToSubSection(i, $18, dwVirtAddr + $00);
  m_bzSection[i][$1C] := chr(0);

  m_bzSection[i][$20] := chr(0);
  m_bzSection[i][$24] := chr(0);
  m_bzSection[i][$28] := chr(0);
  m_bzSection[i][$2C] := chr(0);

  AppendDWordToSubSection(i, $30, dwVirtAddr + $38);
  m_bzSection[i][$34] := chr(0);
  m_bzSection[i][$38] := chr($0001);

{$IFDEF DEBUG}
  m_bzSection[i][$3A] := '_';
  m_bzSection[i][$3B] := 'E';
  m_bzSection[i][$3C] := 'm';
  m_bzSection[i][$3D] := 'u';
  m_bzSection[i][$3E] := 'N';
  m_bzSection[i][$3F] := 'o';
  m_bzSection[i][$40] := 'F';
  m_bzSection[i][$41] := 'u';
  m_bzSection[i][$42] := 'n';
  m_bzSection[i][$43] := 'c';
  m_bzSection[i][$44] := '@';
  m_bzSection[i][$45] := '0';
  m_bzSection[i][$46] := chr(0);
  m_bzSection[i][$47] := chr(0);
  m_bzSection[i][$48] := 'D';
  m_bzSection[i][$49] := 'x';
  m_bzSection[i][$4A] := 'b';
  m_bzSection[i][$4B] := 'x';
  m_bzSection[i][$4C] := 'K';
  m_bzSection[i][$4D] := 'r';
  m_bzSection[i][$4E] := 'n';
  m_bzSection[i][$4F] := 'l';
  m_bzSection[i][$50] := '.';
  m_bzSection[i][$51] := 'd';
  m_bzSection[i][$52] := 'l';
  m_bzSection[i][$53] := 'l';
  m_bzSection[i][$54] := chr(0);
  m_bzSection[i][$55] := chr(0);
{$ELSE}
  m_bzSection[i][$3A] := '_';
  m_bzSection[i][$3B] := 'E';
  m_bzSection[i][$3C] := 'm';
  m_bzSection[i][$3D] := 'u';
  m_bzSection[i][$3E] := 'N';
  m_bzSection[i][$3F] := 'o';
  m_bzSection[i][$40] := 'F';
  m_bzSection[i][$41] := 'u';
  m_bzSection[i][$42] := 'n';
  m_bzSection[i][$43] := 'c';
  m_bzSection[i][$44] := '@';
  m_bzSection[i][$45] := '0';
  m_bzSection[i][$46] := chr(0);
  m_bzSection[i][$47] := chr(0);
  m_bzSection[i][$48] := 'D';
  m_bzSection[i][$49] := 'x';
  m_bzSection[i][$4A] := 'b';
  m_bzSection[i][$4B] := 'x';
  m_bzSection[i][$4C] := '.';
  m_bzSection[i][$4D] := 'd';
  m_bzSection[i][$4E] := 'l';
  m_bzSection[i][$4F] := 'l';
  m_bzSection[i][$50] := chr(0);
  m_bzSection[i][$51] := chr(0);
  m_bzSection[i][$52] := chr(0);
  m_bzSection[i][$53] := chr(0);
  m_bzSection[i][$54] := chr(0);
  m_bzSection[i][$55] := chr(0);
{$ENDIF}

  ep := m_Xbe.m_Header.dwEntryAddr;
  i := m_Header.m_sections - 1;

  // decode entry point
  if ((ep xor XOR_EP_RETAIL) > $01000000) then
    ep := ep xor XOR_EP_DEBUG
  else
    ep := ep xor XOR_EP_RETAIL;

  setlength(m_bzSection[i], m_SectionHeader[i].m_sizeof_raw);

  // append prolog section
  AppendProlog(i);
  pWriteCursor := $100;

  // append xbe header
  AppendXbeHeader(i, pWriteCursor);
  pWriteCursor := pWriteCursor + SizeOf(m_Xbe.m_Header);

  // append xbe extra header bytes
  for c := 0 to (m_Xbe.m_Header.dwSizeofHeaders - sizeOf(m_Xbe.m_Header)) - 1 do begin
    m_bzSection[i][pWriteCursor] := m_Xbe.m_HeaderEx[c];
    Inc(pWriteCursor);
  end;

  // append x_debug_filename
  for c := length(m_KrnlDebugFilename) to 259 do begin
    m_bzSection[i][pWriteCursor] := chr(0);
    Inc(pWriteCursor);
  end;

  // append library versions
  for c := 0 to m_xbe.m_Header.dwLibraryVersions - 1 do begin
    AppendXbeLibVersion(i, pWriteCursor, c);
    pWriteCursor := pWriteCursor + sizeOf(m_xbe.m_LibraryVersion[c]);
  end;

  // append TLS data
  if (sizeOf(m_Xbe.m_TLS) <> 0) then begin
    AppendTLS(i, pWriteCursor);
    pWriteCursor := pWriteCursor + SizeOf(m_Xbe.m_TLS);
    AppendTLSData(i, pWriteCursor);
    pWriteCursor := pWriteCursor + m_Xbe.m_TLS.dwDataEndAddr - m_Xbe.m_TLS.dwDataStartAddr;
  end;

  // patch prolog function parameters
  WriteCursor := m_SectionHeader[i].m_virtual_addr + m_optionalHeader.m_image_base + $100;

  // Function Pointer
  pEmuInit := @EmuInit; // We need to access the procedure once so it's in memory
  KrnlHandle := GetModuleHandle('DxbxKrnl.dll');
  if KrnlHandle >= 32 then begin
    pEmuInit := GetProcAddress(KrnlHandle, '_EmuInit@36');
    AppendDWordToSubSection(i, 1, DWord(pEmuInit));
  end;
  FreeLibrary(KrnlHandle);

  // Param 8 : Entry
  AppendDWordToSubSection(i, 6, ep);

  // Param 7 : dwXbeHeaderSize
  AppendDWordToSubSection(i, 11, m_Xbe.m_Header.dwSizeofHeaders);

  // Param 6 : pXbeHeader
  AppendDWordToSubSection(i, 16, WriteCursor);
  WriteCursor := WriteCursor + m_Xbe.m_Header.dwSizeofHeaders;

  // Param 5 : szDebugFilename
  AppendDWordToSubSection(i, 21, WriteCursor);
  WriteCursor := WriteCursor + 260;

  // Param 4 : DbgMode
  AppendDWordToSubSection(i, 26, DWord(m_KrnlDebug));

  // Param 3 : pLibraryVersion
  if (length(m_Xbe.m_LibraryVersion) <> 0) then begin
    AppendDWordToSubSection(i, 31, WriteCursor);
    WriteCursor := WriteCursor + (16 * m_xbe.m_Header.dwLibraryVersions);
  end
  else
  begin
    AppendDWordToSubSection(i, 31, 0);
  end;

  // Param 2 : pTLS
  if (sizeOf(m_Xbe.m_TLS) <> 0) then begin
    AppendDWordToSubSection(i, 36, WriteCursor);
    WriteCursor := WriteCursor + sizeof(m_Xbe.m_TLS);
  end
  else
  begin
    AppendDWordToSubSection(i, 36, 0);
  end;

 // Param 1 : pTLSData
  if (sizeOf(m_Xbe.m_TLS) <> 0) then begin
    AppendDWordToSubSection(i, 41, WriteCursor);
    WriteCursor := WriteCursor + m_Xbe.m_TLS.dwDataEndAddr - m_Xbe.m_TLS.dwDataStartAddr;
  end
  else
  begin
    AppendDWordToSubSection(i, 41, 0);
  end;

  // Param 0 : hwndParent
  AppendDWordToSubSection(i, 46, hwndParent);


  // END GENERATE SECTIONS  ------ WE STUCK HERE
 // ******************************************************************
 // * patch kernel thunk table
 // ******************************************************************
 (* WriteLog('EmuExe: Hijacking Kernel Imports...');
  // generate xbe sections
  kt := m_Xbe.m_Header.dwKernelImageThunkAddr;

  // decode kernel thunk address
  if ((kt xor XOR_KT_DEBUG) > $01000000) then begin
    kt := kt xor XOR_KT_RETAIL
  end
  else
  begin
    kt := kt xor XOR_KT_DEBUG;
  end;

  for v := 0 to m_Xbe.m_Header.dwSections -1 do begin
    imag_base := m_OptionalHeader.m_image_base;
    virt_addr := m_SectionHeader[v].m_virtual_addr;
    virt_size := m_SectionHeader[v].m_virtual_size;
    if ((kt >= virt_addr + imag_base) and (kt < virt_addr + virt_size + imag_base)) then begin
      WriteLog(Format('EmuExe: Located Thunk Table in Section 0x%.04X (0x%.08X)...', [v, kt]));
      m_bzSection[v][kt - virt_addr - imag_base] := m_bzSection[v][kt - virt_addr - imag_base];

    end;
  end;*)


  // update imcomplete header fields
  // calculate size of code / data / image
  (*SizeOf_Code := 0;
  SizeOf_Data := 0;
  SizeOf_Undata := 0;

  for v := 0 to m_Header.m_sections - 1 do begin
    characteristics := m_SectionHeader[v].m_characteristics;
    if ((characteristics and IMAGE_SCN_MEM_EXECUTE) <> 0) or ((characteristics and IMAGE_SCN_CNT_CODE) <> 0) then begin
      SizeOf_Code := SizeOf_Code + m_SectionHeader[v].m_SizeOf_Raw;
    end;

    if (characteristics and IMAGE_SCN_CNT_INITIALIZED_DATA) <> 0 then begin
      Sizeof_data := Sizeof_Data + m_SectionHeader[v].m_sizeof_raw;
    end;
  end;*)

  // calculate size of image
 { SizeOf_Image := SizeOf_Undata + SizeOf_Data + SizeOf_Code + RoundUp(m_OptionalHeader.m_sizeof_headers, $1000);
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

  for v := 0 to m_Header.m_sections - 1 do begin
    characteristics := m_SectionHeader[v].m_characteristics;
    if (not ((characteristics and IMAGE_SCN_MEM_EXECUTE) <> 0) or not ((characteristics and IMAGE_SCN_CNT_CODE) <> 0)) then begin
      m_optionalHeader.m_data_base := m_SectionHeader[v].m_virtual_addr;
      Break;
    end;
  end;   }

  WriteLog('EmuExe: Finalizing Exe Files...OK');
end; // TEmuExe.Create

//------------------------------------------------------------------------------

end.

