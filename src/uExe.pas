unit uExe;

interface

uses
  Dialogs, Windows, Classes, SysUtils;

type
  TVarCharArray = array of Char;

  DOSStub = array[0..183] of Byte;
  EndFilling1 = array[0..0] of Byte;
  EndFilling2 = array[0..0] of Byte;

  // DOSHeader
  DOSHeader = packed record
    m_magic: Word; // DOS .EXE magic number
    m_cblp: Word; // byte on last page
    m_cp: Word; // number of pages
    m_crlc: Word; // number of relocations
    m_cparhdr: Word; // size of header (in paragraphs)
    m_minalloc: Word; // minimum extra paragraphs needed
    m_maxalloc: Word; // maximum extra paragraphs needed
    m_ss: Word; // initial SS value (relative)
    m_sp: Word; // initial SP value
    m_csum: Word; // checksum
    m_ip: Word; // initial IP value
    m_cs: Word; // initial CS value (relative)
    m_lfarlc: Word; // file address of relocation table
    m_ovno: Word; // overlay number
    m_res: array[0..3] of Word; // reserved words
    m_oemid: Word; // OEM identifier
    m_oeminfo: Word; // OEM information
    m_res2: array[0..9] of Word; // reserved words
    m_lfanew: DWord; // file address of new .EXE header
  end;

  // Header (PE)
  Header = packed record
    m_magic: DWord; // magic number [should be "PE\0\0"]
    m_machine: Word; // machine type
    m_sections: Word; // number of sections
    m_timedate: DWord; // timedate stamp
    m_symbol_table_addr: DWord; // symbol table address
    m_symbols: Dword; // number of symbols
    m_sizeof_optional_header: Word; // size of optional header
    m_characteristics: Word; // characteristics
  end;

  // (PE)
  //struct image_data_directory             // image data directory
  image_data_directory = packed record
    m_virtual_addr: Dword;
    m_size: DWord;
  end;

  OptionalHeader = packed record
    m_magic: Word; // magic number [should be 0x010B]
    m_linker_version_major: byte; // linker version [major]
    m_linker_version_minor: byte; // linker version [minor]
    m_sizeof_code: DWord; // size of code
    m_sizeof_initialized_data: DWord; // size of initialized data
    m_sizeof_uninitialized_data: DWord; // size of uninitialized data
    m_entry: DWord; // address of entry point
    m_code_base: DWord; // address of code base
    m_data_base: DWord; // address of data base
    m_image_base: DWord; // address of image base
    m_section_alignment: Dword; // section alignment
    m_file_alignment: DWord; // file alignment
    m_os_version_major: Word; // operating system version [major]
    m_os_version_minor: Word; // operating system version [minor]
    m_image_version_major: Word; // image version [major]
    m_image_version_minor: Word; // image version [minor]
    m_subsystem_version_major: Word; // subsystem version [major]
    m_subsystem_version_minor: Word; // subsystem version [minor]
    m_win32_version: DWord; // win32 version
    m_sizeof_image: DWord; // size of image
    m_sizeof_headers: DWord; // size of headers
    m_checksum: DWord; // checksum
    m_subsystem: Word; // subsystem
    m_dll_characteristics: Word; // dll characteristics
    m_sizeof_stack_reserve: DWord; // size of stack reserve
    m_sizeof_stack_commit: DWord; // size of stack commit
    m_sizeof_heap_reserve: DWord; // size of heap reserve
    m_sizeof_heap_commit: DWord; // size of heap commit
    m_loader_flags: DWord; // loader flags
    m_data_directories: DWord; // data directories
    m_image_data_directory: array[0..15] of image_data_directory;
  end;

  // PE Section Header
  SectionHeader = packed record
    m_name: array[0..7] of char; // name of section
    m_virtual_size: DWord; // virtual size of segment
    m_virtual_addr: DWord; // virtual address of segment
    m_sizeof_raw: DWord; // size of raw data
    m_raw_addr: DWord; // address of raw data
    m_relocations_addr: DWord; // address of relocations
    m_linenumbers_addr: DWord; // address of line numbers
    m_relocations: Word; // number of relocations
    m_linenumbers: Word; // number of linenumbers
    m_characteristics: DWord; // characteristics for this segment
  end;

  TExe = class
  private
  public
    m_DOSHeader: DOSHeader;
    m_Header: Header;

    m_OptionalHeader: OptionalHeader;
    m_SectionHeader: array of SectionHeader;

    m_bzSection: array of TVarCharArray;

    constructor Create(x_szFilename: string);
    function GetAddr(x_dwVirtualAddress: DWord): ShortInt;
    function doExport(const x_szExeFilename: string): Boolean;
    procedure ConstructorInit;
  end;


const
  bzDOSStub: DOSStub = ($4D, $5A, $90, $00, $03, $00, $00, $00,
    $04, $00, $00, $00, $FF, $FF, $00, $00,
    $B8, $00, $00, $00, $00, $00, $00, $00,
    $40, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $B8, $00, $00, $00,
    $0E, $1F, $BA, $0E, $00, $B4, $09, $CD,
    $21, $B8, $01, $4C, $CD, $21, $54, $68,
    $69, $73, $20, $70, $72, $6F, $67, $72,
    $61, $6D, $20, $63, $61, $6E, $6E, $6F,
    $74, $20, $62, $65, $20, $72, $75, $6E,
    $20, $69, $6E, $20, $44, $4F, $53, $20,
    $6D, $6F, $64, $65, $2E, $0D, $0D, $0A,
    $24, $00, $00, $00, $00, $00, $00, $00,
    $85, $E3, $B8, $DB, $C1, $82, $D6, $88,
    $C1, $82, $D6, $88, $C1, $82, $D6, $88,
    $C1, $82, $D7, $88, $C3, $82, $D6, $88,
    $3E, $A2, $D2, $88, $C2, $82, $D6, $88,
    $95, $A1, $E7, $88, $C0, $82, $D6, $88,
    $52, $69, $63, $68, $C1, $82, $D6, $88,
    $00, $00, $00, $00, $00, $00, $00, $00);

  bzEndFilling1: EndFilling1 = ($CC);
  bzEndFilling2: EndFilling2 = ($CD);


implementation

uses
  uLog;


{ TExe }

//------------------------------------------------------------------------------

procedure TExe.ConstructorInit;
begin
  m_SectionHeader := nil;
  m_bzSection := nil;
end; // TExe.ConstructorInit

//------------------------------------------------------------------------------

constructor TExe.Create(x_szFilename: string);
var
  ExeFile: TFileStream;
begin
  ConstructorInit();

  ExeFile := TFileStream.Create(x_szFileName, fmOpenRead);

  // verify exe file was opened
  if ExeFile.Size < 0 then begin
    WriteLog('Could not open Exe file.');
    Exit;
  end;
  WriteLog('Exe: Opening Exe file... Ok');

  // ignore dos stub (if it exists)
  try
    ExeFile.Read(m_DOSHeader.m_magic, SizeOf(m_DosHeader.m_magic));
  except
    messageDlg('Unexpected read error while reading magic number', mtError, [mbOk], 0);
  end;

  (*if (m_DOSHeader.m_magic = 'MZ') then begin
        {
            printf("Found, Ignoring...");

            if(fread(&m_DOSHeader.m_cblp, sizeof(m_DOSHeader)-2, 1, ExeFile) != 1)
            {
                SetError("Unexpected read error while reading DOS stub", true);
                goto cleanup;
            }

            fseek(ExeFile, m_DOSHeader.m_lfanew, SEEK_SET);

            printf("OK\n");
        }

  end
  else begin
    WriteLog('Exe: Reading DOS stub... OK');
  end;  *)
end;

function TExe.doExport(const x_szExeFilename: string): Boolean;
var
  ExeFile: TFileStream;
  lIndex: integer;
  RawSize, RawAddr: DWord;
begin
  Result := True;
  ExeFile := TFileStream.Create(x_szExeFilename, fmCreate);
  try
  except
    Result := False;
    WriteLog('Export: Could not open .exe file.');
    Exit;
  end;
  WriteLog('Export: Opening Exe file...OK');

  // write dos stub
  try
    ExeFile.Write(bzDosStub, SizeOf(bzDosStub))
  except
    Result := False;
    MessageDlg('Could not write dos stub', mtError, [mbOk], 0);
    WriteLog('Export: Could not write dos stub');
    exit;
  end;

  WriteLog('Export: Writting DOS stub...OK');

  // write pe header
  try
    ExeFile.Write(m_Header, SizeOf(m_Header));
  except
    Result := False;
    WriteLog('Export: Could not write PE header');
    MessageDlg('Could not write PE header', mtError, [mbOk], 0);
    Exit;
  end;
  WriteLog('Export: Writing PE Header...OK');

  // write optional header
  try
    ExeFile.Write(m_OptionalHeader, SizeOf(m_OptionalHeader));
  except
    Result := False;
    WriteLog('Export: Could not write PE optional header');
    MessageDlg('Could not write PE optional header', mtError, [mbOk], 0);
    exit;
  end;
  WriteLog('Export: Writing Optional Header...OK');

  // write section headers
  for lIndex := 0 to m_Header.m_sections - 1 do begin
    WriteLog('Export: Writing Section Header 0x' + IntToStr(lIndex) + '04X...');
    try
      ExeFile.Write(m_Sectionheader[lIndex], SIzeOf(m_SectionHeader[lIndex]));
    except
      WriteLog('Export: Could not write PE section header ' + IntToStr(lIndex));
      MessageDlg('Could not write PE section header ' + IntToStr(lIndex), mtError, [mbOk], 0);
      Result := False;
      exit;
    end;
  end;
  WriteLog('Export: Writing Section Headers...OK');

  // write sections
  for lIndex := 0 to m_Header.m_sections - 1 do begin

    //Debug info of turok from cxbx
    //m_Header.m_sections = 13
    //v = 0   RawwSize = 1578272  RawAddr = 4096
    //v = 1   RawwSize = 76960    RawAddr = 1582368
    //v = 2   RawwSize = 1344     RawAddr = 1659328
    //v = 3   RawwSize = 36544    RawAddr = 1660672
    //v = 4   RawwSize = 121280   RawAddr = 1697216
    //v = 5   RawwSize = 160448   RawAddr = 1818496
    //v = 6   RawwSize = 29024    RawAddr = 1978944
    //v = 7   RawwSize = 246624   RawAddr = 2007968
    //v = 8   RawwSize = 140192   RawAddr = 2254592
    //v = 9   RawwSize =  29056   RawAddr = 2394784
    //v = 10  RawwSize = 10240    RawAddr = 2423840
    //v = 11  RawwSize = 128      RawAddr = 2434080
    //v = 12  RawwSize = 69632    RawAddr = 2434208
    //v = 13  RawwSize = 577      RawAddr = 305
    RawSize := m_SectionHeader[lIndex].m_sizeof_raw;
    RawAddr := m_SectionHeader[lIndex].m_raw_addr;

    ExeFIle.Seek(RawAddr, soFromBeginning);

    if RawSize = 0 then begin
      Result := False;
    end
    else begin
      try
        ExeFIle.Write(Pointer(m_bzSection[lIndex])^, RawSize);
      except
        WriteLog('Export: Could not write PE section ' + IntToStr(lIndex));
        MessageDlg('Could not write PE section ' + IntToStr(lIndex), mtError, [mbOk], 0);
        WriteLog('Export: Writing Section 0x' + IntToStr(lIndex) + '.04X OK');
        Exit;
      end;
    end;
    WriteLog('Export: Writing Sections...OK');
  end;

 (* ExeFIle.Seek( 2437340, soFromBeginning );
  for i := 0 to 963 do begin
   ExeFile.Write( bzEndFilling1, Length ( bzEndFilling1 ) );
  end;

  for i := 0 to 65535 do begin
    ExeFile.Write( bzEndFilling2, Length ( bzEndFilling2 ) );
  end;    *)

  ExeFile.Free;

end; // TExe.doExport

//------------------------------------------------------------------------------

function TExe.GetAddr(x_dwVirtualAddress: DWord): ShortInt;
begin
//  for(uint32 v=0;v<m_Header.m_sections;v++)
    {
        uint32 virt_addr = m_SectionHeader[v].m_virtual_addr;
        uint32 virt_size = m_SectionHeader[v].m_virtual_size;

        if( (x_dwVirtualAddress >= virt_addr) && (x_dwVirtualAddress < (virt_addr + virt_size)) )
            return &m_bzSection[v][x_dwVirtualAddress - virt_addr];
    }

  //  return 0;
  Result := 0;
end;

//------------------------------------------------------------------------------

end.
