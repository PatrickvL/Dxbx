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
unit uXbe;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows, // for DWord
  Classes,
  Controls,
  SysUtils, // for Format
  StrUtils, // for IfThen
  Dialogs, // for MessageDlg
  Graphics, // for TBitmap
  // Dxbx
  uConsts,
  uTypes,
  uTime,
  uLog,
  uDxbxUtils;

const
  XBE_INIT_FLAG_MountUtilityDrive = $00000001;
  XBE_INIT_FLAG_FormatUtilityDrive = $00000002;
  XBE_INIT_FLAG_Limit64MB = $00000004;
  XBE_INIT_FLAG_DontSetupHarddisk = $00000008;

  XBE_SECTIONHEADER_FLAG_Writable = $00000001;
  XBE_SECTIONHEADER_FLAG_Preload = $00000002;
  XBE_SECTIONHEADER_FLAG_Executable = $00000004;
  XBE_SECTIONHEADER_FLAG_InsertedFile = $00000008;
  XBE_SECTIONHEADER_FLAG_HeadPageRO = $00000010;
  XBE_SECTIONHEADER_FLAG_TailPageRO = $00000020;

  XBE_LIBRARYVERSION_FLAG_ApprovedNo = $00;
  XBE_LIBRARYVERSION_FLAG_ApprovedPossibly = $20;
  XBE_LIBRARYVERSION_FLAG_ApprovedYes = $40;
  XBE_LIBRARYVERSION_FLAG_ApprovedMask = $60;
  XBE_LIBRARYVERSION_FLAG_DebugBuild = $80;

type
  // Note : These types are copied from JwaWinType, so we don't have to include that unit :
  PCSZ = ^AnsiChar; // Dxbx assumption!
  PVOID = Pointer;
  LONG = Longint;

type
  TLogType = (ltLog, ltFile);
  TFileType = (ftXbe, ftExe);

  XBE_HEADER = packed record
    dwMagic: array[0..3] of AnsiChar; // 0x0000 - magic number [should be "XBEH"]
    pbDigitalSignature: array[0..255] of Byte; // 0x0004 - digital signature
    dwBaseAddr: DWord; // 0x0104 - base address
    dwSizeofHeaders: DWord; // 0x0108 - size of headers
    dwSizeofImage: DWord; // 0x010C - size of image
    dwSizeofImageHeader: DWord; // 0x0110 - size of image header
    dwTimeDate: DWord; // 0x0114 - timedate stamp
    dwCertificateAddr: DWord; // 0x0118 - certificate address
    dwSections: DWord; // 0x011C - number of sections
    dwSectionHeadersAddr: DWord; // 0x0120 - section headers address

    dwInitFlags: array[0..3] of Byte; // 0x0124 - initialization flags

    dwEntryAddr: DWord; // 0x0128 - entry point address
    dwTLSAddr: DWord; // 0x012C - thread local storage directory address
    dwPeStackCommit: DWord; // 0x0130 - size of stack commit
    dwPeHeapReserve: DWord; // 0x0134 - size of heap reserve
    dwPeHeapCommit: DWord; // 0x0138 - size of heap commit
    dwPeBaseAddr: DWord; // 0x013C - original base address
    dwPeSizeofImage: DWord; // 0x0140 - size of original image
    dwPeChecksum: DWord; // 0x0144 - original checksum
    dwPeTimeDate: DWord; // 0x0148 - original timedate stamp
    dwDebugPathNameAddr: DWord; // 0x014C - debug pathname address
    dwDebugFileNameAddr: DWord; // 0x0150 - debug FileName address
    dwDebugUnicodeFileNameAddr: DWord; // 0x0154 - debug unicode FileName address
    dwKernelImageThunkAddr: DWord; // 0x0158 - kernel image thunk address
    dwNonKernelImportDirAddr: DWord; // 0x015C - non kernel import directory address
    dwLibraryVersions: DWord; // 0x0160 - number of library versions
    dwLibraryVersionsAddr: DWord; // 0x0164 - library versions address
    dwKernelLibraryVersionAddr: DWord; // 0x0168 - kernel library version address
    dwXAPILibraryVersionAddr: DWord; // 0x016C - xapi library version address
    dwLogoBitmapAddr: DWord; // 0x0170 - logo bitmap address
    dwSizeofLogoBitmap: DWord; // 0x0174 - logo bitmap size
  end;
  PXBE_HEADER = ^XBE_HEADER;

  XBE_CERTIFICATE = packed record
    dwSize: DWord; // 0x0000 - size of certificate
    dwTimeDate: DWord; // 0x0004 - timedate stamp
    dwTitleId: DWord; // 0x0008 - title id
    wszTitleName: array[0..39] of WideChar; // 0x000C - title name (unicode)
    dwAlternateTitleId: array[0..15] of Dword; // 0x005C - alternate title ids
    dwAllowedMedia: Dword; // 0x009C - allowed media types
    dwGameRegion: DWord; // 0x00A0 - game region
    dwGameRatings: DWord; // 0x00A4 - game ratings
    dwDiskNumber: DWord; // 0x00A8 - disk number
    dwVersion: Dword; // 0x00AC - version
    bzLanKey: array[0..15] of AnsiChar; // 0x00B0 - lan key
    bzSignatureKey: array[0..15] of AnsiChar; // 0x00C0 - signature key
    bzTitleAlternateSignatureKey: array[0..15] of array[0..15] of AnsiChar; // 0x00D0 - alternate signature keys
  end;
  PXBE_CERTIFICATE = ^XBE_CERTIFICATE;


  // Source: Cxbx
  XBE_SECTIONHEADER = packed record
    dwFlags: array[0..3] of Byte;
    dwVirtualAddr: DWord; // virtual address
    dwVirtualSize: DWord; // virtual size
    dwRawAddr: DWord; // file offset to raw Data
    dwSizeofRaw: DWord; // size of raw Data
    dwSectionNameAddr: DWord; // section name addr
    dwSectionRefCount: DWord; // section reference count
    dwHeadSharedRefCountAddr: DWord; // head shared page reference count address
    dwTailSharedRefCountAddr: DWord; // tail shared page reference count address
    bzSectionDigest: array[0..19] of AnsiChar; // section digest
  end;

  // Section headers - Source: XBMC
  XBE_SECTION = packed record
    Flags: ULONG;
    VirtualAddress: PVOID; // Virtual address (where this section loads in RAM)
    VirtualSize: ULONG; // Virtual size (size of section in RAM; after FileSize it's 00'd)
    FileAddress: ULONG; // File address (where in the file from which this section comes)
    FileSize: ULONG; // File size (size of the section in the XBE file)
    SectionName: PCSZ; // Pointer to section name
    SectionReferenceCount: LONG; // Section reference count - when >= 1, section is loaded
    HeadReferenceCount: PWORD; // Pointer to head shared page reference count
    TailReferenceCount: PWORD; // Pointer to tail shared page reference count
    ShaHash: array[0..5 - 1] of DWord; // SHA hash.  Hash DWORD containing FileSize, then hash section.
  end; // SizeOf() = 38
  PXBE_SECTION = ^XBE_SECTION;

  // Dxbx TODO : Should we use _XBE_SECTIONHEADER or _XBE_SECTION ?

  XBE_LIBRARYVERSION = packed record
    szName: array[0..7] of AnsiChar; // library name
    wMajorVersion: Word; // major version
    wMinorVersion: Word; // minor version
    wBuildVersion: Word; // build version
    dwFlags: array[0..1] of Byte;
           { struct Flags
            {
                uint16 QFEVersion       : 13;      // QFE Version
                uint16 Approved         : 2;       // Approved? (0:no, 1:possibly, 2:yes)
                uint16 bDebugBuild      : 1;       // Is this a debug build?
            }
            //dwFlags;
  end;
  PXBE_LIBRARYVERSION = ^XBE_LIBRARYVERSION;

  XBE_TLS = packed record
    dwDataStartAddr: DWord; // raw start address
    dwDataEndAddr: DWord; // raw end address
    dwTLSIndexAddr: DWord; // tls index  address
    dwTLSCallbackAddr: DWord; // tls callback address
    dwSizeofZeroFill: DWord; // size of zero fill
    dwCharacteristics: DWord; // characteristics
  end;
  PXBE_TLS = ^XBE_TLS;

  _Eight = Byte; // AnsiChar?
    //Bit 0  : bType1
    //Bit 1,2,3  : Len
    //Bit 4,5,6,7 : Data
  Eight = _Eight;

  _Sixteen = array[0..1] of Byte; // AnsiChar? Word?
    //from Char[0]
    //Bit 0  : bType1
    //Bit 1  : bType2
    //Bit 2,3,4,5,6,7 :Len
    //from Char[2]
    //Bit 0,1,2,3  : Len
    //Bit 4,5,6,7 : Data
  Sixteen = _Sixteen;

  // used to encode / decode logo bitmap Data
  _LogoRLE = packed record
    m_Eight: Eight;
    m_Sixteen: Sixteen;
  end;
  LogoRLE = _LogoRLE;

  TXbe = class(TObject)
  private
    MyFile: TMemoryStream;
    Buffer: MathPtr;
    m_KernelLibraryVersion: array of AnsiChar;
    m_XAPILibraryVersion: array of AnsiChar;
    m_Certificate: XBE_CERTIFICATE;
    procedure ConstructorInit;
  protected
    XbeFile: file of Byte;
    m_LogoRLE: LogoRLE;
  public
    m_szPath: string;
    m_Header: XBE_HEADER;
    m_SectionHeader: array of XBE_SECTIONHEADER;
    m_LibraryVersion: array of XBE_LIBRARYVERSION;
    m_szSectionName: array of array of AnsiChar;
    m_HeaderEx: array of Byte;
    m_TLS: PXBE_TLS;
    m_bzSection: array of TVarByteArray;

    constructor Create(aFileName: string; aFileType: TFileType);
    destructor Destroy; override;

    function DetermineDumpFileName: string;
    function DumpInformation(FileName: string = ''): Boolean;
    function GetAddr(x_dwVirtualAddress: DWord): Integer;

    procedure ExportLogoBitmap(ImgCont: TBitmap);

    function GetTLSData: DWord;
  end;

function GetDWordVal(aBuffer: MathPtr; i: Integer): DWord;
function GetWordVal(aBuffer: MathPtr; i: Integer): Word;

function OpenXbe(aFileName: string; var aXbe: TXbe; var aExeFileName, aXbeFileName: string): Boolean;

procedure XbeLoaded;
procedure LoadLogo;


var
  DumpToolString: string;
  m_szAsciiTitle: string;

implementation

procedure LoadLogo;
begin
   (* uint08 i_gray[100*17];

    m_Xbe.ExportLogoBitmap(i_gray);

    if(m_Xbe.GetError() != 0)
    {
        MessageBox(m_hwnd, m_Xbe.GetError(), 'Cxbx', MB_ICONEXCLAMATION | MB_OK);

        if(m_Xbe.IsFatal())
            CloseXbe();

        return;
    }

    uint32 v=0;
    for(uint32 y=0;y<17;y++)
    {
        for(uint32 x=0;x<100;x++)
        {
            SetPixel(m_LogoDC, x, y, RGB(i_gray[v], i_gray[v], i_gray[v]));
            v++;
        }
    }

    RedrawWindow(m_hwnd, NULL, NULL, RDW_INVALIDATE);  *)
end;


procedure XbeLoaded;
begin
  LoadLogo();
  WriteLog(DxbxFormat('DXBX: %s  loaded.', [m_szAsciiTitle]));
end;

function OpenXbe(aFileName: string; var aXbe: TXbe; var aExeFileName, aXbeFileName: string): Boolean;
begin
  Result := False;
  if Assigned(aXbe) or not (FileExists(aFileName)) then
    Exit;

  aExeFileName := '';
  aXbeFileName := aFileName;

  {var}aXbe := TXbe.Create(aXbeFileName, ftXbe);
  try
    XbeLoaded();
    Result := True;
  except
    FreeAndNil(aXbe);
    raise;
  end;
end;


//------------------------------------------------------------------------------

function GetDWordVal(aBuffer: MathPtr; i: Integer): DWord;
begin
  Result := (Ord(aBuffer[i + 0]) shl 0)
          + (Ord(aBuffer[i + 1]) shl 8)
          + (Ord(aBuffer[i + 2]) shl 16)
          + (Ord(aBuffer[i + 3]) shl 24);
end; // GetDwordVal

//------------------------------------------------------------------------------

function GetWordVal(aBuffer: MathPtr; i: Integer): Word;
begin
  Result := (Ord(aBuffer[i + 0]) shl 0)
          + (Ord(aBuffer[i + 1]) shl 8);
end; // GetWordVal


{ TXbe }
//------------------------------------------------------------------------------

procedure TXbe.ConstructorInit;
begin
  FreeAndNil(MyFile);
  Buffer := nil;

  SetLength(m_HeaderEx, 0);
  SetLength(m_SectionHeader, 0);
  SetLength(m_szSectionName, 0);
  SetLength(m_LibraryVersion, 0);
  SetLength(m_KernelLibraryVersion, 0);
  SetLength(m_XAPILibraryVersion, 0);
  FreeMem({var}m_TLS);
  SetLength(m_bzSection, 0);
end; // TXbe.ConstructorInit

//------------------------------------------------------------------------------

constructor TXbe.Create(aFileName: string; aFileType: TFileType);
var
  ExSize: LongInt;
  lIndex, lIndex2: DWord;
  RawSize, RawAddr: DWord;
  I: DWord;
  sFileType: string;
begin
  sFileType := iif(aFileType = ftXbe, 'Xbe', 'Exe');

  ConstructorInit();

  MyFile := TMemoryStream.Create;
  MyFile.LoadFromFile(aFileName);
  Buffer := MyFile.Memory;

  // verify xbe file was opened
  if MyFile.Size = 0 then
  begin
    MessageDlg(DxbxFormat('Could not open %s file', [sFileType]), mtError, [mbOk], 0);
    Exit;
  end;

  WriteLog(DxbxFormat('DXBX: Opening %s file...OK', [sFileType]));

  // remember xbe path
  m_szPath := ExtractFilePath(aFileName);
  WriteLog(DxbxFormat('DXBX: Storing %s Path...Ok', [sFileType]));

  if MyFile.Size < SizeOf(m_Header) then
  begin
    MessageDlg(DxbxFormat('Unexpected end of file while reading %s Image Header', [sFileType]), mtError, [mbOk], 0);
    Exit;
  end;

  i := 0;
  CopyMemory(@m_Header, Buffer, SizeOf(m_Header));
  Inc(i, SizeOf(m_Header));

  // check xbe image header
  if m_Header.dwMagic <> _MagicNumber then
  begin
    MessageDlg(DxbxFormat('Invalid magic number in %s file', [sFileType]), mtError, [mbOk], 0);
    Exit;
  end;

  WriteLog('DXBX: Reading Image Header...Ok');

  ExSize := RoundUp(m_Header.dwSizeofHeaders, $1000) - SizeOf(m_Header);

  // Read Xbe Image Header Extra Bytes
  if (m_Header.dwSizeofHeaders > SizeOf(m_Header)) then
  begin
    WriteLog('DXBX: Reading Image Header Extra Bytes...');

    SetLength(m_HeaderEx, ExSize);
    CopyMemory(m_HeaderEx, @(Buffer[i]), ExSize);
//    Inc(i, ExSize);
  end;

  // read xbe certificate
  i := m_Header.dwCertificateAddr - m_Header.dwBaseAddr;
  CopyMemory(@m_Certificate, @(Buffer[i]), SizeOf(m_Certificate));

  WriteLog('DXBX: Reading Certificate...OK');

  m_szAsciiTitle := WideCharToString(m_Certificate.wszTitleName);
  WriteLog('DXBX: Title identified as ' + m_szAsciiTitle);

  // read xbe section headers
  begin
    DbgPrintf('DXBX: Reading Section Headers...');
    i := m_Header.dwSectionHeadersAddr - m_Header.dwBaseAddr;
    SetLength(m_SectionHeader, m_Header.dwSections);
    for lIndex := 0 to m_Header.dwSections - 1 do
    begin
      CopyMemory(@(m_SectionHeader[lIndex]), @(Buffer[i]), SizeOf(m_SectionHeader[lIndex]));
      Inc(i, SizeOf(m_SectionHeader[lIndex]));

      DbgPrintf('DXBX: Reading Section Header 0x%.4x... OK', [lIndex]);
    end;
  end;

  // Read xbe section names
  begin
    DbgPrintf('DXBX: Reading Section Names...');
    SetLength(m_szSectionName, m_Header.dwSections, 9);
    for lIndex := 0 to m_Header.dwSections - 1 do
    begin
      RawAddr := GetAddr(m_SectionHeader[lIndex].dwSectionNameAddr);
      if m_SectionHeader[lIndex].dwSectionNameAddr <> 0 then
      begin
        for lIndex2 := 0 to 8 do
        begin
          m_szSectionName[lIndex][lIndex2] := AnsiChar(Buffer[RawAddr + lIndex2]);
          if m_szSectionName[lIndex][lIndex2] = #0 then
            Break;
        end; // for lIndex2
      end; // if
      DbgPrintf('DXBX: Reading Section Name 0x%.04X... OK (%s)', [lIndex, PAnsiChar(m_szSectionName[lIndex])]);
    end; // for lIndex
  end;

  // Read xbe library versions
  if m_Header.dwLibraryVersionsAddr <> 0 then
  begin
    WriteLog('DXBX: Reading Library Versions...');

    i := m_Header.dwLibraryVersionsAddr - m_Header.dwBaseAddr;

    SetLength(m_LibraryVersion, m_Header.dwLibraryVersions);
    for lIndex := 0 to m_Header.dwLibraryVersions - 1 do
    begin
      CopyMemory(@(m_LibraryVersion[lIndex]), @(Buffer[i]), SizeOf(m_LibraryVersion[lIndex]));
      Inc(i, SizeOf(m_LibraryVersion[lIndex]));

      WriteLog(DxbxFormat('DXBX: Reading Library Version 0x%.4x... OK', [lIndex]));
    end;

    // read xbe kernel library version
    WriteLog('DXBX: Reading Kernel Library Version...');
    if m_Header.dwKernelLibraryVersionAddr = 0 then
      MessageDlg('Could not locate kernel library version', mtError, [mbOk], 0);

    i := m_Header.dwKernelLibraryVersionAddr - m_Header.dwBaseAddr;
    SetLength(m_KernelLibraryVersion, SizeOf(m_LibraryVersion));
    CopyMemory({Dest=}m_KernelLibraryVersion, {Source=}@(Buffer[i]), SizeOf(m_LibraryVersion));
//  WriteLog(DxbxFormat('DXBX: Kernel Library Version 0x%.4x... OK', [PChar(m_KernelLibraryVersion[0])]));

    // read xbe xapi library version
    WriteLog('DXBX: Reading Xapi Library Version...');
    if m_Header.dwXAPILibraryVersionAddr = 0 then
      MessageDlg('Could not locate Xapi Library Version', mtError, [mbOk], 0);

    i := m_Header.dwXAPILibraryVersionAddr - m_Header.dwBaseAddr;
    SetLength(m_XAPILibraryVersion, SizeOf(m_LibraryVersion));
    CopyMemory(m_XAPILibraryVersion, @(Buffer[i]), SizeOf(m_LibraryVersion));
//  WriteLog(DxbxFormat('DXBX: XAPI Library Version 0x%.4x... OK', [PChar(m_XAPILibraryVersion[0])]));
  end;

  // read Xbe sections
  begin
    WriteLog('DXBX: Reading Sections...');

    SetLength(m_bzSection, m_Header.dwSections);

    for lIndex := 0 to m_Header.dwSections - 1 do
    begin
      //Debug info of turok from cxbx
      //v=0  RawSize: 1578256  RawAddr: 4096
      //v=2  RawSize: 1585152  RawAddr: 63024
      //v=2  RawSize: 1650688  RawAddr: 1344
      //v=3  RawSize: 1654784  RawAddr: 33336
      //v=4  RawSize: 1691648  RawAddr: 120692
      //v=5  RawSize: 1814528  RawAddr: 160420
      //v=6  RawSize: 1978368  RawAddr: 28996
      //v=7  RawSize: 2011136  RawAddr: 246588
      //v=8  RawSize: 2260992  RawAddr: 99384

      RawSize := m_SectionHeader[lIndex].dwSizeofRaw;
      RawAddr := m_SectionHeader[lIndex].dwRawAddr;
      SetLength(m_bzSection[lIndex], RawSize);
      if RawSize > 0 then
        for lIndex2 := 0 to RawSize - 1 do
          m_bzSection[lIndex][lIndex2] := Byte(Buffer[RawAddr + lIndex2]);

      WriteLog(DxbxFormat('DXBX: Reading Section 0x%.4x... OK', [lIndex]));
    end;

//    if lIndex2 < RawSize then
//      WriteLog(DxbxFormat('Unexpected end of file while reading %s Section', [sFileType] ));
  end;

  if m_Header.dwTLSAddr <> 0 then
  begin
    WriteLog('DXBX: Reading Thread Local Storage...');
    if GetAddr(m_Header.dwTLSAddr) <> 0 then
    begin
      m_TLS := AllocMem(SizeOf(XBE_TLS));

      i := GetAddr(m_Header.dwTLSAddr);
      m_TLS.dwDataStartAddr := GetDwordVal(Buffer, i);
      i := i + 4;
      m_TLS.dwDataEndAddr := GetDwordVal(Buffer, i);
      i := i + 4;
      m_TLS.dwTLSIndexAddr := GetDwordVal(Buffer, i);
      i := i + 4;
      m_TLS.dwTLSCallbackAddr := GetDwordVal(Buffer, i);

      i := i + 4;
      m_TLS.dwSizeofZeroFill := GetDwordVal(Buffer, i);

      i := i + 4;
      m_TLS.dwCharacteristics := GetDwordVal(Buffer, i);
    end;
  end;
end; // TXbe.Create

//------------------------------------------------------------------------------

function TXbe.DetermineDumpFileName: string;
begin
  Result := WideCharToString(m_Certificate.wszTitleName);
  Result := FixInvalidFilePath(Result, '_');

  if (m_Certificate.dwGameRegion and 4) > 0 then
    Result := Result + '-PAL'
  else
    if (m_Certificate.dwGameRegion and 1) > 0 then
      Result := Result + '-NTSC';

  Result := Result + '.txt';
end;

//------------------------------------------------------------------------------

function TXbe.DumpInformation(FileName: string): Boolean;
var
  FileEx: TextFile;
  lIndex, lIndex2: Integer;
  TmpStr: string;
  TmpChr: Char;
  StrAsciiFileName: string;
  Flag: Byte;
//  BIndex: Byte;
  QVersion: Word;
  LogType: TLogType;

  DumpToFile: Boolean;

  procedure _LogEx(Text: string);
  begin
    if LogType = ltLog then
      WriteLog(Text)
    else
      Writeln(FileEx, Text);
  end;

begin
  Result := True;

  DumpToFile := (FileName <> '');
  if DumpToFile then
  begin
    AssignFile({var}FileEx, FileName);
    Rewrite({var}FileEx);
    LogType := ltFile;
  end
  else
    LogType := ltLog;

  _LogEx(DxbxFormat('XBE information generated by %s', [DumpToolString]));
  _LogEx('');

  _LogEx(DxbxFormat('Title identified as "%s"', [m_szAsciiTitle]));
  _LogEx('');

  _LogEx('Dumping XBE file header...');
  _LogEx('');

  _LogEx('Magic Number                     : XBEH');

  _LogEx('Digitial Signature               : <Hex Dump>');
  TmpStr := '';
  lIndex2 := 0;
  for lIndex := 0 to 255 do
  begin
    TmpStr := TmpStr + IntToHex(m_Header.pbDigitalSignature[lIndex], 2);

    if lIndex2 = 15 then
    begin
      _LogEx('                                   ' + TmpStr);
      TmpStr := '';
      lIndex2 := -1;
    end;

    Inc(lIndex2);
  end;

  _LogEx('                                   </Hex Dump>');
  _LogEx(DxbxFormat('Base Address                     : 0x%.8x', [m_Header.dwBaseAddr]));
  _LogEx(DxbxFormat('Size of Headers                  : 0x%.8x', [m_Header.dwSizeofHeaders]));
  _LogEx(DxbxFormat('Size of Image                    : 0x%.8x', [m_Header.dwSizeofImage]));
  _LogEx(DxbxFormat('Size of Image Header             : 0x%.8x', [m_Header.dwSizeofImageHeader]));
  TmpStr := '';
  DateTimeToString(TmpStr, 'ddd mmm dd hh:mm:ss yyyy', CTimeToDateTime(m_Header.dwTimeDate));
  _LogEx(DxbxFormat('TimeDate Stamp                   : 0x%.8x', [m_Header.dwTimeDate]) + ' (' + TmpStr + ')');
  _LogEx(DxbxFormat('Certificate Address              : 0x%.8x', [m_Header.dwCertificateAddr]));
  _LogEx(DxbxFormat('Number of Sections               : 0x%.8x', [m_Header.dwSections]));
  _LogEx(DxbxFormat('Section Headers Address          : 0x%.8x', [m_header.dwSectionHeadersAddr]));

  // Print init flags
  TmpStr := DxbxFormat('Init Flags                       : 0x%.2x%.2x%.2x%.2x ', [m_Header.dwInitFlags[3], m_Header.dwInitFlags[2], m_Header.dwInitFlags[1], m_Header.dwInitFlags[0]]);
  Flag := m_Header.dwInitFlags[0];

  if (Flag and XBE_INIT_FLAG_MountUtilityDrive) > 0 then
    TmpStr := TmpStr + '[Mount Utility Drive] ';

  if (Flag and XBE_INIT_FLAG_FormatUtilityDrive) > 0 then
    TmpStr := TmpStr + '[Format Utility Drive] ';

  if (Flag and XBE_INIT_FLAG_Limit64MB) > 0 then
    TmpStr := TmpStr + '[Limit Devkit Run Time Memory to 64MB] ';

  if (Flag and XBE_INIT_FLAG_DontSetupHarddisk) > 0 then
    TmpStr := TmpStr + '[Setup Harddisk] ';

  _LogEx(TmpStr);

  lIndex := GetAddr(m_Header.dwDebugUnicodeFileNameAddr);
  lIndex2 := 0;
  TmpStr := '';
  while lIndex2 < 40 do
  begin
    TmpStr := TmpStr + Char(Buffer[lIndex]);
    Inc(lIndex2);
    lIndex := lIndex + 2;
    if Char(Buffer[lIndex]) = #0 then
      Break;
  end;

  //TmpStr := WideStringToString(AsciiFileName, 437);

  StrAsciiFileName := TmpStr;
  TmpStr := '';
  _LogEx(DxbxFormat('Entry Point                      : 0x%.8x (Retail: 0x%.8x, Debug: 0x%.8x)', [m_Header.dwEntryAddr, m_Header.dwEntryAddr xor XOR_EP_Retail, m_Header.dwEntryAddr xor XOR_EP_DEBUG]));
  _LogEx(DxbxFormat('TLS Address                      : 0x%.8x', [m_Header.dwTLSAddr]));
  _LogEx(DxbxFormat('(PE) Stack Commit                : 0x%.8x', [m_Header.dwPeStackCommit]));
  _LogEx(DxbxFormat('(PE) Heap Reserve                : 0x%.8x', [m_Header.dwPeHeapReserve]));
  _LogEx(DxbxFormat('(PE) Heap Commit                 : 0x%.8x', [m_Header.dwPeHeapCommit]));
  _LogEx(DxbxFormat('(PE) Base Address                : 0x%.8x', [m_Header.dwPeBaseAddr]));
  _LogEx(DxbxFormat('(PE) Size of Image               : 0x%.8x', [m_Header.dwPeSizeofImage]));
  _LogEx(DxbxFormat('(PE) Checksum                    : 0x%.8x', [m_Header.dwPeChecksum]));

  TmpStr := '';
  DateTimeToString(TmpStr, 'ddd mmm dd hh:mm:ss yyyy', CTimeToDateTime(m_Header.dwPeTimeDate));
  _LogEx(DxbxFormat('(PE) TimeDate Stamp              : 0x%.8x (%s)', [m_Header.dwPeTimeDate, TmpStr]));

  lIndex := GetAddr(m_Header.dwDebugPathNameAddr);
  TmpStr := '';
  TmpChr := Char(Buffer[lIndex]);
  Inc(lIndex);
  while TmpChr <> #0 do
  begin
    TmpStr := TmpStr + TmpChr;
    TmpChr := Char(Buffer[lIndex]);
    Inc(lIndex);
  end;

  _LogEx(DxbxFormat('Debug PathName Address           : 0x%.8x ("%s")', [m_Header.dwDebugPathnameAddr, TmpStr]));

  lIndex := GetAddr(m_Header.dwDebugFileNameAddr);
  TmpStr := '';
  TmpChr := Char(Buffer[lIndex]);
  Inc(lIndex);
  while TmpChr <> #0 do
  begin
    TmpStr := TmpStr + TmpChr;
    TmpChr := Char(Buffer[lIndex]);
    Inc(lIndex);
  end;

  _LogEx(DxbxFormat('Debug FileName Address           : 0x%.8x ("%s")', [m_Header.dwDebugFileNameAddr, TmpStr]));

  _LogEx(DxbxFormat('Debug Unicode FileName Address   : 0x%.8x (L"%s")', [m_Header.dwDebugUnicodeFileNameAddr, StrAsciiFileName]));
  _LogEx(DxbxFormat('Kernel Image Thunk Address       : 0x%.8x (Retail: 0x%.8x, Debug: 0x%.8x)', [m_Header.dwKernelImageThunkAddr, m_Header.dwKernelImageThunkAddr xor XOR_KT_RETAIL, m_Header.dwKernelImageThunkAddr xor XOR_KT_DEBUG]));
  _LogEx(DxbxFormat('NonKernel Import Dir Address     : 0x%.8x', [m_Header.dwNonKernelImportDirAddr]));
  _LogEx(DxbxFormat('Library Versions                 : 0x%.8x', [m_Header.dwLibraryVersions]));
  _LogEx(DxbxFormat('Library Versions Address         : 0x%.8x', [m_Header.dwLibraryVersionsAddr]));
  _LogEx(DxbxFormat('Kernel Library Version Address   : 0x%.8x', [m_Header.dwKernelLibraryVersionAddr]));
  _LogEx(DxbxFormat('XAPI Library Version Address     : 0x%.8x', [m_Header.dwXAPILibraryVersionAddr]));
  _LogEx(DxbxFormat('Logo Bitmap Address              : 0x%.8x', [m_Header.dwLogoBitmapAddr]));
  _LogEx(DxbxFormat('Logo Bitmap Size                 : 0x%.8x', [m_Header.dwSizeofLogoBitmap]));

  _LogEx('');
  _LogEx('Dumping XBE Certificate...');
  _LogEx('');

  _LogEx(DxbxFormat('Size of Certificate              : 0x%.8x', [m_Certificate.dwSize]));
  TmpStr := '';
  DateTimeToString(TmpStr, 'ddd mmm dd hh:mm:ss yyyy', CTimeToDateTime(m_Certificate.dwTimeDate));
  _LogEx(DxbxFormat('TimeDate Stamp                   : 0x%.8x (%s)', [m_Certificate.dwTimeDate, TmpStr]));
  _LogEx(DxbxFormat('Title ID                         : 0x%.8x', [m_Certificate.dwTitleId]));
  _LogEx(DxbxFormat('Title                            : "%s"', [m_szAsciiTitle]));

  // print alternate titles
  _LogEx(DxbxFormat('Alternate Titles IDs             : 0x%.8x', [m_Certificate.dwAlternateTitleId[0]]));
  for lIndex := 1 to 15 do
    _LogEx(DxbxFormat('                                   0x%.8x', [m_Certificate.dwAlternateTitleId[lIndex]]));

  _LogEx(DxbxFormat('Allowed Media                    : 0x%.8x', [m_Certificate.dwAllowedMedia]));
  _LogEx(DxbxFormat('Game Region                      : 0x%.8x', [m_Certificate.dwGameRegion]));
  _LogEx(DxbxFormat('Game Ratings                     : 0x%.8x', [m_Certificate.dwGameRatings]));
  _LogEx(DxbxFormat('Disk Number                      : 0x%.8x', [m_Certificate.dwDiskNumber]));
  _LogEx(DxbxFormat('Version                          : 0x%.8x', [m_Certificate.dwVersion]));

  // Print Lan Key
  TmpStr := '';
  for lIndex := 0 to 15 do
    TmpStr := TmpStr + IntToHex(Ord(m_Certificate.bzLanKey[lIndex]), 2);

  _LogEx('LAN Key                          : ' + TmpStr);

  // print signature key
  TmpStr := '';
  for lIndex := 0 to 15 do
    TmpStr := TmpStr + IntToHex(Ord(m_Certificate.bzSignatureKey[lIndex]), 2);

  _LogEx('Signature Key                    : ' + TmpStr);

  // print alternative signature keys
  _LogEx('Title Alternative Signature Keys : <Hex Dump>');

  for lIndex := 0 to 15 do
  begin
    TmpStr := '';
    for lIndex2 := 0 to 15 do
      TmpStr := TmpStr + IntToHex(Ord(m_Certificate.bzTitleAlternateSignatureKey[lIndex][lIndex2]), 2);

    _LogEx('                                   ' + TmpStr);
  end;

  _LogEx('                                   </Hex Dump>');

  // print section headers
  _LogEx('');
  _LogEx('Dumping XBE Section Headers...');
  _LogEx('');
  for lIndex := 0 to m_Header.dwSections - 1 do
  begin
    TmpStr := '';
    for lIndex2 := 0 to 8 do
    begin
      if m_szSectionName[lIndex][lIndex2] <> #0 then
        TmpStr := TmpStr + Char(m_szSectionName[lIndex][lIndex2])
      else
        Break;
    end;

    _LogEx(DxbxFormat('Section Name                     : 0x%.8x ("%s")', [m_SectionHeader[lIndex].dwSectionNameAddr, TmpStr]));

    TmpStr := '';
    TmpStr := DxbxFormat('Flags                            : 0x%.2x%.2x%.2x%.2x', [m_SectionHeader[lIndex].dwFlags[3], m_SectionHeader[lIndex].dwFlags[2], m_SectionHeader[lIndex].dwFlags[1], m_SectionHeader[lIndex].dwFlags[0]]);

    Flag := m_SectionHeader[lIndex].dwFlags[0];

    TmpStr := TmpStr + ' '; // Insert open space
    if (Flag and XBE_SECTIONHEADER_FLAG_Writable) > 0 then
      TmpStr := TmpStr + '(Writable) ';

    if (Flag and XBE_SECTIONHEADER_FLAG_Preload) > 0 then
      TmpStr := TmpStr + '(Preload) ';

    if (Flag and XBE_SECTIONHEADER_FLAG_Executable) > 0 then
      TmpStr := TmpStr + '(Executable) ';

    if (Flag and XBE_SECTIONHEADER_FLAG_InsertedFile) > 0 then
      TmpStr := TmpStr + '(Inserted File) ';

    if (Flag and XBE_SECTIONHEADER_FLAG_HeadPageRO) > 0 then
      TmpStr := TmpStr + '(Head Page RO) ';

    if (Flag and XBE_SECTIONHEADER_FLAG_TailPageRO) > 0 then
      TmpStr := TmpStr + '(Tail Page RO) ';

    _LogEx(TmpStr);

    _LogEx(DxbxFormat('Virtual Address                  : 0x%.8x', [m_SectionHeader[lIndex].dwVirtualAddr]));
    _LogEx(DxbxFormat('Virtual Size                     : 0x%.8x', [m_SectionHeader[lIndex].dwVirtualSize]));
    _LogEx(DxbxFormat('Raw Address                      : 0x%.8x', [m_SectionHeader[lIndex].dwRawAddr]));
    _LogEx(DxbxFormat('Size of Raw                      : 0x%.8x', [m_SectionHeader[lIndex].dwSizeofRaw]));
    _LogEx(DxbxFormat('Section Name Address             : 0x%.8x', [m_SectionHeader[lIndex].dwSectionNameAddr]));
    _LogEx(DxbxFormat('Section Reference Count          : 0x%.8x', [m_SectionHeader[lIndex].dwSectionRefCount]));
    _LogEx(DxbxFormat('Head Shared Reference Count Addr : 0x%.8x', [m_SectionHeader[lIndex].dwHeadSharedRefCountAddr]));
    _LogEx(DxbxFormat('Tail Shared Reference Count Addr : 0x%.8x', [m_SectionHeader[lIndex].dwTailSharedRefCountAddr]));
    TmpStr := '';

    for lIndex2 := 0 to 19 do
      TmpStr := TmpStr + IntToHex(Ord(m_SectionHeader[lIndex].bzSectionDigest[lIndex2]), 2);

    _LogEx('Section Digest                   : ' + TmpStr);
    _LogEx('');
  end;

  // print library versions
  _LogEx('Dumping XBE Library Versions...');
  _LogEx('');
  if (SizeOf(m_LibraryVersion) = 0) or (m_Header.dwLibraryVersions = 0) then
  begin
    _LogEx('(This XBE contains no Library Versions)');
    _LogEx('');
  end
  else
  begin
    for lIndex := 0 to m_Header.dwLibraryVersions - 1 do
    begin
      TmpStr := '';
      for lIndex2 := 0 to 7 do
      begin
        if m_LibraryVersion[lIndex].szName[lIndex2] <> #0 then
          TmpStr := TmpStr + Char(m_LibraryVersion[lIndex].szName[lIndex2]);
      end;

      _LogEx('Library Name                     : ' + TmpStr);
      _LogEx(DxbxFormat('Version                          : %d.%d.%d', [m_LibraryVersion[lIndex].wMajorVersion, m_LibraryVersion[lIndex].wMinorVersion, m_LibraryVersion[lIndex].wBuildVersion]));

      //Some bit maths the QVersion Flag is only 13 bits long so i convert the 13 bits to a number

      QVersion := m_LibraryVersion[lIndex].dwFlags[0]
              + ((m_LibraryVersion[lIndex].dwFlags[1] and 31) shl 8);

      Flag := m_LibraryVersion[lIndex].dwFlags[1] and (not 31);

      TmpStr := DxbxFormat('Flags                            : QFEVersion : 0x%.4x, ', [QVersion]);

      if (Flag and XBE_LIBRARYVERSION_FLAG_DebugBuild) > 0 then
        TmpStr := TmpStr + 'Debug, '
      else
        TmpStr := TmpStr + 'Retail, ';

      if (Flag and XBE_LIBRARYVERSION_FLAG_ApprovedYes) > 0 then
        TmpStr := TmpStr + 'Approved'
      else
        if (Flag and XBE_LIBRARYVERSION_FLAG_ApprovedPossibly) > 0 then
          TmpStr := TmpStr + 'Possibly Approved'
        else
          TmpStr := TmpStr + 'Unapproved';

      _LogEx(TmpStr);
      _LogEx('');
    end; // for lIndex
  end;

  _LogEx('Dumping XBE TLS...');
  _LogEx('');
  // print thread local storage
  if Assigned(m_TLS) then
  begin
    _LogEx(DxbxFormat('Data Start Address               : 0x%.8x', [m_TLS.dwDataStartAddr]));
    _LogEx(DxbxFormat('Data End Address                 : 0x%.8x', [m_TLS.dwDataEndAddr]));
    _LogEx(DxbxFormat('TLS Index Address                : 0x%.8x', [m_TLS.dwTLSIndexAddr]));
    _LogEx(DxbxFormat('TLS Callback Address             : 0x%.8x', [m_TLS.dwTLSCallbackAddr]));
    _LogEx(DxbxFormat('Size of Zero Fill                : 0x%.8x', [m_TLS.dwSizeofZeroFill]));
    _LogEx(DxbxFormat('Characteristics                  : 0x%.8x', [m_TLS.dwCharacteristics]));
  end
  else
    _LogEx('(This XBE contains no TLS)');

  if DumpToFile then
    CloseFile(FileEx);
end; // TXbe.DumpInformation

//------------------------------------------------------------------------------

function TXbe.GetAddr(x_dwVirtualAddress: DWord): Integer;
var
  lIndex, VirtAddr, VirtSize, dwoffs: DWord;
begin
  dwoffs := x_dwVirtualAddress - m_Header.dwBaseAddr;
  Result := 0;
  // offset in image header
  if dwoffs < m_Header.dwSizeofHeaders then
    Result := dwOffs
  else
  begin
    // offset in image header extra bytes
    if dwoffs < m_Header.dwSizeofHeaders then
      Result := dwOffs - SizeOf(m_Header)
    else
    begin
      for lIndex := 0 to m_Header.dwSections - 1 do
      begin
        VirtAddr := m_SectionHeader[lIndex].dwVirtualAddr;
        VirtSize := m_SectionHeader[lIndex].dwVirtualSize;
        if (x_dwVirtualAddress >= VirtAddr) and (x_dwVirtualAddress < (VirtAddr + VirtSize)) then
          Result := m_SectionHeader[lIndex].dwRawAddr + (x_dwVirtualAddress - VirtAddr)
      end;
    end;
  end;
end; // TXbe.GetAddr

//------------------------------------------------------------------------------

procedure TXbe.ExportLogoBitmap(ImgCont: TBitmap);
var
  x_Gray: array[0..100 * 17] of Byte;
  dwLength, o, lIndex, lIndex2, Len, Data: DWord;
  RLE: DWord;
  pos0, pos1: Byte;
begin
  dwLength := m_Header.dwSizeofLogoBitmap;
  RLE := GetAddr(m_Header.dwLogoBitmapAddr);
  if RLE = 0 then
    Exit;

  Len := 0;
  Data := 0;
  o := 0;
  lIndex := 0;
  while lIndex < dwLength do
  begin
    // Read 2 bytes.
    Pos0 := Ord(Buffer[RLE + lIndex]);
    Pos1 := Ord(Buffer[RLE + 1 + lIndex]);

    if (Pos0 and 1) > 0 then // Check if the bit 0 is set.
    begin
      Len := Pos0 shr 1 and 7; // Select the bits from 1 to 3
      Data := Pos0 shr 4 and 15; // Select the bits from 4 to 7
    end
    else
    begin
      if (Pos0 and 2) > 0 then // Check if the bit 1 is set.
      begin
        Len := (Pos0 shr 2 and 63) + (Pos1 and 15) * 256; // Select the bits from 2 to 7 from the first byte (Pos0) and the bits from 0 to 3 from the second byte (Pos1) and form a number.
        Data := Pos1 shr 4 and 15; // Select the bits from 4 to 7 from the second byte (Pos1)
        Inc(lIndex); // The index is incremented because 2 bytes were read.
      end;
    end;

    for lIndex2 := 0 to Len - 1 do
    begin
      Inc(o);
      if o >= 100 * 17 then
        Exit;

      x_Gray[o] := Byte(Data shl 4);
      ImgCont.Canvas.Pixels[o mod 100, o div 100] := RGB(x_Gray[o], x_Gray[o], x_Gray[o]);
    end;

    Inc(lIndex); // Index increment
  end;
end; // TXbe.ExportLogoBitmap

//------------------------------------------------------------------------------

function TXbe.GetTLSData: DWord;
begin
  if m_TLS.dwDataStartAddr = 0 then
    Result := 0
  else
    Result := GetAddr(m_TLS.dwDataStartAddr);
end; // TXbe.GetTLSData

//------------------------------------------------------------------------------

destructor TXbe.Destroy;
begin
  ConstructorInit();

  inherited Destroy;
end;

//------------------------------------------------------------------------------

end.
