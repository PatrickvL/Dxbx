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
  SysUtils, // for Format
  StrUtils, // for IfThen
  Classes,
  Controls,
  Dialogs, // for MessageDlg
  Graphics, // for TBitmap
  // Dxbx
  uConsts,
  uTypes,
  uTime,
  uDxbxUtils,
  uLog,
  uEmuD3D8Types,
  uFileSystem; // Drives

type _XBE_HEADER = packed record
    dwMagic: array [0..3] of AnsiChar; // 0x0000 - magic number [should be "XBEH"]
    pbDigitalSignature: array [0..255] of Byte; // 0x0004 - digital signature
    dwBaseAddr: DWord; // 0x0104 - base address
    dwSizeofHeaders: DWord; // 0x0108 - size of headers
    dwSizeofImage: DWord; // 0x010C - size of image
    dwSizeofImageHeader: DWord; // 0x0110 - size of image header
    dwTimeDate: DWord; // 0x0114 - timedate stamp
    dwCertificateAddr: DWord; // 0x0118 - certificate address
    dwSections: DWord; // 0x011C - number of sections
    dwSectionHeadersAddr: DWord; // 0x0120 - section headers address

    dwInitFlags: array [0..3] of Byte; // 0x0124 - initialization flags

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
  XBE_HEADER = _XBE_HEADER;
  PXBE_HEADER = ^XBE_HEADER;

  TXbeHeader = XBE_HEADER;
  PXbeHeader = PXBE_HEADER;

  _XBE_CERTIFICATE = packed record
    dwSize: DWord; // 0x0000 - size of certificate
    dwTimeDate: DWord; // 0x0004 - timedate stamp
    dwTitleId: DWord; // 0x0008 - title id
    wszTitleName: array [0..XBE_TITLENAME_MAXLENGTH-1] of WideChar; // 0x000C - title name (unicode)
    dwAlternateTitleId: array [0..15] of Dword; // 0x005C - alternate title ids
    dwAllowedMedia: Dword; // 0x009C - allowed media types
    dwGameRegion: DWord; // 0x00A0 - game region
    dwGameRatings: DWord; // 0x00A4 - game ratings
    dwDiskNumber: DWord; // 0x00A8 - disk number
    dwVersion: Dword; // 0x00AC - version
    bzLanKey: array [0..15] of Byte; // 0x00B0 - lan key
    bzSignatureKey: array [0..15] of Byte; // 0x00C0 - signature key
    bzTitleAlternateSignatureKey: array [0..15] of array [0..15] of Byte; // 0x00D0 - alternate signature keys
  end;
  XBE_CERTIFICATE = _XBE_CERTIFICATE;
  PXBE_CERTIFICATE = ^XBE_CERTIFICATE;

  TXbeCertificate = XBE_CERTIFICATE;
  PXbeCertificate = PXBE_CERTIFICATE;

  // A list of sections in this Xbe used for section loading
  _XBE_SECTIONLIST = packed record
    szSectionName: array [0..XBE_SECTIONNAME_MAXLENGTH-1] of AnsiChar;
    dwSectionAddr: uint32;
    dwSectionSize: uint32;
  end;
  XBE_SECTIONLIST = _XBE_SECTIONLIST;
  PXBE_SECTIONLIST = ^XBE_SECTIONLIST;

  TSectionList = XBE_SECTIONLIST;
  PSectionList = PXBE_SECTIONLIST;

type _XBE_SECTIONHEADER = packed record
  // Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
    dwFlags: array [0..3] of Byte;
    dwVirtualAddr: DWord; // virtual address
    dwVirtualSize: DWord; // virtual size
    dwRawAddr: DWord; // file offset to raw Data
    dwSizeofRaw: DWord; // size of raw Data
    dwSectionNameAddr: DWord; // section name addr
    dwSectionRefCount: DWord; // section reference count
    dwHeadSharedRefCountAddr: DWord; // head shared page reference count address
    dwTailSharedRefCountAddr: DWord; // tail shared page reference count address
    bzSectionDigest: array [0..19] of Byte; // section digest
  end;
(*
  // Section headers - Source: XBMC
  _XBE_SECTIONHEADER = packed record
    Flags: ULONG;
    VirtualAddress: PVOID; // Virtual address (where this section loads in RAM)
    VirtualSize: ULONG; // Virtual size (size of section in RAM; after FileSize it's 00'd)
    FileAddress: ULONG; // File address (where in the file from which this section comes)
    FileSize: ULONG; // File size (size of the section in the XBE file)
    SectionName: PCSZ; // Pointer to section name
    SectionReferenceCount: LONG; // Section reference count - when >= 1, section is loaded
    HeadReferenceCount: PWORD; // Pointer to head shared page reference count
    TailReferenceCount: PWORD; // Pointer to tail shared page reference count
    ShaHash: array [0..5 - 1] of DWord; // SHA hash.  Hash DWORD containing FileSize, then hash section.
  end; // SizeOf() = 38
*)
  XBE_SECTIONHEADER = _XBE_SECTIONHEADER;
  PXBE_SECTIONHEADER = ^XBE_SECTIONHEADER;

  TXbeSectionHeader = XBE_SECTIONHEADER;
  PXbeSectionHeader = PXBE_SECTIONHEADER;

  _XBE_LIBRARYVERSION = packed record
    szName: array [0..XBE_LIBRARYNAME_MAXLENGTH-1] of AnsiChar; // library name
    wMajorVersion: Word; // major version
    wMinorVersion: Word; // minor version
    wBuildVersion: Word; // build version
    dwFlags: array [0..1] of Byte;
           { struct Flags
            {
                uint16 QFEVersion       : 13;      // QFE Version
                uint16 Approved         : 2;       // Approved? (0:no, 1:possibly, 2:yes)
                uint16 bDebugBuild      : 1;       // Is this a debug build?
            }
            //dwFlags;
  end;
  XBE_LIBRARYVERSION = _XBE_LIBRARYVERSION;
  PXBE_LIBRARYVERSION = ^XBE_LIBRARYVERSION;

  TXbeLibraryVersion = XBE_LIBRARYVERSION;
  PXbeLibraryVersion = PXBE_LIBRARYVERSION;

  _XBE_TLS = packed record
    dwDataStartAddr: DWord; // raw start address
    dwDataEndAddr: DWord; // raw end address
    dwTLSIndexAddr: DWord; // tls index  address
    dwTLSCallbackAddr: DWord; // tls callback address
    dwSizeofZeroFill: DWord; // size of zero fill
    dwCharacteristics: DWord; // characteristics
  end;
  XBE_TLS = _XBE_TLS;
  PXBE_TLS = ^XBE_TLS;

  TXbeTls = XBE_TLS;
  PXbeTls = PXBE_TLS;

  _Eight = Byte; // AnsiChar?
    //Bit 0  : bType1
    //Bit 1,2,3  : Len
    //Bit 4,5,6,7 : Data
  Eight = _Eight;

  _Sixteen = array [0..1] of Byte; // AnsiChar? Word?
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

  // XPR structures

  // Purpose:
  //   The XPR file format allows multiple graphics resources to be pre-defined
  //   and bundled together into one file.  These resources can be copied into
  //   memory and then immediately used in-place as D3D objects such as textures
  //   and vertex buffers.  The structure below defines the XPR header and the
  //   unique identifier for this file type.
  _XPR_HEADER = packed record
    dwMagic: DWORD; // 'XPR0' or 'XPR1'
    dwTotalSize: DWORD;
    dwHeaderSize: DWORD;
  end;
  XPR_HEADER = _XPR_HEADER;
  PXPR_HEADER = ^XPR_HEADER;

  TXprHeader = XPR_HEADER;
  PXprHeader = PXPR_HEADER;
  
  // Layout of SaveImage.xbx saved game image file
  //
  // File is XPR0 format. Since the XPR will always contain only a single
  // 256x256 DXT1 image, we know exactly what the header portion will look like
  _XPR_IMAGEHEADER = packed record
    Header: XPR_HEADER;      // Standard XPR struct
    Texture: X_D3DBaseTexture;  // Standard D3D texture struct
    EndOfHeader: DWORD; // $FFFFFFFF
  end;
  XPR_IMAGEHEADER = _XPR_IMAGEHEADER;
  PXPR_IMAGEHEADER = ^XPR_IMAGEHEADER;

  TXprImageHeader = XPR_IMAGEHEADER;
  PXprImageHeader = PXPR_IMAGEHEADER;

  _XPR_IMAGE = packed record
    hdr: XPR_IMAGEHEADER;
    strPad: array [0..XPR_IMAGE_HDR_SIZE-SizeOf(XPR_IMAGEHEADER)-1] of AnsiChar;
    pBits: array [0..XPR_IMAGE_DATA_SIZE-1] of Byte; // data bits
  end;
  XPR_IMAGE = _XPR_IMAGE;
  PXPR_IMAGE = ^XPR_IMAGE;

  TXprImage = XPR_IMAGE;
  PXprImage = PXPR_IMAGE;

(*
  _XPR_File_Header_ = packed record
    Type_: record
      case Integer of
        0: ( dwXPRMagic: DWORD );
        1: ( wBitmapMagic: WORD );
    end;

    dwTotalSize: DWORD;
    dwHeaderSize: DWORD;

    dwTextureCommon: DWORD;
    dwTextureData: DWORD;
    dwTextureLock: DWORD;
    btTextureMisc1: BYTE;
    btTextureFormat: BYTE;
    btTextureLevel_Width: BYTE;
    //BYTE        btTextureLevel  : 4;
    //BYTE        btTextureWidth  : 4;
    btTextureHeight_Misc2: BYTE;
    //BYTE        btTextureHeight : 4;
    //BYTE        btTextureMisc2  : 4;
    dwTextureSize: DWORD;

    dwEndOfHeader: DWORD; // 0xFFFFFFFF
  end;
  XPR_FILE_HEADER = _XPR_File_Header_;
  PXPR_FILE_HEADER = ^XPR_FILE_HEADER;
  PPXPR_FILE_HEADER = ^PXPR_FILE_HEADER;
*)

  // TODO -oDxbx : Remove most dependancies on this TXbe type, at least in
  // MapAndRun but maybe also in OpenXbe and it's callers.
  // Instead, start accessesing Xbe's (and other resources) via Drives.D.FileSystem,
  // which would create a better layer of seperation. Do note, that some kernel
  // I/O functions (like asynchronuous file access) don't map too well to this
  // setup... but even if this happens, we could still use the FileSystem abstraction
  // but bypass it for the special cases (which means we can only do this on MappedFolders)
  TXbe = class(TObject)
  private
    MyFile: TMemoryStream;
    FRawData: MathPtr;
    m_KernelLibraryVersion: XBE_LIBRARYVERSION;
    m_XAPILibraryVersion: XBE_LIBRARYVERSION;
    procedure ConstructorInit;
    function GetFileSize: Int64;
  public
    XbePath: string;
    m_Header: XBE_HEADER;
    m_Certificate: XBE_CERTIFICATE;
    m_SectionHeader: array of XBE_SECTIONHEADER;
    m_LibraryVersion: array of XBE_LIBRARYVERSION;
    m_szSectionName: array of array of AnsiChar; // TODO -oDXBX: Use XBE_SECTIONNAME_MAXLENGTH
    m_HeaderEx: array of Byte;
    m_TLS: PXBE_TLS;
    m_bzSection: array of TRawSection;

    property RawData: MathPtr read FRawData;
    property FileSize: Int64 read GetFileSize;
    class function FileExists(aFileName: string): Boolean;
    
    constructor Create(const aFileName: string);
    destructor Destroy; override;

    function GetTLSData: DWord;

    function DetermineDumpFileName: string;
    function DumpInformation(FileName: string = ''): Boolean;
    
    function GetAddr(x_dwVirtualAddress: DWord): Integer;
    function GetAddrStr(x_dwVirtualAddress: DWord; const aMaxLen: Integer = MaxInt): string;
    function GetAddrWStr(x_dwVirtualAddress: DWord; const aMaxLen: Integer = MaxInt): WideString;

    function FindSection(const aSectionName: string; out Size: Integer): TRawSection;
    function ExportLogoBitmap(aBitmap: TBitmap): Boolean;
    function ExportIconBitmap(aBitmap: TBitmap): Boolean;
    function ExportXPRToBitmap(XprImage: PXPR_IMAGE; aBitmap: TBitmap): Boolean;
  end;

var
  // OpenXDK logo bitmap (used by cxbe by default)
  OpenXDK: array of uint08;
  dwSizeOfOpenXDK: uint32;

function GetDWordVal(aBuffer: MathPtr; i: Integer): DWord;
function GetWordVal(aBuffer: MathPtr; i: Integer): Word;

function BetterTime(x_timeDate: uint32): string;

function OpenXbe(aFileName: string; var aXbe: TXbe{; var aExeFileName, aXbeFileName: string}): Boolean;

procedure XbeLoaded;
function GameRegionToString(const aGameRegion: Integer): string;

procedure LoadSymbolsFromCache(const aStringList: TStringList; const aCacheFile: string);

var
  DumpToolString: string;
  m_szAsciiTitle: string;

implementation

procedure LoadSymbolsFromCache(const aStringList: TStringList; const aCacheFile: string);
var
  i, j: Integer;
  FuncStr, AddrStr: string;
  Addr: Pointer;
begin
  with aStringList do
  begin
    LoadFromFile(aCacheFile);
    // Split up each line into name and address, putting the name back as a string
    // and the address in the Object column :
    for i := 0 to Count - 1 do
    begin
      FuncStr := Strings[i];

      j := Pos(':$', FuncStr); // TODO : Use LastPos here
      AddrStr := Copy(FuncStr, j + 2, MaxInt);
      System.Delete(FuncStr, j, MaxInt);

      Addr := Pointer(HexToIntDef(AddrStr, 0));

      Strings[i] := FuncStr;
      Objects[i] := TObject(Addr);
    end;

    // Sort the list on address :
    CustomSort(@SortObjects);
  end;
end;

procedure XbeLoaded;
begin
  WriteLog(DxbxFormat('DXBX: %s  loaded.', [m_szAsciiTitle]));
end;

function OpenXbe(aFileName: string; var aXbe: TXbe{; var aExeFileName, aXbeFileName: string}): Boolean;
begin
  Result := False;
  if Assigned(aXbe) or not (TXbe.FileExists(aFileName)) then
    Exit;

{  aExeFileName := '';
  aXbeFileName := aFileName;}
  {var}aXbe := TXbe.Create({aXbe}aFileName);
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


function GameRegionToString(const aGameRegion: Integer): string;
begin
  if (aGameRegion and XBEIMAGE_GAME_REGION_ALL) = XBEIMAGE_GAME_REGION_ALL then
    Result := 'ALL'
  else
  begin
    Result := '';
    if (aGameRegion and XBEIMAGE_GAME_REGION_JAPAN) > 0 then
      Result := Result + ' JAP';

    if (aGameRegion and XBEIMAGE_GAME_REGION_US_CANADA) > 0 then
      Result := Result + ' NTSC';

    if (aGameRegion and XBEIMAGE_GAME_REGION_RESTOFWORLD) > 0 then
      Result := Result + ' PAL';
  end;

  if (aGameRegion and XBEIMAGE_GAME_REGION_MANUFACTURING) > 0 then
    Result := Result + ' DEBUG';

  Result := StringReplace(Trim(Result), ' ', '+', [rfReplaceAll]);
  if Result = '' then
  begin
    if aGameRegion = 0 then
      Result := 'UNKNOWN'
    else
      Result := 'REGION ' + IntToStr(aGameRegion);
  end;
end;

{ TXbe }
//------------------------------------------------------------------------------

procedure TXbe.ConstructorInit;
begin
  FreeAndNil(MyFile);
  FRawData := nil;

  SetLength(m_HeaderEx, 0);
  SetLength(m_SectionHeader, 0);
  SetLength(m_szSectionName, 0);
  SetLength(m_LibraryVersion, 0);
  FreeMem({var}m_TLS);
  SetLength(m_bzSection, 0);
end; // TXbe.ConstructorInit

//------------------------------------------------------------------------------

class function TXbe.FileExists(aFileName: string): Boolean;
var
  DummyStr: string;
begin
  Result := Drives.D.OpenImage(aFileName, {out}DummyStr)
//        and SameText(ExtractFileExt(Drives.D.FileSystem.SelectedFile), '.xbe');;
end;

constructor TXbe.Create(const aFileName: string);
var
  FileName: string;
  FileHandle: TFileHandle;
  sFileType: string;
  ExSize: LongInt;
  lIndex, lIndex2: DWord;
  RawSize, RawAddr: DWord;
  I: DWord;
  Drive: PLogicalVolume;
begin
  ConstructorInit();

  Drive := Drives.D;
  if not Drive.OpenImage(aFileName, {out}FileName) then
  begin
    MessageDlg(DxbxFormat('Could not open path : %s', [aFileName]), mtError, [mbOk], 0);
    Exit;
  end;

  MyFile := TMemoryStream.Create;

  FileHandle := Drive.FileSystem.Open(FileName);
  try
    MyFile.Size := Drive.FileSystem.Seek(FileHandle, 0, soFromEnd);
    Drive.FileSystem.Seek(FileHandle, 0, soFromBeginning);
    Drive.FileSystem.Read(FileHandle, MyFile.Memory^, MyFile.Size);
  finally
    Drive.FileSystem.Close(FileHandle);
  end;

  FRawData := MyFile.Memory;

  // verify xbe file was opened
  sFileType := ExtractFileExt(aFileName);

  if MyFile.Size = 0 then
  begin
    MessageDlg(DxbxFormat('Could not open %s file', [sFileType]), mtError, [mbOk], 0);
    Exit;
  end;

  WriteLog(DxbxFormat('DXBX: Opening %s file...OK', [sFileType]));

  // remember xbe path
  XbePath := aFileName;
  WriteLog(DxbxFormat('DXBX: Storing %s Path...Ok', [sFileType]));

  if MyFile.Size < SizeOf(m_Header) then
  begin
    MessageDlg(DxbxFormat('Unexpected end of file while reading %s Image Header', [sFileType]), mtError, [mbOk], 0);
    Exit;
  end;

  i := 0;
  CopyMemory(@m_Header, RawData, SizeOf(m_Header));
  Inc(i, SizeOf(m_Header));

  // check xbe image header
  if m_Header.dwMagic <> _MagicNumber then
  begin
    MessageDlg(DxbxFormat('Invalid magic number in %s file', [sFileType]), mtError, [mbOk], 0);
    Exit;
  end;

  WriteLog('DXBX: Reading Image Header...Ok');

  ExSize := RoundUp(m_Header.dwSizeofHeaders, PE_HEADER_ALIGNMENT) - SizeOf(m_Header);

  // Read Xbe Image Header Extra Bytes
  if (m_Header.dwSizeofHeaders > SizeOf(m_Header)) then
  begin
    WriteLog('DXBX: Reading Image Header Extra Bytes...');

    SetLength(m_HeaderEx, ExSize);
    CopyMemory(m_HeaderEx, @(RawData[i]), ExSize);
//    Inc(i, ExSize);
  end;

  // read xbe certificate
  i := m_Header.dwCertificateAddr - m_Header.dwBaseAddr;
  CopyMemory(@m_Certificate, @(RawData[i]), SizeOf(m_Certificate));

  WriteLog('DXBX: Reading Certificate...OK');

  m_szAsciiTitle := WideCharToString(m_Certificate.wszTitleName);
  WriteLog('DXBX: Title identified as ' + m_szAsciiTitle);

  // read xbe section headers
  begin
{$IFDEF DXBX_DEBUG}
    DbgPrintf('DXBX: Reading Section Headers...');
{$ENDIF}
    i := m_Header.dwSectionHeadersAddr - m_Header.dwBaseAddr;
    SetLength(m_SectionHeader, m_Header.dwSections);
    for lIndex := 0 to m_Header.dwSections - 1 do
    begin
      CopyMemory(@(m_SectionHeader[lIndex]), @(RawData[i]), SizeOf(m_SectionHeader[lIndex]));
      Inc(i, SizeOf(m_SectionHeader[lIndex]));

{$IFDEF DXBX_DEBUG}
      DbgPrintf('DXBX: Reading Section Header 0x%.4x... OK', [lIndex]);
{$ENDIF}
    end;
  end;

  // Read xbe section names
  begin
{$IFDEF DXBX_DEBUG}
    DbgPrintf('DXBX: Reading Section Names...');
{$ENDIF}
    SetLength(m_szSectionName, m_Header.dwSections, XBE_SECTIONNAME_MAXLENGTH);
    for lIndex := 0 to m_Header.dwSections - 1 do
    begin
      RawAddr := GetAddr(m_SectionHeader[lIndex].dwSectionNameAddr);
      if RawAddr <> 0 then
      begin
        for lIndex2 := 0 to XBE_SECTIONNAME_MAXLENGTH - 1 do
        begin
          m_szSectionName[lIndex][lIndex2] := AnsiChar(RawData[RawAddr + lIndex2]);
          if m_szSectionName[lIndex][lIndex2] = #0 then
            break;
        end; // for lIndex2
      end; // if
{$IFDEF DXBX_DEBUG}
      DbgPrintf('DXBX: Reading Section Name 0x%.04X... OK (%s)', [lIndex, PAnsiChar(m_szSectionName[lIndex])]);
{$ENDIF}
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
      CopyMemory(@(m_LibraryVersion[lIndex]), @(RawData[i]), SizeOf(m_LibraryVersion[lIndex]));
      Inc(i, SizeOf(m_LibraryVersion[lIndex]));

      WriteLog(DxbxFormat('DXBX: Reading Library Version 0x%.4x... OK', [lIndex]));
    end;

    // read xbe kernel library version
    WriteLog('DXBX: Reading Kernel Library Version...');
    if m_Header.dwKernelLibraryVersionAddr = 0 then
      MessageDlg('Could not locate kernel library version', mtError, [mbOk], 0);

    i := m_Header.dwKernelLibraryVersionAddr - m_Header.dwBaseAddr;
    CopyMemory({Dest=}@m_KernelLibraryVersion, {Source=}@(RawData[i]), SizeOf(XBE_LIBRARYVERSION));
    WriteLog(DxbxFormat('DXBX: Kernel Library Version = %d... OK', [m_KernelLibraryVersion.wBuildVersion]));

    // read xbe xapi library version
    WriteLog('DXBX: Reading Xapi Library Version...');
    if m_Header.dwXAPILibraryVersionAddr = 0 then
      MessageDlg('Could not locate Xapi Library Version', mtError, [mbOk], 0);

    i := m_Header.dwXAPILibraryVersionAddr - m_Header.dwBaseAddr;
    CopyMemory({Dest=}@m_XAPILibraryVersion, @(RawData[i]), SizeOf(XBE_LIBRARYVERSION));
    WriteLog(DxbxFormat('DXBX: XAPI Library Version = %d... OK', [m_XAPILibraryVersion.wBuildVersion]));
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
          m_bzSection[lIndex][lIndex2] := Byte(RawData[RawAddr + lIndex2]);

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
      m_TLS.dwDataStartAddr := GetDwordVal(RawData, i);
      i := i + 4;
      m_TLS.dwDataEndAddr := GetDwordVal(RawData, i);
      i := i + 4;
      m_TLS.dwTLSIndexAddr := GetDwordVal(RawData, i);
      i := i + 4;
      m_TLS.dwTLSCallbackAddr := GetDwordVal(RawData, i);

      i := i + 4;
      m_TLS.dwSizeofZeroFill := GetDwordVal(RawData, i);

      i := i + 4;
      m_TLS.dwCharacteristics := GetDwordVal(RawData, i);
    end;
  end;
end; // TXbe.Create

destructor TXbe.Destroy;
begin
  ConstructorInit();

  inherited Destroy;
end;

function TXbe.GetTLSData: DWord;
begin
  if m_TLS.dwDataStartAddr = 0 then
    Result := 0
  else
    Result := GetAddr(m_TLS.dwDataStartAddr);
end; // TXbe.GetTLSData

function TXbe.DetermineDumpFileName: string;
begin
  // Use Title, or when that's empty, the parent folder name : 
  Result := WideCharToString(m_Certificate.wszTitleName);
  if Result = '' then
    Result := 'xbe';

  // Fixup invalid filename characters :
  Result := FixInvalidFilePath(Result);

  // Replace '_' with space :
  Result := StringReplace(Result, '_', ' ', [rfReplaceAll]);

  // Try to capitalize string better :
  Result := RecapitalizeString(Result);

  // Include game region (and possibly version) :
  if (m_Certificate.dwVersion > 0)
  or (m_Certificate.dwGameRegion > 0) then
  begin
    Result := Result + '-' + GameRegionToString(m_Certificate.dwGameRegion);
    if (m_Certificate.dwVersion > 1) and (m_Certificate.dwVersion < 20) then
      Result := Result + ' V' + IntToStr(m_Certificate.dwVersion);
  end;

  Result := Result + '.txt';
end;

function BetterTime(x_timeDate: uint32): string;
begin
  Result := '';
  DateTimeToString(Result, 'ddd mmm dd hh:mm:ss yyyy', CTimeToDateTime(x_timeDate));
end;

function TXbe.DumpInformation(FileName: string): Boolean;
var
  FileEx: TextFile;
  lIndex, lIndex2: Integer;
  TmpStr: string;
  StrAsciiFileName: string;
  Flag: Byte;
//  BIndex: Byte;
  QVersion: Word;

  DumpToFile: Boolean;

  procedure _LogEx(Text: string);
  begin
    if DumpToFile then
      Writeln(FileEx, Text)
    else
      WriteLog(Text);
  end;

begin
  Result := True;

  DumpToFile := (FileName <> '');
  if DumpToFile then
  begin
    AssignFile({var}FileEx, FileName);
    Rewrite({var}FileEx);
  end;

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
  _LogEx(DxbxFormat('TimeDate Stamp                   : 0x%.8x (%s)', [m_Header.dwTimeDate, BetterTime(m_Header.dwTimeDate)]));
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
    TmpStr := TmpStr + Char(RawData[lIndex]);
    Inc(lIndex2);
    lIndex := lIndex + 2;
    if Char(RawData[lIndex]) = #0 then
      break;
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
  _LogEx(DxbxFormat('(PE) TimeDate Stamp              : 0x%.8x (%s)', [m_Header.dwPeTimeDate, BetterTime(m_Header.dwPeTimeDate)]));
  _LogEx(DxbxFormat('Debug PathName Address           : 0x%.8x ("%s")', [m_Header.dwDebugPathNameAddr, GetAddrStr(m_Header.dwDebugPathNameAddr)]));
  _LogEx(DxbxFormat('Debug FileName Address           : 0x%.8x ("%s")', [m_Header.dwDebugFileNameAddr, GetAddrStr(m_Header.dwDebugFileNameAddr)]));
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
  _LogEx(DxbxFormat('TimeDate Stamp                   : 0x%.8x (%s)', [m_Certificate.dwTimeDate, BetterTime(m_Certificate.dwTimeDate)]));
  _LogEx(DxbxFormat('Title ID                         : 0x%.8x', [m_Certificate.dwTitleId]));
  _LogEx(DxbxFormat('Title                            : "%s"', [m_szAsciiTitle]));

  // print alternate titles
  _LogEx(DxbxFormat('Alternate Titles IDs             : 0x%.8x', [m_Certificate.dwAlternateTitleId[0]]));
  for lIndex := 1 to 15 do
    _LogEx(DxbxFormat('                                   0x%.8x', [m_Certificate.dwAlternateTitleId[lIndex]]));

  _LogEx(DxbxFormat('Allowed Media                    : 0x%.8x', [m_Certificate.dwAllowedMedia]));
  _LogEx(DxbxFormat('Game Region                      : 0x%.8x (%s)', [m_Certificate.dwGameRegion, GameRegionToString(m_Certificate.dwGameRegion)]));
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
    for lIndex2 := 0 to XBE_SECTIONNAME_MAXLENGTH - 1 do
    begin
      if m_szSectionName[lIndex][lIndex2] <> #0 then
        TmpStr := TmpStr + Char(m_szSectionName[lIndex][lIndex2])
      else
        break;
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

// TODO -oDXBX: Return real adresses like Cxbx
function TXbe.GetAddr(x_dwVirtualAddress: DWord): Integer;
var
  lIndex, VirtAddr, VirtSize, dwoffs: DWord;
begin
  dwoffs := x_dwVirtualAddress - m_Header.dwBaseAddr;
  Result := 0;
  // offset in image header
  if dwoffs < SizeOf(m_Header) then
    Result := dwOffs
  else
  begin
    // offset in image header extra bytes
    if dwoffs < m_Header.dwSizeofHeaders then
      Result := dwOffs// - SizeOf(m_Header) // TODO -oDXBX: Return adresses in m_HeaderEx
    else
    begin
      // offset into some random section
      for lIndex := 0 to m_Header.dwSections - 1 do
      begin
        VirtAddr := m_SectionHeader[lIndex].dwVirtualAddr;
        VirtSize := m_SectionHeader[lIndex].dwVirtualSize;
        if (x_dwVirtualAddress >= VirtAddr) and (x_dwVirtualAddress < (VirtAddr + VirtSize)) then
        begin
          Result := m_SectionHeader[lIndex].dwRawAddr + (x_dwVirtualAddress - VirtAddr);
          Exit;
        end;
      end;
    end;
  end;
end;

function TXbe.GetAddrStr(x_dwVirtualAddress: DWord; const aMaxLen: Integer = MaxInt): string;
var
  lIndex: Integer;
begin
  lIndex := GetAddr(x_dwVirtualAddress);
  Result := '';
  try
    Result := string(PCharToString(PAnsiChar(@RawData[lIndex]), aMaxLen));
  except
    // ignore - probably out of bounds read
  end;
end;

function TXbe.GetAddrWStr(x_dwVirtualAddress: DWord; const aMaxLen: Integer = MaxInt): WideString;
var
  lIndex: Integer;
begin
  lIndex := GetAddr(x_dwVirtualAddress);
  Result := '';
  try
    Result := PWideCharToString(@RawData[lIndex], aMaxLen);
  except
    // ignore - probably out of bounds read
  end;
end;

function TXbe.GetFileSize: Int64;
begin
  Result := MyFile.Size;
end;

// TXbe.GetAddr

function TXbe.FindSection(const aSectionName: string; out Size: Integer): TRawSection;
var
  i: Integer;
begin
  for i := 0 to m_Header.dwSections - 1 do
  begin
    if SameText(aSectionName, string(StrLPas(PAnsiChar(m_szSectionName[i]), XBE_SECTIONNAME_MAXLENGTH))) then
    begin
      Result := m_bzSection[i];
      {out}Size := m_SectionHeader[i].dwSizeofRaw;
      Exit;
    end;
  end;

  Result := nil;
end;

(*
// import logo bitmap from raw monochrome data (100x17 8bit grayscale)
procedure TXbe.ImportLogoBitmap(const uint08 x_Gray[100*17])
var
  LogoBuffer: PAnsiChar;
  LogoSize: uint32;
begin
  char  *LogoBuffer = new char[4*1024];
  LogoSize := 0;

  // encode logo bitmap
  begin
    for(uint32 v=1;v<100*17;LogoSize++)
    begin
      char color = x_Gray[v] shr 4;

      uint32 len = 1;

      while(++v<100*17-1) and (len < 1024) and (color = (x_Gray[v] shr 4)) do
        Inc(len);

      LogoRLE *cur = (LogoRLE * )@LogoBuffer[LogoSize];

      if (len <= 7) then
      begin
        cur.m_Eight.bType1 := 1;
        cur.m_Eight.Len    := len;
        cur.m_Eight.Data   := color;
      end;
      else
      begin
        cur.m_Sixteen.bType1 := 0;
        cur.m_Sixteen.bType2 := 1;
        cur.m_Sixteen.Len    := len;
        cur.m_Sixteen.Data   := color;
        Inc(LogoSize);
      end;
    end;
  end;

  // check if there is room to save this, if not then throw an error
  begin
    uint08 *RLE = GetLogoBitmap(LogoSize);

    if (RLE = 0) then
    begin
      if (GetError() = 0) then
        SetError('Logo bitmap could not be imported (not enough space in file?)', False);

      Exit;
    end;

    memcpy(RLE, LogoBuffer, LogoSize);
  end;
end;
*)

// export raw monochrome data (100x17 8bit grayscale) to logo bitmap
function TXbe.ExportLogoBitmap(aBitmap: TBitmap): Boolean;
const
  XBE_LOGO_WIDTH = 100;
  XBE_LOGO_HEIGHT = 17;
var
  x_Gray: Byte;
  dwLength, x, y, lIndex, lIndex2, Len, Data: DWord;
  RLE: DWord;
  pos0, pos1: Byte;
begin
  Result := False;
  dwLength := m_Header.dwSizeofLogoBitmap;
  RLE := GetAddr(m_Header.dwLogoBitmapAddr);
  if RLE = 0 then
    Exit;

  try
    aBitmap.PixelFormat := pf32bit;
    aBitmap.SetSize(XBE_LOGO_WIDTH, XBE_LOGO_HEIGHT);

    Len := 0;
    Data := 0;
    x := 0;
    y := 0;
    lIndex := 0;
    while lIndex < dwLength do
    begin
      // Read 2 bytes.
      Pos0 := Ord(RawData[RLE + lIndex]);
      Pos1 := Ord(RawData[RLE + 1 + lIndex]);

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

      lIndex2 := 0; while lIndex2 < Len do
      begin
        x_Gray := Byte(Data shl 4);
        aBitmap.Canvas.Pixels[x, y] := RGB(x_Gray, x_Gray, x_Gray);

        Inc(x);
        if x = XBE_LOGO_WIDTH then
        begin
          x := 0;
          Inc(y);
          if y = XBE_LOGO_HEIGHT then
          begin
            lIndex := dwLength;
            break;
          end;
        end;
        Inc(lIndex2);
      end;

      Inc(lIndex); // Index increment
    end;

    Result := True;
  except
    Result := False;
  end;
end; // TXbe.ExportLogoBitmap

// Reads the $$XTIMAGE Section, decodes the data and returns the bitmap;
function TXbe.ExportIconBitmap(aBitmap: TBitmap): Boolean;
var
  Section: TRawSection;
  SectionSize: Integer;
begin
  Result := False;
  
  // Find icon section :
  Section := FindSection(XBE_SECTIONNAME_GAMEICON, {out}SectionSize);
  if Section = nil then
  begin
    Section := FindSection(XBE_SECTIONNAME_SAVEICON, {out}SectionSize);
    if Section = nil then
      Exit;
  end;

  Result := ExportXPRToBitmap(PXPR_IMAGE(Section), aBitmap);
end;

function TXbe.ExportXPRToBitmap(XprImage: PXPR_IMAGE; aBitmap: TBitmap): Boolean;
var
  Width, Height: Cardinal;
  Scanlines: RGB32Scanlines;
begin
  Result := False;

  // Check if it's an XPR (Xbox Packed Resources) :
  if XprImage.hdr.Header.dwMagic = XPR_MAGIC_VALUE then
  begin
    // Determine image dimensions :
    Width := 1 shl ((XprImage.hdr.Texture.Format and X_D3DFORMAT_USIZE_MASK) shr X_D3DFORMAT_USIZE_SHIFT);
    Height := 1 shl ((XprImage.hdr.Texture.Format and X_D3DFORMAT_VSIZE_MASK) shr X_D3DFORMAT_VSIZE_SHIFT);

    // Prepare easy access to the bitmap data :
    aBitmap.PixelFormat := pf32bit;
    aBitmap.SetSize(Width, Height);
    Scanlines.Initialize(aBitmap);

    // Read the texture into the bitmap :
    Result := ReadD3DTextureFormatIntoBitmap(
      {Format=}(XprImage.hdr.Texture.Format and X_D3DFORMAT_FORMAT_MASK) shr X_D3DFORMAT_FORMAT_SHIFT,
      {Data=}PBytes(@(XprImage.pBits[0])),
      {DataSize=}XprImage.hdr.Header.dwTotalSize - XprImage.hdr.Header.dwHeaderSize,
      {Output=}@Scanlines);
      
    Exit;
  end;

// TODO -oDXBX: Check for 'DDS' format, and read that too, perhaps using these resources :
// http://www.imageconverterplus.com/help-center/about-icp/supported-formats/dds/
// http://archive.netbsd.se/view_attachment.php?id=2463254.32112
//      if StrLPas(PAnsiChar(@XprImage.hdr.Header.dwMagic), 3) = 'DDS' then
//        offset := 124;
end;

end.
