unit uXbe;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, Math, ActnList, ExtCtrls, StrUtils,
  // Dxbx
  uConsts, uExe, uBitsOps, uTime;

type
  TLogType = (ltLog, ltFile);
  TFileType = (ftXbe, ftExe);

  _XBE_HEADER = packed record
    dwMagic: array[0..3] of Char; // 0x0000 - magic number [should be "XBEH"]
    pbDigitalSignature: array[0..255] of Char; // 0x0004 - digital signature
    dwBaseAddr: DWord; // 0x0104 - base address
    dwSizeofHeaders: DWord; // 0x0108 - size of headers
    dwSizeofImage: DWord; // 0x010C - size of image
    dwSizeofImageHeader: DWord; // 0x0110 - size of image header
    dwTimeDate: DWord; // 0x0114 - timedate stamp
    dwCertificateAddr: DWord; // 0x0118 - certificate address
    dwSections: DWord; // 0x011C - number of sections
    dwSectionHeadersAddr: DWord; // 0x0120 - section headers address

    dwInitFlags: array[0..3] of Char; // 0x0124 - initialization flags

            //struct InitFlags                       // 0x0124 - initialization flags
            {
                uint32 bMountUtilityDrive   : 1;     // mount utility drive flag
                uint32 bFormatUtilityDrive  : 1;     // format utility drive flag
                uint32 bLimit64MB           : 1;     // limit development kit run time memory to 64mb flag
                uint32 bDontSetupHarddisk   : 1;     // don't setup hard disk flag
                uint32 Unused               : 4;     // unused (or unknown)
                uint32 Unused_b1            : 8;     // unused (or unknown)
                uint32 Unused_b2            : 8;     // unused (or unknown)
                uint32 Unused_b3            : 8;     // unused (or unknown)
            }
            //dwInitFlags;

    dwEntryAddr: DWord; // 0x0128 - entry point address
    dwTLSAddr: DWord; // 0x012C - thread local storage directory address
    dwPeStackCommit: DWord; // 0x0130 - size of stack commit
    dwPeHeapReserve: DWord; // 0x0134 - size of heap reserve
    dwPeHeapCommit: DWord; // 0x0138 - size of heap commit
    dwPeBaseAddr: DWord; // 0x013C - original base address
    dwPeSizeofImage: DWord; // 0x0140 - size of original image
    dwPeChecksum: DWord; // 0x0144 - original checksum
    dwPeTimeDate: DWord; // 0x0148 - original timedate stamp
    dwDebugPathnameAddr: DWord; // 0x014C - debug pathname address
    dwDebugFilenameAddr: DWord; // 0x0150 - debug filename address
    dwDebugUnicodeFilenameAddr: DWord; // 0x0154 - debug unicode filename address
    dwKernelImageThunkAddr: DWord; // 0x0158 - kernel image thunk address
    dwNonKernelImportDirAddr: DWord; // 0x015C - non kernel import directory address
    dwLibraryVersions: DWord; // 0x0160 - number of library versions
    dwLibraryVersionsAddr: DWord; // 0x0164 - library versions address
    dwKernelLibraryVersionAddr: DWord; // 0x0168 - kernel library version address
    dwXAPILibraryVersionAddr: DWord; // 0x016C - xapi library version address
    dwLogoBitmapAddr: DWord; // 0x0170 - logo bitmap address
    dwSizeofLogoBitmap: DWord; // 0x0174 - logo bitmap size
  end;
  P_XBE_HEADER = ^_XBE_HEADER;
  XBE_HEADER = _XBE_HEADER;

  _XBE_CERTIFICATE = packed record
    dwSize: DWord; // 0x0000 - size of certificate
    dwTimeDate: DWord; // 0x0004 - timedate stamp
    dwTitleId: DWord; // 0x0008 - title id
    wszTitleName: array[0..39] of widechar; // 0x000C - title name (unicode)
    dwAlternateTitleId: array[0..15] of Dword; // 0x005C - alternate title ids
    dwAllowedMedia: Dword; // 0x009C - allowed media types
    dwGameRegion: DWord; // 0x00A0 - game region
    dwGameRatings: DWord; // 0x00A4 - game ratings
    dwDiskNumber: DWord; // 0x00A8 - disk number
    dwVersion: Dword; // 0x00AC - version
    bzLanKey: array[0..15] of Char; // 0x00B0 - lan key
    bzSignatureKey: array[0..15] of Char; // 0x00C0 - signature key
    bzTitleAlternateSignatureKey: array[0..15] of array[0..15] of Char; // 0x00D0 - alternate signature keys
  end;
  XBE_CERTIFICATE = _XBE_CERTIFICATE;


  _XBE_SECTIONHEADER = packed record

     { struct _Flags
            {
                uint32 bWritable        : 1;       // writable flag
                uint32 bPreload         : 1;       // preload flag
                uint32 bExecutable      : 1;       // executable flag
                uint32 bInsertedFile    : 1;       // inserted file flag
                uint32 bHeadPageRO      : 1;       // head page read only flag
                uint32 bTailPageRO      : 1;       // tail page read only flag
                uint32 Unused_a1        : 1;       // unused (or unknown)
                uint32 Unused_a2        : 1;       // unused (or unknown)
                uint32 Unused_b1        : 8;       // unused (or unknown)
                uint32 Unused_b2        : 8;       // unused (or unknown)
                uint32 Unused_b3        : 8;       // unused (or unknown)
            }
           // dwFlags;

    dwFlags: array[0..3] of Char;
    dwVirtualAddr: DWord; // virtual address
    dwVirtualSize: DWord; // virtual size
    dwRawAddr: DWord; // file offset to raw Data
    dwSizeofRaw: DWord; // size of raw Data
    dwSectionNameAddr: DWord; // section name addr
    dwSectionRefCount: DWord; // section reference count
    dwHeadSharedRefCountAddr: DWord; // head shared page reference count address
    dwTailSharedRefCountAddr: DWord; // tail shared page reference count address
    bzSectionDigest: array[0..19] of Char; // section digest
  end;
  XBE_SECTIONHEADER = _XBE_SECTIONHEADER;


  _XBE_LIBRARYVERSION = packed record
    szName: array[0..7] of Char; // library name
    wMajorVersion: Word; // major version
    wMinorVersion: Word; // minor version
    wBuildVersion: Word; // build version
    dwFlags: array[0..1] of Char;
           { struct Flags
            {
                uint16 QFEVersion       : 13;      // QFE Version
                uint16 Approved         : 2;       // Approved? (0:no, 1:possibly, 2:yes)
                uint16 bDebugBuild      : 1;       // Is this a debug build?
            }
            //dwFlags;
  end;
  P_XBE_LIBRARYVERSION = ^_XBE_LIBRARYVERSION;
  XBE_LIBRARYVERSION = _XBE_LIBRARYVERSION;

  _XBE_TLS = packed record
        {
            uint32 dwDataStartAddr;             // raw start address
            uint32 dwDataEndAddr;               // raw end address
            uint32 dwTLSIndexAddr;              // tls index  address
            uint32 dwTLSCallbackAddr;           // tls callback address
            uint32 dwSizeofZeroFill;            // size of zero fill
            uint32 dwCharacteristics;           // characteristics
        }
    dwDataStartAddr: DWord; // raw start address
    dwDataEndAddr: DWord; // raw end address
    dwTLSIndexAddr: DWord; // tls index  address
    dwTLSCallbackAddr: DWord; // tls callback address
    dwSizeofZeroFill: DWord; // size of zero fill
    dwCharacteristics: DWord; // characteristics
  end;
  P_XBE_TLS = ^_XBE_TLS;
  XBE_TLS = _XBE_TLS;

  _Eight = Char;
    //Bit 0  : bType1
    //Bit 1,2,3  : Len
    //Bit 4,5,6,7 : Data
  Eight = _Eight;

  _Sixteen = array[0..1] of Char;
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
    Buffer: PChar;
    m_KernelLibraryVersion: array of Char;
    m_XAPILibraryVersion: array of Char;
    m_Certificate: XBE_CERTIFICATE;
    procedure ConstructorInit;
  protected
    XbeFile: file of Char;
    m_LogoRLE: LogoRLE;
  public
    m_szPath: string;
    m_Header: XBE_HEADER;
    m_SectionHeader: array of XBE_SECTIONHEADER;
    m_LibraryVersion: array of XBE_LIBRARYVERSION;
    m_szSectionName: array of array of Char;
    m_HeaderEx: array of Char;
    m_TLS: XBE_TLS;
    m_bzSection: array of TVarCharArray;

    constructor Create(aFileName: string; aFileType: TFileType);
    destructor Destroy; override;

    function DumpInformation(FileName: string = ''): Boolean;
    function GetAddr(x_dwVirtualAddress: DWord): Integer;

    procedure ExportLogoBitmap(ImgCont: TBitmap);

    function GetTLSData: DWord;
  end;

function GetDWordVal(ArrPChar: PChar; i: Integer): DWord;
function GetWordVal(ArrPChar: PChar; i: Integer): Word;
function RoundUp(dwValue, dwMult: DWord): DWord;

var
  m_szAsciiTitle: string;

implementation

uses
  // Dxbx
  uLog;

//------------------------------------------------------------------------------

function RoundUp(dwValue, dwMult: DWord): DWord;
begin
  if dwMult = 0 then
    Result := dwValue
  else
    Result := dwValue - ((dwValue - 1) mod dwMult) + (dwMult - 1);
end; // RoundUp

//------------------------------------------------------------------------------

function GetDWordVal(ArrPChar: PChar; i: Integer): DWord;
begin
  Result :=
    GetBitEn(Ord(ArrPChar[i]), 0) * 1 +
    GetBitEn(Ord(ArrPChar[i]), 1) * 2 +
    GetBitEn(Ord(ArrPChar[i]), 2) * 4 +
    GetBitEn(Ord(ArrPChar[i]), 3) * 8 +
    GetBitEn(Ord(ArrPChar[i]), 4) * 16 +
    GetBitEn(Ord(ArrPChar[i]), 5) * 32 +
    GetBitEn(Ord(ArrPChar[i]), 6) * 64 +
    GetBitEn(Ord(ArrPChar[i]), 7) * 128 +
    GetBitEn(Ord(ArrPChar[i + 1]), 0) * 256 +
    GetBitEn(Ord(ArrPChar[i + 1]), 1) * 512 +
    GetBitEn(Ord(ArrPChar[i + 1]), 2) * 1024 +
    GetBitEn(Ord(ArrPChar[i + 1]), 3) * 2048 +
    GetBitEn(Ord(ArrPChar[i + 1]), 4) * 4096 +
    GetBitEn(Ord(ArrPChar[i + 1]), 5) * 8192 +
    GetBitEn(Ord(ArrPChar[i + 1]), 6) * 16384 +
    GetBitEn(Ord(ArrPChar[i + 1]), 7) * 32768 +
    GetBitEn(Ord(ArrPChar[i + 2]), 0) * 65536 +
    GetBitEn(Ord(ArrPChar[i + 2]), 1) * Round(POWER(2, 17)) +
    GetBitEn(Ord(ArrPChar[i + 2]), 2) * Round(POWER(2, 18)) +
    GetBitEn(Ord(ArrPChar[i + 2]), 3) * Round(POWER(2, 19)) +
    GetBitEn(Ord(ArrPChar[i + 2]), 4) * Round(POWER(2, 20)) +
    GetBitEn(Ord(ArrPChar[i + 2]), 5) * Round(POWER(2, 21)) +
    GetBitEn(Ord(ArrPChar[i + 2]), 6) * Round(POWER(2, 22)) +
    GetBitEn(Ord(ArrPChar[i + 2]), 7) * Round(POWER(2, 23)) +
    GetBitEn(Ord(ArrPChar[i + 3]), 0) * Round(POWER(2, 24)) +
    GetBitEn(Ord(ArrPChar[i + 3]), 1) * Round(POWER(2, 25)) +
    GetBitEn(Ord(ArrPChar[i + 3]), 2) * Round(POWER(2, 26)) +
    GetBitEn(Ord(ArrPChar[i + 3]), 3) * Round(POWER(2, 27)) +
    GetBitEn(Ord(ArrPChar[i + 3]), 4) * Round(POWER(2, 28)) +
    GetBitEn(Ord(ArrPChar[i + 3]), 5) * Round(POWER(2, 29)) +
    GetBitEn(Ord(ArrPChar[i + 3]), 6) * Round(POWER(2, 30)) +
    GetBitEn(Ord(ArrPChar[i + 3]), 7) * Round(POWER(2, 31));
end; // GetDwordVal

//------------------------------------------------------------------------------

function GetWordVal(ArrPChar: PChar; i: Integer): Word;
begin
  Result :=
    GetBitEn(Ord(ArrPChar[i]), 0) * 1 +
    GetBitEn(Ord(ArrPChar[i]), 1) * 2 +
    GetBitEn(Ord(ArrPChar[i]), 2) * 4 +
    GetBitEn(Ord(ArrPChar[i]), 3) * 8 +
    GetBitEn(Ord(ArrPChar[i]), 4) * 16 +
    GetBitEn(Ord(ArrPChar[i]), 5) * 32 +
    GetBitEn(Ord(ArrPChar[i]), 6) * 64 +
    GetBitEn(Ord(ArrPChar[i]), 7) * 128 +
    GetBitEn(Ord(ArrPChar[i + 1]), 0) * 256 +
    GetBitEn(Ord(ArrPChar[i + 1]), 1) * 512 +
    GetBitEn(Ord(ArrPChar[i + 1]), 2) * 1024 +
    GetBitEn(Ord(ArrPChar[i + 1]), 3) * 2048 +
    GetBitEn(Ord(ArrPChar[i + 1]), 4) * 4096 +
    GetBitEn(Ord(ArrPChar[i + 1]), 5) * 8192 +
    GetBitEn(Ord(ArrPChar[i + 1]), 6) * 16384 +
    GetBitEn(Ord(ArrPChar[i + 1]), 7) * 32768;
end; // GetDwordVal


{ TXbe }
//------------------------------------------------------------------------------

procedure TXbe.ConstructorInit;
begin
  {m_SectionHeader       := '';
  m_szSectionName        := '';
  m_LibraryVersion       := '';
  m_KernelLibraryVersion := '';
  m_XAPILibraryVersion   := '';
  m_TLS                  := '';
  m_bzSection            := '';}
end; // TXbe.ConstructorInit

//------------------------------------------------------------------------------

constructor TXbe.Create(aFileName: string; aFileType: TFileType);
var
  ExeSize: LongInt;
  lIndex, lIndex2: DWord;
  RawSize, RawAddr: DWord;
  I: DWord;
  F: THandle;
  ReadBytes, FileSz: DWord;
  sFileType: string;
begin
  sFileType := ifthen(aFileType = ftXbe, 'Xbe', 'Exe' );

  ConstructorInit();

  RawSize := 0;
  F := FileOpen(aFileName, fmOpenRead);
  FileSz := GetFileSize(F, nil);
  FileClose(F);

  // verify xbe file was opened
  if FileSz = 0 then
  begin
    MessageDlg(Format('Could not open %s file', [sFileType]), mtError, [mbOk], 0);
    Exit;
  end;

  WriteLog(Format('DXBX: Opening %s file...OK', [sFileType]));

  // remember xbe path
  m_szPath := ExtractFilePath(aFileName);
  WriteLog(Format('DXBX: Storing %s Path...Ok', [sFileType]));

  // read xbe image header
  if SizeOf(m_Header) > FileSz then
  begin
    MessageDlg(Format('Unexpected end of file while reading %s Image Header', [sFileType]), mtError, [mbOk], 0);
    Exit;
  end;

  AssignFile(XbeFile, aFileName);
  FileMode := fmOpenRead;
  Reset(XbeFile);
  FreeMem(Buffer);
  GetMem(Buffer, FileSz);
  BlockRead(XbeFile, Buffer^, FileSz, ReadBytes);
  // m_Header.dwMagic Read (4 Bytes)
  i := 0;
  for lIndex := 0 to 3 do
  begin
    m_Header.dwMagic[lIndex] := Buffer[i];
    Inc(i);
  end;

  if m_Header.dwMagic <> _MagicNumber then
  begin
    MessageDlg( Format ( 'Invalid magic number in %s file', [sFileType]) , mtError, [mbOk], 0);
    Exit;
  end;

  // m_Header.pbDigitalSignature Read (256 Bytes)
  for lIndex := 4 to 259 do
  begin
    m_Header.pbDigitalSignature[i - 4] := Buffer[i];
    Inc(i);
  end;

  // m_Header.dwBaseAddr Read (4 bytes)
  m_Header.dwBaseAddr := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwSizeofHeaders Read (4 bytes)
  m_Header.dwSizeofHeaders := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwSizeofImage Read (4 bytes)
  m_Header.dwSizeofImage := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwSizeofImageHeader Read (4 bytes)
  m_Header.dwSizeofImageHeader := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwTimeDate Read (4 bytes)
  m_Header.dwTimeDate := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwCertificateAddr Read (4 bytes)
  m_Header.dwCertificateAddr := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwSections Read (4 bytes)
  m_Header.dwSections := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwSectionHeadersAddr Read (4 bytes)
  m_Header.dwSectionHeadersAddr := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwInitFlags Read (4 bytes)
  for lIndex := 0 to 3 do
    m_Header.dwInitFlags[lIndex] := Buffer[lIndex + i];

  i := i + 4;
  // m_Header.dwEntryAddr Read (4 bytes)
  m_Header.dwEntryAddr := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwTLSAddr Read (4 bytes)
  m_Header.dwTLSAddr := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwPeStackCommit Read (4 bytes)
  m_Header.dwPeStackCommit := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwPeHeapReserve Read (4 bytes)
  m_Header.dwPeHeapReserve := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwPeHeapCommit Read (4 bytes)
  m_Header.dwPeHeapCommit := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwPeBaseAddr Read (4 bytes)
  m_Header.dwPeBaseAddr := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwPeSizeofImage Read (4 bytes)
  m_Header.dwPeSizeofImage := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwPeChecksum Read (4 bytes)
  m_Header.dwPeChecksum := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwPeTimeDate Read (4 bytes)
  m_Header.dwPeTimeDate := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwDebugPathnameAddr Read (4 bytes)
  m_Header.dwDebugPathnameAddr := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwDebugFilenameAddr Read (4 bytes)
  m_Header.dwDebugFilenameAddr := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwDebugUnicodeFilenameAddr Read (4 bytes)
  m_Header.dwDebugUnicodeFilenameAddr := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwKernelImageThunkAddr Read (4 bytes)
  m_Header.dwKernelImageThunkAddr := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwNonKernelImportDirAddr Read (4 bytes)
  m_Header.dwNonKernelImportDirAddr := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwLibraryVersions Read (4 bytes)
  m_Header.dwLibraryVersions := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwLibraryVersionsAddr Read (4 bytes)
  m_Header.dwLibraryVersionsAddr := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwKernelLibraryVersionAddr Read (4 bytes)
  m_Header.dwKernelLibraryVersionAddr := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwXAPILibraryVersionAddr Read (4 bytes)
  m_Header.dwXAPILibraryVersionAddr := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwLogoBitmapAddr Read (4 bytes)
  m_Header.dwLogoBitmapAddr := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Header.dwSizeofLogoBitmap Read (4 bytes)
  m_Header.dwSizeofLogoBitmap := GetDwordVal(Buffer, i);
  i := i + 4;

  WriteLog('DXBX: Reading Image Header...Ok');

  // Read Xbe Image Header Extra Bytes
  if (m_Header.dwSizeofHeaders > SizeOf(m_Header)) then
  begin
    WriteLog('DXBX: Reading Image Header Extra Bytes... NOT DONE YET');
  end;

  ExeSize := RoundUp(m_Header.dwSizeofHeaders, $1000) - SizeOf(m_Header);

  if SizeOf(m_HeaderEx) > FileSz then
  begin
    MessageDlg(Format ('Unexpected end of file while reading %s Image Header (Ex)',[sFileType] ), mtError, [mbOk], 0);
    Exit;
  end;

  try
    SetLength(m_HeaderEx, ExeSize);
    for lIndex := 0 to ExeSize - 1 do
    begin
      m_HeaderEx[lIndex] := Buffer[i];
      Inc(i);
    end;

    WriteLog('Ok');
  except
    WriteLog('Error');
  end;

  // read xbe certificate
  i := m_Header.dwCertificateAddr - m_Header.dwBaseAddr;

  // m_Certificate.dwSize Read (4 bytes)
  m_Certificate.dwSize := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Certificate.dwTimeDate Read (4 bytes)
  m_Certificate.dwTimeDate := GetDwordVal(Buffer, i);
  i := i + 4;
  // m_Certificate.dwTitleId Read (4 bytes)
  m_Certificate.dwTitleId := GetDwordVal(Buffer, i);
  i := i + 4;

  //m_Certificate.wszTitleName array of 40 widechar (2 bytes each wdchr so i need to store 80 bytes)
  for lIndex := 0 to 39 do
  begin
    m_Certificate.wszTitleName[lIndex] := WideChar(Ord(Buffer[i]));
    i := i + 2;
  end;

  m_szAsciiTitle := WideCharToString(m_Certificate.wszTitleName);

  //m_Certificate.dwAlternateTitleId (4 bytes each element of the array)
  for lIndex := 0 to 15 do
  begin
    m_Certificate.dwAlternateTitleId[lIndex] := GetDwordVal(Buffer, i);
    i := i + 4;
  end;

  //m_Certificate.dwAllowedMedia 4 bytes
  m_Certificate.dwAllowedMedia := GetDwordVal(Buffer, i);
  i := i + 4;
  //m_Certificate.dwGameRegion 4 bytes
  m_Certificate.dwGameRegion := GetDwordVal(Buffer, i);
  i := i + 4;
  //m_Certificate.dwGameRatings 4 bytes
  m_Certificate.dwGameRatings := GetDwordVal(Buffer, i);
  i := i + 4;
  //m_Certificate.dwDiskNumber 4 bytes
  m_Certificate.dwDiskNumber := GetDwordVal(Buffer, i);
  i := i + 4;
  //m_Certificate.dwVersion 4 bytes
  m_Certificate.dwVersion := GetDwordVal(Buffer, i);
  i := i + 4;
  //m_Certificate.bzLanKey 1 Char
  for lIndex := 0 to 15 do
    m_Certificate.bzLanKey[lIndex] := Buffer[i + lIndex];

  i := i + 16;
  //m_Certificate.bzLanKey 1 Char
  for lIndex := 0 to 15 do
    m_Certificate.bzSignatureKey[lIndex] := Buffer[i + lIndex];

  i := i + 16;
  //m_Certificate.bzLanKey array of array 16x16 Char
  for lIndex := 0 to 15 do
  begin
    for lIndex2 := 0 to 15 do
      m_Certificate.bzTitleAlternateSignatureKey[lIndex][lIndex2] := Buffer[i + lIndex2];

    i := i + 16;
  end;

  WriteLog('DXBX: Reading Certificate...OK');
  WriteLog('DXBX: Title identified as ' + m_szAsciiTitle);

  // read xbe section headers
  i := m_Header.dwSectionHeadersAddr - m_Header.dwBaseAddr;

  SetLength(m_SectionHeader, m_Header.dwSections);

  for lIndex := 0 to m_Header.dwSections - 1 do
  begin
    try
      //XbeFile.Read(m_SectionHeader[lIndex], SizeOf(XBE_SECTIONHEADER));
        //m_SectionHeader[].dwFlags 4 bytes
      for lIndex2 := 0 to 3 do
        m_SectionHeader[lIndex].dwFlags[lIndex2] := Buffer[i + lIndex2];

      i := i + 4;
      // m_SectionHeader[].dwVirtualAddr 4 bytes
      m_SectionHeader[lIndex].dwVirtualAddr := GetDwordVal(Buffer, i);
      i := i + 4;
      // m_SectionHeader[].dwVirtualSize 4 bytes
      m_SectionHeader[lIndex].dwVirtualSize := GetDwordVal(Buffer, i);
      i := i + 4;
      // m_SectionHeader[].dwRawAddr 4 bytes
      m_SectionHeader[lIndex].dwRawAddr := GetDwordVal(Buffer, i);
      i := i + 4;
      // m_SectionHeader[].dwSizeofRaw 4 bytes
      m_SectionHeader[lIndex].dwSizeofRaw := GetDwordVal(Buffer, i);
      i := i + 4;
      // m_SectionHeader[].dwSectionNameAddr 4 bytes
      m_SectionHeader[lIndex].dwSectionNameAddr := GetDwordVal(Buffer, i);
      i := i + 4;
      // m_SectionHeader[].dwSectionRefCount 4 bytes
      m_SectionHeader[lIndex].dwSectionRefCount := GetDwordVal(Buffer, i);
      i := i + 4;
      // m_SectionHeader[].dwHeadSharedRefCountAddr 4 bytes
      m_SectionHeader[lIndex].dwHeadSharedRefCountAddr := GetDwordVal(Buffer, i);
      i := i + 4;
      // m_SectionHeader[].dwTailSharedRefCountAddr 4 bytes
      m_SectionHeader[lIndex].dwTailSharedRefCountAddr := GetDwordVal(Buffer, i);
      i := i + 4;
      // m_SectionHeader[].bzSectionDigest 20 chars
      for lIndex2 := 0 to 19 do
        m_SectionHeader[lIndex].bzSectionDigest[lIndex2] := Buffer[i + lIndex2];

      i := i + 20;
    except
      MessageDlg(Format ( 'Unexpected end of file while reading %s Section Header',[sFileType]) , mtError, [mbOk], 0);
    end;

    WriteLog('DXBX: Reading Section Header 0x%.04X...' + IntToStr(lIndex) + ' OK');
  end;

  // Read xbe section names
  SetLength(m_szSectionName, m_Header.dwSections, 9);
  for lIndex := 0 to m_Header.dwSections - 1 do
  begin
    RawAddr := GetAddr(m_SectionHeader[lIndex].dwSectionNameAddr);
    if m_SectionHeader[lIndex].dwSectionNameAddr <> 0 then
    begin
      for lIndex2 := 0 to 8 do
      begin
        m_szSectionName[lIndex][lIndex2] := Buffer[RawAddr + lIndex2];
        if Ord(m_szSectionName[lIndex][lIndex2]) = 0 then
          Break;
      end; // for lIndex2
    end; // if
  end; // for lIndex

  // Read xbe library versions
  if m_Header.dwLibraryVersionsAddr <> 0 then
  begin
    WriteLog('DXBX: Reading Library Versions...');

    i := m_Header.dwLibraryVersionsAddr - m_Header.dwBaseAddr;

    SetLength(m_LibraryVersion, m_Header.dwLibraryVersions);

    for lIndex := 0 to m_Header.dwLibraryVersions - 1 do
    begin
      WriteLog('DXBX: Reading Library Version 0x' + IntToHex(lIndex, 4) + '....');
      for lIndex2 := 0 to 7 do
        m_LibraryVersion[lIndex].szName[lIndex2] := Buffer[i + lIndex2];

      i := i + 8;
      m_LibraryVersion[lIndex].wMajorVersion := GetWordVal(Buffer, i);
      i := i + 2;
      m_LibraryVersion[lIndex].wMinorVersion := GetWordVal(Buffer, i);
      i := i + 2;
      m_LibraryVersion[lIndex].wBuildVersion := GetWordVal(Buffer, i);
      i := i + 2;
      for lIndex2 := 0 to 1 do
        m_LibraryVersion[lIndex].dwFlags[lIndex2] := Buffer[i + lIndex2];

      i := i + 2;
    end;

    // read xbe kernel library version
    WriteLog('DXBX: Reading Kernel Library Version...');
    if m_Header.dwKernelLibraryVersionAddr = 0 then
      MessageDlg('Could not locate kernel library version', mtError, [mbOk], 0);

    i := m_Header.dwKernelLibraryVersionAddr - m_Header.dwBaseAddr;

    SetLength(m_KernelLibraryVersion, SizeOf(m_LibraryVersion));
    for lIndex := 0 to SizeOf(m_LibraryVersion) - 1 do
      m_KernelLibraryVersion[lIndex] := Buffer[lIndex + i];

    // read xbe xapi library version
    WriteLog('DXBX: Reading Xapi Library Version...');
    if m_Header.dwXAPILibraryVersionAddr = 0 then
      MessageDlg('Could not locate Xapi Library Version', mtError, [mbOk], 0);

    i := m_Header.dwXAPILibraryVersionAddr - m_Header.dwBaseAddr;
    SetLength(m_XAPILibraryVersion, SizeOf(m_LibraryVersion));
    for lIndex := 0 to SizeOf(m_LibraryVersion) - 1 do
      m_XAPILibraryVersion[lIndex] := Buffer[lIndex + i];

    WriteLog('DXBX: Reading Sections...');

    SetLength(m_bzSection, m_Header.dwSections);

    for lIndex := 0 to m_Header.dwSections - 1 do
    begin
      WriteLog('DXBX: Reading Section 0x' + IntToHex(lIndex, 4) + '...');

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
      if RawSize = 0 then
        Break;

      WriteLog('Ok');

      for lIndex2 := 0 to Rawsize - 1 do
        m_bzSection[lIndex][lIndex2] := Buffer[RawAddr + lIndex2];

    end;

    if lIndex2 < Rawsize then
      WriteLog(Format('Unexpected end of file while reading %s Section', [sFileType] ));
  end;

  if m_Header.dwTLSAddr <> 0 then
  begin
    WriteLog('DXBX: Reading Thread Local Storage...');
    if GetAddr(m_Header.dwTLSAddr) <> 0 then
    begin
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
  
  CloseFile(XbeFile);
end; // TXbe.Create

//------------------------------------------------------------------------------

function TXbe.DumpInformation(FileName: string): Boolean;
var
  FileEx: TextFile;
  lIndex, lIndex2: Integer;
  TmpStr: string;
  AsciiFilename: array[0..39] of Char;
  TmpChr: Char;
  StrAsciiFilename: string;
  Flag, BIndex: Byte;
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
    AssignFile(FileEx, FileName);
    Rewrite(FileEx);
    LogType := ltFile;
  end
  else
    LogType := ltLog;

  _LogEx('XBE information generated by DXBX (Version ' + _DXBX_VERSION + ')');
  _LogEx('');

  _LogEx('Title identified as "' + m_szAsciiTitle + '"');
  _LogEx('');

  _LogEx('Dumping XBE file header...');
  _LogEx('');

  _LogEx('Magic Number                     : XBEH');

  _LogEx('Digitial Signature               : <Hex Dump>');
  TmpStr := '';
  lIndex2 := 0;
  for lIndex := 0 to 255 do
  begin
    TmpStr := TmpStr + IntToHex(Ord(m_Header.pbDigitalSignature[lIndex]), 2);

    if lIndex2 = 15 then
    begin
      _LogEx('                                   ' + TmpStr);
      TmpStr := '';
      lIndex2 := -1;
    end;

    Inc(lIndex2);
  end;

  _LogEx('                                   </Hex Dump>');
  _LogEx('Base Address                     : 0x' + IntToHex(m_Header.dwBaseAddr, 8));
  _LogEx('Size of Headers                  : 0x' + IntToHex(m_Header.dwSizeofHeaders, 8));
  _LogEx('Size of Image                    : 0x' + IntToHex(m_Header.dwSizeofImage, 8));
  _LogEx('Size of Image Header             : 0x' + IntToHex(m_Header.dwSizeofImageHeader, 8));
  TmpStr := '';
  DateTimeToString(TmpStr, 'ddd mmm dd hh:mm:ss yyyy', CTimeToDateTime(m_Header.dwTimeDate));
  _LogEx('TimeDate Stamp                   : 0x' + IntToHex(m_Header.dwTimeDate, 8) + ' (' + TmpStr + ')');
  _LogEx('Certificate Address              : 0x' + IntToHex(m_Header.dwCertificateAddr, 8));
  _LogEx('Number of Sections               : 0x' + IntToHex(m_Header.dwSections, 8));
  _LogEx('Section Headers Address          : 0x' + IntToHex(m_header.dwSectionHeadersAddr, 8));

  // Print init flags
  TmpStr := '';
  TmpStr := 'Init Flags                       : 0x' + IntToHex(Ord(m_Header.dwInitFlags[3]), 2) + IntToHex(Ord(m_Header.dwInitFlags[2]), 2) + IntToHex(Ord(m_Header.dwInitFlags[1]), 2) + IntToHex(Ord(m_Header.dwInitFlags[0]), 2) + ' '; //IntToHex ( m_Header.dwInitFlags, 8));
  Flag := Ord(m_Header.dwInitFlags[0]);

  if GetBitEn(Flag, 0) > 0 then
    TmpStr := TmpStr + '[Mount Utility Drive] ';

  if GetBitEn(Flag, 1) > 0 then
    TmpStr := TmpStr + '[Format Utility Drive] ';

  if GetBitEn(Flag, 2) > 0 then
    TmpStr := TmpStr + '[Limit Devkit Run Time Memory to 64MB] ';

  if GetBitEn(Flag, 2) > 0 then
    TmpStr := TmpStr + '[Setup Harddisk] ';

  _LogEx(TmpStr);

  lIndex := GetAddr(m_Header.dwDebugUnicodeFilenameAddr);
  lIndex2 := 0;
  TmpStr := '';
  while lIndex2 < SizeOf(AsciiFilename) do
  begin
    TmpStr := TmpStr + WideChar(Ord(Buffer[lIndex]));
    Inc(lIndex2);
    lIndex := lIndex + 2;
    if Ord(Buffer[lIndex]) = 0 then
      Break;
  end;

  //TmpStr := WideStringToString(AsciiFilename, 437);

  StrAsciiFilename := TmpStr;
  TmpStr := '';
  _LogEx('Entry Point                      : 0x' + IntToHex(m_Header.dwEntryAddr, 8) + ' (Retail: 0x' + IntToHex(m_Header.dwEntryAddr xor XOR_EP_Retail, 8) + ', Debug: 0x' + IntToHex(m_Header.dwEntryAddr xor XOR_EP_DEBUG, 8) + ')');
  _LogEx('TLS Address                      : 0x' + IntToHex(m_Header.dwTLSAddr, 8));
  _LogEx('(PE) Stack Commit                : 0x' + IntToHex(m_Header.dwPeStackCommit, 8));
  _LogEx('(PE) Heap Reserve                : 0x' + IntToHex(m_Header.dwPeHeapReserve, 8));
  _LogEx('(PE) Heap Commit                 : 0x' + IntToHex(m_Header.dwPeHeapCommit, 8));
  _LogEx('(PE) Base Address                : 0x' + IntToHex(m_Header.dwPeBaseAddr, 8));
  _LogEx('(PE) Size of Image               : 0x' + IntToHex(m_Header.dwPeSizeofImage, 8));
  _LogEx('(PE) Checksum                    : 0x' + IntToHex(m_Header.dwPeChecksum, 8));

  TmpStr := '';
  DateTimeToString(TmpStr, 'ddd mmm dd hh:mm:ss yyyy', CTimeToDateTime(m_Header.dwPeTimeDate));
  _LogEx('(PE) TimeDate Stamp              : 0x' + IntToHex(m_Header.dwPeTimeDate, 8) + ' (' + TmpStr + ')');

  lIndex := GetAddr(m_Header.dwDebugPathnameAddr);
  TmpStr := '';
  TmpChr := Buffer[lIndex];
  Inc(lIndex);
  while Ord(TmpChr) <> 0 do
  begin
    TmpStr := TmpStr + TmpChr;
    TmpChr := Buffer[lIndex];
    Inc(lIndex);
  end;
    
  _LogEx('Debug Pathname Address           : 0x' + IntToHex(m_Header.dwDebugPathnameAddr, 8) + ' ("' + TmpStr + '")');

  lIndex := GetAddr(m_Header.dwDebugFilenameAddr);
  TmpStr := '';
  TmpChr := Buffer[lIndex];
  Inc(lIndex);
  while Ord(TmpChr) <> 0 do
  begin
    TmpStr := TmpStr + TmpChr;
    TmpChr := Buffer[lIndex];
    Inc(lIndex);
  end;
    
  _LogEx('Debug Filename Address           : 0x' + IntToHex(m_Header.dwDebugFilenameAddr, 8) + ' ("' + TmpStr + '")');

  _LogEx('Debug Unicode filename Address   : 0x' + IntToHex(m_Header.dwDebugUnicodeFilenameAddr, 8) + ' (L"' + StrAsciiFilename + '")');
  _LogEx('Kernel Image Thunk Address       : 0x' + IntToHex(m_Header.dwKernelImageThunkAddr, 8) + ' (Retail: 0x' + IntToHex(m_Header.dwKernelImageThunkAddr xor XOR_KT_RETAIL, 8) + ', Debug: 0x' + IntToHex(m_Header.dwKernelImageThunkAddr xor XOR_KT_DEBUG, 8) + ')');
  _LogEx('NonKernel Import Dir Address     : 0x' + IntToHex(m_Header.dwNonKernelImportDirAddr, 8));
  _LogEx('Library Versions                 : 0x' + IntToHex(m_Header.dwLibraryVersions, 8));
  _LogEx('Library Versions Address         : 0x' + IntToHex(m_Header.dwLibraryVersionsAddr, 8));
  _LogEx('Kernel Library Version Address   : 0x' + IntToHex(m_Header.dwKernelLibraryVersionAddr, 8));
  _LogEx('XAPI Library Version Address     : 0x' + IntToHex(m_Header.dwXAPILibraryVersionAddr, 8));
  _LogEx('Logo Bitmap Address              : 0x' + IntToHex(m_Header.dwLogoBitmapAddr, 8));
  _LogEx('Logo Bitmap Size                 : 0x' + IntToHex(m_Header.dwSizeofLogoBitmap, 8));

  _LogEx('');
  _LogEx('Dumping XBE Certificate...');
  _LogEx('');

  _LogEx('Size of Certificate              : 0x' + IntToHex(m_Certificate.dwSize, 8));
  TmpStr := '';
  DateTimeToString(TmpStr, 'ddd mmm dd hh:mm:ss yyyy', CTimeToDateTime(m_Certificate.dwTimeDate));
  _LogEx('TimeDate Stamp                   : 0x' + IntToHex(m_Certificate.dwTimeDate, 8) + ' (' + TmpStr + ')');
  _LogEx('Title ID                         : 0x' + IntToHex(m_Certificate.dwTitleId, 8));
  _LogEx('Title                            : "' + m_szAsciiTitle + '"');

  // print alternate titles
  _LogEx('Alternate Titles IDs             : 0x' + IntToHex(m_Certificate.dwAlternateTitleId[0], 8));
  for lIndex := 1 to 15 do
    _LogEx('                                   0x' + IntToHex(m_Certificate.dwAlternateTitleId[lIndex], 8));

  _LogEx('Allowed Media                    : 0x' + IntToHex(m_Certificate.dwAllowedMedia, 8));
  _LogEx('Game Region                      : 0x' + IntToHex(m_Certificate.dwGameRegion, 8));
  _LogEx('Game Ratings                     : 0x' + IntToHex(m_Certificate.dwGameRatings, 8));
  _LogEx('Disk Number                      : 0x' + IntToHex(m_Certificate.dwDiskNumber, 8));
  _LogEx('Version                          : 0x' + IntToHex(m_Certificate.dwVersion, 8));

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
      if Ord(m_szSectionName[lIndex][lIndex2]) <> 0 then
        TmpStr := TmpStr + m_szSectionName[lIndex][lIndex2]
      else
        Break;
    end;

    _LogEx('Section Name                     : 0x' + IntToHex(Ord(m_SectionHeader[lIndex].dwSectionNameAddr), 8) + ' ("' + TmpStr + '")');

    TmpStr := '';
    TmpStr := 'Flags                            : 0x' + IntToHex(Ord(m_SectionHeader[lIndex].dwFlags[3]), 2) + IntToHex(Ord(m_SectionHeader[lIndex].dwFlags[2]), 2) + IntToHex(Ord(m_SectionHeader[lIndex].dwFlags[1]), 2) + IntToHex(Ord(m_SectionHeader[lIndex].dwFlags[0]), 2);

    Flag := Ord(m_SectionHeader[lIndex].dwFlags[0]);

    TmpStr := TmpStr + ' '; // Insert open space
    if GetBitEn(Flag, 0) > 0 then
      TmpStr := TmpStr + '(Writable) ';

    if GetBitEn(Flag, 1) > 0 then
      TmpStr := TmpStr + '(Preload) ';

    if GetBitEn(Flag, 2) > 0 then
      TmpStr := TmpStr + '(Executable) ';

    if GetBitEn(Flag, 3) > 0 then
      TmpStr := TmpStr + '(Inserted File) ';

    if GetBitEn(Flag, 4) > 0 then
      TmpStr := TmpStr + '(Head Page RO) ';

    if GetBitEn(Flag, 5) > 0 then
      TmpStr := TmpStr + '(Tail Page RO) ';

    _LogEx(TmpStr);

    _LogEx('Virtual Address                  : 0x' + IntToHex(m_SectionHeader[lIndex].dwVirtualAddr, 8));
    _LogEx('Virtual Size                     : 0x' + IntToHex(m_SectionHeader[lIndex].dwVirtualSize, 8));
    _LogEx('Raw Address                      : 0x' + IntToHex(m_SectionHeader[lIndex].dwRawAddr, 8));
    _LogEx('Size of Raw                      : 0x' + IntToHex(m_SectionHeader[lIndex].dwSizeofRaw, 8));
    _LogEx('Section Name Address             : 0x' + IntToHex(m_SectionHeader[lIndex].dwSectionNameAddr, 8));
    _LogEx('Section Reference Count          : 0x' + IntToHex(m_SectionHeader[lIndex].dwSectionRefCount, 8));
    _LogEx('Head Shared Reference Count Addr : 0x' + IntToHex(m_SectionHeader[lIndex].dwHeadSharedRefCountAddr, 8));
    _LogEx('Tail Shared Reference Count Addr : 0x' + IntToHex(m_SectionHeader[lIndex].dwTailSharedRefCountAddr, 8));
    TmpStr := '';

    for lIndex2 := 0 to 19 do
      TmpStr := TmpStr + IntToHex(Ord(m_SectionHeader[lIndex].bzSectionDigest[lIndex2]), 2);

    _LogEx('Section Digest                   : ' + TmpStr);
    _LogEx('');
  end;

  // print library versions
  _LogEx('Dumping XBE Library Versions...');
  if (SizeOf(m_LibraryVersion) = 0) or (m_Header.dwLibraryVersions = 0) then
    _LogEx('(This XBE contains no Library Versions)')
  else
  begin
    for lIndex := 0 to m_Header.dwLibraryVersions - 1 do
    begin
      TmpStr := '';
      for lIndex2 := 0 to 7 do
      begin
        if m_LibraryVersion[lIndex].szName[lIndex2] <> #0 then
          TmpStr := TmpStr + m_LibraryVersion[lIndex].szName[lIndex2];
      end;

      _LogEx('Library Name                     : ' + TmpStr);
      _LogEx('Version                          : ' + inttostr(m_LibraryVersion[lIndex].wMajorVersion) + '.' + inttostr(m_LibraryVersion[lIndex].wMinorVersion) + '.' + inttostr(m_LibraryVersion[lIndex].wBuildVersion));

      //Some bit maths the QVersion Flag is only 13 bits long so i convert the 13 bits to a number

      QVersion := 0;

      Flag := Ord(m_LibraryVersion[lIndex].dwFlags[0]);
      for bIndex := 0 to 7 do
        QVersion := QVersion + GetBitEn(Flag, bIndex) * Round(Power(2, bIndex));

      Flag := Ord(m_LibraryVersion[lIndex].dwFlags[1]);
      for bIndex := 0 to 4 do
        QVersion := QVersion + GetBitEn(Flag, BIndex) * Round(Power(2, bIndex + 8));

     //end of bits maths

      TmpStr := 'Flags                            : QFEVersion : 0x' + IntToHex(QVersion, 4) + ', ';

      if GetBitEn(Flag, 7) > 0 then
        TmpStr := TmpStr + 'Debug, '
      else
        TmpStr := TmpStr + 'Retail, ';

      if (GetBitEn(Flag, 5) * 1 + GetBitEn(Flag, 6) * 2) = 0 then
        TmpStr := TmpStr + 'Unapproved'
      else
        if (GetBitEn(Flag, 5) * 1 + GetBitEn(Flag, 6) * 2) = 1 then
          TmpStr := TmpStr + 'Possibly Approved'
        else
          TmpStr := TmpStr + 'Approved';

      _LogEx(TmpStr);
    end; // for lIndex
  end;

  _LogEx('Dumping XBE TLS...');
  _LogEx('Data Start Address               : 0x' + IntToHex(m_TLS.dwDataStartAddr, 8));
  _LogEx('Data End Address                 : 0x' + IntToHex(m_TLS.dwDataEndAddr, 8));
  _LogEx('TLS Index Address                : 0x' + IntToHex(m_TLS.dwTLSIndexAddr, 8));
  _LogEx('TLS Callback Address             : 0x' + IntToHex(m_TLS.dwTLSCallbackAddr, 8));
  _LogEx('Size of Zero Fill                : 0x' + IntToHex(m_TLS.dwSizeofZeroFill, 8));
  _LogEx('Characteristics                  : 0x' + IntToHex(m_TLS.dwCharacteristics, 8));

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
      Result := dwOffs //- SizeOf(m_Header)
    else
    begin
      for lIndex := 0 to m_Header.dwSections - 1 do
      begin
        VirtAddr := m_SectionHeader[lIndex].dwVirtualAddr;
        VirtSize := m_SectionHeader[lIndex].dwVirtualSize;
        if (x_dwVirtualAddress >= VirtAddr) and (x_dwVirtualAddress < (VirtAddr + VirtSize)) then
          Result := m_SectionHeader[lIndex].dwRawAddr + (x_dwVirtualAddress - VirtAddr);
      end;
    end;
  end;
end; // TXbe.GetAddr

//------------------------------------------------------------------------------

procedure TXbe.ExportLogoBitmap(ImgCont: TBitmap);
var
  x_Gray: array[0..100 * 17] of Char;
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

    if (Pos0 and 1) > 0 then                              // Check if the bit 0 is set.
    begin
      Len := Pos0 shr 1 and 7;                            // Select the bits from 1 to 3
      Data := Pos0 shr 4 and 15;                          // Select the bits from 4 to 7
    end
    else
    begin
      if (Pos0 and 2) > 0 then                             // Check if the bit 1 is set.
      begin
        Len := (Pos0 shr 2 and 63) + (Pos1 and 15)*256;   // Select the bits from 2 to 7 from the first byte (Pos0) and the bits from 0 to 3 from the second byte (Pos1) and form a number.
        Data := Pos1 shr 4 and 15;                        // Select the bits from 4 to 7 from the second byte (Pos1)
        Inc(lIndex);                                      // The index is incremented because 2 bytes were read.
      end;
    end;

    for lIndex2 := 0 to Len - 1 do
    begin
      Inc(o);
      if o >= 100 * 17 then
        Exit;

      x_Gray[o] := Chr(Data shl 4);
      ImgCont.Canvas.Pixels[o mod 100, o div 100] := RGB(Ord(x_Gray[o]), Ord(x_Gray[o]), Ord(x_Gray[o]));
    end;

    Inc(lIndex);                                          // Index increment
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
  FreeMem(Buffer);

  inherited Destroy;
end;

//------------------------------------------------------------------------------

end.
