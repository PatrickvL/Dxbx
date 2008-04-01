unit uXbe;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, Math, ActnList, ExtCtrls,
  // DXBX
  uConsts, uExe, BitsOps, CTime;

type
  LogType = (ltLog, ltFile);

  _XBE_HEADER = packed record
    dwMagic: array[0..3] of char; // 0x0000 - magic number [should be "XBEH"]
    pbDigitalSignature: array[0..255] of char; // 0x0004 - digital signature
    dwBaseAddr: DWord; // 0x0104 - base address
    dwSizeofHeaders: DWord; // 0x0108 - size of headers
    dwSizeofImage: DWord; // 0x010C - size of image
    dwSizeofImageHeader: DWord; // 0x0110 - size of image header
    dwTimeDate: DWord; // 0x0114 - timedate stamp
    dwCertificateAddr: DWord; // 0x0118 - certificate address
    dwSections: DWord; // 0x011C - number of sections
    dwSectionHeadersAddr: DWord; // 0x0120 - section headers address

    dwInitFlags: array[0..3] of char; // 0x0124 - initialization flags

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

    dwFlags: array[0..3] of char;
    dwVirtualAddr: DWord; // virtual address
    dwVirtualSize: DWord; // virtual size
    dwRawAddr: DWord; // file offset to raw data
    dwSizeofRaw: DWord; // size of raw data
    dwSectionNameAddr: DWord; // section name addr
    dwSectionRefCount: DWord; // section reference count
    dwHeadSharedRefCountAddr: DWord; // head shared page reference count address
    dwTailSharedRefCountAddr: DWord; // tail shared page reference count address
    bzSectionDigest: array[0..19] of CHAR; // section digest
  end;
  XBE_SECTIONHEADER = _XBE_SECTIONHEADER;


  _XBE_LIBRARYVERSION = packed record
    szName: array[0..7] of Char; // library name
    wMajorVersion: Word; // major version
    wMinorVersion: Word; // minor version
    wBuildVersion: Word; // build version
    dwFlags: array[0..1] of char;
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

  _Eight = char;
    //Bit 0  : bType1
    //Bit 1,2,3  : Len
    //Bit 4,5,6,7 : Data
  Eight = _Eight;

  _Sixteen = array[0..1] of char;
    //from char[0]
    //Bit 0  : bType1
    //Bit 1  : bType2
    //Bit 2,3,4,5,6,7 :len
    //from char[2]
    //Bit 0,1,2,3  : Len
    //Bit 4,5,6,7 : Data
  Sixteen = _Sixteen;

  // used to encode / decode logo bitmap data
  _LogoRLE = packed record
    m_Eight: Eight;
    m_Sixteen: Sixteen;
  end;
  LogoRLE = _LogoRLE;

  TXbe = class
  private
    Buffer: PChar;
    m_KernelLibraryVersion: array of Char;
    m_XAPILibraryVersion: array of Char;
    m_Certificate: XBE_CERTIFICATE;
    procedure ConstructorInit;
  public
    m_szPath: string;
    m_bzSection: array of TVarCharArray;
    XbeFile: file of char;
    m_LogoRLE: LogoRLE;
    m_Header: XBE_HEADER;
    m_HeaderEx: array of Char;
    m_SectionHeader: array of XBE_SECTIONHEADER;
    m_szSectionName: array of array of Char;
    m_LibraryVersion: array of XBE_LIBRARYVERSION;
    m_TLS: XBE_TLS;

    constructor Create(aFileName: string);
    destructor Destroy; override;


    function DumpInformation(FileName: string = ''): Boolean;
    function GetAddr(x_dwVirtualAddress: DWord): integer;
    procedure ExportLogoBitmap(ImgCont: TBitmap);

    function GetTLSData: DWord;

  protected
  end;

function GetDWordVal(arrPchar: PChar; i: integer): DWord;
function GetWordVal(arrPchar: PChar; i: integer): Word;
function RoundUp(dwValue, dwMult: DWord): DWord;

var
  m_szAsciiTitle: string;

implementation

uses
  uLog;

//------------------------------------------------------------------------------

function RoundUp(dwValue, dwMult: DWord): DWord;
begin
  if dwMult = 0 then begin
    result := dwValue
  end
  else begin
    result := dwValue - ((dwValue - 1) mod dwMult) + (dwMult - 1);
  end;
end; // RoundUp

//------------------------------------------------------------------------------

function GetDWordVal(arrPchar: PChar; i: integer): DWord;
begin
  result := GetBitEn(ORD(arrPchar[i]), 0) * 1 + GetBitEn(ORD(arrPchar[i]), 1) * 2 + GetBitEn(ORD(arrPchar[i]), 2) * 4 +
    GetBitEn(ORD(arrPchar[i]), 3) * 8 + GetBitEn(ORD(arrPchar[i]), 4) * 16 + GetBitEn(ORD(arrPchar[i]), 5) * 32 +
    GetBitEn(ORD(arrPchar[i]), 6) * 64 + GetBitEn(ORD(arrPchar[i]), 7) * 128 +
    GetBitEn(ORD(arrPchar[i + 1]), 0) * 256 + GetBitEn(ORD(arrPchar[i + 1]), 1) * 512 + GetBitEn(ORD(arrPchar[i + 1]), 2) * 1024 +
    GetBitEn(ORD(arrPchar[i + 1]), 3) * 2048 + GetBitEn(ORD(arrPchar[i + 1]), 4) * 4096 + GetBitEn(ORD(arrPchar[i + 1]), 5) * 8192 +
    GetBitEn(ORD(arrPchar[i + 1]), 6) * 16384 + GetBitEn(ORD(arrPchar[i + 1]), 7) * 32768 +
    GetBitEn(ORD(arrPchar[i + 2]), 0) * 65536 + GetBitEn(ORD(arrPchar[i + 2]), 1) * ROUND(POWER(2, 17)) + GetBitEn(ORD(arrPchar[i + 2]), 2) * ROUND(POWER(2, 18)) +
    GetBitEn(ORD(arrPchar[i + 2]), 3) * ROUND(POWER(2, 19)) + GetBitEn(ORD(arrPchar[i + 2]), 4) * ROUND(POWER(2, 20)) + GetBitEn(ORD(arrPchar[i + 2]), 5) * ROUND(POWER(2, 21)) +
    GetBitEn(ORD(arrPchar[i + 2]), 6) * ROUND(POWER(2, 22)) + GetBitEn(ORD(arrPchar[i + 2]), 7) * ROUND(POWER(2, 23)) +
    GetBitEn(ORD(arrPchar[i + 3]), 0) * ROUND(POWER(2, 24)) + GetBitEn(ORD(arrPchar[i + 3]), 1) * ROUND(POWER(2, 25)) + GetBitEn(ORD(arrPchar[i + 3]), 2) * ROUND(POWER(2, 26)) +
    GetBitEn(ORD(arrPchar[i + 3]), 3) * ROUND(POWER(2, 27)) + GetBitEn(ORD(arrPchar[i + 3]), 4) * ROUND(POWER(2, 28)) + GetBitEn(ORD(arrPchar[i + 3]), 5) * ROUND(POWER(2, 29)) +
    GetBitEn(ORD(arrPchar[i + 3]), 6) * ROUND(POWER(2, 30)) + GetBitEn(ORD(arrPchar[i + 3]), 7) * ROUND(POWER(2, 31));
end; // GetDwordVal

//------------------------------------------------------------------------------

function GetWordVal(arrPchar: PChar; i: integer): Word;
begin
  result := GetBitEn(ORD(arrPchar[i]), 0) * 1 + GetBitEn(ORD(arrPchar[i]), 1) * 2 + GetBitEn(ORD(arrPchar[i]), 2) * 4 +
    GetBitEn(ORD(arrPchar[i]), 3) * 8 + GetBitEn(ORD(arrPchar[i]), 4) * 16 + GetBitEn(ORD(arrPchar[i]), 5) * 32 +
    GetBitEn(ORD(arrPchar[i]), 6) * 64 + GetBitEn(ORD(arrPchar[i]), 7) * 128 +
    GetBitEn(ORD(arrPchar[i + 1]), 0) * 256 + GetBitEn(ORD(arrPchar[i + 1]), 1) * 512 + GetBitEn(ORD(arrPchar[i + 1]), 2) * 1024 +
    GetBitEn(ORD(arrPchar[i + 1]), 3) * 2048 + GetBitEn(ORD(arrPchar[i + 1]), 4) * 4096 + GetBitEn(ORD(arrPchar[i + 1]), 5) * 8192 +
    GetBitEn(ORD(arrPchar[i + 1]), 6) * 16384 + GetBitEn(ORD(arrPchar[i + 1]), 7) * 32768;
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

constructor TXbe.Create(aFileName: string);
var
  ExeSize: LongInt;
  lIndex, lIndex2: DWord;
  RawSize, RawAddr: DWord;
  I: DWord;
  F: THandle;
  ReadBytes, FileSz: DWord;
begin
  ConstructorInit();

  RawSize := 0;
  F := FileOpen(aFileName, fmOpenRead);
  FileSz := GetFileSize(f, nil);
  FileClose(F);

  // verify xbe file was opened
  if FileSz = 0 then begin
    MessageDlg('Could not open Xbe file.', mtError, [mbOk], 0);
    Exit;
  end;
  WriteLog('DXBX: Opening Xbe file...OK');

  // remember xbe path
  m_szPath := ExtractFilePath(aFileName);
  WriteLog('DXBX: Storing Xbe Path...Ok');

  // read xbe image header
  if SizeOf(m_Header) > FileSz then begin
    MessageDlg('Unexpected end of file while reading Xbe Image Header', mtError, [mbOk], 0);
    Exit;
  end;
  AssignFile(XbeFile, aFileName);
  FileMode := fmOpenRead;
  Reset(XbeFile);
  freemem(buffer);
  getmem(buffer, FileSz);
  BlockRead(XBeFile, Buffer^, FileSz, ReadBytes);
  // m_Header.dwMagic Read (4 Bytes)
  i := 0;
  for lIndex := 0 to 3 do begin
    m_Header.dwMagic[lIndex] := Buffer[i];
    inc(i);
  end;

  if m_Header.dwMagic <> _MagicNumber then begin
    MessageDlg('Invalid magic number in Xbe file', mtError, [mbOk], 0);
    Exit;
  end;

  // m_Header.pbDigitalSignature Read (256 Bytes)
  for lIndex := 4 to 259 do begin
    m_Header.pbDigitalSignature[i - 4] := Buffer[i];
    inc(i);
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
  for lIndex := 0 to 3 do begin
    m_Header.dwInitFlags[lIndex] := Buffer[lIndex + i];
  end;
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
  if (m_Header.dwSizeofHeaders > SizeOf(m_Header)) then begin
    WriteLog('DXBX: Reading Image Header Extra Bytes... NOT DONE YET');
  end;

  ExeSize := RoundUp(m_Header.dwSizeofHeaders, $1000) - sizeof(m_Header);

  if SizeOf(m_HeaderEx) > FileSz then begin
    MessageDlg('Unexpected end of file while reading Xbe Image Header (Ex)', mtError, [mbOk], 0);
    Exit;
  end;

  try
    SetLength(m_HeaderEx, ExeSize);
    for lIndex := 0 to ExeSize - 1 do begin
      m_HeaderEx[lIndex] := Buffer[i];
      inc(i);
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
  for lIndex := 0 to 39 do begin
    m_Certificate.wszTitleName[lIndex] := WideChar(ord(Buffer[i]));
    i := i + 2;
  end;

  m_szAsciiTitle := WideCharToString(m_Certificate.wszTitleName);

  //m_Certificate.dwAlternateTitleId (4 bytes each element of the array)
  for lIndex := 0 to 15 do begin
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
  //m_Certificate.bzLanKey 1 char
  for lIndex := 0 to 15 do begin
    m_Certificate.bzLanKey[lIndex] := Buffer[i + lIndex];
  end;
  i := i + 16;
  //m_Certificate.bzLanKey 1 char
  for lIndex := 0 to 15 do begin
    m_Certificate.bzSignatureKey[lIndex] := Buffer[i + lIndex];
  end;
  i := i + 16;
  //m_Certificate.bzLanKey array of array 16x16 char
  for lIndex := 0 to 15 do begin
    for lIndex2 := 0 to 15 do begin
      m_Certificate.bzTitleAlternateSignatureKey[lIndex][lIndex2] := Buffer[i + lIndex2];
    end;
    i := i + 16;
  end;

  WriteLog('DXBX: Reading Certificate...OK');
  WriteLog('DXBX: Title identified as ' + m_szAsciiTitle);

  // read xbe section headers
  i := m_Header.dwSectionHeadersAddr - m_Header.dwBaseAddr;

  SetLength(m_SectionHeader, m_Header.dwSections);

  for lIndex := 0 to m_Header.dwSections - 1 do begin
    try
      //XbeFile.Read(m_SectionHeader[lIndex], SizeOf(XBE_SECTIONHEADER));
        //m_SectionHeader[].dwFlags 4 bytes
      for lIndex2 := 0 to 3 do begin
        m_SectionHeader[lIndex].dwFlags[lIndex2] := Buffer[i + lIndex2];
      end;
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
      for lIndex2 := 0 to 19 do begin
        m_SectionHeader[lIndex].bzSectionDigest[lIndex2] := Buffer[i + lIndex2];
      end;
      i := i + 20;
    except
      MessageDlg('Unexpected end of file while reading Xbe Section Header', mtError, [mbOk], 0);
    end;
    WriteLog('DXBX: Reading Section Header 0x%.04X...' + IntToStr(lIndex) + ' OK');
  end;

  // read xbe section names
  SetLength(m_szSectionName, m_Header.dwSections, 9);
  for lIndex := 0 to m_Header.dwSections - 1 do begin
    RawAddr := GetAddr(m_SectionHeader[lIndex].dwSectionNameAddr);
    if m_SectionHeader[lIndex].dwSectionNameAddr <> 0 then begin
      for lIndex2 := 0 to 8 do begin
        m_szSectionName[lIndex][lIndex2] := buffer[RawAddr + lIndex2];
        if ord(m_szSectionName[lIndex][lIndex2]) = 0 then begin
          break;
        end;
      end;
    end;
  end;

  // read xbe library versions
  if m_Header.dwLibraryVersionsAddr <> 0 then begin
    WriteLog('DXBX: Reading Library Versions...');

    i := m_Header.dwLibraryVersionsAddr - m_Header.dwBaseAddr;

    SetLength(m_LibraryVersion, m_Header.dwLibraryVersions);

    for lIndex := 0 to m_Header.dwLibraryVersions - 1 do begin
      WriteLog('DXBX: Reading Library Version 0x' + inttohex(lIndex, 4) + '....');
      for lIndex2 := 0 to 7 do begin
        m_LibraryVersion[lIndex].szName[lIndex2] := Buffer[i + lIndex2];
      end;
      i := i + 8;
      m_LibraryVersion[lIndex].wMajorVersion := GetWordVal(Buffer, i);
      i := i + 2;
      m_LibraryVersion[lIndex].wMinorVersion := GetWordVal(Buffer, i);
      i := i + 2;
      m_LibraryVersion[lIndex].wBuildVersion := GetWordVal(Buffer, i);
      i := i + 2;
      for lIndex2 := 0 to 1 do begin
        m_LibraryVersion[lIndex].dwFlags[lIndex2] := Buffer[i + lIndex2];
      end;
      i := i + 2;
    end;

    // read xbe kernel library version
    WriteLog('DXBX: Reading Kernel Library Version...');
    if m_Header.dwKernelLibraryVersionAddr = 0 then
      MessageDlg('Could not locate kernel library version', mtError, [mbOk], 0);

    i := m_Header.dwKernelLibraryVersionAddr - m_Header.dwBaseAddr;

    SetLength(m_KernelLibraryVersion, sizeof(m_LibraryVersion));
    for lIndex := 0 to sizeof(m_LibraryVersion) - 1 do begin
      m_KernelLibraryVersion[lIndex] := Buffer[lIndex + i]
    end;

    // read xbe xapi library version
    WriteLog('DXBX: Reading Xapi Library Version...');
    if m_Header.dwXAPILibraryVersionAddr = 0 then
      MessageDlg('Could not locate Xapi Library Version', mtError, [mbOk], 0);

    i := m_Header.dwXAPILibraryVersionAddr - m_Header.dwBaseAddr;
    SetLength(m_XAPILibraryVersion, sizeof(m_LibraryVersion));
    for lIndex := 0 to sizeof(m_LibraryVersion) - 1 do begin
      m_XAPILibraryVersion[lIndex] := Buffer[lIndex + i]
    end;

    WriteLog('DXBX: Reading Sections...');


    SetLength(m_bzSection, m_Header.dwSections);

    for lIndex := 0 to m_Header.dwSections - 1 do begin
      WriteLog('DXBX: Reading Section 0x' + inttohex(lIndex, 4) + '...');

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
      setlength(m_bzSection[lIndex], RawSize);
      if RawSize = 0 then begin
        Break;
      end;
      WriteLog('Ok');

      for lIndex2 := 0 to Rawsize - 1 do begin
        m_bzSection[lIndex][lIndex2] := Buffer[RawAddr + lIndex2];
      end;

    end;

    if lIndex2 < Rawsize then
      WriteLog('Unexpected end of file while reading Xbe Section');
  end;

  if m_Header.dwTLSAddr <> 0 then begin
    WriteLog('DXBX: Reading Thread Local Storage...');
    if GetAddr(m_Header.dwTLSAddr) <> 0 then begin
      i := GetAddr(m_Header.dwTLSAddr);
      m_TLS.dwDataStartAddr := GetDwordVal(buffer, i);
      i := i + 4;
      m_TLS.dwDataEndAddr := GetDwordVal(buffer, i);
      i := i + 4;
      m_TLS.dwTLSIndexAddr := GetDwordVal(buffer, i);
      i := i + 4;
      m_TLS.dwTLSCallbackAddr := GetDwordVal(buffer, i);

      i := i + 4;
      m_TLS.dwSizeofZeroFill := GetDwordVal(buffer, i);

      i := i + 4;
      m_TLS.dwCharacteristics := GetDwordVal(buffer, i);
    end;
  end;
  closefile(XBeFile);
end; // TXbe.Create

//------------------------------------------------------------------------------

function TXbe.DumpInformation(FileName: string): Boolean;
var
  FileEx: TextFile;
  lIndex, lIndex2: Integer;
  tmpStr: string;
  AsciiFilename: array[0..39] of Char;
  tmpchr: char;
  strAsciiFilename: string;
  Flag, BIndex: Byte;
  QVersion: Word;
  lType: LogType;

  DumpToFile: Boolean;

  procedure LogEx(Text: string; lType: LogType);
  begin
    if lType = ltLog then begin
      WriteLog(Text);
    end
    else
    begin
      Writeln(FileEx, Text);
    end;
  end;
begin
  DumpToFile := FileName <> '';

  try
    if DumpToFile then begin
      AssignFile(FileEx, FileName);
      Rewrite(FileEx);
      LType := ltFile;
    end
    else begin
      lType := ltLog;
    end;

    LogEx('XBE information generated by DXBX (Version ' + _DXBX_VERSION + ')', lType);
    LogEx('', lType);

    LogEx('Title identified as "' + m_szAsciiTitle + '"', lType);
    LogEx('', lType);

    LogEx('Dumping XBE file header...', lType);
    LogEx('', lType);

    LogEx('Magic Number                     : XBEH', lType);

    LogEx('Digitial Signature               : <Hex Dump>', lType);
    tmpStr := '';
    lIndex2 := 0;
    for lIndex := 0 to 255 do begin
      tmpStr := tmpStr + intToHex(ord(m_Header.pbDigitalSignature[lIndex]), 2);

      if lIndex2 = 15 then begin
        LogEx('                                   ' + TmpStr, lType);
        TmpStr := '';
        lIndex2 := -1;
      end;

      Inc(lIndex2);
    end;
    LogEx('                                   </Hex Dump>', lType);

    LogEx('Base Address                     : 0x' + IntToHex(m_Header.dwBaseAddr, 8), lType);
    LogEx('Size of Headers                  : 0x' + IntToHex(m_Header.dwSizeofHeaders, 8), lType);
    LogEx('Size of Image                    : 0x' + IntToHex(m_Header.dwSizeofImage, 8), lType);
    LogEx('Size of Image Header             : 0x' + IntToHex(m_Header.dwSizeofImageHeader, 8), lType);
    tmpStr := '';
    DateTimeToString(tmpStr, 'ddd mmm dd hh:mm:ss yyyy', CTimeToDateTime(m_Header.dwTimeDate));
    LogEx('TimeDate Stamp                   : 0x' + IntToHex(m_Header.dwTimeDate, 8) + ' (' + tmpstr + ')', lType);
    LogEx('Certificate Address              : 0x' + IntToHex(m_Header.dwCertificateAddr, 8), lType);
    LogEx('Number of Sections               : 0x' + IntToHEx(m_Header.dwSections, 8), lType);
    LogEx('Section Headers Address          : 0x' + IntToHex(m_header.dwSectionHeadersAddr, 8), lType);


    // print init flags
    TmpStr := '';
    TmpStr := 'Init Flags                       : 0x' + IntToHex(ord(m_Header.dwInitFlags[3]), 2) + IntToHex(ord(m_Header.dwInitFlags[2]), 2) + IntToHex(ord(m_Header.dwInitFlags[1]), 2) + IntToHex(ord(m_Header.dwInitFlags[0]), 2) + ' '; //IntToHex ( m_Header.dwInitFlags, 8));
    Flag := ord(m_Header.dwInitFlags[0]);

    if GetBitEn(Flag, 0) = 1 then begin
      TmpStr := TmpStr + '[Mount Utility Drive] ';
    end;

    if GetBitEn(Flag, 1) = 1 then begin
      TmpStr := TmpStr + '[Format Utility Drive] ';
    end;

    if GetBitEn(Flag, 2) = 1 then begin
      TmpStr := TmpStr + '[Limit Devkit Run Time Memory to 64MB] ';
    end;

    if GetBitEn(Flag, 2) = 1 then begin
      TmpStr := TmpStr + '[Setup Harddisk] ';
    end;

    LogEx(TmpStr, lType);

    lIndex := GetAddr(m_Header.dwDebugUnicodeFilenameAddr);
    lIndex2 := 0;
    tmpstr := '';
    while lIndex2 < sizeof(AsciiFilename) do begin
      tmpstr := tmpstr + WideChar(ord(Buffer[lIndex]));
      inc(lIndex2);
      lIndex := lIndex + 2;
      if ord(Buffer[lIndex]) = 0 then begin
        break;
      end;
    end;

    //tmpstr := WideStringToString(AsciiFilename, 437);

    strAsciiFilename := tmpStr;
    tmpstr := '';
    LogEx('Entry Point                      : 0x' + IntToHex(m_Header.dwEntryAddr, 8) + ' (Retail: 0x' + IntToHex(m_Header.dwEntryAddr xor XOR_EP_Retail, 8) + ', Debug: 0x' + IntToHex(m_Header.dwEntryAddr xor XOR_EP_DEBUG, 8) + ')', lType);
    LogEx('TLS Address                      : 0x' + IntToHex(m_Header.dwTLSAddr, 8), lType);
    LogEx('(PE) Stack Commit                : 0x' + IntToHex(m_Header.dwPeStackCommit, 8), lType);
    LogEx('(PE) Heap Reserve                : 0x' + IntToHex(m_Header.dwPeHeapReserve, 8), lType);
    LogEx('(PE) Heap Commit                 : 0x' + IntToHex(m_Header.dwPeHeapCommit, 8), lType);
    LogEx('(PE) Base Address                : 0x' + IntToHex(m_Header.dwPeBaseAddr, 8), lType);
    LogEx('(PE) Size of Image               : 0x' + IntToHex(m_Header.dwPeSizeofImage, 8), lType);
    LogEx('(PE) Checksum                    : 0x' + IntToHex(m_Header.dwPeChecksum, 8), lType);

    tmpStr := '';
    DateTimeToString(tmpStr, 'ddd mmm dd hh:mm:ss yyyy', CTimeToDateTime(m_Header.dwPeTimeDate));
    LogEx('(PE) TimeDate Stamp              : 0x' + IntToHex(m_Header.dwPeTimeDate, 8) + ' (' + tmpstr + ')', lType);

    lIndex := GetAddr(m_Header.dwDebugPathnameAddr);
    tmpStr := '';
    tmpchr := Buffer[lIndex];
    inc(lIndex);
    while ord(tmpchr) <> 0 do begin
      TmpStr := tmpStr + tmpchr;
      tmpchr := Buffer[lIndex];
      inc(lIndex);
    end;
    LogEx('Debug Pathname Address           : 0x' + IntToHex(m_Header.dwDebugPathnameAddr, 8) + ' ("' + TmpStr + '")', lType);

    lIndex := GetAddr(m_Header.dwDebugFilenameAddr);
    tmpStr := '';
    tmpchr := Buffer[lIndex];
    inc(lIndex);
    while ord(tmpchr) <> 0 do begin
      TmpStr := tmpStr + tmpchr;
      tmpchr := Buffer[lIndex];
      inc(lIndex);
    end;
    LogEx('Debug Filename Address           : 0x' + IntToHex(m_Header.dwDebugFilenameAddr, 8) + ' ("' + tmpstr + '")', lType);

    LogEx('Debug Unicode filename Address   : 0x' + IntToHex(m_Header.dwDebugUnicodeFilenameAddr, 8) + ' (L"' + strAsciiFilename + '")', lType);
    LogEx('Kernel Image Thunk Address       : 0x' + IntToHex(m_Header.dwKernelImageThunkAddr, 8) + ' (Retail: 0x' + IntToHex(m_Header.dwKernelImageThunkAddr xor XOR_KT_RETAIL, 8) + ', Debug: 0x' + IntToHex(m_Header.dwKernelImageThunkAddr xor XOR_KT_DEBUG, 8) + ')', lType);
    LogEx('NonKernel Import Dir Address     : 0x' + IntToHex(m_Header.dwNonKernelImportDirAddr, 8), lType);
    LogEx('Library Versions                 : 0x' + IntToHex(m_Header.dwLibraryVersions, 8), lType);
    LogEx('Library Versions Address         : 0x' + IntToHex(m_Header.dwLibraryVersionsAddr, 8), lType);
    LogEx('Kernel Library Version Address   : 0x' + IntToHex(m_Header.dwKernelLibraryVersionAddr, 8), lType);
    LogEx('XAPI Library Version Address     : 0x' + IntToHex(m_Header.dwXAPILibraryVersionAddr, 8), lType);
    LogEx('Logo Bitmap Address              : 0x' + IntToHex(m_Header.dwLogoBitmapAddr, 8), lType);
    LogEx('Logo Bitmap Size                 : 0x' + IntToHex(m_Header.dwSizeofLogoBitmap, 8), lType);

    LogEx('', lType);
    LogEx('Dumping XBE Certificate...', lType);
    LogEx('', lType);

    LogEx('Size of Certificate              : 0x' + IntToHex(m_Certificate.dwSize, 8), lType);
    tmpStr := '';
    DateTimeToString(tmpStr, 'ddd mmm dd hh:mm:ss yyyy', CTimeToDateTime(m_Certificate.dwTimeDate));
    LogEx('TimeDate Stamp                   : 0x' + IntToHex(m_Certificate.dwTimeDate, 8) + ' (' + TmpStr + ')', lType);
    LogEx('Title ID                         : 0x' + IntToHex(m_Certificate.dwTitleId, 8), lType);
    LogEx('Title                            : "' + m_szAsciiTitle + '"', lType);

    // print alternate titles
    LogEx('Alternate Titles IDs             : 0x' + IntToHex(m_Certificate.dwAlternateTitleId[0], 8), lType);
    for lIndex := 1 to 15 do begin
      LogEx('                                   0x' + IntToHex(m_Certificate.dwAlternateTitleId[lIndex], 8), lType);
    end;

    LogEx('Allowed Media                    : 0x' + IntToHex(m_Certificate.dwAllowedMedia, 8), lType);
    LogEx('Game Region                      : 0x' + IntToHex(m_Certificate.dwGameRegion, 8), lType);
    LogEx('Game Ratings                     : 0x' + IntToHex(m_Certificate.dwGameRatings, 8), lType);
    LogEx('Disk Number                      : 0x' + IntToHex(m_Certificate.dwDiskNumber, 8), lType);
    LogEx('Version                          : 0x' + IntToHex(m_Certificate.dwVersion, 8), lType);

    // Print Lan Key
    tmpStr := '';
    for lIndex := 0 to 15 do begin
      tmpStr := tmpStr + intToHex(ord(m_Certificate.bzLanKey[lIndex]), 2);
    end;
    LogEx('LAN Key                          : ' + tmpStr, lType);

    // print signature key
    tmpStr := '';
    for lIndex := 0 to 15 do begin
      tmpStr := tmpStr + intToHex(ord(m_Certificate.bzSignatureKey[lIndex]), 2);
    end;
    LogEx('Signature Key                    : ' + tmpStr, lType);

    // print alternative signature keys
    LogEx('Title Alternative Signature Keys : <Hex Dump>', lType);

    for lIndex := 0 to 15 do begin
      tmpStr := '';
      for lIndex2 := 0 to 15 do begin
        tmpStr := tmpStr + intToHex(ord(m_Certificate.bzTitleAlternateSignatureKey[lIndex][lIndex2]), 2);
      end;
      LogEx('                                   ' + tmpStr, lType);
    end;
    LogEx('                                   </Hex Dump>', lType);

    // print section headers
    LogEx('', lType);
    LogEx('Dumping XBE Section Headers...', lType);
    LogEx('', lType);
    for lIndex := 0 to m_Header.dwSections - 1 do begin

      TmpStr := '';
      for lIndex2 := 0 to 8 do begin
        if ord(m_szSectionName[lIndex][lIndex2]) <> 0 then begin
          TmpStr := TmpStr + m_szSectionName[lIndex][lIndex2]
        end
        else begin
          Break;
        end;
      end;

      LogEx('Section Name                     : 0x' + IntToHex(ORD(m_SectionHeader[lIndex].dwSectionNameAddr), 8) + ' ("' + TmpStr + '")', lType);

      TmpStr := '';
      TmpStr := 'Flags                            : 0x' + inttohex(ord(m_SectionHeader[lIndex].dwFlags[3]), 2) + inttohex(ord(m_SectionHeader[lIndex].dwFlags[2]), 2) + inttohex(ord(m_SectionHeader[lIndex].dwFlags[1]), 2) + inttohex(ord(m_SectionHeader[lIndex].dwFlags[0]), 2);

      Flag := Ord(m_SectionHeader[lIndex].dwFlags[0]);

      TmpStr := TmpStr + ' '; // Insert open space
      if GetBitEn(Flag, 0) = 1 then begin
        TmpStr := TmpStr + '(Writable) ';
      end;

      if GetBitEn(Flag, 1) = 1 then begin
        TmpStr := TmpStr + '(Preload) ';
      end;

      if GetBitEn(Flag, 2) = 1 then begin
        TmpStr := TmpStr + '(Executable) ';
      end;

      if GetBitEn(Flag, 3) = 1 then begin
        TmpStr := TmpStr + '(Inserted File) ';
      end;

      if GetBitEn(Flag, 4) = 1 then begin
        TmpStr := TmpStr + '(Head Page RO) ';
      end;

      if GetBitEn(Flag, 5) = 1 then begin
        TmpStr := TmpStr + '(Tail Page RO) ';
      end;

      LogEx(TmpStr, lType);

      LogEx('Virtual Address                  : 0x' + IntToHex(m_SectionHeader[lIndex].dwVirtualAddr, 8), lType);
      LogEx('Virtual Size                     : 0x' + IntToHex(m_SectionHeader[lIndex].dwVirtualSize, 8), lType);
      LogEx('Raw Address                      : 0x' + IntToHex(m_SectionHeader[lIndex].dwRawAddr, 8), lType);
      LogEx('Size of Raw                      : 0x' + IntToHex(m_SectionHeader[lIndex].dwSizeofRaw, 8), lType);
      LogEx('Section Name Address             : 0x' + IntToHex(m_SectionHeader[lIndex].dwSectionNameAddr, 8), lType);
      LogEx('Section Reference Count          : 0x' + IntToHex(m_SectionHeader[lIndex].dwSectionRefCount, 8), lType);
      LogEx('Head Shared Reference Count Addr : 0x' + IntToHex(m_SectionHeader[lIndex].dwHeadSharedRefCountAddr, 8), lType);
      LogEx('Tail Shared Reference Count Addr : 0x' + IntToHex(m_SectionHeader[lIndex].dwTailSharedRefCountAddr, 8), lType);
      TmpStr := '';

      for lIndex2 := 0 to 19 do begin
        TmpStr := TmpStr + IntToHex(ord(m_SectionHeader[lIndex].bzSectionDigest[lIndex2]), 2);
      end;

      LogEx('Section Digest                   : ' + TmpStr, lType);
      LogEx('', lType);
    end;
    // print library versions
    LogEx('Dumping XBE Library Versions...', lType);
    if (sizeof(m_LibraryVersion) = 0) or (m_Header.dwLibraryVersions = 0) then begin
      LogEx('(This XBE contains no Library Versions)', lType);
    end
    else
    begin
      for lIndex := 0 to m_Header.dwLibraryVersions - 1 do begin
        TmpStr := '';
        for lIndex2 := 0 to 7 do begin
          if m_LibraryVersion[lIndex].szName[lIndex2] <> #0 then begin
            TmpStr := TmpStr + m_LibraryVersion[lIndex].szName[lIndex2];
          end;
        end;
        LogEx('Library Name                     : ' + TmpStr, lType);
        LogEx('Version                          : ' + inttostr(m_LibraryVersion[lIndex].wMajorVersion) + '.' + inttostr(m_LibraryVersion[lIndex].wMinorVersion) + '.' + inttostr(m_LibraryVersion[lIndex].wBuildVersion), lType);

        //Some bit maths the QVersion Flag is only 13 bits long so i convert the 13 bits to a number

        QVersion := 0;

        Flag := Ord(m_LibraryVersion[lIndex].dwFlags[0]);

        for bIndex := 0 to 7 do begin
          QVersion := QVersion + GetBitEn(Flag, bIndex) * round(Power(2, bIndex));
        end;

        Flag := Ord(m_LibraryVersion[lIndex].dwFlags[1]);

        for bIndex := 0 to 4 do begin
          QVersion := QVersion + GetBitEn(Flag, BIndex) * round(Power(2, bIndex + 8));
        end;
       //end of bits maths

        TmpStr := 'Flags                            : QFEVersion : 0x' + inttohex(QVersion, 4) + ', ';

        if GetBitEn(Flag, 7) = 1 then begin
          TmpStr := TmpStr + 'Debug, '
        end
        else begin
          TmpStr := TmpStr + 'Retail, ';
        end;

        if (GetBitEn(Flag, 5) * 1 + GetBitEn(Flag, 6) * 2) = 0 then begin
          TmpStr := TmpStr + 'Unapproved'
        end
        else
          if (GetBitEn(Flag, 5) * 1 + GetBitEn(Flag, 6) * 2) = 1 then begin
            TmpStr := TmpStr + 'Possibly Approved'
          end
          else begin
            TmpStr := TmpStr + 'Approved';
          end;

        LogEx(TmpStr, lType);
      end;
    end;


    LogEx('Dumping XBE TLS...', lType);
    LogEx('Data Start Address               : 0x' + IntToHex(m_TLS.dwDataStartAddr, 8), lType);
    LogEx('Data End Address                 : 0x' + IntToHex(m_TLS.dwDataEndAddr, 8), lType);
    LogEx('TLS Index Address                : 0x' + IntToHex(m_TLS.dwTLSIndexAddr, 8), lType);
    LogEx('TLS Callback Address             : 0x' + IntToHex(m_TLS.dwTLSCallbackAddr, 8), lType);
    LogEx('Size of Zero Fill                : 0x' + IntToHex(m_TLS.dwSizeofZeroFill, 8), lType);
    LogEx('Characteristics                  : 0x' + IntToHex(m_TLS.dwCharacteristics, 8), lType);


    if DumpToFile then begin
      CloseFile(FileEx);
    end

  finally
    Result := True;
  end
end; // TXbe.DumpInformation

//------------------------------------------------------------------------------

function TXbe.GetAddr(x_dwVirtualAddress: DWord): integer;
var
  lIndex, VirtAddr, VirtSize, dwoffs: DWord;
begin
  dwoffs := x_dwVirtualAddress - m_Header.dwBaseAddr;
  Result := 0;

  // offset in image header
  if dwoffs < m_Header.dwSizeofHeaders then begin
    result := dwOffs
  end
  else begin
    // offset in image header extra bytes
    if dwoffs < m_Header.dwSizeofHeaders then begin
      result := dwOffs //- sizeof(m_Header)
    end
    else begin
      for lIndex := 0 to m_Header.dwSections - 1 do begin
        VirtAddr := m_SectionHeader[lIndex].dwVirtualAddr;
        VirtSize := m_SectionHeader[lIndex].dwVirtualSize;
        if ((x_dwVirtualAddress >= VirtAddr) and (x_dwVirtualAddress < (VirtAddr + VirtSize))) then begin
          result := m_SectionHeader[lIndex].dwRawAddr + (x_dwVirtualAddress - VirtAddr);
        end;
      end;
    end;
  end;
end; // TXbe.GetAddr

//------------------------------------------------------------------------------

procedure TXbe.ExportLogoBitmap(ImgCont: TBitmap);
var
  x_Gray: array[0..100 * 17] of char;
  dwLength, o, lIndex, lIndex2, len, data: DWord;
  RLE: DWord;
  pos0, pos1: Byte;
begin
  dwLength := m_Header.dwSizeofLogoBitmap;
  RLE := GetAddr(m_Header.dwLogoBitmapAddr);
  if RLE = 0 then begin
    exit
  end;
  len := 0;
  data := 0;
  o := 0;
  lIndex := 0;
  while lIndex < dwLength do begin

    // Read 2 bytes.
    Pos0 := ord(Buffer[RLE + lIndex]);
    Pos1 := ord(Buffer[RLE + 1 + lIndex]);

    if Pos0 and 1 = 1 then begin                          // Check if the bit 0 is set.
      len := Pos0 shr 1 and 7;                            // Select the bits from 1 to 3
      data := Pos0 shr 4 and 15;                          // Select the bits from 4 to 7
    end
    else begin
      if Pos0 and 2 <> 1 then begin                       // Check if the bit 1 is set.
        len := (Pos0 shr 2 and 63) + (Pos1 and 15)*256;   // Select the bits from 2 to 7 from the first byte (Pos0) and the bits from 0 to 3 from the second byte (Pos1) and form a number.
        data := Pos1 shr 4 and 15;                        // Select the bits from 4 to 7 from the second byte (Pos1)
        inc(lIndex);                                      // The index is incremented because 2 bytes were read.
      end;
    end;
    for lIndex2 := 0 to len - 1 do begin
      inc(o);
      if (o < 100 * 17) then begin
        x_Gray[o] := chr(data shl 4);
        ImgCont.Canvas.Pixels[o mod 100, o div 100] := RGB(ord(x_Gray[o]), ord(x_Gray[o]), ord(x_Gray[o]));
      end
      else begin
        Exit;
      end;
    end;
    inc(lIndex);                                          // Index increment
  end;
end; // TXbe.ExportLogoBitmap

//------------------------------------------------------------------------------

function TXbe.GetTLSData: DWord;
begin
  Result := GetAddr(m_TLS.dwDataStartAddr);
end; // TXbe.GetTLSData

//------------------------------------------------------------------------------

destructor TXbe.Destroy;
begin
  freemem(Buffer);
  inherited;
end;

//------------------------------------------------------------------------------

end.
