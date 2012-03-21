unit XbeHeaders;

interface

Uses
  // Delphi
  Windows,
  // Dxbx
  uEmuD3D8Types,
  uConsts;

const
  XBOX_KEY_LENGTH = 16;
  XBEIMAGE_ALTERNATE_TITLE_ID_COUNT = 16;
  XBE_TITLENAME_MAXLENGTH = 40;
  XBE_SECTIONNAME_MAXLENGTH = 9;
  XBE_LIBRARYNAME_MAXLENGTH = 8;
  XPR_IMAGE_HDR_SIZE = 2048; // 2K
  XPR_IMAGE_WH = 128; // width and height
  XPR_IMAGE_DATA_SIZE = (XPR_IMAGE_WH * XPR_IMAGE_WH) div 2; // DXT1 is 4 bits per pixel

type
  _XBEIMAGE_HEADER = packed record
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
    dwInitFlags: DWord; // 0x0124 - initialization flags
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
  XBEIMAGE_HEADER = _XBEIMAGE_HEADER;
  PXBEIMAGE_HEADER = ^XBEIMAGE_HEADER;

  TXbeHeader = XBEIMAGE_HEADER;
  PXbeHeader = PXBEIMAGE_HEADER;

  XBOX_KEY_DATA = array [0..XBOX_KEY_LENGTH-1] of UCHAR;

  _XBE_CERTIFICATE = packed record
    dwSize: DWord; // 0x0000 - size of certificate
    dwTimeDate: DWord; // 0x0004 - timedate stamp
    dwTitleId: DWord; // 0x0008 - title id
    wszTitleName: array [0..XBE_TITLENAME_MAXLENGTH-1] of WideChar; // 0x000C - title name (unicode)
    dwAlternateTitleId: array [0..XBEIMAGE_ALTERNATE_TITLE_ID_COUNT-1] of Dword; // 0x005C - alternate title ids
    dwAllowedMedia: Dword; // 0x009C - allowed media types
    dwGameRegion: DWord; // 0x00A0 - game region
    dwGameRatings: DWord; // 0x00A4 - game ratings
    dwDiskNumber: DWord; // 0x00A8 - disk number
    dwVersion: Dword; // 0x00AC - version
    bzLanKey: XBOX_KEY_DATA; // 0x00B0 - lan key
    bzSignatureKey: XBOX_KEY_DATA; // 0x00C0 - signature key
    bzTitleAlternateSignatureKey: array [0..XBEIMAGE_ALTERNATE_TITLE_ID_COUNT-1] of XBOX_KEY_DATA; // 0x00D0 - alternate signature keys
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


implementation

end.
