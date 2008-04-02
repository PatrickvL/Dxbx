unit uExternals;

interface

Uses
  Types, uEnums, uXbe;

type
  tEntryProc = Procedure();
  pEntryProc = ^tEntryProc;

procedure EmuInit( pTLSData : pointer;
                   pTLS : P_XBE_TLS;
                   pLibraryVersion : P_XBE_LIBRARYVERSION;
                   DbgMode : DebugMode;
                   szDebugFilename : PChar;
                   pXbeHeader : P_XBE_HEADER;
                   dwXbeHeaderSize : DWord;
                   Entry : pEntryProc );  stdcall; external 'DxbxKrnl.dll' name '_EmuInit@32';

implementation

end.
