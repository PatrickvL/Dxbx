unit uExternals;

interface

Uses
  Types, uEnums, uXbe;

type
  tEntryProc = Procedure();
  pEntryProc = ^tEntryProc;
  tThunkTable = packed array[0..366] of DWord;
  pThunkTable = ^tThunkTable;

procedure CxbxKrnlInit( hwndParent : THandle;
                        pTLSData : pointer;
                        pTLS : P_XBE_TLS;
                        pLibraryVersion : P_XBE_LIBRARYVERSION;
                        DbgMode : DebugMode;
                        szDebugFilename : PChar;
                        pXbeHeader : P_XBE_HEADER;
                        dwXbeHeaderSize : DWord;
                        Entry : pEntryProc );  stdcall; external 'DxbxKrnl.dll';    

procedure SetXbePath(const path : PChar ); external 'DxbxKrnl.dll' name '?SetXbePath@EmuShared@@QAEXPBD@Z';

implementation

end.
