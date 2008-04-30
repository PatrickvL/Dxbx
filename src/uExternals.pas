unit uExternals;

interface

Uses
  Types, uEnums, uXbe, uConsts;

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
                        Entry : pEntryProc );  stdcall; external cDLLNAME;

procedure SetXbePath(const path : PChar ); stdCall; external cDLLNAME name '?SetXbePath@EmuShared@@QAEXPBD@Z';
procedure CxbxKrnlNoFunc; stdCall; external cDLLNAME;

implementation

end.
