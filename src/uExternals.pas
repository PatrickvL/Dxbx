unit uExternals;

interface

uses
  // Delphi
  Types, // for DWord
  // Dxbx
  uConsts, // for cDLLName
  uEnums, // for DebugMode
  uXbe; // for P_XBE_TLS

type
  TEntryProc = procedure();
  PEntryProc = ^TEntryProc;
  TThunkTable = packed array[0..366] of DWord;
  PThunkTable = ^TThunkTable;

procedure CxbxKrnlInit(hwndParent: THandle;
                       pTLSData: Pointer;
                       pTLS: P_XBE_TLS;
                       pLibraryVersion: P_XBE_LIBRARYVERSION;
                       DbgMode: DebugMode;
                       szDebugFilename: PChar;
                       pXbeHeader: P_XBE_HEADER;
                       dwXbeHeaderSize: DWord;
                       Entry: PEntryProc); cdecl; external cDLLNAME;

procedure SetXbePath(const path: PChar); stdcall; external cDLLNAME name '?SetXbePath@EmuShared@@QAEXPBD@Z';
procedure CxbxKrnlNoFunc; cdecl; external cDLLNAME;

implementation

end.
