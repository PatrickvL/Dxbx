unit uData;

interface

Type
  PXDKInfo = ^XDKInfo;
  XDKInfo = Record
    GameName : String;
    XAPILIB  : String;
    XBOXKRNL : String;
    LIBCMT   : String;
    D3D8     : String;
    XGRAPHC  : String;
    DSOUND   : String;
    XMV      : String;
  end;

implementation

end.
