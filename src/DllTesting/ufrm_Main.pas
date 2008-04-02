unit ufrm_Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  uXbe, uEnums, uEmu;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


  procedure EmuInit( pTLSData : pointer;
                     pTLS : P_XBE_TLS;
                     pLibraryVersion : P_XBE_LIBRARYVERSION;
                     DbgMode : DebugMode;
                     szDebugFilename : PChar;
                     pXbeHeader : P_XBE_HEADER;
                     dwXbeHeaderSize : DWord;
                     Entry : pEntryProc ); external 'DxbxKrnl.dll';


var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  EmuInit ( nil, nil, nil, DM_CONSOLE, 'test', nil, 0, nil );
end;

end.
