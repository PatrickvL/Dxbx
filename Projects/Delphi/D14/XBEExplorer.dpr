program XBEExplorer;

{$R 'XBEExplorerResources.res' '..\..\..\resources\XBEExplorerResources.rc'}

uses
  Forms,
  SysUtils,
  Winsock,
  Dialogs,
  IdHttp,
  Classes,
  Controls,
  ShellApi,
  Windows,
  uDisassembleViewer in '..\..\..\Source\Delphi\src\Tools\XBEExplorer\uDisassembleViewer.pas',
  uExploreFileSystem in '..\..\..\Source\Delphi\src\Tools\XBEExplorer\uExploreFileSystem.pas' {frmExploreFileSystem},
  uHexViewer in '..\..\..\Source\Delphi\src\Tools\XBEExplorer\uHexViewer.pas',
  uSectionViewer in '..\..\..\Source\Delphi\src\Tools\XBEExplorer\uSectionViewer.pas',
  uStringsViewer in '..\..\..\Source\Delphi\src\Tools\XBEExplorer\uStringsViewer.pas',
  uViewerUtils in '..\..\..\Source\Delphi\src\Tools\XBEExplorer\uViewerUtils.pas',
  uXBEExplorerMain in '..\..\..\Source\Delphi\src\Tools\XBEExplorer\uXBEExplorerMain.pas' {FormXBEExplorer},
  uConsoleClass in '..\..\..\Source\Delphi\src\uConsoleClass.pas',
  uConsts in '..\..\..\Source\Delphi\src\uConsts.pas',
  uCRC16 in '..\..\..\Source\Delphi\src\uCRC16.pas',
  uDisassembleUtils in '..\..\..\Source\Delphi\src\uDisassembleUtils.pas',
  uDxbxUtils in '..\..\..\Source\Delphi\src\uDxbxUtils.pas',
  uFileSystem in '..\..\..\Source\Delphi\src\uFileSystem.pas',
  uLog in '..\..\..\Source\Delphi\src\uLog.pas',
  uTime in '..\..\..\Source\Delphi\src\uTime.pas',
  uTypes in '..\..\..\Source\Delphi\src\uTypes.pas',
  uXbe in '..\..\..\Source\Delphi\src\uXbe.pas',
  uXDVDFS in '..\..\..\Source\Delphi\src\uXDVDFS.pas',
  uEmuD3D8Types in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuD3D8Types.pas',
  VistaIconFix in '..\..\..\Source\Delphi\src\VistaIconFix.pas',
  XbeHeaders in '..\..\..\Source\Delphi\src\Headers\XbeHeaders.pas',
  BeaEngine in '..\..\..\Libraries\Pascal\BeaEngine\BeaEngine.pas';

{$R *.res}

function IAddrToHostName(const IP: string): string;
var
  i: Integer;
  p: PHostEnt;
begin
  Result := '';
  i      := inet_addr(PAnsiChar(IP));
  if i <> u_long(INADDR_NONE) then
  begin
    p := GetHostByAddr(@i, SizeOf(Integer), PF_INET);
    if p <> nil then Result := p^.h_name;
  end
  else
    Result := 'Invalid IP address';
end;

function GetOnlineVersion: string;
var
  lHTTP: TIdHTTP;
  lParamList: TStringList;
begin
  lParamList := TStringList.Create;
  lHTTP := TIdHTTP.Create(nil);
  try
    Result := lHTTP.Post('http://dxbx.svn.sourceforge.net/viewvc/dxbx/version.xdktracker', lParamList);
  finally
    FreeAndNil(lHTTP);
    FreeAndNil(lParamList);
  end;
end;

var
  MyVersion: Integer;
  MyVersionString: string;
  OnlineVersion: Integer;
  OnlineVersionString: string;
  Url: string;

begin
  // Check for internet connection by getting ip from dns
  If IAddrToHostName('www.google.com') <> '' then
  begin
     MyVersionString := _XBE_EXPLORER_VERSION;
     MyVersionString := StringReplace(MyVersionString, '.', '', [rfReplaceAll]);
     MyVersion := StrToInt(MyVersionString);

     OnlineVersionString := GetOnlineVersion;
     OnlineVersion := StrToInt(StringReplace(OnlineVersionString, '.', '', [rfReplaceAll]));
     if MyVersion < OnlineVersion then
     begin
       if (MessageDlg('There is a new version of Xbe Explorer, do you want to download it ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
       begin
         Url := 'http://sourceforge.net/projects/dxbx/files/XBE%20Explorer/' + OnlineVersionString + '%20Release/';
         ShellExecute(0, 'open', PChar(Url), nil, nil, SW_SHOWNORMAL);
         Exit;
       end;
     end;
  end;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormXBEExplorer, FormXBEExplorer);
  Application.CreateForm(TfrmExploreFileSystem, frmExploreFileSystem);
  if ParamCount > 0 then
    if FileExists(ParamStr(1)) then
      FormXBEExplorer.OpenFile(ParamStr(1));

  Application.Run;
end.

