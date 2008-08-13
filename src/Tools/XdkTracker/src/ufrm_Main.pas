unit ufrm_Main;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Messages,
  Contnrs, Controls, Forms, Dialogs, ComCtrls, Menus, ExtCtrls, ShellApi,
  xmldom, XMLIntf, msxmldom, XMLDoc,
  // Dxbx
  uData,
  u_xdkversions,
  u_About,
  uPublisher,
  uImportGames,
  uXbe,
  uDxbxXml,
  uConsts, jpeg;


const
  cXDKLIST_LOADED = 'XDK List Loaded';
  cNO_XDKLIST_LOADED = 'No XDK List Loaded';
  cVista_dwMajorVersion = 6;

type
  TfrmXdkTracker = class(TForm)
    MainMenu1: TMainMenu;
    Viewxdkversion1: TMenuItem;
    Viewxdkversion2: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    VisitShadowTjwebsite1: TMenuItem;
    VisitCaustikswebsite1: TMenuItem;
    VisitCxbxForum1: TMenuItem;
    N2: TMenuItem;
    Image1: TImage;
    File2: TMenuItem;
    Exit2: TMenuItem;
    N4: TMenuItem;
    ImportGameList1: TMenuItem;
    ExportGameList1: TMenuItem;
    ImportDialog: TOpenDialog;
    ExportDialog: TSaveDialog;
    XMLDocument: TXMLDocument;
    N1: TMenuItem;
    GetXDKInfofromXbe1: TMenuItem;
    XbeOpenDialog: TOpenDialog;
    StatusBar1: TStatusBar;
    procedure Viewxdkversion2Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure VisitShadowTjwebsite1Click(Sender: TObject);
    procedure VisitCaustikswebsite1Click(Sender: TObject);
    procedure VisitCxbxForum1Click(Sender: TObject);
    procedure Exit2Click(Sender: TObject);
    procedure ExportGameList1Click(Sender: TObject);
    procedure GetXDKInfofromXbe1Click(Sender: TObject);
    procedure ImportGameList1Click(Sender: TObject);
  private
    ApplicationDir: string;

    ImportList: TList;
    GameList: TObjectList;

    function SearchGameName(GameName: string): Boolean;

    procedure InsertXDKInfo(XInfo: TXDKInfo);

    procedure LoadGameData;
    procedure SaveGameData(const aFilePath, aPublishedBy: string);

    procedure ImportGameData;
    procedure ExportGameData;
    procedure ImportXbeDump;

    procedure WMCopyData(var Msg: TWMCopyData); message WM_COPYDATA;

  public
    XInfo: TXDKInfo;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmXdkTracker: TfrmXdkTracker;
  mHandle: THandle;    // Mutexhandle

implementation

{$R *.dfm}

//------------------------------------------------------------------------------

function IsWindowsVista: Boolean;
var
  VerInfo: TOSVersionInfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(VerInfo);
  Result := VerInfo.dwMajorVersion >= cVista_dwMajorVersion;
end;

function SortGameList(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareText(TXDKInfo(Item1).GameName, TXDKInfo(Item2).GameName);
end;

//------------------------------------------------------------------------------

procedure TfrmXdkTracker.Viewxdkversion2Click(Sender: TObject);
var
  lIndex, j: Integer;
  XDKlist: TStringList;
  XDKInfo: TXDKInfo;
begin
  frm_xdkversion := Tfrm_xdkversion.Create(Application);

  XDKlist := TStringList.Create;
  XDKlist.Clear;
  XDKlist.Duplicates := dupIgnore;
  XDKlist.Sorted := True;
  with frm_XdkVersion do
  begin
    lst_Games.Clear;
    for lIndex := 0 to GameList.Count - 1 do
    begin
      XDKInfo := TXDKInfo(GameList.Items[lIndex]);
      for j := 0 to XDKInfo.LibVersions.Count - 1 do
        XDKlist.Add(XDKInfo.LibVersions.ValueFromIndex[j]);
    end;

    cmb_gametype.Items.Clear;
    cmb_gametype.Items.Add('All XDK Versions');
    for lIndex := 0 to XDKlist.Count - 1 do
      if XDKlist.Strings[lIndex] <> '' then
        cmb_gametype.Items.Add(XDKlist.Strings[lIndex]);

    cmb_gametype.ItemIndex := 0;
  end;

  frm_XdkVersion.FillGameList(GameList);

  if frm_XdkVersion.ShowModal = mrOk then
  begin
  end;

  FreeAndNil({var}frm_XdkVersion);
end; // TfrmMain.Viewxdkversion2Click

//------------------------------------------------------------------------------

procedure TfrmXdkTracker.About1Click(Sender: TObject);
begin
  frm_About := Tfrm_About.Create(Application);

  if frm_About.ShowModal = mrOk then
  begin
  end;

  FreeAndNil({var}frm_About);
end; // TfrmMain.About1Click

//------------------------------------------------------------------------------

procedure TfrmXdkTracker.VisitShadowTjwebsite1Click(Sender: TObject);
begin
  ShellExecute(0, cOpen, cWEBSITE_SHADOWTJ, nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmXdkTracker.WMCopyData(var Msg: TWMCopyData);

  procedure HandleCopyDataString(CopyDataStruct: PCopyDataStruct);
  var
    s: string;
  begin
    s := PChar(copyDataStruct.lpData);
    if s = 'READXML' then
      ImportXbeDump;
  end;

begin
  case Msg.CopyDataStruct.dwData of
    0: HandleCopyDataString(Msg.CopyDataStruct);
  end;
  //Send something back
  msg.Result := 1;
end;

//------------------------------------------------------------------------------

procedure TfrmXdkTracker.VisitCaustikswebsite1Click(Sender: TObject);
begin
  ShellExecute(0, cOpen, cWEBSITE_CXBX, nil, nil, SW_SHOWNORMAL);
end; // TfrmMain.VisitCaustikswebsite1Click

//------------------------------------------------------------------------------

procedure TfrmXdkTracker.VisitCxbxForum1Click(Sender: TObject);
begin
  ShellExecute(0, cOpen, cWEBSITE_FORUM, nil, nil, SW_SHOWNORMAL);
end; // TfrmMain.VisitCxbxForum1Click

//------------------------------------------------------------------------------

procedure TfrmXdkTracker.Exit2Click(Sender: TObject);
begin
  Close;
end; // TfrmMain.Exit2Click

//------------------------------------------------------------------------------

constructor TfrmXdkTracker.Create(aOwner: TComponent);
var
  parameter: string;
begin
  inherited Create(aOwner);

  ApplicationDir := ExtractFilePath(Application.ExeName);
  StatusBar1.SimpleText := cNO_XDKLIST_LOADED;

  GameList := TObjectList.Create({OwnsObjects=}True);
  LoadGameData;

  Parameter := ParamStr(1);
  if Parameter = '/XBEDUMP' then
    ImportXbeDump;
end; // TfrmMain.Create

//------------------------------------------------------------------------------

destructor TfrmXdkTracker.Destroy;
begin
  SaveGameData(ApplicationDir + cXDK_TRACKER_DATA_FILE, {aPublishedBy=} '');
  FreeAndNil({var}GameList);

  inherited Destroy;
end; // TfrmMain.Destroy

//------------------------------------------------------------------------------

procedure TfrmXdkTracker.ExportGameData;
begin
  frm_Publisher := Tfrm_Publisher.Create(Self);
  if frm_Publisher.ShowModal = mrOk then
    if ExportDialog.Execute then
      SaveGameData(ExportDialog.FileName, frm_Publisher.edtPublisher.Text);

  frm_Publisher.Release;
end; // TfrmMain.ExportGameData

//------------------------------------------------------------------------------

function _ReadGameFromNode(const GameNode: IXmlNode): TXDKInfo;
var
  XDKNode, LibNode: IXmlNode;
begin
  Result := TXDKInfo.Create;
  Result.GameName := XML_ReadString(GameNode, 'Name');
  XDKNode := GameNode.ChildNodes.FindNode('XDKVersions');
  if Assigned(XDKNode) then
  begin
    LibNode := XDKNode.ChildNodes.First;
    while Assigned(LibNode) do
    begin
      Result.LibVersions.Values[LibNode.LocalName] := LibNode.Text;
      LibNode := LibNode.NextSibling;
    end;
  end;
end;

procedure TfrmXdkTracker.ImportGameData;
var
  xmlRootNode: IXmlNode;
  InfoNode: IXmlNode;
  GameNode: IXmlNode;
  lIndex: Integer;
  Publisher: string;
  GameName: string;
  Line: TListItem;
  XDKInfo: TXDKInfo;
begin
  if ImportDialog.Execute then
  begin
    ImportList := TList.Create;

    XmlDocument.Active := False;
    XmlDocument.FileName := ImportDialog.FileName;
    try
      XmlDocument.Active := True;
    except
      on E: EDOMParseError do
      begin
        MessageDlg('Error parsing the file!', mtError, [mbOk], -1);
        XmlDocument.Active := False;
      end
      else
        XmlDocument.Active := False;
    end;

    if XmlDocument.Active then
    begin

      XmlRootNode := XMLDocument.DocumentElement;

      InfoNode := XmlRootNode.ChildNodes.FindNode('PublishedInfo');
      Publisher := XML_ReadString(InfoNode, 'PublishedBy');

      InfoNode := XmlRootNode.ChildNodes.FindNode('GameList');

      GameNode := InfoNode.ChildNodes.First;
      while Assigned(GameNode) do
      begin
        GameName := XML_ReadString(GameNode, 'Name');
        if not SearchGameName(GameName) then
        begin
          XInfo := _ReadGameFromNode(GameNode);
          ImportList.Add(XInfo);
        end;

        GameNode := GameNode.NextSibling;
      end;

      ImportList.Sort(SortGameList);

      frm_ImportGames := Tfrm_ImportGames.Create(Self);

      with frm_ImportGames do
      begin
        edt_Publisher.Text := Publisher;
        for lIndex := 0 to ImportList.Count - 1 do
        begin
          XDKInfo := TXDKInfo(ImportList.Items[lIndex]);
          Line := lst_Import.Items.Add;
          Line.Caption := XDKInfo.GameName;
          Line.SubItems.Add(XDKInfo.LibVersions.Values['XAPILIB']);
          Line.SubItems.Add(XDKInfo.LibVersions.Values['XBOXKRNL']);
          Line.SubItems.Add(XDKInfo.LibVersions.Values['LIBCPMT']);
          Line.SubItems.Add(XDKInfo.LibVersions.Values['LIBCMT']);
          Line.SubItems.Add(XDKInfo.LibVersions.Values['LIBC']);
          Line.SubItems.Add(XDKInfo.LibVersions.Values['D3D8']);
          Line.SubItems.Add(XDKInfo.LibVersions.Values['D3DX8']);
          Line.SubItems.Add(XDKInfo.LibVersions.Values['XGRAPHC']);
          Line.SubItems.Add(XDKInfo.LibVersions.Values['DSOUND']);
          Line.SubItems.Add(XDKInfo.LibVersions.Values['XVOICE']);
          Line.SubItems.Add(XDKInfo.LibVersions.Values['XMV']);
          Line.SubItems.Add(XDKInfo.LibVersions.Values['XONLINES']);
          Line.SubItems.Add(XDKInfo.LibVersions.Values['UIX']);
          Line.SubItems.Add(XDKInfo.LibVersions.Values['VOICMAIL']);
          Line.SubItems.Add(XDKInfo.LibVersions.Values['XVOCREC']);
          Line.SubItems.Add(XDKInfo.LibVersions.Values['XACTENG']);
          Line.SubItems.Add(XDKInfo.LibVersions.Values['CalcSig']);
          Line.SubItems.Add(XDKInfo.LibVersions.Values['DMUSIC']);
        end;
      end;

      if frm_ImportGames.ShowModal = mrOk then
      begin
        for lIndex := 0 to ImportList.Count - 1 do
          InsertXDKInfo(ImportList[lIndex]);
      end;

      frm_ImportGames.Release;

      FreeAndNil({var}ImportList);
    end;
  end;
end; // TfrmMain.ImportGameData

//------------------------------------------------------------------------------

procedure TfrmXdkTracker.LoadGameData;
var
  GameDataFilePath: string;

  XmlRoot: IXmlNode;
  GameListNode: IXmlNode;
  GameNode: IXmlNode;
begin
  GameDataFilePath := ApplicationDir + cXDK_TRACKER_DATA_FILE;
  if FileExists(GameDataFilePath) then
  begin
    XMLDocument.Active := False;
    XMLDocument.LoadFromFile(GameDataFilePath);
    XMLDocument.Active := True;

    XmlRoot := XmlDocument.DocumentElement;
    GameListNode := XmlRoot.ChildNodes.FindNode('GameList');

    GameNode := GameListNode.ChildNodes.FindNode('Game');
    while Assigned(GameNode) do
    begin

      XInfo := _ReadGameFromNode(GameNode);
      InsertXDKInfo(XInfo);

      GameNode := GameNode.NextSibling;
    end;

    Gamelist.Sort(SortGameList);
    StatusBar1.SimpleText := cXDKLIST_LOADED;
  end;
end; // TfrmMain.LoadGameData

//------------------------------------------------------------------------------

procedure TfrmXdkTracker.ExportGameList1Click(Sender: TObject);
begin
  ExportGameData;
end; // TfrmMain.ExportGameList1Click

procedure TfrmXdkTracker.GetXDKInfofromXbe1Click(Sender: TObject);
var
  m_Xbe: TXbe;
  m_XbeFilename: string;
  m_ExeFilename: string;
begin
  XbeOpenDialog.Filter := DIALOG_FILTER_XBE;
  m_Xbe := Nil;
  if not XbeOpenDialog.Execute then
    Exit;

  if OpenXbe(XbeOpenDialog.Filename, m_Xbe, m_ExeFilename, m_XbeFilename ) then
  begin
    DxbxXml.CreateXmlXbeDump(ExtractFilePath(Application.ExeName) + 'Dump.dat', m_Xbe);
    ImportXbeDump;
  end;
end;

procedure TfrmXdkTracker.ImportGameList1Click(Sender: TObject);
begin
  ImportGameData;
end; // TfrmMain.ImportGameList1Click

//------------------------------------------------------------------------------

procedure TfrmXdkTracker.SaveGameData(const aFilePath, aPublishedBy: string);
var
  XmlRootNode: IXmlNode;
  PublishedNode: IXmlNode;
  GameListNode: IXmlNode;
  GameNode: IXmlNode;
  XDKnode: IXmlNode;
  lIndex, j: Integer;
  XDKInfo: TXDKInfo;
begin
  if XMLDocument.Active then
  begin
    XMLDocument.ChildNodes.Clear;
    XmlRootNode := XMLDocument.AddChild('XDKInfo');
    XmlRootNode.SetAttribute('Version', cXDk_TRACKER_XML_VERSION);

    PublishedNode := XmlRootNode.AddChild('PublishedInfo');

    XML_WriteString(PublishedNode, 'PublishedBy', aPublishedBy);

    GameListNode := XmlRootNode.AddChild('GameList');

    for lIndex := 0 to GameList.Count - 1 do
    begin
      XDKInfo := TXDKInfo(GameList.Items[lIndex]);
      GameNode := GameListNode.AddChild('Game');

      XML_WriteString(GameNode, 'Name', XDKInfo.GameName);
      XDKnode := GameNode.AddChild('XDKVersions');

      for j := 0 to XDKInfo.LibVersions.Count - 1 do
        XML_WriteString(XDKnode, XDKInfo.LibVersions.Names[j], XDKInfo.LibVersions.ValueFromIndex[j]);
    end;

    XMLDocument.SaveToFile(aFilePath);
  end;

end; // TfrmMain.SaveGameData
//------------------------------------------------------------------------------

procedure TfrmXdkTracker.ImportXbeDump;
var
  DumpFilePath: string;
  RootNode: IXmlNode;
  GameNode: IXmlNode;
begin
  DumpFilePath := ApplicationDir + 'Dump.dat';
  if FileExists(DumpFilePath) then
  begin
    XmlDocument.Active := False;
    XMLDocument.LoadFromFile(DumpFilePath);
    XmlDocument.Active := True;

    RootNode := XMLDocument.DocumentElement;

    GameNode := RootNode;

    while Assigned(GameNode) do
    begin
      if not SearchGameName(XML_ReadString(GameNode, 'Name')) then
        InsertXDKInfo(_ReadGameFromNode(GameNode));

      GameNode := GameNode.NextSibling;
    end;

    DeleteFile(DumpFilePath);
  end;
end; // TfrmMain.ImportXbeDump

//------------------------------------------------------------------------------

function TfrmXdkTracker.SearchGameName(GameName: string): Boolean;
var
  lIndex: Integer;
begin
  Result := False;
  for lIndex := 0 to GameList.Count - 1 do
    if TXDKInfo(GameList.Items[lIndex]).GameName = GameName then
    begin
      Result := True;
      Break;
    end;
end; // TfrmMain.SearchGameName

//------------------------------------------------------------------------------

procedure TfrmXdkTracker.InsertXDKInfo(XInfo: TXDKInfo);
begin
  if XInfo.GameName = '' then
  begin
    FreeAndNil(XInfo);
    Exit;
  end;

  XInfo.LibVersions.Sort;
  GameList.Add(XInfo);
  GameList.Sort(SortGameList);
end; // TfrmMain.InsertXDKInfo
   
//------------------------------------------------------------------------------

initialization

  mHandle := CreateMutex(nil, True, 'XYZ');
  if GetLastError = ERROR_ALREADY_EXISTS then
  begin
    ShowMessage('Program is already running!');
    Halt;
  end;

//------------------------------------------------------------------------------

finalization

  if mHandle <> 0 then
    CloseHandle(mHandle)

//------------------------------------------------------------------------------


end.

