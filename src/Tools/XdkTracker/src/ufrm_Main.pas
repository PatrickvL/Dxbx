unit ufrm_Main;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Messages, FileCtrl,
  Contnrs, Controls, Forms, Dialogs, ComCtrls, Menus, ExtCtrls, ShellApi,
  xmldom, XMLIntf, msxmldom, XMLDoc, jpeg,
  // Dxbx
  uDxbxUtils,
  uData,
  u_xdkversions,
  u_About,
  uPublisher,
  uImportGames,
  uXbe,
  uDxbxXml,
  uConsts;

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
    ImportTxtDumps1: TMenuItem;
    procedure Viewxdkversion2Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure VisitShadowTjwebsite1Click(Sender: TObject);
    procedure VisitCaustikswebsite1Click(Sender: TObject);
    procedure VisitCxbxForum1Click(Sender: TObject);
    procedure Exit2Click(Sender: TObject);
    procedure ExportGameList1Click(Sender: TObject);
    procedure GetXDKInfofromXbe1Click(Sender: TObject);
    procedure ImportGameList1Click(Sender: TObject);
    procedure ImportTxtDumps1Click(Sender: TObject);
  private
    ApplicationDir: string;
    MyXBEList: TStringList;
    function FindByFileName(const aFileName: string): Integer;
    function InsertXBEInfo(const aXbeInfo: TXBEInfo): Boolean;
    procedure SaveXBEList(const aFilePath, aPublishedBy: string);
    function ShowImportList(const XBEImportList: TList; Publisher: string): Integer;
    function LoadXBEList(aImportFilePath: string = '';
      aDeleteAfterRead: Boolean = True; aUseImportDialog: Boolean = False): Integer;
    procedure ImportTxtDumps(const aTxtDumpsFolder: TFileName);
    procedure WMCopyData(var Msg: TWMCopyData); message WM_COPYDATA;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmXdkTracker: TfrmXdkTracker;

implementation

{$R *.dfm}

procedure FreeStringListObjects(const aStringList: TStringList);
var
  i: Integer;
begin
  if Assigned(aStringList) then
    for i := 0 to aStringList.Count - 1 do
    begin
      aStringList.Objects[i].Free;
      aStringList.Objects[i] := nil;
    end;
end;

{ TfrmXdkTracker }

constructor TfrmXdkTracker.Create(aOwner: TComponent);
var
  parameter: string;
begin
  inherited Create(aOwner);

  ApplicationDir := ExtractFilePath(Application.ExeName);
  StatusBar1.SimpleText := cNO_XDKLIST_LOADED;

  MyXBEList := TStringList.Create;
  if LoadXBEList(ApplicationDir + cXDK_TRACKER_DATA_FILE, {aDeleteAfterRead=}False, {aUseImportDialog=}False) > 0 then
    StatusBar1.SimpleText := cXDKLIST_LOADED;

  Parameter := ParamStr(1);
  if SameText(Parameter, '/XBEDUMP') then
    LoadXBEList;

  if SameText(Parameter, '/TxtDumps') then
    ImportTxtDumps(ParamStr(2));
end; // TfrmMain.Create

destructor TfrmXdkTracker.Destroy;
begin
  if Assigned(frm_Publisher) then
    SaveXBEList(ApplicationDir + cXDK_TRACKER_DATA_FILE, {aPublishedBy=}frm_Publisher.edtPublisher.Text)
  else
    SaveXBEList(ApplicationDir + cXDK_TRACKER_DATA_FILE, {aPublishedBy=}'');
    
  FreeStringListObjects(MyXBEList);
  FreeAndNil({var}MyXBEList);

  inherited Destroy;
end; // TfrmMain.Destroy

procedure TfrmXdkTracker.Exit2Click(Sender: TObject);
begin
  Close;
end; // TfrmMain.Exit2Click

procedure TfrmXdkTracker.About1Click(Sender: TObject);
begin
  frm_About := Tfrm_About.Create(Application);

  if frm_About.ShowModal = mrOk then
  begin
  end;

  frm_About.Release;
  frm_About := nil;
end; // TfrmMain.About1Click

procedure TfrmXdkTracker.VisitShadowTjwebsite1Click(Sender: TObject);
begin
  ShellExecute(0, cOpen, cWEBSITE_SHADOWTJ, nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmXdkTracker.VisitCaustikswebsite1Click(Sender: TObject);
begin
  ShellExecute(0, cOpen, cWEBSITE_CXBX, nil, nil, SW_SHOWNORMAL);
end; // TfrmMain.VisitCaustikswebsite1Click

procedure TfrmXdkTracker.VisitCxbxForum1Click(Sender: TObject);
begin
  ShellExecute(0, cOpen, cWEBSITE_FORUM, nil, nil, SW_SHOWNORMAL);
end; // TfrmMain.VisitCxbxForum1Click

procedure TfrmXdkTracker.WMCopyData(var Msg: TWMCopyData);

  procedure _HandleCopyDataString(CopyDataStruct: PCopyDataStruct);
  var
    s: string;
  begin
    s := PChar(CopyDataStruct.lpData);
    if s = 'READXML' then
      LoadXBEList;
  end;

begin
  case Msg.CopyDataStruct.dwData of
    0: _HandleCopyDataString(Msg.CopyDataStruct);
  end;
  //Send something back
  msg.Result := 1;
end;

procedure TfrmXdkTracker.ImportTxtDumps1Click(Sender: TObject);
var
  TxtDumpsFolder: string;
begin
  TxtDumpsFolder := '..\..\resource\XBEDumps\';
  if SelectDirectory(TxtDumpsFolder, [], 0) then
    ImportTxtDumps(TxtDumpsFolder);
end;

procedure TfrmXdkTracker.ExportGameList1Click(Sender: TObject);
begin
  if not Assigned(frm_Publisher) then
    frm_Publisher := Tfrm_Publisher.Create(Self);

  if frm_Publisher.ShowModal = mrOk then
    if ExportDialog.Execute then
      SaveXBEList(ExportDialog.FileName, frm_Publisher.edtPublisher.Text);

  // Note : frm_Publisher is kept around, so Destroy can use it!
end; // TfrmMain.ExportGameList1Click

procedure TfrmXdkTracker.ImportGameList1Click(Sender: TObject);
begin
  // Import another gamedata (next to the already loaded version)
  if ImportDialog.Execute then
    LoadXBEList(
      ImportDialog.FileName,
      {aDeleteAfterRead=}False,
      {aUseImportDialog=}True);
end; // TfrmMain.ImportGameList1Click

procedure TfrmXdkTracker.GetXDKInfofromXbe1Click(Sender: TObject);
var
  m_Xbe: TXbe;
  m_XbeFileName: string;
  m_ExeFileName: string;
  FilePath: string;
begin
  XbeOpenDialog.Filter := DIALOG_FILTER_XBE;
  if not XbeOpenDialog.Execute then
    Exit;

  m_Xbe := nil;
  // First, open the XBE :
  if OpenXbe(XbeOpenDialog.FileName, {var}m_Xbe, {var}m_ExeFileName, {var}m_XbeFileName ) then
  begin
    FilePath := ExtractFilePath(Application.ExeName) + 'Dump.dat';
    // Then dump the info to a temporary file :
    DxbxXml.CreateXmlXbeDump(FilePath, m_Xbe);
    // And finally import that file into the current list :
    LoadXBEList(FilePath);
  end;
end;

procedure TfrmXdkTracker.Viewxdkversion2Click(Sender: TObject);
var
  i, j: Integer;
  XDKlist: TStringList;
  XDKInfo: TXBEInfo;
begin
  frm_xdkversion := Tfrm_xdkversion.Create(Application);
  XDKlist := TStringList.Create;
  try
    XDKlist.Clear;
    XDKlist.Duplicates := dupIgnore;
    XDKlist.Sorted := True;
    with frm_XdkVersion do
    begin
      lst_Games.Clear;
      for i := 0 to MyXBEList.Count - 1 do
      begin
        XDKInfo := TXBEInfo(MyXBEList.Objects[i]);
        for j := 0 to XDKInfo.LibVersions.Count - 1 do
          XDKlist.Add(XDKInfo.LibVersions.ValueFromIndex[j]);
      end;

      cmb_gametype.Items.Clear;
      cmb_gametype.Items.Add('All XDK Versions');
      for i := 0 to XDKlist.Count - 1 do
        if XDKlist.Strings[i] <> '' then
          cmb_gametype.Items.Add(XDKlist.Strings[i]);

      cmb_gametype.ItemIndex := 0;
    end;

    frm_XdkVersion.FillGameList(MyXBEList);

    if frm_XdkVersion.ShowModal = mrOk then
    begin
    end;
  finally
    FreeAndNil(XDKlist);
    
    frm_XdkVersion.Release;
    frm_XdkVersion := nil;
  end;
end; // TfrmMain.Viewxdkversion2Click

function _ReadGameFromNode(const GameNode: IXmlNode): TXBEInfo;
var
  XDKNode, LibNode: IXmlNode;
begin
  Result := TXBEInfo.Create;
  Result.FileName := XML_ReadString(GameNode, 'FileName');
  if Result.FileName = '' then
  begin
    // Old-style 'Name' values are read here :
    Result.FileName := XML_ReadString(GameNode, 'Name');
    Result.DumpInfo := '';
    Result.Title := Result.FileName;
  end
  else
  begin
    Result.DumpInfo := XML_ReadString(GameNode, 'DumpInfo');
    Result.FileName := XML_ReadString(GameNode, 'FileName');
    Result.Title := XML_ReadString(GameNode, 'Title');
  end;

  XDKNode := GameNode.ChildNodes.FindNode('XDKVersions');
  if Assigned(XDKNode) then
  begin
    LibNode := XDKNode.ChildNodes.First;
    while Assigned(LibNode) do
    begin
      Result.LibVersions.Values[LibNode.LocalName] := LibNode.Text;
      LibNode := LibNode.NextSibling;
    end;

    Result.LibVersions.Sort;
  end;
end;

function TfrmXdkTracker.FindByFileName(const aFileName: string): Integer;
begin
  for Result := 0 to MyXBEList.Count - 1 do
    if TXBEInfo(MyXBEList.Objects[Result]).FileName = aFileName then
      Exit;

  Result := -1;
end; // TfrmMain.FindByFileName

function TfrmXdkTracker.InsertXBEInfo(const aXBEInfo: TXBEInfo): Boolean;
var
  i: Integer;
begin
  Result := False;
  if not Assigned(aXBEInfo) then
    Exit;

  i := FindByFileName(aXbeInfo.FileName);
  if i >= 0 then
  begin
    // Replace existing :
    MyXBEList.Objects[i].Free;
    MyXBEList.Objects[i] := aXBEInfo;
    MyXBEList.Strings[i] := aXBEInfo.Title;
  end
  else
    MyXBEList.AddObject(aXBEInfo.Title, aXBEInfo);

  Result := True;
end; // TfrmMain.InsertXBEInfo

function TfrmXdkTracker.ShowImportList(const XBEImportList: TList; Publisher: string): Integer;
var
  i: Integer;
  LibNames: TStringList;
  Line: TListItem;
  XDKInfo: TXBEInfo;
  j: Integer;
begin
  Result := 0;

  frm_ImportGames := Tfrm_ImportGames.Create(Self);
  LibNames := TStringList.Create;
  try
    // Build up a list of all libraries :
    LibNames.Sorted := True;
    LibNames.Duplicates := dupIgnore;
    for i := 0 to XBEImportList.Count - 1 do
    begin
      XDKInfo := TXBEInfo(XBEImportList.Items[i]);
      for j := 0 to XDKInfo.LibVersions.Count - 1 do
        LibNames.Add(XDKInfo.LibVersions.Names[j]);
    end;

    frm_ImportGames.edt_Publisher.Text := Publisher;

    frm_ImportGames.lst_Import.Columns.Clear;
    with frm_ImportGames.lst_Import.Columns.Add do
    begin
      Caption := 'Title';
      Width := 250;
    end;

    with frm_ImportGames.lst_Import.Columns.Add do
    begin
      Caption := 'Dumped with';
      Width := 250;
    end;

    with frm_ImportGames.lst_Import.Columns.Add do
    begin
      Caption := 'Filename';
      Width := 250;
    end;

    for j := 0 to LibNames.Count - 1 do
      with frm_ImportGames.lst_Import.Columns.Add do
      begin
        Caption := LibNames[j];
        Width := 75;
      end;

    for i := 0 to XBEImportList.Count - 1 do
    begin
      XDKInfo := TXBEInfo(XBEImportList.Items[i]);

      Line := frm_ImportGames.lst_Import.Items.Add;
      Line.Data := XBEImportList[i];
      Line.Caption := XDKInfo.Title;
      Line.Checked := FindByFileName(XDKInfo.FileName) < 0; // uncheck when already present
      Line.SubItems.Add(XDKInfo.DumpInfo);
      Line.SubItems.Add(XDKInfo.FileName);
      for j := 0 to LibNames.Count - 1 do
        Line.SubItems.Add(XDKInfo.LibVersions.Values[LibNames[j]]);
    end;

    if frm_ImportGames.ShowModal = mrOk then
    begin
      for i := 0 to frm_ImportGames.lst_Import.Items.Count - 1 do
        if  frm_ImportGames.lst_Import.Items[i].Checked
        and InsertXBEInfo(TXBEInfo(frm_ImportGames.lst_Import.Items[i].Data)) then
          Inc(Result);

      MyXBEList.Sort;
    end;
  finally
    FreeAndNil(LibNames);
    
    frm_ImportGames.Release;
    frm_ImportGames := nil;
  end;
end; // ShowImportList

procedure TfrmXdkTracker.SaveXBEList(const aFilePath, aPublishedBy: string);
var
  XmlRootNode: IXmlNode;
  PublishedNode: IXmlNode;
  GameListNode: IXmlNode;
  GameNode: IXmlNode;
  XDKnode: IXmlNode;
  i, j: Integer;
  XDKInfo: TXBEInfo;
begin
  if not XMLDocument.Active then
    Exit;

  XMLDocument.ChildNodes.Clear;
  XmlRootNode := XMLDocument.AddChild('XDKInfo');
  XmlRootNode.SetAttribute('Version', cXDk_TRACKER_XML_VERSION);

  PublishedNode := XmlRootNode.AddChild('PublishedInfo');
  XML_WriteString(PublishedNode, 'PublishedBy', aPublishedBy);

  GameListNode := XmlRootNode.AddChild('GameList');

  for i := 0 to MyXBEList.Count - 1 do
  begin
    XDKInfo := TXBEInfo(MyXBEList.Objects[i]);
    GameNode := GameListNode.AddChild('Game');

    XML_WriteString(GameNode, 'DumpInfo', XDKInfo.DumpInfo);
    XML_WriteString(GameNode, 'FileName', XDKInfo.FileName);
    XML_WriteString(GameNode, 'Title', XDKInfo.Title);
    XDKnode := GameNode.AddChild('XDKVersions');

    for j := 0 to XDKInfo.LibVersions.Count - 1 do
      XML_WriteString(XDKnode, XDKInfo.LibVersions.Names[j], XDKInfo.LibVersions.ValueFromIndex[j]);
  end;

  XMLDocument.SaveToFile(aFilePath);
end; // TfrmMain.SaveXBEList

function TfrmXdkTracker.LoadXBEList(
  aImportFilePath: string = '';
  aDeleteAfterRead: Boolean = True;
  aUseImportDialog: Boolean = False): Integer;
var
  xmlRootNode: IXmlNode;
  InfoNode: IXmlNode;
  GameNode: IXmlNode;
  Publisher: string;
  FileName: string;
  XBEImportList: TList;
  i: Integer;
begin
  Result := 0;
  if aImportFilePath = '' then
    aImportFilePath := ApplicationDir + 'Dump.dat';

  if not FileExists(aImportFilePath) then
    Exit;

  XmlDocument.Active := False;
  XmlDocument.FileName := aImportFilePath;
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

  if not XmlDocument.Active then
    Exit;

  XBEImportList := TList.Create;
  try
    XmlRootNode := XMLDocument.DocumentElement;

    InfoNode := XmlRootNode.ChildNodes.FindNode('PublishedInfo');
    if Assigned(InfoNode) then
      Publisher := XML_ReadString(InfoNode, 'PublishedBy');

    InfoNode := XmlRootNode.ChildNodes.FindNode('GameList');
    if Assigned(InfoNode) then
      GameNode := InfoNode.ChildNodes.First
    else
      GameNode := XmlRootNode;

    while Assigned(GameNode) do
    begin
      FileName := XML_ReadString(GameNode, 'FileName');
      if FileName = '' then
        // Old-style 'Name' values are read here, interpreted as FileName :
        FileName := XML_ReadString(GameNode, 'Name');

      // For now, only add to list when the user can intervene,
      // or when not yet present :
      if aUseImportDialog
      or (FindByFileName(FileName) < 0) then
        XBEImportList.Add(_ReadGameFromNode(GameNode));

      GameNode := GameNode.NextSibling;
    end;

    if aUseImportDialog then
      Result := ShowImportList(XBEImportList, Publisher)
    else
      for i := 0 to XBEImportList.Count - 1 do
        if InsertXBEInfo(XBEImportList[i]) then
          Inc(Result);

    MyXBEList.Sort;

    if aDeleteAfterRead then
      DeleteFile(aImportFilePath);

  finally
    FreeAndNil({var}XBEImportList);
  end;
end; // TfrmMain.LoadXBEList

// Imports all Xbe dumps from a folder.
procedure TfrmXdkTracker.ImportTxtDumps(const aTxtDumpsFolder: TFileName);
var
  FileNames, FileContents: TStringList;
  i, j, k: Integer;
  FileName, LibName, LibVersion: string;
  XDKInfo: TXBEInfo;
  XBEImportList: TList;
begin
  FileNames := TStringList.Create;
  FileContents := TStringList.Create;
  XBEImportList := TList.Create;
  try
    FindFiles(aTxtDumpsFolder, '*.txt', FileNames);

    for i := 0 to FileNames.Count - 1 do
    begin
      FileName := ChangeFileExt(ExtractFileName(FileNames[i]), '');

      FileContents.LoadFromFile(FileNames[i]);

      XDKInfo := TXBEInfo.Create;
      XDKInfo.FileName := FileName;

      // Read DumpInfo :
      j := 0;
      k := Pos('by ', FileContents[j]);
      if k > 0 then
        XDKInfo.DumpInfo := Copy(FileContents[j], k + Length('by '), MaxInt)
      else
        XDKInfo.DumpInfo := '';

      Inc(j);
      while j < FileContents.Count do
      begin
        if StartsWithText(FileContents[j], 'Title identified as "') then
        begin
          XDKInfo.Title := Copy(FileContents[j], Length('Title identified as "') + 1, Length(FileContents[j]) - Length('Title identified as "') - 1);
        end
        else
        if StartsWithText(FileContents[j], 'Library Name') then
        begin
          k := Pos(':', FileContents[j]);
          LibName := Trim(Copy(FileContents[j], k + 1, MaxInt));
          Inc(j);
          k := Pos(':', FileContents[j]);
          LibVersion := Trim(Copy(FileContents[j], k + 1, MaxInt));
          XDKInfo.LibVersions.Values[LibName] := LibVersion;
        end;

        Inc(j);
      end;

      // Some XBE's have "This XBE contains no Library Versions" inside...
      XDKInfo.LibVersions.Sort;
      XBEImportList.Add(XDKInfo);
    end; // for FileNames

    ShowImportList(XBEImportList, '');

  finally
    FreeAndNil(XBEImportList);
    FreeAndNil(FileContents);
    FreeAndNil(FileNames);
  end;
end;

//

var
  MutexHandle: THandle;

initialization

  MutexHandle := CreateMutex(nil, True, 'XYZ');
  if GetLastError = ERROR_ALREADY_EXISTS then
  begin
    ShowMessage('Program is already running!');
    Halt;
  end;

finalization

  if MutexHandle <> 0 then
    CloseHandle(MutexHandle);

end.

