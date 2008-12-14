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
    function FindDuplicate(const aXBEInfo: TXBEInfo): Integer;
    function FindByFileName(const aFileName: string): Integer;
    function InsertXBEInfo(const aXbeInfo: TXBEInfo): Boolean;
    procedure SaveXBEList(const aFilePath, aPublishedBy: string);
    function ShowImportList(const XBEImportList: TStringList; Publisher: string): Integer;
    function LoadXBEList(aImportFilePath: string = ''; aUseImportDialog: Boolean = False): Integer;
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
  MyXBEList.CaseSensitive := False;
  if LoadXBEList(ApplicationDir + cXDK_TRACKER_DATA_FILE) > 0 then
    StatusBar1.SimpleText := cXDKLIST_LOADED;

  Parameter := ParamStr(1);
  if SameText(Parameter, '/XBEDUMP') then
    LoadXBEList();

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
      LoadXBEList();
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
    LoadXBEList(ImportDialog.FileName, {aUseImportDialog=}True);
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
  if OpenXbe(XbeOpenDialog.FileName, {var}m_Xbe, {var}m_ExeFileName, {var}m_XbeFileName) then
  begin
    FilePath := ExtractFilePath(Application.ExeName) + 'Dump.dat';
    // Then dump the info to a temporary file :
    DxbxXml.CreateXmlXbeDump(FilePath, m_Xbe);
    // And finally import that file into the current list :
    LoadXBEList(FilePath);
    // Delete dump file after reading it :
    DeleteFile(FilePath);
  end;
end;

procedure TfrmXdkTracker.Viewxdkversion2Click(Sender: TObject);
begin
  frm_XBEList := Tfrm_XBEList.Create(Self);
  try
(*
    // Precalculate the IsDuplicate members (this will be used to feed the checkboxes) :
    for i := 0 to MyXBEList.Count - 1 do
    begin
      XBEInfo := TXBEInfo(MyXBEList.Objects[i]);
      XBEInfo.IsDuplicate := False;
    end;
*)
    // Put this list into the view :
    frm_XBEList.FillXBEList(MyXBEList, {ShowAsImport=}False);

    if frm_XBEList.ShowModal = mrOk then
    begin
    end;
  finally
    frm_XBEList.Release;
    frm_XBEList := nil;
  end;
end; // TfrmMain.Viewxdkversion2Click

function _ReadXBEInfoFromNode(const XBEInfoNode: IXMLNode): TXBEInfo;
var
  XDKNode, LibNode: IXMLNode;
begin
  Result := TXBEInfo.Create;
  Result.FileName := XML_ReadString(XBEInfoNode, 'FileName');
  if Result.FileName = '' then
  begin
    // Old-style 'Name' values are read here :
    Result.Title := XML_ReadString(XBEInfoNode, 'Name');
    Result.DumpInfo := '';
    Result.GameRegion := 0;
  end
  else
  begin
    Result.Title := XML_ReadString(XBEInfoNode, 'Title');
    Result.GameRegion := StrToIntDef(XML_ReadString(XBEInfoNode, 'GameRegion'), 0);
    Result.DumpInfo := XML_ReadString(XBEInfoNode, 'DumpInfo');
  end;

  XDKNode := XBEInfoNode.ChildNodes.FindNode('XDKVersions');
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

function TfrmXdkTracker.FindDuplicate(const aXBEInfo: TXBEInfo): Integer;
begin
  // First, check title :
  Result := MyXBEList.IndexOf(aXBEInfo.Title);

  // For backwards compatibility, try FileName too :
  if Result < 0 then
    Result := MyXBEList.IndexOf(aXBEInfo.FileName);

  // If found by title
  if Result >= 0 then
  begin
    // Then check region :
    if TXBEInfo(MyXBEList.Objects[Result]).GameRegion = aXBEInfo.GameRegion then
      // It seems this is the same dump, return this :
      Exit;
  end;

  // No match, try searching by filename as default :
  if aXBEInfo.FileName <> '' then
    Result := FindByFileName(aXBEInfo.FileName);

  if Result < 0 then
    // Still no match, try searching by filename, but use Title as last resort : 
    Result := FindByFileName(aXBEInfo.Title);
end;

function TfrmXdkTracker.FindByFileName(const aFileName: string): Integer;
begin
  for Result := 0 to MyXBEList.Count - 1 do
    if SameText(TXBEInfo(MyXBEList.Objects[Result]).FileName, aFileName) then
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

  i := FindDuplicate(aXbeInfo);
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

function TfrmXdkTracker.ShowImportList(const XBEImportList: TStringList; Publisher: string): Integer;
var
  i: Integer;
  XBEInfo: TXBEInfo;
begin
  Result := 0;

  frm_XBEList := Tfrm_XBEList.Create(Self);
  try
    // Precalculate the IsDuplicate members (this will be used to feed the checkboxes) :
    for i := 0 to XBEImportList.Count - 1 do
    begin
      XBEInfo := TXBEInfo(XBEImportList.Objects[i]);
      XBEInfo.IsDuplicate := FindDuplicate(XBEInfo) >= 0;
    end;

    // Put this list into the view :
    frm_XBEList.FillXBEList(XBEImportList, {ShowAsImport=}True);

    if frm_XBEList.ShowModal = mrOk then
    begin
      for i := 0 to frm_XBEList.lst_XBEs.Items.Count - 1 do
        if  frm_XBEList.lst_XBEs.Items[i].Checked
        and InsertXBEInfo(TXBEInfo(frm_XBEList.lst_XBEs.Items[i].Data)) then
          Inc(Result);

      MyXBEList.Sort;
    end;
  finally
    frm_XBEList.Release;
    frm_XBEList := nil;
  end;
end; // ShowImportList

procedure TfrmXdkTracker.SaveXBEList(const aFilePath, aPublishedBy: string);
var
  XMLRootNode: IXMLNode;
  PublishedNode: IXMLNode;
  GameListNode: IXMLNode;
  XBEInfoNode: IXMLNode;
  XDKnode: IXMLNode;
  i, j: Integer;
  XBEInfo: TXBEInfo;
begin
  if not XMLDocument.Active then
    Exit;

  XMLDocument.ChildNodes.Clear;
  XMLRootNode := XMLDocument.AddChild('XBEInfo');
  XMLRootNode.SetAttribute('Version', cXDk_TRACKER_XML_VERSION);

  PublishedNode := XMLRootNode.AddChild('PublishedInfo');
  XML_WriteString(PublishedNode, 'PublishedBy', aPublishedBy);

  GameListNode := XMLRootNode.AddChild('GameList');

  for i := 0 to MyXBEList.Count - 1 do
  begin
    XBEInfo := TXBEInfo(MyXBEList.Objects[i]);
    XBEInfoNode := GameListNode.AddChild('Game');

    XML_WriteString(XBEInfoNode, 'FileName', XBEInfo.FileName);
    XML_WriteString(XBEInfoNode, 'Title', XBEInfo.Title);
    XML_WriteString(XBEInfoNode, 'GameRegion', IntToStr(XBEInfo.GameRegion));
    XML_WriteString(XBEInfoNode, 'DumpInfo', XBEInfo.DumpInfo);
    XDKnode := XBEInfoNode.AddChild('XDKVersions');

    for j := 0 to XBEInfo.LibVersions.Count - 1 do
      XML_WriteString(XDKnode, XBEInfo.LibVersions.Names[j], XBEInfo.LibVersions.ValueFromIndex[j]);
  end;

  XMLDocument.SaveToFile(aFilePath);
end; // TfrmMain.SaveXBEList

function TfrmXdkTracker.LoadXBEList(aImportFilePath: string = '';
  aUseImportDialog: Boolean = False): Integer;
var
  XMLRootNode: IXMLNode;
  XMLNode: IXMLNode;
  XBEInfoNode: IXMLNode;
  Publisher: string;
  FileName: string;
  XBEImportList: TStringList;
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

  XBEImportList := TStringList.Create;
  try
    XMLRootNode := XMLDocument.DocumentElement;

    XMLNode := XMLRootNode.ChildNodes.FindNode('PublishedInfo');
    if Assigned(XMLNode) then
      Publisher := XML_ReadString(XMLNode, 'PublishedBy');

    XMLNode := XMLRootNode.ChildNodes.FindNode('GameList');
    if Assigned(XMLNode) then
      XBEInfoNode := XMLNode.ChildNodes.First
    else
      XBEInfoNode := XMLRootNode;

    while Assigned(XBEInfoNode) do
    begin
      FileName := XML_ReadString(XBEInfoNode, 'FileName');
      if FileName = '' then
        // Old-style 'Name' values are read here, interpreted as FileName :
        FileName := XML_ReadString(XBEInfoNode, 'Name');

      // For now, only add to list when the user can intervene,
      // or when not yet present :
      if aUseImportDialog
      or (FindByFileName(FileName) < 0) then
        XBEImportList.AddObject('', _ReadXBEInfoFromNode(XBEInfoNode));

      XBEInfoNode := XBEInfoNode.NextSibling;
    end;

    if aUseImportDialog then
      Result := ShowImportList(XBEImportList, Publisher)
    else
      for i := 0 to XBEImportList.Count - 1 do
        if InsertXBEInfo(TXBEInfo(XBEImportList.Objects[i])) then
          Inc(Result);

    MyXBEList.Sort;

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
  XBEInfo: TXBEInfo;
  XBEImportList: TStringList;
begin
  FileNames := TStringList.Create;
  FileContents := TStringList.Create;
  XBEImportList := TStringList.Create;
  try
    FindFiles(aTxtDumpsFolder, '*.txt', FileNames);

    for i := 0 to FileNames.Count - 1 do
    begin
      FileName := ChangeFileExt(ExtractFileName(FileNames[i]), '');

      FileContents.LoadFromFile(FileNames[i]);

      XBEInfo := TXBEInfo.Create;
      XBEInfo.FileName := FileName;

      // Read DumpInfo :
      j := 0;
      k := Pos('by ', FileContents[j]);
      if k > 0 then
        XBEInfo.DumpInfo := Copy(FileContents[j], k + Length('by '), MaxInt)
      else
        XBEInfo.DumpInfo := '';

      Inc(j);
      while j < FileContents.Count do
      begin
        if StartsWithText(FileContents[j], 'Title identified as "') then
        begin
          XBEInfo.Title := Copy(FileContents[j], Length('Title identified as "') + 1, Length(FileContents[j]) - Length('Title identified as "') - 1);
        end
        else
        if StartsWithText(FileContents[j], 'Game Region') then
        begin
          ScanHexDWord(PAnsiChar(Copy(FileContents[j], Length(FileContents[j]) - 7, 8)), {var}XBEInfo.GameRegion);
        end
        else
        if StartsWithText(FileContents[j], 'Library Name') then
        begin
          k := Pos(':', FileContents[j]);
          LibName := Trim(Copy(FileContents[j], k + 1, MaxInt));
          Inc(j);
          k := Pos(':', FileContents[j]);
          LibVersion := Trim(Copy(FileContents[j], k + 1, MaxInt));
          XBEInfo.LibVersions.Values[LibName] := LibVersion;
        end;

        Inc(j);
      end;

      // Some XBE's have "This XBE contains no Library Versions" inside...
      XBEInfo.LibVersions.Sort;
      XBEImportList.AddObject('', XBEInfo);
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

