(*
    This file is part of Dxbx - a XBox emulator written in Delphi (ported over from cxbx)
    Copyright (C) 2007 Shadow_tj and other members of the development team.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)
unit uXBEExplorerMain;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, Grids, ExtCtrls,
  Math, // Min
  // Dxbx
  uConsts,
  uTypes,
  uXbe,
  uHexViewer;

type
  TStringArray = array of string;

  TFormXBEExplorer = class(TForm)
    MainMenu1: TMainMenu;
    TreeView1: TTreeView;
    PageControl1: TPageControl;
    File1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    OpenDialog1: TOpenDialog;
    Close1: TMenuItem;
    procedure Open1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
  protected
    MyXBE: TXbe;
    FXBEFileName: string;
    procedure CloseXBE;
    procedure GridAddRow(const aStringGrid: TStringGrid; const aStrings: array of string);
    function NewGrid(const aFixedCols: Integer; const aTitles: array of string): TStringGrid;
    procedure SectionClick(Sender: TObject);
    procedure OpenXBE(const aFilePath: string);
  public
    destructor Destroy; override;
  end;

var
  FormXBEExplorer: TFormXBEExplorer;

implementation

type
  TStringsViewer = class(THexViewer);

{$R *.dfm}

destructor TFormXBEExplorer.Destroy;
begin
  CloseXBE;

  inherited Destroy;
end;

procedure TFormXBEExplorer.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TFormXBEExplorer.Close1Click(Sender: TObject);
begin
  CloseXBE;
end;

procedure TFormXBEExplorer.CloseXBE;
begin
  FreeAndNil(MyXBE);
  TreeView1.Items.Clear;
  Caption := Application.Title;
  PageControl1.Visible := False;
  while PageControl1.PageCount > 0 do
    PageControl1.Pages[0].Free;
end;

procedure TFormXBEExplorer.Open1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    OpenXBE(OpenDialog1.FileName);
end;

procedure TFormXBEExplorer.GridAddRow(const aStringGrid: TStringGrid; const aStrings: array of string);
var
  i, w: Integer;
  Row: TStrings;
begin
  i := 0;
  while (i <= aStringGrid.FixedRows) and (aStringGrid.Cells[0, i] <> '') do
    Inc(i);

  if i > aStringGrid.FixedRows then
  begin
    i := aStringGrid.RowCount;
    aStringGrid.RowCount := aStringGrid.RowCount + 1;
  end;

  Row := aStringGrid.Rows[i];
  for i := 0 to Length(aStrings) - 1 do
  begin
    Row[i] := aStrings[i];
    w := {aStringGrid.}Canvas.TextWidth(aStrings[i]) + 5;
    if aStringGrid.ColWidths[i] < w then
      aStringGrid.ColWidths[i] := w
  end;
end;

function _DWORD(const aValue: DWORD): string;
begin
  Result := IntToHex(aValue, 8);
end;

procedure TFormXBEExplorer.SectionClick(Sender: TObject);
var
  Grid: TStringGrid;
  i: Integer;
  Hdr: PXbeSectionHeader;
begin
  if Sender is TStringGrid then
  else
    Exit;

  Grid := TStringGrid(Sender);
  i := Grid.Row - Grid.FixedRows;
  Hdr := @(MyXBE.m_SectionHeader[i]);
  i := (i * SizeOf(TXbeSectionHeader)) + MyXBE.m_Header.dwSectionHeadersAddr - MyXBE.m_Header.dwBaseAddr;

  Grid.Cells[1, 1] :=  _Dword(i + FIELD_OFFSET(PXbeSectionHeader(nil).dwFlags));
  Grid.Cells[2, 1] :=  _Dword(i + FIELD_OFFSET(PXbeSectionHeader(nil).dwVirtualAddr));
  Grid.Cells[3, 1] :=  _Dword(i + FIELD_OFFSET(PXbeSectionHeader(nil).dwVirtualSize));
  Grid.Cells[4, 1] :=  _Dword(i + FIELD_OFFSET(PXbeSectionHeader(nil).dwRawAddr));
  Grid.Cells[5, 1] :=  _Dword(i + FIELD_OFFSET(PXbeSectionHeader(nil).dwSizeofRaw));
  Grid.Cells[6, 1] :=  _Dword(i + FIELD_OFFSET(PXbeSectionHeader(nil).dwSectionNameAddr));
  Grid.Cells[7, 1] :=  _Dword(i + FIELD_OFFSET(PXbeSectionHeader(nil).dwSectionRefCount));
  Grid.Cells[8, 1] :=  _Dword(i + FIELD_OFFSET(PXbeSectionHeader(nil).dwHeadSharedRefCountAddr));
  Grid.Cells[9, 1] :=  _Dword(i + FIELD_OFFSET(PXbeSectionHeader(nil).dwTailSharedRefCountAddr));
  Grid.Cells[10, 1] :=  _Dword(i + FIELD_OFFSET(PXbeSectionHeader(nil).bzSectionDigest));

  THexViewer(TStringGrid(Sender).Tag).SetRegion(@MyXBE.RawData[Hdr.dwRawAddr], Hdr.dwSizeofRaw);
end;

function TFormXBEExplorer.NewGrid(const aFixedCols: Integer; const aTitles: array of string): TStringGrid;
var
  i: Integer;
begin
  Result := TStringGrid.Create(Self);
  Result.RowCount := 2;
  Result.DefaultRowHeight := Canvas.TextHeight('Wg') + 5;
  Result.ColCount := Length(aTitles);
  Result.FixedCols := aFixedCols;
  Result.Options := Result.Options + [goColSizing] - [goRowSizing, goRangeSelect];
  GridAddRow(Result, aTitles);
  for i := 0 to Length(aTitles) - 1 do
    Result.Cells[i, 0] := string(aTitles[i]);
end;

procedure TFormXBEExplorer.OpenXBE(const aFilePath: string);
var
  NodeResources,
  Level0, NodeXBEHeader: TTreeNode;

  function _CreateGrid_File: TStringGrid;
  begin
    Result := NewGrid(1, ['Property', 'Value']);
    GridAddRow(Result, ['File Name', aFilePath]);
    GridAddRow(Result, ['File Type', 'XBE File']);
    GridAddRow(Result, ['File Size', IntToStr(MyXBE.FileSize) + ' bytes']);
  end;

  function _Offset(var aValue; const aBaseAddr: DWord = 0): string;
  begin
    Result := _DWord(aBaseAddr + FIELD_OFFSET(aValue));
  end;

  function _Initialize_XBEHeader: TStringGrid;
  var
    Hdr: PXbeHeader;
  begin
    Hdr := @(MyXBE.m_Header);
    Result := NewGrid(3, ['Member', 'Offset', 'Size', 'Value', 'Meaning']);
    GridAddRow(Result, ['dwMagic', _offset(PXbeHeader(nil).dwMagic), 'Char[4]', Hdr.dwMagic]);
    GridAddRow(Result, ['pbDigitalSignature', _offset(PXbeHeader(nil).pbDigitalSignature), 'Byte[256]', PByteToHexString(@Hdr.pbDigitalSignature[0], 16) + '...']);
    GridAddRow(Result, ['dwBaseAddr', _offset(PXbeHeader(nil).dwBaseAddr), 'Dword', _DWORD(Hdr.dwBaseAddr)]);
    GridAddRow(Result, ['dwSizeofHeaders', _offset(PXbeHeader(nil).dwSizeofHeaders), 'Dword', _DWORD(Hdr.dwSizeofHeaders)]);
    GridAddRow(Result, ['dwSizeofImage', _offset(PXbeHeader(nil).dwSizeofImage), 'Dword', _DWORD(Hdr.dwSizeofImage)]);
    GridAddRow(Result, ['dwSizeofImageHeader', _offset(PXbeHeader(nil).dwSizeofImageHeader), 'Dword', _DWORD(Hdr.dwSizeofImageHeader)]);
    GridAddRow(Result, ['dwTimeDate', _offset(PXbeHeader(nil).dwTimeDate), 'Dword', _DWORD(Hdr.dwTimeDate), BetterTime(Hdr.dwTimeDate)]);
    GridAddRow(Result, ['dwCertificateAddr', _offset(PXbeHeader(nil).dwCertificateAddr), 'Dword', _DWORD(Hdr.dwCertificateAddr)]);
    GridAddRow(Result, ['dwSections', _offset(PXbeHeader(nil).dwSections), 'Dword', _DWORD(Hdr.dwSections)]);
    GridAddRow(Result, ['dwSectionHeadersAddr', _offset(PXbeHeader(nil).dwSectionHeadersAddr), 'Dword', _DWORD(Hdr.dwSectionHeadersAddr)]);
    GridAddRow(Result, ['dwInitFlags', _offset(PXbeHeader(nil).dwInitFlags), 'Dword', PByteToHexString(@Hdr.dwInitFlags[0], 4)]);
    GridAddRow(Result, ['dwEntryAddr', _offset(PXbeHeader(nil).dwEntryAddr), 'Dword', _DWORD(Hdr.dwEntryAddr), Format('Retail: 0x%.8x, Debug: 0x%.8x', [Hdr.dwEntryAddr xor XOR_EP_Retail, Hdr.dwEntryAddr xor XOR_EP_DEBUG])]);
    GridAddRow(Result, ['dwTLSAddr', _offset(PXbeHeader(nil).dwTLSAddr), 'Dword', _DWORD(Hdr.dwTLSAddr)]);
    GridAddRow(Result, ['dwPeStackCommit', _offset(PXbeHeader(nil).dwPeStackCommit), 'Dword', _DWORD(Hdr.dwPeStackCommit)]);
    GridAddRow(Result, ['dwPeHeapReserve', _offset(PXbeHeader(nil).dwPeHeapReserve), 'Dword', _DWORD(Hdr.dwPeHeapReserve)]);
    GridAddRow(Result, ['dwPeHeapCommit', _offset(PXbeHeader(nil).dwPeHeapCommit), 'Dword', _DWORD(Hdr.dwPeHeapCommit)]);
    GridAddRow(Result, ['dwPeBaseAddr', _offset(PXbeHeader(nil).dwPeBaseAddr), 'Dword', _DWORD(Hdr.dwPeBaseAddr)]);
    GridAddRow(Result, ['dwPeSizeofImage', _offset(PXbeHeader(nil).dwPeSizeofImage), 'Dword', _DWORD(Hdr.dwPeSizeofImage)]);
    GridAddRow(Result, ['dwPeChecksum', _offset(PXbeHeader(nil).dwPeChecksum), 'Dword', _DWORD(Hdr.dwPeChecksum)]);
    GridAddRow(Result, ['dwPeTimeDate', _offset(PXbeHeader(nil).dwPeTimeDate), 'Dword', _DWORD(Hdr.dwPeTimeDate), BetterTime(Hdr.dwPeTimeDate)]);
    GridAddRow(Result, ['dwDebugPathNameAddr', _offset(PXbeHeader(nil).dwDebugPathNameAddr), 'Dword', _DWORD(Hdr.dwDebugPathNameAddr), MyXBE.GetAddrStr(Hdr.dwDebugPathNameAddr)]);
    GridAddRow(Result, ['dwDebugFileNameAddr', _offset(PXbeHeader(nil).dwDebugFileNameAddr), 'Dword', _DWORD(Hdr.dwDebugFileNameAddr), MyXBE.GetAddrStr(Hdr.dwDebugFileNameAddr)]);
    GridAddRow(Result, ['dwDebugUnicodeFileNameAddr', _offset(PXbeHeader(nil).dwDebugUnicodeFileNameAddr), 'Dword', _DWORD(Hdr.dwDebugUnicodeFileNameAddr)]);
    GridAddRow(Result, ['dwKernelImageThunkAddr', _offset(PXbeHeader(nil).dwKernelImageThunkAddr), 'Dword', _DWORD(Hdr.dwKernelImageThunkAddr), Format('Retail: 0x%.8x, Debug: 0x%.8x', [Hdr.dwKernelImageThunkAddr xor XOR_KT_RETAIL, Hdr.dwKernelImageThunkAddr xor XOR_KT_DEBUG])]);
    GridAddRow(Result, ['dwNonKernelImportDirAddr', _offset(PXbeHeader(nil).dwNonKernelImportDirAddr), 'Dword', _DWORD(Hdr.dwNonKernelImportDirAddr)]);
    GridAddRow(Result, ['dwLibraryVersions', _offset(PXbeHeader(nil).dwLibraryVersions), 'Dword', _DWORD(Hdr.dwLibraryVersions)]);
    GridAddRow(Result, ['dwLibraryVersionsAddr', _offset(PXbeHeader(nil).dwLibraryVersionsAddr), 'Dword', _DWORD(Hdr.dwLibraryVersionsAddr)]);
    GridAddRow(Result, ['dwKernelLibraryVersionAddr', _offset(PXbeHeader(nil).dwKernelLibraryVersionAddr), 'Dword', _DWORD(Hdr.dwKernelLibraryVersionAddr)]);
    GridAddRow(Result, ['dwXAPILibraryVersionAddr', _offset(PXbeHeader(nil).dwXAPILibraryVersionAddr), 'Dword', _DWORD(Hdr.dwXAPILibraryVersionAddr)]);
    GridAddRow(Result, ['dwLogoBitmapAddr', _offset(PXbeHeader(nil).dwLogoBitmapAddr), 'Dword', _DWORD(Hdr.dwLogoBitmapAddr)]);
    GridAddRow(Result, ['dwSizeofLogoBitmap', _offset(PXbeHeader(nil).dwSizeofLogoBitmap), 'Dword', _DWORD(Hdr.dwSizeofLogoBitmap)]);
  end;

  function _Initialize_Certificate: TStringGrid;
  var
    o: DWord;
    i: Integer;
    Cert: PXbeCertificate;
  begin
    o := MyXBE.m_Header.dwCertificateAddr - MyXBE.m_Header.dwBaseAddr;
    Cert := @(MyXBE.m_Certificate);
    Result := NewGrid(3, ['Member', 'Offset', 'Size', 'Value', 'Meaning']);
    GridAddRow(Result, ['dwSize', _offset(PXbeCertificate(nil).dwSize, o), 'Dword', _DWORD(Cert.dwSize)]);
    GridAddRow(Result, ['dwTimeDate', _offset(PXbeCertificate(nil).dwTimeDate, o), 'Dword', _DWORD(Cert.dwTimeDate), BetterTime(Cert.dwTimeDate)]);
    GridAddRow(Result, ['dwTitleId', _offset(PXbeCertificate(nil).dwTitleId, o), 'Dword', _DWORD(Cert.dwTitleId)]);
    GridAddRow(Result, ['wszTitleName', _offset(PXbeCertificate(nil).wszTitleName, o), 'WChar[40]', PWideCharToString(@Cert.wszTitleName[0], 40)]);
    for i := Low(Cert.dwAlternateTitleId) to High(Cert.dwAlternateTitleId) do
      GridAddRow(Result, ['dwAlternateTitleId[' + IntToStr(i) + ']', _offset(PXbeCertificate(nil).dwAlternateTitleId[i], o), 'Dword', _DWORD(Cert.dwAlternateTitleId[i])]);
    GridAddRow(Result, ['dwAllowedMedia', _offset(PXbeCertificate(nil).dwAllowedMedia, o), 'Dword', _DWORD(Cert.dwAllowedMedia)]);
    GridAddRow(Result, ['dwGameRegion', _offset(PXbeCertificate(nil).dwGameRegion, o), 'Dword', _DWORD(Cert.dwGameRegion), GameRegionToString(Cert.dwGameRegion)]);
    GridAddRow(Result, ['dwGameRatings', _offset(PXbeCertificate(nil).dwGameRatings, o), 'Dword', _DWORD(Cert.dwGameRatings)]);
    GridAddRow(Result, ['dwDiskNumber', _offset(PXbeCertificate(nil).dwDiskNumber, o), 'Dword', _DWORD(Cert.dwDiskNumber)]);
    GridAddRow(Result, ['dwVersion', _offset(PXbeCertificate(nil).dwVersion, o), 'Dword', _DWORD(Cert.dwVersion)]);
    GridAddRow(Result, ['bzLanKey', _offset(PXbeCertificate(nil).bzLanKey, o), 'Byte[16]', PByteToHexString(@Cert.bzLanKey[0], 16)]);
    GridAddRow(Result, ['bzSignatureKey', _offset(PXbeCertificate(nil).bzSignatureKey, o), 'Byte[16]', PByteToHexString(@Cert.bzSignatureKey[0], 16)]);
    for i := Low(Cert.bzTitleAlternateSignatureKey) to High(Cert.bzTitleAlternateSignatureKey) do
      GridAddRow(Result, ['bzTitleAlternateSignatureKey[' + IntToStr(i) + ']', _offset(PXbeCertificate(nil).bzTitleAlternateSignatureKey[i], o), 'Byte[16]', PByteToHexString(@Cert.bzTitleAlternateSignatureKey[i][0], 16)]);
  end;

  function _Initialize_Stub: TPanel;
  begin
    Result := TPanel.Create(Self);
  end;

  function _Initialize_Logo: TImage;
  begin
    Result := TImage.Create(Self);
    MyXbe.ExportLogoBitmap(Result.Picture.Bitmap)
  end;

  function _Initialize_XPRSection(const aXPR_IMAGE: PXPR_IMAGE): TImage;
  begin
    Result := TImage.Create(Self);
    MyXbe.ExportXPRToBitmap(aXPR_IMAGE, Result.Picture.Bitmap);
  end;

  function _Initialize_HexViewer: THexViewer;
  begin
    Result := THexViewer.Create(Self);
    Result.SetRegion(MyXBE.RawData, MyXBE.FileSize);
  end;

  function _Initialize_Strings: TStringsViewer;
  begin
    Result := TStringsViewer.Create(Self);
    Result.SetRegion(MyXBE.RawData, MyXBE.FileSize);
  end;
  
  function _CreateNode(const aParentNode: TTreeNode; const aName: string; aContents: TControl): TTreeNode;
  var
    TabSheet: TTabSheet;
  begin
    TabSheet := TTabSheet.Create(Self);
    TabSheet.PageControl := PageControl1;
    TabSheet.TabVisible := False;
    if Assigned(aContents) then
    begin
      aContents.Parent := TabSheet;
      aContents.Align := alClient;
    end
    else
      TabSheet.Caption := 'Select subnode for more details';
    
    Result := TreeView1.Items.AddChildObject(aParentNode, aName, TabSheet);
  end;

  function _Initialize_SectionHeaders: TPanel;
  var
    Grid: TStringGrid;
    i: Integer;
    Hdr: PXbeSectionHeader;
    Name: string;
    Splitter: TSplitter;
    HexViewer: THexViewer;
  begin
    Result := TPanel.Create(Self);

    Grid := NewGrid(0, ['Name', 'Flags', 'Virtual Address', 'Virtual Size',
      'Raw Address', 'Raw Size', 'SectionNamAddr', 'dwSectionRefCount',
      'dwHeadSharedRefCountAddr', 'dwTailSharedRefCountAddr', 'bzSectionDigest']);
    Grid.Parent := Result;
    Grid.Align := alTop;
    Grid.Height := Height div 2;
    Grid.RowCount := 4;
    Grid.Options := Grid.Options + [goRowSelect];
    Grid.FixedRows := 3;
    GridAddRow(Grid, [' ']); // Put space in offset-row
    GridAddRow(Grid, [' ', 'Dword', 'Dword', 'Dword', 'Dword', 'Dword', 'Dword', 'Dword', 'Dword', 'Dword', 'Byte[20]']);

    for i := 0 to Length(MyXBE.m_SectionHeader) - 1 do
    begin
      Hdr := @(MyXBE.m_SectionHeader[i]);
      Name := MyXBE.GetAddrStr(Hdr.dwSectionNameAddr);
      GridAddRow(Grid, [
        Name,
        PByteToHexString(@Hdr.dwFlags[0], 4),
        _DWord(Hdr.dwVirtualAddr),
        _DWord(Hdr.dwVirtualSize),
        _DWord(Hdr.dwRawAddr),
        _DWord(Hdr.dwSizeofRaw),
        _DWord(Hdr.dwSectionNameAddr),
        _DWord(Hdr.dwSectionRefCount),
        _DWord(Hdr.dwHeadSharedRefCountAddr),
        _DWord(Hdr.dwTailSharedRefCountAddr),
        PByteToHexString(@Hdr.bzSectionDigest[0], 20) // TODO : Whole array?!
      ]);

      if PXPR_IMAGE(MyXBE.m_bzSection[i]).hdr.Header.dwMagic = XPR_MAGIC_VALUE then
        _CreateNode(NodeResources, 'Image ' + Name, _Initialize_XPRSection(PXPR_IMAGE(MyXBE.m_bzSection[i])));
      
    end;

    Splitter := TSplitter.Create(Self);
    Splitter.Parent := Result;
    Splitter.Align := alTop;
    Splitter.Top := Grid.Height;

    HexViewer := THexViewer.Create(Self);
    HexViewer.Parent := Result;
    HexViewer.Align := alClient;

    Grid.Tag := Integer(HexViewer);
    Grid.OnClick := SectionClick;
  end;

begin
  CloseXBE;
  MyXBE := TXbe.Create(aFilePath, ftXbe);
  FXBEFileName := ExtractFileName(aFilePath);
  Caption := Application.Title + ' - [' + FXBEFileName + ']';
  PageControl1.Visible := True;

  Level0 := _CreateNode(nil, 'File : ' + FXBEFileName, _CreateGrid_File);
  NodeXBEHeader := _CreateNode(Level0, 'XBE Header', _Initialize_XBEHeader);
  NodeResources := _CreateNode(Level0, 'Resources', nil);
  
  _CreateNode(NodeXBEHeader, 'Certificate', _Initialize_Certificate);
  _CreateNode(NodeXBEHeader, 'Section Headers', _Initialize_SectionHeaders);
  _CreateNode(NodeXBEHeader, 'TLS', _Initialize_Stub);
  _CreateNode(NodeXBEHeader, 'Library Versions', _Initialize_Stub);
  _CreateNode(NodeResources, 'Logo Bitmap', _Initialize_Logo);

  _CreateNode(Level0, 'Hex Viewer', _Initialize_HexViewer);
  _CreateNode(Level0, 'Strings', _Initialize_Strings);

  Level0.Expand(True);
  TreeView1.Selected := Level0;
end;

procedure TFormXBEExplorer.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  PageControl1.ActivePage := TTabSheet(Node.Data);
end;

end.

