{
   xISO
   Copyright 1984, 1986, 1989, 1992, 2000, 2001, 2002
   Free Software Foundation, Inc.

   This file is part of xISO.

   xISO is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Bison is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Bison; see the file COPYING.  If not, write to the Free
   Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.
}

unit ufrm_Main;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ShlObj, ActiveX, ExtCtrls, ImgList, CDROM, WIN32ASPI,
  Menus, ShellApi, ToolWin, Graphics, Registry,
  // XIso
  uxisomaker,
  xisomakerv3,
  uxiso,
  TextConsts,
  ufrm_Language,
  Grabacion,
  FormCreacionISO;


type
  Tfrm_Main = class(TForm)
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    TreeView1: TTreeView;
    Panel3: TPanel;
    ListView1: TListView;
    Splitter1: TSplitter;
    ImageList1: TImageList;
    StatusBar1: TStatusBar;
    PopupMenu1: TPopupMenu;
    ExtraerFichero1: TMenuItem;
    OpenDialog2: TOpenDialog;
    ExtraerFicheroyEjecutar1: TMenuItem;
    Introducirfichero1: TMenuItem;
    SaveDialog2: TSaveDialog;
    CoolBar1: TCoolBar;
    ToolBar1: TToolBar;
    MainMenu1: TMainMenu;
    Archivo1: TMenuItem;
    Salir1: TMenuItem;
    Herramientas1: TMenuItem;
    Parches1: TMenuItem;
    XBEDevKitRetail1: TMenuItem;
    Crea1: TMenuItem;
    ToolBar2: TToolBar;
    btnExtractAll: TToolButton;
    ImageList2: TImageList;
    ImageList3: TImageList;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolBar3: TToolBar;
    cLectores: TComboBox;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton9: TToolButton;
    AcercadeXISO1: TMenuItem;
    ToolButton10: TToolButton;
    SaveDialog3: TSaveDialog;
    N1: TMenuItem;
    Opciones1: TMenuItem;
    ToolButton7: TToolButton;
    AcercadexISO2: TMenuItem;
    Sitiooficial1: TMenuItem;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ExtensionesShell1: TMenuItem;
    SaveDialog1: TSaveDialog;
    AadirsoporteISO96601: TMenuItem;
    GrabarISO1: TMenuItem;
    PopupMenu2: TPopupMenu;
    Extraercarpeta1: TMenuItem;
    Abrir1: TMenuItem;
    N2: TMenuItem;
    btnXIsoStudio: TToolButton;
    ToolButton14: TToolButton;
    xISOStudio1: TMenuItem;
    procedure TreeView1Click(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ListView1DblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);
    procedure ExtraerFicheroyEjecutar1Click(Sender: TObject);
    procedure Introducirfichero1Click(Sender: TObject);
    procedure Salir1Click(Sender: TObject);
    procedure XBEDevKitRetail1Click(Sender: TObject);
    procedure Crea1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure btnExtractAllClick(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton10Click(Sender: TObject);
    procedure Opciones1Click(Sender: TObject);
    procedure AcercadexISO2Click(Sender: TObject);
    procedure Sitiooficial1Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure ExtensionesShell1Click(Sender: TObject);
    procedure AadirsoporteISO96601Click(Sender: TObject);
    procedure GrabarISO1Click(Sender: TObject);
    procedure Extraercarpeta1Click(Sender: TObject);
    procedure TreeView1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure btnXIsoStudioClick(Sender: TObject);
  private
    procedure WMDROPFILES(var msg: TMessage); message WM_DROPFILES;
  public
    ProgressBar1: TProgressBar;
    procedure OpenImage(Image: string);
  end;

  // SCSI Structures
  TDevice = record
    Name: string;
    HA: Byte;
    SCSI: Byte;
    LUN: Byte;
    IsWriter: Boolean;
    Letter: AnsiChar;
  end;

const
  OD_IMAGEN = 0;
  OD_DVD = 1;
  OD_CD = 2;

var
  frm_Main: Tfrm_Main;

  ImageName: string;
  DataSource: Integer;
  CDUnit: TCDROM;
  Devices: array[0..255] of TDevice;
  NumberOfDevices: Integer;
  StopReading: Boolean;

function SelectDirectory(const Caption: string; const Root: WideString;
  out Directory: string): Boolean;
procedure ExtractCD(HA, SCSI, LUN: Byte; DirNumber: Integer; Folder: string);
procedure CreateImage(Folder: string; Fichero: string);
function QuitarComilla(Texto: string): string;

implementation

uses
  ufrmProgress;

{$R *.dfm}
//{$R WinXP.res}

function QuitarComilla(Texto: string): string;
begin
  if Length(Texto) > 1 then
    Result := Copy(Texto, 1, Length(Texto))
  else
    Result := '';
end;

procedure ScanSCSI();
var
  i, j, k, l: Byte;
  DeviceType: TDeviceType;
  DeviceInfo: TDeviceInfo;
begin
  with CDUnit do
  begin
    l := 0;
    for i := 0 to 7 do
      for j := 0 to 7 do
        for k := 0 to 7 do
        begin
          DeviceType := GetDeviceType(i, j, k);
          if DeviceType <> dt_CDROM then
            Continue;

          DeviceInfo := Inquiry(i, j, k);

          Devices[l].Name := Format('%d:%d:%d %s %s %s', [i, j, k, DeviceInfo.VendorID, DeviceInfo.ProductID, DeviceInfo.Revision]);
          Devices[l].HA := i;
          Devices[l].SCSI := j;
          Devices[l].LUN := k;
          Devices[l].Letter := #00;
          Devices[l].IsWriter := False;
          l := l + 1;
        end;
  end;
  NumberOfDevices := l;
end;

procedure AssignDevicesToList();
var
  i: Integer;
begin
  with frm_Main do
  begin
    for i := 0 to NumberOfDevices - 1 do
    begin
      cLectores.Items.Add(Devices[i].Name);
    end;
    cLectores.ItemIndex := 0;
  end;
end;

procedure AssignSCSIID(var HA, SCSI, LUN: Byte);
begin
  HA := Devices[frm_Main.cLectores.ItemIndex].HA;
  SCSI := Devices[frm_Main.cLectores.ItemIndex].SCSI;
  LUN := Devices[frm_Main.cLectores.ItemIndex].LUN;
end;

function SelectDirectory(const Caption: string; const Root: WideString;
  out Directory: string): Boolean;
const
  BIF_NEWDIALOGSTYLE = $0040;
  BIF_BROWSEINCLUDEURLS = $0080;
var
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Eaten, Flags: LongWord;
begin
  Result := False;
  Directory := '';
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      RootItemIDList := nil;
      if Root <> '' then
      begin
        SHGetDesktopFolder(IDesktopFolder);
        IDesktopFolder.ParseDisplayName(0, nil,
          POleStr(Root), Eaten, RootItemIDList, Flags);
      end;
      with BrowseInfo do
      begin
        hwndOwner := 0;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        ulFlags := BIF_RETURNONLYFSDIRS or BIF_RETURNFSANCESTORS or BIF_EDITBOX or BIF_NEWDIALOGSTYLE;
      end;
      try
        ItemIDList := ShBrowseForFolder(BrowseInfo);
      finally
      end;
      Result := ItemIDList <> nil;
      if Result then
      begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        if length(Buffer) > 3 then Buffer := PChar(string(Buffer) + '\');
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

function ParchearXBE(sXBE: string): Boolean;
var
  fXBE: TFileStream;
  Entry: Cardinal;
  Kernel: Cardinal;
begin
  Result := False;
  fXBE := TFileStream.Create(sXBE, fmOpenReadWrite);
  if fXBE = nil then
    Exit;

  fXBE.Seek(296, soFromBeginning);
  fXBE.Read(Entry, 4);
  fXBE.Seek(344, soFromBeginning);
  fXBE.Read(Kernel, 4);

  Entry := Entry xor 2491784523 xor 2835109803;
  Kernel := Kernel xor 4021416274 xor 1533886646;

  fXBE.Seek(296, soFromBeginning);
  fXBE.Write(Entry, 4);
  fXBE.Seek(344, soFromBeginning);
  fXBE.Write(Kernel, 4);

  fXBE.Free;

  Result := True;
end;

function ParchearMediaCheck(sXBE: string): Boolean;
const
  MediaCheck: array[0..15] of Byte =
  ($01, $00, $8B, $80, $9C, $00, $00, $00, $25, $FF, $FF, $FF, $00, $3B, $C7, $75);
var
  fXBE: TFileStream;
  Buffer: array[0..65535] of Byte;
  i, j: Integer;
begin
  Result := False;
  fXBE := TFileStream.Create(sXBE, fmOpenReadWrite);
  if fXBE = nil then
    Exit;

  fXBE.Seek(0, soBeginning);

  while (fXBE.Position < fXBE.Size) do
  begin
    fXBE.Seek(fXBE.Position - 16, soBeginning);
    fXBE.Read(Buffer, SizeOf(Buffer));
    for i := 0 to SizeOf(Buffer) - 1 do
    begin
      for j := i to i + 15 do
      begin
        if (Buffer[j] <> MediaCheck[j - i]) then
          Break;
          
        if (j = i + 15) then
        begin
          fXBE.Seek(fXBE.Position - SizeOf(Buffer), soBeginning);
          Buffer[j] := $EB;
          fXBE.Write(Buffer, SizeOf(Buffer));
          fXBE.Free;
          Result := True;
          Exit;
        end;
      end;
    end;
  end;

  fXBE.Free;
end;

procedure ReadFiles(DirNumber: Integer);
var
  i, Sector, Size: Integer;
  NamePtr: PAnsiChar;
  pSector: PInteger;
  Fila: TListItem;
  Attributes: string;
  Icono: TIcon;

  function GetAssociatedIcon(const AExtension: string; ASmall: Boolean): HIcon;
  var
    Info: TSHFileInfo;
    Flags: Cardinal;
  begin
    if ASmall then
      Flags := SHGFI_ICON or SHGFI_SMALLICON or SHGFI_USEFILEATTRIBUTES or SHGFI_SHELLICONSIZE
    else
      Flags := SHGFI_ICON or SHGFI_LARGEICON or SHGFI_USEFILEATTRIBUTES or SHGFI_SHELLICONSIZE;

    SHGetFileInfo(PChar(AExtension), FILE_ATTRIBUTE_NORMAL, Info, SizeOf(TSHFileInfo), Flags);
    Result := Info.hIcon;
  end;

begin
  frm_Main.ListView1.Items.BeginUpdate;
  frm_Main.ListView1.Items.Clear;
  Icono := TIcon.Create;
  for i := 0 to xISO.List.Count - 1 do
  begin
    Attributes := '';
    if PxFile(xISO.List.Items[i])^.ParentDir <> DirNumber then
      Continue;

    NamePtr := @PxFile(xISO.List.Items[i])^.Name;
    Sector := PxFile(xISO.List.Items[i])^.SectorIn;
    Size := PxFile(xISO.List.Items[i])^.Size;
    if (PxFile(xISO.List.Items[i])^.Attributes and XF_READONLY) = XF_READONLY then Attributes := Attributes + 'R';
    if (PxFile(xISO.List.Items[i])^.Attributes and XF_HIDDEN) = XF_HIDDEN then Attributes := Attributes + 'O';
    if (PxFile(xISO.List.Items[i])^.Attributes and XF_SYSTEM) = XF_SYSTEM then Attributes := Attributes + 'S';
    if (PxFile(xISO.List.Items[i])^.Attributes and XF_DIRECTORY) = XF_DIRECTORY then Attributes := Attributes + 'D';
    if (PxFile(xISO.List.Items[i])^.Attributes and XF_FILE) = XF_FILE then Attributes := Attributes + 'F';
    if (PxFile(xISO.List.Items[i])^.Attributes and XF_NORMAL) = XF_NORMAL then Attributes := Attributes + 'N';

    Fila := frm_Main.ListView1.Items.Add;
    Fila.Caption := string(NamePtr);
    New(pSector);
    pSector^ := PxFile(xISO.List.Items[i])^.ChildDir;
    Fila.Data := pSector;
    Icono.Handle := GetAssociatedIcon(ExtractFileExt(string(NamePtr)), True);
    frm_Main.ImageList1.ReplaceIcon(6, Icono);
    if (PxFile(xISO.List.Items[i])^.Attributes and XF_DIRECTORY) = XF_DIRECTORY then
      Fila.ImageIndex := 0
    else
      Fila.ImageIndex := 6;
    Fila.SubItems.Add(IntToStr(Sector));
    Fila.SubItems.Add(IntToStr(Size));
    Fila.SubItems.Add(Attributes);

    Fila.SubItems.Add(IntToStr(PxFile(xISO.List.Items[i])^.pRight));
  end;
  Icono.Free;
  frm_Main.ListView1.Items.EndUpdate;
end;

procedure Tfrm_Main.WMDROPFILES(var msg: TMessage);
var
  dr: HDrop;
  fn: array[0..254] of Char;
  ext: string;
begin
  dr := msg.wparam;
  DragQueryFile(dr, $FFFFFFFF, fn, SizeOf(fn));
  DragQueryFile(dr, 0, fn, SizeOf(fn));
  DragFinish(dr);
  ext := Lowercase(ExtractFileExt(fn));
  if (ext = '.iso') or (ext = '.xiso') or (ext = '.bin') then
    OpenImage(fn);
end;

procedure Tfrm_Main.OpenImage(Image: string);
var
  pDirSector: PInteger;
  NamePtr: PAnsiChar;
  i: Integer;
  Node: TTreeNode;
begin
  DataSource := OD_IMAGEN;
  ImageName := Image;
  if not OpenXISO(ImageName) then
  begin
    MessageBox(frm_Main.Handle, PChar(SImagenNoXBOX), PChar(SMessage), MB_ICONINFORMATION or MB_OK);
    Exit;
  end;

  Treeview1.Items.BeginUpdate;
  Treeview1.Items.Clear;
  Node := Treeview1.Items.Add(nil, ChangeFileExt(ExtractFileName(ImageName), ''));
  Node.ImageIndex := 3; //0;
  Node.SelectedIndex := 3; //1;

  for i := 0 to xISO.List.Count - 1 do
  begin
    if ((PxFile(xISO.List.Items[i])^.Attributes and XF_DIRECTORY) = XF_DIRECTORY) then
    begin
      New(pDirSector);
      NamePtr := @PxFile(xISO.List.Items[i])^.Name;
      pDirSector^ := PxFile(xISO.List.Items[i])^.ChildDir;
      Node := Treeview1.Items.AddChildObject(Treeview1.Items[PxFile(xISO.List.Items[i])^.ParentDir], string(NamePtr), pDirSector);
      Node.ImageIndex := 0;
      Node.SelectedIndex := 1;
    end;
  end;

  btnExtractAll.Enabled := True;

  Treeview1.Items.EndUpdate;
  Treeview1.Items[0].Expand(True);
  Treeview1.Items[0].Selected := True;
  ReadFiles(0);
  StatusBar1.Panels[0].Text := Format('%s: %d', [SFiles, xISO.NrFiles]);
  StatusBar1.Panels[1].Text := Format('%s: %d', [SFolders, xISO.NrFolders]);
  StatusBar1.Panels[2].Text := Format('%s: %d KB', [SISOSize, xISO.ISOSize div 1024]);
end;

procedure Tfrm_Main.TreeView1Click(Sender: TObject);
var
  Dir: Integer;
begin
  if Treeview1.Selected = nil then
    Exit;

  if Treeview1.Selected.AbsoluteIndex = 0 then
    Dir := 0
  else
    Dir := PInteger(Treeview1.Selected.Data)^;

  ReadFiles(Dir);
end;

procedure Tfrm_Main.ListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if ListView1.SelCount = 0 then ToolButton2.Enabled := False
  else ToolButton2.Enabled := True;
  if ListView1.SelCount = 1 then
    ToolButton9.Enabled := True
  else
    ToolButton9.Enabled := False;
end;

procedure Tfrm_Main.ListView1DblClick(Sender: TObject);
var
  Dir: PInteger;
  i: Integer;
begin
  if ListView1.Selected = nil then
    Exit;

  if ListView1.Selected.ImageIndex = 0 then
  begin
    Dir := PInteger(ListView1.Selected.Data);
    for i := 0 to Treeview1.Items.Count - 1 do
    begin
      if (Treeview1.Items[i].Text = ListView1.Selected.Caption) and (PInteger(Treeview1.Items[i].Data)^ = Dir^) then
        Treeview1.Items[i].Selected := True;
    end;

    ReadFiles(Dir^);
  end
  else
    ToolButton2.Click;
end;

procedure Tfrm_Main.FormCreate(Sender: TObject);
var
  Parameter: string;
  i: Integer;
begin
  DragAcceptFiles(Self.Handle, True);

(*  CDUnit := TCDROM.Create(Self);
  if (CDUnit <> nil) and (CDUnit.ASPISupport) then
  begin
    CDUnit.ErrorEnable := True;
    CDUnit.TimeOut_ExecSCSICommand := 4 * 60 * 1000;
    CDUnit.TimeOut_GetDeviceType := 5000;
    CDUnit.TimeOut_GetHostAdapterInquiry := 5000;
    CDUnit.TimeOut_ServiceAbort := 1000;
    CDUnit.TimeOut_ResetDevice := 1000;
    CDUnit.TimeOut_GetDiskInfo := 1000;
    CDUnit.TimeOut_RescanBus := 1000;

    ScanSCSI();
    AssignDevicesToList();
  end
  else
    MessageBox(frm_Main.Handle, PChar(SInstalarASPI), nil, MB_OK or MB_ICONWARNING); *)

  for i := 0 to ParamCount - 1 do
  begin
    Parameter := Copy(ParamStr(i), 1, 2);
    if Parameter = '-o' then
      OpenImage(ParamStr(i + 1));
  end;
end;

procedure ExtractCD(HA, SCSI, LUN: Byte; DirNumber: Integer; Folder: string);
var
  i, Sector, Size: Integer;
  NamePtr: PAnsiChar;
begin
  if xISO.List = nil then
    Exit;

  for i := 0 to xISO.List.Count - 1 do
  begin
    if PxFile(xISO.List.Items[i])^.ParentDir <> DirNumber then
      Continue;

    NamePtr := @PxFile(xISO.List.Items[i])^.Name;
    Sector := PxFile(xISO.List.Items[i])^.SectorIn;
    Size := PxFile(xISO.List.Items[i])^.Size;

    if (PxFile(xISO.List.Items[i])^.Attributes and XF_DIRECTORY) = XF_DIRECTORY then
    begin
      CreateDir(Folder + string(NamePtr));
      ExtractCD(HA, SCSI, LUN, PxFile(xISO.List.Items[i])^.ChildDir, Folder + string(NamePtr) + '\');
    end
    else
      case DataSource of
        OD_IMAGEN: ExtractFile(ImageName, Folder + string(NamePtr), Sector, Size);
        OD_DVD: ExtractFileXDVD(HA, SCSI, LUN, CDUnit, Folder + string(NamePtr), Sector, Size);
      end;
    if frm_Main <> nil then
    begin
      frm_Main.ProgressBar1.StepIt;
      Application.ProcessMessages;
      Application.ProcessMessages;
      Application.ProcessMessages;
      frm_Main.StatusBar1.Repaint;
      frm_Main.StatusBar1.Refresh;
    end;
  end;
end;

procedure ExtractCDToXBOX(HA, SCSI, LUN: Byte; DirNumber: Integer; Folder: string);
var
  i, Sector, Size: Integer;
  NamePtr: PAnsiChar;
begin
  if xISO.List = nil then
    Exit;

  for i := 0 to xISO.List.Count - 1 do
  begin
    if PxFile(xISO.List.Items[i])^.ParentDir <> DirNumber then
      Continue;

    NamePtr := @PxFile(xISO.List.Items[i])^.Name;
    Sector := PxFile(xISO.List.Items[i])^.SectorIn;
    Size := PxFile(xISO.List.Items[i])^.Size;

    if (PxFile(xISO.List.Items[i])^.Attributes and XF_DIRECTORY) = XF_DIRECTORY then
    begin
      CreateDir(Folder + string(NamePtr));
      ExtractCDToXBOX(HA, SCSI, LUN, PxFile(xISO.List.Items[i])^.ChildDir, Folder + string(NamePtr) + '\');
    end
    else
      case DataSource of
        OD_IMAGEN: ExtractFile(ImageName, Folder + string(NamePtr), Sector, Size);
        OD_DVD: ExtractFileXDVD(HA, SCSI, LUN, CDUnit, Folder + string(NamePtr), Sector, Size);
      end;
    if frm_Main <> nil then
    begin
      frm_Main.ProgressBar1.StepIt;
      Application.ProcessMessages;
      Application.ProcessMessages;
      Application.ProcessMessages;
      frm_Main.StatusBar1.Repaint;
      frm_Main.StatusBar1.Refresh;
    end;
  end;
end;

function FormatearSector(Sector: Integer): string;
var
  i: Integer;
begin
  Result := IntToStr(Sector);
  if (10 - Length(Result) <> 0) then
    for i := 1 to 10 - Length(Result) do
    begin
      Result := '0' + Result;
    end;
end;

function CompararDirPadre(Item1, Item2: Pointer): Integer;
begin
  Result := 0;
  if (PxFile(Item1)^.ParentDir < PxFile(Item2)^.ParentDir) then Result := -1
  else
    if (PxFile(Item1)^.ParentDir > PxFile(Item2)^.ParentDir) then Result := 1
    else
      if (PxFile(Item1)^.ParentDir = PxFile(Item2)^.ParentDir) then Result := 0;
end;

procedure GenerarFileListRec(var F: TextFile; DirNumber: Integer; Folder: string; SectorDirectorio: Integer);
var
  i, Sector: Integer;
  NamePtr: PAnsiChar;
  Directorios: TList;
begin
  Directorios := TList.Create();
  WriteLn(F, FormatearSector(SectorDirectorio) + ',' + Folder);

  for i := 0 to xISO.List.Count - 1 do
  begin
    if PxFile(xISO.List.Items[i])^.ParentDir <> DirNumber then
      Continue;

    NamePtr := @PxFile(xISO.List.Items[i])^.Name;
    Sector := PxFile(xISO.List.Items[i])^.SectorIn;

    if ((PxFile(xISO.List.Items[i])^.Attributes and XF_DIRECTORY) = XF_DIRECTORY)
      and (PxFile(xISO.List.Items[i])^.SectorIn <> 0) then
    begin
      Directorios.Add(xISO.List.Items[i]);
    end
    else
    begin
      WriteLn(F, FormatearSector(Sector) + ',' + Folder + string(NamePtr));
    end;

    frm_Main.ProgressBar1.StepIt;
    Application.ProcessMessages;
    Application.ProcessMessages;
    Application.ProcessMessages;
    frm_Main.StatusBar1.Repaint;
    frm_Main.StatusBar1.Refresh;
  end;

  for i := 0 to Directorios.Count - 1 do
  begin
    NamePtr := @PxFile(Directorios[i])^.Name;
    Sector := PxFile(Directorios[i])^.SectorIn;
    GenerarFileListRec(F, PxFile(Directorios[i])^.ChildDir, Folder + string(NamePtr) + '\', Sector);
  end;

  Directorios.Free;
end;

function GenerarFileList(FileList: string): Boolean;
var
  F: TextFile;
begin
  AssignFile(F, FileList);
  ReWrite(F);
  frm_Main.ProgressBar1.Min := 0;
  frm_Main.ProgressBar1.Max := xISO.List.Count;
  xISO.List.Sort(CompararDirPadre);
  GenerarFileListRec(F, 0, '\', 0);
  CloseFile(F);
  frm_Main.ProgressBar1.Position := 0;
  frm_Main.StatusBar1.Repaint;
  frm_Main.StatusBar1.Refresh;
  Result := True;
end;

procedure Tfrm_Main.PopupMenu1Popup(Sender: TObject);
begin
  if ListView1.Selected = nil then
  begin
    ExtraerFichero1.Enabled := False;
    ExtraerFicheroyEjecutar1.Enabled := False;
    Introducirfichero1.Enabled := False;
  end
  else
    if ListView1.Selected.ImageIndex = 0 then
    begin
      ExtraerFichero1.Enabled := True;
      ExtraerFicheroyEjecutar1.Enabled := False;
      Introducirfichero1.Enabled := False;
    end
    else
    begin
      ExtraerFichero1.Enabled := True;
      ExtraerFicheroyEjecutar1.Enabled := True;
      if (DataSource = OD_IMAGEN) and (ListView1.SelCount = 1) then
      begin
        Introducirfichero1.Enabled := True;
        ExtraerFicheroyEjecutar1.Enabled := True;
      end
      else
        if (DataSource = OD_DVD) and (ListView1.SelCount = 1) then
        begin
          Introducirfichero1.Enabled := False;
          ExtraerFicheroyEjecutar1.Enabled := True;
        end
        else
        begin
          Introducirfichero1.Enabled := False;
          ExtraerFicheroyEjecutar1.Enabled := False;
        end;
    end;
end;

procedure Tfrm_Main.StatusBar1DrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  R: TRect;
begin
  if (Panel.ID = 3) and (Progressbar1 <> nil) then
  begin
    R := Rect;
    R.Right := R.Right;
    R.Top := R.Top - 1;
    R.Bottom := R.Bottom + 1;
    ProgressBar1.BoundsRect := R;
  end;
end;

procedure Tfrm_Main.ExtraerFicheroyEjecutar1Click(Sender: TObject);
var
  Size, Sector, i: Integer;
  Name: string;
  Folder: string;
  HA, SCSI, LUN: Byte;
  DirWindows: array[0..MAX_PATH] of Char;
  DirTemp: string;
begin
  if ListView1.Selected = nil then
    Exit;

  AssignSCSIID(HA, SCSI, LUN);
  if GetWindowsDirectory(DirWindows, MAX_PATH) = 0 then
    Exit;

  DirTemp := Trim(DirWindows) + '\Temp\';
  ProgressBar1.Min := 1;
  ProgressBar1.Max := ListView1.SelCount;
  for i := 0 to ListView1.Items.Count - 1 do
  begin
    if not ListView1.Items[i].Selected then
      Continue;

    Name := ListView1.Items[i].Caption;
    Sector := StrToInt(ListView1.Items[i].SubItems[0]);
    Size := StrToInt(ListView1.Items[i].SubItems[1]);
    case DataSource of
      OD_IMAGEN: ExtractFile(ImageName, DirTemp + Name, Sector, Size);
      OD_DVD: ExtractFileXDVD(HA, SCSI, LUN, CDUnit, Folder + Name, Sector, Size);
    end;
    ProgressBar1.StepIt;
    Statusbar1.Repaint;
    Statusbar1.Refresh;
    Application.ProcessMessages;
    Application.ProcessMessages;
    Application.ProcessMessages;
  end;
  Progressbar1.Position := 0;

  ShellExecute(0, 'open', PChar(DirTemp + Name), nil, nil, SW_SHOW);
end;

procedure Tfrm_Main.Introducirfichero1Click(Sender: TObject);
var
  Sector, Size, RoundedBytes, RestBytes, a: Integer;
  ISOImageFile, Fichero: TFileStream;
  Buffer: array[0..2047] of Byte;
begin
  if ListView1.Selected = nil then
    Exit;

  if not SaveDialog2.Execute then
    Exit;

  Sector := StrToInt(ListView1.Selected.SubItems[0]);
  Size := StrToInt(ListView1.Selected.SubItems[1]);

  ISOImageFile := TFileStream.Create(ImageName, fmOpenWrite);
  Fichero := TFileStream.Create(SaveDialog2.FileName, fmOpenRead);
  if (ISOImageFile = nil) or (Fichero = nil) then
    Exit;

  if Fichero.Size <> Size then
  begin
    Fichero.Free;
    ISOImageFile.Free;
    MessageBox(Handle, PChar(STamDiferentes), PChar(SMessage), MB_OK or MB_ICONINFORMATION);
    Exit;
  end;

  ISOImageFile.Seek(Sector * 2048, soBeginning);

  RoundedBytes := (Fichero.Size div 2048) * 2048;
  RestBytes := Fichero.Size mod 2048;

  a := 0;
  if RoundedBytes <> 0 then
    while (a < RoundedBytes) do
    begin
      Fichero.Read(Buffer, 2048);
      ISOImageFile.Write(Buffer, 2048);
      a := a + 2048;
    end;

  if RestBytes <> 0 then
  begin
    Fichero.Read(Buffer, RestBytes);
    ISOImageFile.Write(Buffer, RestBytes);
  end;

  ISOImageFile.Free;
  Fichero.Free;
  MessageBox(Handle, PChar(SIntroducidoOK), PChar(SMessage), MB_OK or MB_ICONINFORMATION);
end;

procedure Tfrm_Main.Salir1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure Tfrm_Main.XBEDevKitRetail1Click(Sender: TObject);
begin
  if not OpenDialog2.Execute then
    Exit;

  if not ParchearXBE(OpenDialog2.FileName) then
    MessageBox(frm_Main.Handle, PChar(SErrorAbrirXBE), PChar(SError), MB_OK or MB_ICONERROR)
  else
    MessageBox(frm_Main.Handle, PChar(SParcheadoOK), PChar(SMessage), MB_OK or MB_ICONINFORMATION);
end;

procedure CreateImage(Folder: string; Fichero: string);
begin
  if frmProgress = nil then
    frmProgress := frmProgress.Create(frm_Main);
  if (Folder <> '') then
    ufrmProgress.Folder := Folder;
  if (Fichero <> '') then
    frmProgress.SaveDialog1.FileName := Fichero;

  frmProgress.FormPadre := frm_Main;
  frmProgress.ShowModal;
  frmProgress.Free;
  frmProgress := nil;
end;

procedure Tfrm_Main.Crea1Click(Sender: TObject);
begin
  if SelectDirectory(SSelectFolder, '', ufrmProgress.Folder) and SaveDialog1.Execute then
    CreateImage(ufrmProgress.Folder, SaveDialog1.FileName);
end;

procedure Tfrm_Main.ToolButton2Click(Sender: TObject);
var
  Size, Sector, i: Integer;
  Name: string;
  Folder: string;
  HA, SCSI, LUN: Byte;
  Dir: PInteger;
begin
  if ListView1.Selected = nil then
    Exit;

  AssignSCSIID(HA, SCSI, LUN);
  if not SelectDirectory(PChar(SIntroduceDirEx), '', Folder) then
    Exit;

  ProgressBar1.Min := 1;
  ProgressBar1.Max := ListView1.SelCount;
  for i := 0 to ListView1.Items.Count - 1 do
  begin
    if not ListView1.Items[i].Selected then
      Continue;

    if ListView1.Items[i].ImageIndex = 0 then
    begin
      Dir := PInteger(ListView1.Items[i].Data);
      ExtractCD(HA, SCSI, LUN, Dir^, Folder);
      Statusbar1.Repaint;
      Statusbar1.Refresh;
      Continue;
    end;
    Name := ListView1.Items[i].Caption;
    Sector := StrToInt(ListView1.Items[i].SubItems[0]);
    Size := StrToInt(ListView1.Items[i].SubItems[1]);
    case DataSource of
      OD_IMAGEN: ExtractFile(ImageName, Folder + Name, Sector, Size);
      OD_DVD: ExtractFileXDVD(HA, SCSI, LUN, CDUnit, Folder + Name, Sector, Size);
    end;
    ProgressBar1.StepIt;
    Statusbar1.Repaint;
    Statusbar1.Refresh;
    Application.ProcessMessages;
    Application.ProcessMessages;
    Application.ProcessMessages;
  end;

  Progressbar1.Position := 1;
  Statusbar1.Repaint;
  Statusbar1.Refresh;
  MessageBox(Handle, PChar(SFinExtraccion), PChar(SMessage), MB_OK or MB_ICONINFORMATION);
end;

procedure Tfrm_Main.btnExtractAllClick(Sender: TObject);
var
  Folder: string;
  HA, SCSI, LUN: Byte;
begin
  if xISO.List = nil then
    Exit;

  if xISO.List.Count = 0 then
    Exit;

  AssignSCSIID(HA, SCSI, LUN);
  if not SelectDirectory(PChar(SIntroduceDirEx), '', Folder) then
    Exit;

  ProgressBar1.Min := 1;
  ProgressBar1.Max := xISO.List.Count;
  ExtractCD(HA, SCSI, LUN, 0, Folder);
  CDUnit.SetCDSpeed(HA, SCSI, LUN, $FF, $FF);
  ProgressBar1.Position := 1;
  Statusbar1.Repaint;
  Statusbar1.Refresh;
  MessageBox(Handle, PChar(SFinExtraccion), PChar(SMessage), MB_OK or MB_ICONINFORMATION);
end;

procedure Tfrm_Main.ToolButton4Click(Sender: TObject);
var
  pDirSector: PInteger;
  NamePtr: PAnsiChar;
  i: Integer;
  Node: TTreeNode;
  HA, SCSI, LUN: Byte;
begin
  if (CDUnit = nil) or (not CDUnit.ASPISupport) then
    Exit;

        { CODIGO DE LECTURA DIRECTA DESDE DVD}
        { DIRECT DVD READING CODE}
  DataSource := OD_DVD;
  AssignSCSIID(HA, SCSI, LUN);
  if not ReadXDVD(HA, SCSI, LUN, CDUnit) then
  begin
    MessageBox(frm_Main.Handle, PChar(SDVDnoXBOX), Pchar(SMessage), MB_ICONINFORMATION or MB_OK);
    Exit;
  end;

  Treeview1.Items.BeginUpdate;
  Treeview1.Items.Clear;
  Node := Treeview1.Items.Add(nil, 'XDVD');
  Node.ImageIndex := 4; //0
  Node.SelectedIndex := 4; //1

  for i := 0 to xISO.List.Count - 1 do
  begin
    if (PxFile(xISO.List.Items[i])^.Attributes and XF_DIRECTORY) = XF_DIRECTORY then
    begin
      New(pDirSector);
      NamePtr := @PxFile(xISO.List.Items[i])^.Name;
      pDirSector^ := PxFile(xISO.List.Items[i])^.ChildDir;
      Node := Treeview1.Items.AddChildObject(Treeview1.Items[PxFile(xISO.List.Items[i])^.ParentDir], string(NamePtr), pDirSector);
      Node.ImageIndex := 0;
      Node.SelectedIndex := 1;
    end;
  end;

  Treeview1.Items.EndUpdate;
  Treeview1.Items[0].Expand(True);
  Treeview1.Items[0].Selected := True;
  ReadFiles(0);
  StatusBar1.Panels[0].Text := Format('%s: %d', [SFiles, xISO.NrFiles]);
  StatusBar1.Panels[1].Text := Format('%s: %d', [SFolders, xISO.NrFolders]);
  StatusBar1.Panels[2].Text := Format('%s: %d KB', [SISOSize, xISO.ISOSize div 1024]);
end;

procedure Tfrm_Main.ToolButton10Click(Sender: TObject);
begin
  if xISO.List = nil then
    Exit;

  if xISO.List.Count = 0 then
    Exit;

  if not SaveDialog3.Execute then
    Exit;

  GenerarFileList(SaveDialog3.FileName);
end;

procedure Tfrm_Main.Opciones1Click(Sender: TObject);
begin
  frmLanguage.ShowModal;
end;

procedure Tfrm_Main.AcercadexISO2Click(Sender: TObject);
begin
  MessageBox(Handle, PChar('xISO' + #13 + #13 + SDesarrollado + ' Yursoft.' + #13 + SManejoASPI + ' Yursoft' + #13 + SLecturaXBOX + ' Yursoft' + #13 + SEscrituraXBOX + ' Yursoft'), PChar(SAcercaDe), MB_OK or MB_ICONINFORMATION);
end;

procedure Tfrm_Main.Sitiooficial1Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://www.yursoft.com', nil, nil, SW_SHOWNORMAL);
end;

procedure Tfrm_Main.ToolButton5Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then
    Exit;

  OpenImage(OpenDialog1.FileName);
end;

procedure Tfrm_Main.ExtensionesShell1Click(Sender: TObject);
var
  Reg: TRegistry;
  S, T: string;
begin
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_CLASSES_ROOT;
  Reg.OpenKey('Directory\shell\xISO', True);
  Reg.WriteString('', SRegCrearXISO);
  Reg.OpenKey('command', True);
  Reg.WriteString('', ParamStr(0) + ' -d "%1" -f "%1"');
  Reg.CloseKey;

  Reg.OpenKey('XISOFILE', True);
  Reg.WriteString('', 'xISO File');
  Reg.OpenKey('\XISOFILE\DefaultIcon', True);
  Reg.WriteString('', ParamStr(0) + ',0');
  Reg.OpenKey('\XISOFILE\shell', True);
  Reg.OpenKey('open', True);
  Reg.OpenKey('command', True);
  Reg.WriteString('', ParamStr(0) + ' -o "%1"');
  Reg.CloseKey;

  Reg.OpenKey('\XISOFILE\shell', True);
  Reg.OpenKey('Extraer', True);
  Reg.WriteString('', SRegExtraerXISO);
  Reg.OpenKey('\XISOFILE\shell\Extraer\command', True);
  Reg.WriteString('', ParamStr(0) + ' -x "%1" -f "%1"');
  Reg.CloseKey;

  Reg.OpenKey('.xiso', True);
  Reg.WriteString('', 'XISOFILE');
  Reg.CloseKey;

  Reg.OpenKey('\.iso', True);
  S := Reg.ReadString('');
  if (S = '') or (not Reg.KeyExists('\' + S)) then
    S := '.iso';
  Reg.OpenKey('\' + S + '\Shell', True);
  Reg.OpenKey('Extraer', True);
  Reg.WriteString('', SRegExtraerConXISO);
  Reg.OpenKey('\' + S + '\shell\Extraer\command', True);
  if Reg.KeyExists('\' + S + '\Shell\open') then
    T := 'OpenXISO'
  else
    T := 'open';
  Reg.WriteString('', ParamStr(0) + ' -x "%1" -f "%1"');
  Reg.OpenKey('\' + S + '\Shell\' + T, True);
  Reg.WriteString('', SRegAbrirConXISO);
  Reg.OpenKey('\' + S + '\Shell\' + T + '\command', True);
  Reg.WriteString('', ParamStr(0) + ' -o "%1"');
  Reg.CloseKey;

  Reg.Free;
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

procedure Tfrm_Main.AadirsoporteISO96601Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then
    Exit;

  if not XDFS2ISO9660(OpenDialog1.FileName) then
    MessageBox(frm_Main.Handle, PChar(SImagenNoXBOX), PChar(SMessage), MB_ICONINFORMATION or MB_OK)
  else
    MessageBox(frm_Main.Handle, PChar(SISO9660ok), PChar(SMessage), MB_ICONINFORMATION or MB_OK);
end;

procedure Tfrm_Main.GrabarISO1Click(Sender: TObject);
begin
  if Form4 = nil then
    Form4 := TForm4.Create(nil);

  Form4.ShowModal;
  Form4.Free;
  Form4 := nil;
end;

procedure Tfrm_Main.Extraercarpeta1Click(Sender: TObject);
var
  Folder: string;
  HA, SCSI, LUN: Byte;
  Dir: PInteger;
begin
  if Treeview1.Selected = nil then
    Exit;

  AssignSCSIID(HA, SCSI, LUN);
  if not SelectDirectory(PChar(SIntroduceDirEx), '', Folder) then
    Exit;

  ProgressBar1.Min := 1;
  ProgressBar1.Max := xISO.List.Count;
  if Treeview1.Selected.AbsoluteIndex = 0 then
    ExtractCD(HA, SCSI, LUN, 0, Folder)
  else
  begin
    Dir := PInteger(Treeview1.Selected.Data);
    ExtractCD(HA, SCSI, LUN, Dir^, Folder);
  end;
  
  ProgressBar1.Position := 1;
  Statusbar1.Repaint;
  Statusbar1.Refresh;
end;

procedure Tfrm_Main.TreeView1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  TreeView1Click(Self);
end;

procedure Tfrm_Main.FormShow(Sender: TObject);
begin
  if ProgressBar1 = nil then
  begin
    ProgressBar1 := TProgressBar.Create(Self);
    ProgressBar1.Parent := StatusBar1;
    ProgressBar1.Position := 0;
    ProgressBar1.Step := 1;
  end;
end;

procedure Tfrm_Main.btnXIsoStudioClick(Sender: TObject);
begin
  if frmCreateIso = nil then
    frmCreateIso := TfrmCreateIso.Create(Self);

  frmCreateIso.ShowModal;
  FreeAndNil(frmCreateIso);
end;

end.
