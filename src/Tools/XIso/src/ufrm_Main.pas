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
  xbe,
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
    ToolButton1: TToolButton;
    ImageList2: TImageList;
    ImageList3: TImageList;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolBar3: TToolBar;
    cLectores: TComboBox;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton8: TToolButton;
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
    ToolButton13: TToolButton;
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
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
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
    procedure ToolButton13Click(Sender: TObject);
  private
    procedure WMDROPFILES(var msg: TMessage); message WM_DROPFILES;
  public
    ProgressBar1: TProgressBar;
    procedure AbrirImagen(Imagen: string);
  end;

  // Estructuras SCSI
  TDevice = record
    Nombre: string;
    HA: Byte;
    SCSI: Byte;
    LUN: Byte;
    Grabadora: Boolean;
    Letra: Char;
  end;

const
  OD_IMAGEN = 0;
  OD_DVD = 1;
  OD_CD = 2;

var
  frm_Main: Tfrm_Main;

  NombreImagen: string;
  OrigenDatos: Integer;
  Unidad: TCDROM;
  Devices: array[0..255] of TDevice;
  NumberOfDevices: Integer;
  PararLectura: Boolean;

function SelectDirectory(const Caption: string; const Root: WideString;
  out Directory: string): Boolean;
procedure ExtraerCD(HA, SCSI, LUN: Byte; Directorio: Integer; Folder: string);
procedure CrearImagen(Folder: string; Fichero: string);
function QuitarComilla(Texto: string): string;

implementation

uses ufrmProgress;

function IsWindowsVista: Boolean;
var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(VerInfo);
  Result := VerInfo.dwMajorVersion >= 6;
end;

{$R *.dfm}
//{$R WinXP.res}

function QuitarComilla(Texto: string): string;
begin
  if Length(Texto) > 1 then
    Result := Copy(Texto, 1, Length(Texto))
  else
    Result := '';
end;

procedure EscanearSCSI();
var
  i, j, k, l: Byte;
  DeviceType: TDeviceType;
  DeviceInfo: TDeviceInfo;
begin
  with Unidad do
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

          Devices[l].Nombre := Format('%d:%d:%d %s %s %s', [i, j, k, DeviceInfo.VendorID, DeviceInfo.ProductID, DeviceInfo.Revision]);
          Devices[l].HA := i;
          Devices[l].SCSI := j;
          Devices[l].LUN := k;
          Devices[l].Letra := #00;
          Devices[l].Grabadora := False;
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
      cLectores.Items.Add(Devices[i].Nombre);
    end;
    cLectores.ItemIndex := 0;
  end;
end;

procedure AsignarSCSIID(var HA, SCSI, LUN: Byte);
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
    fXBE.Read(Buffer, sizeof(Buffer));
    for i := 0 to SizeOf(Buffer) - 1 do
    begin
      for j := i to i + 15 do
      begin
        if (Buffer[j] <> MediaCheck[j - i]) then
          Break;
          
        if (j = i + 15) then
        begin
          fXBE.Seek(fXBE.Position - sizeof(Buffer), soBeginning);
          Buffer[j] := $EB;
          fXBE.Write(Buffer, sizeof(Buffer));
          fXBE.Free;
          Result := True;
          Exit;
        end;
      end;
    end;
  end;

  fXBE.Free;
end;

procedure LeerFicheros(Directorio: Integer);
var
  i, Sector, Tamano: Integer;
  pNombre: PChar;
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
  for i := 0 to xIISO.Lista.Count - 1 do
  begin
    Attributes := '';
    if PxFichero(xIISO.Lista.Items[i])^.DirPadre <> Directorio then
      Continue;

    pNombre := @PxFichero(xIISO.Lista.Items[i])^.Nombre;
    Sector := PxFichero(xIISO.Lista.Items[i])^.SectorIn;
    Tamano := PxFichero(xIISO.Lista.Items[i])^.Tamano;
    if (PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_SOLOLECTURA) = XF_SOLOLECTURA then Attributes := Attributes + 'R';
    if (PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_OCULTO) = XF_OCULTO then Attributes := Attributes + 'O';
    if (PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_SISTEMA) = XF_SISTEMA then Attributes := Attributes + 'S';
    if (PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_DIRECTORIO) = XF_DIRECTORIO then Attributes := Attributes + 'D';
    if (PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_FICHERO) = XF_FICHERO then Attributes := Attributes + 'F';
    if (PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_NORMAL) = XF_NORMAL then Attributes := Attributes + 'N';

    Fila := frm_Main.ListView1.Items.Add;
    Fila.Caption := pNombre;
    New(pSector);
    pSector^ := PxFichero(xIISO.Lista.Items[i])^.DirHijo;
    Fila.Data := pSector;
    Icono.Handle := GetAssociatedIcon(ExtractFileExt(pNombre), True);
    frm_Main.ImageList1.ReplaceIcon(6, Icono);
    if (PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_DIRECTORIO) = XF_DIRECTORIO then
      Fila.ImageIndex := 0
    else
      Fila.ImageIndex := 6;
    Fila.SubItems.Add(IntToStr(Sector));
    Fila.SubItems.Add(IntToStr(Tamano));
    Fila.SubItems.Add(Attributes);

    Fila.SubItems.Add(IntToStr(PxFichero(xIISO.Lista.Items[i])^.pDer));
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
  DragQueryFile(dr, $FFFFFFFF, fn, sizeof(fn));
  DragQueryFile(dr, 0, fn, sizeof(fn));
  DragFinish(dr);
  ext := Lowercase(ExtractFileExt(fn));
  if (ext = '.iso') or (ext = '.xiso') or (ext = '.bin') then
    AbrirImagen(fn);
end;

procedure Tfrm_Main.AbrirImagen(Imagen: string);
var
  pDirSector: PInteger;
  pNombre: PChar;
  i: Integer;
  Node: TTreeNode;
begin
  OrigenDatos := OD_IMAGEN;
  NombreImagen := Imagen;
  if not AbrirXISO(NombreImagen) then
  begin
    MessageBox(frm_Main.Handle, PChar(SImagenNoXBOX), PChar(SMessage), MB_ICONINFORMATION or MB_OK);
    Exit;
  end;

  Treeview1.Items.BeginUpdate;
  Treeview1.Items.Clear;
  Node := Treeview1.Items.Add(nil, ChangeFileExt(ExtractFileName(NombreImagen), ''));
  Node.ImageIndex := 3; //0;
  Node.SelectedIndex := 3; //1;

  for i := 0 to xIISO.Lista.Count - 1 do
  begin
    if ((PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_DIRECTORIO) = XF_DIRECTORIO) then
    begin
      New(pDirSector);
      pNombre := @PxFichero(xIISO.Lista.Items[i])^.Nombre;
      pDirSector^ := PxFichero(xIISO.Lista.Items[i])^.DirHijo;
      Node := Treeview1.Items.AddChildObject(Treeview1.Items[PxFichero(xIISO.Lista.Items[i])^.DirPadre], pNombre, pDirSector);
      Node.ImageIndex := 0;
      Node.SelectedIndex := 1;
    end;
  end;

  Treeview1.Items.EndUpdate;
  Treeview1.Items[0].Expand(True);
  Treeview1.Items[0].Selected := True;
  LeerFicheros(0);
  StatusBar1.Panels[0].Text := Format('%s: %d', [SFiles, xIISO.NrFiles]);
  StatusBar1.Panels[1].Text := Format('%s: %d', [SFolders, xIISO.NrFolders]);
  StatusBar1.Panels[2].Text := Format('%s: %d KB', [SISOSize, xIISO.ISOSize div 1024]);
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

  LeerFicheros(Dir);
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

    LeerFicheros(Dir^);
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

  Unidad := TCDROM.Create(Self);
  if (Unidad <> nil) and (Unidad.ASPISupport) then
  begin
    Unidad.ErrorEnable := True;
    Unidad.TimeOut_ExecSCSICommand := 4 * 60 * 1000;
    Unidad.TimeOut_GetDeviceType := 5000;
    Unidad.TimeOut_GetHostAdapterInquiry := 5000;
    Unidad.TimeOut_ServiceAbort := 1000;
    Unidad.TimeOut_ResetDevice := 1000;
    Unidad.TimeOut_GetDiskInfo := 1000;
    Unidad.TimeOut_RescanBus := 1000;

    EscanearSCSI();
    AssignDevicesToList();
  end
  else
    MessageBox(frm_Main.Handle, PChar(SInstalarASPI), nil, MB_OK or MB_ICONWARNING);

  for i := 0 to ParamCount - 1 do
  begin
    Parameter := Copy(ParamStr(i), 1, 2);
    if Parameter = '-o' then
      AbrirImagen(ParamStr(i + 1));
  end;
end;

procedure ExtraerCD(HA, SCSI, LUN: Byte; Directorio: Integer; Folder: string);
var
  i, Sector, Tamano: Integer;
  pNombre: PChar;
begin
  if xIISO.Lista = nil then
    Exit;

  for i := 0 to xIISO.Lista.Count - 1 do
  begin
    if PxFichero(xIISO.Lista.Items[i])^.DirPadre <> Directorio then
      Continue;

    pNombre := @PxFichero(xIISO.Lista.Items[i])^.Nombre;
    Sector := PxFichero(xIISO.Lista.Items[i])^.SectorIn;
    Tamano := PxFichero(xIISO.Lista.Items[i])^.Tamano;

    if (PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_DIRECTORIO) = XF_DIRECTORIO then
    begin
      CreateDir(Folder + pNombre);
      ExtraerCD(HA, SCSI, LUN, PxFichero(xIISO.Lista.Items[i])^.DirHijo, Folder + pNombre + '\');
    end
    else
      case OrigenDatos of
        OD_IMAGEN: ExtraerFichero(NombreImagen, Folder + pNombre, Sector, Tamano);
        OD_DVD: ExtraerFicheroXDVD(HA, SCSI, LUN, Unidad, Folder + pNombre, Sector, Tamano);
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

procedure ExtraerCDaXBOX(HA, SCSI, LUN: Byte; Directorio: Integer; Folder: string);
var
  i, Sector, Tamano: Integer;
  pNombre: PChar;
begin
  if xIISO.Lista = nil then
    Exit;

  for i := 0 to xIISO.Lista.Count - 1 do
  begin
    if PxFichero(xIISO.Lista.Items[i])^.DirPadre <> Directorio then
      Continue;

    pNombre := @PxFichero(xIISO.Lista.Items[i])^.Nombre;
    Sector := PxFichero(xIISO.Lista.Items[i])^.SectorIn;
    Tamano := PxFichero(xIISO.Lista.Items[i])^.Tamano;

    if (PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_DIRECTORIO) = XF_DIRECTORIO then
    begin
      CreateDir(Folder + pNombre);
      ExtraerCDaXBOX(HA, SCSI, LUN, PxFichero(xIISO.Lista.Items[i])^.DirHijo, Folder + pNombre + '\');
    end
    else
      case OrigenDatos of
        OD_IMAGEN: ExtraerFichero(NombreImagen, Folder + pNombre, Sector, Tamano);
        OD_DVD: ExtraerFicheroXDVD(HA, SCSI, LUN, Unidad, Folder + pNombre, Sector, Tamano);
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
  if (PxFichero(Item1)^.DirPadre < PxFichero(Item2)^.DirPadre) then Result := -1
  else
    if (PxFichero(Item1)^.DirPadre > PxFichero(Item2)^.DirPadre) then Result := 1
    else
      if (PxFichero(Item1)^.DirPadre = PxFichero(Item2)^.DirPadre) then Result := 0;
end;

procedure GenerarFileListRec(var F: TextFile; Directorio: Integer; Folder: string; SectorDirectorio: Integer);
var
  i, Sector: Integer;
  pNombre: PChar;
  Directorios: TList;
begin
  Directorios := TList.Create();
  WriteLn(F, FormatearSector(SectorDirectorio) + ',' + Folder);

  for i := 0 to xIISO.Lista.Count - 1 do
  begin
    if PxFichero(xIISO.Lista.Items[i])^.DirPadre <> Directorio then
      Continue;

    pNombre := @PxFichero(xIISO.Lista.Items[i])^.Nombre;
    Sector := PxFichero(xIISO.Lista.Items[i])^.SectorIn;

    if ((PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_DIRECTORIO) = XF_DIRECTORIO)
      and (PxFichero(xIISO.Lista.Items[i])^.SectorIn <> 0) then
    begin
      Directorios.Add(xIISO.Lista.Items[i]);
    end
    else
    begin
      WriteLn(F, FormatearSector(Sector) + ',' + Folder + pNombre);
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
    pNombre := @PxFichero(Directorios[i])^.Nombre;
    Sector := PxFichero(Directorios[i])^.SectorIn;
    GenerarFileListRec(F, PxFichero(Directorios[i])^.DirHijo, Folder + pNombre + '\', Sector);
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
  frm_Main.ProgressBar1.Max := xIISO.Lista.Count;
  xIISO.Lista.Sort(CompararDirPadre);
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
      if (OrigenDatos = OD_IMAGEN) and (ListView1.SelCount = 1) then
      begin
        Introducirfichero1.Enabled := True;
        ExtraerFicheroyEjecutar1.Enabled := True;
      end
      else
        if (OrigenDatos = OD_DVD) and (ListView1.SelCount = 1) then
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
  tamano, Sector, i: Integer;
  nombre: string;
  Folder: string;
  HA, SCSI, LUN: Byte;
  DirWindows: array[0..MAX_PATH] of Char;
  DirTemp: string;
begin
  if ListView1.Selected = nil then
    Exit;

  AsignarSCSIID(HA, SCSI, LUN);
  if GetWindowsDirectory(DirWindows, MAX_PATH) = 0 then
    Exit;

  DirTemp := Trim(DirWindows) + '\Temp\';
  ProgressBar1.Min := 1;
  ProgressBar1.Max := ListView1.SelCount;
  for i := 0 to ListView1.Items.Count - 1 do
  begin
    if not ListView1.Items[i].Selected then
      Continue;

    nombre := ListView1.Items[i].Caption;
    Sector := StrToInt(ListView1.Items[i].SubItems[0]);
    tamano := StrToInt(ListView1.Items[i].SubItems[1]);
    case OrigenDatos of
      OD_IMAGEN: ExtraerFichero(NombreImagen, DirTemp + nombre, Sector, tamano);
      OD_DVD: ExtraerFicheroXDVD(HA, SCSI, LUN, Unidad, Folder + nombre, Sector, tamano);
    end;
    ProgressBar1.StepIt;
    Statusbar1.Repaint;
    Statusbar1.Refresh;
    Application.ProcessMessages;
    Application.ProcessMessages;
    Application.ProcessMessages;
  end;
  Progressbar1.Position := 0;

  ShellExecute(0, 'open', PChar(DirTemp + nombre), nil, nil, SW_SHOW);
end;

procedure Tfrm_Main.Introducirfichero1Click(Sender: TObject);
var
  Sector, Size, Ent, Resto, a: Integer;
  ISOImageFile, Fichero: TFileStream;
  Buffer: array[0..2047] of Byte;
begin
  if ListView1.Selected = nil then
    Exit;

  if not SaveDialog2.Execute then
    Exit;

  Sector := StrToInt(ListView1.Selected.SubItems[0]);
  Size := StrToInt(ListView1.Selected.SubItems[1]);

  ISOImageFile := TFileStream.Create(NombreImagen, fmOpenWrite);
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

  Ent := (Fichero.Size div 2048) * 2048;
  Resto := Fichero.Size mod 2048;

  a := 0;
  if ent <> 0 then
    while (a < ent) do
    begin
      Fichero.Read(Buffer, 2048);
      ISOImageFile.Write(Buffer, 2048);
      a := a + 2048;
    end;

  if Resto <> 0 then
  begin
    Fichero.Read(Buffer, Resto);
    ISOImageFile.Write(Buffer, Resto);
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

procedure CrearImagen(Folder: string; Fichero: string);
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
    CrearImagen(ufrmProgress.Folder, SaveDialog1.Filename);
end;

procedure Tfrm_Main.ToolButton2Click(Sender: TObject);
var
  tamano, Sector, i: Integer;
  nombre: string;
  Folder: string;
  HA, SCSI, LUN: Byte;
  Dir: PInteger;
begin
  if ListView1.Selected = nil then
    Exit;

  AsignarSCSIID(HA, SCSI, LUN);
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
      ExtraerCD(HA, SCSI, LUN, Dir^, Folder);
      Statusbar1.Repaint;
      Statusbar1.Refresh;
      Continue;
    end;
    nombre := ListView1.Items[i].Caption;
    Sector := StrToInt(ListView1.Items[i].SubItems[0]);
    tamano := StrToInt(ListView1.Items[i].SubItems[1]);
    case OrigenDatos of
      OD_IMAGEN: ExtraerFichero(NombreImagen, Folder + nombre, Sector, tamano);
      OD_DVD: ExtraerFicheroXDVD(HA, SCSI, LUN, Unidad, Folder + nombre, Sector, tamano);
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

procedure Tfrm_Main.ToolButton1Click(Sender: TObject);
var
  Folder: string;
  HA, SCSI, LUN: Byte;
begin
  if xIISO.Lista = nil then
    Exit;

  if xIISO.Lista.Count = 0 then
    Exit;

  AsignarSCSIID(HA, SCSI, LUN);
  if not SelectDirectory(PChar(SIntroduceDirEx), '', Folder) then
    Exit;

  ProgressBar1.Min := 1;
  ProgressBar1.Max := xIISO.Lista.Count;
  ExtraerCD(HA, SCSI, LUN, 0, Folder);
  Unidad.SetCDSpeed(HA, SCSI, LUN, $FF, $FF);
  ProgressBar1.Position := 1;
  Statusbar1.Repaint;
  Statusbar1.Refresh;
  MessageBox(Handle, PChar(SFinExtraccion), PChar(SMessage), MB_OK or MB_ICONINFORMATION);
end;

procedure Tfrm_Main.ToolButton4Click(Sender: TObject);
var
  pDirSector: PInteger;
  pNombre: PChar;
  i: Integer;
  Node: TTreeNode;
  HA, SCSI, LUN: Byte;
begin
  if (Unidad = nil) or (not Unidad.ASPISupport) then
    Exit;

        { CODIGO DE LECTURA DIRECTA DESDE DVD}
        { DIRECT DVD READING CODE}
  OrigenDatos := OD_DVD;
  AsignarSCSIID(HA, SCSI, LUN);
  if not LeerXDVD(HA, SCSI, LUN, Unidad) then
  begin
    MessageBox(frm_Main.Handle, PChar(SDVDnoXBOX), Pchar(SMessage), MB_ICONINFORMATION or MB_OK);
    Exit;
  end;

  Treeview1.Items.BeginUpdate;
  Treeview1.Items.Clear;
  Node := Treeview1.Items.Add(nil, 'XDVD');
  Node.ImageIndex := 4; //0
  Node.SelectedIndex := 4; //1

  for i := 0 to xIISO.Lista.Count - 1 do
  begin
    if (PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_DIRECTORIO) = XF_DIRECTORIO then
    begin
      New(pDirSector);
      pNombre := @PxFichero(xIISO.Lista.Items[i])^.Nombre;
      pDirSector^ := PxFichero(xIISO.Lista.Items[i])^.DirHijo;
      Node := Treeview1.Items.AddChildObject(Treeview1.Items[PxFichero(xIISO.Lista.Items[i])^.DirPadre], pNombre, pDirSector);
      Node.ImageIndex := 0;
      Node.SelectedIndex := 1;
    end;
  end;

  Treeview1.Items.EndUpdate;
  Treeview1.Items[0].Expand(True);
  Treeview1.Items[0].Selected := True;
  LeerFicheros(0);
  StatusBar1.Panels[0].Text := Format('%s: %d', [SFiles, xIISO.NrFiles]);
  StatusBar1.Panels[1].Text := Format('%s: %d', [SFolders, xIISO.NrFolders]);
  StatusBar1.Panels[2].Text := Format('%s: %d KB', [SISOSize, xIISO.ISOSize div 1024]);
end;

procedure Tfrm_Main.ToolButton8Click(Sender: TObject);
var
  sXBE: TXBE;
  s: PWideChar;
begin
  LeerXBE('c:\downloads\default.xbe', sXBE);
  s := @sXBE.Certificado.NombreJuego;
  ShowMessage(s);
end;

procedure Tfrm_Main.ToolButton10Click(Sender: TObject);
begin
  if xIISO.Lista = nil then
    Exit;

  if xIISO.Lista.Count = 0 then
    Exit;

  if not SaveDialog3.Execute then
    Exit;

  GenerarFileList(SaveDialog3.Filename);
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

  AbrirImagen(OpenDialog1.Filename);
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
    T := 'AbrirXISO'
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

  if not XDFS2ISO9660(OpenDialog1.Filename) then
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

  AsignarSCSIID(HA, SCSI, LUN);
  if not SelectDirectory(PChar(SIntroduceDirEx), '', Folder) then
    Exit;

  ProgressBar1.Min := 1;
  ProgressBar1.Max := xIISO.Lista.Count;
  if Treeview1.Selected.AbsoluteIndex = 0 then
    ExtraerCD(HA, SCSI, LUN, 0, Folder)
  else
  begin
    Dir := PInteger(Treeview1.Selected.Data);
    ExtraerCD(HA, SCSI, LUN, Dir^, Folder);
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
    ProgressBar1 := TProgressBar.Create(self);
    ProgressBar1.Parent := StatusBar1;
    ProgressBar1.Position := 0;
    ProgressBar1.Step := 1;
  end;
end;

procedure Tfrm_Main.ToolButton13Click(Sender: TObject);
begin
  if Form5 = nil then Form5 := TForm5.Create(Self);
  Form5.ShowModal;
  Form5.Free;
  Form5 := nil;
end;

end.
