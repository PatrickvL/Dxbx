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
  Windows, Messages, SysUtils, Variants, Classes, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ShlObj, ActiveX, ExtCtrls, ImgList, CDROM, WIN32ASPI,
  Menus, ShellApi, ToolWin, Graphics, Registry, uxisomaker, xisomakerv3,
  sSkinProvider, sSkinManager;

type
  TForm1 = class(TForm)
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
    sSkinManager1: TsSkinManager;
    sSkinProvider1: TsSkinProvider;
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
  TUnidades = record
    Nombre: string;
    HA: byte;
    SCSI: byte;
    LUN: byte;
    Grabadora: boolean;
    Letra: char;
  end;

const
  OD_IMAGEN = 0;
  OD_DVD = 1;
  OD_CD = 2;

var
  Form1: TForm1;

  NombreImagen: string;
  OrigenDatos: integer;
  Unidad: TCDROM;
  Unidades: array[0..255] of TUnidades;
  CantidadUnidades: integer;
  PararLectura: Boolean;

function SelectDirectory(const Caption: string; const Root: WideString;
  out Directory: string): Boolean;
procedure ExtraerCD(HA, SCSI, LUN: byte; Directorio: integer; Carpeta: string);
procedure CrearImagen(Carpeta: string; Fichero: string);
function QuitarComilla(Texto: string): string;

implementation

uses uxiso, xbe, Textos, ufrm_Language, progreso, Grabacion, FormCreacionISO;

function IsWindowsVista: Boolean;
var VerInfo: TOSVersioninfo;
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
  i, j, k, l: byte;
  TipoUnidad: TDeviceType;
  InfoUnidad: TDeviceInfo;
begin
  with Unidad do
  begin
    l := 0;
    for i := 0 to 7 do
      for j := 0 to 7 do
        for k := 0 to 7 do
        begin
          TipoUnidad := GetDeviceType(i, j, k);
          if (TipoUnidad <> dt_CDROM) then continue;
          InfoUnidad := Inquiry(i, j, k);

          Unidades[l].Nombre := Format('%d:%d:%d %s %s %s', [i, j, k, InfoUnidad.VendorID, InfoUnidad.ProductID, InfoUnidad.Revision]);
          Unidades[l].HA := i;
          Unidades[l].SCSI := j;
          Unidades[l].LUN := k;
          Unidades[l].Letra := #00;
          Unidades[l].Grabadora := False;
          l := l + 1;
        end;
  end;
  CantidadUnidades := l;
end;

procedure AsignarUnidadesAListas();
var
  i: integer;
begin
  with Form1 do
  begin
    for i := 0 to CantidadUnidades - 1 do
    begin
      cLectores.Items.Add(Unidades[i].Nombre);
    end;
    cLectores.ItemIndex := 0;
  end;
end;

procedure AsignarSCSIID(var HA, SCSI, LUN: byte);
begin
  HA := Unidades[Form1.cLectores.ItemIndex].HA;
  SCSI := Unidades[Form1.cLectores.ItemIndex].SCSI;
  LUN := Unidades[Form1.cLectores.ItemIndex].LUN;
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
     //WindowList := DisableTaskWindows(0);
      try
        ItemIDList := ShBrowseForFolder(BrowseInfo);
      finally
       // EnableTaskWindows(WindowList);
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

function ParchearXBE(sXBE: string): boolean;
var
  fXBE: TFileStream;
  Entry: Cardinal;
  Kernel: Cardinal;
begin
  Result := False;
  fXBE := TFileStream.Create(sXBE, fmOpenReadWrite);
  if fXBE = nil then exit;

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

//ORIGINAL: 01008B809C00000025FFFFFF003BC775
//PARCHEADO: 01008B809C00000025FFFFFF003BC7EB

function ParchearMediaCheck(sXBE: string): boolean;
const
  MediaCheck: array[0..15] of byte =
  ($01, $00, $8B, $80, $9C, $00, $00, $00, $25, $FF, $FF, $FF, $00, $3B, $C7, $75);
var
  fXBE: TFileStream;
  Buffer: array[0..65535] of byte;
  i, j: integer;
begin
  Result := False;
  fXBE := TFileStream.Create(sXBE, fmOpenReadWrite);
  if fXBE = nil then exit;
  fXBE.Seek(0, soBeginning);

  while (fXBE.Position < fXBE.Size) do
  begin
    fXBE.Seek(fXBE.Position - 16, soBeginning);
    fXBE.Read(Buffer, sizeof(Buffer));
    for i := 0 to SizeOf(Buffer) - 1 do
    begin
      for j := i to i + 15 do
      begin
        if (Buffer[j] <> MediaCheck[j - i]) then break;
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
  i, Sector, Tamano: integer;
  pNombre: PChar;
  pSector: PInteger;
  Fila: TListItem;
  Atributos: string;
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
  Form1.Listview1.Items.BeginUpdate;
  Form1.Listview1.Items.Clear;
  Icono := TIcon.Create;
  for i := 0 to xIISO.Lista.Count - 1 do
  begin
    Atributos := '';
             //if (PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_DIRECTORIO) = XF_DIRECTORIO then continue;
    if PxFichero(xIISO.Lista.Items[i])^.DirPadre <> Directorio then continue;
    pNombre := @PxFichero(xIISO.Lista.Items[i])^.Nombre;
    Sector := PxFichero(xIISO.Lista.Items[i])^.SectorIn;
    Tamano := PxFichero(xIISO.Lista.Items[i])^.Tamano;
    if (PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_SOLOLECTURA) = XF_SOLOLECTURA then Atributos := Atributos + 'R';
    if (PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_OCULTO) = XF_OCULTO then Atributos := Atributos + 'O';
    if (PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_SISTEMA) = XF_SISTEMA then Atributos := Atributos + 'S';
    if (PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_DIRECTORIO) = XF_DIRECTORIO then Atributos := Atributos + 'D';
    if (PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_FICHERO) = XF_FICHERO then Atributos := Atributos + 'F';
    if (PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_NORMAL) = XF_NORMAL then Atributos := Atributos + 'N';

    Fila := Form1.Listview1.Items.Add;
    Fila.Caption := pNombre;
    new(pSector);
    pSector^ := PxFichero(xIISO.Lista.Items[i])^.DirHijo;
    Fila.Data := pSector;
    Icono.Handle := GetAssociatedIcon(ExtractFileExt(pNombre), True);
    Form1.ImageList1.ReplaceIcon(6, Icono);
    if (PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_DIRECTORIO) = XF_DIRECTORIO then
      Fila.ImageIndex := 0
    else
      Fila.ImageIndex := 6;
    Fila.SubItems.Add(IntToStr(Sector));
    Fila.SubItems.Add(IntToStr(Tamano));
    Fila.SubItems.Add(Atributos);

    Fila.SubItems.Add(IntToStr(PxFichero(xIISO.Lista.Items[i])^.pDer));
  end;
  Icono.Free;
  Form1.Listview1.Items.EndUpdate;
end;

procedure TForm1.WMDROPFILES(var msg: TMessage);
var
  dr: HDrop;
  nb: integer;
  fn: array[0..254] of char;
  ext: string;
begin
  dr := msg.wparam;
  nb := DragQueryFile(dr, $FFFFFFFF, fn, sizeof(fn));
  DragQueryFile(dr, 0, fn, sizeof(fn));
  DragFinish(dr);
  ext := Lowercase(ExtractFileExt(fn));
  if (ext = '.iso') or (ext = '.xiso') or (ext = '.bin') then
    AbrirImagen(fn);
end;

procedure TForm1.AbrirImagen(Imagen: string);
var
  pDirSector: PInteger;
  pNombre: PChar;
  i: integer;
  Nodo: TTreeNode;
begin
  OrigenDatos := OD_IMAGEN;
  NombreImagen := Imagen;
  if not AbrirXISO(NombreImagen) then
  begin
    MessageBox(Form1.Handle, PChar(rcEngImagenNoXBOX), PChar(rcEngMensaje), MB_ICONINFORMATION or MB_OK);
    Exit;
  end;

  Treeview1.Items.BeginUpdate;
  Treeview1.Items.Clear;
  Nodo := Treeview1.Items.Add(nil, ChangeFileExt(ExtractFileName(NombreImagen), ''));
  Nodo.ImageIndex := 3; //0;
  Nodo.SelectedIndex := 3; //1;

  for i := 0 to xIISO.Lista.Count - 1 do
  begin
    if ((PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_DIRECTORIO) = XF_DIRECTORIO) then
    begin
      new(pDirSector);
      pNombre := @PxFichero(xIISO.Lista.Items[i])^.Nombre;
      pDirSector^ := PxFichero(xIISO.Lista.Items[i])^.DirHijo;
      Nodo := Treeview1.Items.AddChildObject(Treeview1.Items[PxFichero(xIISO.Lista.Items[i])^.DirPadre], pNombre, pDirSector);
      Nodo.ImageIndex := 0;
      Nodo.SelectedIndex := 1;
    end;
  end;

  Treeview1.Items.EndUpdate;
  Treeview1.Items[0].Expand(True);
  Treeview1.Items[0].Selected := True;
  LeerFicheros(0);
  StatusBar1.Panels[0].Text := Format('%s: %d', [rcEngFicheros, xIISO.numFicheros]);
  StatusBar1.Panels[1].Text := Format('%s: %d', [rcEngCarpetas, xIISO.numCarpetas]);
  StatusBar1.Panels[2].Text := Format('%s: %d KB', [rcEngTamISO, xIISO.tamISO div 1024]);
end;

procedure TForm1.TreeView1Click(Sender: TObject);
var
  Dir: integer;
begin
  if Treeview1.Selected = nil then exit;
  if Treeview1.Selected.AbsoluteIndex = 0 then
    Dir := 0
  else
    Dir := PInteger(Treeview1.Selected.Data)^;
  LeerFicheros(Dir);
end;

procedure TForm1.ListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if Listview1.SelCount = 0 then ToolButton2.Enabled := False
  else ToolButton2.Enabled := True;
  if Listview1.SelCount = 1 then
    ToolButton9.Enabled := True
  else
    ToolButton9.Enabled := False;
end;

procedure TForm1.ListView1DblClick(Sender: TObject);
var
  Dir: PInteger;
  i: integer;
begin
  if Listview1.Selected = nil then Exit;
  if Listview1.Selected.ImageIndex = 0 then
  begin
    Dir := PInteger(Listview1.Selected.Data);
    for i := 0 to Treeview1.Items.Count - 1 do
    begin
      if (Treeview1.Items[i].Text = Listview1.Selected.Caption) and (PInteger(Treeview1.Items[i].Data)^ = Dir^) then
        Treeview1.Items[i].Selected := True;
    end;

    LeerFicheros(Dir^);
  end
  else
    ToolButton2.Click;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Parametro, CarpetaParametro: string;
  i: integer;
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
    AsignarUnidadesAListas();
  end
  else
  begin
    MessageBox(Form1.Handle, PChar(rcEngInstalarASPI), nil, MB_OK or MB_ICONWARNING);
  end;

  for i := 0 to ParamCount - 1 do
  begin
    Parametro := Copy(ParamStr(i), 1, 2);
    if Parametro = '-o' then
      AbrirImagen(ParamStr(i + 1));
  end;

  if IsWindowsVista then begin
    sSkinManager1.SkinningRules := [srStdForms, srThirdParty];
  end
  else begin
    sSkinManager1.SkinningRules := [srStdForms, srStdDialogs, srThirdParty];
  end;


end;

procedure ExtraerCD(HA, SCSI, LUN: byte; Directorio: integer; Carpeta: string);
var
  i, Sector, Tamano: integer;
  pNombre: PChar;
begin
  if xIISO.Lista = nil then Exit;
  for i := 0 to xIISO.Lista.Count - 1 do
  begin
    if PxFichero(xIISO.Lista.Items[i])^.DirPadre <> Directorio then continue;
    pNombre := @PxFichero(xIISO.Lista.Items[i])^.Nombre;
    Sector := PxFichero(xIISO.Lista.Items[i])^.SectorIn;
    Tamano := PxFichero(xIISO.Lista.Items[i])^.Tamano;

    if (PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_DIRECTORIO) = XF_DIRECTORIO then
    begin
      CreateDir(Carpeta + pNombre);
      ExtraerCD(HA, SCSI, LUN, PxFichero(xIISO.Lista.Items[i])^.DirHijo, Carpeta + pNombre + '\');
    end
    else
      case OrigenDatos of
        OD_IMAGEN: ExtraerFichero(NombreImagen, carpeta + pNombre, Sector, Tamano);
        OD_DVD: ExtraerFicheroXDVD(HA, SCSI, LUN, Unidad, carpeta + pNombre, Sector, Tamano);
      end;
    if Form1 <> nil then
    begin
      Form1.ProgressBar1.StepIt;
      Application.ProcessMessages;
      Application.ProcessMessages;
      Application.ProcessMessages;
      Form1.StatusBar1.Repaint;
      Form1.StatusBar1.Refresh;
    end;
  end;
end;

procedure ExtraerCDaXBOX(HA, SCSI, LUN: byte; Directorio: integer; Carpeta: string);
var
  i, Sector, Tamano: integer;
  pNombre: PChar;
begin
  if xIISO.Lista = nil then Exit;
  for i := 0 to xIISO.Lista.Count - 1 do
  begin
    if PxFichero(xIISO.Lista.Items[i])^.DirPadre <> Directorio then Continue;
    pNombre := @PxFichero(xIISO.Lista.Items[i])^.Nombre;
    Sector := PxFichero(xIISO.Lista.Items[i])^.SectorIn;
    Tamano := PxFichero(xIISO.Lista.Items[i])^.Tamano;

    if (PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_DIRECTORIO) = XF_DIRECTORIO then
    begin
      CreateDir(Carpeta + pNombre);
      ExtraerCDaXBOX(HA, SCSI, LUN, PxFichero(xIISO.Lista.Items[i])^.DirHijo, Carpeta + pNombre + '\');
    end
    else
      case OrigenDatos of
        OD_IMAGEN: ExtraerFichero(NombreImagen, carpeta + pNombre, Sector, Tamano);
        OD_DVD: ExtraerFicheroXDVD(HA, SCSI, LUN, Unidad, carpeta + pNombre, Sector, Tamano);
      end;
    if Form1 <> nil then
    begin
      Form1.ProgressBar1.StepIt;
      Application.ProcessMessages;
      Application.ProcessMessages;
      Application.ProcessMessages;
      Form1.StatusBar1.Repaint;
      Form1.StatusBar1.Refresh;
    end;
  end;
end;

function FormatearSector(Sector: integer): string;
var
  i: integer;
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

procedure GenerarFileListRec(var F: TextFile; Directorio: integer; Carpeta: string; SectorDirectorio: integer);
var
  i, Sector, Tamano: integer;
  pNombre: PChar;
  Directorios: TList;
begin
  Directorios := TList.Create();
  WriteLn(F, FormatearSector(SectorDirectorio) + ',' + Carpeta);

  for i := 0 to xIISO.Lista.Count - 1 do
  begin
    if PxFichero(xIISO.Lista.Items[i])^.DirPadre <> Directorio then continue;

    pNombre := @PxFichero(xIISO.Lista.Items[i])^.Nombre;
    Sector := PxFichero(xIISO.Lista.Items[i])^.SectorIn;
    Tamano := PxFichero(xIISO.Lista.Items[i])^.Tamano;

    if ((PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_DIRECTORIO) = XF_DIRECTORIO)
      and (PxFichero(xIISO.Lista.Items[i])^.SectorIn <> 0) then
    begin
      Directorios.Add(xIISO.Lista.Items[i]);
                  //GenerarFileListRec(F,PxFichero(xIISO.Lista.Items[i])^.DirHijo,Carpeta+pNombre+'\');
    end
    else
    begin
      WriteLn(F, FormatearSector(Sector) + ',' + Carpeta + pNombre);
    end;

    Form1.ProgressBar1.StepIt;
    Application.ProcessMessages;
    Application.ProcessMessages;
    Application.ProcessMessages;
    Form1.StatusBar1.Repaint;
    Form1.StatusBar1.Refresh;
  end;

  for i := 0 to Directorios.Count - 1 do
  begin
    pNombre := @PxFichero(Directorios[i])^.Nombre;
    Sector := PxFichero(Directorios[i])^.SectorIn;
    GenerarFileListRec(F, PxFichero(Directorios[i])^.DirHijo, Carpeta + pNombre + '\', Sector);
  end;

  Directorios.Free;
end;

function GenerarFileList(FileList: string): Boolean;
var
  F: TextFile;
begin
  AssignFile(F, FileList);
  ReWrite(F);
  Form1.ProgressBar1.Min := 0;
  Form1.ProgressBar1.Max := xIISO.Lista.Count;
  xIISO.Lista.Sort(CompararDirPadre);
  GenerarFileListRec(F, 0, '\', 0);
  CloseFile(F);
  Form1.ProgressBar1.Position := 0;
  Form1.StatusBar1.Repaint;
  Form1.StatusBar1.Refresh;
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);
begin
  if Listview1.Selected = nil then
  begin
    ExtraerFichero1.Enabled := False;
    ExtraerFicheroyEjecutar1.Enabled := False;
    Introducirfichero1.Enabled := False;
  end
  else
    if Listview1.Selected.ImageIndex = 0 then
    begin
      ExtraerFichero1.Enabled := True;
      ExtraerFicheroyEjecutar1.Enabled := False;
      Introducirfichero1.Enabled := False;
    end
    else
    begin
      ExtraerFichero1.Enabled := True;
      ExtraerFicheroyEjecutar1.Enabled := True;
      if (OrigenDatos = OD_IMAGEN) and (Listview1.SelCount = 1) then
      begin
        Introducirfichero1.Enabled := True;
        ExtraerFicheroyEjecutar1.Enabled := True;
      end
      else
        if (OrigenDatos = OD_DVD) and (Listview1.SelCount = 1) then
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

procedure TForm1.StatusBar1DrawPanel(StatusBar: TStatusBar;
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

procedure TForm1.ExtraerFicheroyEjecutar1Click(Sender: TObject);
var
  tamano, sector, i: integer;
  nombre: string;
  carpeta: string;
  HA, SCSI, LUN: byte;
  DirWindows: array[0..MAX_PATH] of char;
  DirTemp: string;
begin
  if Listview1.Selected = nil then exit;
  AsignarSCSIID(HA, SCSI, LUN);
  if GetWindowsDirectory(DirWindows, MAX_PATH) = 0 then exit;
  DirTemp := Trim(DirWindows) + '\Temp\';
  ProgressBar1.Min := 1;
  ProgressBar1.Max := Listview1.SelCount;
  for i := 0 to Listview1.Items.Count - 1 do
  begin
    if not Listview1.Items[i].Selected then continue;
    nombre := Listview1.Items[i].Caption;
    sector := StrToInt(Listview1.Items[i].SubItems[0]);
    tamano := StrToInt(Listview1.Items[i].SubItems[1]);
    case OrigenDatos of
      OD_IMAGEN: ExtraerFichero(NombreImagen, DirTemp + nombre, sector, tamano);
      OD_DVD: ExtraerFicheroXDVD(HA, SCSI, LUN, Unidad, carpeta + nombre, sector, tamano);
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

procedure TForm1.Introducirfichero1Click(Sender: TObject);
var
  Sector, Tam, Ent, Resto, a: integer;
  Imagen, Fichero: TFileStream;
  Buffer: array[0..2047] of byte;
begin
  if Listview1.Selected = nil then exit;
  if not SaveDialog2.Execute then exit;
  Sector := StrToInt(ListView1.Selected.SubItems[0]);
  Tam := StrToInt(Listview1.Selected.SubItems[1]);

  Imagen := TFileStream.Create(NombreImagen, fmOpenWrite);
  Fichero := TFileStream.Create(SaveDialog2.FileName, fmOpenRead);
  if (Imagen = nil) or (Fichero = nil) then exit;
  if Fichero.Size <> Tam then
  begin
    Fichero.Free;
    Imagen.Free;
    MessageBox(Handle, PChar(rcEngTamDiferentes), PChar(rcEngMensaje), MB_OK or MB_ICONINFORMATION);
    Exit;
  end;
  Imagen.Seek(Sector * 2048, soBeginning);

  Ent := (Fichero.Size div 2048) * 2048;
  Resto := Fichero.Size mod 2048;

  a := 0;
  if ent <> 0 then
    while (a < ent) do
    begin
      Fichero.Read(Buffer, 2048);
      Imagen.Write(Buffer, 2048);
      a := a + 2048;
    end;

  if Resto <> 0 then
  begin
    Fichero.Read(Buffer, Resto);
    Imagen.Write(Buffer, Resto);
  end;

  Imagen.Free;
  Fichero.Free;
  MessageBox(Handle, PChar(rcEngIntroducidoOK), PChar(rcEngMensaje), MB_OK or MB_ICONINFORMATION);
end;

procedure TForm1.Salir1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.XBEDevKitRetail1Click(Sender: TObject);
begin
  if not OpenDialog2.Execute then exit;
  if not ParchearXBE(OpenDialog2.FileName) then
    MessageBox(Form1.Handle, PChar(rcEngErrorAbrirXBE), PChar(rcEngError), MB_OK or MB_ICONERROR)
  else
    MessageBox(Form1.Handle, PChar(rcEngParcheadoOK), PChar(rcEngMensaje), MB_OK or MB_ICONINFORMATION);
end;

procedure CrearImagen(Carpeta: string; Fichero: string);
begin
  if Form3 = nil then
    Form3 := TForm3.Create(Form1);
  if (Carpeta <> '') then
    progreso.Carpeta := Carpeta;
  if (Fichero <> '') then
    Form3.SaveDialog1.FileName := Fichero;

  Form3.FormPadre := Form1;
  Form3.ShowModal;
  Form3.Free;
  Form3 := nil;
end;

procedure TForm1.Crea1Click(Sender: TObject);
begin
  if SelectDirectory(rcEngIntroduceDir, '', progreso.Carpeta) and SaveDialog1.Execute then
    CrearImagen(progreso.Carpeta, SaveDialog1.Filename);
end;

procedure TForm1.ToolButton2Click(Sender: TObject);
var
  tamano, sector, i: integer;
  nombre: string;
  carpeta: string;
  HA, SCSI, LUN: byte;
  Dir: PInteger;
begin
  if Listview1.Selected = nil then exit;
  AsignarSCSIID(HA, SCSI, LUN);
  if not SelectDirectory(PChar(rcEngIntroduceDirEx), '', carpeta) then exit;
  ProgressBar1.Min := 1;
  ProgressBar1.Max := Listview1.SelCount;
  for i := 0 to Listview1.Items.Count - 1 do
  begin
    if not Listview1.Items[i].Selected then continue;
    if Listview1.Items[i].ImageIndex = 0 then
    begin
      Dir := PInteger(Listview1.Items[i].Data);
      ExtraerCD(HA, SCSI, LUN, Dir^, Carpeta);
      Statusbar1.Repaint;
      Statusbar1.Refresh;
      Continue;
    end;
    nombre := Listview1.Items[i].Caption;
    sector := StrToInt(Listview1.Items[i].SubItems[0]);
    tamano := StrToInt(Listview1.Items[i].SubItems[1]);
    case OrigenDatos of
      OD_IMAGEN: ExtraerFichero(NombreImagen, carpeta + nombre, sector, tamano);
      OD_DVD: ExtraerFicheroXDVD(HA, SCSI, LUN, Unidad, carpeta + nombre, sector, tamano);
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
  MessageBox(Handle, PChar(rcEngFinExtraccion), PChar(rcEngMensaje), MB_OK or MB_ICONINFORMATION);
end;

procedure TForm1.ToolButton1Click(Sender: TObject);
var
  carpeta: string;
  HA, SCSI, LUN: byte;
begin
  if xIISO.Lista = nil then exit;
  if xIISO.Lista.Count = 0 then exit;
  AsignarSCSIID(HA, SCSI, LUN);
  if not SelectDirectory(PChar(rcEngIntroduceDirEx), '', carpeta) then exit;
  ProgressBar1.Min := 1;
  ProgressBar1.Max := xIISO.Lista.Count;
  ExtraerCD(HA, SCSI, LUN, 0, Carpeta);
  Unidad.SetCDSpeed(HA, SCSI, LUN, $FF, $FF);
  ProgressBar1.Position := 1;
  Statusbar1.Repaint;
  Statusbar1.Refresh;
  MessageBox(Handle, PChar(rcEngFinExtraccion), PChar(rcEngMensaje), MB_OK or MB_ICONINFORMATION);
end;

procedure TForm1.ToolButton4Click(Sender: TObject);
var
  pDirSector: PInteger;
  pNombre: PChar;
  i: integer;
  Nodo: TTreeNode;
  HA, SCSI, LUN: byte;
begin
  if (Unidad = nil) or (not Unidad.ASPISupport) then Exit;
        { CODIGO DE LECTURA DIRECTA DESDE DVD}
        { DIRECT DVD READING CODE}
  OrigenDatos := OD_DVD;
  AsignarSCSIID(HA, SCSI, LUN);
  if not LeerXDVD(HA, SCSI, LUN, Unidad) then
  begin
    MessageBox(Form1.Handle, PChar(rcEngDVDnoXBOX), Pchar(rcEngMensaje), MB_ICONINFORMATION or MB_OK);
    Exit;
  end;

  Treeview1.Items.BeginUpdate;
  Treeview1.Items.Clear;
  Nodo := Treeview1.Items.Add(nil, 'XDVD');
  Nodo.ImageIndex := 4; //0
  Nodo.SelectedIndex := 4; //1

  for i := 0 to xIISO.Lista.Count - 1 do
  begin
    if ((PxFichero(xIISO.Lista.Items[i])^.Atributo and XF_DIRECTORIO) = XF_DIRECTORIO) then
    begin
      new(pDirSector);
      pNombre := @PxFichero(xIISO.Lista.Items[i])^.Nombre;
      pDirSector^ := PxFichero(xIISO.Lista.Items[i])^.DirHijo;
      Nodo := Treeview1.Items.AddChildObject(Treeview1.Items[PxFichero(xIISO.Lista.Items[i])^.DirPadre], pNombre, pDirSector);
      Nodo.ImageIndex := 0;
      Nodo.SelectedIndex := 1;
    end;
  end;

  Treeview1.Items.EndUpdate;
  Treeview1.Items[0].Expand(True);
  Treeview1.Items[0].Selected := True;
  LeerFicheros(0);
  StatusBar1.Panels[0].Text := Format('%s: %d', [rcEngFicheros, xIISO.numFicheros]);
  StatusBar1.Panels[1].Text := Format('%s: %d', [rcEngCarpetas, xIISO.numCarpetas]);
  StatusBar1.Panels[2].Text := Format('%s: %d KB', [rcEngTamISO, xIISO.tamISO div 1024]);
end;

procedure TForm1.ToolButton8Click(Sender: TObject);
var
  sXBE: TXBE;
  s: PWideChar;
begin
  LeerXBE('c:\downloads\default.xbe', sXBE);
  s := @sXBE.Certificado.NombreJuego;
  showmessage(s);
end;

procedure TForm1.ToolButton10Click(Sender: TObject);
begin
  if xIISO.Lista = nil then exit;
  if xIISO.Lista.Count = 0 then exit;
  if not SaveDialog3.Execute then exit;
  GenerarFileList(SaveDialog3.Filename);
end;

procedure TForm1.Opciones1Click(Sender: TObject);
begin
  Form2.ShowModal;
end;

procedure TForm1.AcercadexISO2Click(Sender: TObject);
begin
  MessageBox(Handle, PChar('xISO' + #13 + #13 + rcEngDesarrollado + ' Yursoft.' + #13 + rcEngManejoASPI + ' Yursoft' + #13 + rcEngLecturaXBOX + ' Yursoft' + #13 + rcEngEscrituraXBOX + ' Yursoft'), PChar(rcEngAcercaDe), MB_OK or MB_ICONINFORMATION);
end;

procedure TForm1.Sitiooficial1Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://www.yursoft.com', nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.ToolButton5Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then exit;
  AbrirImagen(OpenDialog1.Filename);
end;

procedure TForm1.ExtensionesShell1Click(Sender: TObject);
var
  Reg: TRegistry;
  S, T: string;
begin
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_CLASSES_ROOT;
  Reg.OpenKey('Directory\shell\xISO', True);
  Reg.WriteString('', rcEngRegCrearXISO);
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
  Reg.WriteString('', rcEngRegExtraerXISO);
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
  Reg.WriteString('', rcEngRegExtraerConXISO);
  Reg.OpenKey('\' + S + '\shell\Extraer\command', True);
  if Reg.KeyExists('\' + S + '\Shell\open') then
    T := 'AbrirXISO'
  else
    T := 'open';
  Reg.WriteString('', ParamStr(0) + ' -x "%1" -f "%1"');
  Reg.OpenKey('\' + S + '\Shell\' + T, True);
  Reg.WriteString('', rcEngRegAbrirConXISO);
  Reg.OpenKey('\' + S + '\Shell\' + T + '\command', True);
  Reg.WriteString('', ParamStr(0) + ' -o "%1"');
  Reg.CloseKey;

  Reg.Free;
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);

//        'HKEY_CLASSES_ROOT\Directory\shell\xISO'
//        'HKEY_CLASSES_ROOT\xISOFILE'
end;

procedure TForm1.AadirsoporteISO96601Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then Exit;
  if not XDFS2ISO9660(OpenDialog1.Filename) then
    MessageBox(Form1.Handle, PChar(rcEngImagenNoXBOX), PChar(rcEngMensaje), MB_ICONINFORMATION or MB_OK)
  else
    MessageBox(Form1.Handle, PChar(rcEngISO9660ok), PChar(rcEngMensaje), MB_ICONINFORMATION or MB_OK);
end;

procedure TForm1.GrabarISO1Click(Sender: TObject);
begin
  if Form4 = nil then Form4 := TForm4.Create(nil);
  Form4.ShowModal;
  Form4.Free;
  Form4 := nil;
end;

procedure TForm1.Extraercarpeta1Click(Sender: TObject);
var
  carpeta: string;
  HA, SCSI, LUN: byte;
  Dir: PInteger;
begin
  if Treeview1.Selected = nil then exit;
  AsignarSCSIID(HA, SCSI, LUN);
  if not SelectDirectory(PChar(rcEngIntroduceDirEx), '', carpeta) then exit;
  ProgressBar1.Min := 1;
  ProgressBar1.Max := xIISO.Lista.Count;
  if Treeview1.Selected.AbsoluteIndex = 0 then
    ExtraerCD(HA, SCSI, LUN, 0, Carpeta)
  else
  begin
    Dir := PInteger(Treeview1.Selected.Data);
    ExtraerCD(HA, SCSI, LUN, Dir^, Carpeta);
  end;
  ProgressBar1.Position := 1;
  Statusbar1.Repaint;
  Statusbar1.Refresh;
end;

procedure TForm1.TreeView1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  TreeView1Click(Self);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if ProgressBar1 = nil then
  begin
    ProgressBar1 := TProgressBar.Create(self);
    ProgressBar1.Parent := StatusBar1;
    ProgressBar1.Position := 0;
    ProgressBar1.Step := 1;
  end;
end;

procedure TForm1.ToolButton13Click(Sender: TObject);
begin
  if Form5 = nil then Form5 := TForm5.Create(Self);
  Form5.ShowModal;
  Form5.Free;
  Form5 := nil;
end;

end.

