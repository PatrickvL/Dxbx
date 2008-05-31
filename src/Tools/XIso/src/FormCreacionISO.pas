{
   xISO
   Copyright 1984, 1986, 1989, 1992, 2000, 2001, 2002
   Free Software Foundation, Inc.

   This file is part of xISO, made it by Yursoft.com

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


unit FormCreacionISO;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CreacionISO, ComCtrls, StdCtrls, Menus, {FolderBrowser, } Buttons,
  ShellAPI, ImgList, ToolWin, ExtCtrls,
  // Dxbx
  TextConsts,
  xisomakerv2,
  ProgresoCreacionISO;


type
  TForm5 = class(TForm)
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    Eliminar1: TMenuItem;
    Nuevacarpeta1: TMenuItem;
//    FolderBrowser1: TFolderBrowser;
    ImageList1: TImageList;
    Aadircarpeta1: TMenuItem;
    Aadirficheros1: TMenuItem;
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    N1: TMenuItem;
    MenuItemSave: TMenuItem;
    Opciones1: TMenuItem;
    N2: TMenuItem;
    MenuItemExit: TMenuItem;
    Panel1: TPanel;
    TreeViewDirectorios: TTreeView;
    ListviewFicheros: TListView;
    Splitter1: TSplitter;
    CoolBar1: TCoolBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolBar2: TToolBar;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    SaveDialog1: TSaveDialog;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure Eliminar1Click(Sender: TObject);
    procedure Nuevacarpeta1Click(Sender: TObject);
    procedure ListviewFicherosDblClick(Sender: TObject);
    procedure ListviewFicherosCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure Aadirficheros1Click(Sender: TObject);
    procedure Aadircarpeta1Click(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure TreeViewDirectoriosClick(Sender: TObject);
    procedure TreeViewDirectoriosKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ListviewFicherosCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure TreeViewDirectoriosDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure TreeViewDirectoriosDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure TreeViewDirectoriosEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure ToolButton3Click(Sender: TObject);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);
    procedure MenuItemSaveClick(Sender: TObject);
  private
    Etiqueta: string;
    procedure ActualizarDirectoriosRec(Padre: TTreeNode; Lista: TListaContenido);
    procedure FinCreacion(Sender: TObject);
    procedure WMDROPFILES(var msg: TMessage); message WM_DROPFILES;
  public
    ProgressBar1: TProgressBar;
    Manager: TAdminFicheros;
    procedure ActualizarLista;
    procedure ActualizarDirectorios;
  end;

var
  Form5: TForm5;

implementation

{$R *.dfm}

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

procedure TForm5.ActualizarLista;
var
  i: Integer;
  Fila: TListItem;
  Entrada: PEntrada;
  attrs: string;
begin
  if Manager.ListaActual = nil then
    Exit;

  ListviewFicheros.Items.BeginUpdate;
  ListviewFicheros.Clear;
  for i := 0 to Manager.ListaActual.Cantidad - 1 do
  begin
    attrs := '';
    Entrada := Manager.ListaActual.Entrada[i];
    Fila := ListviewFicheros.Items.Add;
    Fila.Data := Entrada;
    Fila.Caption := Entrada.Nombre;
    Fila.SubItems.Add(IntToStr(Entrada.Tamano));
    Fila.SubItems.Add(DateTimeToStr(Entrada.FechaHora));

    if Entrada.Attributes and FILE_ATTRIBUTE_ARCHIVE = FILE_ATTRIBUTE_ARCHIVE then
    begin
      attrs := attrs + 'F';
      Fila.ImageIndex := 6;
    end
    else
      if Entrada.Attributes and FILE_ATTRIBUTE_NORMAL = FILE_ATTRIBUTE_NORMAL then
      begin
        attrs := attrs + 'F';
        Fila.ImageIndex := 6;
      end
      else
        if Entrada.Attributes and FILE_ATTRIBUTE_DIRECTORY = FILE_ATTRIBUTE_DIRECTORY then
        begin
          attrs := attrs + 'D';
          Fila.ImageIndex := 0;
        end
        else
        begin
          attrs := attrs + 'F';
          Fila.ImageIndex := 6;
        end;
    if Entrada.Attributes and FILE_ATTRIBUTE_HIDDEN = FILE_ATTRIBUTE_HIDDEN then attrs := attrs + 'H';
    if Entrada.Attributes and FILE_ATTRIBUTE_READONLY = FILE_ATTRIBUTE_READONLY then attrs := attrs + 'R';
    if Entrada.Attributes and FILE_ATTRIBUTE_SYSTEM = FILE_ATTRIBUTE_SYSTEM then attrs := attrs + 'S';
    if Entrada.Attributes and FILE_ATTRIBUTE_COMPRESSED = FILE_ATTRIBUTE_COMPRESSED then attrs := attrs + 'C';

    Fila.SubItems.Add(attrs);
  end;

  ListviewFicheros.Items.EndUpdate;
end;

procedure TForm5.ActualizarDirectoriosRec(Padre: TTreeNode; Lista: TListaContenido);
var
  i: Integer;
  Entrada: PEntrada;
  Hijo: TTreeNode;
begin
  for i := 0 to Lista.Cantidad - 1 do
  begin
    Entrada := Lista.Entrada[i];
    if Entrada.Attributes and FILE_ATTRIBUTE_DIRECTORY = FILE_ATTRIBUTE_DIRECTORY then
    begin
      if Entrada.Contenido = nil then
        Break;
        
      Hijo := TreeViewDirectorios.Items.AddChildObject(Padre, Entrada.Nombre, Entrada);
      Hijo.SelectedIndex := 1;
      ActualizarDirectoriosRec(Hijo, Entrada.Contenido);
    end;
  end;
end;

procedure TForm5.ActualizarDirectorios;
var
  Raiz: TTreeNode;
  i: Integer;
  Entrada, Entrada2: PEntrada;
begin
  if (TreeviewDirectorios.Selected <> nil) and (TreeviewDirectorios.Selected.Data <> nil) then
    Entrada := PEntrada(TreeviewDirectorios.Selected.Data)
  else
    Entrada := nil;

  TreeviewDirectorios.Items.BeginUpdate;
  TreeviewDirectorios.Items.Clear;
  if Etiqueta <> SEtiqueta then
    Raiz := TreeviewDirectorios.Items.Add(nil, Etiqueta)
  else
    Raiz := TreeviewDirectorios.Items.Add(nil, SEtiqueta);
  Raiz.ImageIndex := 3;
  Raiz.SelectedIndex := 3;
  ActualizarDirectoriosRec(Raiz, Manager.Raiz);
  TreeviewDirectorios.FullExpand;

  if Entrada <> nil then
    for i := 1 to TreeviewDirectorios.Items.Count - 1 do
    begin
      if TreeviewDirectorios.Items[i] = nil then
        Continue;

      Entrada2 := PEntrada(TreeviewDirectorios.Items[i].Data);
      if Entrada.Id = Entrada2.Id then
      begin
        TreeviewDirectorios.Items[i].Selected := True;
        Break;
      end;
    end;

  TreeviewDirectorios.AlphaSort(True);
  TreeviewDirectorios.Items.EndUpdate;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  Manager := TAdminFicheros.Create;
  DragAcceptFiles(Self.Handle, True);
  Etiqueta := SEtiqueta;
end;

procedure TForm5.Eliminar1Click(Sender: TObject);
var
  i: Integer;
begin
  for i := ListviewFicheros.Items.Count - 1 downto 0 do
  begin
    if not ListviewFicheros.Items[i].Selected then
      Continue;

    Manager.EliminarFichero(ListviewFicheros.Items[i].Caption);
    ListviewFicheros.Items.Delete(i);
  end;
  
  ActualizarLista();
  ActualizarDirectorios();
end;

procedure TForm5.Nuevacarpeta1Click(Sender: TObject);
var
  s: string;
  i: Integer;
begin
  if not InputQuery(SNuevaCarpeta, SNombreCarpeta, s) then
    Exit;
    
  for i := 1 to Length(s) do
    if s[i] in ['\', '/', ':', '*', '?', '<', '>', '|'] then
      Exit;

  Manager.AgregarCarpetaNueva(s);
  ActualizarLista();
  ActualizarDirectorios();
end;

procedure TForm5.ListviewFicherosDblClick(Sender: TObject);
var
  Entrada, Entrada2: PEntrada;
  i: Integer;
begin
  if ListviewFicheros.Selected = nil then
    Exit;

  Entrada := PEntrada(ListviewFicheros.Selected.Data);
  if Entrada.Attributes and FILE_ATTRIBUTE_DIRECTORY = FILE_ATTRIBUTE_DIRECTORY then
  begin
    Manager.Avanzar(Entrada.Id);
    ActualizarLista();
    for i := 1 to TreeviewDirectorios.Items.Count - 1 do
    begin
      if TreeviewDirectorios.Items[i] = nil then
        Continue;
        
      Entrada2 := PEntrada(TreeviewDirectorios.Items[i].Data);
      if (Entrada.Id = Entrada2.Id) then
      begin
        TreeviewDirectorios.Items[i].Selected := True;
        Break;
      end;
    end;
  end
end;

procedure TForm5.ListviewFicherosCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
{var
   Icono: TIcon;
   R,T: TRect;  }
begin
     {   Icono := TIcon.Create;
        Icono.Handle := GetAssociatedIcon(ExtractFileExt(Item.Caption),True);
        R := Item.DisplayRect(drIcon);
        R.Right := 9;
        R.Bottom := 9;
        R.BottomRight.X := R.Left+R.Right;
        R.BottomRight.Y := R.Top+R.Bottom;
        Sender.Canvas.StretchDraw(R,Icono);
        T := Item.DisplayRect(drLabel);
        T.Left := R.Right+T.Left;
        Sender.Canvas.TextRect(T,T.Left,T.Top,Item.Caption);

        DefaultDraw := False;      }
  Sender.Canvas.Brush.Color := $F7F7F7;
end;

procedure TForm5.Aadirficheros1Click(Sender: TObject);
var
  i: Integer;
begin
  if not OpenDialog1.Execute then
    Exit;

  for i := 0 to OpenDialog1.Files.Count - 1 do
    Manager.AgregarFichero(OpenDialog1.Files[i]);

  ActualizarLista();
  ActualizarDirectorios();
end;

procedure TForm5.Aadircarpeta1Click(Sender: TObject);
begin
{
  if not FolderBrowser1.Execute then
    Exit;

  Manager.AgregarCarpeta(FolderBrowser1.Folder);
  ActualizarLista();
  ActualizarDirectorios();}
end;

procedure TForm5.WMDROPFILES(var msg: TMessage);
var
  dr: HDrop;
  nb, j: Integer;
  fn: array[0..254] of Char;
begin
  dr := msg.wparam;
  nb := DragQueryFile(dr, $FFFFFFFF, fn, sizeof(fn));
  for j := 0 to nb - 1 do
  begin
    DragQueryFile(dr, j, fn, sizeof(fn));
    Manager.AgregarFichero(fn);
    Manager.AgregarCarpeta(fn);
  end;
  DragFinish(dr);
  ActualizarLista();
end;

procedure TForm5.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm5.TreeViewDirectoriosClick(Sender: TObject);
var
  Entrada: PEntrada;
begin
  if TreeviewDirectorios.Selected = nil then
    Exit;

  if TreeviewDirectorios.Selected.AbsoluteIndex = 0 then
    Manager.Avanzar(Manager.Raiz)
  else
  begin
    Entrada := PEntrada(TreeviewDirectorios.Selected.Data);
    Manager.Avanzar(Entrada.Contenido);
  end;

  ActualizarLista();
end;

procedure TForm5.TreeViewDirectoriosKeyPress(Sender: TObject;
  var Key: Char);
begin
  TreeViewDirectoriosClick(Self);
end;

procedure TForm5.FormShow(Sender: TObject);
begin
  if ProgressBar1 = nil then
  begin
    ProgressBar1 := TProgressBar.Create(self);
    ProgressBar1.Parent := StatusBar1;
    ProgressBar1.Position := 0;
    ProgressBar1.Step := 1;
  end;
  ActualizarLista();
  ActualizarDirectorios();
end;

procedure TForm5.ToolButton1Click(Sender: TObject);
begin
  if Manager.Raiz = nil then
    Exit;

  Manager.Retroceder();
  ActualizarLista();
  if (TreeviewDirectorios.Selected <> nil) and (TreeviewDirectorios.Selected.Parent <> nil) then
    TreeviewDirectorios.Selected.Parent.Selected := True;
end;

procedure TForm5.ListviewFicherosCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  Sender.Canvas.Brush.Color := $FFFFFF;
end;

procedure TForm5.TreeViewDirectoriosDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Nodo: TTreeNode;
  Entrada: PEntrada;
  i: Integer;
begin
  Nodo := TreeviewDirectorios.GetNodeAt(X, Y);
  if Nodo = nil then
    Exit;

  // Comprobamos si han soltado el elemento a mover encima del nodo raiz.
  if Nodo.Data = nil then
    Entrada := Manager.Raiz.Entrada[-1]
  else
    Entrada := PEntrada(Nodo.Data);

  if Source.ClassNameIs('TListview') then
  begin
    if TListview(Source).SelCount = 1 then
    begin
      if not Manager.Mover(PEntrada(TListview(Source).Selected.Data), Entrada) then
        Exit;
    end
    else
    begin
      for i := 0 to TListview(Source).Items.Count - 1 do
      begin
        if not TListview(Source).Items[i].Selected then
          Continue;
          
        if not Manager.Mover(PEntrada(TListview(Source).Items[i].Data), Entrada) then
          Continue;

      end;
    end;
  end
  else
    if Source.ClassNameIs('TTreeview') then
    begin
      if TTreeview(Source).Selected.Data = nil then
        Exit;
        
      if not Manager.Mover(PEntrada(TTreeview(Source).Selected.Data), Entrada) then
        Exit;
    end;

  ActualizarLista();
  ActualizarDirectorios();
end;

procedure TForm5.TreeViewDirectoriosDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Source.ClassNameIs('TListView') or Source.ClassNameIs('TTreeview') then
    Accept := True;
end;

procedure TForm5.TreeViewDirectoriosEdited(Sender: TObject;
  Node: TTreeNode; var S: string);
var
  Entrada: PEntrada;
begin
  if Node.Data = nil then
    Etiqueta := S
  else
  begin
    Entrada := PEntrada(Node.Data);
    Entrada.Nombre := S;
  end;
end;

procedure TForm5.ToolButton3Click(Sender: TObject);
begin
  Manager.Free;
  Manager := TAdminFicheros.Create;
  Etiqueta := SEtiqueta;
  ActualizarLista();
  ActualizarDirectorios();
end;

procedure TForm5.FinCreacion(Sender: TObject);
begin
  if Estado then
    MessageBox(Form5.Handle, PChar(SFinCreacion), 'xISO', MB_OK or MB_ICONINFORMATION);
    
  Form5.Enabled := True;
  if ProgressBar1 <> nil then
    ProgressBar1.Position := 0;
end;

procedure TForm5.StatusBar1DrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  R: TRect;
begin
  if (Panel.ID = 1) and (Progressbar1 <> nil) then
  begin
    R := Rect;
    R.Right := R.Right;
    R.Top := R.Top - 1;
    R.Bottom := R.Bottom + 1;
    ProgressBar1.BoundsRect := R;
  end;
end;

procedure TForm5.MenuItemSaveClick(Sender: TObject);
var
  Hilo: TProgresoCreacionISO;
begin
  if ProgressBar1 <> nil then
  begin
    ProgressBar1.Min := 0;
    ProgressBar1.Max := Manager.Cantidad;
  end;
  Hilo := TProgresoCreacionISO.Create(True);
  Hilo.OnTerminate := FinCreacion;
  Form5.Enabled := False;
  Hilo.Resume;
  Hilo.FreeOnTerminate := True;
end;

end.
