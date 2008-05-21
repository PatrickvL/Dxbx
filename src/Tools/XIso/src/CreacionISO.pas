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

unit CreacionISO;

interface

uses
  // Delphi
  Classes, SysUtils, Windows, Messages, Dialogs;

type
  PListaContenido = ^TListaContenido;
  PEntrada = ^TEntrada;

  TListaContenido = class(TList)
  private
    function Obtener(i: Integer): PEntrada;
    function Duplicado(Nombre: string): Boolean;
  public
    constructor Create;
    function Insertar(Nuevo: PEntrada): Boolean;
    procedure Eliminar(i: Integer); overload;
    procedure Eliminar(Entrada: PEntrada); overload;
    procedure Eliminar(Nombre: string); overload;
    property Entrada[i: Integer]: PEntrada read Obtener;
    function Cantidad: Integer;
  end;

  TEntrada = record
    Id: Integer;
    Nombre: string;
    FullPath: string;
    Tamano: Integer;
    FechaHora: TDateTime;
    Attributes: Integer;
    Contenido: TListaContenido;
    NivelAnterior: TListaContenido;
  end;

  TAdminFicheros = class(TObject)
  private
    Lista: TListaContenido;
    ListaAnterior: TListaContenido;
    UltimoID: Integer;
    function GenerarID: Integer;
    procedure AgregarEntradaNula(L: TListaContenido; LAnterior: PListaContenido);
    function AgregarCarpetaRec(Folder: string; LAnterior: PListaContenido): Boolean;
    procedure CantidadRec(L: TListaContenido; var N: Integer);
    function EsPadre(Padre: PEntrada; Hijo: PEntrada): Boolean;
  public
    ListaActual: TListaContenido;

    constructor Create;
    destructor Destroy; override;
    procedure Reiniciar;
    function AgregarFichero(Fichero: string): Boolean;
    function AgregarCarpeta(Folder: string): Boolean;
    function AgregarCarpetaNueva(Folder: string): Boolean;
    function EliminarFichero(Fichero: string): Boolean;
    function Buscar(const id: Integer; LContenido: TListaContenido): TListaContenido; overload;
    function Buscar(id: Integer): TListaContenido; overload;
    function Mover(ElementoOrigen: PEntrada; ElementoDestino: PEntrada): Boolean;
    function Avanzar(ID: Integer): Boolean; overload;
    function Avanzar(ListaNueva: TListaContenido): Boolean; overload;
    function Retroceder: Boolean;
    function Cantidad: Integer;
    function Raiz: TListaContenido;
    function EsDirectorio(Attributes: Integer): Boolean; overload;
    function EsDirectorio(Entrada: PEntrada): Boolean; overload;
  end;

implementation

constructor TListaContenido.Create;
begin
  inherited Create;
end;

function TListaContenido.Obtener(i: Integer): PEntrada;
begin
  Result := PEntrada(Self[i + 1]);
end;

function TListaContenido.Duplicado(Nombre: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  Nombre := LowerCase(Nombre);

  for i := 0 to Cantidad - 1 do
  begin
    if (Nombre = LowerCase(Entrada[i].Nombre)) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TListaContenido.Insertar(Nuevo: PEntrada): Boolean;
begin
  Result := True;
  if Duplicado(Nuevo.Nombre) then
  begin
    Result := False;
    Exit;
  end;

  Add(Nuevo);
end;

procedure TListaContenido.Eliminar(i: Integer);
begin
  Delete(i + 1);
end;

procedure TListaContenido.Eliminar(Entrada: PEntrada);
var
  i: Integer;
begin
  for i := 1 to Self.Count - 1 do
  begin
    if (Entrada = PEntrada(Items[i])) then
    begin
      Delete(i);
      Break;
    end;
  end;
end;

procedure TListaContenido.Eliminar(Nombre: string);
var
  i: Integer;
begin
  Nombre := LowerCase(Nombre);

  for i := 1 to Self.Count - 1 do
  begin
    if (Nombre = LowerCase(PEntrada(Items[i]).Nombre)) then
    begin
      Delete(i);
      Break;
    end;
  end;
end;

function TListaContenido.Cantidad: Integer;
begin
  Result := Count - 1;
end;

constructor TAdminFicheros.Create;
begin
  Lista := TListaContenido.Create;
  AgregarEntradaNula(Lista, @ListaActual);
  ListaActual := Lista;
  ListaAnterior := nil;
  UltimoID := 0;
  inherited Create;
end;

destructor TAdminFicheros.Destroy;
begin
  Lista.Free;

  inherited Destroy;
end;

procedure TAdminFicheros.AgregarEntradaNula(L: TListaContenido; LAnterior: PListaContenido);
var
  Nuevo: PEntrada;
begin
  New(Nuevo);
  Nuevo.Id := GenerarID();
  Nuevo.Nombre := '';
  Nuevo.FullPath := '';
  Nuevo.Tamano := 0;
  Nuevo.FechaHora := Date;
  Nuevo.Attributes := 0;
  Nuevo.Contenido := nil;
  Nuevo.NivelAnterior := LAnterior^;
  L.Insertar(Nuevo);
end;

function TAdminFicheros.GenerarID: Integer;
begin
  Inc(UltimoID);
  Result := UltimoID;
end;

procedure TAdminFicheros.Reiniciar;
begin
  Avanzar(Lista);
end;

function TAdminFicheros.AgregarFichero(Fichero: string): Boolean;
var
  Nuevo: PEntrada;
  F: TFileStream;
begin
  Result := False;
  if FileExists(Fichero) then
  begin
    New(Nuevo);
    Nuevo.Id := GenerarID();
    Nuevo.Nombre := ExtractFileName(Fichero);
    Nuevo.FullPath := Fichero;
    Nuevo.Attributes := GetFileAttributes(PChar(Fichero));
    Nuevo.FechaHora := FileDateToDateTime(FileAge(Fichero));
    F := TFileStream.Create(Fichero, fmOpenRead);
    Nuevo.Tamano := F.Size;
    F.Free;
    Nuevo.Contenido := nil;
    Nuevo.NivelAnterior := ListaActual;
    ListaActual.Insertar(Nuevo);
    Result := True;
  end;
end;

{$WARN SYMBOL_PLATFORM OFF}

function TAdminFicheros.AgregarCarpetaRec(Folder: string; LAnterior: PListaContenido): Boolean;
var
  SR: TSearchRec;
  Nuevo: PEntrada;
  C: TListaContenido;
begin
  Result := False;
  if Folder[Length(Folder)] = '\' then
    Folder[Length(Folder)] := ' ';

  Folder := Trim(Folder);

  if DirectoryExists(Folder) then
  begin
          // Si no estamos introduciendo una unidad.
    if Folder[Length(Folder)] <> ':' then
    begin
      New(Nuevo);
      Nuevo.Id := GenerarID();
      Nuevo.Nombre := ExtractFileName(Folder);
      Nuevo.FullPath := Folder + '\';
      Nuevo.Attributes := GetFileAttributes(PChar(Folder + '\'));
      Nuevo.FechaHora := 0;
      Nuevo.Tamano := 0;
      C := TListaContenido.Create;
      Nuevo.Contenido := C;
      Nuevo.NivelAnterior := LAnterior^;

      AgregarEntradaNula(C, LAnterior);
      LAnterior.Insertar(Nuevo);
    end
    else
    begin
      C := ListaActual;
    end;

    if (FindFirst(Folder + '\*.*', faArchive or faDirectory or faHidden or faSysFile or faReadOnly, SR) = 0) then
    begin
      repeat
        if (SR.Name[1] = '.') then
          Continue;
          
        New(Nuevo);
        Nuevo.Id := GenerarID();
        Nuevo.Nombre := SR.Name;
        Nuevo.FullPath := Folder + '\' + SR.Name;
        Nuevo.Attributes := GetFileAttributes(PChar(Folder + '\' + SR.Name));
        Nuevo.FechaHora := FileDateToDateTime(SR.Time);
        Nuevo.Tamano := SR.Size;
        Nuevo.Contenido := nil;
        Nuevo.NivelAnterior := LAnterior^;
        if Nuevo.Attributes and FILE_ATTRIBUTE_DIRECTORY = FILE_ATTRIBUTE_DIRECTORY then
          AgregarCarpetaRec(Folder + '\' + SR.Name, @C)
        else
          C.Insertar(Nuevo);
      until (FindNext(SR) <> 0);
    end;
  end;
end;

{$WARN SYMBOL_PLATFORM ON}


function TAdminFicheros.AgregarCarpeta(Folder: string): Boolean;
begin
  if ListaAnterior = nil then
    AgregarCarpetaRec(Folder, @Lista)
  else
    AgregarCarpetaRec(Folder, @ListaActual);
  Result := True;
end;

function TAdminFicheros.AgregarCarpetaNueva(Folder: string): Boolean;
var
  Nuevo: PEntrada;
  C: TListaContenido;
begin
  New(Nuevo);
  Nuevo.Id := GenerarID();
  Nuevo.Nombre := ExtractFileName(Folder);
  Nuevo.FullPath := Folder;
  Nuevo.Attributes := FILE_ATTRIBUTE_DIRECTORY;
  Nuevo.FechaHora := Date;
  Nuevo.Tamano := 0;
  C := TListaContenido.Create;
  Nuevo.Contenido := C;
  Nuevo.NivelAnterior := ListaActual;
  AgregarEntradaNula(C, @ListaActual);
  ListaActual.Insertar(Nuevo);
  Result := True;
end;

function TAdminFicheros.EliminarFichero(Fichero: string): Boolean;
begin
  ListaActual.Eliminar(Fichero);
  Result := True;
end;

function TAdminFicheros.Buscar(const id: Integer; LContenido: TListaContenido): TListaContenido;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to LContenido.Cantidad - 1 do
  begin
    if Result <> nil then
      Exit;

    if LContenido.Entrada[i].Id = id then
    begin
      Result := LContenido;
      Exit;
    end;

    if EsDirectorio(LContenido.Entrada[i].Attributes) then
      Result := Buscar(id, LContenido.Entrada[i].Contenido);
  end;
end;

function TAdminFicheros.Buscar(id: Integer): TListaContenido;
begin
  Result := Buscar(id, Lista);
end;

function TAdminFicheros.Mover(ElementoOrigen: PEntrada; ElementoDestino: PEntrada): Boolean;
var
  Origen, Destino: TListaContenido;
begin
  Result := False;
  if ElementoOrigen = ElementoDestino then
    Exit;

  if (EsDirectorio(ElementoOrigen)) and EsPadre(ElementoOrigen, ElementoDestino) then
    Exit;

  Origen := Buscar(ElementoOrigen.Id);
  Destino := ElementoDestino.Contenido;

  if (Origen = nil) or (Destino = nil) then
    Exit;

  if not Destino.Insertar(ElementoOrigen) then
    Exit;

  Origen.Eliminar(ElementoOrigen);

  Result := True;
end;

function TAdminFicheros.Avanzar(ID: Integer): Boolean;
var
  i: Integer;
  Entrada: PEntrada;
begin
  Result := False;
  for i := 0 to ListaActual.Cantidad - 1 do
  begin
    Entrada := ListaActual.Entrada[i];
    if Entrada.Id = ID then
    begin
      if Entrada.Contenido <> nil then
      begin
        ListaAnterior := ListaActual;
        ListaActual := Entrada.Contenido;
        Result := True;
      end;
      Break;
    end;
  end;
end;

function TAdminFicheros.Avanzar(ListaNueva: TListaContenido): Boolean;
begin
  ListaAnterior := ListaNueva.Entrada[-1].NivelAnterior;
  ListaActual := ListaNueva;
  Result := True;
end;

function TAdminFicheros.Retroceder: Boolean;
var
  P: TListaContenido;
begin
  Result := False;
  if ListaAnterior = nil then
    Exit;

  P := ListaActual.Entrada[-1].NivelAnterior;
  ListaAnterior := P.Entrada[-1].NivelAnterior;
  ListaActual := P;

  Result := True;
end;

procedure TAdminFicheros.CantidadRec(L: TListaContenido; var N: Integer);
var
  i: Integer;
begin
  for i := 0 to L.Cantidad - 1 do
  begin
    N := N + 1;
    if EsDirectorio(L.Entrada[i]) then
      CantidadRec(L.Entrada[i].Contenido, N);
  end;
end;

function TAdminFicheros.Cantidad: Integer;
begin
  Result := 0;
  CantidadRec(Raiz(), Result);
end;

function TAdminFicheros.Raiz: TListaContenido;
begin
  Result := Lista;
end;

function TAdminFicheros.EsDirectorio(Attributes: Integer): Boolean;
begin
  if Attributes and FILE_ATTRIBUTE_DIRECTORY = FILE_ATTRIBUTE_DIRECTORY then
    Result := True
  else
    Result := False;
end;

function TAdminFicheros.EsDirectorio(Entrada: PEntrada): Boolean;
begin
  Result := EsDirectorio(Entrada.Attributes);
end;

function TAdminFicheros.EsPadre(Padre: PEntrada; Hijo: PEntrada): Boolean;
begin
  Result := False;
  if (Padre = nil) or (Hijo = nil) then
    Exit;

  if Padre.Contenido.Entrada[-1].Id = Hijo.Id then
    Result := True
  else
  begin
    if Hijo.NivelAnterior <> nil then
      Result := EsPadre(Padre, Hijo.NivelAnterior.Entrada[-1]);
  end;
end;

end.
 
