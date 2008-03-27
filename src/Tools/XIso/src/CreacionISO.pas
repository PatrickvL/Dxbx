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

uses Classes, SysUtils, Windows, Messages, Dialogs;

type


  PListaContenido = ^TListaContenido;
  PEntrada = ^TEntrada;

  TListaContenido = class(TList)
  private
    function Obtener(i: integer): PEntrada;
    function Duplicado(Nombre: string): boolean;
  public
    constructor Create;
    function Insertar(Nuevo: PEntrada): boolean;
    procedure Eliminar(i: integer); overload;
    procedure Eliminar(Entrada: PEntrada); overload;
    procedure Eliminar(Nombre: string); overload;
    property Entrada[i: integer]: PEntrada read Obtener;
    function Cantidad: integer;
  end;

  TEntrada = record
    Id: integer;
    Nombre: string;
    RutaCompleta: string;
    Tamano: integer;
    FechaHora: TDateTime;
    Atributos: integer;
    Contenido: TListaContenido;
    NivelAnterior: TListaContenido;
  end;

  TAdminFicheros = class
  private
    Lista: TListaContenido;
    ListaAnterior: TListaContenido;
    UltimoID: integer;
    function GenerarID: integer;
    procedure AgregarEntradaNula(L: TListaContenido; LAnterior: PListaContenido);
    function AgregarCarpetaRec(Carpeta: string; LAnterior: PListaContenido): boolean;
    procedure CantidadRec(L: TListaContenido; var N: integer);
    function EsPadre(Padre: PEntrada; Hijo: PEntrada): boolean;
  public
    ListaActual: TListaContenido;

    constructor Create;
    destructor Destroy; override;
    procedure Reiniciar;
    function AgregarFichero(Fichero: string): boolean;
    function AgregarCarpeta(Carpeta: string): boolean;
    function AgregarCarpetaNueva(Carpeta: string): boolean;
    function EliminarFichero(Fichero: string): boolean;
    function Buscar(const id: integer; LContenido: TListaContenido): TListaContenido; overload;
    function Buscar(id: integer): TListaContenido; overload;
    function Mover(ElementoOrigen: PEntrada; ElementoDestino: PEntrada): boolean;
    function Avanzar(ID: integer): boolean; overload;
    function Avanzar(ListaNueva: TListaContenido): boolean; overload;
    function Retroceder: boolean;
    function Cantidad: integer;
    function Raiz: TListaContenido;
    function EsDirectorio(Atributos: integer): boolean; overload;
    function EsDirectorio(Entrada: PEntrada): boolean; overload;
  end;

implementation

constructor TListaContenido.Create;
begin
  inherited;
end;

function TListaContenido.Obtener(i: integer): PEntrada;
begin
  Result := PEntrada(Self[i + 1]);
end;

function TListaContenido.Duplicado(Nombre: string): boolean;
var
  i: integer;
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

function TListaContenido.Insertar(Nuevo: PEntrada): boolean;
begin
  Result := True;
  if Duplicado(Nuevo.Nombre) then
  begin
    Result := False;
    Exit;
  end;
  Add(Nuevo);
end;

procedure TListaContenido.Eliminar(i: integer);
begin
  Delete(i + 1);
end;

procedure TListaContenido.Eliminar(Entrada: PEntrada);
var
  i: integer;
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
  i: integer;
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

function TListaContenido.Cantidad: integer;
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
  inherited;
end;

destructor TAdminFicheros.Destroy;
begin
  Lista.Free;
  inherited;
end;

procedure TAdminFicheros.AgregarEntradaNula(L: TListaContenido; LAnterior: PListaContenido);
var
  Nuevo: PEntrada;
begin
  New(Nuevo);
  Nuevo.Id := GenerarID();
  Nuevo.Nombre := '';
  Nuevo.RutaCompleta := '';
  Nuevo.Tamano := 0;
  Nuevo.FechaHora := Date;
  Nuevo.Atributos := 0;
  Nuevo.Contenido := nil;
  Nuevo.NivelAnterior := LAnterior^;
  L.Insertar(Nuevo);
end;

function TAdminFicheros.GenerarID: integer;
begin
  Inc(UltimoID);
  Result := UltimoID;
end;

procedure TAdminFicheros.Reiniciar;
begin
  Avanzar(Lista);
end;

function TAdminFicheros.AgregarFichero(Fichero: string): boolean;
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
    Nuevo.RutaCompleta := Fichero;
    Nuevo.Atributos := GetFileAttributes(PChar(Fichero));
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

function TAdminFicheros.AgregarCarpetaRec(Carpeta: string; LAnterior: PListaContenido): boolean;
var
  SR: TSearchRec;
  Nuevo: PEntrada;
  C: TListaContenido;
begin
  if Carpeta[Length(Carpeta)] = '\' then
    Carpeta[Length(Carpeta)] := ' ';

  Carpeta := Trim(Carpeta);

  if DirectoryExists(Carpeta) then
  begin
          // Si no estamos introduciendo una unidad.
    if Carpeta[Length(Carpeta)] <> ':' then
    begin
      New(Nuevo);
      Nuevo.Id := GenerarID();
      Nuevo.Nombre := ExtractFileName(Carpeta);
      Nuevo.RutaCompleta := Carpeta + '\';
      Nuevo.Atributos := GetFileAttributes(PChar(Carpeta + '\'));
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

    if (FindFirst(Carpeta + '\*.*', faArchive or faDirectory or faHidden or faSysFile or faReadOnly, SR) = 0) then
    begin
      repeat
        if (SR.Name[1] = '.') then Continue;
        New(Nuevo);
        Nuevo.Id := GenerarID();
        Nuevo.Nombre := SR.Name;
        Nuevo.RutaCompleta := Carpeta + '\' + SR.Name;
        Nuevo.Atributos := GetFileAttributes(PChar(Carpeta + '\' + SR.Name));
        Nuevo.FechaHora := FileDateToDateTime(SR.Time);
        Nuevo.Tamano := SR.Size;
        Nuevo.Contenido := nil;
        Nuevo.NivelAnterior := LAnterior^;
        if Nuevo.Atributos and FILE_ATTRIBUTE_DIRECTORY = FILE_ATTRIBUTE_DIRECTORY then
          AgregarCarpetaRec(Carpeta + '\' + SR.Name, @C)
        else
          C.Insertar(Nuevo);
      until (FindNext(SR) <> 0);
    end;
  end;
end;

function TAdminFicheros.AgregarCarpeta(Carpeta: string): boolean;
begin
  Result := False;
  if ListaAnterior = nil then
    AgregarCarpetaRec(Carpeta, @Lista)
  else
    AgregarCarpetaRec(Carpeta, @ListaActual);
  Result := True;
end;

function TAdminFicheros.AgregarCarpetaNueva(Carpeta: string): boolean;
var
  Nuevo: PEntrada;
  C: TListaContenido;
begin
  Result := False;
  New(Nuevo);
  Nuevo.Id := GenerarID();
  Nuevo.Nombre := ExtractFileName(Carpeta);
  Nuevo.RutaCompleta := Carpeta;
  Nuevo.Atributos := FILE_ATTRIBUTE_DIRECTORY;
  Nuevo.FechaHora := Date;
  Nuevo.Tamano := 0;
  C := TListaContenido.Create;
  Nuevo.Contenido := C;
  Nuevo.NivelAnterior := ListaActual;
  AgregarEntradaNula(C, @ListaActual);
  ListaActual.Insertar(Nuevo);
  Result := True;
end;

function TAdminFicheros.EliminarFichero(Fichero: string): boolean;
begin
  Result := False;
  ListaActual.Eliminar(Fichero);
  Result := True;
end;

function TAdminFicheros.Buscar(const id: integer; LContenido: TListaContenido): TListaContenido;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to LContenido.Cantidad - 1 do
  begin
    if Result <> nil then Exit;

    if LContenido.Entrada[i].Id = id then
    begin
      Result := LContenido;
      Exit;
    end;

    if EsDirectorio(LContenido.Entrada[i].Atributos) then
    begin
      Result := Buscar(id, LContenido.Entrada[i].Contenido);
    end;
  end;
end;

function TAdminFicheros.Buscar(id: integer): TListaContenido;
begin
  Result := Buscar(id, Lista);
end;

function TAdminFicheros.Mover(ElementoOrigen: PEntrada; ElementoDestino: PEntrada): boolean;
var
  Origen, Destino: TListaContenido;
begin
  Result := False;

  if ElementoOrigen = ElementoDestino then Exit;
  if (EsDirectorio(ElementoOrigen)) and EsPadre(ElementoOrigen, ElementoDestino) then Exit;

  Origen := Buscar(ElementoOrigen.Id);
  Destino := ElementoDestino.Contenido;

  if (Origen = nil) or (Destino = nil) then Exit;
  if not Destino.Insertar(ElementoOrigen) then Exit;
  Origen.Eliminar(ElementoOrigen);

  Result := True;
end;

function TAdminFicheros.Avanzar(ID: integer): boolean;
var
  i: integer;
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

function TAdminFicheros.Avanzar(ListaNueva: TListaContenido): boolean;
begin
  ListaAnterior := ListaNueva.Entrada[-1].NivelAnterior;
  ListaActual := ListaNueva;
  Result := true;
end;

function TAdminFicheros.Retroceder: boolean;
var
  P: TListaContenido;
begin
  Result := False;
  if (ListaAnterior = nil) then Exit;

  P := ListaActual.Entrada[-1].NivelAnterior;
  ListaAnterior := P.Entrada[-1].NivelAnterior;
  ListaActual := P;

  Result := True;
end;

procedure TAdminFicheros.CantidadRec(L: TListaContenido; var N: integer);
var
  i: integer;
begin
  for i := 0 to L.Cantidad - 1 do
  begin
    N := N + 1;
    if EsDirectorio(L.Entrada[i]) then
      CantidadRec(L.Entrada[i].Contenido, N);
  end;
end;

function TAdminFicheros.Cantidad: integer;
begin
  Result := 0;
  CantidadRec(Raiz(), Result);
end;

function TAdminFicheros.Raiz: TListaContenido;
begin
  Result := Lista;
end;

function TAdminFicheros.EsDirectorio(Atributos: integer): boolean;
begin
  if Atributos and FILE_ATTRIBUTE_DIRECTORY = FILE_ATTRIBUTE_DIRECTORY then
    Result := True
  else
    Result := False;
end;

function TAdminFicheros.EsDirectorio(Entrada: PEntrada): boolean;
begin
  Result := EsDirectorio(Entrada.Atributos);
end;

function TAdminFicheros.EsPadre(Padre: PEntrada; Hijo: PEntrada): boolean;
begin
  Result := False;
  if (Padre = nil) or (Hijo = nil) then Exit;
  if Padre.Contenido.Entrada[-1].Id = Hijo.Id then
    Result := True
  else
  begin
    if (Hijo.NivelAnterior <> nil) then
      Result := EsPadre(Padre, Hijo.NivelAnterior.Entrada[-1]);
  end;
end;

end.
 
