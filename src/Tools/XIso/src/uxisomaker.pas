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


unit uxisomaker;

interface

uses
  // Delphi
  Windows, Messages, Classes, SysUtils, Dialogs,
  // Dxbx
  uxiso, TextConsts;

type
  TxISOProgreso = procedure(Fichero: string);
  TxISOMensaje = procedure(aMessage: string);

var
  ProgresoxISO: TxISOProgreso;
  MensajesxISO: TxISOMensaje;
  Parar: Boolean;

  var
  BufCopia: array[0..65535] of Byte;


function NumeroFicheros(Folder: string): Integer;
function CrearXISO(NombreImagen: string; Folder: string): Boolean;
function EscanearXISO(Imagen: TFilestream; Folder: string; var SigSectorVacio: Int64): Boolean;
function XDFS2ISO9660(Fichero: string): Boolean;
procedure AsignarNodos(var Lista: TList);

implementation

type
  PByteArray64 = ^TByteArray64;
  TByteArray64 = array[0..65535] of Byte;

  PByteArray256 = ^TByteArray256;
  TByteArray256 = array[0..262143] of Byte;

{$A-}
  TxVD = record
    IDIn: array[0..19] of Char;
    DirRaiz: Integer;
    TamRaiz: Integer;
    FechaHora: FILETIME;
    SinUso: array[0..1991] of Byte;
    IDOut: array[0..19] of Char;
  end;

  pxFichero = ^TxFichero;
  TxFichero = record
    pIzq: Word;
    pDer: Word;
    SectorIn: Integer;
    Tamano: Integer;
    Atributo: Byte;
    LongNombre: Byte;
    Nombre: array[0..255] of Char;
  end;
{$A+}

  PEntradaDir = ^TEntradaDir;
  TEntradaDir = record
    Nombre: string;
    Tamano: Integer;
    SectorIn: Integer;
    Sectores: Integer;
    Directorio: Boolean;
    NodoDer: Word;
    NodoIzq: Word;
    Attributes: Integer;
  end;

{$A-}
  TFechaHora = record
    Ano: Byte;
    Mes: Byte;
    Dia: Byte;

    Hora: Byte;
    Minuto: Byte;
    Segundo: Byte;

    DifHora: shortint;
  end;

  TFechaHora2 = record
    Ano: array[0..3] of Char;
    Mes: array[0..1] of Char;
    Dia: array[0..1] of Char;

    Hora: array[0..1] of Char;
    Minuto: array[0..1] of Char;
    Segundo: array[0..1] of Char;

    Cero1: array[0..1] of Char;

    Cero2: Byte;
  end;

  TDirectorio_Raiz = record
    Longitud: Byte;
    Extended: Byte;
    SectorLE: Integer;
    SectorBE: Integer;
    TamanoLE: Integer;
    TamanoBE: Integer;
    FechaHora: TFechaHora;
    Attributes: Byte; // 0 = Fichero, 1 = Fichero Oculto, 2 = Directorio, 3 = Directorio Oculto
    FUnitSize: Byte; // File Unit Size ???
    IGapSize: Byte; // Interleave Gap Size ???
    VSeqSizeLE: Word; // Volume Sequence Size ??? Little Endian
    VSeqSizeBE: Word; // Volume Sequence Size ??? Big Endian
    LongNombre: Byte; // Longitud del nombre
    Nombre: Byte; // Nombre del directorio
  end;

  TPVD = record
    Id1: Byte; // 0 = Boot Record, 1 = PVD, 2 = SVD
                                        // 3 = VPD , 255 = Terminador de VD
    CD001: array[0..4] of Char; // Siempre CD001
    Id2: Byte; // Version del VD (Volume Descriptor)
    Nulo2: Byte; // 1 $00. No se usa.
    IdSistema: array[0..31] of Char; // Identificador del CD (interno)
    Etiqueta: array[0..31] of Char; // Etiqueta del CD.
    Nulo3: array[0..7] of Byte; // 8 $00
    SectorTLE: Integer; // Tamaño total de la imagen en sectores. Little Endian
    SectorTBE: Integer; // Tamaño total de la imagen en sectores. Big Endian
    Nulo4: array[0..31] of Char; // 32 $00

    VStamLE: Word; // Siempre $1. Tamaño del Volume Set. Little Endian
    VStamBE: Word; // Siempre $1. Tamaño del Volume Set. Big Endian
    VSnumLE: Word; // Siempre $1. Numero de Volume Sequence. Little Endian
    VSnumBE: Word; // Siempre $1. Numero de Volume Sequence. Big Endian

    TSectLE: Word; // Siempre 2048. Tamaño Sector. Little Endian
    TSectBE: Word; // Siempre 2048. Tamaño Sector. Big Endian

    PTtamLE: Integer; // Tamaño de la tabla de directorios. Little Endian
    PTtamBE: Integer; // Tamaño de la tabla de directorios. Big Endian

    PTLLE1: Integer; // Sector de la tabla de directorios tipo L 1. Little Endian
    PTLLE2: Integer; // Sector de la tabla de directorios tipo L 2. Little Endian
    PTMBE1: Cardinal; // Sector de la tabla de directorios tipo M 1. Big Endian
    PTMBE2: Cardinal; // Sector de la tabla de directorios tipo M 2. Big Endian

    DirRaiz: TDirectorio_Raiz;

    NomVolume: array[0..127] of Char; // Nombre del CD extendido 128 caracteres
    NomPubli: array[0..127] of Char; // Nombre del Publicador.
    NomCreador: array[0..127] of Char; // Nombre del creador del CD 128 caracteres
    NomApplica: array[0..127] of Char; // Nombre del programa con en el que se hizo
    FichCopy: array[0..36] of Char; // Copyrigh.txt
    FichAbst: array[0..36] of Char; // Abstract.txt
    FichBibl: array[0..36] of Char; // Bibliogr.txt

    FeHoCrea: TFechaHora2;
    FeHoModi: TFechaHora2;
    FeHoExpi: TFechaHora2;
    FeHoEfec: TFechaHora2;

    VerEstruc: Byte;
    Cero: Byte;
    DatosApli: array[0..511] of Byte;
  end;

  PDirectorios = ^TDirectorios;
  TDirectorios = record
    LongNombre: Byte; // Longitud del nombre MAX 31 caracteres
    Nulo: Byte; // Numero de sectores en modo Extendido del directorio
    Sector: Integer; // Sector donde se encuentra el Directorio
    Nivel: Word; // Nivel a que se encuentra el directorio sobre la raiz. Raiz = 1
    Nombre: array[0..31] of Char; // Nombre del directorio
    Pad: Byte; // Si la longitud del nombre es Impar se pone 1 sino no existe
  end;

  PTOC = ^TTOC;
  TTOC = record
    Tamano: Byte;
    Extent: Byte;
    PSectorBE: Integer;
    PSectorLE: Integer;
    TamanoBE: Integer;
    TamanoLE: Integer;
    Ano: Byte;
    Mes: Byte;
    Dia: Byte;
    Hora: Byte;
    Minuto: Byte;
    Segundo: Byte;
    Zona: ShortInt;
    Attributes: Byte;
    Nulo1: Byte;
    Nulo2: Byte;
    VolumeSBE: array[0..1] of Byte;
    VolumeSLE: array[0..1] of Byte;
    LongNombre: Byte;
    Nombre: array[0..49] of Char;
  end;
{$A+}

const
  XBOX_MEDIA = 'MICROSOFT*XBOX*MEDIA';
  SECTOR_RAIZ = 33;

  XF_SOLOLECTURA = $01;
  XF_OCULTO = $02;
  XF_SISTEMA = $04;
  XF_DIRECTORIO = $10;
  XF_FICHERO = $20;
  XF_NORMAL = $80;

//--- 4 Bytes a DWORD formato Motorola

function Intel2Motorola(ValorInt32: Integer): Integer;
var
  b3, b2, b1, b0: Byte;
begin
  b0 := (ValorInt32 and $FF000000) shr 24; //Hi(Hi(ValorInt32));
  b1 := (ValorInt32 and $00FF0000) shr 16; //Hi(Lo(ValorInt32));
  b2 := (ValorInt32 and $0000FF00) shr 8; //Lo(Hi(ValorInt32));
  b3 := (ValorInt32 and $000000FF); //Lo(Lo(ValorInt32));
  Result := (b3 shl 24) or (b2 shl 16) or (b1 shl 8) or b0;
      {Result := ((LongInt(b3) shl 24) and $FF000000) or
                ((LongInt(b2) shl 16) and $00FF0000) or
                ((LongInt(b1) shl  8) and $0000FF00) or
                ((LongInt(b0)       ) and $000000FF);  }
end;

function Intel2MotorolaWORD(Valor: Word): Word;
begin
  Result := (Lo(Valor) shl 8) or (Hi(Valor) shr 8);
end;

{$WARN SYMBOL_PLATFORM OFF}

function NumeroFicheros(Folder: string): Integer;
var
  SR: TSearchRec;
begin
  Result := 0;

  if Folder[Length(Folder)] <> '\' then Folder := Folder + '\';

  if (FindFirst(Folder + '*.*', faArchive or faDirectory or faHidden or faSysFile or faReadOnly, SR) = 0) then
  begin
    repeat
      if SR.Name[1] = '.' then
        Continue;
        
      if (SR.Attr and faDirectory) = faDirectory then
        Result := Result + NumeroFicheros(Folder + SR.Name + '\')
      else
        Result := Result + 1;
    until (FindNext(SR) <> 0);
  end;
end;

{$WARN SYMBOL_PLATFORM ON}


function Tamano2Sector(Tamano: Int64): Int64;
begin
  Result := Tamano div 2048;
  if ((Tamano mod 2048) <> 0) then
    Result := Result + 1;
end;

{$WARN SYMBOL_PLATFORM OFF}

function Vacio(Folder: string): Integer;
var
  SR: TSearchRec;
  Ent, Resto: Integer;
begin
  Result := 0;
  if (FindFirst(Folder + '*.*', faArchive or faDirectory or faHidden or faSysFile or faReadOnly, SR) = 0) then
  begin
    repeat
      if SR.Name[1] = '.' then
        Continue;

      Ent := 14 + Length(SR.Name);
      Resto := ((4 - (Ent mod 4)) mod 4);
      Ent := Ent + Resto;
      Result := Result + Ent;
    until (FindNext(SR) <> 0);
       // Con esto ajustamos el tamaño aunque con la linea siguiente lo anula.
       //if (Resto <> 0) then Result := Result - Resto;
       // Ajustamos a sector
    if (Result mod 2048) <> 0 then
      Result := (Result div 2048) * 2048 + 2048;
  end;
end;

{$WARN SYMBOL_PLATFORM ON}


// Funcion que devuelve el tamaño de un directorio.
// Siempre es multiplo de 2048 (Tamaño Sector)
// Si es 0 entonces el directorio es vacio

{$WARN SYMBOL_PLATFORM OFF}

function TamanoDirectorio(Folder: string): Integer;
var
  SR: TSearchRec;
  Ent, i: Integer;
  Lista: TList;
  Entrada: PEntradaDir;
begin
  Result := 0;
  Lista := TList.Create();
  if (FindFirst(Folder + '*.*', faArchive or faDirectory or faHidden or faSysFile or faReadOnly, SR) = 0) then
  begin
    repeat
      if (SR.Name[1] = '.') then
        Continue;

      New(Entrada);
      Entrada.Nombre := SR.Name;
      Lista.Add(Entrada);
            { if (SR.Attr and faDirectory) = faDirectory then
             begin
                  if Vacio(Folder+SR.Name+'\') = 0 then
                  begin
                        New(Entrada);
                        Entrada.Nombre := SR.Name;
                        Lista.Add(Entrada);
                        Continue;
                  end;
             end;    }

            { Ent := 14 + Length(SR.Name);
             if (Ent mod 4) <> 0 then
               Ent := ((Ent div 4)+1)*4;
             //Resto := ((4 - (Ent mod 4)) mod 4);
             //Ent := Ent + Resto;
             if (((Result div 2048)+1)*2048 - Result) < Ent then//(14+Length(SR.Name)) then
               Result := ((Result div 2048) + 1)*2048;
             Result := Result + Ent; }
    until (FindNext(SR) <> 0);

    AsignarNodos(Lista);

    for i := 0 to Lista.Count - 1 do
    begin
      Entrada := PEntradaDir(Lista[i]);
      Ent := 14 + Length(Entrada.Nombre);
           { if (Ent mod 4) <> 0 then
              Ent := ((Ent div 4)+1)*4;  }
            //Resto := ((4 - (Ent mod 4)) mod 4);
            //Ent := Ent + Resto;

            // Si no cabe el nombre + los 14 bytes dentro del sector...
            // sino comprobamos si cabe en el sector con la correccion de los 4 bytes
            // sino se graba tal cual el nombre+14 bytes.
      if (((Result div 2048) + 1) * 2048 - Result) < Ent then //(14+Length(SR.Name)) then
      begin
        Result := ((Result div 2048) + 1) * 2048;

        if (Ent mod 4) <> 0 then
          Ent := ((Ent div 4) + 1) * 4;

              // Si es el ultimo fichero de la tabla entonces ignoramos el Ent.
        if i < Lista.Count - 1 then
          Result := Result + Ent;
      end
      else
      begin
        if (Ent mod 4) <> 0 then
          Ent := ((Ent div 4) + 1) * 4;
        if (((Result div 2048) + 1) * 2048 - Result) < Ent then
          Result := Result + (14 + Length(Entrada.Nombre))
        else
          Result := Result + Ent;
      end;
    end;

       // Ajustamos a sector
    if (Result mod 2048) <> 0 then
      Result := ((Result div 2048) + 1) * 2048;
  end;

  Lista.Free;
end;

{$WARN SYMBOL_PLATFORM ON}


function Comparar(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(PEntradaDir(Item1)^.Nombre, PEntradaDir(Item2)^.Nombre);
end;

function CompararSectores(Item1, Item2: Pointer): Integer;
begin
  Result := 0;
  if (PEntradaDir(Item1)^.SectorIn < PEntradaDir(Item2)^.SectorIn) then Result := -1
  else
    if (PEntradaDir(Item1)^.SectorIn > PEntradaDir(Item2)^.SectorIn) then Result := 1
    else
      if (PEntradaDir(Item1)^.SectorIn = PEntradaDir(Item2)^.SectorIn) then Result := 0;
end;

procedure AsignarNodos(var Lista: TList);
var
  i, j, l, k, Cont: Integer;
  Elem, Elem2: PEntradaDir;
  ListaAux: TList;
begin
  ListaAux := TList.Create();
  for i := 0 to Lista.Count - 1 do
  begin
    ListaAux.Add(Lista[i]);
  end;
  ListaAux.Sort(Comparar);

     {for i := 0 to Lista.Count-1 do
     begin
          Elem := PEntradaDir(Lista[i]);
          j := Lista.IndexOf(Elem);
          Elem.NodoIzq := 0;
          if i = Lista.Count-1 then
          begin
               Elem.NodoDer := 0;
               Break;
          end;
          Cont := (Cont*4+14+Length(Elem.Nombre));
          if (Cont mod 4) <> 0 then
             Elem.NodoDer := (cont div 4)*4 + 4;

          Elem.NodoDer := Elem.NodoDer div 4;
          PEntradaDir(Lista[j])^.NodoDer := Elem.NodoDer;
          Cont := Elem.NodoDer;
     end;}

  for i := 0 to ListaAux.Count - 1 do
  begin
    Cont := 0;

    Elem := PEntradaDir(ListaAux[i]);
          //Elem.NodoIzq := 0;
    k := Lista.IndexOf(Elem);
    if i = (ListaAux.Count - 1) then
    begin
      PEntradaDir(Lista[k]).NodoIzq := 0;
      PEntradaDir(Lista[k]).NodoDer := 0;
      Break;
    end;

    Elem := PEntradaDir(ListaAux[i + 1]);
          //Elem.NodoIzq := 0;
    j := Lista.IndexOf(Elem);

    for l := 0 to j - 1 do
    begin
      Elem2 := PEntradaDir(Lista[l]);
                // Si no queda bytes libres en el sector para introducir la
                // nueva entrada, entonces saltamos al siguiente sector.
      if (((Cont div 2048) + 1) * 2048 - Cont) < (14 + Length(Elem2.Nombre)) then
      begin
        Cont := ((Cont div 2048) + 1) * 2048 + 14 + Length(Elem2.Nombre);
        if l = j - 1 then
        begin
          PEntradaDir(Lista[k - 1])^.NodoDer := ((((PEntradaDir(Lista[k - 1])^.NodoDer * 4) div 2048) + 1) * 2048) div 4;

        end;
      end
      else
        Cont := (Cont + 14 + Length(Elem2.Nombre));
      if (Cont mod 4) <> 0 then Cont := (cont div 4) * 4 + 4;
    end;

    PEntradaDir(Lista[k])^.NodoIzq := 0;
    PEntradaDir(Lista[k])^.NodoDer := Cont div 4;
  end;
  ListaAux.Free;
end;

{$WARN SYMBOL_PLATFORM OFF}

procedure GenerarTabla(var lDirectorio: TList; var PosBuf: Integer; Buffer: Pointer);
var
  xEntrada: TxFichero;
  i, j: Integer;
  Entrada: PEntradaDir;
  Attributes: Byte;
begin
  for i := 0 to lDirectorio.Count - 1 do
  begin
    Entrada := PEntradaDir(lDirectorio[i]);
    xEntrada.pIzq := Entrada.NodoIzq;
    xEntrada.pDer := Entrada.NodoDer;
    xEntrada.SectorIn := Entrada.SectorIn;
    xEntrada.Tamano := Entrada.Tamano;

    if Entrada.Directorio then
      Attributes := XF_DIRECTORIO
    else
      Attributes := XF_FICHERO;

    if (Entrada.Attributes and faReadOnly) = faReadOnly then Attributes := Attributes + XF_SOLOLECTURA;
    if (Entrada.Attributes and faHidden) = faHidden then Attributes := Attributes + XF_OCULTO;
    if (Entrada.Attributes and faSysFile) = faSysFile then Attributes := Attributes + XF_SISTEMA;
               //if (Entrada.Attributes and faDirectory) = faDirectory then Attributes := Attributes + XF_DIRECTORIO;
               //if (Entrada.Attributes and faArchive) = faArchive then Attributes := Attributes + XF_NORMAL;

    xEntrada.Atributo := Attributes;
    xEntrada.LongNombre := Length(Entrada.Nombre);

    for j := 1 to Length(Entrada.Nombre) do
      xEntrada.Nombre[j - 1] := Entrada.Nombre[j];

    if (((PosBuf div 2048) + 1) * 2048 - PosBuf) < (14 + xEntrada.LongNombre) then
      PosBuf := ((PosBuf div 2048) + 1) * 2048;

    Move(xEntrada, PByteArray256(Buffer)^[PosBuf], 14 + xEntrada.LongNombre);
    PosBuf := PosBuf + 14 + xEntrada.LongNombre;
    if (PosBuf mod 4) <> 0 then PosBuf := PosBuf + ((4 - (PosBuf mod 4)) mod 4);
  end;
end;

{$WARN SYMBOL_PLATFORM ON}

{$WARN SYMBOL_PLATFORM OFF}

function EscanearXISO(Imagen: TFilestream; Folder: string; var SigSectorVacio: Int64): Boolean;
var
  Buffer: Pointer;
  B1: PByteArray64;
  B2: PByteArray256;
  TamBufRellenar: Integer;

  SR: TSearchRec;
  lDirectorio, SubDirectorios: TList;
  Entrada: PEntradaDir;
  Fichero: TFileStream;
  SectorTabla: Int64;
  PosBuf, i: Integer;
  j, leido: Integer;
begin
  EscanearXISO := False;
  Buffer := nil;
  TamBufRellenar := -1;

  if TamanoDirectorio(Folder) <= 65536 then
  begin
    New(B1);
    Buffer := B1;
    TamBufRellenar := 65536;
  end
  else
  begin
    if TamanoDirectorio(Folder) > 65536 then
    begin
      New(B2);
      Buffer := B2;
      TamBufRellenar := 262144;
    end;
  end;

  lDirectorio := TList.Create();
  SubDirectorios := TList.Create();
  SectorTabla := (SigSectorVacio - 1) * 2048;
  PosBuf := 0;
  Imagen.Seek(Imagen.Size, soBeginning);
  MensajesxISO(SEscaneandoCarpeta + Folder);

  if (FindFirst(Folder + '*.*', faArchive or faDirectory or faHidden or faSysFile or faReadOnly, SR) = 0) then
  begin
    repeat
      if SR.Name[1] = '.' then
        Continue;
        
      if Parar then
        Exit;
        
            //if Assigned(ProgresoxISO) then
            //  ProgresoxISO(Folder+SR.Name);
      New(Entrada);
      Entrada.Nombre := SR.Name;
      Entrada.Tamano := SR.Size;
      Entrada.Sectores := Tamano2Sector(SR.Size);
      Entrada.Directorio := (SR.Attr and faDirectory) = faDirectory;
      Entrada.Attributes := SR.Attr;

      if not Entrada^.Directorio then //((SR.Attr and faArchive) = faArchive) or (SR.Attr = 0) then
      begin
                 // 2 Lineas Borradas: 18-06-2002
                 //Entrada^.SectorIn := SigSectorVacio;
                 //SigSectorVacio := SigSectorVacio + Entrada.Sectores;
        lDirectorio.Add(Entrada);
      end;

            // Si es directorio lo metemos en la lista de directorios a procesar
            // sino metemos el fichero en la lista de elementos del directorio
      if Entrada^.Directorio then
      begin
        Entrada^.Tamano := TamanoDirectorio(Folder + SR.Name + '\');
        Entrada^.Sectores := Tamano2Sector(Entrada^.Tamano);

                 // Si el directorio esta vacio entonces establecemos a 0 los parametros
                 // y no lo añadimos a la lista de directorios
        if Entrada^.Tamano = 0 then
        begin
          Entrada^.SectorIn := 0;
          Entrada^.Sectores := 0;
          lDirectorio.Add(Entrada);
        end
        else SubDirectorios.Add(Entrada);
      end
    until (FindNext(SR) <> 0);
    FindClose(SR);

          // Introducimos ahora al final las carpetas
    for i := 0 to SubDirectorios.Count - 1 do
    begin
      //PEntradaDir(SubDirectorios[i]);
      lDirectorio.Add(SubDirectorios[i]);
    end;

          // Nuevo: 18-6-2002
    lDirectorio.Sort(Comparar);

    AsignarNodos(lDirectorio);

          // Nuevo: 18-06-2002
    for i := 0 to lDirectorio.Count - 1 do
    begin
      Entrada := PEntradaDir(lDirectorio[i]);
      if Entrada.Directorio then
        Continue;

      Entrada^.SectorIn := SigSectorVacio;
      SigSectorVacio := SigSectorVacio + Entrada.Sectores;
    end;

    MensajesxISO(SGenerandoEntrada);

    FillChar(Buffer^, TamBufRellenar, $FF);
    GenerarTabla(lDirectorio, PosBuf, Buffer);
          // Extendemos la entrada del directorio hasta alcanzar el final del sector con $FF
          // y grabamos la entrada del directorio en la imagen.
    if (PosBuf mod 2048) <> 0 then
      PosBuf := (PosBuf div 2048) * 2048 + 2048;
    Imagen.Write(Buffer^, PosBuf);

          // Copiamos los ficheros dentro de la imagen
    for i := 0 to lDirectorio.Count - 1 do
    begin
      if Parar then
        Exit;
          
      Entrada := PEntradaDir(lDirectorio[i]);
      if Entrada.Directorio then
        Continue;
        
      Fichero := TFilestream.Create(Folder + Entrada.Nombre, fmOpenRead);
      if Fichero = nil then
        Continue;
        
      while (Fichero.Position < Fichero.Size) do
      begin
        Imagen.Seek(Imagen.Size, soBeginning);
        FillChar(BufCopia, SizeOf(BufCopia), 0);
        leido := Fichero.Read(BufCopia, SizeOf(BufCopia));
        Imagen.Write(BufCopia, leido);
        if (leido mod 2048) <> 0 then
        begin
          FillChar(BufCopia, (((leido div 2048) + 1) * 2048) - leido, 0);
          Imagen.Write(BufCopia, (((leido div 2048) + 1) * 2048) - leido);
        end;
               {     Imagen.Seek(Imagen.Size,soBeginning);
                    FillChar(BufCopia,SizeOf(BufCopia),0);
                    Fichero.Read(BufCopia,SizeOf(BufCopia));
                    Imagen.Write(BufCopia,SizeOf(BufCopia));        }
      end;
      Fichero.Free;
               // 22 de Junio de 2002
      if Assigned(ProgresoxISO) then
        ProgresoxISO(SIntroduciendoFichero + Entrada.Nombre);
    end;

          // Nuevo: 18-06-2002
    lDirectorio.Sort(CompararSectores);

          // Procesamos las subcarpetas
    for i := 0 to SubDirectorios.Count - 1 do
    begin
      if Parar then
        Exit;
        
      j := lDirectorio.IndexOf(SubDirectorios[i]);
      PEntradaDir(lDirectorio[j])^.SectorIn := SigSectorVacio;
      SigSectorVacio := SigSectorVacio + PEntradaDir(lDirectorio[j])^.Sectores;
      EscanearXISO(Imagen, Folder + PEntradaDir(SubDirectorios[i])^.Nombre + '\', SigSectorVacio);
    end;
          // Nuevo: 18-06-2002
    lDirectorio.Sort(Comparar);

    FillChar(Buffer^, SizeOf(Buffer^), $FF);
    PosBuf := 0;
    GenerarTabla(lDirectorio, PosBuf, Buffer);
    if (PosBuf mod 2048) <> 0 then
      PosBuf := (PosBuf div 2048) * 2048 + 2048;

    Imagen.Seek(SectorTabla - TamanoDirectorio(Folder) + 2048, soBeginning);
    Imagen.Write(Buffer^, PosBuf);
    Imagen.Seek(Imagen.Size, soBeginning);

    Result := True;
  end
  else Result := False;

  lDirectorio.Free;
  SubDirectorios.Free;
  Dispose(Buffer);
end;

{$WARN SYMBOL_PLATFORM ON}


function CrearXISO(NombreImagen: string; Folder: string): Boolean;
var
  VD: TxVD;
  PVD: TPVD;
  Imagen: TFileStream;
  Buffer: PByteArray;
  FechaActual: FILETIME;
  SectorInicio: Int64;
  i: Integer;
begin
  Result := False;
  if (Folder = '') or not DirectoryExists(Folder) then
    Exit;
    
  if Folder[Length(Folder)] <> '\' then
    Folder := Folder + '\';

     // Creamos las variables dinamicas e inicializamos
  Imagen := TFileStream.Create(NombreImagen, fmCreate);
  if Imagen = nil then
    Exit;

  New(Buffer);
  FillChar(Buffer^, SizeOf(Buffer^), $00);
  FillChar(VD, SizeOf(VD), $00);

     // Rellenamos el PVD y el xPVD
  GetSystemTimeAsFileTime(FechaActual);
  VD.IDIn := XBOX_MEDIA;
  VD.DirRaiz := SECTOR_RAIZ;
  VD.TamRaiz := TamanoDirectorio(Folder);
  VD.FechaHora := FechaActual;
  VD.IDOut := XBOX_MEDIA;

     // Escribimos el PVD (ISO 9660)
  Imagen.Write(Buffer^, SizeOf(Buffer^));
  MensajesxISO(SEscritoPVD);
     // Escribimos el xPVD (XBOX Estandard)
  Imagen.Write(Buffer^, SizeOf(Buffer^));
  Imagen.Write(VD, SizeOf(VD));
  MensajesxISO(SEscritoXPVD);

     // Iniciamos el escaneo, estructuracion y creacion de la imagen
  SectorInicio := SECTOR_RAIZ + 1;
  SectorInicio := SectorInicio + (TamanoDirectorio(Folder) div 2048) - 1;
  MensajesxISO(SInicioCreacion);
  Result := EscanearXISO(Imagen, Folder, SectorInicio);
  if Parar then
    MensajesxISO(SParadaAnormal)
  else
    MensajesxISO(SFinCreacion);

  FillChar(PVD, 2048, 0);
  PVD.Id1 := 01;
  PVD.CD001 := 'CD001';
  PVD.Id2 := 01;
  PVD.SectorTLE := Imagen.Size div 2048;
  PVD.SectorTBE := Intel2Motorola(Imagen.Size div 2048);
  PVD.VStamLE := 1;
  PVD.VStamBE := 256;
  PVD.VSnumLE := 1;
  PVD.VSnumBE := 256;
  PVD.TSectLE := 2048;
  PVD.TSectBE := 8;
  FillChar(PVD.NomVolume, SizeOf(PVD.NomVolume), $20);
  FillChar(PVD.NomPubli, SizeOf(PVD.NomPubli), $20);
  FillChar(PVD.NomCreador, SizeOf(PVD.NomCreador), $20);
  FillChar(PVD.NomApplica, SizeOf(PVD.NomApplica), $20);
     //PVD.NomApplica := 'Creador XISO - Yursoft';
  FillChar(PVD.FichCopy, SizeOf(PVD.FichCopy), $20);
  FillChar(PVD.FichAbst, SizeOf(PVD.FichAbst), $20);
  FillChar(PVD.FichBibl, SizeOf(PVD.FichBibl), $20);
  FillChar(PVD.FeHoCrea, SizeOf(PVD.FeHoCrea) - 1, $30);
  FillChar(PVD.FeHoModi, SizeOf(PVD.FeHoModi) - 1, $30);
  FillChar(PVD.FeHoExpi, SizeOf(PVD.FeHoExpi) - 1, $30);
  FillChar(PVD.FeHoEfec, SizeOf(PVD.FeHoEfec) - 1, $30);
  PVD.VerEstruc := 1;

  Imagen.Seek(32768, soBeginning);
  Imagen.Write(PVD, 2048);

  FillChar(PVD, 2048, 0);
  PVD.Id1 := $FF;
  PVD.CD001 := 'CD001';
  PVD.Id2 := 01;
  Imagen.Write(PVD, 2048);

  Imagen.Seek(Imagen.Size, soBeginning);
  FillChar(Buffer^, SizeOf(Buffer^), $00);
  for i := 1 to 32 do
    Imagen.Write(Buffer^, SizeOf(Buffer^));

  Imagen.Free;
end;
 {
function CrearXISO(NombreImagen: string; Folder: string): Boolean;
var
   VD: TxVD;
   PVD: TPVD;
   Imagen: TFileStream;
   Buffer: PByteArray;
   FechaActual: FILETIME;
   SectorInicio: Int64;
   i: Integer;
   b: array[0..8191] of Byte;
begin
     if (Folder = '') or not DirectoryExists(Folder) then
      Exit;
     if Folder[Length(Folder)] <> '\' then Folder := Folder + '\';

     // Creamos las variables dinamicas e inicializamos
     Imagen := TFileStream.Create(NombreImagen, fmCreate);
     if (Imagen = nil) then
      Exit;
     New(Buffer);
     FillChar(Buffer^,SizeOf(Buffer^),$00);
     FillChar(VD,SizeOf(VD),$00);

     // Rellenamos el PVD y el xPVD
     GetSystemTimeAsFileTime(FechaActual);
     VD.IDIn      := XBOX_MEDIA;
     //VD.DirRaiz   := SECTOR_RAIZ;
     VD.DirRaiz := 198177;
     VD.TamRaiz   := TamanoDirectorio(Folder);
     VD.FechaHora := FechaActual;
     VD.IDOut     := XBOX_MEDIA;

     // Escribimos el PVD (ISO 9660)
     Imagen.Write(Buffer^,SizeOf(Buffer^));
     MensajesxISO( rcEscritoPVD );
     // Escribimos el xPVD (XBOX Estandard)
     Imagen.Write(Buffer^,SizeOf(Buffer^));
     MensajesxISO( rcEscritoXPVD );

     for i := 1 to 198144 do
        Imagen.Write(b,2048);
     Imagen.Write(VD,SizeOf(VD));
     // Iniciamos el escaneo, estructuracion y creacion de la imagen

     //SectorInicio := SECTOR_RAIZ+1;
     SectorInicio := 198177+1;
     SectorInicio := SectorInicio + (TamanoDirectorio(Folder) div 2048)-1;
     MensajesxISO( rcInicioCreacion );
     Result := EscanearXISO(Imagen,Folder,SectorInicio);
     if Parar then
       MensajesxISO( rcParadaAnormal )
     else
       MensajesxISO( rcFinCreacion );

     FillChar(PVD,2048,0);
     PVD.Id1 := 01;
     PVD.CD001 := 'CD001';
     PVD.Id2 := 01;
     PVD.SectorTLE := Imagen.Size div 2048;
     PVD.SectorTBE := Intel2Motorola(Imagen.Size div 2048);
     PVD.VStamLE := 1;
     PVD.VStamBE := 256;
     PVD.VSnumLE := 1;
     PVD.VSnumBE := 256;
     PVD.TSectLE := 2048;
     PVD.TSectBE := 8;
     FillChar(PVD.NomVolume,SizeOf(PVD.NomVolume),$20);
     FillChar(PVD.NomPubli,SizeOf(PVD.NomPubli),$20);
     FillChar(PVD.NomCreador,SizeOf(PVD.NomCreador),$20);
     FillChar(PVD.NomApplica,SizeOf(PVD.NomApplica),$20);
     //PVD.NomApplica := 'Creador XISO - Yursoft';
     FillChar(PVD.FichCopy,SizeOf(PVD.FichCopy),$20);
     FillChar(PVD.FichAbst,SizeOf(PVD.FichAbst),$20);
     FillChar(PVD.FichBibl,SizeOf(PVD.FichBibl),$20);
     FillChar(PVD.FeHoCrea,SizeOf(PVD.FeHoCrea)-1,$30);
     FillChar(PVD.FeHoModi,SizeOf(PVD.FeHoModi)-1,$30);
     FillChar(PVD.FeHoExpi,SizeOf(PVD.FeHoExpi)-1,$30);
     FillChar(PVD.FeHoEfec,SizeOf(PVD.FeHoEfec)-1,$30);
     PVD.VerEstruc := 1;

     Imagen.Seek(32768,soBeginning);
     Imagen.Write(PVD,2048);

     FillChar(PVD,2048,0);
     PVD.Id1 := $FF;
     PVD.CD001 := 'CD001';
     PVD.Id2 := 01;
     Imagen.Write(PVD,2048);

     Imagen.Seek(Imagen.Size,soBeginning);
     FillChar(Buffer^,SizeOf(Buffer^),$00);
     for i := 1 to 32 do
        Imagen.Write(Buffer^,SizeOf(Buffer^));

     Imagen.Free;
end;   }

function CompararPosiciones(Item1, Item2: Pointer): Integer;
begin
  if PDirectorios(Item1)^.Nivel < PDirectorios(Item2)^.Nivel then Result := -1
  else
    if PDirectorios(Item1)^.Nivel = PDirectorios(Item2)^.Nivel then Result := 0
    else
      if PDirectorios(Item1)^.Nivel > PDirectorios(Item2)^.Nivel then Result := 1
      else
        Result := 0;
end;

function XDFS2ISO9660(Fichero: string): Boolean;
const
  ByteNulo: Byte = 0;
  PuntoComa: array[0..1] of Char = ';1';
var
  i: Integer;
  XDFS: TFilestream;
  Entrada, EntradaPadre: uxiso.pxFichero;
  ListaDirectorios: TList;
  Directorio: PDirectorios;
  EntradaISO: PTOC;
  PVD: TPVD;
  BufferDirectorios: array[0..65535] of Byte;
  PosBufDirectorios: Integer;
  PosicionTablaDirectorios: Integer;
  LongNombre: Byte;
  SectorDirectorioRaiz, OffsetDirectorio, OffsetRestaurado, OffsetDirectorios: Int64;

  procedure CrearTablaDirectorios(Modo: Byte);
  var
    i: Integer;
    OffIni: Integer;
  begin
    OffIni := XDFS.Position div 2048;
    PosBufDirectorios := 0;
    OffsetDirectorios := XDFS.Position;
    ListaDirectorios.Clear;
     // Directorio raiz
    New(Directorio);
    FillChar(Directorio^, SizeOf(Directorio^), 0);
    Directorio.LongNombre := 1;
    if Modo = 0 then
    begin
      Directorio^.Sector := SectorDirectorioRaiz;
      Directorio^.Nivel := 1;
    end
    else
      if Modo = 1 then
      begin
        Directorio^.Sector := Intel2Motorola(SectorDirectorioRaiz);
        Directorio^.Nivel := Intel2MotorolaWORD(1);
      end;

    ListaDirectorios.Add(Directorio);

     // Si estamos sin espacio en este sector saltamos al siguiente.
     //Move(Directorio^,BufferDirectorios[PosBufDirectorios],8+Directorio.LongNombre+1);
     //PosBufDirectorios := PosBufDirectorios + 8 + Directorio.LongNombre+1;
     // Escaneamos en busca de directorios
    for i := 0 to xIISO.Lista.Count - 1 do
    begin
      Entrada := uxiso.pxFichero(xIISO.Lista[i]);
      if (Entrada.Atributo and XF_DIRECTORIO) = XF_DIRECTORIO then
      begin
        New(Directorio);
        FillChar(Directorio^, SizeOf(Directorio^), 0);
           {  if (Directorio.LongNombre mod 2) <> 0 then
             begin
               if Entrada^.LongNombre+1 > 32 then
                 LongNombre := 32
               else
                 LongNombre := Entrada^.LongNombre+1;
             end
             else  }
        begin
          if Entrada^.LongNombre > 30 then
            LongNombre := 30
          else
            LongNombre := Entrada^.LongNombre;
        end;
        Directorio^.LongNombre := LongNombre;
        if Modo = 0 then
        begin
          Directorio^.Sector := Entrada^.SectorIn;
          Directorio^.Nivel := Entrada^.DirPadre + 1;
        end
        else
          if Modo = 1 then
          begin
            Directorio^.Sector := Intel2Motorola(Entrada^.SectorIn);
            Directorio^.Nivel := Intel2MotorolaWORD(Entrada^.DirPadre + 1);
          end;
        Move(Entrada^.Nombre, Directorio^.Nombre, LongNombre);
        ListaDirectorios.Add(Directorio);

        {     // Si estamos sin espacio en este sector saltamos al siguiente.
             //if ((PosBufDirectorios mod 2048) < (8+LongNombre)) and (PosBufDirectorios > 2048) then
             if (((XDFS.Position div 2048)+1)*2048)-XDFS.Position < (8+LongNombre) then
               PosBufDirectorios := ((PosBufDirectorios div 2048)+1)*2048;
             Move(Directorio^,BufferDirectorios[PosBufDirectorios],8+LongNombre);
             PosBufDirectorios := PosBufDirectorios + 8 + LongNombre;
             // Si no estamos en un offset par, dejamos un Byte en blanco.
             if (PosBufDirectorios mod 2) <> 0 then
               PosBufDirectorios := PosBufDirectorios+1;    }
      end;
    end;


     // Revisar
     {ListaDirectorios.Sort(CompararPosiciones);

     for i := 0 to ListaDirectorios.Count-1 do
     begin
             Directorio := ListaDirectorios[i];
             if Trim(Directorio.Nombre) = '' then
               ListaDirectorios.Move(i,0);
     end;  }

    for i := 0 to ListaDirectorios.Count - 1 do
    begin
      Directorio := ListaDirectorios[i];
      LongNombre := Directorio^.LongNombre;

             // Si estamos sin espacio en este sector saltamos al siguiente.
             //if ((PosBufDirectorios mod 2048) < (8+LongNombre)) and (PosBufDirectorios > 2048) then
      if (((XDFS.Position div 2048) + 1) * 2048) - XDFS.Position < (8 + LongNombre) then
        PosBufDirectorios := ((PosBufDirectorios div 2048) + 1) * 2048;
      Move(Directorio^, BufferDirectorios[PosBufDirectorios], 8 + LongNombre);
      PosBufDirectorios := PosBufDirectorios + 8 + LongNombre;
             // Si no estamos en un offset par, dejamos un Byte en blanco.
      if (PosBufDirectorios mod 2) <> 0 then
        PosBufDirectorios := PosBufDirectorios + 1;
    end;

    if (PosBufDirectorios mod 2048) <> 0 then
      PosBufDirectorios := ((PosBufDirectorios div 2048) + 1) * 2048;
     // Escribimos tabla de Directorios
    XDFS.Write(BufferDirectorios, PosBufDirectorios);
     // Introducimos en el PVD los datos consiguientes de la tabla de directorios. Tamaño
    if Modo = 0 then
    begin
      PVD.PTtamLE := PosBufDirectorios;
      PVD.PTLLE1 := OffIni;
    end
    else
      if Modo = 1 then
      begin
        PVD.PTtamBE := Intel2Motorola(PosBufDirectorios);
        PVD.PTMBE1 := Intel2Motorola(OffIni);
      end;
     // Procesamos los directorios y los pasamos al buffer de escritura.
    { PosBufDirectorios := 0;
     OffsetDirectorios := XDFS.Position;
     for i := 0 to ListaDirectorios.Count-1 do
     begin
        Directorio := PDirectorios(ListaDirectorios[i]);

        // Si estamos sin espacio en este sector saltamos al siguiente.
        if ((PosBufDirectorios mod 2048) < (8+Directorio.LongNombre+1)) and (PosBufDirectorios > 2048) then
          PosBufDirectorios := ((PosBufDirectorios div 2048)+1)*2048;
        Move(Directorio^,BufferDirectorios[PosBufDirectorios],8+Directorio.LongNombre);
        PosBufDirectorios := PosBufDirectorios + 8 + Directorio.LongNombre;
     end;
     if (PosBufDirectorios mod 2048) <> 0 then
       PosBufDirectorios := ((PosBufDirectorios div 2048)+1)*2048;      }
  end;

  procedure CrearTablaFicheros;
  var
    i, j, t: Integer;
  begin
    New(EntradaISO);
    for i := -1 to xIISO.Lista.Count - 1 do
    begin
      OffsetDirectorio := XDFS.Position;
      if i <> -1 then
      begin
        EntradaPadre := uxiso.pxFichero(xIISO.Lista[i]);
        if (EntradaPadre.Atributo and XF_DIRECTORIO) <> XF_DIRECTORIO then
          Continue;
          
        EntradaPadre.SectorIn := XDFS.Position div 2048;
        {if EntradaPadre.DirPadre = 0 then EntradaPadre.pIzq := 1
        else
        begin
         for t := 0 to i do
            if EntradaPadre.DirPadre = xiso.pxFichero(xIISO.Lista[t]).DirHijo then
              EntradaPadre.pIzq := xiso.pxFichero(xIISO.Lista[t]).pIzq+1;
        end;}
      end
      else
      begin
        New(EntradaPadre);
        EntradaPadre.pIzq := 1;
        EntradaPadre.DirHijo := 0;
      end;

       //Dejamos espacio para grabar mas tarde las 2 entradas del directorio.
      for t := 1 to 68 do
        XDFS.Write(ByteNulo, 1);
       // Iniciamos la busqueda de elementos de este nivel.
      for j := i + 1 to xIISO.Lista.Count - 1 do
      begin
        Entrada := uxiso.pxFichero(xIISO.Lista[j]);
        if Entrada^.DirPadre <> EntradaPadre.DirHijo then
          Continue;
          
        FillChar(EntradaISO^, SizeOf(EntradaISO^), 0);
        EntradaISO.TamanoLE := Intel2Motorola(Entrada^.Tamano);
        EntradaISO.TamanoBE := Entrada^.Tamano;
        EntradaISO.PSectorLE := Intel2Motorola(Entrada^.SectorIn);
        EntradaISO.PSectorBE := Entrada^.SectorIn;

         // Establecemos Attributes. (HAY QUE AMPLIAR)
        if (Entrada.Atributo and XF_DIRECTORIO) = XF_DIRECTORIO then
          EntradaISO.Attributes := 2
        else
          EntradaISO.Attributes := 0;

        if (Entrada.Atributo and XF_OCULTO) = XF_OCULTO then
          EntradaISO.Attributes := EntradaISO.Attributes or 1;

         // Copiamos el nombre del fichero/Folder al buffer.
         {if Entrada^.LongNombre > 30 then
           LongNombre := 30
         else                  }
        LongNombre := Entrada^.LongNombre;
        if (LongNombre mod 2) <> 0 then LongNombre := LongNombre + 1;
        EntradaISO^.LongNombre := LongNombre;
        Move(Entrada^.Nombre, EntradaISO^.Nombre, LongNombre);
        EntradaISO.Tamano := 33 + LongNombre;
        if ((((XDFS.Position div 2048) + 1) * 2048) - XDFS.Position < (EntradaISO^.Tamano)) then
          for t := 1 to (((XDFS.Position div 2048) + 1) * 2048) - XDFS.Position do
            XDFS.Write(ByteNulo, 1);
        XDFS.Write(EntradaISO^, EntradaISO^.Tamano);
        { if (XDFS.Position mod 2) <> 0 then
           XDFS.Write(ByteNulo,1);}
      end;
      if (XDFS.Position mod 2048) <> 0 then
        for t := 1 to (((XDFS.Position div 2048) + 1) * 2048) - XDFS.Position do
          XDFS.Write(ByteNulo, 1);

       // Generamos las 2 entradas iniciales de la Folder. (68 bytes)
      EntradaISO.Tamano := $22;
      EntradaISO.Extent := 0;
      EntradaISO.PSectorBE := OffsetDirectorio div 2048;
      EntradaISO.PSectorLE := Intel2Motorola(OffsetDirectorio div 2048);
      EntradaISO.TamanoBE := (XDFS.Position - OffsetDirectorio);
      EntradaISO.TamanoLE := Intel2Motorola((XDFS.Position - OffsetDirectorio));
      EntradaISO.Attributes := 2;
      EntradaISO.LongNombre := 1;
      EntradaISO.Nombre[0] := #00;
      OffsetRestaurado := XDFS.Position;
      XDFS.Seek(OffsetDirectorio, soBeginning);
      XDFS.Write(EntradaISO^, EntradaISO^.Tamano);
      XDFS.Write(EntradaISO^, EntradaISO^.Tamano);
      XDFS.Seek(OffsetRestaurado, soBeginning);
       //SectorDirectorioRaiz := OffsetDirectorio div 2048;

      if i = -1 then
      begin
        PVD.DirRaiz.Longitud := $22;
        PVD.DirRaiz.SectorLE := EntradaISO.PSectorBE;
        PVD.DirRaiz.SectorBE := EntradaISO.PSectorLE;
        PVD.DirRaiz.TamanoLE := EntradaISO.TamanoBE;
        PVD.DirRaiz.TamanoBE := EntradaISO.TamanoLE;
        PVD.DirRaiz.Attributes := 2;
        PVD.DirRaiz.LongNombre := 1;
        PVD.DirRaiz.Nombre := 0;
      end;
    end;
  end;
begin
  Result := AbrirXISO(Fichero);
  if not Result then
    Exit;
    
  ListaDirectorios := TList.Create;
  if ListaDirectorios = nil then
  begin
    ShowMessage('Please report this error to xiso@yursoft.com: ListaDirectorios');
    Exit;
  end;
  
  XDFS := TFilestream.Create(Fichero, fmOpenReadWrite);
  XDFS.Seek(XDFS.Size, soBeginning);

  PosicionTablaDirectorios := XDFS.Size;

     // Rellenamos el PVD
  FillChar(PVD, SizeOf(PVD), 0);
  PVD.Id1 := 01;
  PVD.CD001 := 'CD001';
  PVD.Id2 := 01;
  PVD.SectorTLE := XDFS.Size div 2048;
  PVD.SectorTBE := Intel2Motorola(XDFS.Size div 2048);
  PVD.VStamLE := 1;
  PVD.VStamBE := 256;
  PVD.VSnumLE := 1;
  PVD.VSnumBE := 256;
  PVD.TSectLE := 2048;
  PVD.TSectBE := 8;
  FillChar(PVD.NomVolume, SizeOf(PVD.NomVolume), $20);
  FillChar(PVD.NomPubli, SizeOf(PVD.NomPubli), $20);
  FillChar(PVD.NomCreador, SizeOf(PVD.NomCreador), $20);
  FillChar(PVD.NomApplica, SizeOf(PVD.NomApplica), $20);
  PVD.NomApplica := 'xISO - Yursoft';
     //PVD.NomApplica := 'Creador XISO - Yursoft';
  FillChar(PVD.FichCopy, SizeOf(PVD.FichCopy), $20);
  FillChar(PVD.FichAbst, SizeOf(PVD.FichAbst), $20);
  FillChar(PVD.FichBibl, SizeOf(PVD.FichBibl), $20);
  FillChar(PVD.FeHoCrea, SizeOf(PVD.FeHoCrea) - 1, $30);
  FillChar(PVD.FeHoModi, SizeOf(PVD.FeHoModi) - 1, $30);
  FillChar(PVD.FeHoExpi, SizeOf(PVD.FeHoExpi) - 1, $30);
  FillChar(PVD.FeHoEfec, SizeOf(PVD.FeHoEfec) - 1, $30);
  PVD.VerEstruc := 1;

     // Tabla directorios Little Endian
  CrearTablaDirectorios(0);
     // Tabla directorios Big Endian
  CrearTablaDirectorios(1);

  SectorDirectorioRaiz := XDFS.Position div 2048;
  CrearTablaFicheros();

  XDFS.Seek(PosicionTablaDirectorios, soBeginning);
  CrearTablaDirectorios(0);
  CrearTablaDirectorios(1);

  CrearTablaFicheros();

  PVD.SectorTLE := XDFS.Position div 2048;
  PVD.SectorTBE := Intel2Motorola(XDFS.Position div 2048);

     // Rellenamos la imagen con 150 sectores para
  XDFS.Seek(XDFS.Size, soBeginning);
  FillChar(BufferDirectorios, SizeOf(BufferDirectorios), $00);
  for i := 1 to 32 do
    XDFS.Write(BufferDirectorios, SizeOf(BufferDirectorios));

     // Escribimos el PVD final.
  XDFS.Seek(32768, soBeginning);
  XDFS.Write(PVD, 2048);
  XDFS.Free;
  ListaDirectorios.Free;
end;


end.
