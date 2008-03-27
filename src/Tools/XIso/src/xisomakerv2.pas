{
  Unidad: XISOMAKER.PAS
  Creador: Yursoft
  Fecha: 03 de Noviembre de 2002
  Descripcion: Realiza la creacion de imagenes ISO de XBOX

  27 de Enero de 2003:
   -Arreglado un problema en la busqueda de ficheros ocultos, sistema, etc..
  17 de Noviembre de 2002:
   -Mejorado la asignacion de los punteros al nodo siguiente.
  21 de Junio de 2002:
   -Posible solucion a la creacion de imagenes de mas de 2 Gb.
}

unit xisomakerv2;

interface

uses Windows, Messages, Classes, SysUtils, Dialogs, uxiso, Textos, CreacionISO;

type
  TxISOProgreso = procedure(Fichero: string);
  TxISOMensaje = procedure(Mensaje: string);

  TAdminXISO = class
  private
    AdminFicheros: TAdminFicheros;
    function Intel2Motorola(ValorInt32: integer): integer;
    function Intel2MotorolaWORD(Valor: Word): Word;
    function Tamano2Sector(Tamano: int64): int64;
    function TamanoDirectorio(Carpeta: TListaContenido): integer;
    //function Vacio(Carpeta: string): integer;
    function EscanearXISO(Imagen: TFilestream; Carpeta: TListaContenido; var SigSectorVacio: int64): Boolean;
    procedure GenerarTabla(var lDirectorio: TList; var PosBuf: integer; Buffer: Pointer);
    procedure AsignarNodos(var Lista: TList);
  public
    ProgresoxISO: TxISOProgreso;
    MensajesxISO: TxISOMensaje;
    Parar: Boolean;

    constructor Create(AdministradorFicheros: TAdminFicheros);
    function CrearXISO(NombreImagen: string): Boolean;
    function XDFS2ISO9660(Fichero: string): boolean;
  end;

implementation

type
  PByteArray64 = ^TByteArray64;
  TByteArray64 = array[0..65535] of byte;

  PByteArray256 = ^TByteArray256;
  TByteArray256 = array[0..262143] of byte;

{$A-}
  TxVD = record
    IDIn: array[0..19] of char;
    DirRaiz: integer;
    TamRaiz: integer;
    FechaHora: FILETIME;
    SinUso: array[0..1991] of byte;
    IDOut: array[0..19] of char;
  end;

  pxFichero = ^TxFichero;
  TxFichero = record
    pIzq: word;
    pDer: word;
    SectorIn: integer;
    Tamano: integer;
    Atributo: byte;
    LongNombre: byte;
    Nombre: array[0..255] of char;
  end;

  PEntradaDir = ^TEntradaDir;
  TEntradaDir = record
    Nombre: string;
    RutaCompleta: string;
    Tamano: integer;
    SectorIn: integer;
    Sectores: integer;
    Directorio: Boolean;
    NodoDer: Word;
    NodoIzq: Word;
    Atributos: integer;
    Contenido: TListaContenido;
  end;

  TFechaHora = record
    Ano: byte;
    Mes: byte;
    Dia: byte;

    Hora: byte;
    Minuto: byte;
    Segundo: byte;

    DifHora: shortint;
  end;

  TFechaHora2 = record
    Ano: array[0..3] of char;
    Mes: array[0..1] of char;
    Dia: array[0..1] of char;

    Hora: array[0..1] of char;
    Minuto: array[0..1] of char;
    Segundo: array[0..1] of char;

    Cero1: array[0..1] of char;

    Cero2: byte;
  end;

  TDirectorio_Raiz = record
    Longitud: byte;
    Extended: byte;
    SectorLE: integer;
    SectorBE: integer;
    TamanoLE: integer;
    TamanoBE: integer;
    FechaHora: TFechaHora;
    Atributos: byte; // 0 = Fichero, 1 = Fichero Oculto, 2 = Directorio, 3 = Directorio Oculto
    FUnitSize: byte; // File Unit Size ???
    IGapSize: byte; // Interleave Gap Size ???
    VSeqSizeLE: word; // Volume Sequence Size ??? Little Endian
    VSeqSizeBE: word; // Volume Sequence Size ??? Big Endian
    LongNombre: byte; // Longitud del nombre
    Nombre: byte; // Nombre del directorio
  end;

  TPVD = record
    Id1: Byte; // 0 = Boot Record, 1 = PVD, 2 = SVD
                                        // 3 = VPD , 255 = Terminador de VD
    CD001: array[0..4] of char; // Siempre CD001
    Id2: byte; // Version del VD (Volume Descriptor)
    Nulo2: byte; // 1 $00. No se usa.
    IdSistema: array[0..31] of char; // Identificador del CD (interno)
    Etiqueta: array[0..31] of char; // Etiqueta del CD.
    Nulo3: array[0..7] of byte; // 8 $00
    SectorTLE: Integer; // Tamaño total de la imagen en sectores. Little Endian
    SectorTBE: Integer; // Tamaño total de la imagen en sectores. Big Endian
    Nulo4: array[0..31] of char; // 32 $00

    VStamLE: word; // Siempre $1. Tamaño del Volume Set. Little Endian
    VStamBE: word; // Siempre $1. Tamaño del Volume Set. Big Endian
    VSnumLE: word; // Siempre $1. Numero de Volume Sequence. Little Endian
    VSnumBE: word; // Siempre $1. Numero de Volume Sequence. Big Endian

    TSectLE: word; // Siempre 2048. Tamaño Sector. Little Endian
    TSectBE: word; // Siempre 2048. Tamaño Sector. Big Endian

    PTtamLE: integer; // Tamaño de la tabla de directorios. Little Endian
    PTtamBE: integer; // Tamaño de la tabla de directorios. Big Endian

    PTLLE1: Integer; // Sector de la tabla de directorios tipo L 1. Little Endian
    PTLLE2: Integer; // Sector de la tabla de directorios tipo L 2. Little Endian
    PTMBE1: Cardinal; // Sector de la tabla de directorios tipo M 1. Big Endian
    PTMBE2: Cardinal; // Sector de la tabla de directorios tipo M 2. Big Endian

    DirRaiz: TDirectorio_Raiz;

    NomVolume: array[0..127] of char; // Nombre del CD extendido 128 caracteres
    NomPubli: array[0..127] of char; // Nombre del Publicador.
    NomCreador: array[0..127] of char; // Nombre del creador del CD 128 caracteres
    NomApplica: array[0..127] of char; // Nombre del programa con en el que se hizo
    FichCopy: array[0..36] of char; // Copyrigh.txt
    FichAbst: array[0..36] of char; // Abstract.txt
    FichBibl: array[0..36] of char; // Bibliogr.txt

    FeHoCrea: TFechaHora2;
    FeHoModi: TFechaHora2;
    FeHoExpi: TFechaHora2;
    FeHoEfec: TFechaHora2;

    VerEstruc: byte;
    Cero: byte;
    DatosApli: array[0..511] of char;
  end;

  PDirectorios = ^TDirectorios;
  TDirectorios = record
    LongNombre: byte; // Longitud del nombre MAX 31 caracteres
    Nulo: byte; // Numero de sectores en modo Extendido del directorio
    Sector: Integer; // Sector donde se encuentra el Directorio
    Nivel: word; // Nivel a que se encuentra el directorio sobre la raiz. Raiz = 1
    Nombre: array[0..31] of char; // Nombre del directorio
    Pad: byte; // Si la longitud del nombre es Impar se pone 1 sino no existe
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
    Atributos: Byte;
    Nulo1: Byte;
    Nulo2: Byte;
    VolumeSBE: array[0..1] of byte;
    VolumeSLE: array[0..1] of byte;
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

constructor TAdminXISO.Create(AdministradorFicheros: TAdminFicheros);
begin
  AdminFicheros := AdministradorFicheros;
  inherited Create;
end;

//--- 4 Bytes a DWORD formato Motorola

function TAdminXISO.Intel2Motorola(ValorInt32: integer): integer;
var
  b3, b2, b1, b0: byte;
begin
  b0 := (ValorInt32 and $FF000000) shr 24;
  b1 := (ValorInt32 and $00FF0000) shr 16;
  b2 := (ValorInt32 and $0000FF00) shr 8;
  b3 := (ValorInt32 and $000000FF);
  Result := (b3 shl 24) or (b2 shl 16) or (b1 shl 8) or b0;
end;

function TAdminXISO.Intel2MotorolaWORD(Valor: Word): Word;
begin
  Result := (Lo(Valor) shl 8) or (Hi(Valor) shr 8);
end;

function TAdminXISO.Tamano2Sector(Tamano: int64): int64;
begin
  Result := Tamano div 2048;
  if ((Tamano mod 2048) <> 0) then
    Result := Result + 1;
end;

{function TAdminXISO.Vacio(Carpeta: string): integer;
var
   SR: TSearchRec;
   Ent,Resto: integer;
   Entrada: PEntradaDir;
begin
     Result := 0;
     Resto := 0;
     if (FindFirst(Carpeta+'*.*',faArchive or faDirectory or faHidden or faSysFile or faReadOnly,SR) = 0) then
     begin
       repeat
             if (SR.Name[1] = '.') then continue;

             Ent := 14 + Length(SR.Name);
             Resto := ((4 - (Ent mod 4)) mod 4);
             Ent := Ent + Resto;
             Result := Result + Ent;
       until (FindNext(SR) <> 0);
       // Con esto ajustamos el tamaño aunque con la linea siguiente lo anula.
       //if (Resto <> 0) then Result := Result - Resto;
       // Ajustamos a sector
       if (Result mod 2048) <> 0 then
          Result := (Result div 2048)*2048 + 2048;
     end;
end;    }

// Funcion que devuelve el tamaño de un directorio.
// Siempre es multiplo de 2048 (Tamaño Sector)
// Si es 0 entonces el directorio es vacio

function TAdminXISO.TamanoDirectorio(Carpeta: TListaContenido): integer;
var
  Ent, Resto, i: integer;
  Lista: TList;
  Entrada: PEntradaDir;
begin
  Result := 0;
  Lista := TList.Create();

  for i := 0 to Carpeta.Cantidad - 1 do
  begin
    New(Entrada);
    Entrada.Nombre := Carpeta.Entrada[i].Nombre;
    Lista.Add(Entrada);
  end;

  AsignarNodos(Lista);

  for i := 0 to Lista.Count - 1 do
  begin
    Entrada := PEntradaDir(Lista[i]);
    Ent := 14 + Length(Entrada.Nombre);

          // Si no cabe el nombre + los 14 bytes dentro del sector...
          // sino comprobamos si cabe en el sector con la correccion de los 4 bytes
          // sino se graba tal cual el nombre+14 bytes.
    if (((Result div 2048) + 1) * 2048 - Result) < Ent then
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

  Lista.Free;
end;

function Comparar(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(PEntradaDir(Item1)^.Nombre, PEntradaDir(Item2)^.Nombre);
end;

function CompararSectores(Item1, Item2: Pointer): Integer;
begin
  Result := 0;

  if (PEntradaDir(Item1)^.SectorIn < PEntradaDir(Item2)^.SectorIn) then
    Result := -1
  else
    if (PEntradaDir(Item1)^.SectorIn > PEntradaDir(Item2)^.SectorIn) then
      Result := 1
    else
      if (PEntradaDir(Item1)^.SectorIn = PEntradaDir(Item2)^.SectorIn) then
        Result := 0;
end;

procedure TAdminXISO.AsignarNodos(var Lista: TList);
var
  i, j, l, k, Cont: integer;
  Elem, Elem2: PEntradaDir;
  ListaAux: TList;
begin
  ListaAux := TList.Create();
  for i := 0 to Lista.Count - 1 do
  begin
    ListaAux.Add(Lista[i]);
  end;
  ListaAux.Sort(Comparar);

  for i := 0 to ListaAux.Count - 1 do
  begin
    Cont := 0;

    Elem := PEntradaDir(ListaAux[i]);
          //Elem.NodoIzq := 0;
    k := Lista.IndexOf(Elem);
    if (i = ListaAux.Count - 1) then
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

procedure TAdminXISO.GenerarTabla(var lDirectorio: TList; var PosBuf: integer; Buffer: Pointer);
var
  xEntrada: TxFichero;
  i, j: integer;
  Entrada: PEntradaDir;
  Atributos: Byte;
begin
  for i := 0 to lDirectorio.Count - 1 do
  begin
    Entrada := PEntradaDir(lDirectorio[i]);
    xEntrada.pIzq := Entrada.NodoIzq;
    xEntrada.pDer := Entrada.NodoDer;
    xEntrada.SectorIn := Entrada.SectorIn;
    xEntrada.Tamano := Entrada.Tamano;

    if Entrada.Directorio then
      Atributos := XF_DIRECTORIO
    else
      Atributos := XF_FICHERO;

    if (Entrada.Atributos and faReadOnly) = faReadOnly then Atributos := Atributos + XF_SOLOLECTURA;
    if (Entrada.Atributos and faHidden) = faHidden then Atributos := Atributos + XF_OCULTO;
    if (Entrada.Atributos and faSysFile) = faSysFile then Atributos := Atributos + XF_SISTEMA;
          //if (Entrada.Atributos and faDirectory) = faDirectory then Atributos := Atributos + XF_DIRECTORIO;
          //if (Entrada.Atributos and faArchive) = faArchive then Atributos := Atributos + XF_NORMAL;

    xEntrada.Atributo := Atributos;
    xEntrada.LongNombre := Length(Entrada.Nombre);

    for j := 1 to Length(Entrada.Nombre) do
      xEntrada.Nombre[j - 1] := Entrada.Nombre[j];

    if (((PosBuf div 2048) + 1) * 2048 - PosBuf) < (14 + xEntrada.LongNombre) then
      PosBuf := ((PosBuf div 2048) + 1) * 2048;

    Move(xEntrada, PByteArray256(Buffer)^[PosBuf], 14 + xEntrada.LongNombre);
    PosBuf := PosBuf + 14 + xEntrada.LongNombre;
    if (PosBuf mod 4) <> 0 then
      PosBuf := PosBuf + ((4 - (PosBuf mod 4)) mod 4);
  end;
end;

var
  BufCopia: array[0..65535] of byte;

function TAdminXISO.EscanearXISO(Imagen: TFilestream; Carpeta: TListaContenido; var SigSectorVacio: int64): Boolean;
var
  Buffer: Pointer;
  B1: PByteArray64;
  B2: PByteArray256;
  TamBufRellenar: integer;

  lDirectorio, SubDirectorios: TList;
  Entrada: PEntradaDir;
  Fichero: TFileStream;
  SectorTabla: int64;
  PosBuf, leido, i, j: integer;
begin
  if TamanoDirectorio(Carpeta) <= 65536 then
  begin
    New(B1);
    Buffer := B1;
    TamBufRellenar := 65536;
  end
  else
  begin
    if TamanoDirectorio(Carpeta) > 65536 then
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
        //MensajesxISO( rcEscaneandoCarpeta + Carpeta );

  for i := 0 to Carpeta.Cantidad - 1 do
  begin
    if Parar then Exit;
          //if Assigned(ProgresoxISO) then
          //  ProgresoxISO(Carpeta+SR.Name);
    New(Entrada);
    Entrada.Nombre := Carpeta.Entrada[i].Nombre;
    Entrada.RutaCompleta := Carpeta.Entrada[i].RutaCompleta;
    Entrada.Tamano := Carpeta.Entrada[i].Tamano;
    Entrada.Sectores := Tamano2Sector(Carpeta.Entrada[i].Tamano);
    Entrada.Directorio := AdminFicheros.EsDirectorio(Carpeta.Entrada[i]);
    Entrada.Atributos := Carpeta.Entrada[i].Atributos;
    Entrada.Contenido := Carpeta.Entrada[i].Contenido;

    if not Entrada.Directorio then
      lDirectorio.Add(Entrada);

          // Si es directorio lo metemos en la lista de directorios a procesar
          // sino metemos el fichero en la lista de elementos del directorio
    if Entrada.Directorio then
    begin
      Entrada.Tamano := TamanoDirectorio(Carpeta.Entrada[i].Contenido);
      Entrada.Sectores := Tamano2Sector(Entrada.Tamano);

            // Si el directorio esta vacio entonces establecemos a 0 los parametros
            // y no lo añadimos a la lista de directorios
      if Entrada.Tamano = 0 then
      begin
        Entrada.SectorIn := 0;
        Entrada.Sectores := 0;
        lDirectorio.Add(Entrada);
      end
      else
        SubDirectorios.Add(Entrada);
    end;
  end;

          // Introducimos ahora al final las carpetas
  for i := 0 to SubDirectorios.Count - 1 do
  begin
    Entrada := PEntradaDir(SubDirectorios[i]);
    lDirectorio.Add(SubDirectorios[i]);
  end;

        // Nuevo: 18-6-2002
  lDirectorio.Sort(Comparar);

  AsignarNodos(lDirectorio);

        // Nuevo: 18-06-2002
  for i := 0 to lDirectorio.Count - 1 do
  begin
    Entrada := PEntradaDir(lDirectorio[i]);
    if Entrada.Directorio then Continue;
    Entrada.SectorIn := SigSectorVacio;
    SigSectorVacio := SigSectorVacio + Entrada.Sectores;
  end;

  if Assigned(MensajesxISO) then
    MensajesxISO(rcEngGenerandoEntrada);

  Fillchar(Buffer^, TamBufRellenar, $FF);
  GenerarTabla(lDirectorio, PosBuf, Buffer);
        // Extendemos la entrada del directorio hasta alcanzar el final del sector con $FF
        // y grabamos la entrada del directorio en la imagen.
  if (PosBuf mod 2048) <> 0 then
    PosBuf := (PosBuf div 2048) * 2048 + 2048;
  Imagen.Write(Buffer^, PosBuf);

        // Copiamos los ficheros dentro de la imagen
  for i := 0 to lDirectorio.Count - 1 do
  begin
    if Parar then Exit;
    Entrada := PEntradaDir(lDirectorio[i]);
    if Entrada.Directorio then Continue;
    try
      Fichero := TFilestream.Create(Entrada.RutaCompleta, fmOpenRead);
    except
      Parar := True;
      Exit;
    end;
    if Fichero = nil then Continue;
    while (Fichero.Position < Fichero.Size) do
    begin
      Imagen.Seek(Imagen.Size, soBeginning);
      Fillchar(BufCopia, sizeof(BufCopia), 0);
      leido := Fichero.Read(BufCopia, sizeof(BufCopia));
      Imagen.Write(BufCopia, leido);
      if (leido mod 2048) <> 0 then
      begin
        Fillchar(BufCopia, (((leido div 2048) + 1) * 2048) - leido, 0);
        Imagen.Write(BufCopia, (((leido div 2048) + 1) * 2048) - leido);
      end;
               {     Imagen.Seek(Imagen.Size,soBeginning);
                    Fillchar(BufCopia,sizeof(BufCopia),0);
                    Fichero.Read(BufCopia,sizeof(BufCopia));
                    Imagen.Write(BufCopia,sizeof(BufCopia));        }
    end;
    Fichero.Free;
          // 22 de Junio de 2002
    if Assigned(ProgresoxISO) then
      ProgresoxISO(rcEngIntroduciendoFichero + Entrada.Nombre);
  end;

        // Nuevo: 18-06-2002
  lDirectorio.Sort(CompararSectores);

        // Procesamos las subcarpetas
  for i := 0 to SubDirectorios.Count - 1 do
  begin
    if Parar then Exit;
    j := lDirectorio.IndexOf(SubDirectorios[i]);
    PEntradaDir(lDirectorio[j])^.SectorIn := SigSectorVacio;
    SigSectorVacio := SigSectorVacio + PEntradaDir(lDirectorio[j])^.Sectores;
    EscanearXISO(Imagen, PEntradaDir(SubDirectorios[i]).Contenido, SigSectorVacio);
  end;
        // Nuevo: 18-06-2002
  lDirectorio.Sort(Comparar);

  Fillchar(Buffer^, sizeof(Buffer^), $FF);
  PosBuf := 0;
  GenerarTabla(lDirectorio, PosBuf, Buffer);
  if (PosBuf mod 2048) <> 0 then
    PosBuf := (PosBuf div 2048) * 2048 + 2048;

  Imagen.Seek(SectorTabla - TamanoDirectorio(Carpeta) + 2048, soBeginning);
  Imagen.Write(Buffer^, PosBuf);
  Imagen.Seek(Imagen.Size, soBeginning);

  Result := True;

  lDirectorio.Free;
  SubDirectorios.Free;
  Dispose(Buffer);
end;

function TAdminXISO.CrearXISO(NombreImagen: string): Boolean;
var
  VD: TxVD;
  PVD: TPVD;
  Imagen: TFileStream;
  Buffer: PByteArray;
  FechaActual: FILETIME;
  SectorInicio: int64;
  i: integer;
begin
     // Creamos las variables dinamicas e inicializamos
  Imagen := TFileStream.Create(NombreImagen, fmCreate);
  if (Imagen = nil) then exit;
  New(Buffer);
  Fillchar(Buffer^, sizeof(Buffer^), $00);
  Fillchar(VD, sizeof(VD), $00);

     // Rellenamos el PVD y el xPVD
  GetSystemTimeAsFileTime(FechaActual);
  VD.IDIn := XBOX_MEDIA;
  VD.DirRaiz := SECTOR_RAIZ;
  VD.TamRaiz := TamanoDirectorio(AdminFicheros.Raiz);
  VD.FechaHora := FechaActual;
  VD.IDOut := XBOX_MEDIA;

     // Escribimos el PVD (ISO 9660)
  Imagen.Write(Buffer^, sizeof(Buffer^));
  if Assigned(MensajesxISO) then
    MensajesxISO(rcEngEscritoPVD);
     // Escribimos el xPVD (XBOX Estandard)
  Imagen.Write(Buffer^, sizeof(Buffer^));
  Imagen.Write(VD, sizeof(VD));
  if Assigned(MensajesxISO) then
    MensajesxISO(rcEngEscritoXPVD);

     // Iniciamos el escaneo, estructuracion y creacion de la imagen
  SectorInicio := SECTOR_RAIZ + 1;
  SectorInicio := SectorInicio + (TamanoDirectorio(AdminFicheros.Raiz) div 2048) - 1;
  if Assigned(MensajesxISO) then
    MensajesxISO(rcEngInicioCreacion);
  Result := EscanearXISO(Imagen, AdminFicheros.Raiz, SectorInicio);

  if Parar then
    if Assigned(MensajesxISO) then
      MensajesxISO(rcEngParadaAnormal)
    else
      if Assigned(MensajesxISO) then
        MensajesxISO(rcEngFinCreacion);

  Fillchar(PVD, 2048, 0);
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
  Fillchar(PVD.NomVolume, sizeof(PVD.NomVolume), $20);
  Fillchar(PVD.NomPubli, sizeof(PVD.NomPubli), $20);
  Fillchar(PVD.NomCreador, sizeof(PVD.NomCreador), $20);
  Fillchar(PVD.NomApplica, sizeof(PVD.NomApplica), $20);
     //PVD.NomApplica := 'Creador XISO - Yursoft';
  Fillchar(PVD.FichCopy, sizeof(PVD.FichCopy), $20);
  Fillchar(PVD.FichAbst, sizeof(PVD.FichAbst), $20);
  Fillchar(PVD.FichBibl, sizeof(PVD.FichBibl), $20);
  Fillchar(PVD.FeHoCrea, sizeof(PVD.FeHoCrea) - 1, $30);
  Fillchar(PVD.FeHoModi, sizeof(PVD.FeHoModi) - 1, $30);
  Fillchar(PVD.FeHoExpi, sizeof(PVD.FeHoExpi) - 1, $30);
  Fillchar(PVD.FeHoEfec, sizeof(PVD.FeHoEfec) - 1, $30);
  PVD.VerEstruc := 1;

  Imagen.Seek(32768, soBeginning);
  Imagen.Write(PVD, 2048);

  Fillchar(PVD, 2048, 0);
  PVD.Id1 := $FF;
  PVD.CD001 := 'CD001';
  PVD.Id2 := 01;
  Imagen.Write(PVD, 2048);

  Imagen.Seek(Imagen.Size, soBeginning);
  Fillchar(Buffer^, sizeof(Buffer^), $00);
  for i := 1 to 32 do
    Imagen.Write(Buffer^, sizeof(Buffer^));

  Imagen.Free;
end;
 {
function CrearXISO(NombreImagen: string; Carpeta: string): Boolean;
var
   VD: TxVD;
   PVD: TPVD;
   Imagen: TFileStream;
   Buffer: PByteArray;
   FechaActual: FILETIME;
   SectorInicio: int64;
   i: integer;
   b: array[0..8191] of byte;
begin
     if (Carpeta = '') or not DirectoryExists(Carpeta) then exit;
     if Carpeta[Length(Carpeta)] <> '\' then Carpeta := Carpeta + '\';

     // Creamos las variables dinamicas e inicializamos
     Imagen := TFileStream.Create(NombreImagen, fmCreate);
     if (Imagen = nil) then exit;
     new(Buffer);
     Fillchar(Buffer^,sizeof(Buffer^),$00);
     Fillchar(VD,sizeof(VD),$00);

     // Rellenamos el PVD y el xPVD
     GetSystemTimeAsFileTime(FechaActual);
     VD.IDIn      := XBOX_MEDIA;
     //VD.DirRaiz   := SECTOR_RAIZ;
     VD.DirRaiz := 198177;
     VD.TamRaiz   := TamanoDirectorio(Carpeta);
     VD.FechaHora := FechaActual;
     VD.IDOut     := XBOX_MEDIA;

     // Escribimos el PVD (ISO 9660)
     Imagen.Write(Buffer^,sizeof(Buffer^));
     MensajesxISO( rcEscritoPVD );
     // Escribimos el xPVD (XBOX Estandard)
     Imagen.Write(Buffer^,sizeof(Buffer^));
     MensajesxISO( rcEscritoXPVD );

     for i := 1 to 198144 do
        Imagen.Write(b,2048);
     Imagen.Write(VD,sizeof(VD));
     // Iniciamos el escaneo, estructuracion y creacion de la imagen

     //SectorInicio := SECTOR_RAIZ+1;
     SectorInicio := 198177+1;
     SectorInicio := SectorInicio + (TamanoDirectorio(Carpeta) div 2048)-1;
     MensajesxISO( rcInicioCreacion );
     Result := EscanearXISO(Imagen,Carpeta,SectorInicio);
     if Parar then
       MensajesxISO( rcParadaAnormal )
     else
       MensajesxISO( rcFinCreacion );

     Fillchar(PVD,2048,0);
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
     Fillchar(PVD.NomVolume,sizeof(PVD.NomVolume),$20);
     Fillchar(PVD.NomPubli,sizeof(PVD.NomPubli),$20);
     Fillchar(PVD.NomCreador,sizeof(PVD.NomCreador),$20);
     Fillchar(PVD.NomApplica,sizeof(PVD.NomApplica),$20);
     //PVD.NomApplica := 'Creador XISO - Yursoft';
     Fillchar(PVD.FichCopy,sizeof(PVD.FichCopy),$20);
     Fillchar(PVD.FichAbst,sizeof(PVD.FichAbst),$20);
     Fillchar(PVD.FichBibl,sizeof(PVD.FichBibl),$20);
     Fillchar(PVD.FeHoCrea,sizeof(PVD.FeHoCrea)-1,$30);
     Fillchar(PVD.FeHoModi,sizeof(PVD.FeHoModi)-1,$30);
     Fillchar(PVD.FeHoExpi,sizeof(PVD.FeHoExpi)-1,$30);
     Fillchar(PVD.FeHoEfec,sizeof(PVD.FeHoEfec)-1,$30);
     PVD.VerEstruc := 1;

     Imagen.Seek(32768,soBeginning);
     Imagen.Write(PVD,2048);

     Fillchar(PVD,2048,0);
     PVD.Id1 := $FF;
     PVD.CD001 := 'CD001';
     PVD.Id2 := 01;
     Imagen.Write(PVD,2048);

     Imagen.Seek(Imagen.Size,soBeginning);
     Fillchar(Buffer^,sizeof(Buffer^),$00);
     for i := 1 to 32 do
        Imagen.Write(Buffer^,sizeof(Buffer^));

     Imagen.Free;
end;   }

function CompararPosiciones(Item1, Item2: Pointer): Integer;
begin
  if PDirectorios(Item1)^.Nivel < PDirectorios(Item2)^.Nivel then Result := -1
  else
    if PDirectorios(Item1)^.Nivel = PDirectorios(Item2)^.Nivel then Result := 0
    else
      if PDirectorios(Item1)^.Nivel > PDirectorios(Item2)^.Nivel then Result := 1;
end;

function TAdminXISO.XDFS2ISO9660(Fichero: string): boolean;
const
  ByteNulo: byte = 0;
  PuntoComa: array[0..1] of char = ';1';
var
  i: integer;
  XDFS: TFilestream;
  Entrada, EntradaPadre: uxiso.pxFichero;
  ListaDirectorios: TList;
  Directorio: PDirectorios;
  EntradaISO: PTOC;
  PVD: TPVD;
  BufferDirectorios: array[0..65535] of byte;
  PosBufDirectorios: integer;
  PosicionTablaDirectorios: integer;
  LongNombre: byte;
  SectorDirectorioRaiz, OffsetDirectorio, OffsetRestaurado, OffsetDirectorios: int64;

  procedure CrearTablaDirectorios(Modo: byte);
  var
    i: integer;
    OffIni: Integer;
  begin
    OffIni := XDFS.Position div 2048;
    PosBufDirectorios := 0;
    OffsetDirectorios := XDFS.Position;
    ListaDirectorios.Clear;
     // Directorio raiz
    New(Directorio);
    Fillchar(Directorio^, sizeof(Directorio^), 0);
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
        Fillchar(Directorio^, sizeof(Directorio^), 0);
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
             // Si no estamos en un offset par, dejamos un byte en blanco.
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
             // Si no estamos en un offset par, dejamos un byte en blanco.
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
    i, j, t: integer;
  begin
    new(EntradaISO);
    for i := -1 to xIISO.Lista.Count - 1 do
    begin
      OffsetDirectorio := XDFS.Position;
      if i <> -1 then
      begin
        EntradaPadre := uxiso.pxFichero(xIISO.Lista[i]);
        if (EntradaPadre.Atributo and XF_DIRECTORIO) <> XF_DIRECTORIO then Continue;
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
        if Entrada^.DirPadre <> EntradaPadre.DirHijo then continue;
        Fillchar(EntradaISO^, sizeof(EntradaISO^), 0);
        EntradaISO.TamanoLE := Intel2Motorola(Entrada^.Tamano);
        EntradaISO.TamanoBE := Entrada^.Tamano;
        EntradaISO.PSectorLE := Intel2Motorola(Entrada^.SectorIn);
        EntradaISO.PSectorBE := Entrada^.SectorIn;

         // Establecemos atributos. (HAY QUE AMPLIAR)
        if (Entrada.Atributo and XF_DIRECTORIO) = XF_DIRECTORIO then
          EntradaISO.Atributos := 2
        else
          EntradaISO.Atributos := 0;

        if (Entrada.Atributo and XF_OCULTO) = XF_OCULTO then
          EntradaISO.Atributos := EntradaISO.Atributos or 1;

         // Copiamos el nombre del fichero/carpeta al buffer.
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

       // Generamos las 2 entradas iniciales de la carpeta. (68 bytes)
      EntradaISO.Tamano := $22;
      EntradaISO.Extent := 0;
      EntradaISO.PSectorBE := OffsetDirectorio div 2048;
      EntradaISO.PSectorLE := Intel2Motorola(OffsetDirectorio div 2048);
      EntradaISO.TamanoBE := (XDFS.Position - OffsetDirectorio);
      EntradaISO.TamanoLE := Intel2Motorola((XDFS.Position - OffsetDirectorio));
      EntradaISO.Atributos := 2;
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
        PVD.DirRaiz.Atributos := 2;
        PVD.DirRaiz.LongNombre := 1;
        PVD.DirRaiz.Nombre := 0;
      end;
    end;
  end;
begin
  Result := AbrirXISO(Fichero);
  if not Result then Exit;
  ListaDirectorios := TList.Create;
  if ListaDirectorios = nil then
  begin
    Showmessage('Please report this error to xiso@yursoft.com: ListaDirectorios');
    Exit;
  end;
  XDFS := TFilestream.Create(Fichero, fmOpenReadWrite);
  XDFS.Seek(XDFS.Size, soBeginning);

  PosicionTablaDirectorios := XDFS.Size;

     // Rellenamos el PVD
  Fillchar(PVD, sizeof(PVD), 0);
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
  Fillchar(PVD.NomVolume, sizeof(PVD.NomVolume), $20);
  Fillchar(PVD.NomPubli, sizeof(PVD.NomPubli), $20);
  Fillchar(PVD.NomCreador, sizeof(PVD.NomCreador), $20);
  Fillchar(PVD.NomApplica, sizeof(PVD.NomApplica), $20);
  PVD.NomApplica := 'xISO - Yursoft';
     //PVD.NomApplica := 'Creador XISO - Yursoft';
  Fillchar(PVD.FichCopy, sizeof(PVD.FichCopy), $20);
  Fillchar(PVD.FichAbst, sizeof(PVD.FichAbst), $20);
  Fillchar(PVD.FichBibl, sizeof(PVD.FichBibl), $20);
  Fillchar(PVD.FeHoCrea, sizeof(PVD.FeHoCrea) - 1, $30);
  Fillchar(PVD.FeHoModi, sizeof(PVD.FeHoModi) - 1, $30);
  Fillchar(PVD.FeHoExpi, sizeof(PVD.FeHoExpi) - 1, $30);
  Fillchar(PVD.FeHoEfec, sizeof(PVD.FeHoEfec) - 1, $30);
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
  Fillchar(BufferDirectorios, sizeof(BufferDirectorios), $00);
  for i := 1 to 32 do
    XDFS.Write(BufferDirectorios, sizeof(BufferDirectorios));

     // Escribimos el PVD final.
  XDFS.Seek(32768, soBeginning);
  XDFS.Write(PVD, 2048);
  XDFS.Free;
  ListaDirectorios.Free;
end;


end.

