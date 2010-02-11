{
  Unit: XISOMAKER.PAS
  Creator: Yursoft
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

uses
  // Delphi
  Windows, Messages, Classes, SysUtils, Dialogs,
  // Dxbx
  uxiso, TextConsts, CreacionISO;

type
  TxISOProgreso = procedure(aFile: string);
  TxISOMensaje = procedure(aMessage: string);

  TAdminXISO = class(TObject)
  private
    AdminFicheros: TAdminFicheros;
    function Intel2Motorola(ValorInt32: Integer): Integer;
    function Intel2MotorolaWORD(Valor: Word): Word;
    function Size2Sector(Size: Int64): Int64;
    function FolderSize(Folder: TListContents): Integer;
    //function Vacio(Folder: string): Integer;
    function ScanXISO(Image: TFileStream; Folder: TListContents; var SigSectorVacio: Int64): Boolean;
    procedure GenerarTabla(var lDirectorio: TList; var PosBuf: Integer; Buffer: Pointer);
    procedure AssignNodes(var List: TList);
  public
    ProgresoxISO: TxISOProgreso;
    MensajesxISO: TxISOMensaje;
    Stop: Boolean;

    constructor Create(AdministradorFicheros: TAdminFicheros);
    function CrearXISO(ImageName: string): Boolean;
    function XDFS2ISO9660(aFile: string): Boolean;
  end;

implementation

type
  PByteArray64 = ^TByteArray64;
  TByteArray64 = array[0..65535] of Byte;

  PByteArray256 = ^TByteArray256;
  TByteArray256 = array[0..262143] of Byte;

{$A-}
  TxVD = record
    IDIn: array[0..19] of AnsiChar;
    RootDir: Integer;
    RootSize: Integer;
    DateTime: FILETIME;
    Unused: array[0..1991] of Byte;
    IDOut: array[0..19] of AnsiChar;
  end;

  PxFile = ^TxFile;
  TxFile = record
    pLeft: Word;
    pRight: Word;
    SectorIn: Integer;
    Size: Integer;
    Attributes: Byte;
    NameLength: Byte;
    Name: array[0..255] of AnsiChar;
  end;

  PDirectoryEntry = ^TDirectoryEntry;
  TDirectoryEntry = record
    Name: AnsiString;
    FullPath: string;
    Size: Integer;
    SectorIn: Integer;
    Sectores: Integer;
    Directorio: Boolean;
    RightNode: Word;
    LeftNode: Word;
    Attributes: Integer;
    Contents: TListContents;
  end;

  TFechaHora = record
    Year: Byte;
    Month: Byte;
    Day: Byte;

    Hour: Byte;
    Minute: Byte;
    Second: Byte;

    DifHora: shortint;
  end;

  TFechaHora2 = record
    Year: array[0..3] of AnsiChar;
    Month: array[0..1] of AnsiChar;
    Day: array[0..1] of AnsiChar;

    Hour: array[0..1] of AnsiChar;
    Minute: array[0..1] of AnsiChar;
    Second: array[0..1] of AnsiChar;

    Cero1: array[0..1] of AnsiChar;

    Cero2: Byte;
  end;

  TRootDirectory = record
    Length: Byte;
    Extended: Byte;
    SectorLE: Integer;
    SectorBE: Integer;
    TamanoLE: Integer;
    TamanoBE: Integer;
    DateTime: TFechaHora;
    Attributes: Byte; // 0 = aFile, 1 = aFile Oculto, 2 = Directorio, 3 = Directorio Oculto
    FileUnitSize: Byte; // File Unit Size ???
    IGapSize: Byte; // Interleave Gap Size ???
    VSeqSizeLE: Word; // Volume Sequence Size ??? Little Endian
    VSeqSizeBE: Word; // Volume Sequence Size ??? Big Endian
    NameLength: Byte; // Longitud del Name
    Name: Byte; // Name del directorio
  end;

  TPVD = record
    Id1: Byte; // 0 = Boot Record, 1 = PVD, 2 = SVD
                                        // 3 = VPD , 255 = Terminador de VD
    CD001: array[0..4] of AnsiChar; // Siempre CD001
    Id2: Byte; // Version del VD (Volume Descriptor)
    Nulo2: Byte; // 1 $00. No se usa.
    IdSistema: array[0..31] of AnsiChar; // Identificador del CD (interno)
    Etiqueta: array[0..31] of AnsiChar; // Etiqueta del CD.
    Nulo3: array[0..7] of Byte; // 8 $00
    SectorTLE: Integer; // Tamaño total de la Image en sectores. Little Endian
    SectorTBE: Integer; // Tamaño total de la Image en sectores. Big Endian
    Nulo4: array[0..31] of AnsiChar; // 32 $00

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

    RootDir: TRootDirectory;

    NomVolume: array[0..127] of AnsiChar; // Name del CD extendido 128 caracteres
    NomPubli: array[0..127] of AnsiChar; // Name del Publicador.
    NomCreador: array[0..127] of AnsiChar; // Name del creador del CD 128 caracteres
    NomApplica: array[0..127] of AnsiChar; // Name del programa con en el que se hizo
    FichCopy: array[0..36] of AnsiChar; // Copyrigh.txt
    FichAbst: array[0..36] of AnsiChar; // Abstract.txt
    FichBibl: array[0..36] of AnsiChar; // Bibliogr.txt

    FeHoCrea: TFechaHora2;
    FeHoModi: TFechaHora2;
    FeHoExpi: TFechaHora2;
    FeHoEfec: TFechaHora2;

    VerEstruc: Byte;
    Cero: Byte;
    DatosApli: array[0..511] of Byte;
  end;

  PDirectories = ^TDirectories;
  TDirectories = record
    NameLength: Byte; // Longitud del Name MAX 31 caracteres
    Nulo: Byte; // Numero de sectores en modo Extendido del directorio
    Sector: Integer; // Sector donde se encuentra el Directorio
    Level: Word; // Level a que se encuentra el directorio sobre la raiz. Raiz = 1
    Name: array[0..31] of AnsiChar; // Name del directorio
    Pad: Byte; // Si la longitud del Name es Impar se pone 1 sino no existe
  end;

  PTOC = ^TTOC;
  TTOC = record
    Size: Byte;
    Extent: Byte;
    PSectorBE: Integer;
    PSectorLE: Integer;
    TamanoBE: Integer;
    TamanoLE: Integer;
    Year: Byte;
    Month: Byte;
    Day: Byte;
    Hour: Byte;
    Minute: Byte;
    Second: Byte;
    Zona: ShortInt;
    Attributes: Byte;
    Nulo1: Byte;
    Nulo2: Byte;
    VolumeSBE: array[0..1] of Byte;
    VolumeSLE: array[0..1] of Byte;
    NameLength: Byte;
    Name: array[0..49] of AnsiChar;
  end;
{$A+}

const
  XBOX_MEDIA = 'MICROSOFT*XBOX*MEDIA';
  ROOT_SECTOR = 33;

  XF_READONLY = $01;
  XF_HIDDEN = $02;
  XF_SYSTEM = $04;
  XF_DIRECTORY = $10;
  XF_FILE = $20;
  XF_NORMAL = $80;

constructor TAdminXISO.Create(AdministradorFicheros: TAdminFicheros);
begin
  AdminFicheros := AdministradorFicheros;
  inherited Create;
end;

//--- 4 Bytes a DWORD formato Motorola

function TAdminXISO.Intel2Motorola(ValorInt32: Integer): Integer;
var
  b3, b2, b1, b0: Byte;
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

function TAdminXISO.Size2Sector(Size: Int64): Int64;
begin
  Result := Size div 2048;
  if ((Size mod 2048) <> 0) then
    Result := Result + 1;
end;

{function TAdminXISO.Vacio(Folder: string): Integer;
var
   SR: TSearchRec;
   RoundedBytes,RestBytes: Integer;
   Entry: PDirectoryEntry;
begin
     Result := 0;
     RestBytes := 0;
     if (FindFirst(Folder+'*.*',faArchive or faDirectory or faHidden or faSysFile or faReadOnly,SR) = 0) then
     begin
       repeat
             if (SR.Name[1] = '.') then Continue;

             RoundedBytes := 14 + Length(SR.Name);
             RestBytes := ((4 - (RoundedBytes mod 4)) mod 4);
             RoundedBytes := RoundedBytes + RestBytes;
             Result := Result + RoundedBytes;
       until (FindNext(SR) <> 0);
       // Con esto ajustamos el tamaño aunque con la linea siguiente lo anula.
       //if (RestBytes <> 0) then Result := Result - RestBytes;
       // Ajustamos a sector
       if (Result mod 2048) <> 0 then
          Result := (Result div 2048)*2048 + 2048;
     end;
end;    }

// Funcion que devuelve el tamaño de un directorio.
// Siempre es multiplo de 2048 (Tamaño Sector)
// Si es 0 entonces el directorio es vacio

function TAdminXISO.FolderSize(Folder: TListContents): Integer;
var
  RoundedBytes, i: Integer;
  List: TList;
  Entry: PDirectoryEntry;
begin
  Result := 0;
  List := TList.Create();

  for i := 0 to Folder.Count - 1 do
  begin
    New(Entry);
    Entry.Name := AnsiString(Folder.Entry[i].Name);
    List.Add(Entry);
  end;

  AssignNodes(List);

  for i := 0 to List.Count - 1 do
  begin
    Entry := PDirectoryEntry(List[i]);
    RoundedBytes := 14 + Length(Entry.Name);

          // Si no cabe el Name + los 14 bytes dentro del sector...
          // sino comprobamos si cabe en el sector con la correccion de los 4 bytes
          // sino se graba tal cual el Name+14 bytes.
    if (((Result div 2048) + 1) * 2048 - Result) < RoundedBytes then
    begin
      Result := ((Result div 2048) + 1) * 2048;

      if (RoundedBytes mod 4) <> 0 then
        RoundedBytes := ((RoundedBytes div 4) + 1) * 4;

            // Si es el ultimo aFile de la tabla entonces ignoramos el RoundedBytes.
      if i < List.Count - 1 then
        Result := Result + RoundedBytes;
    end
    else
    begin
      if (RoundedBytes mod 4) <> 0 then
        RoundedBytes := ((RoundedBytes div 4) + 1) * 4;

      if (((Result div 2048) + 1) * 2048 - Result) < RoundedBytes then
        Result := Result + (14 + Length(Entry.Name))
      else
        Result := Result + RoundedBytes;
    end;
  end;

       // Ajustamos a sector
  if (Result mod 2048) <> 0 then
    Result := ((Result div 2048) + 1) * 2048;

  List.Free;
end;

function Comparar(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(string(PDirectoryEntry(Item1)^.Name), string(PDirectoryEntry(Item2)^.Name));
end;

function CompararSectores(Item1, Item2: Pointer): Integer;
begin
  Result := 0;

  if (PDirectoryEntry(Item1)^.SectorIn < PDirectoryEntry(Item2)^.SectorIn) then
    Result := -1
  else
    if (PDirectoryEntry(Item1)^.SectorIn > PDirectoryEntry(Item2)^.SectorIn) then
      Result := 1
    else
      if (PDirectoryEntry(Item1)^.SectorIn = PDirectoryEntry(Item2)^.SectorIn) then
        Result := 0;
end;

procedure TAdminXISO.AssignNodes(var List: TList);
var
  i, j, l, k, Cont: Integer;
  Elem, Elem2: PDirectoryEntry;
  ListaAux: TList;
begin
  ListaAux := TList.Create();
  for i := 0 to List.Count - 1 do
  begin
    ListaAux.Add(List[i]);
  end;
  ListaAux.Sort(Comparar);

  for i := 0 to ListaAux.Count - 1 do
  begin
    Cont := 0;

    Elem := PDirectoryEntry(ListaAux[i]);
          //Elem.LeftNode := 0;
    k := List.IndexOf(Elem);
    if (i = ListaAux.Count - 1) then
    begin
      PDirectoryEntry(List[k]).LeftNode := 0;
      PDirectoryEntry(List[k]).RightNode := 0;
      Break;
    end;

    Elem := PDirectoryEntry(ListaAux[i + 1]);
          //Elem.LeftNode := 0;
    j := List.IndexOf(Elem);

    for l := 0 to j - 1 do
    begin
      Elem2 := PDirectoryEntry(List[l]);
                // Si no queda bytes libres en el sector para introducir la
                // nueva Entry, entonces saltamos al siguiente sector.
      if (((Cont div 2048) + 1) * 2048 - Cont) < (14 + Length(Elem2.Name)) then
      begin
        Cont := ((Cont div 2048) + 1) * 2048 + 14 + Length(Elem2.Name);
        if l = j - 1 then
        begin
          PDirectoryEntry(List[k - 1])^.RightNode := ((((PDirectoryEntry(List[k - 1])^.RightNode * 4) div 2048) + 1) * 2048) div 4;

        end;
      end
      else
        Cont := (Cont + 14 + Length(Elem2.Name));
      if (Cont mod 4) <> 0 then Cont := (cont div 4) * 4 + 4;
    end;

    PDirectoryEntry(List[k])^.LeftNode := 0;
    PDirectoryEntry(List[k])^.RightNode := Cont div 4;
  end;
  ListaAux.Free;
end;

{$WARN SYMBOL_PLATFORM OFF}

procedure TAdminXISO.GenerarTabla(var lDirectorio: TList; var PosBuf: Integer; Buffer: Pointer);
var
  xEntry: TxFile;
  i, j: Integer;
  Entry: PDirectoryEntry;
  Attributes: Byte;
begin
  for i := 0 to lDirectorio.Count - 1 do
  begin
    Entry := PDirectoryEntry(lDirectorio[i]);
    xEntry.pLeft := Entry.LeftNode;
    xEntry.pRight := Entry.RightNode;
    xEntry.SectorIn := Entry.SectorIn;
    xEntry.Size := Entry.Size;

    if Entry.Directorio then
      Attributes := XF_DIRECTORY
    else
      Attributes := XF_FILE;

    if (Entry.Attributes and faReadOnly) = faReadOnly then Attributes := Attributes + XF_READONLY;
    if (Entry.Attributes and faHidden) = faHidden then Attributes := Attributes + XF_HIDDEN;
    if (Entry.Attributes and faSysFile) = faSysFile then Attributes := Attributes + XF_SYSTEM;
          //if (Entry.Attributes and faDirectory) = faDirectory then Attributes := Attributes + XF_DIRECTORY;
          //if (Entry.Attributes and faArchive) = faArchive then Attributes := Attributes + XF_NORMAL;

    xEntry.Attributes := Attributes;
    xEntry.NameLength := Length(Entry.Name);

    for j := 1 to Length(Entry.Name) do
      xEntry.Name[j - 1] := Entry.Name[j];

    if (((PosBuf div 2048) + 1) * 2048 - PosBuf) < (14 + xEntry.NameLength) then
      PosBuf := ((PosBuf div 2048) + 1) * 2048;

    Move(xEntry, PByteArray256(Buffer)^[PosBuf], 14 + xEntry.NameLength);
    PosBuf := PosBuf + 14 + xEntry.NameLength;
    if (PosBuf mod 4) <> 0 then
      PosBuf := PosBuf + ((4 - (PosBuf mod 4)) mod 4);
  end;
end;

{$WARN SYMBOL_PLATFORM ON}


var
  BufCopia: array[0..65535] of Byte;

function TAdminXISO.ScanXISO(Image: TFileStream; Folder: TListContents; var SigSectorVacio: Int64): Boolean;
var
  Buffer: Pointer;
  B1: PByteArray64;
  B2: PByteArray256;
  TamBufRellenar: Integer;

  lDirectorio, SubDirectories: TList;
  Entry: PDirectoryEntry;
  aFile: TFileStream;
  SectorTabla: Int64;
  PosBuf, leido, i, j: Integer;
begin
  aFile := nil;
  Buffer := nil;
  TamBufRellenar := -1;
  Result := False;

  if FolderSize(Folder) <= 65536 then
  begin
    New(B1);
    Buffer := B1;
    TamBufRellenar := 65536;
  end
  else
  begin
    if FolderSize(Folder) > 65536 then
    begin
      New(B2);
      Buffer := B2;
      TamBufRellenar := 262144;
    end;
  end;

  lDirectorio := TList.Create();
  SubDirectories := TList.Create();
  SectorTabla := (SigSectorVacio - 1) * 2048;
  PosBuf := 0;
  Image.Seek(Image.Size, soBeginning);
        //MensajesxISO( rcEscaneandoCarpeta + Folder );

  for i := 0 to Folder.Count - 1 do
  begin
    if Stop then
      Exit;
      
          //if Assigned(ProgresoxISO) then
          //  ProgresoxISO(Folder+SR.Name);
    New(Entry);
    Entry.Name := AnsiString(Folder.Entry[i].Name);
    Entry.FullPath := Folder.Entry[i].FullPath;
    Entry.Size := Folder.Entry[i].Size;
    Entry.Sectores := Size2Sector(Folder.Entry[i].Size);
    Entry.Directorio := AdminFicheros.EsDirectorio(Folder.Entry[i]);
    Entry.Attributes := Folder.Entry[i].Attributes;
    Entry.Contents := Folder.Entry[i].Contents;

    if not Entry.Directorio then
      lDirectorio.Add(Entry);

          // Si es directorio lo metemos en la List de directorios a procesar
          // sino metemos el aFile en la List de elementos del directorio
    if Entry.Directorio then
    begin
      Entry.Size := FolderSize(Folder.Entry[i].Contents);
      Entry.Sectores := Size2Sector(Entry.Size);

            // Si el directorio esta vacio entonces establecemos a 0 los parametros
            // y no lo añadimos a la List de directorios
      if Entry.Size = 0 then
      begin
        Entry.SectorIn := 0;
        Entry.Sectores := 0;
        lDirectorio.Add(Entry);
      end
      else
        SubDirectories.Add(Entry);
    end;
  end;

  // Introducimos ahora al final las carpetas
  for i := 0 to SubDirectories.Count - 1 do
  begin
    //PDirectoryEntry(SubDirectories[i]);
    lDirectorio.Add(SubDirectories[i]);
  end;

  // Nuevo: 18-6-2002
  lDirectorio.Sort(Comparar);

  AssignNodes(lDirectorio);

  // Nuevo: 18-06-2002
  for i := 0 to lDirectorio.Count - 1 do
  begin
    Entry := PDirectoryEntry(lDirectorio[i]);
    if Entry.Directorio then
      Continue;
      
    Entry.SectorIn := SigSectorVacio;
    SigSectorVacio := SigSectorVacio + Entry.Sectores;
  end;

  if Assigned(MensajesxISO) then
    MensajesxISO(SGenerandoEntrada);

  FillChar(Buffer^, TamBufRellenar, $FF);
  GenerarTabla(lDirectorio, PosBuf, Buffer);
        // Extendemos la Entry del directorio hasta alcanzar el final del sector con $FF
        // y grabamos la Entry del directorio en la Image.
  if (PosBuf mod 2048) <> 0 then
    PosBuf := (PosBuf div 2048) * 2048 + 2048;
  Image.Write(Buffer^, PosBuf);

        // Copiamos los ficheros dentro de la Image
  for i := 0 to lDirectorio.Count - 1 do
  begin
    if Stop then
      Exit;
      
    Entry := PDirectoryEntry(lDirectorio[i]);
    if Entry.Directorio then
      Continue;

    try
      aFile := TFileStream.Create(Entry.FullPath, fmOpenRead);
    except
      Stop := True;
      Exit;
    end;

    if aFile = nil then
      Continue;

    while (aFile.Position < aFile.Size) do
    begin
      Image.Seek(Image.Size, soBeginning);
      FillChar(BufCopia, SizeOf(BufCopia), 0);
      leido := aFile.Read(BufCopia, SizeOf(BufCopia));
      Image.Write(BufCopia, leido);
      if (leido mod 2048) <> 0 then
      begin
        FillChar(BufCopia, (((leido div 2048) + 1) * 2048) - leido, 0);
        Image.Write(BufCopia, (((leido div 2048) + 1) * 2048) - leido);
      end;
               {     Image.Seek(Image.Size,soBeginning);
                    FillChar(BufCopia,SizeOf(BufCopia),0);
                    aFile.Read(BufCopia,SizeOf(BufCopia));
                    Image.Write(BufCopia,SizeOf(BufCopia));        }
    end;
    aFile.Free;
          // 22 de Junio de 2002
    if Assigned(ProgresoxISO) then
      ProgresoxISO(SIntroduciendoFichero + string(Entry.Name));
  end;

        // Nuevo: 18-06-2002
  lDirectorio.Sort(CompararSectores);

        // Procesamos las subcarpetas
  for i := 0 to SubDirectories.Count - 1 do
  begin
    if Stop then
      Exit;
      
    j := lDirectorio.IndexOf(SubDirectories[i]);
    PDirectoryEntry(lDirectorio[j])^.SectorIn := SigSectorVacio;
    SigSectorVacio := SigSectorVacio + PDirectoryEntry(lDirectorio[j])^.Sectores;
    ScanXISO(Image, PDirectoryEntry(SubDirectories[i]).Contents, SigSectorVacio);
  end;
        // Nuevo: 18-06-2002
  lDirectorio.Sort(Comparar);

  FillChar(Buffer^, SizeOf(Buffer^), $FF);
  PosBuf := 0;
  GenerarTabla(lDirectorio, PosBuf, Buffer);
  if (PosBuf mod 2048) <> 0 then
    PosBuf := (PosBuf div 2048) * 2048 + 2048;

  Image.Seek(SectorTabla - FolderSize(Folder) + 2048, soBeginning);
  Image.Write(Buffer^, PosBuf);
  Image.Seek(Image.Size, soBeginning);

  Result := True;

  lDirectorio.Free;
  SubDirectories.Free;
  Dispose(Buffer);
end;

function TAdminXISO.CrearXISO(ImageName: string): Boolean;
var
  VD: TxVD;
  PVD: TPVD;
  Image: TFileStream;
  Buffer: PByteArray;
  CurrentDate: FILETIME;
  SectorInicio: Int64;
  i: Integer;
begin
  Result := False;

  // Creamos las variables dinamicas e inicializamos
  Image := TFileStream.Create(ImageName, fmCreate);
  if Image = nil then
    Exit;

  New(Buffer);
  FillChar(Buffer^, SizeOf(Buffer^), $00);
  FillChar(VD, SizeOf(VD), $00);

     // Rellenamos el PVD y el xPVD
  GetSystemTimeAsFileTime(CurrentDate);
  VD.IDIn := XBOX_MEDIA;
  VD.RootDir := ROOT_SECTOR;
  VD.RootSize := FolderSize(AdminFicheros.Root);
  VD.DateTime := CurrentDate;
  VD.IDOut := XBOX_MEDIA;

     // Escribimos el PVD (ISO 9660)
  Image.Write(Buffer^, SizeOf(Buffer^));
  if Assigned(MensajesxISO) then
    MensajesxISO(SEscritoPVD);
     // Escribimos el xPVD (XBOX Estandard)
  Image.Write(Buffer^, SizeOf(Buffer^));
  Image.Write(VD, SizeOf(VD));
  if Assigned(MensajesxISO) then
    MensajesxISO(SEscritoXPVD);

     // Iniciamos el escaneo, estructuracion y creacion de la Image
  SectorInicio := ROOT_SECTOR + 1;
  SectorInicio := SectorInicio + (FolderSize(AdminFicheros.Root) div 2048) - 1;
  if Assigned(MensajesxISO) then
    MensajesxISO(SInicioCreacion);
  Result := ScanXISO(Image, AdminFicheros.Root, SectorInicio);

  if Stop then
    if Assigned(MensajesxISO) then
      MensajesxISO(SParadaAnormal)
    else
      if Assigned(MensajesxISO) then
        MensajesxISO(SFinCreacion);

  FillChar(PVD, 2048, 0);
  PVD.Id1 := 01;
  PVD.CD001 := 'CD001';
  PVD.Id2 := 01;
  PVD.SectorTLE := Image.Size div 2048;
  PVD.SectorTBE := Intel2Motorola(Image.Size div 2048);
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

  Image.Seek(32768, soBeginning);
  Image.Write(PVD, 2048);

  FillChar(PVD, 2048, 0);
  PVD.Id1 := $FF;
  PVD.CD001 := 'CD001';
  PVD.Id2 := 01;
  Image.Write(PVD, 2048);

  Image.Seek(Image.Size, soBeginning);
  FillChar(Buffer^, SizeOf(Buffer^), $00);
  for i := 1 to 32 do
    Image.Write(Buffer^, SizeOf(Buffer^));

  Image.Free;
end;
 {
function CrearXISO(ImageName: string; Folder: string): Boolean;
var
   VD: TxVD;
   PVD: TPVD;
   Image: TFileStream;
   Buffer: PByteArray;
   CurrentDate: FILETIME;
   SectorInicio: Int64;
   i: Integer;
   b: array[0..8191] of Byte;
begin
     if (Folder = '') or not DirectoryExists(Folder) then
      Exit;
     if Folder[Length(Folder)] <> '\' then Folder := Folder + '\';

     // Creamos las variables dinamicas e inicializamos
     Image := TFileStream.Create(ImageName, fmCreate);
     if (Image = nil) then
      Exit;
     New(Buffer);
     FillChar(Buffer^,SizeOf(Buffer^),$00);
     FillChar(VD,SizeOf(VD),$00);

     // Rellenamos el PVD y el xPVD
     GetSystemTimeAsFileTime(CurrentDate);
     VD.IDIn      := XBOX_MEDIA;
     //VD.RootDir   := ROOT_SECTOR;
     VD.RootDir := 198177;
     VD.RootSize   := FolderSize(Folder);
     VD.DateTime := CurrentDate;
     VD.IDOut     := XBOX_MEDIA;

     // Escribimos el PVD (ISO 9660)
     Image.Write(Buffer^,SizeOf(Buffer^));
     MensajesxISO( rcEscritoPVD );
     // Escribimos el xPVD (XBOX Estandard)
     Image.Write(Buffer^,SizeOf(Buffer^));
     MensajesxISO( rcEscritoXPVD );

     for i := 1 to 198144 do
        Image.Write(b,2048);
     Image.Write(VD,SizeOf(VD));
     // Iniciamos el escaneo, estructuracion y creacion de la Image

     //SectorInicio := ROOT_SECTOR+1;
     SectorInicio := 198177+1;
     SectorInicio := SectorInicio + (FolderSize(Folder) div 2048)-1;
     MensajesxISO( rcInicioCreacion );
     Result := ScanXISO(Image,Folder,SectorInicio);
     if Stop then
       MensajesxISO( rcParadaAnormal )
     else
       MensajesxISO( rcFinCreacion );

     FillChar(PVD,2048,0);
     PVD.Id1 := 01;
     PVD.CD001 := 'CD001';
     PVD.Id2 := 01;
     PVD.SectorTLE := Image.Size div 2048;
     PVD.SectorTBE := Intel2Motorola(Image.Size div 2048);
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

     Image.Seek(32768,soBeginning);
     Image.Write(PVD,2048);

     FillChar(PVD,2048,0);
     PVD.Id1 := $FF;
     PVD.CD001 := 'CD001';
     PVD.Id2 := 01;
     Image.Write(PVD,2048);

     Image.Seek(Image.Size,soBeginning);
     FillChar(Buffer^,SizeOf(Buffer^),$00);
     for i := 1 to 32 do
        Image.Write(Buffer^,SizeOf(Buffer^));

     Image.Free;
end;   }

function CompararPosiciones(Item1, Item2: Pointer): Integer;
begin
  if PDirectories(Item1)^.Level < PDirectories(Item2)^.Level then Result := -1
  else
    if PDirectories(Item1)^.Level = PDirectories(Item2)^.Level then Result := 0
    else
      if PDirectories(Item1)^.Level > PDirectories(Item2)^.Level then Result := 1
      else
        Result := 0;
end;

function TAdminXISO.XDFS2ISO9660(aFile: string): Boolean;
const
  ZeroByte: Byte = 0;
//  PuntoComa: array[0..1] of Char = ';1';
var
  i: Integer;
  XDFS: TFileStream;
  Entry, ParentEntry: uxiso.PxFile;
  ListaDirectorios: TList;
  Directorio: PDirectories;
  ISOEntry: PTOC;
  PVD: TPVD;
  BufferDirectorios: array[0..65535] of Byte;
  PosBufDirectorios: Integer;
  PosicionTablaDirectorios: Integer;
  NameLength: Byte;
  SectorDirectorioRaiz, DirectoryOffset, PreviousOffset, OffsetDirectorios: Int64;

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
    Directorio.NameLength := 1;
    if Modo = 0 then
    begin
      Directorio^.Sector := SectorDirectorioRaiz;
      Directorio^.Level := 1;
    end
    else
      if Modo = 1 then
      begin
        Directorio^.Sector := Intel2Motorola(SectorDirectorioRaiz);
        Directorio^.Level := Intel2MotorolaWORD(1);
      end;

    ListaDirectorios.Add(Directorio);

     // Si estamos sin espacio en este sector saltamos al siguiente.
     //Move(Directorio^,BufferDirectorios[PosBufDirectorios],8+Directorio.NameLength+1);
     //PosBufDirectorios := PosBufDirectorios + 8 + Directorio.NameLength+1;
     // Escaneamos en busca de directorios
    for i := 0 to xISO.List.Count - 1 do
    begin
      Entry := uxiso.PxFile(xISO.List[i]);
      if (Entry.Attributes and XF_DIRECTORY) = XF_DIRECTORY then
      begin
        New(Directorio);
        FillChar(Directorio^, SizeOf(Directorio^), 0);
           {  if (Directorio.NameLength mod 2) <> 0 then
             begin
               if Entry^.NameLength+1 > 32 then
                 NameLength := 32
               else
                 NameLength := Entry^.NameLength+1;
             end
             else  }
        begin
          if Entry^.NameLength > 30 then
            NameLength := 30
          else
            NameLength := Entry^.NameLength;
        end;
        Directorio^.NameLength := NameLength;
        if Modo = 0 then
        begin
          Directorio^.Sector := Entry^.SectorIn;
          Directorio^.Level := Entry^.ParentDir + 1;
        end
        else
          if Modo = 1 then
          begin
            Directorio^.Sector := Intel2Motorola(Entry^.SectorIn);
            Directorio^.Level := Intel2MotorolaWORD(Entry^.ParentDir + 1);
          end;
        Move(Entry^.Name, Directorio^.Name, NameLength);
        ListaDirectorios.Add(Directorio);

        {     // Si estamos sin espacio en este sector saltamos al siguiente.
             //if ((PosBufDirectorios mod 2048) < (8+NameLength)) and (PosBufDirectorios > 2048) then
             if (((XDFS.Position div 2048)+1)*2048)-XDFS.Position < (8+NameLength) then
               PosBufDirectorios := ((PosBufDirectorios div 2048)+1)*2048;
             Move(Directorio^,BufferDirectorios[PosBufDirectorios],8+NameLength);
             PosBufDirectorios := PosBufDirectorios + 8 + NameLength;
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
             if Trim(Directorio.Name) = '' then
               ListaDirectorios.Move(i,0);
     end;  }

    for i := 0 to ListaDirectorios.Count - 1 do
    begin
      Directorio := ListaDirectorios[i];
      NameLength := Directorio^.NameLength;

             // Si estamos sin espacio en este sector saltamos al siguiente.
             //if ((PosBufDirectorios mod 2048) < (8+NameLength)) and (PosBufDirectorios > 2048) then
      if (((XDFS.Position div 2048) + 1) * 2048) - XDFS.Position < (8 + NameLength) then
        PosBufDirectorios := ((PosBufDirectorios div 2048) + 1) * 2048;
      Move(Directorio^, BufferDirectorios[PosBufDirectorios], 8 + NameLength);
      PosBufDirectorios := PosBufDirectorios + 8 + NameLength;
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
        Directorio := PDirectories(ListaDirectorios[i]);

        // Si estamos sin espacio en este sector saltamos al siguiente.
        if ((PosBufDirectorios mod 2048) < (8+Directorio.NameLength+1)) and (PosBufDirectorios > 2048) then
          PosBufDirectorios := ((PosBufDirectorios div 2048)+1)*2048;
        Move(Directorio^,BufferDirectorios[PosBufDirectorios],8+Directorio.NameLength);
        PosBufDirectorios := PosBufDirectorios + 8 + Directorio.NameLength;
     end;
     if (PosBufDirectorios mod 2048) <> 0 then
       PosBufDirectorios := ((PosBufDirectorios div 2048)+1)*2048;      }
  end;

  procedure CrearTablaFicheros;
  var
    i, j, t: Integer;
  begin
    New(ISOEntry);
    for i := -1 to xISO.List.Count - 1 do
    begin
      DirectoryOffset := XDFS.Position;
      if i <> -1 then
      begin
        ParentEntry := uxiso.PxFile(xISO.List[i]);
        if (ParentEntry.Attributes and XF_DIRECTORY) <> XF_DIRECTORY then
          Continue;
          
        ParentEntry.SectorIn := XDFS.Position div 2048;
        {if ParentEntry.ParentDir = 0 then ParentEntry.pLeft := 1
        else
        begin
         for t := 0 to i do
            if ParentEntry.ParentDir = xiso.PxFile(xISO.List[t]).ChildDir then
              ParentEntry.pLeft := xiso.PxFile(xISO.List[t]).pLeft+1;
        end;}
      end
      else
      begin
        New(ParentEntry);
        ParentEntry.pLeft := 1;
        ParentEntry.ChildDir := 0;
      end;

       //Dejamos espacio para grabar mas tarde las 2 entradas del directorio.
      for t := 1 to 68 do
        XDFS.Write(ZeroByte, 1);
       // We began the search for elements of this level.
      for j := i + 1 to xISO.List.Count - 1 do
      begin
        Entry := uxiso.PxFile(xISO.List[j]);
        if Entry^.ParentDir <> ParentEntry.ChildDir then
          Continue;
          
        FillChar(ISOEntry^, SizeOf(ISOEntry^), 0);
        ISOEntry.TamanoLE := Intel2Motorola(Entry^.Size);
        ISOEntry.TamanoBE := Entry^.Size;
        ISOEntry.PSectorLE := Intel2Motorola(Entry^.SectorIn);
        ISOEntry.PSectorBE := Entry^.SectorIn;

         // Establecemos Attributes. (HAY QUE AMPLIAR)
        if (Entry.Attributes and XF_DIRECTORY) = XF_DIRECTORY then
          ISOEntry.Attributes := 2
        else
          ISOEntry.Attributes := 0;

        if (Entry.Attributes and XF_HIDDEN) = XF_HIDDEN then
          ISOEntry.Attributes := ISOEntry.Attributes or 1;

         // Copiamos el Name del aFile/Folder al buffer.
         {if Entry^.NameLength > 30 then
           NameLength := 30
         else                  }
        NameLength := Entry^.NameLength;
        if (NameLength mod 2) <> 0 then NameLength := NameLength + 1;
        ISOEntry^.NameLength := NameLength;
        Move(Entry^.Name, ISOEntry^.Name, NameLength);
        ISOEntry.Size := 33 + NameLength;
        if ((((XDFS.Position div 2048) + 1) * 2048) - XDFS.Position < (ISOEntry^.Size)) then
          for t := 1 to (((XDFS.Position div 2048) + 1) * 2048) - XDFS.Position do
            XDFS.Write(ZeroByte, 1);
        XDFS.Write(ISOEntry^, ISOEntry^.Size);
        { if (XDFS.Position mod 2) <> 0 then
           XDFS.Write(ZeroByte,1);}
      end;
      if (XDFS.Position mod 2048) <> 0 then
        for t := 1 to (((XDFS.Position div 2048) + 1) * 2048) - XDFS.Position do
          XDFS.Write(ZeroByte, 1);

      // Generamos las 2 entradas iniciales de la Folder. (68 bytes)
      ISOEntry.Size := $22;
      ISOEntry.Extent := 0;
      ISOEntry.PSectorBE := DirectoryOffset div 2048;
      ISOEntry.PSectorLE := Intel2Motorola(DirectoryOffset div 2048);
      ISOEntry.TamanoBE := (XDFS.Position - DirectoryOffset);
      ISOEntry.TamanoLE := Intel2Motorola((XDFS.Position - DirectoryOffset));
      ISOEntry.Attributes := 2;
      ISOEntry.NameLength := 1;
      ISOEntry.Name[0] := #00;
      PreviousOffset := XDFS.Position;
      XDFS.Seek(DirectoryOffset, soBeginning);
      XDFS.Write(ISOEntry^, ISOEntry^.Size);
      XDFS.Write(ISOEntry^, ISOEntry^.Size);
      XDFS.Seek(PreviousOffset, soBeginning);
      //SectorDirectorioRaiz := DirectoryOffset div 2048;

      if i = -1 then
      begin
        PVD.RootDir.Length := $22;
        PVD.RootDir.SectorLE := ISOEntry.PSectorBE;
        PVD.RootDir.SectorBE := ISOEntry.PSectorLE;
        PVD.RootDir.TamanoLE := ISOEntry.TamanoBE;
        PVD.RootDir.TamanoBE := ISOEntry.TamanoLE;
        PVD.RootDir.Attributes := 2;
        PVD.RootDir.NameLength := 1;
        PVD.RootDir.Name := 0;
      end;
    end;
  end;
begin
  Result := OpenXISO(aFile);
  if not Result then
    Exit;
    
  ListaDirectorios := TList.Create;
  if ListaDirectorios = nil then
  begin
    ShowMessage('Please report this error to xiso@yursoft.com: ListaDirectorios');
    Exit;
  end;
  
  XDFS := TFileStream.Create(aFile, fmOpenReadWrite);
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

     // Rellenamos la Image con 150 sectores para
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
