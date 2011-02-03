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

unit xisomakerv3;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Dialogs;

const
  XBOX_MEDIA_ID = 'MICROSOFT*XBOX*MEDIA';

  XBOX_FILE_ATTRIBUTE_READONLY = $01;
  XBOX_FILE_ATTRIBUTE_HIDDEN = $02;
  XBOX_FILE_ATTRIBUTE_SYSTEM = $04;
  XBOX_FILE_ATTRIBUTE_DIRECTORY = $10;
  XBOX_FILE_ATTRIBUTE_FILE = $20;
  XBOX_FILE_ATTRIBUTE_NORMAL = $80;

type
{$A-}
  TXBOX_FS_VOLUME_DESCRIPTOR = record
    IDIn: array[0..19] of AnsiChar;
    RootSector: Integer;
    RootSize: Integer;
    FileTime: FILETIME;
    Blank: array[0..1991] of Byte;
    IDOut: array[0..19] of AnsiChar;
  end;

  PXBOX_FS_ENTRY = ^TXBOX_FS_ENTRY;
  TXBOX_FS_ENTRY = record
    LNode: Word;
    RNode: Word;
    EntrySector: Integer;
    EntrySize: Cardinal;
    Attributes: Byte;
    LongFileName: Byte;
    FileName: array[0..255] of AnsiChar;
  end;
{$A+}

  TDirectoryList = class; // forward

  PFile = ^TFile;
  TFile = record
    FileName: string;
    Size: Integer;
    Attributes: Integer;
    DirectoryPointer: TDirectoryList;

    RNode: Word;
    RecordSize: Word;
  end;

  TDirectoryList = class(TList)
  public
    Parent: TDirectoryList;
    constructor Create(ParentDirectory: TDirectoryList);
    destructor Destroy; override;
    function IsDirectory(FileName: PFile): Boolean;
    function AddEntry(FileName: PFile): Integer;
    function Entry(i: Integer): PFile;
    function Empty: Boolean;
  end;

  TXBOX_FILESYSTEM = class(TObject)
  private
    // Buffer interno de objeto.
    Buffer: array[0..65535] of Byte;
    // Padre del arbol de ficheros.
    Root: TDirectoryList;
    // Devuelve el tamaño de una entrada de directorio.
    function SizeDirEntry(DirectoryList: TDirectoryList): Integer;
    // Crea apartir del parametro Directory la tabla de directorios y ficheros en DirectoryList como padre.
    procedure MakeFileList(Directory: string; DirectoryList: TDirectoryList);
    // Genera la tabla de ficheros de la imagen.
    function NMakeISO(ISOStream: TFileStream; DirectoryList: TDirectoryList; var NextSectorAvailable: Integer): Integer;
    // Genera el VD y llama a NMakeISO para generar el resto de la imagen.
    procedure MakeISO(ISOName: string);
  public
    RootSector: Integer;
    constructor Create;
    destructor Destroy; override;
    // Genera el fichero ISO apartir del directorio y el nombre de la ISO especificado.
    function Make(Directory: string; ISOName: string): Boolean;
  end;

implementation

// Devuelve el offset del sector siguiente al pasado.

function NextSector(Offset: Int64): Int64;
begin
  Result := ((Offset div 2048) * 2048) + 2048;
end;

// Redondea el valor pasado a multiplo de un sector 2048 bytes.

function OffsetToSector(Offset: Int64): Int64;
begin
  if (Offset mod 2048) <> 0 then
    Result := (Offset div 2048) + 1
  else
    Result := Offset div 2048;
end;

///////////// TDirectoryList //////////////

function Compare(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(PFile(Item1)^.FileName, PFile(Item2)^.FileName);
end;

constructor TDirectoryList.Create(ParentDirectory: TDirectoryList);
begin
  Parent := ParentDirectory;

  inherited Create;
end;

destructor TDirectoryList.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if IsDirectory(PFile(Items[i])) then
      if PFile(Items[i]).DirectoryPointer <> nil then
      begin
        PFile(Items[i]).DirectoryPointer.Free;
        Dispose(PFile(Items[i]));
      end;
  end;

  inherited Destroy;
end;

function TDirectoryList.IsDirectory(FileName: PFile): Boolean;
begin
  Result := (FileName.Attributes and XBOX_FILE_ATTRIBUTE_DIRECTORY) = XBOX_FILE_ATTRIBUTE_DIRECTORY;
end;

function TDirectoryList.AddEntry(FileName: PFile): Integer;
begin
  Result := Add(FileName);
end;

function TDirectoryList.Entry(i: Integer): PFile;
begin
  Result := PFile(Items[i]);
end;

function TDirectoryList.Empty: Boolean;
begin
  Result := Count = 0;
end;


///////////// TXBOX_FILESYSTEM //////////////

constructor TXBOX_FILESYSTEM.Create;
begin
  Root := TDirectoryList.Create(nil);
  RootSector := 264;

  inherited Create;
end;

destructor TXBOX_FILESYSTEM.Destroy;
begin
  Root.Free;

  inherited Destroy;
end;


function TXBOX_FILESYSTEM.SizeDirEntry(DirectoryList: TDirectoryList): Integer;
var
  i, PositionDirEntry, SizeEntry: Integer;
  ListEntry: PFile;
begin
  Result := 0;
  if DirectoryList = nil then
    Exit;

  PositionDirEntry := 0;
  for i := 0 to DirectoryList.Count - 1 do
  begin
    ListEntry := DirectoryList.Entry(i);

    SizeEntry := 14 + Length(ListEntry.FileName);

    if (2048 - (PositionDirEntry mod 2048)) < SizeEntry then
      PositionDirEntry := NextSector(PositionDirEntry);

    PositionDirEntry := PositionDirEntry + SizeEntry;

    if (PositionDirEntry mod 4) <> 0 then
      PositionDirEntry := PositionDirEntry + 4 - (PositionDirEntry mod 4);

    if i <> DirectoryList.Count - 1 then
    begin
      SizeEntry := 14 + Length(ExtractFileName(DirectoryList.Entry(i + 1).FileName));
      if (2048 - (PositionDirEntry mod 2048)) < SizeEntry then
        PositionDirEntry := NextSector(PositionDirEntry);
    end;
  end;
  Result := PositionDirEntry;
end;

{$WARN SYMBOL_PLATFORM OFF}


procedure TXBOX_FILESYSTEM.MakeFileList(Directory: string; DirectoryList: TDirectoryList);
var
  SR: TSearchRec;
  Entry: PFile;
begin
  if Directory[Length(Directory)] <> '\' then
    Directory := Directory + '\';

  if (FindFirst(Directory + '*.*', faArchive or faDirectory or faHidden or faSysFile or faReadOnly, SR) = 0) then
  begin
    repeat
      if SR.Name[1] = '.' then
        Continue;

      New(Entry);
      Entry.FileName := Directory + SR.Name;
      Entry.Size := SR.Size;
      Entry.Attributes := SR.Attr;
      DirectoryList.AddEntry(Entry);

      if (Entry.Attributes and faDirectory) = faDirectory then
      begin
        Entry.DirectoryPointer := TDirectoryList.Create(DirectoryList);
        MakeFileList(Entry.FileName, Entry.DirectoryPointer);
      end;
    until (FindNext(SR) <> 0);
    FindClose(SR);
    DirectoryList.Sort(Compare);
  end;
end;

{$WARN SYMBOL_PLATFORM ON}


function TXBOX_FILESYSTEM.NMakeISO(ISOStream: TFileStream; DirectoryList: TDirectoryList; var NextSectorAvailable: Integer): Integer;
var
  i, j: Integer;
  PositionDirEntry, SizeEntry, ReadIt: Int64;
  OffsetDirEntry: Int64;
  F: TFileStream;
  ListEntry: PFile;
  Entry: TXBOX_FS_ENTRY;
  s: string;
begin
  Result := -1;
  if DirectoryList = nil then
    Exit;

  // Realizamos la asignación del Nodo Derecho de la entrada.
  PositionDirEntry := 0;
  for i := 0 to DirectoryList.Count - 1 do
  begin
    ListEntry := DirectoryList.Entry(i);

    SizeEntry := 14 + Length(ExtractFileName(ListEntry.FileName));

    if (2048 - (PositionDirEntry mod 2048)) < SizeEntry then
      PositionDirEntry := NextSector(PositionDirEntry);

    PositionDirEntry := PositionDirEntry + SizeEntry;

    if (PositionDirEntry mod 4) <> 0 then
    begin
      ListEntry.RecordSize := SizeEntry + 4 - (PositionDirEntry mod 4);
      PositionDirEntry := PositionDirEntry + 4 - (PositionDirEntry mod 4);
    end
    else
      ListEntry.RecordSize := SizeEntry;

    if i <> DirectoryList.Count - 1 then
    begin
      SizeEntry := 14 + Length(ExtractFileName(DirectoryList.Entry(i + 1).FileName));
      if (2048 - (PositionDirEntry mod 2048)) < SizeEntry then
        PositionDirEntry := NextSector(PositionDirEntry);
    end;

    if i = DirectoryList.Count - 1 then
      ListEntry.RNode := 0
    else
      ListEntry.RNode := PositionDirEntry div 4;
  end;

  // Offset of the directory.
  OffsetDirEntry := ISOStream.Position;
  NextSectorAvailable := NextSectorAvailable + OffsetToSector(SizeDirEntry(DirectoryList));
  FillChar(Buffer, SizeOf(Buffer), $FF);
  ISOStream.Write(Buffer, OffsetToSector(SizeDirEntry(DirectoryList)) * 2048);
  ISOStream.Seek(NextSectorAvailable * 2048, soBeginning);
  SetEndOfFile(ISOStream.Handle);

  for i := 0 to DirectoryList.Count - 1 do
  begin
    ListEntry := DirectoryList.Entry(i);

    Entry.LNode := 0;
    Entry.RNode := ListEntry.RNode;

    if DirectoryList.IsDirectory(ListEntry) then
    begin
      if not ListEntry.DirectoryPointer.Empty then
      begin
        Entry.EntrySize := SizeDirEntry(ListEntry.DirectoryPointer);
        Entry.EntrySector := NextSectorAvailable;
      end
      else
      begin
        Entry.EntrySize := 0;
        Entry.EntrySector := 0;
      end;
    end
    else
    begin
      Entry.EntrySize := ListEntry.Size;
      Entry.EntrySector := NextSectorAvailable;
    end;
    NextSectorAvailable := NextSectorAvailable + OffsetToSector(Entry.EntrySize);

    Entry.LongFileName := Length(ExtractFileName(ListEntry.FileName));
    Entry.Attributes := ListEntry.Attributes;

    FillChar(Entry.FileName, SizeOf(Entry.FileName), $FF);
    s := ExtractFileName(ListEntry.FileName);
    for j := 1 to Length(s) do
      Entry.FileName[j - 1] := AnsiChar(s[j]);

    // If the Entry is a Directory, we are going to process it.
    // else copy the file into the ISO.
    if DirectoryList.IsDirectory(ListEntry) then
      NMakeISO(ISOStream, ListEntry.DirectoryPointer, NextSectorAvailable)
    else
    begin
      try
        F := TFileStream.Create(ListEntry.FileName, fmOpenRead or fmShareDenyNone);
      except
        F := nil;
      end;
      // Si se ha conseguido abrir el fichero lo archivamos en la ISO.
      // sino metemos un dummy.
      if F <> nil then
      begin
        while (F.Position < F.Size) do
        begin
          ReadIt := F.Read(Buffer, SizeOf(Buffer));
          ISOStream.Write(Buffer, ReadIt);
        end;
        F.Free;
      end
      else
      begin
        j := 0;
        FillChar(Buffer, SizeOf(Buffer), 0);
        while (j < (ListEntry.Size div SizeOf(Buffer))) do
        begin
          ISOStream.Write(Buffer, SizeOf(Buffer));
          j := j + 1;
        end;
        if j <> ListEntry.Size then
          ISOStream.Write(Buffer, ListEntry.Size - j);
      end;

      if (ISOStream.Position mod 2048) <> 0 then
      begin
        ISOStream.Seek(NextSector(ISOStream.Position), soBeginning);
        SetEndOfFile(ISOStream.Handle);
      end;
    end;

    // Comprobamos que el Entry entra en el sector actual, sino saltamos al siguiente.
    if (2048 - (ISOStream.Position mod 2048)) < ListEntry.RecordSize then
    begin
      ISOStream.Seek(NextSector(ISOStream.Position), soBeginning);
      SetEndOfFile(ISOStream.Handle);
    end;

    // Volvemos al punto donde dejamos la tabla de ficheros y copiamos la entrada
    // actual.
    ISOStream.Seek(OffsetDirEntry, soBeginning);
    if (2048 - (ISOStream.Position mod 2048)) < ListEntry.RecordSize then
      ISOStream.Seek(NextSector(ISOStream.Position), soBeginning);
    ISOStream.Write(Entry, ListEntry.RecordSize);
    OffsetDirEntry := ISOStream.Position;
    ISOStream.Seek(0, soEnd);
    // Despues de esto hemos vuelto al final de la ISO.
  end;
  // Comprobamos que el Entry entra en el sector actual, sino saltamos al siguiente.
  if (ISOStream.Position mod 2048) <> 0 then
  begin
    ISOStream.Seek(NextSector(ISOStream.Position), soBeginning);
    SetEndOfFile(ISOStream.Handle);
  end;
end;

procedure TXBOX_FILESYSTEM.MakeISO(ISOName: string);
var
  F: TFileStream;
  XBOX_VD: TXBOX_FS_VOLUME_DESCRIPTOR;
  Sector: Integer;
  ActualDate: FILETIME;
begin
  if not Root.Empty then
  begin
    F := TFileStream.Create(ISOName, fmCreate);
    F.Seek(65536, soBeginning);
    SetEndOfFile(F.Handle);

    if RootSector < 33 then
      RootSector := 33;

    GetSystemTimeAsFileTime(ActualDate);

    FillChar(XBOX_VD, SizeOf(XBOX_VD), 0);
    XBOX_VD.IDIn := XBOX_MEDIA_ID;
    XBOX_VD.RootSector := RootSector;
    XBOX_VD.RootSize := SizeDirEntry(Root);
    XBOX_VD.FileTime := ActualDate;
    XBOX_VD.IDOut := XBOX_MEDIA_ID;
    F.Write(XBOX_VD, SizeOf(XBOX_VD));

    // Establecemos el sector del raiz segun la variable RootSector.
    F.Seek(RootSector * 2048, soBeginning);
    SetEndOfFile(F.Handle);

    Sector := RootSector;
    NMakeISO(F, Root, Sector);
    F.Free;
  end;
end;

function TXBOX_FILESYSTEM.Make(Directory: string; ISOName: string): Boolean;
begin
  MakeFileList(Directory, Root);
  MakeISO(ISOName);
  Result := True;
end;


end.
