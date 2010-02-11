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
  PListContents = ^TListContents;
  PEntry = ^TEntry;

  TListContents = class(TList)
  private
    function GetEntry(i: Integer): PEntry;
    function IsPresent(Name: string): Boolean;
  public
    constructor Create;
    function Append(NewEntry: PEntry): Boolean;
    procedure Remove(i: Integer); overload;
    procedure Remove(Entry: PEntry); overload;
    procedure Remove(Name: string); overload;
    property Entry[i: Integer]: PEntry read GetEntry;
  end;

  TEntry = record
    Id: Integer;
    Name: string;
    FullPath: string;
    Size: Integer;
    DateTime: TDateTime;
    Attributes: Integer;
    Contents: TListContents;
    PreviousLevel: TListContents;
  end;

  TAdminFicheros = class(TObject)
  private
    List: TListContents;
    PreviousList: TListContents;
    LastID: Integer;
    function GenerateID: Integer;
    procedure AddZeroEntry(L: TListContents; aPreviousLevel: PListContents);
    function AgregarCarpetaRec(Folder: string; aPreviousLevel: PListContents): Boolean;
    procedure CantidadRec(L: TListContents; var N: Integer);
    function EsPadre(Parent: PEntry; Child: PEntry): Boolean;
  public
    CurrentList: TListContents;

    constructor Create;
    destructor Destroy; override;
    procedure Restart;
    function AddFile(aFile: string): Boolean;
    function AddFolder(Folder: string): Boolean;
    function AddNewFolder(Folder: string): Boolean;
    function DeleteFile(aFile: string): Boolean;
    function Find(const id: Integer; LContenido: TListContents): TListContents; overload;
    function Find(id: Integer): TListContents; overload;
    function Move(ElementoOrigen: PEntry; ElementoDestino: PEntry): Boolean;
    function Avanzar(ID: Integer): Boolean; overload;
    function Avanzar(NewList: TListContents): Boolean; overload;
    function Retroceder: Boolean;
    function Count: Integer;
    function Root: TListContents;
    function EsDirectorio(Attributes: Integer): Boolean; overload;
    function EsDirectorio(Entry: PEntry): Boolean; overload;
  end;

implementation

constructor TListContents.Create;
begin
  inherited Create;
end;

function TListContents.GetEntry(i: Integer): PEntry;
begin
  Result := PEntry(Self[i + 1]);
end;

function TListContents.IsPresent(Name: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  Name := LowerCase(Name);

  for i := 0 to Count - 1 do
  begin
    if (Name = LowerCase(Entry[i].Name)) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TListContents.Append(NewEntry: PEntry): Boolean;
begin
  Result := True;
  if IsPresent(NewEntry.Name) then
  begin
    Result := False;
    Exit;
  end;

  Add(NewEntry);
end;

procedure TListContents.Remove(i: Integer);
begin
  Delete(i + 1);
end;

procedure TListContents.Remove(Entry: PEntry);
var
  i: Integer;
begin
  for i := 1 to Self.Count - 1 do
  begin
    if (Entry = PEntry(Items[i])) then
    begin
      Delete(i);
      Break;
    end;
  end;
end;

procedure TListContents.Remove(Name: string);
var
  i: Integer;
begin
  Name := LowerCase(Name);

  for i := 1 to Self.Count - 1 do
  begin
    if (Name = LowerCase(PEntry(Items[i]).Name)) then
    begin
      Delete(i);
      Break;
    end;
  end;
end;

constructor TAdminFicheros.Create;
begin
  List := TListContents.Create;
  AddZeroEntry(List, @CurrentList);
  CurrentList := List;
  PreviousList := nil;
  LastID := 0;
  inherited Create;
end;

destructor TAdminFicheros.Destroy;
begin
  List.Free;

  inherited Destroy;
end;

procedure TAdminFicheros.AddZeroEntry(L: TListContents; aPreviousLevel: PListContents);
var
  NewEntry: PEntry;
begin
  New(NewEntry);
  NewEntry.Id := GenerateID();
  NewEntry.Name := '';
  NewEntry.FullPath := '';
  NewEntry.Size := 0;
  NewEntry.DateTime := Date;
  NewEntry.Attributes := 0;
  NewEntry.Contents := nil;
  NewEntry.PreviousLevel := aPreviousLevel^;
  L.Append(NewEntry);
end;

function TAdminFicheros.GenerateID: Integer;
begin
  Inc(LastID);
  Result := LastID;
end;

procedure TAdminFicheros.Restart;
begin
  Avanzar(List);
end;

function TAdminFicheros.AddFile(aFile: string): Boolean;
var
  NewEntry: PEntry;
  F: TFileStream;
  Age: TDateTime;
begin
  Result := False;
  if FileExists(aFile) then
  begin
    New(NewEntry);
    NewEntry.Id := GenerateID();
    NewEntry.Name := ExtractFileName(aFile);
    NewEntry.FullPath := aFile;
    NewEntry.Attributes := GetFileAttributes(PChar(aFile));
    if FileAge(aFile, {out}Age) then
      NewEntry.DateTime := FileDateToDateTime(Round(Age));
    F := TFileStream.Create(aFile, fmOpenRead);
    NewEntry.Size := F.Size;
    F.Free;
    NewEntry.Contents := nil;
    NewEntry.PreviousLevel := CurrentList;
    CurrentList.Append(NewEntry);
    Result := True;
  end;
end;

{$WARN SYMBOL_PLATFORM OFF}

function TAdminFicheros.AgregarCarpetaRec(Folder: string; aPreviousLevel: PListContents): Boolean;
var
  SR: TSearchRec;
  NewEntry: PEntry;
  C: TListContents;
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
      New(NewEntry);
      NewEntry.Id := GenerateID();
      NewEntry.Name := ExtractFileName(Folder);
      NewEntry.FullPath := Folder + '\';
      NewEntry.Attributes := GetFileAttributes(PChar(Folder + '\'));
      NewEntry.DateTime := 0;
      NewEntry.Size := 0;
      C := TListContents.Create;
      NewEntry.Contents := C;
      NewEntry.PreviousLevel := aPreviousLevel^;

      AddZeroEntry(C, aPreviousLevel);
      aPreviousLevel.Append(NewEntry);
    end
    else
    begin
      C := CurrentList;
    end;

    if (FindFirst(Folder + '\*.*', faArchive or faDirectory or faHidden or faSysFile or faReadOnly, SR) = 0) then
    begin
      repeat
        if (SR.Name[1] = '.') then
          Continue;
          
        New(NewEntry);
        NewEntry.Id := GenerateID();
        NewEntry.Name := SR.Name;
        NewEntry.FullPath := Folder + '\' + SR.Name;
        NewEntry.Attributes := GetFileAttributes(PChar(Folder + '\' + SR.Name));
        NewEntry.DateTime := FileDateToDateTime(SR.Time);
        NewEntry.Size := SR.Size;
        NewEntry.Contents := nil;
        NewEntry.PreviousLevel := aPreviousLevel^;
        if NewEntry.Attributes and FILE_ATTRIBUTE_DIRECTORY = FILE_ATTRIBUTE_DIRECTORY then
          AgregarCarpetaRec(Folder + '\' + SR.Name, @C)
        else
          C.Append(NewEntry);
      until (FindNext(SR) <> 0);
    end;
  end;
end;

{$WARN SYMBOL_PLATFORM ON}


function TAdminFicheros.AddFolder(Folder: string): Boolean;
begin
  if PreviousList = nil then
    AgregarCarpetaRec(Folder, @List)
  else
    AgregarCarpetaRec(Folder, @CurrentList);
  Result := True;
end;

function TAdminFicheros.AddNewFolder(Folder: string): Boolean;
var
  NewEntry: PEntry;
  C: TListContents;
begin
  New(NewEntry);
  NewEntry.Id := GenerateID();
  NewEntry.Name := ExtractFileName(Folder);
  NewEntry.FullPath := Folder;
  NewEntry.Attributes := FILE_ATTRIBUTE_DIRECTORY;
  NewEntry.DateTime := Date;
  NewEntry.Size := 0;
  C := TListContents.Create;
  NewEntry.Contents := C;
  NewEntry.PreviousLevel := CurrentList;
  AddZeroEntry(C, @CurrentList);
  CurrentList.Append(NewEntry);
  Result := True;
end;

function TAdminFicheros.DeleteFile(aFile: string): Boolean;
begin
  CurrentList.Remove(aFile);
  Result := True;
end;

function TAdminFicheros.Find(const id: Integer; LContenido: TListContents): TListContents;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to LContenido.Count - 1 do
  begin
    if Result <> nil then
      Exit;

    if LContenido.Entry[i].Id = id then
    begin
      Result := LContenido;
      Exit;
    end;

    if EsDirectorio(LContenido.Entry[i].Attributes) then
      Result := Find(id, LContenido.Entry[i].Contents);
  end;
end;

function TAdminFicheros.Find(id: Integer): TListContents;
begin
  Result := Find(id, List);
end;

function TAdminFicheros.Move(ElementoOrigen: PEntry; ElementoDestino: PEntry): Boolean;
var
  Origen, Destino: TListContents;
begin
  Result := False;
  if ElementoOrigen = ElementoDestino then
    Exit;

  if (EsDirectorio(ElementoOrigen)) and EsPadre(ElementoOrigen, ElementoDestino) then
    Exit;

  Origen := Find(ElementoOrigen.Id);
  Destino := ElementoDestino.Contents;

  if (Origen = nil) or (Destino = nil) then
    Exit;

  if not Destino.Append(ElementoOrigen) then
    Exit;

  Origen.Remove(ElementoOrigen);

  Result := True;
end;

function TAdminFicheros.Avanzar(ID: Integer): Boolean;
var
  i: Integer;
  Entry: PEntry;
begin
  Result := False;
  for i := 0 to CurrentList.Count - 1 do
  begin
    Entry := CurrentList.Entry[i];
    if Entry.Id = ID then
    begin
      if Entry.Contents <> nil then
      begin
        PreviousList := CurrentList;
        CurrentList := Entry.Contents;
        Result := True;
      end;
      Break;
    end;
  end;
end;

function TAdminFicheros.Avanzar(NewList: TListContents): Boolean;
begin
  PreviousList := NewList.Entry[-1].PreviousLevel;
  CurrentList := NewList;
  Result := True;
end;

function TAdminFicheros.Retroceder: Boolean;
var
  P: TListContents;
begin
  Result := False;
  if PreviousList = nil then
    Exit;

  P := CurrentList.Entry[-1].PreviousLevel;
  PreviousList := P.Entry[-1].PreviousLevel;
  CurrentList := P;

  Result := True;
end;

procedure TAdminFicheros.CantidadRec(L: TListContents; var N: Integer);
var
  i: Integer;
begin
  for i := 0 to L.Count - 1 do
  begin
    N := N + 1;
    if EsDirectorio(L.Entry[i]) then
      CantidadRec(L.Entry[i].Contents, N);
  end;
end;

function TAdminFicheros.Count: Integer;
begin
  Result := 0;
  CantidadRec(Root(), Result);
end;

function TAdminFicheros.Root: TListContents;
begin
  Result := List;
end;

function TAdminFicheros.EsDirectorio(Attributes: Integer): Boolean;
begin
  if Attributes and FILE_ATTRIBUTE_DIRECTORY = FILE_ATTRIBUTE_DIRECTORY then
    Result := True
  else
    Result := False;
end;

function TAdminFicheros.EsDirectorio(Entry: PEntry): Boolean;
begin
  Result := EsDirectorio(Entry.Attributes);
end;

function TAdminFicheros.EsPadre(Parent: PEntry; Child: PEntry): Boolean;
begin
  Result := False;
  if (Parent = nil) or (Child = nil) then
    Exit;

  if Parent.Contents.Entry[-1].Id = Child.Id then
    Result := True
  else
  begin
    if Child.PreviousLevel <> nil then
      Result := EsPadre(Parent, Child.PreviousLevel.Entry[-1]);
  end;
end;

end.
 
