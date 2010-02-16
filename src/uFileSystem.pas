(*
    This file is part of Dxbx - a XBox emulator written in Delphi (ported over from cxbx)
    Copyright (C) 2010 PatrickvL and other members of the development team.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)
unit uFileSystem;

interface

uses
  // Delphi
  Windows,
  Classes, // TMemoryStream
  // Dxbx
  uTypes;

type
  TFileSystem = class(TObject)
  protected
    FLastErrorString: string;
    FSelectedFile: string;
    function Error(const aMessage: string): Boolean;
  public
    property LastErrorString: string read FLastErrorString;
    property SelectedFile: string read FSelectedFile;
    function Select(const aFilePath: string): Boolean;
    function FileExists(const aFilePath: string): Boolean; virtual;
    function Load(const aStream: TMemoryStream): Boolean; virtual;
  end;

  PLogicalVolume = ^RLogicalVolume;
  RLogicalVolume = record
  private
    MyFileSystem: TFileSystem;
  public
    property FileSystem: TFileSystem read MyFileSystem;
    function IsMounted: Boolean;
    function Mount(aDevice: string): Boolean;
    procedure Unmount;
  end;

  RDrives = record
  private
    MyC: PLogicalVolume;
    MyD: PLogicalVolume;
    MyE: PLogicalVolume;
    MyT: PLogicalVolume;
    MyU: PLogicalVolume;
    MyX: PLogicalVolume;
    MyY: PLogicalVolume;
    MyZ: PLogicalVolume;
  public
    procedure Create;
    procedure Free;

    function GetVolume(const aLetter: Char): PLogicalVolume;
    function Select(const aPath: string): Boolean;

    // Xbox1 can have these partitions :
    property C: PLogicalVolume read MyC;    // C: for system files
    property D: PLogicalVolume read MyD;    // D: Optical drive. Mount game ISO/Folder here
    property E: PLogicalVolume read MyE;    // E: Data drive. Stores savegames, audio rips, etc.
    property T: PLogicalVolume read MyT;    // T: ??
    property U: PLogicalVolume read MyU;    // U: ??
    property X: PLogicalVolume read MyX;    // X, Y, Z: Cache drives. (Map Y: to D: too)
    property Y: PLogicalVolume read MyY;
    property Z: PLogicalVolume read MyZ;
  end;

var
  Drives: RDrives;

implementation

uses
  // Delphi
  SysUtils,
  // Dxbx
  uXDVDFS;

{ TFileSystem }

function TFileSystem.Error(const aMessage: string): Boolean;
begin
  FLastErrorString := aMessage;
  Result := aMessage <> '';
  if Result then
    raise Exception.Create(aMessage);
end;

function TFileSystem.FileExists(const aFilePath: string): Boolean;
begin
  Result := False;
end;

function TFileSystem.Select(const aFilePath: string): Boolean;
begin
  Result := (aFilePath <> '') and FileExists(aFilePath);
  if Result then
    FSelectedFile := aFilePath
  else
    FSelectedFile := '';
end;

function TFileSystem.Load(const aStream: TMemoryStream): Boolean;
begin
  Result := False;
end;

{ TFolderFS }

type
  TFolderFS = class(TFileSystem)
  public
    RootFolder: string;
    constructor Create(const aRootFolder, aSelectedFile: string);
    destructor Destroy; override;

    function FileExists(const aFilePath: string): Boolean; override;
    function Load(const aStream: TMemoryStream): Boolean; override;
  end;

constructor TFolderFS.Create(const aRootFolder, aSelectedFile: string);
begin
  inherited Create;
  RootFolder := aRootFolder;
  Select(aSelectedFile);
end;

destructor TFolderFS.Destroy;
begin
  inherited Destroy;
end;

function TFolderFS.FileExists(const aFilePath: string): Boolean; // override
begin
  Result := SysUtils.FileExists(RootFolder + '\' + aFilePath);
end;

function TFolderFS.Load(const aStream: TMemoryStream): Boolean;
begin
  aStream.LoadFromFile(RootFolder + '\' + SelectedFile);
  Result := aStream.Size > 0;
end;

{ TXDVDFS }

type
  TXDVDFS = class(TFileSystem)
  protected
    MyContainer: TFileStream;
    MySession: PXDVDFS_SESSION;
    MySearchRecord: SEARCH_RECORD;
    MyFileRecord: FILE_RECORD;
  public
    constructor Create(const aContainer, aSelectedFile: string);
    destructor Destroy; override;
    function FileExists(const aFilePath: string): Boolean; override;
    function Load(const aStream: TMemoryStream): Boolean; override;
  end;

function TXDVDFS_ReadSectorsFunc(
      Data: PVOID;        //  Pointer to arbitrary data
      Buffer: PVOID;      //  Buffer to fill
      StartSector: DWord; //  Start sector
      ReadSize: DWORD     //  Number of sectors to read
): BOOL;
var
  fs: TFileStream;
begin
  fs := TFileStream(Data);
  fs.Position := Int64(StartSector) * SECTOR_SIZE;
  Result := fs.Read(Buffer^, ReadSize * SECTOR_SIZE) > 0;
end;

constructor TXDVDFS.Create(const aContainer, aSelectedFile: string);
begin
  inherited Create;
  MyContainer := TFileStream.Create(aContainer, fmOpenRead or fmShareDenyWrite);
  New(MySession);
  ZeroMemory(MySession, SizeOf(MySession^));
  if not XDVDFS_Mount(MySession, @TXDVDFS_ReadSectorsFunc, MyContainer) then
;//    Error('Couldn''t mount image!');
  Select(aSelectedFile);
end;

destructor TXDVDFS.Destroy;
begin
  XDVDFS_UnMount(MySession);
  Dispose(MySession); MySession := nil;
  FreeAndNil(MyContainer);
  inherited Destroy;
end;

function TXDVDFS.FileExists(const aFilePath: string): Boolean;
begin
  Result := XDVDFS_GetFileInfo(MySession, PAnsiChar(AnsiString(aFilePath)), @MySearchRecord) = XDVDFS_NO_ERROR;
end;

function TXDVDFS.Load(const aStream: TMemoryStream): Boolean;
begin
  Result := XDVDFS_OpenFileEx(MySession, @MySearchRecord, @MyFileRecord) = XDVDFS_NO_ERROR;
  if not Result then
    Exit;

  aStream.Size := MyFileRecord.FileSize;
  Result := XDVDFS_FileRead(MySession, @MyFileRecord, aStream.Memory, aStream.Size) = MyFileRecord.FileSize;
  if not Result then
    aStream.Size := 0;
end;

{ RLogicalVolume }

function RLogicalVolume.IsMounted: Boolean;
begin
  Result := Assigned(MyFileSystem);
end;

function RLogicalVolume.Mount(aDevice: string): Boolean;
var
  ExtStr: string;
  Parent: string;
begin
  Unmount;

  aDevice := ExpandFileName(aDevice);
  Parent := ExtractFilePath(aDevice);
  if FileExists(aDevice) then
  begin
    // Existing file, what format is it?
    ExtStr := LowerCase(ExtractFileExt(aDevice));
    if ExtStr = '.xbe' then
      MyFileSystem := TFolderFS.Create(Parent, ExtractFileName(aDevice))
    else
    if ExtStr = '.iso' then
      MyFileSystem := TXDVDFS.Create(aDevice, '');
  end
  else
  // No existing file, is it a container perhaps?
  if FileExists(Parent) then
  begin
    // Existing container, what format is it?
    ExtStr := LowerCase(ExtractFileExt(Parent));
    if ExtStr = '.iso' then
      MyFileSystem := TXDVDFS.Create(Parent, ExtractFileName(aDevice));
  end;

  Result := Assigned(MyFileSystem);
  if (Result = False)
  or (MyFileSystem.SelectedFile <> '') then
    Exit;

  // Select default.xbe (or any other xbe if not present) :
  if not MyFileSystem.Select('default.xbe') then
    MyFileSystem.Select('*.xbe'); // TODO : Support wildcards in Select method
end;

procedure RLogicalVolume.Unmount;
begin
  FreeAndNil(MyFileSystem);
end;

{ RDrives }

procedure RDrives.Create;
begin
  New(MyC);
  New(MyD);
  New(MyE);
  New(MyT);
  New(MyU);
  New(MyX);
  New(MyY);
  New(MyZ);
end;

procedure RDrives.Free;
begin
  Dispose(MyC);
  Dispose(MyD);
  Dispose(MyE);
  Dispose(MyT);
  Dispose(MyU);
  Dispose(MyX);
  Dispose(MyY);
  Dispose(MyZ);
end;

function RDrives.GetVolume(const aLetter: Char): PLogicalVolume;
begin
  case aLetter of
    'c', 'C': Result := C;
    'd', 'D': Result := D;
    'e', 'E': Result := E;
    't', 'T': Result := T;
    'u', 'U': Result := U;
    'x', 'X': Result := X;
    'y', 'Y': Result := Y;
    'z', 'Z': Result := Z;
  else
    Result := nil;
  end;
end;

function RDrives.Select(const aPath: string): Boolean;
var
  PathPtr: PChar;
  Drive: PLogicalVolume;
begin
  Result := False;
  PathPtr := PChar(aPath);
  if Copy(aPath, 1, 4) = '\??\' then
    Inc(PathPtr, 4);

  if PathPtr[1] <> ':' then
    Exit;

  Drive := GetVolume(PathPtr[0]);
  if Assigned(Drive) and Drive.IsMounted then
    Result := Drive.FileSystem.Select(PathPtr+2);
end;

initialization

  Drives.Create;

finalization

  Drives.Free;

end.

