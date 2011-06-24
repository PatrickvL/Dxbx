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
  TFileHandle = class(TObject)
  end;

  TDirHandle = class(TFileHandle)
  end;

  TSearchInfo = class(TObject)
  protected
    function GetAttributes: Integer; virtual; abstract;
    function GetFilename: string; virtual; abstract;
    function GetFileSize: Int64; virtual; abstract;
  public
    property Filename: string read GetFilename;
    property Attributes: Integer read GetAttributes;
    property FileSize: Int64 read GetFileSize;
  end;

  // TFileSystem is the base-class for all file systems,
  // introducing most of the shared functionality needed
  // to access folders and files in the file system.
  // Because most code in this is abstract, we instantiate
  // a concrete TFileSystem sub-class when actually mounting
  // a volume (see RLogicalVolume.Mount).
  TFileSystem = class(TObject)
  protected
    FMountPoint: string;
    MyRoot: TDirHandle;
  public
    function TypeStr: string; virtual; abstract;
    property MountPoint: string read FMountPoint;
    property Root: TDirHandle read MyRoot;
  // TODO : Officially, everything below should be implemented thread-safe via a session!
  protected
    FLastErrorString: string;
    FCurrentDir: TDirHandle;
    function Error(const aMessage: string): Boolean;
  public
    property LastErrorString: string read FLastErrorString;
    property CurrentDir: TDirHandle read FCurrentDir;
    function ChangeDir(const aPath: string): Boolean; virtual;
    function FileExists(const aFilePath: string): Boolean; virtual;

    // TODO : It's probably wise to keep a doubly linked list of all open files
    // so we can quickly remove entries, and cleanup once the filesystem
    // is closed while still having open handles!

    function Open(const aFilePath: string): TFileHandle; virtual; abstract;
    function Seek(const aFileHandle: TFileHandle; const Offset: Int64; const Origin: Integer): Int64; virtual; abstract;
    function Read(const aFileHandle: TFileHandle; var Buffer; const Size: Int64): Int64; virtual; abstract;
    function Close(const aFileHandle: TFileHandle): Boolean; virtual; abstract;

    function FindFirst(const aFilePath: string = '\*'): TSearchInfo; virtual; abstract;
    function FindNext(SearchInfo: TSearchInfo): Boolean; virtual; abstract;
    procedure FindClose(SearchInfo: TSearchInfo); virtual; abstract;

    // TODO : Add functions get and set current directory, get (and set?) folder and file attributes.
  end;

  // Logical volume acts as a layer of seperation between the drive letter
  // and the type of filesystem currently active behind this letter.
  // The only real functionality in this type, is instantiating the correct
  // TFileSystem sub-class when mounting a volume (currently based on extension).
  PLogicalVolume = ^RLogicalVolume;
  RLogicalVolume = record
  private
    FLetter: Char;
    MyFileSystem: TFileSystem;
  public
    property Letter: Char read FLetter;
    property FileSystem: TFileSystem read MyFileSystem;

    procedure Create;

    function IsMounted: Boolean;
    procedure Unmount;
    function Mount(aDevice: string): Boolean;

    function OpenImage(const aFileName: string; out aRelativeXBEFilePath: string): Boolean;
  end;

  // Drives is the top-level in our emulated filesystem,
  // offering an shared entry-point for all Xbox1 volumes
  // and some minimal functionality.
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

function AttributesToString(Attr: Integer): string;

implementation

uses
  // Delphi
  SysUtils,
  // Dxbx
  uXDVDFS;

function AttributesToString(Attr: Integer): string;
begin
  if Attr = Integer(INVALID_FILE_ATTRIBUTES) then
  begin
    Result := 'Invalid';
    Exit;
  end;

  Result := '';
  if Attr and FILE_ATTRIBUTE_READONLY             > 0 then Result := Result + ', ReadOnly';
  if Attr and FILE_ATTRIBUTE_HIDDEN               > 0 then Result := Result + ', Hidden';
  if Attr and FILE_ATTRIBUTE_SYSTEM               > 0 then Result := Result + ', System';
  if Attr and FILE_ATTRIBUTE_DIRECTORY            > 0 then Result := Result + ', Directory';
  if Attr and FILE_ATTRIBUTE_ARCHIVE              > 0 then Result := Result + ', Archive';
  if Attr and FILE_ATTRIBUTE_DEVICE               > 0 then Result := Result + ', Device';
  if Attr and FILE_ATTRIBUTE_NORMAL               > 0 then Result := Result + ', Normal';
  if Attr and FILE_ATTRIBUTE_TEMPORARY            > 0 then Result := Result + ', Temporary';
  if Attr and FILE_ATTRIBUTE_SPARSE_FILE          > 0 then Result := Result + ', Sparse file';
  if Attr and FILE_ATTRIBUTE_REPARSE_POINT        > 0 then Result := Result + ', Reparse point';
  if Attr and FILE_ATTRIBUTE_COMPRESSED           > 0 then Result := Result + ', Compressed';
  if Attr and FILE_ATTRIBUTE_OFFLINE              > 0 then Result := Result + ', Offline';
  if Attr and FILE_ATTRIBUTE_NOT_CONTENT_INDEXED  > 0 then Result := Result + ', Not content indexed';
  if Attr and FILE_ATTRIBUTE_ENCRYPTED            > 0 then Result := Result + ', Encrypted';
{$IF DECLARED (FILE_ATTRIBUTE_VIRTUAL)}
  if Attr and FILE_ATTRIBUTE_VIRTUAL              > 0 then Result := Result + ', Virtual';
{$IFEND}
  Delete(Result, 1, 2);
end;

{ TFileSystem }

function TFileSystem.Error(const aMessage: string): Boolean;
begin
  FLastErrorString := aMessage;
  Result := aMessage <> '';
  if Result then
    raise Exception.Create(aMessage);
end;

function TFileSystem.FileExists(const aFilePath: string): Boolean;
var
  FileHandle: TFileHandle;
begin
  FileHandle := Open(aFilePath);
  Result := Assigned(FileHandle);
  if Result then
    Close(FileHandle);
end;

function TFileSystem.ChangeDir(const aPath: string): Boolean;
var
  PathPtr: PChar;
begin
  Result := True;
  if aPath = '' then
    Exit; // No dir given means: stay here

  PathPtr := PChar(aPath);
  case PathPtr^ of
    '.':
      // start at CurrentDir
      if (aPath = '.') or (aPath = '.\') then
        ;
    '\':
      if PathPtr[1] = '\' then
        // TODO : Handle UNC paths
      else
        // Start at Root
      ;
    'a'..'z',
    'A'..'Z':
      // Path starts with a letter, check if it's a volume indicator :
      if (PathPtr[1] = ':') then
      begin
        Result := Drives.GetVolume(PathPtr^).IsMounted;
        if Result then
          Result := Drives.GetVolume(PathPtr^).FileSystem.ChangeDir(string(PathPtr+2));

        Exit;
      end;
  end;
  // TODO : Finish this implementation
end;

/// Mapped-folder support : --------------------------------------------------

type
  TMappedFileHandle = class(TFileHandle)
  protected
    Handle: THandle;
  public
    destructor Destroy; override;
  end;

  TMappedSearchInfo = class(TSearchInfo)
  protected
    SearchRec: TSearchRec;
    function GetAttributes: Integer; override;
    function GetFilename: string; override;
    function GetFileSize: Int64; override;
  public
    destructor Destroy; override;
  end;

  TMappedFolderFileSystem = class(TFileSystem)
  public
    constructor Create(const aRootFolder: string);
    destructor Destroy; override;

    function TypeStr: string; override;

    function Open(const aFilePath: string): TFileHandle; override;
    function Seek(const aFileHandle: TFileHandle; const Offset: Int64; const Origin: Integer): Int64; override;
    function Read(const aFileHandle: TFileHandle; var Buffer; const Size: Int64): Int64; override;
    function Close(const aFileHandle: TFileHandle): Boolean; override;

    function FindFirst(const aFilePath: string = '\*'): TSearchInfo; override;
    function FindNext(SearchInfo: TSearchInfo): Boolean; override;
    procedure FindClose(SearchInfo: TSearchInfo); override;
  end;

{ TMappedFolderFileSystem }

constructor TMappedFolderFileSystem.Create(const aRootFolder: string);
begin
  inherited Create;
  FMountPoint := aRootFolder;
end;

destructor TMappedFolderFileSystem.Destroy;
begin
  inherited Destroy;
end;

function TMappedFolderFileSystem.TypeStr: string;
begin
  Result := 'Virtual folder';
end;

function TMappedFolderFileSystem.Open(const aFilePath: string): TFileHandle;
var
  f: THandle;
begin
  Result := nil;
  f := THandle(FileOpen(MountPoint + '\' + aFilePath, fmOpenRead));
  if f = THandle(-1) then
  begin
    // Handle GetLastError
    Exit;
  end;

  Result := TMappedFileHandle.Create;
  TMappedFileHandle(Result).Handle := f;
end;

function TMappedFolderFileSystem.Seek(const aFileHandle: TFileHandle; const Offset: Int64; const Origin: Integer): Int64;
begin
  Result := FileSeek(TMappedFileHandle(aFileHandle).Handle, Offset, Origin);
end;

function TMappedFolderFileSystem.Read(const aFileHandle: TFileHandle; var Buffer; const Size: Int64): Int64;
begin
  Result := FileRead(TMappedFileHandle(aFileHandle).Handle, Buffer, Size);
end;

function TMappedFolderFileSystem.Close(const aFileHandle: TFileHandle): Boolean;
begin
  Result := Assigned(aFileHandle);
  if Result then
    aFileHandle.Free;
end;

function TMappedFolderFileSystem.FindFirst(const aFilePath: string = '\*'): TSearchInfo;
begin
  Result := TMappedSearchInfo.Create;
  if SysUtils.FindFirst(MountPoint + '\' + aFilePath, faAnyFile, TMappedSearchInfo(Result).SearchRec) <> 0 then
  begin
    FreeAndNil(Result);
    Exit;
  end;

  // Skip '.' and '..' directory entries :
  if (TMappedSearchInfo(Result).SearchRec.Name = '.')
  or (TMappedSearchInfo(Result).SearchRec.Name = '..') then
    if not FindNext(Result) then
      FreeAndNil(Result);
end;

function TMappedFolderFileSystem.FindNext(SearchInfo: TSearchInfo): Boolean;
begin
  Result := SysUtils.FindNext(TMappedSearchInfo(SearchInfo).SearchRec) = 0;
  if not Result then
    Exit;

  // Skip '.' and '..' directory entries :
  if (TMappedSearchInfo(SearchInfo).SearchRec.Name = '.')
  or (TMappedSearchInfo(SearchInfo).SearchRec.Name = '..') then
    Result := FindNext(SearchInfo)
end;

procedure TMappedFolderFileSystem.FindClose(SearchInfo: TSearchInfo);
begin
  if Assigned(SearchInfo) then
    SearchInfo.Free;
end;

{ TMappedFileHandle }

destructor TMappedFileHandle.Destroy;
begin
  FileClose(Handle);

  inherited Destroy;
end;

{ TMappedSearchInfo }

destructor TMappedSearchInfo.Destroy;
begin
  SysUtils.FindClose(SearchRec);

  inherited Destroy;
end;

function TMappedSearchInfo.GetAttributes: Integer;
begin
  Result := SearchRec.Attr;
end;

function TMappedSearchInfo.GetFilename: string;
begin
  Result := SearchRec.Name;
end;

function TMappedSearchInfo.GetFileSize: Int64;
begin
  Result := SearchRec.Size;
end;

/// XDVDFS support : ---------------------------------------------------------

type
  TXDVDFSFileHandle = class(TFileHandle)
    MyFileRecord: FILE_RECORD;
  end;

  TXDVDFSSearchInfo = class(TSearchInfo)
  protected
    MySearchRecord: SEARCH_RECORD;
    Mask: string;
    function GetAttributes: Integer; override;
    function GetFilename: string; override;
    function GetFileSize: Int64; override;
  end;

  TXDVDFileSystem = class(TFileSystem)
  protected
    MyContainer: THandle;
    MySession: PXDVDFS_SESSION;
  public
    constructor Create(const aContainer: string);
    destructor Destroy; override;
    function TypeStr: string; override;

    function Open(const aFilePath: string): TFileHandle; override;
    function Seek(const aFileHandle: TFileHandle; const Offset: Int64; const Origin: Integer): Int64; override;
    function Read(const aFileHandle: TFileHandle; var Buffer; const Size: Int64): Int64; override;
    function Close(const aFileHandle: TFileHandle): Boolean; override;

    function FindFirst(const aFilePath: string = '\*'): TSearchInfo; override;
    function FindNext(SearchInfo: TSearchInfo): Boolean; override;
    procedure FindClose(SearchInfo: TSearchInfo); override;
  end;

function TXDVDFS_ReadSectorsFunc(
      Data: PVOID;        //  Pointer to arbitrary data
      Buffer: PVOID;      //  Buffer to fill
      StartSector: DWord; //  Start sector
      ReadSize: DWORD     //  Number of sectors to read
): LongBool;
begin
  FileSeek(THandle(Data), Int64(StartSector) * SECTOR_SIZE, 0);
  Result := FileRead(THandle(Data), Buffer^, ReadSize * SECTOR_SIZE) > 0;
end;

constructor TXDVDFileSystem.Create(const aContainer: string);
begin
  inherited Create;
  FMountPoint := aContainer;
  MyContainer := THandle(FileOpen(aContainer, fmOpenRead or fmShareDenyWrite));
  New(MySession);
  ZeroMemory(MySession, SizeOf(MySession^));
  if not XDVDFS_Mount(MySession, @TXDVDFS_ReadSectorsFunc, Pointer(MyContainer)) then
    Error('Couldn''t mount image!');
end;

destructor TXDVDFileSystem.Destroy;
begin
  XDVDFS_UnMount(MySession);
  Dispose(MySession); MySession := nil;
  FileClose(MyContainer);
  inherited Destroy;
end;

function TXDVDFileSystem.TypeStr: string;
begin
  Result := 'XDVDFS';
end;

function TXDVDFileSystem.Open(const aFilePath: string): TFileHandle;
var
  Session: PXDVDFS_SESSION;
  FileRecord: PFILE_RECORD;
begin
  Result := TXDVDFSFileHandle.Create;

  Session := MySession;
  FileRecord := @(TXDVDFSFileHandle(Result).MyFileRecord);

  if XDVDFS_OpenFile(Session, PAnsiChar(AnsiString(aFilePath)), FileRecord) = XDVDFS_NO_ERROR then
    Exit;

  FreeAndNil(Result);
end;

function TXDVDFileSystem.Seek(const aFileHandle: TFileHandle; const Offset: Int64; const Origin: Integer): Int64;
var
  Session: PXDVDFS_SESSION;
  FileRecord: PFILE_RECORD;
begin
  Assert(aFileHandle is TXDVDFSFileHandle);

  Session := MySession;
  FileRecord := @(TXDVDFSFileHandle(aFileHandle).MyFileRecord);

  if XDVDFS_FileSeek(Session, FileRecord, {Delta=}Offset, {SeekMode=}Origin) = XDVDFS_NO_ERROR then
    Result := FileRecord.CurrentPosition
  else
    Result := -1; // TODO : Handle error
end;

function TXDVDFileSystem.Read(const aFileHandle: TFileHandle; var Buffer; const Size: Int64): Int64;
var
  Session: PXDVDFS_SESSION;
  FileRecord: PFILE_RECORD;
begin
  Assert(aFileHandle is TXDVDFSFileHandle);

  Session := MySession;
  FileRecord := @(TXDVDFSFileHandle(aFileHandle).MyFileRecord);

  Result := XDVDFS_FileRead(Session, FileRecord, PVoid(@Buffer), Size);
end;

function TXDVDFileSystem.Close(const aFileHandle: TFileHandle): Boolean;
var
  Session: PXDVDFS_SESSION;
  FileRecord: PFILE_RECORD;
begin
  Assert(aFileHandle is TXDVDFSFileHandle);

  Session := MySession;
  FileRecord := @(TXDVDFSFileHandle(aFileHandle).MyFileRecord);

  Result := XDVDFS_FileClose(Session, FileRecord) = XDVDFS_NO_ERROR;
  aFileHandle.Free;
end;

function TXDVDFileSystem.FindFirst(const aFilePath: string = '\*'): TSearchInfo;
var
  Session: PXDVDFS_SESSION;
  FilePath: AnsiString;
begin
  Session := MySession;

  Result := TXDVDFSSearchInfo.Create;
  if Pos('*', aFilePath) > 0 then
  begin
    FilePath := AnsiString(ExtractFilePath(aFilePath));
    TXDVDFSSearchInfo(Result).Mask := ExtractFileName(aFilePath);
  end
  else
    FilePath := AnsiString(aFilePath);

  if XDVDFS_GetFileInfo(Session, PAnsiChar(FilePath), @(TXDVDFSSearchInfo(Result).MySearchRecord)) = XDVDFS_NO_ERROR then
  begin
    // Skip root dir :
    if TXDVDFSSearchInfo(Result).MySearchRecord.Position = 0 then
    begin
      if not FindNext(Result) then
      begin
        FreeAndNil(Result);
        Exit;
      end;
    end;

    // TODO : Check mask
    Exit;
  end;

  FreeAndNil(Result);
end;

function TXDVDFileSystem.FindNext(SearchInfo: TSearchInfo): Boolean;
var
  Session: PXDVDFS_SESSION;
begin
  Session := MySession;

  Result := XDVDFS_EnumFiles(Session, @(TXDVDFSSearchInfo(SearchInfo).MySearchRecord)) = XDVDFS_NO_ERROR;
  // TODO : Check mask
end;

procedure TXDVDFileSystem.FindClose(SearchInfo: TSearchInfo);
begin
  SearchInfo.Free;
end;

{ RLogicalVolume }

procedure RLogicalVolume.Create;
begin
  ZeroMemory(@Self, SizeOf(Self));
end;

function RLogicalVolume.IsMounted: Boolean;
begin
  Result := Assigned(MyFileSystem);
end;

procedure RLogicalVolume.Unmount;
begin
  FreeAndNil(MyFileSystem);
end;

function RLogicalVolume.Mount(aDevice: string): Boolean;
var
  DeviceAttr: Integer;
  ExtStr: string;
begin
  Unmount;

  // Determine full path and attributes for the supplied image :
  aDevice := ExpandFileName(aDevice);
  DeviceAttr := Integer(GetFileAttributes(PChar(aDevice)));

  // Break out when device path doesn't exist :
  if DeviceAttr = -1 then
  begin
    Result := False;
    Exit;
  end;

  if (DeviceAttr and FILE_ATTRIBUTE_DIRECTORY) > 0 then
  begin
    // Opening a folder - create a mapping for it :
    MyFileSystem := TMappedFolderFileSystem.Create(aDevice);
  end
  else
  if (DeviceAttr and (FILE_ATTRIBUTE_ARCHIVE or FILE_ATTRIBUTE_NORMAL)) > 0 then
  begin
    // Opening a file, what format is it?
    ExtStr := LowerCase(ExtractFileExt(aDevice));
    if ExtStr = '.iso' then
    begin
      // TODO : Check validity of ISO
      MyFileSystem := TXDVDFileSystem.Create(aDevice);
    end;
    // TODO : Add more archive formats here, like .bin/.cue/.zip perhaps?
  end;

  // Indicate succes/failure :
  Result := Assigned(MyFileSystem);
end;

function RLogicalVolume.OpenImage(const aFileName: string; out aRelativeXBEFilePath: string): Boolean;

  function _FindXbe(const aXbeFileName: string): Boolean;
  var
    SearchInfo: TSearchInfo;
  begin
    SearchInfo := MyFileSystem.FindFirst(aXbeFileName);
    Result := Assigned(SearchInfo);
    if Result then
      MyFileSystem.FindClose(SearchInfo);
  end;

  function _FindSomeXbe(out aXbeFileName: string): Boolean;
  var
    SearchInfo: TSearchInfo;
  begin
    SearchInfo := MyFileSystem.FindFirst('*.xbe');
    Result := Assigned(SearchInfo);
    if Result then
    begin
      {out}aXbeFileName := SearchInfo.Filename;
      MyFileSystem.FindClose(SearchInfo);
    end;
  end;

var
  Folder: string;
begin
  Unmount;

  // Split given filename in folder & filename :
  Folder := ExpandFileName(aFileName);
  aRelativeXBEFilePath := '';

  // Walk path until we can mount the volume :
  while Folder <> '' do
  begin
    if Mount(Folder) then
      Break;

    if aRelativeXBEFilePath <> '' then
      aRelativeXBEFilePath := '\' + aRelativeXBEFilePath;

    aRelativeXBEFilePath := ExtractFileName(Folder) + aRelativeXBEFilePath;
    Folder := ExtractFilePath(Folder);
  end;

  if not IsMounted then
  begin
    Result := False;
    Exit;
  end;

  if (aRelativeXBEFilePath <> '') and _FindXbe(aRelativeXBEFilePath) then
  begin
    Result := True;
    Exit;
  end;

  if _FindXbe('default.xbe') then
  begin
    aRelativeXBEFilePath := 'default.xbe';
    Result := True;
    Exit;
  end;

  // Search for any xbe when the given and 'default.xbe' are not available :
  Result := _FindSomeXbe({var}aRelativeXBEFilePath);
end;

{ RDrives }

procedure RDrives.Create;
begin
  ZeroMemory(@Self, SizeOf(Self));

  New(MyC); MyC.Create; MyC^.FLetter := 'C';
  New(MyD); MyD.Create; MyD^.FLetter := 'D';
  New(MyE); MyE.Create; MyE^.FLetter := 'E';
  New(MyT); MyT.Create; MyT^.FLetter := 'T';
  New(MyU); MyU.Create; MyU^.FLetter := 'U';
  New(MyX); MyX.Create; MyX^.FLetter := 'X';
  New(MyY); MyY.Create; MyY^.FLetter := 'Y';
  New(MyZ); MyZ.Create; MyZ^.FLetter := 'Z';
end;

procedure RDrives.Free;
begin
  MyC.Unmount; Dispose(MyC);
  MyD.Unmount; Dispose(MyD);
  MyE.Unmount; Dispose(MyE);
  MyT.Unmount; Dispose(MyT);
  MyU.Unmount; Dispose(MyU);
  MyX.Unmount; Dispose(MyX);
  MyY.Unmount; Dispose(MyY);
  MyZ.Unmount; Dispose(MyZ);
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

{ TXDVDFSSearchInfo }

function TXDVDFSSearchInfo.GetAttributes: Integer;
begin
  Result := MySearchRecord.Attributes;
end;

function TXDVDFSSearchInfo.GetFilename: string;
begin
  Result := string(AnsiString(MySearchRecord.Filename));
end;

function TXDVDFSSearchInfo.GetFileSize: Int64;
begin
  Result := MySearchRecord.FileSize;
end;

initialization

  Drives.Create;

finalization

  Drives.Free;

end.

