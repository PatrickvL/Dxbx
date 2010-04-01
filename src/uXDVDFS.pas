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
unit uXDVDFS;

(*
  This unit is translated from "XDVDFS Tools" by [SNK] / Supremacy.

  There also exists an extende version of this by CloneXB,
  but I (PatrickvL) haven't found any sources for that.

  After translation, some thread-safety modifications where made.
*)

interface

uses
  // Delphi
  Windows,
  // Dxbx
  uTypes;

//-- Defines ------------------------------------------------------------------

// Determines how many sectors are buffered in each instance of CDIO_READ
const DISK_BUFFER = 64;
const SECTOR_SIZE = 2048;

type
  // Routine to get sectors
  TReadFunc = function(
      Data: PVOID;        //  Pointer to arbitrary data
      Buffer: PVOID;      //  Buffer to fill
      StartSector: DWord; //  Start sector
      ReadSize: DWORD     //  Number of sectors to read
      ): LongBool;

type CDIO_READ = packed record
  SectorList: array [0..DISK_BUFFER-1] of DWORD;    // Ring buffer for buffered disk i/o
  LockList: array [0..DISK_BUFFER-1] of DWORD;      // Lock for each buffered sector
  DiskBuffer: array [0..(SECTOR_SIZE * DISK_BUFFER)-1] of BYTE;  // Storage room for buffered sectors
  WriteIndex: DWORD;            // Write pointer
  // Pointer to arbitrary data passed at init
  // (usually a file or device handle)
  Data: PVOID;
  Sectors: TReadFunc;
end; PCDIO_READ = ^CDIO_READ;

// Get a sector from buffer and lock it
function GetSectorBuffered(
        This: PCDIO_READ;
        SectorNumber: DWORD): PBYTE;

// Release a locked buffer
procedure ReleaseBufferedSector(
        This: PCDIO_READ;
        SectorNumber: DWORD);

// Attributes
const XDVDFS_ATTRIBUTE_READONLY = $01;
const XDVDFS_ATTRIBUTE_HIDDEN = $02;
const XDVDFS_ATTRIBUTE_SYSTEM = $04;
const XDVDFS_ATTRIBUTE_DIRECTORY = $10;
const XDVDFS_ATTRIBUTE_ARCHIVE = $20;
const XDVDFS_ATTRIBUTE_NORMAL = $80;

// Error Codes
const XDVDFS_NO_ERROR = 0;
const XDVDFS_EXPIRED_SESSION = 1;
const XDVDFS_NO_MORE_FILES = 2;
const XDVDFS_DISK_ERROR = 3;
const XDVDFS_FILE_NOT_FOUND = 4;
const XDVDFS_INVALID_PARAMETER = 5;

// Seek Modes
const SM_BEGIN = 0;
const SM_CURRENT = 1;
const SM_END = 2;

//-- Type Definitions ---------------------------------------------------------

// XDVDFS Volume descriptor
type XDVDFS_VOLUME_DESCRIPTOR = packed record
  Signature1: array [0..20-1] of BYTE;
  RootDirectory: DWORD;
  RootDirectorySize: DWORD;
  ImageCreationTime: FILETIME;
  Unused: array [0..1992-1] of BYTE;
  Signature2: array [0..20-1] of BYTE;
end; PXDVDFS_VOLUME_DESCRIPTOR = ^XDVDFS_VOLUME_DESCRIPTOR;

// File Record
type FILE_RECORD = packed record
  Magic: DWORD;
  PartialData: array [0..SECTOR_SIZE-1] of BYTE;
  PartialSector: DWORD;
  StartSector: DWORD;
  FileSize: DWORD;
  CurrentPosition: DWORD;
end; PFILE_RECORD = ^FILE_RECORD;

// Search Record
type SEARCH_RECORD = packed record
  Magic: DWORD;
  StartSector: DWORD;
  DirectorySize: DWORD;
  Position: DWORD;
  Filename: array [0..256-1] of AnsiChar;
  Attributes: DWORD;
  FileSize: DWORD;
  FileStartSector: DWORD;
  FileEndSector: DWORD;
end; PSEARCH_RECORD = ^SEARCH_RECORD;

function UPPERCASE(a: AnsiChar): AnsiChar;
function MIN(a,b: DWORD): DWORD;

const DIRECTORY_SEPARATOR = '\';
const TRANSFER_SIZE = 32; // Read granularity (in sectors) = 64 Kb

function ENDIAN_SAFE32(a: DWORD): DWORD;

CONST XDVDFS_Signature: PAnsiChar = 'MICROSOFT*XBOX*MEDIA';

type XDVDFS_DIRECTORY_ENTRY = packed record
  LeftSubTree: WORD;
  RightSubTree: WORD;
  StartSector: DWORD;
  FileSize: DWORD;
  FileAttributes: BYTE;
  FilenameLength: BYTE;
  Filename: array [0..0] of BYTE;
end; PXDVDFS_DIRECTORY_ENTRY = ^XDVDFS_DIRECTORY_ENTRY;

// XDVDFS session
type XDVDFS_SESSION = packed record
  // Start sector of current session
  StartSector: DWORD;
  // Volume Descriptor of the current session
  Root: XDVDFS_VOLUME_DESCRIPTOR;
  // Our little interface for reading sectors
  Read: CDIO_READ;
  // The dword below is incremented when the filesystem is unmounted
  // automatically invalidating all open files and search records
  Magic: DWORD;
end; PXDVDFS_SESSION = ^XDVDFS_SESSION;

function XDVDFS_Mount(
   Session: PXDVDFS_SESSION;
   ReadFunc: TReadFunc;
   Data: PVOID): LongBool;
function XDVDFS_UnMount(
   Session: PXDVDFS_SESSION): LongBool;
function XDVDFS_GetRootDir(
   Session: PXDVDFS_SESSION;
   SearchRecord: PSEARCH_RECORD): DWORD;
function XDVDFS_EnumFiles(
   Session: PXDVDFS_SESSION;
   SearchRecord: PSEARCH_RECORD): DWORD;
function XDVDFS_GetFileInfo(
   Session: PXDVDFS_SESSION;
   Filename: LPSTR;
   SearchRecord: PSEARCH_RECORD): DWORD;
function XDVDFS_OpenFolder(
   Session: PXDVDFS_SESSION;
   Path: LPSTR;
   SearchRecord: PSEARCH_RECORD): DWORD;
function XDVDFS_OpenFile(
   Session: PXDVDFS_SESSION;
   Filename: LPSTR;
   FileRecord: PFILE_RECORD): DWORD;
function XDVDFS_OpenFileEx(
   Session: PXDVDFS_SESSION;
   SearchRecord: PSEARCH_RECORD;
   FileRecord: PFILE_RECORD): DWORD;
function XDVDFS_FileRead(
   Session: PXDVDFS_SESSION;
   FileRecord: PFILE_RECORD;
   OutBuffer: PVOID;
   Size: DWORD): DWORD;
function XDVDFS_FileClose(
   Session: PXDVDFS_SESSION;
   FileRecord: PFILE_RECORD): DWORD;
function XDVDFS_FileSeek(
   Session: PXDVDFS_SESSION;
   FileRecord: PFILE_RECORD;
   Delta: int;
   SeekMode: DWORD): DWORD;

implementation

function UPPERCASE(a: AnsiChar): AnsiChar;
begin
  if a >= 'a' then Result := AnsiChar(Ord(a) and $DF) else Result := a;
end;

function MIN(a,b: DWORD): DWORD;
begin
  if a < b then Result := a else Result := b;
end;

function ENDIAN_SAFE32(a: DWORD): DWORD;
begin
  Result := a;
//  Result := (((a and $FF000000) shr 24) or ((a and $00FF0000) shr 8) or ((a and $0000FF00) shl 8) or ((a and $FF) shl 24));
end;

function OFFSET(var i): Integer;
begin
  Result := Integer(@i);
end;

//

function GetSectorBuffered(
        This: PCDIO_READ;
        SectorNumber: DWORD): PBYTE;
var
  i, index: int;
  Ptr: PBYTE;
begin
  // Have we got this baby in buffer ?
  for i := 0 to DISK_BUFFER - 1 do
  begin
    if This.SectorList[i] = SectorNumber then
    begin
      Inc(This.LockList[i]);
      Result := @(This.DiskBuffer[i * SECTOR_SIZE]);
      Exit;
    end;
  end;

  // Nope, load the sector and store it in buffer
  for i := 0 to DISK_BUFFER - 1 do
  begin
    index := This.WriteIndex;
    This.WriteIndex := (This.WriteIndex + 1) mod DISK_BUFFER;

    Inc(This.LockList[index]);
    if This.LockList[index] <> 1 then
    begin
      Dec(This.LockList[index]);
      Continue;
    end;

    // Take an entry into the ring buffer but do not
    // write the sector number yet (this is needed
    // for re-entrancy purposes
    Ptr := @(This.DiskBuffer[index * SECTOR_SIZE]);

    if This.Sectors(This.Data, Ptr, SectorNumber, 1) then
    begin
      // OK, write the sector number and exit
      This.SectorList[index] := SectorNumber;
      Result := Ptr;
    end
    else
    begin
      // If something went bad free the slot
      This.SectorList[index] := 0;
      Dec(This.LockList[index]);
      Result := NULL;
    end;

    Exit;
  end;

  // We land here if all entries were locked, and that's BAD !
  Result := NULL;
end;

procedure ReleaseBufferedSector(
        This: PCDIO_READ;
        SectorNumber: DWORD);
var
  i: int;
begin
  // Find the sector in the lock list and decrease its usage count
  for i := 0 to DISK_BUFFER - 1 do
  begin
    if (This.SectorList[i] = SectorNumber) and (This.LockList[i] > 0) then
    begin
      Dec(This.LockList[i]);
      Exit;
    end;
  end;
end;

//

function XDVDFS_Mount(
   Session: PXDVDFS_SESSION;
   ReadFunc: TReadFunc;
   Data: PVOID): LongBool;
begin
  XDVDFS_UnMount(Session);

  Session.Read.Data := Data;
  Session.Read.Sectors := ReadFunc;
  Session.StartSector := 0;

  // Read in the volume descriptor
  if (not Session.Read.Sectors(
      Session.Read.Data,
      PVOID(@Session.Root),
      Session.StartSector + 32,
      1)) then
  begin
    Result := False;
    Exit;
  end;

  // Check signatures
  if (memcmp(@Session.Root.Signature1[0], XDVDFS_Signature, Length(XDVDFS_Signature)) <> 0)
  or (memcmp(@Session.Root.Signature2[0], XDVDFS_Signature, Length(XDVDFS_Signature)) <> 0) then
    Result := False
  else
    Result := True;
end;

// XDVDFS deinit a session object
function XDVDFS_UnMount(
   Session: PXDVDFS_SESSION): LongBool;
begin
  // Reset ring buffer
  memset(@(Session.Read.SectorList[0]), 0, SizeOf(DWORD) * DISK_BUFFER);
  memset(@(Session.Read.LockList[0]), 0, SizeOf(DWORD) * DISK_BUFFER);
  Session.Read.WriteIndex := 0;

  // Invalidate all open files and search structures
  Inc(Session.Magic);

  Result := True;
end;

// Initialize a search record with root dir
// Note: Can return XDVDFS_NO_MORE_FILES if the image is empty
function XDVDFS_GetRootDir(
   Session: PXDVDFS_SESSION;
   SearchRecord: PSEARCH_RECORD): DWORD;
begin
  if Session.Root.RootDirectorySize = 0 then
  begin
    Result := XDVDFS_NO_MORE_FILES;
    Exit;
  end;

  SearchRecord.Magic := Session.Magic;
  SearchRecord.StartSector := ENDIAN_SAFE32(Session.Root.RootDirectory);
  SearchRecord.DirectorySize := ENDIAN_SAFE32(Session.Root.RootDirectorySize);
  SearchRecord.Position := 0;

  Result := XDVDFS_NO_ERROR;
end;

// Enumerate files
function XDVDFS_EnumFiles(
   Session: PXDVDFS_SESSION;
   SearchRecord: PSEARCH_RECORD): DWORD;
var
  Entry: PXDVDFS_DIRECTORY_ENTRY;
  SectorNumber, Position: DWORD;
  i: Integer; // Needs to be signed, so not a DWORD!
  Ptr: PBYTE;
begin
  repeat // enum_retry:
    // Check structure validity
    if SearchRecord.Magic <> Session.Magic then
    begin
      Result := XDVDFS_EXPIRED_SESSION;
      Exit;
    end;

    // Check if we reached the end of the directory
    if SearchRecord.Position >= SearchRecord.DirectorySize then
    begin
      Result := XDVDFS_NO_MORE_FILES;
      Exit;
    end;

    // Get current position in sector, remainder
    SectorNumber := SearchRecord.Position div SECTOR_SIZE;
    Position := SearchRecord.Position mod SECTOR_SIZE;

    // Get the current sector in buffer and lock it
    Ptr := GetSectorBuffered(@Session.Read, SectorNumber + SearchRecord.StartSector);
    if Ptr = nil then
    begin
      Result := XDVDFS_DISK_ERROR;
      Exit;
    end;

    // If we're at the begining...
    if SearchRecord.Position = 0 then
    begin
      // ...bufferize the whole dir.
      for i := 1 to Integer(MIN(DISK_BUFFER - 1, SearchRecord.DirectorySize div SECTOR_SIZE)) - 1 do
      begin
        GetSectorBuffered(@Session.Read, DWORD(i) + SearchRecord.StartSector);
        ReleaseBufferedSector(@Session.Read, DWORD(i) + SearchRecord.StartSector);
      end;
    end;

    Entry := PXDVDFS_DIRECTORY_ENTRY(@Ptr[Position]);

    // If Entry.StartSector = $FFFFFFFF or Position > 2040, we reached the last
    // entry of the sector
    // TODO -oDXBX: Constantify once we know where this 2040 comes from:
    if (Position > 2040) or (Entry.StartSector = $FFFFFFFF) then
    begin
      // Let's get the next one
      ReleaseBufferedSector(@Session.Read, SectorNumber + SearchRecord.StartSector);
      SearchRecord.Position := (SearchRecord.Position and (not (SECTOR_SIZE-1))) + SECTOR_SIZE;
      //goto enum_retry;
    end
    else
      Break; // enum_retry
  until False; // enum_retry

  // Copy the filename (filenames up to 255 chars)
  memcpy(@SearchRecord.Filename[0], @Entry.Filename[0], Entry.FilenameLength);
  SearchRecord.Filename[Entry.FilenameLength] := #0;

  // Copy file parameters in the search_rec
  SearchRecord.Attributes := Entry.FileAttributes;
  SearchRecord.FileSize := ENDIAN_SAFE32(Entry.FileSize);
  SearchRecord.FileStartSector := ENDIAN_SAFE32(Entry.StartSector);
  SearchRecord.FileEndSector := SearchRecord.FileStartSector + (SearchRecord.FileSize div SECTOR_SIZE);
  if (SearchRecord.FileSize mod SECTOR_SIZE) > 0 then
    Inc(SearchRecord.FileEndSector);

  // Advance to next entry
  Inc(SearchRecord.Position, OFFSET(PXDVDFS_DIRECTORY_ENTRY(nil).Filename) + Entry.FilenameLength);
  if (SearchRecord.Position and 3) > 0 then
  begin
    SearchRecord.Position := SearchRecord.Position and (not 3);
    Inc(SearchRecord.Position, 4);
  end;

  // Free the buffer
  ReleaseBufferedSector(@Session.Read, SectorNumber + SearchRecord.StartSector);

  Result := XDVDFS_NO_ERROR;
end;

// Find a file given its path
function XDVDFS_GetFileInfo(
   Session: PXDVDFS_SESSION;
   Filename: LPSTR;
   SearchRecord: PSEARCH_RECORD): DWORD;
var
  Length, i, ReturnCode: DWORD;
begin
  // To begin, we will enter the root directory
  Result := XDVDFS_GetRootDir(Session, SearchRecord);
  if Result <> XDVDFS_NO_ERROR then
    Exit;

  SearchRecord.FileStartSector := SearchRecord.StartSector;
  SearchRecord.FileSize := SearchRecord.DirectorySize;
  SearchRecord.Attributes := XDVDFS_ATTRIBUTE_DIRECTORY;
  SearchRecord.Filename[0] := #0;

  // Skip leading backslash if present
  if Filename^ = DIRECTORY_SEPARATOR then
    Inc(Filename);

  while Filename^ <> #0 do
  begin
    // Skip backslashes
    while Filename^ = DIRECTORY_SEPARATOR do
      Inc(Filename);

    // If previously matched name is not a dir, fail
    if (SearchRecord.Attributes and XDVDFS_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      Result := XDVDFS_FILE_NOT_FOUND;
      Exit;
    end;

    // Enter that directory
    Length := 0;
    SearchRecord.StartSector := SearchRecord.FileStartSector;
    SearchRecord.DirectorySize := SearchRecord.FileSize;
    SearchRecord.Position := 0;

    // Browse the contents of the dir
    repeat
      ReturnCode := XDVDFS_EnumFiles(Session, SearchRecord);
      if ReturnCode <> XDVDFS_NO_ERROR then
        Break;

      // Calculate length of the filename
      Length := 0;
      while (SearchRecord.Filename[Length] <> #0)
        and (SearchRecord.Filename[Length] <> DIRECTORY_SEPARATOR) do
        Inc(Length);

      // Match the filename against the one given
      for i := 0 to Length - 1 do
      begin
        if (Filename[i] = #0)
        or (Filename[i] = DIRECTORY_SEPARATOR)
        or (UPPERCASE(Filename[i]) <>  UPPERCASE(SearchRecord.Filename[i])) then
          Break;
      end;

      // If it matched, exit
      if (i = Length) and ((Filename[i] = #0) or (Filename[i] = DIRECTORY_SEPARATOR)) then
        Break;
    until False;

    // If we reached the end of the dir without matching, fail
    if ReturnCode = XDVDFS_NO_MORE_FILES then
    begin
      Result := XDVDFS_FILE_NOT_FOUND;
      Exit;
    end;

    // If any other error occured, fail
    if ReturnCode <> XDVDFS_NO_ERROR then
    begin
      Result := ReturnCode;
      Exit;
    end;

    // Match next part of the given filename
    Inc(Filename, Length);
  end;

  // If we land here, everything matched and the SEARCH_RECORD structure is
  // filled.
  Result := XDVDFS_NO_ERROR;
end;

// Initialize a search record given a path
function XDVDFS_OpenFolder(
   Session: PXDVDFS_SESSION;
   Path: LPSTR;
   SearchRecord: PSEARCH_RECORD): DWORD;
var
  ReturnCode: DWORD;
begin
  // Find a file matching the given path
  ReturnCode := XDVDFS_GetFileInfo(Session, Path, SearchRecord);

  // If an error occured, fail
  if ReturnCode <> XDVDFS_NO_ERROR then
  begin
    Result := ReturnCode;
    Exit;
  end;

  // If the returned file is not a dir, fail
  if (SearchRecord.Attributes and XDVDFS_ATTRIBUTE_DIRECTORY) = 0 then
  begin
    Result := XDVDFS_FILE_NOT_FOUND;
    Exit;
  end;

  // Copy folder info into the SEARCH_RECORD structure
  SearchRecord.StartSector := SearchRecord.FileStartSector;
  SearchRecord.DirectorySize := SearchRecord.FileSize;
  SearchRecord.Position := 0;

  Result := XDVDFS_NO_ERROR;
end;

// Open a file
function XDVDFS_OpenFile(
   Session: PXDVDFS_SESSION;
   Filename: LPSTR;
   FileRecord: PFILE_RECORD): DWORD;
var
  SearchRecord: SEARCH_RECORD;
  ReturnCode: DWORD;
begin
  // Find a file matching the given path
  ReturnCode := XDVDFS_GetFileInfo(Session, Filename, @SearchRecord);

  // If an error occured, fail
  if ReturnCode <> XDVDFS_NO_ERROR then
  begin
    Result := ReturnCode;
    Exit;
  end;

  // If the returned file is a dir, fail
  if (SearchRecord.Attributes and XDVDFS_ATTRIBUTE_DIRECTORY) > 0 then
  begin
    Result := XDVDFS_FILE_NOT_FOUND;
    Exit;
  end;

  // Copy file info into the FILE_RECORD structure
  FileRecord.Magic := SearchRecord.Magic;
  FileRecord.PartialSector := 0;
  FileRecord.StartSector := SearchRecord.FileStartSector;
  FileRecord.FileSize := SearchRecord.FileSize;
  FileRecord.CurrentPosition := 0;

  Result := XDVDFS_NO_ERROR;
end;

// Open a file pointed by a search rec
function XDVDFS_OpenFileEx(
   Session: PXDVDFS_SESSION;
   SearchRecord: PSEARCH_RECORD;
   FileRecord: PFILE_RECORD): DWORD;
begin
  // Check structure validity
  if SearchRecord.Magic <> Session.Magic then
  begin
    Result := XDVDFS_EXPIRED_SESSION;
    Exit;
  end;

  // Do not accept a directory
  if (SearchRecord.Attributes and XDVDFS_ATTRIBUTE_DIRECTORY) > 0 then
  begin
    Result := XDVDFS_FILE_NOT_FOUND;
    Exit;
  end;

  // Copy file info into the FILE_RECORD structure
  FileRecord.Magic := SearchRecord.Magic;
  FileRecord.PartialSector := 0;
  FileRecord.StartSector := SearchRecord.FileStartSector;
  FileRecord.FileSize := SearchRecord.FileSize;
  FileRecord.CurrentPosition := 0;

  Result := XDVDFS_NO_ERROR;
end;

// Read a file
function XDVDFS_FileRead(
   Session: PXDVDFS_SESSION;
   FileRecord: PFILE_RECORD;
   OutBuffer: PVOID;
   Size: DWORD): DWORD;
var
  CurrentSector, Position, PartialRead, Readed, i: DWORD;
  Buffer: PBYTE;
begin
  Readed := 0;

  // Check structure validity
  if FileRecord.Magic <> Session.Magic then
  begin
    Result := Readed;
    Exit;
  end;

  // Limit read size
  if (FileRecord.CurrentPosition + Size) > FileRecord.FileSize then
    Size := FileRecord.FileSize - FileRecord.CurrentPosition;

  if Size = 0 then
  begin
    Result := Readed;
    Exit;
  end;

  Buffer := OutBuffer;

  // Process partial sector read before
  Position := FileRecord.CurrentPosition mod SECTOR_SIZE;
  if Position > 0 then
  begin
    CurrentSector := (FileRecord.CurrentPosition div SECTOR_SIZE) + FileRecord.StartSector;
    PartialRead := MIN(Size, SECTOR_SIZE - Position);

    if FileRecord.PartialSector <> CurrentSector then
    begin
      if (not Session.Read.Sectors(
        Session.Read.Data,
        @FileRecord.PartialData[0],
        CurrentSector,
        1)) then
      begin
        Result := 0;
        Exit;
      end;

      FileRecord.PartialSector := CurrentSector;
    end;

    for i := 0 to PartialRead - 1 do
    begin
      Buffer^ := FileRecord.PartialData[Position];
      Inc(Position);
      Inc(Buffer);
    end;

    Dec(Size, PartialRead);
    Inc(Readed, PartialRead);
    Inc(FileRecord.CurrentPosition, PartialRead);

    if Size = 0 then
    begin
      Result := Readed;
      Exit;
    end;
  end;

  // Process entire sector read
  PartialRead := Size div SECTOR_SIZE;
  if PartialRead > 0 then
  begin
    CurrentSector := (FileRecord.CurrentPosition div SECTOR_SIZE) + FileRecord.StartSector;

    i := PartialRead;
    while i > 0 do
    begin
      Position := MIN(i, TRANSFER_SIZE);
      if (not Session.Read.Sectors(
        Session.Read.Data,
        Buffer,
        CurrentSector,
        Position)) then
      begin
        Result := Readed;
        Exit;
      end;

      Inc(Buffer, Position * SECTOR_SIZE);
      Inc(CurrentSector, Position);
      Dec(i, Position);
    end;

    PartialRead := PartialRead * SECTOR_SIZE;
    Dec(Size, PartialRead);
    Inc(Readed, PartialRead);
    Inc(FileRecord.CurrentPosition, PartialRead);

    if Size = 0 then
    begin
      Result := Readed;
      Exit;
    end;
  end;

  // Process partial sector read after
  PartialRead := Size;
  CurrentSector := (FileRecord.CurrentPosition div SECTOR_SIZE) + FileRecord.StartSector;
  if FileRecord.PartialSector <> CurrentSector then
  begin
    if (not Session.Read.Sectors(
         Session.Read.Data,
         @FileRecord.PartialData[0],
         CurrentSector,
         1)) then
    begin
      Result := Readed;
      Exit;
    end;

    FileRecord.PartialSector := CurrentSector;
  end;

  for i := 0 to PartialRead - 1 do
  begin
    Buffer^ := FileRecord.PartialData[i];
    Inc(Buffer);
  end;

  Inc(Readed, PartialRead);
  Inc(FileRecord.CurrentPosition, PartialRead);

  Result := Readed;
end;

// Close file
function XDVDFS_FileClose(
   Session: PXDVDFS_SESSION;
   FileRecord: PFILE_RECORD): DWORD;
begin
  // Check structure validity
  if FileRecord.Magic <> Session.Magic then
  begin
    Result := XDVDFS_EXPIRED_SESSION;
    Exit;
  end;

  // Invalidate the structure
  Dec(FileRecord.Magic);

  Result := XDVDFS_NO_ERROR;
end;

// File seek
function XDVDFS_FileSeek(
   Session: PXDVDFS_SESSION;
   FileRecord: PFILE_RECORD;
   Delta: int;
   SeekMode: DWORD): DWORD;
begin
  // Check structure validity
  if FileRecord.Magic <> Session.Magic then
  begin
    Result := XDVDFS_EXPIRED_SESSION;
    Exit;
  end;

  // Change file pointer
  case SeekMode of
    SM_BEGIN:
      FileRecord.CurrentPosition := Delta;
    SM_CURRENT:
      Inc(FileRecord.CurrentPosition, Delta);
    SM_END:
      FileRecord.CurrentPosition := DWORD(int(FileRecord.FileSize) - Delta);
  else
    Result := XDVDFS_INVALID_PARAMETER;
    Exit;
  end;

  // Check the file pointer for limits
  if FileRecord.CurrentPosition > FileRecord.FileSize then
  begin
    if Delta < 0 then
      FileRecord.CurrentPosition := 0
    else
      FileRecord.CurrentPosition := FileRecord.FileSize;
  end;

  Result := XDVDFS_NO_ERROR;
end;

end.

