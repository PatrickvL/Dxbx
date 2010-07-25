(*
    This file is part of Dxbx - a XBox emulator written in Delphi (ported over from cxbx)
    Copyright (C) 2007 Shadow_tj and other members of the development team.

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
unit uEmuFile;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows, // CreateDirectory
  SysUtils, // ForceDirectories, ExtractFilePath
  // Jedi Win32API
  JwaWinType,
  JwaWinNT,
  JwaNTStatus,
  JwaNative,
  // Dxbx
  uTypes,
  uLog,
  uDxbxUtils,
  uDxbxKrnlUtils;

const
  // Maximum number of open handles in the system
  EMU_MAX_HANDLES = 1024;

const
  DriveSerial: AnsiString = '\??\serial:';
  DriveCdRom0: AnsiString = '\??\CdRom0:'; // CD-ROM device
  DriveMbfs: AnsiString = '\??\mbfs:'; // media board's file system area device
  DriveMbcom: AnsiString = '\??\mbcom:'; // media board's communication area device
  DriveMbrom: AnsiString = '\??\mbrom:'; // media board's boot ROM device

  DriveC: AnsiString = '\??\C:'; // C: is HDD0
  DriveD: AnsiString = '\??\D:'; // D: is DVD Player
  DriveE: AnsiString = '\??\E:';
  DriveF: AnsiString = '\??\F:';
  DriveT: AnsiString = '\??\T:'; // T: is Title persistent data region
  DriveU: AnsiString = '\??\U:'; // U: is User persistent data region
  DriveV: AnsiString = '\??\V:';
  DriveW: AnsiString = '\??\W:';
  DriveX: AnsiString = '\??\X:';
  DriveY: AnsiString = '\??\Y:'; // Y: is Dashboard volume (contains "xboxdash.xbe" and "XDASH" folder + contents)
  DriveZ: AnsiString = '\??\Z:'; // Z: is Title utility data region

  DeviceCdrom0: AnsiString = '\Device\Cdrom0';
  DeviceHarddisk0: AnsiString = '\Device\Harddisk0';

  DeviceHarddisk0Partition0: AnsiString = '\Device\Harddisk0\Partition0'; // Contains raw config sectors (like XBOX_REFURB_INFO) + entire hard disk
  DeviceHarddisk0Partition1: AnsiString = '\Device\Harddisk0\Partition1'; // Data partition. Contains TDATA and UDATA folders.
  DeviceHarddisk0Partition2: AnsiString = '\Device\Harddisk0\Partition2'; // Shell partition. Contains Dashboard (cpxdash.xbe, evoxdash.xbe or xboxdash.xbe)
  DeviceHarddisk0Partition3: AnsiString = '\Device\Harddisk0\Partition3'; // First cache partition. Contains cache data (from here up to largest number)
  DeviceHarddisk0Partition4: AnsiString = '\Device\Harddisk0\Partition4';
  DeviceHarddisk0Partition5: AnsiString = '\Device\Harddisk0\Partition5';
  DeviceHarddisk0Partition6: AnsiString = '\Device\Harddisk0\Partition6';
  DeviceHarddisk0Partition7: AnsiString = '\Device\Harddisk0\Partition7';
  DeviceHarddisk0Partition8: AnsiString = '\Device\Harddisk0\Partition8';
  DeviceHarddisk0Partition9: AnsiString = '\Device\Harddisk0\Partition9';
  DeviceHarddisk0Partition10: AnsiString = '\Device\Harddisk0\Partition10';
  DeviceHarddisk0Partition11: AnsiString = '\Device\Harddisk0\Partition11';
  DeviceHarddisk0Partition12: AnsiString = '\Device\Harddisk0\Partition12';
  DeviceHarddisk0Partition13: AnsiString = '\Device\Harddisk0\Partition13';
  DeviceHarddisk0Partition14: AnsiString = '\Device\Harddisk0\Partition14';
  DeviceHarddisk0Partition15: AnsiString = '\Device\Harddisk0\Partition15';
  DeviceHarddisk0Partition16: AnsiString = '\Device\Harddisk0\Partition16';
  DeviceHarddisk0Partition17: AnsiString = '\Device\Harddisk0\Partition17';
  DeviceHarddisk0Partition18: AnsiString = '\Device\Harddisk0\Partition18';
  DeviceHarddisk0Partition19: AnsiString = '\Device\Harddisk0\Partition19';
  DeviceHarddisk0Partition20: AnsiString = '\Device\Harddisk0\Partition20'; // 20 = Largest possible partition number

type
  TEmuNtObject = class; // forward

  // Wrapper of a handle object
  TEmuHandle = class(TObject)
  protected
    FNtObject: TEmuNtObject;
    destructor Destroy; override; // Not public, so it can't be called directly
  public
    property NtObject: TEmuNtObject read FNtObject;

    constructor Create(const aNtObject: TEmuNtObject);

    function NtClose(): NTSTATUS;
    function NtDuplicateObject(TargetHandle: PHANDLE; Options: DWORD): NTSTATUS;
  end;

  // An fake NT object
  TEmuNtObject = class(TObject)
  private
    RefCount: ULONG; // Reference count
  protected
    Name: PWideChar; // Object name (Unicode, because we handle after-conversion strings)
    NameLength: ULONG;
    PermanentFlag: Bool; // Permanent status
    destructor Destroy; override; // Shouln't be called directly
  public
    constructor Create; virtual;

    // Create a new EmuHandle for this object, and return is as a HANDLE :
    function NewHandle: HANDLE;

    // These functions mimic the Nt* calls :

    // Decrements the reference count of this object (never override)
    function NtClose(): NTSTATUS;

    // Increments the reference count of this object.
    // For some object types, a whole new TEmuNtObject object is returned.
    // For other objects (the default implementation), "Self" is returned.
    function NtDuplicateObject(Options: DWORD): TEmuNtObject; virtual;
  end;

(*
  // Emulated handle to file
  TEmuNtFile = class(TEmuNtObject)
  private
    File_: HANDLE; // The Windows file handle
    // Volume: TEmuNtVolume; // Pointer to the volume from which this came
  public
    // TODO -oCXBX: We need to override NtDuplicateObject in this case
    // A whole new EmuNtFile object should be returned.
  end;
*)

  // Emulated handle to symbolic link object
  TEmuNtSymbolicLinkObject = class(TEmuNtObject)
  protected
    destructor Destroy; override; // Shouln't be called directly
  public
    DriveLetter: AnsiChar;
    SymbolicLinkName: AnsiString;
    XboxFullPath: AnsiString;
    NativePath: string;
    RootDirectoryHandle: Handle;

    function Init(aSymbolicLinkName, aFullPath: AnsiString): NTSTATUS;
  end;

function IsEmuHandle(hFile: {xboxkrnl::} HANDLE): Boolean; inline
function HandleToEmuHandle(hFile: {xboxkrnl::} HANDLE): TEmuHandle; inline;
function EmuHandleToHandle(apEmuHandle: TEmuHandle): HANDLE; inline;

function SymbolicLinkToDriveLetter(aSymbolicLinkName: AnsiString): AnsiChar;
function FindNtSymbolicLinkObjectByVolumeLetter(const aVolumeLetter: AnsiChar): TEmuNtSymbolicLinkObject;
function FindNtSymbolicLinkObjectByName(const aSymbolicLinkName: AnsiString): TEmuNtSymbolicLinkObject;
function FindNtSymbolicLinkObjectByDevice(const aDeviceName: AnsiString): TEmuNtSymbolicLinkObject;

function DxbxRegisterDeviceNativePath(XboxFullPath: AnsiString; NativePath: string): Boolean;
function DxbxGetDeviceNativeRootHandle(XboxFullPath: AnsiString): Handle;
function DxbxCreateSymbolicLink(SymbolicLinkName, FullPath: AnsiString): NTSTATUS;
function DxbxPC2XB_FILE_INFORMATION(NativeFileInformation, FileInformation: PVOID;
  FileInformationClass: FILE_INFORMATION_CLASS): Boolean;

implementation

(*
var
  // Array of EmuHandles in the system
  {EmuHandle.}Handles: array [0..EMU_MAX_HANDLES - 1] of TEmuHandle;

  // Pointer to first free handle in array, or NULL if none
  {volatile EmuHandle.}FirstFree: TEmuHandle;

  // Pointer to last free handle in array, or NULL if none
  {volatile EmuHandle.}LastFree: TEmuHandle;

  // Lock on the handle system
  {EmuHandle.}HandleLock: _RTL_CRITICAL_SECTION;
*)

// is hFile a 'special' emulated handle?
function IsEmuHandle(hFile: {xboxkrnl.}HANDLE): Boolean; inline;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  Result := (uint32(hFile) > $80000000) and (int32(hFile) <> -1);
end;

// convert from 'special' emulated handle to a pointer
function HandleToEmuHandle(hFile: {xboxkrnl.}HANDLE): TEmuHandle; inline;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  Result := TEmuHandle(uint32(hFile) - $80000000);
end;

// convert from a pointer to 'special' emulated handle
function EmuHandleToHandle(apEmuHandle: TEmuHandle): HANDLE; inline;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  Result := HANDLE(uint32(apEmuHandle) + $80000000);
end;

{ TEmuHandle }

constructor TEmuHandle.Create(const aNtObject: TEmuNtObject);
begin
  inherited Create;

  FNtObject := aNtObject;
end;

destructor TEmuHandle.Destroy;
begin
  Assert(False, 'May not be destroyed directly!');
end;

function TEmuHandle.NtClose(): NTSTATUS;
begin
  Assert(Assigned(FNtObject));

  Result := FNtObject.NtClose();

  inherited Destroy;
end;

function TEmuHandle.NtDuplicateObject(TargetHandle: PHANDLE; Options: DWORD): NTSTATUS;
begin
  TargetHandle^ := FNtObject.NtDuplicateObject(Options).NewHandle;
  Result := STATUS_SUCCESS;
end;

{ TEmuNtObject }

constructor TEmuNtObject.Create;
begin
  inherited Create;

  RefCount := 1;
end;

destructor TEmuNtObject.Destroy;
begin
  Assert(RefCount <= 0);

  inherited Destroy;
end;

function TEmuNtObject.NewHandle: HANDLE;
begin
  Result := EmuHandleToHandle(TEmuHandle.Create(Self));
end;

function TEmuNtObject.NtClose(): NTSTATUS;
begin
  Dec(RefCount);
  if RefCount <= 0 then
    Destroy;

  Result := STATUS_SUCCESS;
end;

function TEmuNtObject.NtDuplicateObject(Options: DWORD): TEmuNtObject;
begin
  Inc(RefCount);
  Result := Self;
end;

{ TEmuNtSymbolicLinkObject }

var
  NtSymbolicLinkObjects: array['A'..'Z'] of TEmuNtSymbolicLinkObject;

function SymbolicLinkToDriveLetter(aSymbolicLinkName: AnsiString): AnsiChar;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  // SymbolicLinkName must look like this : "\??\D:"
  if  (Length(aSymbolicLinkName) = 6)
  and (aSymbolicLinkName[1] = '\')
  and (aSymbolicLinkName[2] = '?')
  and (aSymbolicLinkName[3] = '?')
  and (aSymbolicLinkName[4] = '\')
  and (aSymbolicLinkName[6] = ':') then
  begin
    Result := aSymbolicLinkName[5];
    case Result of
      'A'..'Z':
        Exit;
      'a'..'z':
      begin
        Result := AnsiChar(Ord(Result) + Ord('A') - Ord('a'));
        Exit;
      end;
    end;
  end;

  Result := #0;
end;

function FindNtSymbolicLinkObjectByVolumeLetter(const aVolumeLetter: AnsiChar): TEmuNtSymbolicLinkObject;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  case aVolumeLetter of
    'A'..'Z':
      Result := NtSymbolicLinkObjects[aVolumeLetter];
    'a'..'z':
      Result := NtSymbolicLinkObjects[AnsiChar(Ord(aVolumeLetter) + Ord('A') - Ord('a'))];
  else
    Result := nil;
  end;
end;

function FindNtSymbolicLinkObjectByName(const aSymbolicLinkName: AnsiString): TEmuNtSymbolicLinkObject;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  Result := FindNtSymbolicLinkObjectByVolumeLetter(SymbolicLinkToDriveLetter(aSymbolicLinkName));
end;

function FindNtSymbolicLinkObjectByDevice(const aDeviceName: AnsiString): TEmuNtSymbolicLinkObject;
// Branch:Dxbx  Translator:PatrickvL  Done:0
var
  VolumeLetter: Char;
begin
  for VolumeLetter := 'A' to 'Z' do
  begin
    Result := NtSymbolicLinkObjects[VolumeLetter];
    if Assigned(Result) and StartsWithText(aDeviceName, Result.XboxFullPath) then
      Exit;
  end;

  Result := nil;
end;

function DxbxCreateSymbolicLink(SymbolicLinkName, FullPath: AnsiString): NTSTATUS;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  EmuNtSymbolicLinkObject: TEmuNtSymbolicLinkObject;
begin
  // Check if this symbolic link already exists :
  EmuNtSymbolicLinkObject := FindNtSymbolicLinkObjectByName(SymbolicLinkName);
  if Assigned(EmuNtSymbolicLinkObject) then
    // In that case, close it :
    EmuNtSymbolicLinkObject.NtClose;

  // Now (re)create a symbolic link object, and initialize it with the new definition :
  EmuNtSymbolicLinkObject := TEmuNtSymbolicLinkObject.Create;
  Result := EmuNtSymbolicLinkObject.Init(SymbolicLinkName, FullPath);
  if Result <> STATUS_SUCCESS then
    EmuNtSymbolicLinkObject.NtClose;
end;

var
  Devices: array of record XboxFullPath: AnsiString; NativePath: string; NativeRootHandle: Handle; end;

function DxbxRegisterDeviceNativePath(XboxFullPath: AnsiString; NativePath: string): Boolean;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  i: Integer;
begin
  Result := ForceDirectories(ExtractFilePath(NativePath));
  if Result then
  begin
    i := Length(Devices);
    SetLength(Devices, i + 1);

    Devices[i].XboxFullPath := XboxFullPath;
    Devices[i].NativePath := NativePath;
  end;
end;

function DxbxGetDeviceNativeRootHandle(XboxFullPath: AnsiString): Handle;
var
  i: Integer;
begin
  for i := 0 to Length(Devices) - 1 do
  begin
    if StartsWithText(XboxFullPath, Devices[i].XboxFullPath) then
    begin
      // Not all that nice; get a handle to the native folder :
      if Devices[i].NativeRootHandle = 0 then
        Devices[i].NativeRootHandle := CreateFile(PChar(Devices[i].NativePath),
          GENERIC_READ,
          FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
          NULL,
          OPEN_EXISTING,
          FILE_FLAG_BACKUP_SEMANTICS,
          HNULL);

      Result := Devices[i].NativeRootHandle;
      Exit;
    end;
  end;

  Result := INVALID_HANDLE_VALUE;
end;

// Create a copy of the native (WideChar based) file information record
// to Xbox (AnsiChar based) format, returning the length of the filling.
function DxbxPC2XB_FILE_INFORMATION(NativeFileInformation, FileInformation: PVOID;
  FileInformationClass: FILE_INFORMATION_CLASS): Boolean;
var
  CopySize: DWord;
  StringLengthOffset: DWord;
  mbstr: P_char;
  wcstr: pwchar_t;
begin
  Result := True;
  case FileInformationClass of
    FileDirectoryInformation: // = 1 FILE_DIRECTORY_INFORMATION
    begin
      CopySize := SizeOf(FILE_DIRECTORY_INFORMATION);
      StringLengthOffset := FIELD_OFFSET(PFILE_DIRECTORY_INFORMATION(nil).FileNameLength);
      wcstr := @(PFILE_DIRECTORY_INFORMATION(NativeFileInformation).FileName[0]);
      mbstr := @(PFILE_DIRECTORY_INFORMATION(FileInformation).FileName[0]);
    end;

//    FileFullDirectoryInformation: ; // = 2 ?
//    FileBothDirectoryInformation: ; // = 3 ?
//    FileBasicInformation: ; // = 4 FILE_BASIC_INFORMATION / FILE_READ_ATTRIBUTES
//    FileStandardInformation: ; // = 5 FILE_STANDARD_INFORMATION
//    FileInternalInformation: ; // = 6 FILE_INTERNAL_INFORMATION
//    FileEaInformation: ; // = 7 FILE_EA_INFORMATION
//    FileAccessInformation: ; // = 8 ?
    FileNameInformation: // = 9 FILE_NAME_INFORMATION
    begin
      CopySize := SizeOf(FILE_NAME_INFORMATION);
      StringLengthOffset := FIELD_OFFSET(PFILE_NAME_INFORMATION(nil).FileNameLength);
      wcstr := @(PFILE_NAME_INFORMATION(NativeFileInformation).FileName[0]);
      mbstr := @(PFILE_NAME_INFORMATION(FileInformation).FileName[0]);
    end;

//    FileRenameInformation: ; // = 10 FILE_RENAME_INFORMATION
    FileLinkInformation: // = 11 FILE_LINK_INFORMATION
    begin
      CopySize := SizeOf(FILE_LINK_INFORMATION);
      StringLengthOffset := FIELD_OFFSET(PFILE_LINK_INFORMATION(nil).FileNameLength);
      wcstr := @(PFILE_LINK_INFORMATION(NativeFileInformation).FileName[0]);
      mbstr := @(PFILE_LINK_INFORMATION(FileInformation).FileName[0]);
    end;

    FileNamesInformation: // = 12 FILE_NAMES_INFORMATION
    begin
      // TODO -oDxbx : How should we support multiple information records (linked via NextEntryOffset) ?
      CopySize := SizeOf(FILE_NAMES_INFORMATION);
      StringLengthOffset := FIELD_OFFSET(PFILE_NAMES_INFORMATION(nil).FileNameLength);
      wcstr := @(PFILE_NAMES_INFORMATION(NativeFileInformation).FileName[0]);
      mbstr := @(PFILE_NAMES_INFORMATION(FileInformation).FileName[0]);
    end;

//    FileDispositionInformation: ; // = 13 FILE_DISPOSITION_INFORMATION
//    FilePositionInformation: ; // = 14 FILE_POSITION_INFORMATION
//    FileFullEaInformation: ; // = 15 ? / FILE_READ_EA
//    FileModeInformation: ; // = 16 FILE_MODE_INFORMATION
//    FileAlignmentInformation: ; // = 17 FILE_ALIGNMENT_INFORMATION
    FileAllInformation: // = 18 FILE_ALL_INFORMATION / FILE_READ_ATTRIBUTES
    begin
      CopySize := SizeOf(FILE_ALL_INFORMATION);
      StringLengthOffset := FIELD_OFFSET(PFILE_ALL_INFORMATION(nil).NameInformation.FileNameLength);
      wcstr := @(PFILE_ALL_INFORMATION(NativeFileInformation).NameInformation.FileName[0]);
      mbstr := @(PFILE_ALL_INFORMATION(FileInformation).NameInformation.FileName[0]);
    end;

//    FileAllocationInformation: ; // = 19 FILE_ALLOCATION_INFORMATION
//    FileEndOfFileInformation: ; // = 20 FILE_END_OF_FILE_INFORMATION
//    FileAlternateNameInformation: ; // = 21 FILE_NAME_INFORMATION
    FileStreamInformation: // = 22 FILE_STREAM_INFORMATION
    begin
      // TODO -oDxbx : How should we support multiple information records (linked via NextEntryOffset) ?
      CopySize := SizeOf(FILE_STREAM_INFORMATION);
      StringLengthOffset := FIELD_OFFSET(PFILE_STREAM_INFORMATION(nil).StreamNameLength);
      wcstr := @(PFILE_STREAM_INFORMATION(NativeFileInformation).StreamName[0]);
      mbstr := @(PFILE_STREAM_INFORMATION(FileInformation).StreamName[0]);
    end;

//    FilePipeInformation: ; // = 23 ? / FILE_READ_ATTRIBUTES
//    FilePipeLocalInformation: ; // = 24 ? / FILE_READ_ATTRIBUTES
//    FilePipeRemoteInformation: ; // = 25 ? / FILE_READ_ATTRIBUTES
//    FileMailslotQueryInformation: ; // = 26 ?
//    FileMailslotSetInformation: ; // = 27 ?
//    FileCompressionInformation: ; // = 28 ?
//    FileObjectIdInformation: ; // = 29 ?
//    FileCompletionInformation: ; // = 30 FILE_COMPLETION_INFORMATION
//    FileMoveClusterInformation: ; // = 31 FILE_MOVE_CLUSTER_INFORMATION
//    FileQuotaInformation: ; // = 32 ?
//    FileReparsePointInformation: ; // = 33 ?
//    FileNetworkOpenInformation: ; // = 34 FILE_NETWORK_OPEN_INFORMATION / FILE_READ_ATTRIBUTES
//    FileAttributeTagInformation: ; // = 35 FILE_ATTRIBUTE_TAG_INFORMATION / FILE_READ_ATTRIBUTES
//    FileTrackingInformation: ; // = 36 ?
//    FileMaximumInformation: ; // = 37 ?

//    FileFsSizeInformation: ;
//    FileFsVolumeInformation: ;
  else
    // No Wide>Ansi conversion needed
    Result := False;
    Exit;
  end;

  // convert from PC to Xbox
  memcpy(FileInformation, NativeFileInformation, CopySize);

  // Halve the amount of memory needed for the string :
  PInteger(MathPtr(FileInformation) + StringLengthOffset)^ := PInteger(MathPtr(FileInformation) + StringLengthOffset)^ div 2;

  // Convert the WideChar string to Ansi :
  wcstombs(mbstr, wcstr, PInteger(MathPtr(FileInformation) + StringLengthOffset)^);
end;

//

destructor TEmuNtSymbolicLinkObject.Destroy;
begin
  if DriveLetter in ['A'..'Z'] then
    NtSymbolicLinkObjects[DriveLetter] := nil;

  if RootDirectoryHandle <> INVALID_HANDLE_VALUE then
    JwaNative.NtClose(RootDirectoryHandle);

  inherited Destroy;
end;

function TEmuNtSymbolicLinkObject.Init(aSymbolicLinkName, aFullPath: AnsiString): NTSTATUS;
var
  IsNativePath: Boolean;
  i: Integer;
  ExtraPath: AnsiString;
  DeviceIndex: Integer;
begin
  Result := STATUS_OBJECT_NAME_INVALID;
  DriveLetter := SymbolicLinkToDriveLetter(aSymbolicLinkName);
  if DriveLetter in ['A'..'Z'] then
  begin
    Result := STATUS_OBJECT_NAME_COLLISION;
    if FindNtSymbolicLinkObjectByVolumeLetter(DriveLetter) = nil then
    begin
      // Look up the partition in the list of pre-registered devices :
      Result := STATUS_DEVICE_DOES_NOT_EXIST; // TODO : Is this the correct error?

      // Make a distinction between Xbox paths (starting with '\Device'...) and Native paths :
      IsNativePath := (aFullPath[1] <> '\');
      if IsNativePath then
        DeviceIndex := 0
      else
      begin
        DeviceIndex := -1;
        for i := 0 to Length(Devices) - 1 do
          if StartsWithText(aFullPath, Devices[i].XboxFullPath) then
          begin
            DeviceIndex := i;
            Break;
          end;
      end;

      if DeviceIndex >= 0 then
      begin
        Result := STATUS_SUCCESS;
        Self.SymbolicLinkName := aSymbolicLinkName;
        Self.XboxFullPath := aFullPath; // TODO : What path do we remember in IsNativePath mode?
        if IsNativePath then
        begin
          Self.NativePath := '';
          ExtraPath := aFullPath;
        end
        else
        begin
          Self.NativePath := Devices[DeviceIndex].NativePath;
          // Handle the case where a sub folder of the partition is mounted (instead of it's root) :
          ExtraPath := Copy(aFullPath, Length(Devices[DeviceIndex].XboxFullPath) + 1, MaxInt);
        end;

        if ExtraPath <> '' then
        begin
          Self.NativePath := Self.NativePath + string(ExtraPath);
          ForceDirectories(Self.NativePath);
        end;

        Self.RootDirectoryHandle := CreateFile(PChar(Self.NativePath),
          GENERIC_READ,
          FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
          NULL,
          OPEN_EXISTING,
          FILE_FLAG_BACKUP_SEMANTICS,
          HNULL);

        if Self.RootDirectoryHandle = INVALID_HANDLE_VALUE then
        begin
          Result := STATUS_DEVICE_DOES_NOT_EXIST; // TODO : Is this the correct error?
          DxbxKrnlCleanup('Could not map ' + string(NativePath));
        end
        else
        begin
          NtSymbolicLinkObjects[DriveLetter] := Self;
          DbgPrintf('EmuMain : Linked "%s" to "%s" (residing at "%s")', [aSymbolicLinkName, aFullPath, NativePath]);
        end;
      end;
    end;
  end;
end;

end.
