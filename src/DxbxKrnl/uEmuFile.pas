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
  // Jedi Win32API
  JwaWinType,
  JwaWinNT,
  JwaNTStatus;

const
  // Maximum number of open handles in the system
  EMU_MAX_HANDLES = 1024;

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
  TEmuNtObject = class(TObject) // TODO -oDXBX:
  private
    RefCount: ULONG; // Reference count
  protected
    Name: PWideChar; // Object name (Unicode, because we handle after-conversion strings)
    NameLength: ULONG;
    PermanentFlag: Bool; // Permanent status
    destructor Destroy; override; // Not public, so it can't be called directly
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
    destructor Destroy; override; // Not public, so it can't be called directly
  public
    SymbolicLinkName, DeviceName: AnsiString;
    function ReturnLength: ULONG;

    function Init(aSymbolicLinkName, aDeviceName: AnsiString): NTSTATUS;
  end;

function IsEmuHandle(hFile: {xboxkrnl::} HANDLE): BOOL; inline
function HandleToEmuHandle(hFile: {xboxkrnl::} HANDLE): TEmuHandle; inline;
function EmuHandleToHandle(apEmuHandle: TEmuHandle): HANDLE; inline;

function FindNtSymbolicLinkObjectByName(const aSymbolicLinkName: string): TEmuNtSymbolicLinkObject;

implementation

var
  // Array of EmuHandles in the system
  {EmuHandle.}Handles: array [0..EMU_MAX_HANDLES - 1] of TEmuHandle;

  // Pointer to first free handle in array, or NULL if none
  {volatile EmuHandle.}FirstFree: TEmuHandle;

  // Pointer to last free handle in array, or NULL if none
  {volatile EmuHandle.}LastFree: TEmuHandle;

  // Lock on the handle system
  {EmuHandle.}HandleLock: _RTL_CRITICAL_SECTION;

// is hFile a 'special' emulated handle?
function IsEmuHandle(hFile: {xboxkrnl.}HANDLE): BOOL; inline;
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
  Assert(False, 'May not be destroyed directly!');
end;

function TEmuNtObject.NewHandle: HANDLE;
begin
  Result := EmuHandleToHandle(TEmuHandle.Create(Self));
end;

function TEmuNtObject.NtClose(): NTSTATUS;
begin
  Dec(RefCount);
  if RefCount <= 0 then
    inherited Destroy;

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

function FindNtSymbolicLinkObjectByName(const aSymbolicLinkName: string): TEmuNtSymbolicLinkObject;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  if (Length(aSymbolicLinkName) = 6) and (aSymbolicLinkName[5] in ['A'..'Z']) then
    Result := NtSymbolicLinkObjects[aSymbolicLinkName[5]]
  else
    Result := nil;
end;

destructor TEmuNtSymbolicLinkObject.Destroy;
begin
  NtSymbolicLinkObjects[SymbolicLinkName[5]] := nil;

  inherited Destroy;
end;

function TEmuNtSymbolicLinkObject.ReturnLength: ULONG;
begin
  Result := Length(DeviceName);
end;

function TEmuNtSymbolicLinkObject.Init(aSymbolicLinkName, aDeviceName: AnsiString): NTSTATUS;
begin
  Result := STATUS_OBJECT_NAME_INVALID;
  if (Length(aSymbolicLinkName) = 6) and (aSymbolicLinkName[5] in ['A'..'Z']) then
  begin
    Result := STATUS_OBJECT_NAME_COLLISION;
    if NtSymbolicLinkObjects[aSymbolicLinkName[5]] = nil then
    begin
      Result := STATUS_SUCCESS;
      Self.SymbolicLinkName := aSymbolicLinkName;
      Self.DeviceName := aDeviceName;
      NtSymbolicLinkObjects[aSymbolicLinkName[5]] := Self;
    end;
  end;
end;

end.
