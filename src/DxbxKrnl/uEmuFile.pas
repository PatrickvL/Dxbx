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

{$IFDEF DXBX_EMUHANDLES}

uses
  // Jedi
  JwaWinType,
  JwaWinNT;

const
  // Maximum number of open handles in the system
  EMU_MAX_HANDLES = 1024;

type
  // Wrapper of a handle object
  TEmuHandle = class(TObject)
    // TODO
  end;

  // An NT fake object
  TEmuNtObject = class(TObject) // Dxbx TODO
  private
    RefCount: ULONG; // Reference count
  protected
    Name: PWideChar; // Object name (Unicode, because we handle after-conversion strings)
    NameLength: ULONG;
    PermanentFlag: Bool; // Permanent status
  public
    // Decrements the reference count of this object (never override)
    procedure NtClose();
    // These functions mimic the Nt* calls

    // Increments the reference count of this object
    // For file handles, a whole new EmuFile structure is returned.
    // For other objects (the default implementation), "self" is returned.
    function NtDuplicateObject(): TEmuNtObject; virtual;
  end;

  // Emulated file handle
  TEmuNtFile = class(TEmuNtObject)
  private
    File_: HANDLE; // The Windows file handle
    // Volume: TEmuNtVolume; // Pointer to the volume from which this came
  public
    // Cxbx TODO : We need to override NtDuplicateObject in this case
  end;

var
  // Array of EmuHandles in the system
  {EmuHandle.}Handles: array[0..EMU_MAX_HANDLES - 1] of TEmuHandle;

  // Pointer to first free handle in array, or NULL if none
  {volatile EmuHandle.}FirstFree: TEmuHandle;

  // Pointer to last free handle in array, or NULL if none
  {volatile EmuHandle.}LastFree: TEmuHandle;

  // Lock on the handle system
  {EmuHandle.}HandleLock: _RTL_CRITICAL_SECTION;

function IsEmuHandle(hFile: {xboxkrnl::} HANDLE): BOOL; inline
function EmuHandleToPtr(hFile: {xboxkrnl::} HANDLE): TEmuHandle; inline;
function PtrToEmuHandle(apEmuHandle: TEmuHandle): HANDLE; inline;

{$ENDIF}

implementation

{$IFDEF DXBX_EMUHANDLES}

// is hFile a 'special' emulated handle?

function IsEmuHandle(hFile: {xboxkrnl.}HANDLE): BOOL; inline;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  Result := (uint32(hFile) > $80000000) and (int32(hFile) <> -1);
end;

// convert from 'special' emulated handle to a pointer

function EmuHandleToPtr(hFile: {xboxkrnl.}HANDLE): TEmuHandle; inline;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  Result := TEmuHandle(uint32(hFile) - $80000000);
end;

// convert from 'special' emulated handle to a pointer

function PtrToEmuHandle(apEmuHandle: TEmuHandle): HANDLE; inline;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  Result := HANDLE(uint32(apEmuHandle) + $80000000);
end;

{$ENDIF}

end.
