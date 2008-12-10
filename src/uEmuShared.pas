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
unit uEmuShared;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows
  , SysUtils // StrCopy
{$IFDEF DXBX_USE_JCLDEBUG}
  // Jcl
  , JclDebug
{$ENDIF}
  // Dxbx
  , uMutex
  , uLog
  , uXbVideo
  , uXBController
{$IFDEF DXBX_DLL}
  , uDxbxKrnlUtils // CxbxKrnlCleanup
{$ENDIF}
  ;

const
  CXBX_MAX_PATH = 260;

type
  PEmuShared = ^EmuShared;
  EmuShared = record
    m_Mutex: Mutex;
    // Shared configuration
    m_XBController: XBController;
    m_XBVideo: XBVideo;
    m_XbePath: array [0..CXBX_MAX_PATH - 1] of AnsiChar;

    // Each process needs to call this to initialize shared memory
    class function Init: Boolean; static;
    // Each process needs to call this to cleanup shared memory
    class procedure Cleanup; static;
    // Constructor / Deconstructor
    procedure Create;
    procedure Destroy;
    procedure DestroyNoFree;

    procedure Lock();
    procedure Unlock();

    // Xbox Video Accessors
    procedure GetXBVideo(var video: XBVideo);
    procedure SetXBVideo(const video: XBVideo);

    // Xbox Controller Accessors
    procedure GetXBController(var ctrl: XBController);
    procedure SetXBController(const ctrl: XBController);

    // Xbe Path Accessors
    procedure GetXbePath(var Path: string);
    procedure SetXbePath(const Path: string);
  end;

procedure SetXbePath(const Path: PAnsiChar); cdecl;

var
  hMapObject: THandle;
  // Exported Global Shared Memory Pointer
  g_EmuShared: PEmuShared;
  g_EmuSharedRefCount: Integer; // extern; ??

implementation

{$IFNDEF DXBX_DLL}
procedure CxbxKrnlCleanup(const aMessage: string);
begin
  raise Exception.Create(aMessage);
end;
{$ENDIF}

{ EmuShared }

class function EmuShared.Init: Boolean;
begin
  // Ensure initialization only occurs once
  Result := True;
  WriteLog('EmuShared.Init');

  // Prevent multiple initializations
  if hMapObject <> 0 then
    Exit;

{$IFDEF DXBX_USE_JCLDEBUG}
  // Start tracking exceptions using JclDebug
  JclStartExceptionTracking;
{$ENDIF}

  // Create the shared memory "file"
  begin
    hMapObject := CreateFileMapping(
      INVALID_HANDLE_VALUE, // Paging file
      nil, // default security attributes
      PAGE_READWRITE, // read/write access
      0, // size: high 32 bits
      SizeOf(EmuShared), // size: low 32 bits
      'Local\EmuShared' // name of map object
      );

    if GetLastError() = ERROR_ALREADY_EXISTS then
      Result := False;

    if hMapObject = 0 then
      CxbxKrnlCleanup('Could not map shared memory!');
  end;

  // Memory map this file
  begin
    g_EmuShared := PEmuShared(MapViewOfFile(
      hMapObject, // object to map view of
      FILE_MAP_WRITE, // read/write access
      0, // high offset:  map from
      0, // low offset:   beginning
      0 // default: map entire file
      ));

    if not Assigned(g_EmuShared) then
      CxbxKrnlCleanup('Could not map view of shared memory!');
  end;

  // Executed only on first initialization of shared memory
  if Result then
  begin
    ZeroMemory(g_EmuShared, SizeOf(EmuShared)); // clear memory
    g_EmuShared.Create; // call constructor
  end;

  Inc(g_EmuSharedRefCount);
end;

class procedure EmuShared.Cleanup;
begin
  WriteLog('EmuShared.Cleanup');
  Dec(g_EmuSharedRefCount);

  if g_EmuSharedRefCount = 0 then
  begin
    g_EmuShared.DestroyNoFree;

    UnmapViewOfFile(g_EmuShared);
    g_EmuShared := nil;

{$IFDEF DXBX_USE_JCLDEBUG}
    JclStopExceptionTracking;
{$ENDIF}
  end;

  CloseLogs;
end;

procedure EmuShared.Create;
begin
  m_Mutex.Create;
  m_XBController.Load(PChar('Software\Cxbx\XBController'));
  m_XBVideo.Load(PChar('Software\Cxbx\XBVideo'));
end;

procedure EmuShared.DestroyNoFree;
begin
  m_XBController.Save(PChar('Software\Cxbx\XBController'));
  m_XBVideo.Save(PChar('Software\Cxbx\XBVideo'));
end;

procedure EmuShared.Destroy;
begin
  DestroyNoFree;
end;

procedure EmuShared.Lock();
begin
  m_Mutex.Lock();
end;

procedure EmuShared.Unlock();
begin
  m_Mutex.Unlock();
end;

// Xbox Video Accessors

procedure EmuShared.GetXBVideo(var video: XBVideo);
begin
  Lock();
  {var}video := {shared}m_XBVideo;
  Unlock();
end;

procedure EmuShared.SetXBVideo(const video: XBVideo);
begin
  Lock();
  {shared}m_XBVideo := video;
  Unlock();
end;

// Xbox Controller Accessors

procedure EmuShared.GetXBController(var ctrl: XBController);
// Branch:martin  Revision:39  Translator:PatrickvL  Done : 100
begin
  Lock();
  {var}ctrl := {shared}m_XBController;
  Unlock();
end;

procedure EmuShared.SetXBController(const ctrl: XBController);
begin
  Lock();
  {shared}m_XBController := ctrl;
  Unlock();
end;

procedure EmuShared.GetXbePath(var Path: string);
begin
  Lock();
  {var}Path := {shared}m_XbePath;
  Unlock();
end;

procedure EmuShared.SetXbePath(const Path: string);
begin
  WriteLog('EmuShared.SetXbePath(' + Path + ')');
  Lock();
  CopyMemory(@({shared}m_XbePath[0]), PChar(Path), Length(Path) + 1);
  Unlock();
end;

//

procedure SetXbePath(const Path: PAnsiChar);
begin
  if Assigned(g_EmuShared) then
    g_EmuShared.SetXbePath(Path);
end;

exports
  SetXbePath name '?SetXbePath@EmuShared@@QAEXPBD@Z';

end.
