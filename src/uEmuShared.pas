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
  Windows,
  SysUtils, // StrCopy
  // Dxbx
  uMutex,
  uLog,
  uXbVideo,
  uXBController,
  uDxbxKrnlUtils;

type
  EmuShared = class(Mutex)
  protected
    m_XbePath: array [0..MAX_PATH-1] of AnsiChar;
    m_XBController : XBController;
    m_XBVideo : XBVideo;
  public
    // Each process needs to call this to initialize shared memory
    class function Init: Boolean;
    // Each process needs to call this to cleanup shared memory
    class procedure Cleanup;
    // Constructor / Deconstructor
    constructor Create; override;
    destructor Destroy; override;
    procedure DestroyNoFree;
    
    // Xbox Video Accessors
    procedure GetXBVideo( video : XBVideo );
    procedure SetXBVideo(const video : XBVideo );

    // Xbox Controller Accessors
    procedure GetXBController( ctrl : XBController );
    procedure SetXBController(const ctrl : XBController );

    // Xbe Path Accessors
    procedure GetXbePath(var Path: string);
    procedure SetXbePath(const Path: string);
  end;

procedure SetXbePath(const Path: PAnsiChar); cdecl;

var
  hMapObject: THandle;
  // Exported Global Shared Memory Pointer
  g_EmuShared: EmuShared;
  g_EmuSharedRefCount: Integer; // extern; ??

implementation

procedure SetXbePath(const Path: PChar);
begin
  g_EmuShared.SetXbePath(Path);
end;

{ EmuShared }

class function EmuShared.Init: Boolean;
begin
  // Ensure initialization only occurs once
  Result := True;
  WriteLog('EmuShared.Init');

  // Prevent multiple initializations
  if hMapObject <> 0 then
    Exit;

  // Create the shared memory "file"
  begin
    hMapObject := CreateFileMapping(
          INVALID_HANDLE_VALUE,   // Paging file
          nil,                    // default security attributes
          PAGE_READWRITE,         // read/write access
          0,                      // size: high 32 bits
          EmuShared.InstanceSize, // size: low 32 bits
          'Local\EmuShared'       // name of map object
      );

    if GetLastError() = ERROR_ALREADY_EXISTS then
      Result := False;

    if hMapObject = 0 then
      CxbxKrnlCleanup('Could not map shared memory!');
  end;

  // Memory map this file
  begin
     g_EmuShared := EmuShared(MapViewOfFile(
            hMapObject,     // object to map view of
            FILE_MAP_WRITE, // read/write access
            0,              // high offset:  map from
            0,              // low offset:   beginning
            0               // default: map entire file
        ));

    if not Assigned(g_EmuShared) then
      CxbxKrnlCleanup('Could not map view of shared memory!');
  end;

  // Executed only on first initialization of shared memory
  if Result then
  begin
    // WATCH OUT: Dirty trick to 'create' a fixed instance in memory :
    ZeroMemory(g_EmuShared, EmuShared.InstanceSize); // clear memory
    PPointer(g_EmuShared)^ := EmuShared; // assign type
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
  end;

  CloseLogs;
end;

constructor EmuShared.Create;
begin
  inherited Create;
  m_XBController.Load(PChar ('Software\Cxbx\XBController'));
  m_XBVideo.Load(PChar ('Software\Cxbx\XBVideo'));
end;

procedure EmuShared.DestroyNoFree;
begin
  m_XBController.Save(PChar ('Software\Cxbx\XBController'));
  m_XBVideo.Save(PChar ('Software\Cxbx\XBVideo'));
end;

destructor EmuShared.Destroy;
begin
  DestroyNoFree;
  inherited Destroy;
end;

// Xbox Video Accessors
procedure EmuShared.GetXBVideo( video : XBVideo );
begin
  { TODO : Need to be translated }
  { Lock(); memcpy(video, &m_XBVideo, sizeof(XBVideo)); Unlock(); }
end;

procedure EmuShared.SetXBVideo(const video : XBVideo );
begin
  { TODO : Need to be translated }
  { Lock(); memcpy(&m_XBVideo, video, sizeof(XBVideo)); Unlock(); }
end;

// Xbox Controller Accessors
procedure EmuShared.GetXBController( ctrl : XBController );
begin
  { TODO : Need to be translated }
  { Lock(); memcpy(ctrl, &m_XBController, sizeof(XBController)); Unlock();}
end;

procedure EmuShared.SetXBController(const ctrl : XBController );
begin
  { TODO : Need to be translated }
  { Lock(); memcpy(&m_XBController, ctrl, sizeof(XBController)); Unlock();}
end;

procedure EmuShared.GetXbePath(var Path: string);
begin
  Lock();
  Path := m_XbePath;
  Unlock();
end;

procedure EmuShared.SetXbePath(const Path: string);
begin
WriteLog('EmuShared.SetXbePath(' + Path + ')');
  Lock();
  CopyMemory(@(m_XbePath[0]), PChar(Path), Length(Path) + 1); 
  Unlock();
end;

end.
