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

unit uXbVideo;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  SysUtils,
  // Dxbx
  uTypes,
  uError;

// Dxbx Note : Cxbx uses BOOL in this unit, but that's an int-based type and we prefer a true boolean type,
// so we'll use the _BOOL type (=System.Boolean) instead for our property getters and setters.

type
  XBVideo = object(Error)
  // Branch:shogun  Revision:162  Translator:PatrickvL  Done:100
  private
    // Configuration
    m_szVideoResolution: array [0..100-1] of _char;
    m_dwDisplayAdapter: DWORD;
    m_dwDirect3DDevice: DWORD;
    m_bFullscreen: BOOL;
    m_bVSync: BOOL;
    m_bHardwareYUV: BOOL;
  public
    procedure Initialize;
    procedure Finalize;

    // Registry Load/Save
    procedure Load(const szRegistryKey: P_char);
    procedure Save(const szRegistryKey: P_char);

    // property Direct3DDevice
    procedure SetDirect3DDevice(Value: DWORD);
    function GetDirect3DDevice: DWORD;

    // property DisplayAdapter
    procedure SetDisplayAdapter(Value: DWORD);
    function GetDisplayAdapter: DWORD;

    // property VideoResolution
    procedure SetVideoResolution(szBuffer: P_char);
    function GetVideoResolution: P_char;

    // property Fullscreen Toggling
    procedure SetFullscreen(bFullscreen: _BOOL);
    function GetFullscreen: _BOOL;

    // property VSync Toggling
    procedure SetVSync(bVSync: _BOOL);
    function GetVSync: _BOOL;

    // Hardware YUV Toggling
    procedure SetHardwareYUV(bHardwareYUV: _BOOL);
    function GetHardwareYUV: _BOOL;
  end; // size = 128 (as in Cxbx)
  PXBVideo = ^XBVideo;

implementation

{ XBVideo }

procedure XBVideo.Initialize;
begin
  m_bVSync := BOOL_FALSE;
  m_bFullscreen := BOOL_FALSE;
  m_bHardwareYUV := BOOL_FALSE;
  m_szVideoResolution := 'Automatic (Default)';
end;

procedure XBVideo.Finalize;
begin
end;

procedure XBVideo.Load(const szRegistryKey: P_char);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  dwDisposition, dwType, dwSize: DWORD;
  hKey: Windows.HKEY;
begin
  // Load Configuration from Registry
  try
    if RegCreateKeyExA(HKEY_CURRENT_USER, szRegistryKey, 0, NULL, REG_OPTION_NON_VOLATILE, KEY_QUERY_VALUE, NULL, {var}hKey, @dwDisposition) = ERROR_SUCCESS then
    try
      dwType := REG_SZ; dwSize := 100;
      RegQueryValueExA(hKey, 'VideoResolution', NULL, @dwType, PBYTE(@(m_szVideoResolution[0])), @dwSize);

      dwType := REG_DWORD; dwSize := sizeof(DWORD);
      RegQueryValueExA(hKey, 'DisplayAdapter', NULL, @dwType, PBYTE(@m_dwDisplayAdapter), @dwSize);

      dwType := REG_DWORD; dwSize := sizeof(DWORD);
      RegQueryValueExA(hKey, 'Direct3DDevice', NULL, @dwType, PBYTE(@m_dwDirect3DDevice), @dwSize);

      dwType := REG_DWORD; dwSize := sizeof(DWORD);
      RegQueryValueExA(hKey, 'Fullscreen', NULL, @dwType, PBYTE(@m_bFullscreen), @dwSize);

      dwType := REG_DWORD; dwSize := sizeof(DWORD);
      RegQueryValueExA(hKey, 'VSync', NULL, @dwType, PBYTE(@m_bVSync), @dwSize);

      dwType := REG_DWORD; dwSize := sizeof(DWORD);
      RegQueryValueExA(hKey, 'HardwareYUV', NULL, @dwType, PBYTE(@m_bHardwareYUV), @dwSize);
    finally
      RegCloseKey(hKey);
    end;
  except
    raise Exception.Create('XBVideo.Load raised an exception');
  end;
end;

procedure XBVideo.Save(const szRegistryKey: P_char);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  dwDisposition, dwType, dwSize: DWORD;
  hKey: Windows.HKEY;
begin
  // Save Configuration to Registry
  try
    if RegCreateKeyExA(HKEY_CURRENT_USER, szRegistryKey, 0, NULL, REG_OPTION_NON_VOLATILE, KEY_SET_VALUE, NULL, {var}hKey, @dwDisposition) = ERROR_SUCCESS then
    try
      dwType := REG_SZ; dwSize := 100;
      RegSetValueExA(hKey, 'VideoResolution', 0, dwType, PBYTE(@(m_szVideoResolution[0])), dwSize);

      dwType := REG_DWORD; dwSize := sizeof(DWORD);
      RegSetValueExA(hKey, 'DisplayAdapter', 0, dwType, PBYTE(@m_dwDisplayAdapter), dwSize);

      dwType := REG_DWORD; dwSize := sizeof(DWORD);
      RegSetValueExA(hKey, 'Direct3DDevice', 0, dwType, PBYTE(@m_dwDirect3DDevice), dwSize);

      dwType := REG_DWORD; dwSize := sizeof(DWORD);
      RegSetValueExA(hKey, 'Fullscreen', 0, dwType, PBYTE(@m_bFullscreen), dwSize);

      dwType := REG_DWORD; dwSize := sizeof(DWORD);
      RegSetValueExA(hKey, 'VSync', 0, dwType, PBYTE(@m_bVSync), dwSize);

      dwType := REG_DWORD; dwSize := sizeof(DWORD);
      RegSetValueExA(hKey, 'HardwareYUV', 0, dwType, PBYTE(@m_bHardwareYUV), dwSize);
    finally
      RegCloseKey(hKey);
    end;
  except
    raise Exception.Create('XBVideo.Save raised an exception');
  end;
end;

procedure XBVideo.SetDirect3DDevice(Value: DWORD);
begin
  m_dwDirect3DDevice := Value;
end;

function XBVideo.GetDirect3DDevice: DWORD;
begin
  Result := m_dwDirect3DDevice;
end;

procedure XBVideo.SetDisplayAdapter(Value: DWORD);
begin
  m_dwDisplayAdapter := Value;
end;

function XBVideo.GetDisplayAdapter: DWORD;
begin
  Result := m_dwDisplayAdapter;
end;

procedure XBVideo.SetVideoResolution(szBuffer: P_char);
begin
  strcpy(@m_szVideoResolution[0], szBuffer);
end;

function XBVideo.GetVideoResolution: P_char;
begin
  Result := @(m_szVideoResolution[0]);
end;

procedure XBVideo.SetFullscreen(bFullscreen: _BOOL);
begin
  if bFullscreen then
    m_bFullscreen := BOOL_TRUE
  else
    m_bFullscreen := BOOL_FALSE;
end;

function XBVideo.GetFullscreen: _BOOL;
begin
  Result := m_bFullscreen <> BOOL_FALSE;
end;

procedure XBVideo.SetVSync(bVSync: _BOOL);
begin
  if bVSync then
    m_bVSync := BOOL_TRUE
  else
    m_bVSync := BOOL_FALSE;
end;

function XBVideo.GetVSync: _BOOL;
begin
  Result := m_bVSync <> BOOL_FALSE;
end;

procedure XBVideo.SetHardwareYUV(bHardwareYUV: _BOOL);
begin
  if bHardwareYUV then
    m_bHardwareYUV := BOOL_TRUE
  else
    m_bHardwareYUV := BOOL_FALSE;
end;

function XBVideo.GetHardwareYUV: _BOOL;
begin
  Result := m_bHardwareYUV <> BOOL_FALSE;
end;

end.

