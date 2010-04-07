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
  uTypes;

type BOOL = LongBool; // In this unit, use LongBool instead of BOOL :

type
  XBVideo = packed record
  private
    // Configuration
    m_szVideoResolution: array [0..100-1] of AnsiChar;
    m_dwDisplayAdapter: DWORD;
    m_dwDirect3DDevice: DWORD;
    m_bFullscreen: Integer;//BOOL; was bool, checked with rergistry should be integer
    m_bVSync: Integer; //BOOL; was bool, checked with rergistry should be integer
    m_bHardwareYUV: Integer; //BOOL; was bool, checked with rergistry should be integer
  public
    procedure Initialize;
    procedure Finalize;

    // Registry Load/Save
    procedure Load(const szRegistryKey: PAnsiChar);
    procedure Save(const szRegistryKey: PAnsiChar);

    // property Direct3DDevice
    procedure SetDirect3DDevice(Value: DWord);
    function GetDirect3DDevice: DWord;

    // property DisplayAdapter
    procedure SetDisplayAdapter(Value: DWord);
    function GetDisplayAdapter: DWord;

    // property VideoResolution
    procedure SetVideoResolution(Value: PAnsiChar);
    function GetVideoResolution: PAnsiChar;

    // property Fullscreen Toggling
    procedure SetFullscreen(bFullscreen: integer);
    function GetFullscreen: integer;

    // property VSync Toggling
    procedure SetVSync(Value: Integer);
    function GetVSync: Integer;

    // Hardware YUV Toggling
    procedure SetHardwareYUV(Value: Integer);
    function GetHardwareYUV: Integer;
  end;
  PXBVideo = ^XBVideo;

implementation

{ XBVideo }

procedure XBVideo.Initialize;
begin
  m_bVSync := 0;
  m_bFullscreen := 0;
  m_bHardwareYUV := 0;
  m_szVideoResolution := 'Automatic (Default)';
end;

procedure XBVideo.Finalize;
begin
end;

procedure XBVideo.Load(const szRegistryKey: PAnsiChar);
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100 
var
  dwDisposition, dwType, dwSize: DWORD;
  hKey: Windows.HKEY;
begin
  // Load Configuration from Registry
  try
    if RegCreateKeyExA(HKEY_CURRENT_USER, szRegistryKey, 0, NULL, REG_OPTION_NON_VOLATILE, KEY_QUERY_VALUE, NULL, {var}hKey, @dwDisposition) = ERROR_SUCCESS then
    begin
      dwType := REG_SZ; dwSize := 100;
      RegQueryValueExA(hKey, 'VideoResolution', NULL, @dwType, PBYTE(@(m_szVideoResolution[0])), @dwSize);

      dwType := REG_DWORD; dwSize := SizeOf(DWORD);
      RegQueryValueExA(hKey, 'DisplayAdapter', NULL, @dwType, PBYTE(@m_dwDisplayAdapter), @dwSize);

      dwType := REG_DWORD; dwSize := SizeOf(DWORD);
      RegQueryValueExA(hKey, 'Direct3DDevice', NULL, @dwType, PBYTE(@m_dwDirect3DDevice), @dwSize);

      dwType := REG_DWORD; dwSize := SizeOf(DWORD);
      RegQueryValueExA(hKey, 'Fullscreen', NULL, @dwType, PBYTE(@m_bFullscreen), @dwSize);

      dwType := REG_DWORD; dwSize := SizeOf(DWORD);
      RegQueryValueExA(hKey, 'VSync', NULL, @dwType, PBYTE(@m_bVSync), @dwSize);

      dwType := REG_DWORD; dwSize := SizeOf(DWORD);
      RegQueryValueExA(hKey, 'HardwareYUV', NULL, @dwType, PBYTE(@m_bHardwareYUV), @dwSize);

      RegCloseKey(hKey);
    end;
  except
    raise Exception.Create('XBVideo.Load raised an exception');
  end;
end;

procedure XBVideo.Save(const szRegistryKey: PAnsiChar);
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100   
var
  dwDisposition, dwType, dwSize: DWORD;
  hKey: Windows.HKEY;
begin
  // Save Configuration to Registry
  try
    if RegCreateKeyExA(HKEY_CURRENT_USER, szRegistryKey, 0, NULL, REG_OPTION_NON_VOLATILE, KEY_SET_VALUE, NULL, {var}hKey, @dwDisposition) = ERROR_SUCCESS then
    begin
      dwType := REG_SZ; dwSize := 100;
      RegSetValueEx(hKey, 'VideoResolution', 0, dwType, PBYTE(@(m_szVideoResolution[0])), dwSize);

      dwType := REG_DWORD; dwSize := SizeOf(DWORD);
      RegSetValueEx(hKey, 'DisplayAdapter', 0, dwType, PBYTE(@m_dwDisplayAdapter), dwSize);

      dwType := REG_DWORD; dwSize := SizeOf(DWORD);
      RegSetValueEx(hKey, 'Direct3DDevice', 0, dwType, PBYTE(@m_dwDirect3DDevice), dwSize);

      dwType := REG_DWORD; dwSize := SizeOf(DWORD);
      RegSetValueEx(hKey, 'Fullscreen', 0, dwType, PBYTE(@m_bFullscreen), dwSize);

      dwType := REG_DWORD; dwSize := SizeOf(DWORD);
      RegSetValueEx(hKey, 'VSync', 0, dwType, PBYTE(@m_bVSync), dwSize);

      dwType := REG_DWORD; dwSize := SizeOf(DWORD);
      RegSetValueEx(hKey, 'HardwareYUV', 0, dwType, PBYTE(@m_bHardwareYUV), dwSize);

      RegCloseKey(hKey);
    end;
  except
    raise Exception.Create('XBVideo.Save raised an exception');
  end;
end;

procedure XBVideo.SetDirect3DDevice(Value: DWord);
begin
  m_dwDirect3DDevice := Value;
end;

function XBVideo.GetDirect3DDevice: DWord;
begin
  Result := m_dwDirect3DDevice;
end;

procedure XBVideo.SetDisplayAdapter(Value: DWord);
begin
  m_dwDisplayAdapter := Value;
end;

function XBVideo.GetDisplayAdapter: DWord;
begin
  Result := m_dwDisplayAdapter;
end;

procedure XBVideo.SetVideoResolution(Value: PAnsiChar);
begin
  strcpy(m_szVideoResolution, Value);
end;

function XBVideo.GetVideoResolution: PAnsiChar;
begin
  Result := @(m_szVideoResolution[0]);
end;

procedure XBVideo.SetFullscreen(bFullscreen: integer);
begin
  m_bFullscreen := bFullscreen;
end;

function XBVideo.GetFullscreen: integer;
begin
  Result := m_bFullscreen;
end;

procedure XBVideo.SetVSync(Value: Integer);
begin
  m_bVSync := Value;
end;

function XBVideo.GetVSync: Integer;
begin
  Result := m_bVSync;
end;

procedure XBVideo.SetHardwareYUV(Value: Integer);
begin
  m_bHardwareYUV := Value;
end;

function XBVideo.GetHardwareYUV: Integer;
begin
  Result := m_bHardwareYUV;
end;

end.

