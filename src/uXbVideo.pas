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

Uses
  Windows;

type
  T100CharArray = Array [0..100] of Char;

  XBVideo = record
    private
      m_bFullscreen: Boolean;
    public
      procedure Load(const szRegistryKey: PChar);
      procedure Save(const szRegistryKey: PChar);
      Function GetDisplayAdapter : DWord;
      Function GetDirect3DDevice : DWord;
      Function GetVSync : BOOL;
      procedure SetFullscreen(bFullscreen: Boolean);
      Function GetVideoResolution : T100CharArray;
      function GetFullscreen: Boolean;
  end;

var
  m_dwDisplayAdapter : DWORD;
  m_dwDirect3DDevice : DWORD;
  m_bVSync : BOOL;
  m_szVideoResolution : T100CharArray;
implementation



// func: XBVideo::XBVideo
(*
XBVideo::XBVideo() : m_bVSync(false), m_bFullscreen(false)
{
    strcpy(m_szVideoResolution, 'Automatic (Default)');
}
*)


// func: XBVideo::~XBVideo
(*XBVideo::~XBVideo()
{
}
*)



{ XBVideo }

function XBVideo.GetDirect3DDevice: DWord;
begin
  Result := m_dwDirect3DDevice;
end;

Function XBVideo.GetDisplayAdapter : DWord;
begin
  Result := m_dwDisplayAdapter;
end;

function XBVideo.GetFullscreen: Boolean;
begin
  Result := m_bFullscreen;
end;

function XBVideo.GetVideoResolution: T100CharArray;
begin
  Result := m_szVideoResolution;
end;

function XBVideo.GetVSync: BOOL;
begin
  Result := m_bVSync;
end;

procedure XBVideo.Load(const szRegistryKey: PChar);
begin
(*
{
    // Load Configuration from Registry
    {
        DWORD   dwDisposition, dwType, dwSize;
        HKEY    hKey;

        if(RegCreateKeyEx(HKEY_CURRENT_USER, szRegistryKey, 0, NULL, REG_OPTION_NON_VOLATILE, KEY_QUERY_VALUE, NULL, &hKey, &dwDisposition) == ERROR_SUCCESS)
        {
            int v=0;

            dwType = REG_SZ; dwSize = 100;
            RegQueryValueEx(hKey, 'VideoResolution', NULL, &dwType, (PBYTE)m_szVideoResolution, &dwSize);

            dwType = REG_DWORD; dwSize = sizeof(DWORD);
            RegQueryValueEx(hKey, 'DisplayAdapter', NULL, &dwType, (PBYTE)&m_dwDisplayAdapter, &dwSize);

            dwType = REG_DWORD; dwSize = sizeof(DWORD);
            RegQueryValueEx(hKey, 'Direct3DDevice', NULL, &dwType, (PBYTE)&m_dwDirect3DDevice, &dwSize);

            dwType = REG_DWORD; dwSize = sizeof(DWORD);
            RegQueryValueEx(hKey, 'Fullscreen', NULL, &dwType, (PBYTE)&m_bFullscreen, &dwSize);

            dwType = REG_DWORD; dwSize = sizeof(DWORD);
            RegQueryValueEx(hKey, 'VSync', NULL, &dwType, (PBYTE)&m_bVSync, &dwSize);

            RegCloseKey(hKey);
        }
    }
}        *)
end;

procedure XBVideo.Save(const szRegistryKey: PChar);
begin
(*{
    // ******************************************************************
    // * Save Configuration to Registry
    // ******************************************************************
    {
        DWORD   dwDisposition, dwType, dwSize;
        HKEY    hKey;

        if(RegCreateKeyEx(HKEY_CURRENT_USER, szRegistryKey, 0, NULL, REG_OPTION_NON_VOLATILE, KEY_SET_VALUE, NULL, &hKey, &dwDisposition) == ERROR_SUCCESS)
        {
            int v=0;

            dwType = REG_SZ; dwSize = 100;
            RegSetValueEx(hKey, 'VideoResolution', 0, dwType, (PBYTE)m_szVideoResolution, dwSize);

            dwType = REG_DWORD; dwSize = sizeof(DWORD);
            RegSetValueEx(hKey, 'DisplayAdapter', 0, dwType, (PBYTE)&m_dwDisplayAdapter, dwSize);

            dwType = REG_DWORD; dwSize = sizeof(DWORD);
            RegSetValueEx(hKey, 'Direct3DDevice', 0, dwType, (PBYTE)&m_dwDirect3DDevice, dwSize);

            dwType = REG_DWORD; dwSize = sizeof(DWORD);
            RegSetValueEx(hKey, 'Fullscreen', 0, dwType, (PBYTE)&m_bFullscreen, dwSize);

            dwType = REG_DWORD; dwSize = sizeof(DWORD);
            RegSetValueEx(hKey, 'VSync', 0, dwType, (PBYTE)&m_bVSync, dwSize);

            RegCloseKey(hKey);
        }
    }
}  *)
end;

procedure XBVideo.SetFullscreen(bFullscreen: Boolean);
begin
  m_bFullscreen := bFullscreen;
end;

end.
