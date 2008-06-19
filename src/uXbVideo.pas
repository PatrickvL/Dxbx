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


type
  XBVideo = class
    private
    public
      procedure Load (const szRegistryKey : Char);
      procedure Save (const szRegistryKey : Char );
  end;


implementation


// func: XBVideo::XBVideo
(*
XBVideo::XBVideo() : m_bVSync(false), m_bFullscreen(false)
{
    strcpy(m_szVideoResolution, "Automatic (Default)");
}
*)


// func: XBVideo::~XBVideo
(*XBVideo::~XBVideo()
{
}
*)



{ XBVideo }

procedure XBVideo.Load(const szRegistryKey: Char);
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
            RegQueryValueEx(hKey, "VideoResolution", NULL, &dwType, (PBYTE)m_szVideoResolution, &dwSize);

            dwType = REG_DWORD; dwSize = sizeof(DWORD);
            RegQueryValueEx(hKey, "DisplayAdapter", NULL, &dwType, (PBYTE)&m_dwDisplayAdapter, &dwSize);

            dwType = REG_DWORD; dwSize = sizeof(DWORD);
            RegQueryValueEx(hKey, "Direct3DDevice", NULL, &dwType, (PBYTE)&m_dwDirect3DDevice, &dwSize);

            dwType = REG_DWORD; dwSize = sizeof(DWORD);
            RegQueryValueEx(hKey, "Fullscreen", NULL, &dwType, (PBYTE)&m_bFullscreen, &dwSize);

            dwType = REG_DWORD; dwSize = sizeof(DWORD);
            RegQueryValueEx(hKey, "VSync", NULL, &dwType, (PBYTE)&m_bVSync, &dwSize);

            RegCloseKey(hKey);
        }
    }
}        *)
end;

procedure XBVideo.Save(const szRegistryKey: Char);
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
            RegSetValueEx(hKey, "VideoResolution", 0, dwType, (PBYTE)m_szVideoResolution, dwSize);

            dwType = REG_DWORD; dwSize = sizeof(DWORD);
            RegSetValueEx(hKey, "DisplayAdapter", 0, dwType, (PBYTE)&m_dwDisplayAdapter, dwSize);

            dwType = REG_DWORD; dwSize = sizeof(DWORD);
            RegSetValueEx(hKey, "Direct3DDevice", 0, dwType, (PBYTE)&m_dwDirect3DDevice, dwSize);

            dwType = REG_DWORD; dwSize = sizeof(DWORD);
            RegSetValueEx(hKey, "Fullscreen", 0, dwType, (PBYTE)&m_bFullscreen, dwSize);

            dwType = REG_DWORD; dwSize = sizeof(DWORD);
            RegSetValueEx(hKey, "VSync", 0, dwType, (PBYTE)&m_bVSync, dwSize);

            RegCloseKey(hKey);
        }
    }
}  *)
end;

end.
