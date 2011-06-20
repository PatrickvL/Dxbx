unit uXbSound;

interface

uses
  // Delphi
  Windows,
  SysUtils,
  // Dxbx
  uError,
  uTypes;

// class: XBSound
type
  XBSound = object(Error)
  private
    m_bMute: BOOL;
    m_SoundAdapterGUID: TGUID;
  public
    procedure Initialize;
    procedure Finalize;

    // Registry Load/Save
    procedure Load(const szRegistryKey: P_char);
    procedure Save(const szRegistryKey: P_char);

    procedure SetSoundAdapterGUID(Value: TGUID);
    function GetSoundAdapterGUID: TGUID;

    // property Mute Toggling
    procedure SetMute(bMute: _BOOL);
    function GetMute: _BOOL;
  end;
  PXBSound = ^XBSound;

implementation

{ XBSound }

procedure XBSound.Finalize;
// Branch:Dxbx  Revision:0.5  Translator:Shadow_tj  Done:100
begin

end;

function XBSound.GetMute: _BOOL;
// Branch:Dxbx  Revision:0.5  Translator:Shadow_tj  Done:100
begin
  Result := m_bMute <> BOOL_FALSE;
end;

function XBSound.GetSoundAdapterGUID: TGUID;
// Branch:Dxbx  Revision:0.5  Translator:Shadow_tj  Done:100
begin
  Result := m_SoundAdapterGUID;
end;

procedure XBSound.Initialize;
// Branch:Dxbx  Revision:0.5  Translator:Shadow_tj  Done:100
begin
  m_bMute := BOOL_FAlSE;
end;

procedure XBSound.Load(const szRegistryKey: P_char);
// Branch:Dxbx  Revision:0.5  Translator:Shadow_tj  Done:100
var
  dwDisposition, dwType, dwSize: DWORD;
  hKey: Windows.HKEY;
begin
  // Load Configuration from Registry
  try
    if RegCreateKeyExA(HKEY_CURRENT_USER, szRegistryKey, 0, NULL, REG_OPTION_NON_VOLATILE, KEY_QUERY_VALUE, NULL, {var}hKey, @dwDisposition) = ERROR_SUCCESS then
    try
      dwType := REG_BINARY; dwSize := sizeof(TGUID);
      RegQueryValueExA(hKey, 'SoundAdapter', NULL, @dwType, PBYTE(@m_SoundAdapterGUID), @dwSize);

      dwType := REG_DWORD; dwSize := sizeof(DWORD);
      RegQueryValueExA(hKey, 'Mute', NULL, @dwType, PBYTE(@m_bMute), @dwSize);
    finally
      RegCloseKey(hKey);
    end;
  except
    raise Exception.Create('XBVideo.Load raised an exception');
  end;
end;

procedure XBSound.Save(const szRegistryKey: P_char);
// Branch:Dxbx  Revision:0.5  Translator:Shadow_tj  Done:100
var
  dwDisposition, dwType, dwSize: DWORD;
  hKey: Windows.HKEY;
begin
  // Save Configuration to Registry
  try
    if RegCreateKeyExA(HKEY_CURRENT_USER, szRegistryKey, 0, NULL, REG_OPTION_NON_VOLATILE, KEY_SET_VALUE, NULL, {var}hKey, @dwDisposition) = ERROR_SUCCESS then
    try
      dwType := REG_BINARY; dwSize := sizeof(TGUID);
      RegSetValueExA(hKey, 'SoundAdapter', 0, dwType, PBYTE(@m_SoundAdapterGUID), dwSize);

      dwType := REG_DWORD; dwSize := sizeof(DWORD);
      RegSetValueExA(hKey, 'Mute', 0, dwType, PBYTE(@m_bMute), dwSize);
    finally
      RegCloseKey(hKey);
    end;
  except
    raise Exception.Create('XBVideo.Save raised an exception');
  end;
end;

procedure XBSound.SetMute(bMute: _BOOL);
// Branch:Dxbx  Revision:0.5  Translator:Shadow_tj  Done:100
begin
  if bMute then
    m_bMute := BOOL_TRUE
  else
    m_bMute := BOOL_FALSE;
end;

procedure XBSound.SetSoundAdapterGUID(Value: TGUID);
// Branch:Dxbx  Revision:0.5  Translator:Shadow_tj  Done:100
begin
  m_SoundAdapterGUID := Value;
end;

end.
