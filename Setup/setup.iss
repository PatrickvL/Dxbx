[Setup]
AppName=DXBX
AppVerName=DXBX 0.0.0.8
AppPublisher=Shadow_tj, Pcucho, Wayo & Zomby
DefaultDirName={pf}\DXBX
DefaultGroupName=DXBX
OutputDir=F:\development\delphi\Projects\Dxbx\Dxbx\Setup
OutputBaseFilename=setup
SetupIconFile=F:\development\delphi\Projects\Dxbx\Dxbx\src\Tools\XdkTracker\resource\Cxbx.ico
Compression=lzma
SolidCompression=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "dutch"; MessagesFile: "compiler:Languages\Dutch.isl"
Name: "french"; MessagesFile: "compiler:Languages\French.isl"
Name: "german"; MessagesFile: "compiler:Languages\German.isl"
Name: "spanish"; MessagesFile: "compiler:Languages\Spanish.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "F:\development\delphi\Projects\Dxbx\Dxbx\bin\Dxbx.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "F:\development\delphi\Projects\Dxbx\Dxbx\bin\Tools\*"; DestDir: "{app}\Tools"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "F:\development\delphi\Projects\Dxbx\Dxbx\bin\msvcrtd.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "F:\development\delphi\Projects\Dxbx\Dxbx\bin\DxbxKrnl.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "F:\development\delphi\Projects\Dxbx\Dxbx\bin\CxbxKrnl.dll"; DestDir: "{app}"; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\DXBX"; Filename: "{app}\Dxbx.exe"
Name: "{group}\{cm:UninstallProgram,DXBX}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\DXBX"; Filename: "{app}\Dxbx.exe"; Tasks: desktopicon
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\DXBX"; Filename: "{app}\Dxbx.exe"; Tasks: quicklaunchicon

[Run]
Filename: "{app}\Dxbx.exe"; Description: "{cm:LaunchProgram,DXBX}"; Flags: nowait postinstall skipifsilent

