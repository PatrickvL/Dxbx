[Setup]
AppName=XBE Explorer
AppVerName=XBE Explorer 1.0
AppPublisher=PatrickvL
DefaultDirName={pf}\DXBX
DefaultGroupName=DXBX
OutputDir=..
OutputBaseFilename=XBEExplorer_Setup
SetupIconFile=..\resource\Cxbx.ico
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
Source: "..\bin\XBEExplorer.exe"; DestDir: "{app}"; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\XBEExplorer"; Filename: "{app}\XBEExplorer.exe"
Name: "{commondesktop}\XBEExplorer"; Filename: "{app}\XBEExplorer.exe"; Tasks: desktopicon
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\DXBX"; Filename: "{app}\XBEExplorer.exe"; Tasks: quicklaunchicon

[Run]
Filename: "{app}\XBEExplorer.exe"; Description: "{cm:LaunchProgram,XBE Explorer}"; Flags: nowait postinstall skipifsilent

