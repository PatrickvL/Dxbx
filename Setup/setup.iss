[Setup]
AppName=DXBX
AppVerName=DXBX 0.1 Release
AppPublisher=Shadow_tj, Patrick
DefaultDirName={pf}\DXBX
DefaultGroupName=DXBX
OutputDir=..
OutputBaseFilename=setup
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
Source: "..\bin\Dxbx.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\bin\DxbxKrnl.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\bin\XBEExplorer.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\bin\XdkTracker.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\bin\XIso.exe"; DestDir: "{app}"; Flags: ignoreversion

Source: "..\Libraries\CxbxKrnl.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Libraries\D3DX81ab.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\Libraries\msvcrtd.dll"; DestDir: "{app}"; Flags: ignoreversion

Source: "..\Readme.txt"; DestDir: "{app}";  Flags: isreadme
Source: "..\Changelog.txt"; DestDir: "{app}"; Flags: isreadme

Source: "..\bin\Tools\*"; DestDir: "{app}\Tools"; Flags: ignoreversion recursesubdirs createallsubdirs
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\DXBX"; Filename: "{app}\Dxbx.exe"
Name: "{group}\XBEExplorer"; Filename: "{app}\XBEExplorer.exe"
Name: "{group}\XdkTracker"; Filename: "{app}\XdkTracker.exe"
Name: "{group}\XIso"; Filename: "{app}\XIso.exe"
Name: "{group}\{cm:UninstallProgram,DXBX}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\DXBX"; Filename: "{app}\Dxbx.exe"; Tasks: desktopicon
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\DXBX"; Filename: "{app}\Dxbx.exe"; Tasks: quicklaunchicon

[Run]
Filename: "{app}\Dxbx.exe"; Description: "{cm:LaunchProgram,DXBX}"; Flags: nowait postinstall skipifsilent

