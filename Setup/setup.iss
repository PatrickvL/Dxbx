[Setup]
AppName=DXBX
AppVerName=DXBX 0.4 Release
AppPublisher=Shadow_tj, Patrick
DefaultDirName={pf}\DXBX
DefaultGroupName=DXBX
OutputDir=..
OutputBaseFilename=setup
SetupIconFile=..\resource\Dxbx.ico
Compression=lzma
SolidCompression=yes


[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "dutch"; MessagesFile: "compiler:Languages\Dutch.isl"
Name: "french"; MessagesFile: "compiler:Languages\French.isl"
Name: "german"; MessagesFile: "compiler:Languages\German.isl"
Name: "spanish"; MessagesFile: "compiler:Languages\Spanish.isl"

[Types]
Name: "compact"; Description: "Compact installation"
Name: "full"; Description: "Full installation"
Name: "custom"; Description: "Custom installation"; Flags: iscustom

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Components]
Name: "Main"; Description: "Main Files"; Types: full compact custom; Flags: fixed
Name: "XBEExplorer"; Description: "XBE Explorer"; Types: full custom
Name: "XDKTracker"; Description: "XDK Tracker"; Types: full custom
Name: "XIso"; Description: "XIso"; Types: full custom

[Files]
Source: "..\bin\Dxbx.exe"; DestDir: "{app}"; Flags: ignoreversion; Components: main
Source: "..\bin\DxbxKrnl.dll"; DestDir: "{app}"; Flags: ignoreversion; Components: main
Source: "..\bin\XBEExplorer.exe"; DestDir: "{app}"; Flags: ignoreversion; Components: XBEExplorer
Source: "..\bin\XdkTracker.exe"; DestDir: "{app}"; Flags: ignoreversion; Components: XDKTracker
Source: "..\bin\XIso.exe"; DestDir: "{app}"; Flags: ignoreversion; Components: XIso 

Source: "..\Libraries\D3DX81ab.dll"; DestDir: "{app}"; Flags: ignoreversion; Components: main
Source: "..\Libraries\msvcrtd.dll"; DestDir: "{app}"; Flags: ignoreversion; Components: main

Source: "..\resource\StoredTrie.dpt"; DestDir: "{app}"; Flags: ignoreversion; Components: main

Source: "..\Readme.txt"; DestDir: "{app}";  Flags: isreadme; Components: main
Source: "..\Changelog.txt"; DestDir: "{app}"; Flags: isreadme; Components: main
Source: "..\Compatibility list.txt"; DestDir: "{app}"; Flags: isreadme; Components: main

Source: "..\bin\Tools\*"; DestDir: "{app}\Tools"; Flags: ignoreversion recursesubdirs createallsubdirs; Components: main
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\DXBX"; Filename: "{app}\Dxbx.exe"; Components: main
Name: "{group}\XBEExplorer"; Filename: "{app}\XBEExplorer.exe"; Components: XBEExplorer
Name: "{group}\XdkTracker"; Filename: "{app}\XdkTracker.exe"; Components: XDKTracker
Name: "{group}\XIso"; Filename: "{app}\XIso.exe"; Components: XIso 
Name: "{group}\{cm:UninstallProgram,DXBX}"; Filename: "{uninstallexe}"; Components: main
Name: "{commondesktop}\DXBX"; Filename: "{app}\Dxbx.exe"; Tasks: desktopicon; Components: main
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\DXBX"; Filename: "{app}\Dxbx.exe"; Tasks: quicklaunchicon; Components: main

[Run]
Filename: "{app}\Dxbx.exe"; Description: "{cm:LaunchProgram,DXBX}"; Flags: nowait postinstall skipifsilent

