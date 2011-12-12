program XBEExplorer;

{$R 'XBEExplorerResources.res' '..\..\..\resources\XBEExplorerResources.rc'}

uses
  Forms,
  SysUtils,
  BeaEngine in '..\..\..\Source\Delphi\Libraries\BeaEngine\BeaEngine.pas',
  uDisassembleViewer in '..\..\..\Source\Delphi\src\Tools\XBEExplorer\uDisassembleViewer.pas',
  uExploreFileSystem in '..\..\..\Source\Delphi\src\Tools\XBEExplorer\uExploreFileSystem.pas' {frmExploreFileSystem},
  uHexViewer in '..\..\..\Source\Delphi\src\Tools\XBEExplorer\uHexViewer.pas',
  uSectionViewer in '..\..\..\Source\Delphi\src\Tools\XBEExplorer\uSectionViewer.pas',
  uStringsViewer in '..\..\..\Source\Delphi\src\Tools\XBEExplorer\uStringsViewer.pas',
  uViewerUtils in '..\..\..\Source\Delphi\src\Tools\XBEExplorer\uViewerUtils.pas',
  uXBEExplorerMain in '..\..\..\Source\Delphi\src\Tools\XBEExplorer\uXBEExplorerMain.pas' {FormXBEExplorer},
  uConsoleClass in '..\..\..\Source\Delphi\src\uConsoleClass.pas',
  uConsts in '..\..\..\Source\Delphi\src\uConsts.pas',
  uCRC16 in '..\..\..\Source\Delphi\src\uCRC16.pas',
  uDisassembleUtils in '..\..\..\Source\Delphi\src\uDisassembleUtils.pas',
  uDxbxUtils in '..\..\..\Source\Delphi\src\uDxbxUtils.pas',
  uFileSystem in '..\..\..\Source\Delphi\src\uFileSystem.pas',
  uLog in '..\..\..\Source\Delphi\src\uLog.pas',
  uTime in '..\..\..\Source\Delphi\src\uTime.pas',
  uTypes in '..\..\..\Source\Delphi\src\uTypes.pas',
  uXbe in '..\..\..\Source\Delphi\src\uXbe.pas',
  uXDVDFS in '..\..\..\Source\Delphi\src\uXDVDFS.pas',
  uEmuD3D8Types in '..\..\..\Source\Delphi\src\DxbxKrnl\uEmuD3D8Types.pas',
  VistaIconFix in '..\..\..\Source\Delphi\src\VistaIconFix.pas',
  XbeHeaders in '..\..\..\Source\Delphi\src\Headers\XbeHeaders.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := FormXBEExplorer.Caption;
  Application.CreateForm(TFormXBEExplorer, FormXBEExplorer);
  Application.CreateForm(TfrmExploreFileSystem, frmExploreFileSystem);
  if ParamCount > 0 then
    if FileExists(ParamStr(1)) then
      FormXBEExplorer.OpenFile(ParamStr(1));

  Application.Run;
end.

