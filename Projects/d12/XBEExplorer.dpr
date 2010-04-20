program XBEExplorer;

uses
  Forms,
  uXBEExplorerMain in '..\..\src\XBEExplorer\uXBEExplorerMain.pas' {FormXBEExplorer},
  uXbe in '..\..\src\uXbe.pas',
  uTypes in '..\..\src\uTypes.pas',
  uConsts in '..\..\src\uConsts.pas',
  uDxbxUtils in '..\..\src\uDxbxUtils.pas',
  uLog in '..\..\src\uLog.pas',
  uTime in '..\..\src\uTime.pas',
  uEmuD3D8Types in '..\..\src\DxbxKrnl\uEmuD3D8Types.pas',
  uConsoleClass in '..\..\src\uConsoleClass.pas',
  uHexViewer in '..\..\src\XBEExplorer\uHexViewer.pas' {HexViewer: TFrame},
  uStringsViewer in '..\..\src\XBEExplorer\uStringsViewer.pas',
  uFileSystem in '..\..\src\uFileSystem.pas',
  uXDVDFS in '..\..\src\uXDVDFS.pas',
  uExploreFileSystem in '..\..\src\XBEExplorer\uExploreFileSystem.pas' {frmExploreFileSystem},
  uViewerUtils in '..\..\src\XBEExplorer\uViewerUtils.pas',
  uRegionViewer in '..\..\src\XBEExplorer\uRegionViewer.pas',
  uDisassembleViewer in '..\..\src\XBEExplorer\uDisassembleViewer.pas',
  BeaEngine in '..\..\Libraries\BeaEngine\BeaEngine.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormXBEExplorer, FormXBEExplorer);
  Application.CreateForm(TfrmExploreFileSystem, frmExploreFileSystem);
  Application.Title := FormXBEExplorer.Caption;
  if ParamCount > 0 then
    FormXBEExplorer.OpenFile(ParamStr(1));
  Application.Run;
end.

