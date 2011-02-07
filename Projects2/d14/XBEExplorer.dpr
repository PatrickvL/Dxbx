program XBEExplorer;

{$R 'XBEExplorerResources.res' '..\..\resource\XBEExplorerResources.rc'}

uses
  Forms,
  SysUtils,
  uXBEExplorerMain in '..\..\src2\XBEExplorer\uXBEExplorerMain.pas' {FormXBEExplorer},
  uXbe in '..\..\src2\uXbe.pas',
  uTypes in '..\..\src2\uTypes.pas',
  uConsts in '..\..\src2\uConsts.pas',
  uDxbxUtils in '..\..\src2\uDxbxUtils.pas',
  uLog in '..\..\src2\uLog.pas',
  uTime in '..\..\src2\uTime.pas',
  uEmuD3D8Types in '..\..\src2\DxbxKrnl\uEmuD3D8Types.pas',
  uConsoleClass in '..\..\src2\uConsoleClass.pas',
  uHexViewer in '..\..\src2\XBEExplorer\uHexViewer.pas' {HexViewer: TFrame},
  uStringsViewer in '..\..\src2\XBEExplorer\uStringsViewer.pas',
  uXDVDFS in '..\..\src2\uXDVDFS.pas',
  uFileSystem in '..\..\src2\uFileSystem.pas',
  uExploreFileSystem in '..\..\src2\XBEExplorer\uExploreFileSystem.pas' {frmExploreFileSystem},
  uDisassembleViewer in '..\..\src2\XBEExplorer\uDisassembleViewer.pas',
  BeaEngine in '..\..\Libraries\BeaEngine\BeaEngine.pas',
  uSectionViewer in '..\..\src2\XBEExplorer\uSectionViewer.pas',
  uViewerUtils in '..\..\src2\XBEExplorer\uViewerUtils.pas',
  uDisassembleUtils in '..\..\src2\uDisassembleUtils.pas',
  VistaIconFix in '..\..\src2\VistaIconFix.pas',
  uCRC16 in '..\..\src2\uCRC16.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormXBEExplorer, FormXBEExplorer);
  Application.Title := FormXBEExplorer.Caption;
  if ParamCount > 0 then
    if FileExists(ParamStr(1)) then
      FormXBEExplorer.OpenFile(ParamStr(1));

  Application.Run;
end.

