program XBEExplorer;

uses
  Forms,
  SysUtils,
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
  uXDVDFS in '..\..\src\uXDVDFS.pas',
  uFileSystem in '..\..\src\uFileSystem.pas',
  uExploreFileSystem in '..\..\src\XBEExplorer\uExploreFileSystem.pas' {frmExploreFileSystem};

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

