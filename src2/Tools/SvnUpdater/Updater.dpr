program Updater;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  apr in '..\..\..\Libraries\DelphiSvn\apr.pas',
  svn_client in '..\..\..\Libraries\DelphiSvn\svn_client.pas',
  SvnClient in '..\..\..\Libraries\DelphiSvn\SvnClient.pas';


var
  SVNClient: TSvnClient;
  UpdatePath: string;
  Paths: TStringList;


begin
  try
    if ParamStr(1) <> '' then
    begin
      Paths := TStringList.Create;
      Paths.Add(ParamStr(1));
      SVNClient := TSvnClient.Create;
      try
        SvnClient.Update(Paths);
      finally
        FreeAndNil(Paths);
        FreeAndNil(SVNClient);
      end;
    end;


    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
