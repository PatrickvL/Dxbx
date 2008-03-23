unit uGuiDbgConsole;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uLogConsole, StdCtrls, sSkinProvider, sSkinManager;

type
  Tfrm_GuiDbgLog = class(Tfrm_LogConsole)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frm_GuiDbgLog: Tfrm_GuiDbgLog;

implementation

{$R *.dfm}

end.
