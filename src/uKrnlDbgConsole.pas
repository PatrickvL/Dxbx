unit uKrnlDbgConsole;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uLogConsole, StdCtrls;

type
  Tfrm_KnlDbgLog = class(Tfrm_LogConsole)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frm_KnlDbgLog: Tfrm_KnlDbgLog;

implementation

Uses uGuiDbgConsole;

{$R *.dfm}

procedure Tfrm_KnlDbgLog.FormCreate(Sender: TObject);
begin
  inherited;
  frm_GuiDbgLog := Tfrm_GuiDbgLog.create ( Nil );
  frm_GuiDbgLog.Left := Width + Left;
  frm_GuiDbgLog.Top := Top;
  frm_GuiDbgLog.Show;
end;

end.
