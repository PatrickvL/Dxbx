{**********************************************************************************************************************}
{                                                                                                                      }
{ delphisvn: Subversion plugin for CodeGear Delphi                                                                     }
{                                                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); you may not use     }
{ this file except in compliance with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either      }
{ express or implied. See the License for the specific language governing rights and limitations under the License.    }
{                                                                                                                      }
{ The Original Code is SvnBaseFrame.pas.                                                                               }
{                                                                                                                      }
{ The Initial Developer of the Original Code is Ondrej Kelle.                                                          }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights reserved.                                    }
{                                                                                                                      }
{ Contributors:                                                                                                        }
{   Ondrej Kelle (tondrej)                                                                                             }
{                                                                                                                      }
{**********************************************************************************************************************}
{                                                                                                                      }
{ This unit contains a base frame class for descendants which provide visual feedback from Subversion calls running in }
{ secondary threads.                                                                                                   }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnBaseFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ActnList;

const
  AM_UPDATE = WM_USER + $100;

type
  TFrameSvnBase = class(TFrame)
  private
    FCancelled: Boolean;
    FRunning: Boolean;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Cancel;
    procedure Finished;
    procedure HandleAddExecute(Action: TAction); virtual;
    procedure HandleAddUpdate(Action: TAction); virtual;
    procedure HandleMergeConflictsExecute(Action: TAction); virtual;
    procedure HandleMergeConflictsUpdate(Action: TAction); virtual;
    procedure HandleOpenExecute(Action: TAction); virtual;
    procedure HandleOpenUpdate(Action: TAction); virtual;
    procedure HandleShowBlameExecute(Action: TAction); virtual;
    procedure HandleShowBlameUpdate(Action: TAction); virtual;
    procedure HandleShowDiffExecute(Action: TAction); virtual;
    procedure HandleShowDiffUpdate(Action: TAction); virtual;
    procedure HandleShowUnversionedExecute(Action: TAction); virtual;
    procedure HandleShowUnversionedUpdate(Action: TAction); virtual;
    function IsRunning: Boolean;
    procedure Starting;

    property Cancelled: Boolean read FCancelled;
    property Running: Boolean read FRunning;
  end;

implementation

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

{ TFrameSvnBase public }

//----------------------------------------------------------------------------------------------------------------------

constructor TFrameSvnBase.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  FCancelled := False;
  FRunning := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnBase.Cancel;

begin
  FCancelled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnBase.Finished;

begin
  FRunning := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnBase.HandleAddExecute(Action: TAction);

begin

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnBase.HandleAddUpdate(Action: TAction);

begin

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnBase.HandleMergeConflictsExecute(Action: TAction);

begin

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnBase.HandleMergeConflictsUpdate(Action: TAction);

begin

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnBase.HandleOpenExecute(Action: TAction);

begin

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnBase.HandleOpenUpdate(Action: TAction);

begin
  Action.Enabled := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnBase.HandleShowBlameExecute(Action: TAction);

begin

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnBase.HandleShowBlameUpdate(Action: TAction);

begin
  Action.Enabled := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnBase.HandleShowDiffExecute(Action: TAction);

begin

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnBase.HandleShowDiffUpdate(Action: TAction);

begin
  Action.Enabled := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnBase.HandleShowUnversionedExecute(Action: TAction);

begin

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnBase.HandleShowUnversionedUpdate(Action: TAction);

begin
  Action.Visible := False;
  Action.Enabled := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TFrameSvnBase.IsRunning: Boolean;

begin
  Result := not FRunning;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnBase.Starting;

begin
  FCancelled := False;
  FRunning := True;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
