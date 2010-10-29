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
{ The Original Code is Dockform.pas.                                                                                   }
{                                                                                                                      }
{ The Initial Developer of the Original Code is Ondrej Kelle.                                                          }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights reserved.                                    }
{                                                                                                                      }
{ Contributors:                                                                                                        }
{   Ondrej Kelle (tondrej)                                                                                             }
{                                                                                                                      }
{**********************************************************************************************************************}
{                                                                                                                      }
{ This is a stub unit for TDockableForm which is contained in designide100.bpl. It's intended purpose is only to be    }
{ able to open and design descendant forms within the IDE.                                                             }
{                                                                                                                      }
{**********************************************************************************************************************}

unit Dockform;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, 
  ActnList,
  Deskform;

type
  TDockableForm = class(TDesktopForm)
    DockActionList: TActionList;
    DockableCmd: TAction;
    StayOnTopCmd: TAction;
    ZoomWindowCmd: TAction;
    procedure FormGetSiteInfo(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean);
    procedure DockableCmdExecute(Sender: TObject);
    procedure DockableCmdUpdate(Sender: TObject);
    procedure StayOnTopCmdExecute(Sender: TObject);
    procedure StayOnTopCmdUpdate(Sender: TObject);
    procedure ZoomWindowCmdExecute(Sender: TObject);
  private
  public
  end;

implementation

{$R *.dfm}

procedure TDockableForm.FormGetSiteInfo(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect;
  MousePos: TPoint; var CanDock: Boolean);

begin
  //
end;

procedure TDockableForm.DockableCmdExecute(Sender: TObject);

begin
  //
end;

procedure TDockableForm.DockableCmdUpdate(Sender: TObject);

begin
  //
end;

procedure TDockableForm.StayOnTopCmdExecute(Sender: TObject);

begin
  //
end;

procedure TDockableForm.StayOnTopCmdUpdate(Sender: TObject);

begin
  //
end;

procedure TDockableForm.ZoomWindowCmdExecute(Sender: TObject);

begin
  //
end;

end.
