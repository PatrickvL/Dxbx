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
{ The Original Code is Docktoolform.pas.                                                                               }
{                                                                                                                      }
{ The Initial Developer of the Original Code is Ondrej Kelle.                                                          }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights reserved.                                    }
{                                                                                                                      }
{ Contributors:                                                                                                        }
{   Ondrej Kelle (tondrej)                                                                                             }
{                                                                                                                      }
{**********************************************************************************************************************}
{                                                                                                                      }
{ This is a stub unit for TDockableToolbarForm which is contained in designide100.bpl. It's intended purpose is only   }
{ to be able to open and design descendant forms within the IDE.                                                       }
{                                                                                                                      }
{**********************************************************************************************************************}

unit Docktoolform;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, ExtCtrls,
  Menus, ToolWin, ActnList, ActnPopup,
  Deskform, Dockform;

type
  TDockableToolbarForm = class(TDockableForm)
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    ToolbarPopupMenu: TPopupActionBar;
    TextLabels1: TMenuItem;
    ToolActionList: TActionList;
    ToolbarCmd: TAction;
    TextLabelsCmd: TAction;
    PopupMenu1: TPopupActionBar;
    Toolbar2: TMenuItem;
    
    procedure Splitter1CanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
    procedure Splitter1Moved(Sender: TObject);
    procedure ToolbarCmdExecute(Sender: TObject);
    procedure ToolbarCmdUpdate(Sender: TObject);
    procedure TextLabelsCmdExecute(Sender: TObject);
    procedure TextLabelsCmdUpdate(Sender: TObject);
  private
  public
  end;

implementation

{$R *.dfm}

procedure TDockableToolbarForm.Splitter1CanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);

begin
  //
end;

procedure TDockableToolbarForm.Splitter1Moved(Sender: TObject);

begin
  //
end;

procedure TDockableToolbarForm.ToolbarCmdExecute(Sender: TObject);

begin
  //
end;

procedure TDockableToolbarForm.ToolbarCmdUpdate(Sender: TObject);

begin
  //
end;

procedure TDockableToolbarForm.TextLabelsCmdExecute(Sender: TObject);

begin
  //
end;

procedure TDockableToolbarForm.TextLabelsCmdUpdate(Sender: TObject);

begin
  //
end;

end.
