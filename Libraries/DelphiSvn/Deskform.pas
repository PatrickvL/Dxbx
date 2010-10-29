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
{ The Original Code is Deskform.pas.                                                                                   }
{                                                                                                                      }
{ The Initial Developer of the Original Code is Ondrej Kelle.                                                          }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights reserved.                                    }
{                                                                                                                      }
{ Contributors:                                                                                                        }
{   Ondrej Kelle (tondrej)                                                                                             }
{                                                                                                                      }
{**********************************************************************************************************************}
{                                                                                                                      }
{ This is a stub unit for TDesktopForm which is contained in designide100.bpl. It's intended purpose is only to be     }
{ able to open and design descendant forms within the IDE.                                                             }
{                                                                                                                      }
{**********************************************************************************************************************}

unit Deskform;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs;

type
  TDesktopForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormEndDock(Sender, Target: TObject; X, Y: Integer);
  private
  public
  end;

implementation

{$R *.dfm}

procedure TDesktopForm.FormCreate(Sender: TObject);
begin
  //
end;

procedure TDesktopForm.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TDesktopForm.FormEndDock(Sender, Target: TObject; X, Y: Integer);
begin
  //
end;

end.
