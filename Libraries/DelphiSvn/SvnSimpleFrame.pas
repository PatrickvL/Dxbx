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
{ The Original Code is SvnSimpleFrame.pas.                                                                             }
{                                                                                                                      }
{ The Initial Developer of the Original Code is Ondrej Kelle.                                                          }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights reserved.                                    }
{                                                                                                                      }
{ Contributors:                                                                                                        }
{   Ondrej Kelle (tondrej)                                                                                             }
{                                                                                                                      }
{**********************************************************************************************************************}
{                                                                                                                      }
{ This unit contains a frame to provide visual feedback from Subversion API calls (which use no callback, e.g.         }
{ svn cleanup) running in separate threads.                                                                            }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnSimpleFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  SvnClient, SvnBaseFrame, StdCtrls;

type
  TFrameSvnSimple = class(TFrameSvnBase)
    LabelStatus: TLabel;
  private
    FException: string;

    procedure AMUpdate(var Message: TMessage); message AM_UPDATE;
  public
    procedure StartCleanup(AClient: TSvnClient; APathNames: TStrings);
  end;

resourcestring
  SCleanupFinished = 'Clean up complete.';
  SCleanupRunning = 'Cleaning up working copy directories...';

implementation

{$R *.dfm}

type
  TSvnCleanupThread = class(TThread)
  private
    FClient: TSvnClient;
    FFrame: TFrameSvnSimple;
    FPathNames: TStrings;
  protected
    procedure Execute; override;
  public
    constructor Create(AFrame: TFrameSvnSimple; AClient: TSvnClient; APathNames: TStrings);
    destructor Destroy; override;
  end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnCleanupThread protected }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnCleanupThread.Execute;

var
  I: Integer;

begin
  try
    for I := 0 to FPathNames.Count - 1 do
    begin
      if Terminated or FFrame.Cancelled then
        Break;

      FClient.Cleanup(FPathNames[I]);
      PostMessage(FFrame.Handle, AM_UPDATE, 1, 0);
    end;
  except
    on E: Exception do
    begin
      FFrame.FException := E.Message;
      PostMessage(FFrame.Handle, AM_UPDATE, 1, 0);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnCleanupThread public }

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnCleanupThread.Create(AFrame: TFrameSvnSimple; AClient: TSvnClient; APathNames: TStrings);

begin
  FFrame := AFrame;
  FClient := AClient;
  FPathNames := TStringList.Create;
  FPathNames.Assign(APathNames);
  inherited Create(False);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSvnCleanupThread.Destroy;

begin
  FPathNames.Free;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFrameSvnSimple private }

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnSimple.AMUpdate(var Message: TMessage);

begin
  inherited;

  if Message.WParam <> 0 then
  begin
    Finished;
    LabelStatus.Caption := SCleanupFinished;
  end;

  if not Running then
    Cursor := crDefault;

  if FException <> '' then
    raise Exception.Create(FException);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFrameSvnSimple public }

//----------------------------------------------------------------------------------------------------------------------

procedure TFrameSvnSimple.StartCleanup(AClient: TSvnClient; APathNames: TStrings);

begin
  Starting;
  FException := '';
  Cursor := crHourGlass;
  LabelStatus.Caption := SCleanupRunning;
  TSvnCleanupThread.Create(Self, AClient, APathNames);
end;

//----------------------------------------------------------------------------------------------------------------------

end.
