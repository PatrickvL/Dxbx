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
{ The Original Code is SvnLogMessagePrompt.pas.                                                                        }
{                                                                                                                      }
{ The Initial Developer of the Original Code is Ondrej Kelle.                                                          }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights reserved.                                    }
{                                                                                                                      }
{ Contributors:                                                                                                        }
{   Ondrej Kelle (tondrej)                                                                                             }
{                                                                                                                      }
{**********************************************************************************************************************}
{                                                                                                                      }
{ This unit contains a Subversion log message prompt dialog.                                                           }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnLogMessagePrompt;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TFormSvnLogMessagePrompt = class(TForm)
    ButtonCancel: TButton;
    ButtonOK: TButton;
    LabelPrompt: TLabel;
    MemoLogMessage: TMemo;

    procedure FormCreate(Sender: TObject);
    procedure MemoLogMessageChange(Sender: TObject);
  private
  public
  end;

function ShowSvnLogMessagePrompt(const Prompt: string; var LogMessage: string): TModalResult;

implementation

uses
  SvnImages, SvnIDEClient;

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

function ShowSvnLogMessagePrompt(const Prompt: string; var LogMessage: string): TModalResult;

var
  Form: TFormSvnLogMessagePrompt;

begin
  Form := TFormSvnLogMessagePrompt.Create(nil);
  try
    Form.LabelPrompt.Caption := Prompt;
    Form.MemoLogMessage.Text := LogMessage;
    Form.MemoLogMessageChange(Form.MemoLogMessage);
    Result := Form.ShowModal;
    if Result = mrOK then
      LogMessage := Form.MemoLogMessage.Text;
  finally
    Form.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFormSvnLogMessage event handlers }

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnLogMessagePrompt.FormCreate(Sender: TObject);

begin
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
  Icon := SvnImageModule.Icon;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnLogMessagePrompt.MemoLogMessageChange(Sender: TObject);

begin
  if not SvnIDEModule.Settings.AllowEmptyCommitMsg then
    ButtonOK.Enabled := MemoLogMessage.Text <> '';
end;

//----------------------------------------------------------------------------------------------------------------------

end.
