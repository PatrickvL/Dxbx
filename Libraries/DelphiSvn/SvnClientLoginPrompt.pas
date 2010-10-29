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
{ The Original Code is SvnClientLoginPrompt.pas.                                                                       }
{                                                                                                                      }
{ The Initial Developer of the Original Code is Ondrej Kelle.                                                          }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights reserved.                                    }
{                                                                                                                      }
{ Contributors:                                                                                                        }
{   Ondrej Kelle (tondrej)                                                                                             }
{                                                                                                                      }
{**********************************************************************************************************************}
{                                                                                                                      }
{ This unit contains login prompt dialog for a Subversion client.                                                      }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnClientLoginPrompt;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  SvnClient;

type
  TLoginPromptOptions = set of (lpoUserName, lpoPassword);
  
  TFormSvnClientLoginPrompt = class(TForm)
    ButtonCancel: TButton;
    ButtonOK: TButton;
    CheckBoxSave: TCheckBox;
    EditPassword: TEdit;
    EditUserName: TEdit;
    ImageLogo: TImage;
    LabelPassword: TLabel;
    LabelRealm: TLabel;
    LabelUserName: TLabel;

    procedure FormCreate(Sender: TObject);

    procedure EditChange(Sender: TObject);
  private
    FOptions: TLoginPromptOptions;
  public
  end;

function ShowSvnClientLoginPrompt(SvnClient: TSvnClient; const Realm: string;
  var UserName, Password: string; var Save: Boolean; Options: TLoginPromptOptions = []): TModalResult;
function ShowSvnClientSSLClientPasswordPrompt(SvnClient: TSvnClient; const Realm: string;
  var Password: string; var Save: Boolean): TModalResult;

implementation

uses
  SvnImages;

{$R *.dfm}

resourcestring
  SLoginCaption = 'Please login to Subversion server.';
  SLoginRealmCaption = 'Please login to Subversion server:'#13#10'%s';

//----------------------------------------------------------------------------------------------------------------------

function ShowSvnClientLoginPrompt(SvnClient: TSvnClient; const Realm: string;
  var UserName, Password: string; var Save: Boolean; Options: TLoginPromptOptions = []): TModalResult;

var
  Form: TFormSvnClientLoginPrompt;

begin
  if Options = [] then
    Options := [lpoUserName, lpoPassword];

  Form := TFormSvnClientLoginPrompt.Create(nil);
  try
    Form.FOptions := Options;
    if Realm = '' then
      Form.LabelRealm.Caption := SLoginCaption
    else
      Form.LabelRealm.Caption := Format(SLoginRealmCaption, [Realm]);

    if not (lpoUserName in Options) then
    begin
      Form.LabelUserName.Visible := False;
      Form.EditUserName.Visible := False;
      Form.LabelPassword.Top := Form.LabelUserName.Top;
      Form.EditPassword.Top := Form.EditUserName.Top;
    end
    else if not (lpoPassword in Options) then
    begin
      Form.LabelPassword.Visible := False;
      Form.EditPassword.Visible := False;
    end;

    if lpoUserName in Options then
      Form.EditUserName.Text := UserName;
    if lpoPassword in Options then
      Form.EditPassword.Text := Password;
    Form.EditChange(Form.EditUserName);
    Form.CheckBoxSave.Checked := Save;

    if Options = [lpoUserName, lpoPassword] then
    begin
      if UserName = '' then
        Form.ActiveControl := Form.EditUserName
      else if Password = '' then
        Form.ActiveControl := Form.EditPassword
      else
        Form.ActiveControl := Form.ButtonOK;
    end
    else if Options = [lpoUserName] then
    begin
      if UserName = '' then
        Form.ActiveControl := Form.EditUserName
      else
        Form.ActiveControl := Form.ButtonOK;
    end
    else if Options = [lpoPassword] then
    begin
      if Password = '' then
        Form.ActiveControl := Form.EditPassword
      else
        Form.ActiveControl := Form.ButtonOK;
    end;

    Result := Form.ShowModal;

    if Result = mrOK then
    begin
      UserName := Form.EditUserName.Text;
      Save := Form.CheckBoxSave.Checked;
      if lpoPassword in Options then
        Password := Form.EditPassword.Text;
    end;
  finally
    Form.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function ShowSvnClientSSLClientPasswordPrompt(SvnClient: TSvnClient; const Realm: string;
  var Password: string; var Save: Boolean): TModalResult;

var
  Form: TFormSvnClientLoginPrompt;

begin
  Form := TFormSvnClientLoginPrompt.Create(nil);
  try
    Form.FOptions := [lpoPassword];
    if Realm = '' then
      Form.LabelRealm.Caption := SLoginCaption
    else
      Form.LabelRealm.Caption := Format(SLoginRealmCaption, [Realm]);
    Form.LabelPassword.Caption := 'SSL Client &Password:';

    Form.LabelUserName.Visible := False;
    Form.EditUserName.Visible := False;
    Form.LabelPassword.Top := Form.LabelUserName.Top;
    Form.EditPassword.Top := Form.EditUserName.Top;

    Form.EditPassword.Text := Password;
    Form.EditChange(Form.EditUserName);
    Form.CheckBoxSave.Checked := Save;

    if Password = '' then
      Form.ActiveControl := Form.EditPassword
    else
      Form.ActiveControl := Form.ButtonOK;

    Result := Form.ShowModal;

    if Result = mrOK then
    begin
      Save := Form.CheckBoxSave.Checked;
      Password := Form.EditPassword.Text;
    end;
  finally
    Form.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFormSvnClientLoginPrompt event handlers }

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnClientLoginPrompt.FormCreate(Sender: TObject);

begin
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
  ImageLogo.Picture.Graphic := SvnImageModule.Logo;
  Icon := SvnImageModule.Icon;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnClientLoginPrompt.EditChange(Sender: TObject);

begin
  ButtonOK.Enabled := (not (lpoUserName in FOptions) or (EditUserName.Text <> ''));
end;

//----------------------------------------------------------------------------------------------------------------------

end.
