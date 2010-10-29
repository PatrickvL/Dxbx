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
{ The Original Code is SvnOptionsDialog.pas.                                                                           }
{                                                                                                                      }
{ The Initial Developer of the Original Code is Ondrej Kelle.                                                          }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights reserved.                                    }
{                                                                                                                      }
{ Contributors:                                                                                                        }
{   Ondrej Kelle (tondrej)                                                                                             }
{   Uwe Schuster (uschuster)                                                                                           }
{                                                                                                                      }
{**********************************************************************************************************************}
{                                                                                                                      }
{ This unit contains a modal dialog which provides user interface to configure Subversion plugin options.              }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnOptionsDialog;

interface

{$INCLUDE Compilers.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  SvnIDEClient;

type
  TFormSvnOptions = class(TForm)
    ButtonBrowse: TButton;
    ButtonCancel: TButton;
    ButtonOK: TButton;
    CheckBoxAllowEmptyCommitMsg: TCheckBox;
    CheckBoxCommitExternals: TCheckBox;
    CheckBoxConfirmAdd: TCheckBox;
    CheckBoxRecurseUnversioned: TCheckBox;
    ComboBoxDirs: TComboBox;
    LabelDirs: TLabel;

    procedure ButtonBrowseClick(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
    procedure ComboBoxDirsChange(Sender: TObject);
  private
    FLoading: Boolean;
  public
  end;

function ShowSvnOptionsDialog(Settings: TSvnIDESettings): TModalResult;

implementation

uses
  FileCtrl,
  SvnImages;

{$R *.dfm}

//----------------------------------------------------------------------------------------------------------------------

function ShowSvnOptionsDialog(Settings: TSvnIDESettings): TModalResult;

var
  Form: TFormSvnOptions;

begin
  Form := TFormSvnOptions.Create(nil);
  try
    Form.FLoading := True;
    try
      Form.Icon := SvnImageModule.Icon;
      Form.ComboBoxDirs.Items.Assign(Settings.DirHistory);
      Form.ComboBoxDirs.Text := Settings.Directories;
      Form.CheckBoxConfirmAdd.Checked := Settings.ConfirmAdd;
      Form.CheckBoxAllowEmptyCommitMsg.Checked := Settings.AllowEmptyCommitMsg;
      Form.CheckBoxCommitExternals.Checked := Settings.CommitExternals;
      Form.CheckBoxRecurseUnversioned.Checked := Settings.RecurseUnversioned;
    finally
      Form.FLoading := False;
    end;

    Result := Form.ShowModal;

    if Result = mrOK then
    begin
      Settings.Directories := Form.ComboBoxDirs.Text;
      Settings.ConfirmAdd := Form.CheckBoxConfirmAdd.Checked;
      Settings.AllowEmptyCommitMsg := Form.CheckBoxAllowEmptyCommitMsg.Checked;
      Settings.CommitExternals := Form.CheckBoxCommitExternals.Checked;
      Settings.RecurseUnversioned := Form.CheckBoxRecurseUnversioned.Checked;
    end;
  finally
    Form.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFormSvnOptions event handlers }

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnOptions.ButtonBrowseClick(Sender: TObject);

var
  Lib: THandle;
  TOrderedListEditDlg: TCustomFormClass;
  Dlg: TCustomForm;
  Label1: TLabel;
  OKButton, CancelButton, HelpButton: TButton;
  ListBox: TListBox;

begin
  {$IFDEF COMPILER_14}
  Lib := GetModuleHandle('coreide140.bpl');
  {$ELSE} {$IFDEF COMPILER_12}
  Lib := GetModuleHandle('coreide120.bpl');
  {$ELSE} {$IFDEF COMPILER_10}
  Lib := GetModuleHandle('coreide100.bpl');
  {$ELSE} {$IFDEF COMPILER_9}
  Lib := GetModuleHandle('coreide90.bpl');
  {$ELSE} {$IFDEF COMPILER_7}
  Lib := GetModuleHandle('coreide70.bpl');
  {$ELSE}
  Lib := 0;
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  if Lib = 0 then
    Exit;
  TOrderedListEditDlg := GetProcAddress(Lib, '@Orderedlisteditdialog@TOrderedListEditDlg@');
  if not Assigned(TOrderedListEditDlg) then
    Exit;

  Dlg := TOrderedListEditDlg.Create(nil);
  try
    Dlg.Caption := 'Subversion Directories';

    Label1 := Dlg.FindComponent('Label1') as TLabel;
    if Assigned(Label1) then
      Label1.Caption := 'Working copy directories:';

    ListBox := Dlg.FindComponent('CreationList') as TListBox;
    if not Assigned(ListBox) then
      Exit;
    ListBox.Items.Delimiter := ';';
    {$IFDEF COMPILER_10_UP}
    ListBox.Items.StrictDelimiter := True;
    {$ENDIF}
    ListBox.Items.DelimitedText := ComboBoxDirs.Text;
    ListBox.Style := lbOwnerDrawFixed;

    // No help is available; hide help button and move OK/Cancel buttons to the right
    HelpButton := Dlg.FindComponent('HelpButton') as TButton;
    CancelButton := Dlg.FindComponent('CancelButton') as TButton;
    OKButton := Dlg.FindComponent('OKButton') as TButton;
    if Assigned(HelpButton) and Assigned(OKButton) and Assigned(CancelButton) then
    begin
      HelpButton.Visible := False;
      OKButton.Left := CancelButton.Left;
      CancelButton.Left := HelpButton.Left;
    end;

    if Dlg.ShowModal = mrOK then
    begin
      ComboBoxDirs.Text := ListBox.Items.DelimitedText;
      ComboBoxDirsChange(ComboBoxDirs);
    end;
  finally
    Dlg.Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnOptions.CheckBoxClick(Sender: TObject);

begin
  if not FLoading then
    ButtonOK.Enabled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TFormSvnOptions.ComboBoxDirsChange(Sender: TObject);

begin
  ButtonOK.Enabled := True;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
