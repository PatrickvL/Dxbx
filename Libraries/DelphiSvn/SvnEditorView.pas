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
{ The Original Code is SvnEditorView.pas.                                                                              }
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
{ This unit contains TSvnEditorView, a class which implements ICustomEditorView and ICustomEditorFrameView interfaces  }
{ for Delphi 2009 (and lower) and implements the INTACustomEditorSubView interface for Delphi 2010 (and higher)        }
{ to provide a new editor view displaying Subversion information about the file.                                       }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnEditorView;

interface

{$INCLUDE Compilers.inc}

uses
  {$IFNDEF COMPILER_14_UP}
  EditorViewSupport,
  {$ENDIF}
  Classes, SysUtils, Forms,
  ToolsAPI, DesignIntf;

type
  {$IFDEF COMPILER_14_UP}
  TSvnEditorView = class(TInterfacedObject, INTACustomEditorSubView)
  {$ELSE}
  TSvnEditorView = class(TInterfacedObject, ICustomEditorView, ICustomEditorFrameView)
  {$ENDIF}
  private
    { ICustomEditorView / INTACustomEditorSubView}
    function GetCaption: string;
    function GetPriority: Integer;
    {$IFNDEF COMPILER_14_UP}
    function GetStyle: TEditorViewStyle;
    {$ENDIF}
    procedure Display(const AContext: IInterface; AViewObject: TObject);
    function EditAction(const AContext: IInterface; Action: TEditAction; AViewObject: TObject): Boolean;
    function GetEditState(const AContext: IInterface; AViewObject: TObject): TEditState;
    function Handles(const AContext: IInterface): Boolean;
    {$IFDEF COMPILER_9_UP}
    function GetCanCloneView: Boolean;
    function GetViewIdentifier: string;
    procedure Hide(const AContext: IInterface; AViewObject: TObject);
    procedure ViewClosed(const AContext: IInterface; AViewObject: TObject);
    {$ENDIF}

    { ICustomEditorFrameView / INTACustomEditorSubView}
    function GetFrameClass: TCustomFrameClass;

    { INTACustomEditorSubView }
    {$IFDEF COMPILER_14_UP}
    procedure FrameCreated(AFrame: TCustomFrame);
    {$ENDIF}
  end;

implementation

uses
  SvnClient, SvnIDEClient, SvnEditorViewFrame;

{$IFDEF COMPILER_14_UP}
function ContextToModule(const AContext: IInterface; out AModule: IOTAModule): Boolean;

var
  EditorViewServices: IOTAEditorViewServices;

begin
  if BorlandIDEServices.GetService(IOTAEditorViewServices, EditorViewServices) then
    Result := EditorViewServices.ContextToModule(AContext, AModule)
  else
    Result := False;
end;
{$ENDIF}

//----------------------------------------------------------------------------------------------------------------------

{ TSvnEditorView private: ICustomEditorView / INTACustomEditorSubView }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnEditorView.Display(const AContext: IInterface; AViewObject: TObject);

var
  Module: IOTAModule;
  Items: TSvnItemArray;
  I: Integer;

begin
  if ContextToModule(AContext, Module) and Assigned(Module) then
  begin
    if AViewObject is TFrameSvnEditorView then
    begin
      SetLength(Items, Module.ModuleFileCount);
      for I := 0 to Module.ModuleFileCount - 1 do
        Items[I] := TSvnItem.Create(SvnIDEModule.SvnClient, nil, Module.ModuleFileEditors[I].FileName);
      TFrameSvnEditorView(AViewObject).Display(Items);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnEditorView.EditAction(const AContext: IInterface; Action: TEditAction; AViewObject: TObject): Boolean;

begin
  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnEditorView.GetCaption: string;

begin
  Result := 'Subversion';
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnEditorView.GetEditState(const AContext: IInterface; AViewObject: TObject): TEditState;

begin
  Result := [];
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnEditorView.GetPriority: Integer;

begin
  Result := {$IFDEF COMPILER_14_UP}svpNormal {$ELSE}NormalViewPriority {$ENDIF};
end;

//----------------------------------------------------------------------------------------------------------------------

{$IFNDEF COMPILER_14_UP}
function TSvnEditorView.GetStyle: TEditorViewStyle;

begin
  Result := [evsDesigntime];
end;
{$ENDIF}

//----------------------------------------------------------------------------------------------------------------------

function TSvnEditorView.Handles(const AContext: IInterface): Boolean;

var
  Module: IOTAModule;

begin
  Result := False;

  if ContextToModule(AContext, Module) and Assigned(Module) then
    Result := SvnIDEModule.SvnClient.IsPathVersioned(Module.FileName);
end;

{$IFDEF COMPILER_9_UP}
//----------------------------------------------------------------------------------------------------------------------

function TSvnEditorView.GetCanCloneView: Boolean;

begin
  Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnEditorView.GetViewIdentifier: string;

begin
  Result := 'TOndrej.SubversionView';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnEditorView.Hide(const AContext: IInterface; AViewObject: TObject);

begin
  if AViewObject is TFrameSvnEditorView then
    TFrameSvnEditorView(AViewObject).FreeItems;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnEditorView.ViewClosed(const AContext: IInterface; AViewObject: TObject);

begin
  if AViewObject is TFrameSvnEditorView then
    TFrameSvnEditorView(AViewObject).Clear;
end;
{$ENDIF}

//----------------------------------------------------------------------------------------------------------------------

{ TSvnEditorView private: ICustomEditorFrameView / INTACustomEditorSubView }

//----------------------------------------------------------------------------------------------------------------------

function TSvnEditorView.GetFrameClass: TCustomFrameClass;

begin
  Result := TFrameSvnEditorView;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnEditorView private: INTACustomEditorSubView }

//----------------------------------------------------------------------------------------------------------------------

{$IFDEF COMPILER_14_UP}
procedure TSvnEditorView.FrameCreated(AFrame: TCustomFrame);

begin

end;
{$ENDIF}

//----------------------------------------------------------------------------------------------------------------------

end.
