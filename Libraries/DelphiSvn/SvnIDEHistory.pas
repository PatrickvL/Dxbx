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
{ The Original Code is SvnIDEHistory.pas.                                                                              }
{                                                                                                                      }
{ The Initial Developer of the Original Code is Ondrej Kelle.                                                          }
{ Portions created by Ondrej Kelle are Copyright Ondrej Kelle. All rights reserved.                                    }
{                                                                                                                      }
{ Contributors:                                                                                                        }
{   Ondrej Kelle (tondrej)                                                                                             }
{                                                                                                                      }
{**********************************************************************************************************************}
{                                                                                                                      }
{ This unit contains Subversion history provider, implementing OpenTools FileHistory API interfaces.                   }
{                                                                                                                      }
{**********************************************************************************************************************}

unit SvnIDEHistory;

interface

{$INCLUDE Compilers.inc}

uses
  Classes, SysUtils, Types, ActiveX, ComObj, Controls,
  ToolsAPI,
  {$IFDEF COMPILER_9_UP}
  FileHistoryAPI,
  {$ENDIF}
  SvnClient;

type
  TDispInterfacedObject = class(TInterfacedObject, IDispatch)
  protected
    { IDispatch }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  end;

{$IFNDEF COMPILER_9_UP}
  {$MINENUMSIZE 4}
  TOTAHistoryStyle = (hsBuffer, hsFile, hsLocalFile, hsRemoteRevision, hsActiveRevision);
  {$MINENUMSIZE 1}
  TOTAFileNameArray = array of WideString;
  IOTAFileHistoryProvider = interface;
  IOTAFileHistory = interface;
  IOTAFileHistoryNotifier = interface;

  IOTAFileHistoryManager = interface
    ['{55A2BEE4-A64C-4749-8388-070CAEFDEFA5}']
    function AddNotifier(const ANotifier: IOTAFileHistoryNotifier): Integer;
    procedure AddTemporaryLabel(const ALabelName: WideString; const AFiles: TOTAFileNameArray);
    function Get_Count: Integer;
    function GetFileHistoryProvider(Index: Integer): IOTAFileHistoryProvider;
    function RegisterHistoryProvider(const HistoryProvider: IOTAFileHistoryProvider): Integer;
    procedure RemoveNotifier(Index: Integer);
    procedure RevertTemporaryLabel(const ALabelName: WideString);
    procedure UnregisterHistoryProvider(Index: Integer);
    procedure UpdateProviders;

    property Count: Integer read Get_Count;
    property FileHistoryProvider[Index: Integer]: IOTAFileHistoryProvider read GetFileHistoryProvider;
  end;

  IOTAFileHistoryNotifier = interface(IOTANotifier)
    ['{286AC9E5-875A-4402-AF70-8ACDD6757EC8}']
    procedure ProvidersUpdated;
  end;

  IOTAFileHistoryProvider = interface(IDispatch)
    ['{B8CDB02D-93D8-4088-AE03-A28052AD0FAD}']
    function Get_Ident: WideString; safecall;
    function Get_Name: WideString; safecall;
    function GetFileHistory(const AFileName: WideString): IOTAFileHistory; safecall;

    property Ident: WideString read Get_Ident;
    property Name: WideString read Get_Name;
  end;

  IOTAFileHistory = interface(IDispatch)
    ['{92E624D2-A7CD-4C89-9B4E-71170955E96C}']
    function Get_Count: Integer; safecall;
    function GetAuthor(Index: Integer): WideString; safecall;
    function GetComment(Index: Integer): WideString; safecall;
    function GetContent(Index: Integer): IStream; safecall;
    function GetDate(Index: Integer): TDateTime; safecall;
    function GetIdent(Index: Integer): WideString; safecall;
    function GetHistoryStyle(Index: Integer): TOTAHistoryStyle; safecall;
    function GetLabelCount(Index: Integer): Integer; safecall;
    function GetLabels(Index, LabelIndex: Integer): WideString; safecall;

    property Author[Index: Integer]: WideString read GetAuthor;
    property Count: Integer read Get_Count;
    property Comment[Index: Integer]: WideString read GetComment;
    property Content[Index: Integer]: IStream read GetContent;
    property Date[Index: Integer]: TDateTime read GetDate;
    property HistoryStyle[Index: Integer]: TOTAHistoryStyle read GetHistoryStyle;
    property Ident[Index: Integer]: WideString read GetIdent;
    property LabelCount[Index: Integer]: Integer read GetLabelCount;
    property Labels[Index, LabelIndex: Integer]: WideString read GetLabels;
  end;
{$ENDIF}

  TSvnFileHistoryProvider = class(TDispInterfacedObject, IOTAFileHistoryProvider)
  private
    FClient: TSvnClient;
    FItems: TStringList;

    procedure ClearItems;

    { IOTAFileHistoryProvider }
    function Get_Ident: WideString; safecall;
    function Get_Name: WideString; safecall;
    function GetFileHistory(const AFileName: WideString): IOTAFileHistory; safecall;
  public
    constructor Create;
    destructor Destroy; override;

    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
  end;

  ISvnFileHistory = interface(IOTAFileHistory)
    ['{BA13BDFA-7E77-409E-9FEB-E282BD0F809D}']
    function GetItem: TSvnItem; safecall;
    property Item: TSvnItem read GetItem;
  end;

const
  SvnFileHistoryProvider = 'TOndrej.SubversionFileHistoryProvider';

implementation

uses
  apr, svn_client,
  SvnIDEClient;

type
  TSvnFileHistory = class(TDispInterfacedObject, IOTAFileHistory, ISvnFileHistory)
  private
    FItem: TSvnItem;

    { IOTAFileHistory }
    function Get_Count: Integer; safecall;
    function GetAuthor(Index: Integer): WideString; safecall;
    function GetComment(Index: Integer): WideString; safecall;
    function GetContent(Index: Integer): IStream; safecall;
    function GetDate(Index: Integer): TDateTime; safecall;
    function GetIdent(Index: Integer): WideString; safecall;
    function GetHistoryStyle(Index: Integer): TOTAHistoryStyle; safecall;
    function GetLabelCount(Index: Integer): Integer; safecall;
    function GetLabels(Index, LabelIndex: Integer): WideString; safecall;

    { ISvnFileHistory }
    function GetItem: TSvnItem; safecall;
  public
    constructor Create(AItem: TSvnItem);
    destructor Destroy; override;

    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
  end;

//----------------------------------------------------------------------------------------------------------------------

{ TDispInterfacedObject private: IDispatch }

//----------------------------------------------------------------------------------------------------------------------

function TDispInterfacedObject.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer;
  DispIDs: Pointer): HResult;

begin
  Result := E_NOTIMPL;
end;

//----------------------------------------------------------------------------------------------------------------------

function TDispInterfacedObject.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;

begin
  Result := E_NOTIMPL;
end;

//----------------------------------------------------------------------------------------------------------------------

function TDispInterfacedObject.GetTypeInfoCount(out Count: Integer): HResult;

begin
  Result := S_OK;
  Count := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TDispInterfacedObject.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params;
  VarResult, ExcepInfo, ArgErr: Pointer): HResult;
  
begin
  Result := E_NOTIMPL;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnFileHistory private: IOTAFileHistory }

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistory.Get_Count: Integer;

begin
  Result := FItem.HistoryCount;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistory.GetAuthor(Index: Integer): WideString;

begin
  Result := TSvnHistoryItem(FItem.HistoryItems[Index]).Author;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistory.GetComment(Index: Integer): WideString;

begin
  Result := TSvnHistoryItem(FItem.HistoryItems[Index]).LogMessage;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistory.GetContent(Index: Integer): IStream;

var
  Item: TSvnHistoryItem;

begin
  Item := FItem.HistoryItems[Index];
  if Item.Revision = Item.Owner.BaseRevision then
    Result := TStreamAdapter.Create(TStringStream.Create(Item.Owner.GetBaseFile), soOwned)
  else if Item.Revision = Item.Owner.CommittedRevision then
    Result := TStreamAdapter.Create(TStringStream.Create(Item.Owner.GetCommittedFile), soOwned)
  else
    Result := TStreamAdapter.Create(TStringStream.Create(Item.GetFile), soOwned);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistory.GetDate(Index: Integer): TDateTime;

begin
  Result := TzToUTCDateTime(TSvnHistoryItem(FItem.HistoryItems[Index]).Time);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistory.GetHistoryStyle(Index: Integer): TOTAHistoryStyle;

var
  Item: TSvnHistoryItem;

begin
  Item := FItem.HistoryItems[Index];
  if Item.Revision = Item.Owner.CommittedRevision then
    Result := hsActiveRevision
  else
    Result := hsRemoteRevision;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistory.GetIdent(Index: Integer): WideString;

begin
  Result := IntToStr(FItem.HistoryItems[Index].Revision);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistory.GetLabelCount(Index: Integer): Integer;

begin
  Result := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistory.GetLabels(Index, LabelIndex: Integer): WideString;

begin
  case LabelIndex of
    0:
      Result := GetIdent(Index);
    else
      Result := '';
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnFileHistory private: ISvnFileHistory }

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistory.GetItem: TSvnItem;

begin
  Result := FItem;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnFileHistory public }

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnFileHistory.Create(AItem: TSvnItem);

begin
  inherited Create;
  FItem := AItem;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSvnFileHistory.Destroy;

begin
  FItem.Tag := 1;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistory.SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult;

begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, IOTAFileHistory, '', '');
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnFileHistoryProvider private }

//----------------------------------------------------------------------------------------------------------------------

procedure TSvnFileHistoryProvider.ClearItems;

var
  I: Integer;

begin
  for I := 0 to FItems.Count - 1 do
    FItems.Objects[I].Free;
  FItems.Clear;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnFileHistoryProvider private: IOTAFileHistoryProvider }

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistoryProvider.GetFileHistory(const AFileName: WideString): IOTAFileHistory;

var
  Index: Integer;
  Item: TSvnItem;

begin
  Result := nil;

  if not FClient.IsPathVersioned(AFileName) then
    Exit;

  if FItems.Find(AFileName, Index) then
  begin
    Item := TSvnItem(FItems.Objects[Index]);
    if Item.Tag <> 0 then
    begin
      Item.Tag := 0;
      Item.Reload;
    end;
  end
  else
  begin
    Item := TSvnItem.Create(FClient, nil, AFileName);
    try
      FItems.AddObject(Item.PathName, Item);
    except
      Item.Free;
      raise;
    end;
  end;

  SvnIDEModule.SetupBlameControl;
  SvnIDEModule.ShowHistoryEditControls;
  {$IFDEF COMPILER_9_UP}
  SvnIDEModule.SetupDiff3Frame;
  {$ENDIF}

  Result := TSvnFileHistory.Create(Item);
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistoryProvider.Get_Ident: WideString;

begin
  Result := SvnFileHistoryProvider;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistoryProvider.Get_Name: WideString;

begin
  Result := 'Subversion history provider';
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSvnFileHistoryProvider public }

//----------------------------------------------------------------------------------------------------------------------

constructor TSvnFileHistoryProvider.Create;

begin
  inherited Create;
  FClient := SvnIDEModule.SvnClient;
  FItems := TStringList.Create;
  FItems.CaseSensitive := False;
  FItems.Duplicates := dupError;
  FItems.Sorted := True;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSvnFileHistoryProvider.Destroy;

begin
  ClearItems;
  FItems.Free;
  FClient := nil;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

function TSvnFileHistoryProvider.SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult;

begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, IOTAFileHistoryProvider, '', '');
end;

//----------------------------------------------------------------------------------------------------------------------

end.
