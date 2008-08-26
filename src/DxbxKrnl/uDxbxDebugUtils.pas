(*
    This file is part of Dxbx - a XBox emulator written in Delphi (ported over from cxbx)
    Copyright (C) 2007 Shadow_tj and other members of the development team.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)
unit uDxbxDebugUtils;

{$INCLUDE ..\Dxbx.inc}

interface

{$IFDEF DXBX_USE_JCLDEBUG}
uses
  // Delphi
  SysUtils,
	// JCL
	JclDebug,
  // Dxbx
  uTypes,
  uXboxLibraryUtils,
  DxLibraryAPIScanning;

type
	TDxbxAPIDebugInfoSource = class(TJclDebugInfoSource)
  public
    function InitializeSource: Boolean; override;
    function GetLocationInfo(const aAddr: Pointer; var Info: TJclLocationInfo): Boolean; override;
  end;

function JclLastExceptStackListToString(OnlyCallerModule: Boolean = False): String;

{$ENDIF}

implementation

{$IFDEF DXBX_USE_JCLDEBUG}

function JclLastExceptStackListToString(OnlyCallerModule: Boolean = False): String;
var
  List: TJclStackInfoList;
  I: Integer;
  CallerModule: String;
  CallerInfo, Info: TJclLocationInfo;
  b: Boolean;
begin
  List := JclLastExceptStackList;
  if not GetLocationInfo(Caller(1), CallerInfo) then
    Result := Format('Exception at address %p', [Caller(1)])
  else
  begin
    if OnlyCallerModule then
    begin
      CallerModule := CallerInfo.SourceName;
      Result := Format('Exception in module %s: ', [CallerModule]);
      b := False;
      for I := 0 to List.Count - 1 do
        if GetLocationInfo(List.Items[I].CallerAdr, Info) then
          with Info do
           if CallerModule = Info.SourceName then
           begin
             Result := Result + Format('[%s at line %d] <- ', [ProcedureName, LineNumber]);
             b := True;
           end;

      if b then
        SetLength(Result, Length(Result) - 4);
    end
    else
    begin
      Result := 'Exception ';
      for I := 0 to List.Count - 1 do
      if GetLocationInfo(List.Items[I].CallerAdr, Info) then
      with Info do
        Result := Result + Format('[%s: %s at line %d] <- ', [UnitName, ProcedureName, LineNumber]);
      if List.Count > 0 then
        SetLength(Result, Length(Result) - 4);
    end;
  end;
end;

{ TDxbxAPIDebugInfoSource }

function TDxbxAPIDebugInfoSource.GetLocationInfo(const aAddr: Pointer; var Info: TJclLocationInfo): Boolean;
var
  DetectedFunction: PDetectedVersionedXboxLibraryFunction;
begin
  DetectedFunction := DetectedFunctions.FindByAddress(TCodePointer(aAddr));
  Result := Assigned(DetectedFunction);
  if not Result then
    Exit;

  with Info do
  begin
    Address := Pointer(DetectedFunction.CodeStart); // Error address
//    UnitName: string;               // Name of Delphi unit
    ProcedureName := DetectedFunction.FunctionName;
    OffsetFromProcName := Integer(IntPtr(aAddr) - IntPtr(Address)); // Offset from Address to ProcedureName symbol location
//    LineNumber: Integer;
//    OffsetFromLineNumber: Integer;  // Offset from Address to LineNumber symbol location
//    SourceName: string;             // Module file name
    Info.DebugInfo := Self; // Location object
    if Assigned(DetectedFunction.VersionedXboxLibrary) then
      BinaryFileName := DetectedFunction.VersionedXboxLibrary.LibName + '.lib'; // Name of the binary file containing the symbol
  end;
end;

function TDxbxAPIDebugInfoSource.InitializeSource: Boolean;
(*
var
  MapFileName: TFileName;
*)
begin
  Result := True; // TODO
(*
  MapFileName := ChangeFileExt(FileName, JclMapFileExtension);
  Result := FileExists(MapFileName);
  if Result then
    FScanner := TJclMapScanner.Create(MapFileName, Module);
*)
end;

initialization

  TJclDebugInfoList.RegisterDebugInfoSource(TDxbxAPIDebugInfoSource);

{$ENDIF}

end.
