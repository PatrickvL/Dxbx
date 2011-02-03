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

{$INCLUDE Dxbx.inc}

{.$DEFINE DXBX_DEBUGMM}

interface

uses
  // Delphi
  SysUtils,
  // JCL
  JclDebug;

{$IFDEF DXBX_USE_JCLDEBUG}
type
  TDxbxAPIDebugInfoSource = class(TJclDebugInfoSource)
  public
    function InitializeSource: Boolean; override;
    function GetLocationInfo(const Addr: Pointer; out Info: TJclLocationInfo): Boolean; override;
  end;

function JclLastExceptStackListToString(OnlyCallerModule: Boolean = False): string;

{$ENDIF}

implementation

{$IFDEF DXBX_USE_JCLDEBUG}

uses
  // Dxbx
  uTypes,
  uLog,
  uXboxLibraryUtils,
  uEmuFS,
  DxLibraryAPIScanning;

function JclLastExceptStackListToString(OnlyCallerModule: Boolean = False): string;
var
  List: TJclStackInfoList;
  I: Integer;
  CallerModule: string;
  CallerInfo, Info: TJclLocationInfo;
  b: Boolean;
begin
  List := JclLastExceptStackList;
  if not GetLocationInfo(Caller(1), {var}CallerInfo) then
    Result := Format('Exception at address $%p', [Caller(1)])
  else
  begin
    if OnlyCallerModule then
    begin
      CallerModule := CallerInfo.SourceName;
      Result := Format('Exception in module %s: ', [CallerModule]);
      if Assigned(List) then
      begin
        b := False;
        for I := 0 to List.Count - 1 do
          if GetLocationInfo(List.Items[I].CallerAddr, {var}Info) then
            with Info do
             if CallerModule = Info.SourceName then
             begin
               Result := Result + Format('[%s at line %d] <- ', [ProcedureName, LineNumber]);
               b := True;
             end;

        if b then
          SetLength(Result, Length(Result) - 4);
      end;
    end
    else
    begin
      Result := 'Exception ';
      if Assigned(List) then
      begin
        for I := 0 to List.Count - 1 do
          if GetLocationInfo(List.Items[I].CallerAddr, {var}Info) then
            Result := Result + LocationInfoToString(Info) + ' <- '
  //          with Info do
  //            Result := Result + Format('[%s: %s at line %d] <- ', [UnitName, ProcedureName, LineNumber]);
          else
            Result := Result + Format('$%p <- ', [List.Items[I].CallerAddr]);

        if List.Count > 0 then
          SetLength(Result, Length(Result) - 4);
      end;
    end;
  end;
end;

{ TDxbxAPIDebugInfoSource }

function TDxbxAPIDebugInfoSource.GetLocationInfo(const Addr: Pointer; out Info: TJclLocationInfo): Boolean;
begin
  Result := Assigned(SymbolManager);
  if not Result then
    Exit;

//  p := SymbolManager.FindPotentialFunctionLocation(TCodePointer(Addr));
//  Result := Assigned(p) and Assigned(p.Symbol);
//  if not Result then
//    Exit;

  with Info do
  begin
    OffsetFromProcName := Integer(IntPtr(Addr) - IntPtr(Address)); // Offset from Address to ProcedureName symbol location

    // Early exit if address falls outside symbol-size;
    // Note that the symbol length defaults to 4 when
    // symbols are loaded from cache !
//    if OffsetFromProcName > Integer(p.Length) then
//    begin
//      Result := False;
//      Exit;
//    end;
//
//    Address := Pointer(p{.Symbol}.Address); // Error address
//
////    UnitName: string;               // Name of Delphi unit
//    ProcedureName := p.Symbol.Name;
//    LineNumber: Integer;
//    OffsetFromLineNumber: Integer;  // Offset from Address to LineNumber symbol location
//    SourceName: string;             // Module file name
    Info.DebugInfo := Self; // Location object
//    if Assigned(DetectedSymbol.VersionedXboxLibrary) then
//      BinaryFileName := DetectedSymbol.VersionedXboxLibrary.LibName + '.lib'; // Name of the binary file containing the symbol
  end;
end;

function TDxbxAPIDebugInfoSource.InitializeSource: Boolean;
(*
var
  MapFileName: TFileName;
*)
begin
  Result := True; // TODO -oDXBX:
(*
  MapFileName := ChangeFileExt(FileName, JclMapFileExtension);
  Result := FileExists(MapFileName);
  if Result then
    FScanner := TJclMapScanner.Create(MapFileName, Module);
*)
end;

{$ELSE}

function JclLastExceptStackListToString(OnlyCallerModule: Boolean = False): string;
begin
  Result := '';
end;

{$ENDIF}

{$IFDEF DXBX_DEBUGMM}
function DxbxGetMem(Size: Integer): Pointer;
begin
  try
    Result := SysGetMem(Size);
  except
    Result := SysGetMem(Size); // Breakpoint opportunity
  end;
end;

function DxbxFreeMem(P: Pointer): Integer;
begin
  try
    Result := SysFreeMem(P);
  except
    Result := SysFreeMem(P); // Breakpoint opportunity
  end;
end;

function DxbxReallocMem(P: Pointer; Size: Integer): Pointer;
begin
  try
    Result := SysReallocMem(P, Size);
  except
    Result := SysReallocMem(P, Size); // Breakpoint opportunity
  end;
end;

function DxbxAllocMem(Size: Cardinal): Pointer;
begin
  try
    Result := SysAllocMem(Size);
  except
    Result := SysAllocMem(Size); // Breakpoint opportunity
  end;
end;

function DxbxRegisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  try
    Result := SysRegisterExpectedMemoryLeak(P);
  except
    Result := SysRegisterExpectedMemoryLeak(P); // Breakpoint opportunity
  end;
end;

function DxbxUnregisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  try
    Result := SysUnregisterExpectedMemoryLeak(P);
  except
    Result := SysUnregisterExpectedMemoryLeak(P); // Breakpoint opportunity
  end;
end;

var
  DxbxMemoryManager: TMemoryManagerEx = (
    GetMem: DxbxGetMem;
    FreeMem: DxbxFreeMem;
    ReallocMem: DxbxReallocMem;
    AllocMem: DxbxAllocMem;
    RegisterExpectedmemoryLeak: DxbxRegisterExpectedMemoryLeak;
    UnregisterExpectedmemoryLeak: DxbxUnregisterExpectedMemoryLeak);
{$ENDIF}

var
  OrigErrorHandler: procedure(ErrorCode: Byte; ErrorAddr: Pointer);

procedure DxbxErrorHandler(ErrorCode: Byte; ErrorAddr: Pointer);// export;
begin
  EmuSwapFS(fsWindows);

  ErrorProc := OrigErrorHandler;
  try
    try
      uLog.DbgPrintf('DxbxErrorHandler(%d,$%.8x) %s',
        [ErrorCode, ErrorAddr, JclLastExceptStackListToString]);
    except
      // ignore
    end;
  finally
    ErrorProc := DxbxErrorHandler;
    OrigErrorHandler(ErrorCode, ErrorAddr);
  end;
end;

initialization

{$IFDEF DXBX_USE_JCLDEBUG}
  TJclDebugInfoList.RegisterDebugInfoSource(TDxbxAPIDebugInfoSource);
{$ENDIF}

  OrigErrorHandler := ErrorProc;
  ErrorProc := DxbxErrorHandler;
{$IFDEF DXBX_DEBUGMM}
  SetMemoryManager(DxbxMemoryManager);
{$ENDIF}

finalization

  ErrorProc := OrigErrorHandler;

end.
