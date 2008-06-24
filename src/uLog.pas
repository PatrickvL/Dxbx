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
unit uLog;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  SysUtils,
  Variants,
  // Dxbx
  uTypes,
  uLogConsole;

var
  m_DxbxDebug: DebugMode = DM_NONE;
  m_DxbxDebugFilename: string = '';
  m_KrnlDebug: DebugMode = DM_CONSOLE;
  m_KrnlDebugFilename: string = '';

procedure CreateLogs(aLogType: TLogType = ltKernel);
procedure CloseLogs;
procedure WriteLog(aText: string);
procedure DbgPrintf(aStr: string); overload;
procedure DbgPrintf(aStr: string; Args: array of const); overload
procedure SetLogMode(aLogMode: DebugMode = DM_NONE); export;

implementation

var
  LogFileOpen: Boolean = False;
  LogFile: TextFile;

  DxbxLogLock: Windows._RTL_CRITICAL_SECTION;

type
  TVarRecType = Byte;

function TVarRecTypeToString(const aVarRecType: TVarRecType): string;
begin
  case aVarRecType of
    vtInteger: Result := 'vtInteger';
    vtBoolean: Result := 'vtBoolean';
    vtChar: Result := 'vtChar';
    vtExtended: Result := 'vtExtended';
    vtString: Result := 'vtString';
    vtPointer: Result := 'vtPointer';
    vtPChar: Result := 'vtPChar';
    vtObject: Result := 'vtObject';
    vtClass: Result := 'vtClass';
    vtWideChar: Result := 'vtWideChar';
    vtPWideChar: Result := 'vtPWideChar';
    vtAnsiString: Result := 'vtAnsiString';
    vtCurrency: Result := 'vtCurrency';
    vtVariant: Result := 'vtVariant';
    vtInterface: Result := 'vtInterface';
    vtWideString: Result := 'vtWideString';
    vtInt64: Result := 'vtInt64';
  else
    Result := 'Unknown:' + IntToStr(aVarRecType);
  end;
end;

function TVarRecToString(const aVarRec: TVarRec): string;
begin
  Result := TVarRecTypeToString(aVarRec.VType) + ':';
  case aVarRec.VType of
    vtInteger: Result := Result + IntToStr(aVarRec.VInteger);
//    vtBoolean: Result := Result + (aVarRec.V);
    vtChar: Result := Result + aVarRec.VChar;
//    vtExtended: Result := Result + (aVarRec.V);
    vtString: Result := Result + aVarRec.VString^;
    vtPointer: Result := Result + PointerToString(aVarRec.VPointer);
    vtPChar: Result := Result + aVarRec.VPChar;
//    vtObject: Result := Result + (aVarRec.V); //Object';
//    vtClass: Result := Result + (aVarRec.V); //Class';
//    vtWideChar: Result := Result + (aVarRec.V); //WideChar';
//    vtPWideChar: Result := Result + (aVarRec.V); //PWideChar';
//    vtAnsiString: Result := Result + (aVarRec.VAnsiString);
//    vtCurrency: Result := Result + (aVarRec.V);
    vtVariant: Result := Result + string(aVarRec.VVariant);
//    vtInterface: Result := Result + (aVarRec.V);
//    vtWideString: Result := Result + (aVarRec.V);
    vtInt64: Result := Result + IntToStr(aVarRec.VInt64^);
  else
    Result := Result + 'Unknown';
  end;
end;

procedure DbgPrintf(aStr: string; Args: array of const); // array of TVarRec actually
var
  i: Integer;
begin
  try
    // First, change all pointers into integers, so those don't raise
    // an EConvertError. (This is easier than having casts everywhere) :
    for i := Low(Args) to High(Args) do
    begin
      case Args[i].VType of
        vtPointer:
          Args[i].VType := vtInteger; // The data doesn't have to be changed, because it's already in-place 
      end;
    end;
    
    // Now try to format the string, including it's arguments :
    WriteLog(Format(aStr, Args));

  except
    on E: Exception do
    begin
      // When something went wrong, log as much details as we can get our hands on,
      // so we have an opportunity to fix a wrong type via cast or whatever : 
      WriteLog('Catched an exception! Type=' + E.ClassName);
      WriteLog(E.Message);
      WriteLog(aStr);
      for i := Low(Args) to High(Args) do
        WriteLog(TVarRecToString(Args[i]));
    end;
  end;
end;

procedure DbgPrintf(aStr: string);
begin
  WriteLog(aStr);
end;

procedure SetLogMode(aLogMode: DebugMode = DM_NONE); export;
begin
  WriteLog('SetLogMode(' + DebugModeToString(aLogMode) + ')');
  m_DxbxDebug := aLogMode;
end;

procedure CreateLogs(aLogType: TLogType);
var
  OutputFileName: string;
begin
  WriteLog('CreateLogs(' + LogTypeToString(aLogType) + ')');

  case m_DxbxDebug of
    DM_NONE:
      CloseLogs;

    DM_CONSOLE:
      if not Assigned(frm_LogConsole) then
      try
        frm_LogConsole := Tfrm_LogConsole.Create(nil);
        if aLogType = ltGui then
          frm_LogConsole.Caption := 'DXBX : Debug Console'
        else // ltKernel
          frm_LogConsole.Caption := 'DXBX : Kernel Debug Console';

        frm_LogConsole.Show;
      except
        m_DxbxDebug := DM_NONE;
        FreeAndNil({var}frm_LogConsole);
        raise Exception.Create('Could not create log console');
      end;

    DM_FILE:
      if not LogFileOpen then
      try
        if aLogType = ltGui then
          OutputFileName := m_DxbxDebugFilename
        else
          OutputFileName := m_KrnlDebugFilename;

        if OutputFileName = '' then
          OutputFileName := 'DxbxKrnlDebug.txt';

        AssignFile({var}LogFile, OutputFileName);

        Rewrite({var}LogFile);
        LogFileOpen := True;
      except
        m_DxbxDebug := DM_NONE;
        raise Exception.Create('Could not create log file');
      end;
  end; // case m_DxbxDebug

  WriteLog('Started logging at ' + DateTimeToStr(Now));
end;

procedure CloseLogs;
begin
  WriteLog('Stop logging.');
  FreeAndNil(frm_LogConsole);

  if LogFileOpen then
  begin
    CloseFile(LogFile);
    LogFileOpen := False;
  end;

  m_DxbxDebug := DM_NONE;
end;

procedure WriteLog(aText: string);

  function _Text: string;
  begin
    Result := '[0x' + IntToHex(GetCurrentThreadID(), 4) + '] ' + aText;
  end;

begin
  EnterCriticalSection({var}DxbxLogLock);

  case m_DxbxDebug of
    DM_CONSOLE:
      if Assigned(frm_LogConsole) then
        frm_LogConsole.Log.Lines.Add(_Text());
    DM_FILE:
      if LogFileOpen then
      begin
        WriteLn({var}LogFile, _Text());
        Flush({var}LogFile);
      end;
  end;

  LeaveCriticalSection({var}DxbxLogLock);
end;

initialization

  InitializeCriticalSection({var}DxbxLogLock);

finalization

  CloseLogs;

end.
