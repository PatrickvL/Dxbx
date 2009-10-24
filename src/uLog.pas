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
  uTypes, // IntPtr
  uDxbxUtils,
  uConsoleClass;

var
  m_DxbxDebug: DebugMode = DM_NONE;
  m_DxbxDebugFileName: string = '';
  m_KrnlDebug: DebugMode = DM_CONSOLE;
  m_KrnlDebugFileName: string = '';

procedure CreateLogs(aLogType: TLogType = ltKernel);
procedure CloseLogs;
procedure WriteLog(const aText: string);

function DxbxFormat(aStr: string; Args: array of const; MayRenderArguments: Boolean = True): string;

procedure DbgPrintf(aStr: string); overload;
procedure DbgPrintf(aStr: string; Arg: Variant); overload;
procedure DbgPrintf(aStr: string; Args: array of const; MayRenderArguments: Boolean = True); overload;
procedure SetLogMode(aLogMode: DebugMode = DM_NONE); export;

implementation

{$IFDEF DXBX_DLL}
uses
  DxLibraryAPIScanning;
{$ENDIF}

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
    vtChar: Result := Result + Char(aVarRec.VChar);
//    vtExtended: Result := Result + (aVarRec.V);
    vtString: Result := Result + string(aVarRec.VString^);
    vtPointer: Result := Result + PointerToString(aVarRec.VPointer);
    vtPChar: Result := Result + string(aVarRec.VPChar);
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

var
  ZeroChar: Char = #0;

// Checks whether the given address is a valid address
// Source : getmem.inc
// TODO : This would be more valuable if it returned a set of relevant flags.
function IsValidAddress(const aPtr: Pointer): Boolean;
var
  LMemInfo: TMemoryBasicInformation;
begin
  Result := IntPtr(aPtr) > (64*1024);
  if not Result then
    Exit;

//      {Do we need to recheck the virtual memory?}
//      if (Cardinal(LMemInfo.BaseAddress) > Cardinal(APAddress))
//        or ((Cardinal(LMemInfo.BaseAddress) + Cardinal(LMemInfo.RegionSize)) < (Cardinal(APAddress) + 4)) then

  // Get the VM status for the pointer
  LMemInfo.RegionSize := 0;
  VirtualQuery(aPtr, {var}LMemInfo, SizeOf(LMemInfo));

  // Check the readability of the memory address
  Result := (Cardinal(LMemInfo.RegionSize) >= 4)
        and (LMemInfo.State = MEM_COMMIT)
        and (LMemInfo.Protect and (   PAGE_READONLY or PAGE_READWRITE
                                   or PAGE_EXECUTE or PAGE_EXECUTE_READ
                                   or PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY) <> 0)
        and (LMemInfo.Protect and PAGE_GUARD = 0);
end;

function TryReadLiteralString(const Ptr: PAnsiChar; var aOutputStr: string): Boolean;
const
  // Here the (rather arbitrary) steering parameters :
  MinStrLen = 3;
  MaxStrLen = 260;
  PrintableChars = [' '..#127];
var
  i: Integer;
  NrAnsiChars: Integer;
  NrWideZeros: Integer;
begin
  Result := False;

  NrAnsiChars := 0;
  NrWideZeros := 0;

  // Dected as much string-contents as we can :
  i := 0;
  while i < MaxStrLen do
  try
    if Ptr[i] = #0 then
    begin
      // Zero's on odd position could indicate an UTF-16LE string :
      if Odd(i) then
      begin
        Inc(NrWideZeros);
        Inc(i);
        Continue;
      end;

      // The string ends on a #0 :
      Break;
    end;

    // It's no string when it contains non-printable characters :
    if not (Ptr[i] in PrintableChars) then
      Exit;

    Inc(NrAnsiChars);
    Inc(i);
  except
    // Save-guard against illegal memory-accesses :
    Exit;
  end;

  // It's no string when it's too short :
  if NrAnsiChars < MinStrLen then
    Exit;

  if Abs(NrAnsiChars - NrWideZeros) <= 1 then
    {var}aOutputStr := '"' + Copy(PWideChar(Ptr), 0, NrAnsiChars) + '"'
  else
    {var}aOutputStr := '"' + Copy(Ptr, 0, NrAnsiChars) + '"';
  Result := True
end;

function TryPointerToString(Ptr: Pointer; var aOutputStr: string): Boolean;
const
  MaxIndirection = 2;
var
  IndirectionLevel: Integer;
{$IFDEF DXBX_DLL}
  Symbol: TDetectedVersionedXboxLibrarySymbol;
{$ENDIF}
begin
  Result := True;
  IndirectionLevel := 0;
  while True do
  begin
    // Only handle valid addresses :
    if not IsValidAddress(Ptr) then
    begin
      Result := False;
      Exit;
    end;
    
{$IFDEF DXBX_DLL}
    // See if it's a symbol :
    Symbol := DetectedSymbols.FindByAddress(Ptr);
    if Assigned(Symbol) then
    begin
      {var}aOutputStr := Symbol.SymbolName;
      // TODO : Also mark if this symbol is patched
      Break;
    end;

    // TODO : See if it's an element from AvailablePatches
{$ENDIF}

    // See if it's a literal string :
    if TryReadLiteralString(PAnsiChar(Ptr), {var}aOutputStr) then
      Break;

    // See if we may still try indirect pointers :
    Inc(IndirectionLevel);
    if IndirectionLevel > MaxIndirection then
    begin
      Result := False;
      Exit;
    end;

    // Try repeating the loop, but one indirection further :
    Ptr := PPointer(Ptr)^;
  end; // while True

  Assert(Result, 'We should only come here when there''s output!');

  // Indicate possible indirection-steps via a '^'-prefix:
  while IndirectionLevel > 0 do
  begin
    {var}aOutputStr := '^' + aOutputStr;
    Dec(IndirectionLevel);
  end;
end;

function DxbxFormat(aStr: string; Args: array of const; MayRenderArguments: Boolean = True): string; // array of TVarRec actually

  function _TryArgumentToString(const aVarRec: TVarRec; var aOutputStr: string): Boolean;
  begin
    // Only handle potential pointer types (but not strings themselves) :
    Result := (aVarRec.VType in [vtInteger, vtPointer{, vtObject}])
          and TryPointerToString(aVarRec.VPointer, {var}aOutputStr);
  end;

var
  i: Integer;
  NrOfArguments: Integer;
  CurrentPercentageOffset: Integer;
  ArgumentAsString: string;
begin
  try
    if Length(Args) = 0 then
      MayRenderArguments := False;
      
    if MayRenderArguments then
    begin
      // Count the number of single '%' characters in the formatting string,
      // which indicates the number of variables that could be extended :
      // Note : This doesn't yet handle indexed arguments!
      NrOfArguments := 0;
      CurrentPercentageOffset := 1;
      while CurrentPercentageOffset < Length(aStr) do
      begin
        Inc(CurrentPercentageOffset);
        if (aStr[CurrentPercentageOffset - 1] = '%') then
        begin
          // Skip double-escaped '%'-characters :
          if (aStr[CurrentPercentageOffset] = '%') then
            Inc(CurrentPercentageOffset)
          else
            Inc(NrOfArguments);
        end;
      end;

      // If that's different than the number of arguments, we can't extend :
      if NrOfArguments <> Length(Args) then
        MayRenderArguments := False;
    end;

    CurrentPercentageOffset := 1;

    // Process all input arguments :
    for i := Low(Args) to High(Args) do
    begin
      // Update some argument types to prevent errors in Format() :
      case Args[i].VType of
        // Make sure PChar points to a zero-terminated string :
        vtPChar:
          if Args[i].VPChar = nil then
            Args[i].VPChar := @ZeroChar;

        // Change all pointers into integers, so those don't raise
        // an EConvertError. (This is easier than having casts everywhere) :
        vtBoolean,
        vtPointer:
          Args[i].VType := vtInteger; // The data doesn't have to be changed, because it's already in-place
      end;

      if not MayRenderArguments then
        Continue;

      // Step to the next percentage-character :
      while CurrentPercentageOffset < Length(aStr) do
      begin
        Inc(CurrentPercentageOffset);
        if (aStr[CurrentPercentageOffset - 1] = '%') then
        begin
          // Skip double-escaped '%'-characters :
          if (aStr[CurrentPercentageOffset] = '%') then
            Inc(CurrentPercentageOffset)
          else
            Break;
        end;
      end;

      // See if this argument can be translated to an additional string :
      if _TryArgumentToString(Args[i], {var}ArgumentAsString) then
      begin
        // Insert the new contents in the format-string, directly after the current argument-position  :
        while not (aStr[CurrentPercentageOffset - 1] in ['d', 'p', 'x', 'X']) do
          Inc(CurrentPercentageOffset);

        // Make sure the argument doesn't interfere with the formatting-string :
        ArgumentAsString := ' (' + StringReplace(ArgumentAsString, '%', '%%', [rfReplaceAll]) + ')';
        Insert(ArgumentAsString, {var}aStr, CurrentPercentageOffset);

        Inc(CurrentPercentageOffset, Length(ArgumentAsString));
      end;
    end;

    // Now try to format the string, including it's arguments :
    Result := SysUtils.Format(aStr, Args);
  except
    on E: Exception do
    begin
      // When something went wrong, log as much details as we can get our hands on,
      // so we have an opportunity to fix a wrong type via cast or whatever :
      Result := 'Catched an exception! Type=' + E.ClassName +
                #13#10 + E.Message +
                #13#10 + aStr;
      for i := Low(Args) to High(Args) do
        Result := Result + #13#10 + TVarRecToString(Args[i]);
    end;
  end;
end;

procedure DbgPrintf(aStr: string; Args: array of const; MayRenderArguments: Boolean = True);
begin
  WriteLog(DxbxFormat(aStr, Args, MayRenderArguments));
end;

procedure DbgPrintf(aStr: string; Arg: Variant);
begin
  WriteLog(DxbxFormat(aStr, [Arg]));
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
      if not Assigned(ConsoleControl) then
      try
        ConsoleControl := TConsoleControl.Create;
        if aLogType = ltGui then
          ConsoleControl.SetWindowTitle('DXBX : Debug Console')
        else // ltKernel
          ConsoleControl.SetWindowTitle('DXBX : Kernel Debug Console');

        ConsoleControl.SetBackgroundColor(False, False, False, False); // = black
        ConsoleControl.SetForegroundColor(False, True, False, False); // = lime
        ConsoleControl.HideCursor;
      except
        m_DxbxDebug := DM_NONE;
        FreeAndNil({var}ConsoleControl);
        raise Exception.Create('Could not create log console');
      end;

    DM_FILE:
      if not LogFileOpen then
      try
        if aLogType = ltGui then
          OutputFileName := m_DxbxDebugFileName
        else
          OutputFileName := m_KrnlDebugFileName;

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
  FreeAndNil(ConsoleControl);

  if LogFileOpen then
  begin
    CloseFile(LogFile);
    LogFileOpen := False;
  end;

  m_DxbxDebug := DM_NONE;
end;

procedure WriteLog(const aText: string);

  function _Text: string;
  begin
    // Prefix the text with the CurrentThreadID :
    Result := '[0x' + IntToHex(GetCurrentThreadID(), 4) + '] '
    // and fix up any c-style newlines :
            + StringReplace(aText, '\n', #13#10, [rfReplaceAll]);
  end;

var
  CurrentFS: Word;
begin
  case m_DxbxDebug of
    DM_CONSOLE:
      if Assigned(ConsoleControl) then
      begin
        EnterCriticalSection({var}DxbxLogLock);
        try
          ConsoleControl.WriteTextLine(_Text());
        finally
          LeaveCriticalSection({var}DxbxLogLock);
        end;
      end;

    DM_FILE:
      if LogFileOpen then
      begin
        EnterCriticalSection({var}DxbxLogLock);
        CurrentFS := GetFS();
        try
          WriteLn({var}LogFile, _Text());
          // BUGFIX : Because the above call goes through kernel32.WriteFile,
          // which alters the FS register, we have to restore it afterwards :
          SetFS(CurrentFS);
          
          Flush({var}LogFile);
        finally
          LeaveCriticalSection({var}DxbxLogLock);
        end;
      end;
  end;
end;

initialization

  InitializeCriticalSection({var}DxbxLogLock);

finalization

  CloseLogs;

end.
