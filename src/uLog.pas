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

{.$DEFINE DXBX_TRACE_MEM} // Enable this to measure memory-increments (done in LogBegin to assure a reasonable frequency)

interface

uses
  // Delphi
  Windows,
  SysUtils,
  Variants,
{$IFDEF DXBX_DLL}
  // Jedi
  JclDebug,
  // OpenXDK
  XboxKrnl,
{$ENDIF}
  // Dxbx
  uTypes, // IntPtr
  uConsts,
  uDxbxUtils,
  uConsoleClass;

var
  DebugMode: TDebugMode = dmNone;
  DebugFileName: string = '';

function IsValidAddress(const aPtr: Pointer): Boolean;

{$IFDEF DXBX_DLL}
function LocationInfoToString(const aLocationInfo: TJclLocationInfo): string;
{$ENDIF}

procedure CreateLogs(aDebugMode: TDebugMode; aOutputFileName: string = '');
procedure CloseLogs;
procedure WriteLog(aText: string);

function DxbxFormat(aStr: string; Args: array of const; MayRenderArguments: Boolean = False): string;

procedure DbgPrintf(aStr: string); overload;
procedure DbgPrintf(aStr: string; Arg: Variant); overload;
procedure DbgPrintf(aStr: string; Args: array of const; MayRenderArguments: Boolean = False); overload;

procedure printf(aStr: string); overload;
procedure printf(aStr: string; Args: array of const); overload;

function sprintf(aBuffer: PAnsiChar; const aString: AnsiString): Integer; overload;
function sprintf(aBuffer: PAnsiChar; const aString: AnsiString; Args: array of const): Integer; overload;

//type Tsprintf = function (aBuffer: PAnsiChar): Integer; cdecl varargs;
//var sprintf: Tsprintf;

type
  TLogProc = procedure;
const
  //TLogFlag = (      // Use these flags to indicate...
    lfAlways        = $00000001; // ... logging that should always be visible
    lfDebug         = $00000002; // ... debug logging  normal level
    lfTrace         = $00000004; // ... debug logging  higher level of detail
    lfExtreme       = $00000008; // ... debug logging  that generates a lot of output
    lfCxbx          = $00000010; // ... Cxbx logs this too
    lfDxbx          = $00000020; // ... logging that's unique to Dxbx
    lfKernel        = $00000040; // ... this concerns a Kernel API
    lfPatch         = $00000080; // ... this concerns a emulation patch
    lfSymbolScan    = $00000100; //
    lfPixelShader   = $00000200; //
    lfVertexShader  = $00000400; //
    lfVertexBuffer  = $00000800; //
    lfPushBuffer    = $00001000; //
    lfInvalid       = $00002000; //
    lfHeap          = $00004000; //
    lfFile          = $00008000; //
    lfSound         = $00010000; //
    lfGraphics      = $00020000; //
    lfThreading     = $00040000; //
    lfXOnline       = $00080000;
    lfXapi          = $00100000;
    lfMemory        = $00200000; // ... for the Mm api's
    lfReturnValue   = $00400000;

// Note : Some units declare (and use) lfUnit, a variable that best describes the logging in these units.


type TLogFlags = DWORD; // 'set of TLogFlag' prevents inlining of MayLog
  PLogFlags = ^TLogFlags;

const
  // This are the default logging flags :
  g_DefaultLogFlags_Enabled: TLogFlags = lfAlways or lfSymbolScan or lfDebug or lfCxbx or lfDxbx or lfKernel or lfPatch or lfReturnValue;
  g_DefaultLogFlags_Disabled: TLogFlags = lfHeap or lfExtreme;

var
  // These pointers indicate the logging flags that are currently active
  // (either by default or via indirection to EmuShared, see SetSpecifiedLogFlags):
  // DO NOT USE THESE OUTSIDE THIS UNIT, ONLY HERE TO ENABLE INLINING OF MayLog!
  pLogFlags_Enabled: PLogFlags;
  pLogFlags_Disabled: PLogFlags;

procedure SetSpecifiedLogFlags(const aLogFlags_Enabled, aLogFlags_Disabled: PLogFlags);
procedure ToggleLogging;
function MayLog(const aFlags: TLogFlags): Boolean; inline;

type
  PLogStack = ^RLogStack;

  RLogStack = record
  private
    LogRoot, Next: PLogStack;
    LogName: string; // LogType: string; ??
    LogValue: string;
  public
    function SetName(const aName, aType: string): PLogStack;
    procedure SetValue(const aValue: string; const aDetails: string = ''); overload;
    procedure SetValue(const aValue: UIntPtr; const aDetails: string = ''); overload;
    procedure SetHexValue(const aValue: DWORD; const aNrNibbles: Integer);
  public
    function _(const aValue: AnsiString; const aName: string = ''): PLogStack; overload;
    function _(const aValue: UnicodeString; const aName: string = ''): PLogStack; overload;
    function _(const aValue: int; const aName: string = ''): PLogStack; overload;
    function _(const aValue: float; const aName: string = ''): PLogStack; overload;
    function _(const aValue: SHORT; const aName: string = ''): PLogStack; overload;
    function _(const aValue: WORD; const aName: string = ''): PlogStack; overload;
    function _(const aValue: DWORD; const aName: string = ''): PLogStack; overload;
    function _(const aValue: BOOL; const aName: string = ''): PLogStack; overload;
    function _(const aValue: _BOOLEAN; const aName: string = ''): PLogStack; overload;
    function _(const aValue: PVOID; const aName: string = ''): PLogStack; overload;
    function _(const aValue: LPCSTR; const aName: string = ''): PLogStack; overload;
    function _(const aValue: LPCWSTR; const aName: string = ''): PLogStack; overload;
    function _(const aValue: PPOINT; const aName: string = ''): PLogStack; overload;
    function _(const aValue: PRECT; const aName: string = ''): PLogStack; overload;
    function _(const aValue: LARGE_INTEGER; const aName: string = ''): PLogStack; overload;
{$IFDEF DXBX_DLL}
    function _(const aValue: PLARGE_INTEGER; const aName: string = ''): PLogStack; overload;
    function _(const aValue: PANSI_STRING; const aName: string = ''): PLogStack; overload;
    function _(const aValue: POBJECT_ATTRIBUTES; const aName: string = ''): PLogStack; overload;
    function _(const aValue: FILE_INFORMATION_CLASS; const aName: string = ''): PLogStack; overload;

    function _ACCESS_MASK(const aValue: ACCESS_MASK; const aName: string = ''): PLogStack;
    function _FileAttributes(const aValue: ULONG; const aName: string = ''): PLogStack;
    function _CreateDisposition(const aValue: ULONG; const aName: string = ''): PLogStack;
    function _CreateOptions(const aValue: ULONG; const aName: string = ''): PLogStack;
{$ENDIF}

    procedure LogEnd();
  end;

  function LogBegin(const aSymbolName: string; const aCategory: string = ''): PLogStack;

implementation

{$IFDEF DXBX_DLL}
uses
  DxLibraryAPIScanning,
  uDxbxKrnlUtils;
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

const
  LogFlags_AllDisabled: TLogFlags = High(TLogFlags); // Used to turn off logging
var
  SpecifiedLogFlags: PLogFlags; // Initialized to g_DefaultLogFlags_Disabled
  LoggingEnabled: Boolean = True;

procedure UpdateLogFlags;
// Make sure the LogFlags represent the enabled/disabled state (controllable with F8).
// Also, no logging is done when the DebugMode = dmNone (configurable in Dxbx GUI).
begin
  if LoggingEnabled and (DebugMode > dmNone) then
    pLogFlags_Disabled := SpecifiedLogFlags
  else
    pLogFlags_Disabled := @LogFlags_AllDisabled;
end;

procedure SetSpecifiedLogFlags(const aLogFlags_Enabled, aLogFlags_Disabled: PLogFlags);
begin
  pLogFlags_Enabled := aLogFlags_Enabled; // No need to set this apart, as it's not overriden
  SpecifiedLogFlags := aLogFlags_Disabled;

  UpdateLogFlags;
end;

procedure ToggleLogging;
begin
  LoggingEnabled := not LoggingEnabled;

  UpdateLogFlags;
end;

// TODO -oDxbx: Apply this to all DbgPrintf calls, and put those in inline methods to speed things up
function MayLog(const aFlags: TLogFlags): Boolean; // inline;
begin
  // Note : Check the disabled flags first, as that's faster than checking the enabled flags
  // in cases where logging is disabled. And besides: If logging is enabled, both flags have
  // to be checked anyway, so in that scenario the checking-order is not really important :
  Result := ((aFlags and pLogFlags_Disabled^) = 0)
        and ((aFlags or pLogFlags_Enabled^) > 0);
end;

// Checks whether the given address is a valid address
// Source : getmem.inc
// TODO -oDXBX: This would be more valuable if it returned a set of relevant flags.
function IsValidAddress(const aPtr: Pointer): Boolean;
var
  LMemInfo: TMemoryBasicInformation;
begin
  // Address should be outside first (invalid) page :
  Result := UIntPtr(aPtr) > (64*1024);
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

{$IFDEF DXBX_DLL}
// Subset of JclDebug.GetLocationInfoStr
function LocationInfoToString(const aLocationInfo: TJclLocationInfo): string;
var
  AddStr: string;
begin
(*
  Address: Pointer;               // Error address
  UnitName: string;               // Name of Delphi unit
  ProcedureName: string;          // Procedure name
  OffsetFromProcName: Integer;    // Offset from Address to ProcedureName symbol location
  LineNumber: Integer;            // Line number
  OffsetFromLineNumber: Integer;  // Offset from Address to LineNumber symbol location
  SourceName: string;             // Module file name
  DebugInfo: TJclDebugInfoSource; // Location object
  BinaryFileName: string;         // Name of the binary file containing the symbol
*)
  Result := Format('[$%p] ', [aLocationInfo.Address]);

  AddStr := aLocationInfo.UnitName + '.';
  if (Length(AddStr) > 1)
  and (AddStr <> Copy(aLocationInfo.ProcedureName, 1, Length(AddStr))) then
    Result := Result + AddStr;

  if aLocationInfo.ProcedureName <> '' then
    Result := Result + aLocationInfo.ProcedureName
  else
    Result := Result + '?proc?';

  if aLocationInfo.OffsetFromProcName > 0 then
    Result := Result + Format(' + $%x', [aLocationInfo.OffsetFromProcName]);

  if (aLocationInfo.LineNumber > 0)
  or (aLocationInfo.SourceName <> '') then
  begin
    if aLocationInfo.OffsetFromLineNumber = 0 then
      AddStr := ''
    else
      if aLocationInfo.OffsetFromLineNumber >= 0 then
        AddStr := Format(' + $%x', [aLocationInfo.OffsetFromLineNumber])
      else
        AddStr := Format(' - $%x', [-aLocationInfo.OffsetFromLineNumber]);

    Result := Result + Format(' (Line %u, "%s")%s', [
        aLocationInfo.LineNumber,
        aLocationInfo.SourceName,
        AddStr]);
  end;

(*
  if IncludeVAddress or IncludeModuleName then
  begin
    if IncludeVAddress then
    begin
      OffsetStr :=  Format('(%p) ', [VAddress]);
      Result := OffsetStr + Result;
    end;
    if IncludeModuleName then
      Insert(Format('{%-12s}', [ModuleName]), Result, 11);
  end;
*)
end; // LocationInfoToString
{$ENDIF DXBX_DLL}

function TryPointerToString(Ptr: Pointer; var aOutputStr: UnicodeString): Boolean;
const
  MaxIndirection = 2;
var
  IndirectionLevel: Integer;
{$IFDEF DXBX_DLL}
  LocationInfo: TJclLocationInfo;
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
    if GetLocationInfo(Ptr, {var}LocationInfo) then
    begin
      {var}aOutputStr := LocationInfoToString(LocationInfo);
      Exit;
    end;
{$ENDIF}

    // See if it's a literal string :
    aOutputStr := TryReadLiteralString(PAnsiChar(Ptr));
    if aOutputStr <> '' then
      break;

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
end; // TryPointerToString

function DxbxFormat(aStr: string; Args: array of const; MayRenderArguments: Boolean = False): string; // array of TVarRec actually

  function _TryArgumentToString(const aVarRec: TVarRec; var aOutputStr: UnicodeString): Boolean;
  begin
    // Only handle potential pointer types (but not strings themselves) :
    Result := (aVarRec.VType in [vtInteger, vtPointer{, vtObject}])
          and TryPointerToString(aVarRec.VPointer, {var}aOutputStr);
  end;

var
  i: Integer;
  NrOfArguments: Integer;
  CurrentPercentageOffset: Integer;
//  ArgumentAsString: UnicodeString;
begin
  EnterCriticalSection({var}DxbxLogLock);
  try
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
        vtBoolean:
        begin
          Args[i].VInteger := Ord(Args[i].VBoolean);
          Args[i].VType := vtInteger;
        end;
        // vtExtended: ; // Delphi converts Single and Double to Extended (SLOW!), and points to it on stack
        vtPointer,
        vtObject,
        vtInterface:
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
(* Note : This is a major slowdown! 7 fps becomes 300 fps without this!
      // See if this argument can be translated to an additional string :
      if _TryArgumentToString(Args[i], {var}ArgumentAsString) then
      begin
        // Insert the new contents in the format-string, directly after the current argument-position  :
        while not CharInSet(aStr[CurrentPercentageOffset - 1], ['d', 'p', 'x', 'X']) do
          Inc(CurrentPercentageOffset);

        // Make sure the argument doesn't interfere with the formatting-string :
        ArgumentAsString := ' (' + StringReplace(ArgumentAsString, '%', '%%', [rfReplaceAll]) + ')';
        Insert(ArgumentAsString, {var}aStr, CurrentPercentageOffset);

        Inc(CurrentPercentageOffset, Length(ArgumentAsString));
      end;
*)
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
  finally
    LeaveCriticalSection({var}DxbxLogLock);
  end;
end; // DxbxFormat

procedure DbgPrintf(aStr: string; Args: array of const; MayRenderArguments: Boolean = False);
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

var
  LineStr: string = '';

procedure printf(aStr: string);
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  // Collect strings into one line :
  LineStr := LineStr + aStr;
  // Check if it's not yet closed off with a newline :
  if LastChar(LineStr) = #10 then
  begin
    // Remove last newline :
    SetLength(LineStr, Length(LineStr) - 1);
    if LastChar(LineStr) = #13 then
      SetLength(LineStr, Length(LineStr) - 1);

    // Print normally (this will re-append the removed newline) :
    DbgPrintf(LineStr);
    // Start afresh :
    LineStr := '';
  end;
end;

procedure printf(aStr: string; Args: array of const);
begin
  printf(Format(aStr, Args));
end;

function sprintf(aBuffer: PAnsiChar; const aString: AnsiString): Integer; // overload;
begin
  Result := Length(aString);
  memcpy(aBuffer, @(aString[1]), Result);
  aBuffer[Result] := #0;
end;

function sprintf(aBuffer: PAnsiChar; const aString: AnsiString; Args: array of const): Integer; // overload;
begin
  Result := sprintf(aBuffer, AnsiString(Format(string(aString), Args)));
end;

procedure SetLogMode(aLogMode: TDebugMode = dmNone); export;
begin
  DebugMode := aLogMode;
  UpdateLogFlags;
end;

procedure CreateLogs(aDebugMode: TDebugMode; aOutputFileName: string = '');
begin
  WriteLog('Entered CreateLogs');

  case aDebugMode of
    dmNone:
      CloseLogs;

    dmConsole:
      try
        if not Assigned(ConsoleControl) then
          ConsoleControl := TConsoleControl.Create;

        ConsoleControl.SetWindowTitle('DXBX : Debug Console for ' + ParamStr(0)); // Application.Name?
        ConsoleControl.SetBackgroundColor(False, False, False, False); // = black
        ConsoleControl.SetForegroundColor(False, True, False, False); // = lime
        ConsoleControl.HideCursor;
      except
        FreeAndNil({var}ConsoleControl);
        raise Exception.Create('Could not create log console');
      end;

    dmFile:
      if not LogFileOpen then
      try
        if aOutputFileName = '' then
          aOutputFileName := DXBX_KERNEL_DEBUG_FILENAME;

        AssignFile({var}LogFile, aOutputFileName);

        Rewrite({var}LogFile);
        LogFileOpen := True;
      except
        raise Exception.Create('Could not create log file');
      end;
  end; // case aDebugMode

  // Make sure the entire logging stack knows about this mode (especially MayLog) :
  SetLogMode(aDebugMode);

  WriteLog('Started logging at ' + DateTimeToStr(Now));
  WriteLog('Dxbx version ' + _DXBX_VERSION + ' (svn revision ' + IntToStr(SvnRevision) + ')');
end; // CreateLogs

procedure CloseLogs;
begin
  WriteLog('Stop logging.');

  FreeAndNil(ConsoleControl);

  if LogFileOpen then
  begin
    CloseFile(LogFile);
    LogFileOpen := False;
  end;
end;


// Dxbx note : We don't use threadvar here (although that would be much easier),
// but TlsAlloc/TlsGetValue, because that doesn't interfere with the XBE's TLS!
var
  TlsIndex: Integer = 0;
type
  RCurrentThreadIDInsert = record
    Str: string;
  end;
  PCurrentThreadIDInsert = ^RCurrentThreadIDInsert;

procedure WriteLog(aText: string);
var
  i: Integer;
  CurrentThreadIDInsert: PCurrentThreadIDInsert;
begin
  EnterCriticalSection({var}DxbxLogLock);
  try
    try
//      OutputDebugString(PChar(aText));

      if Assigned(ConsoleControl) or LogFileOpen then
      begin
        i := System.Pos(':', aText);
        if (i > 0) and (i < 15) and (strncmp(PChar(aText), 'Emu', 3) = 0) then
        begin
          // Calculate the string to insert only once, as in some titles (like TechCertGame)
          // redetermining this on every call leads to crashes (don't know why that is) :
          begin
            if TlsIndex = 0 then
              TlsIndex := TlsAlloc();

            CurrentThreadIDInsert := PCurrentThreadIDInsert(TlsGetValue(TlsIndex));
            if CurrentThreadIDInsert = nil then
            begin
              New(CurrentThreadIDInsert);
              CurrentThreadIDInsert.Str := '(0x' + IntToHex(GetCurrentThreadID(), 1) + ')';
              TlsSetValue(TlsIndex, CurrentThreadIDInsert);
            end;
          end;

          // Prefix the text with the CurrentThreadID :
          Insert(CurrentThreadIDInsert.Str, {var}aText, i);
        end;

        if Assigned(ConsoleControl) then
          ConsoleControl.WriteTextLine(aText);

        if LogFileOpen then
        begin
          WriteLn({var}LogFile, aText);

          Flush({var}LogFile);
        end;
      end;
    except
    end;
  finally
    LeaveCriticalSection({var}DxbxLogLock);
  end;
end; // WriteLog

{ RLogStack }

const
  LOG_MAX_STRING_LENGTH = 50;

var
  LogEntryPool: PLogStack = nil;
  LogEntryPoolCount: Integer = 0;

// Get an entry from the pool (or create an entry is the pool is currently empty).
function GetLogEntry(const aLogRoot: PLogStack): PLogStack;
var
  CurrentRoot: PLogStack;
begin
  // Prevent other threads from acessing the pool, by nilling it out for a short while :
  Result := InterlockedExchangePointer({var}Pointer(LogEntryPool), {Exchange}nil);

  // See if the above action gave us an entry :
  if Assigned(Result) then
  begin
    // We use a loop here, to make sure we're doing this thread-safe :
    if Assigned(Result.Next) then
    repeat
      // We got an entry, so return the rest of the chain after it to the pool as fast as we can :
      CurrentRoot := LogEntryPool;
    until (InterlockedCompareExchangePointer({var}Pointer(LogEntryPool), {Exchange}Result.Next, {Comperand}CurrentRoot) = CurrentRoot);
  end
  else
  begin
    // If the pool was empty (or temporarily unavailable), allocate a new entry and continue with the initialization :
    New(Result);
    InterlockedIncrement({var}LogEntryPoolCount);
  end;

  Result.LogRoot := aLogRoot;
  // Clean out the other variables :
  Result.Next := nil;
  Result.LogName := '';
  Result.LogValue := '';
end;

// Return this entry to the pool
procedure ReturnLogEntry(const aEntry: PLogStack);
var
  CurrentRoot: PLogStack;
begin
  Assert(aEntry <> nil);
  Assert(aEntry.Next = nil);

  // We use a loop here, to make sure we're doing this thread-safe :
  repeat
    // Read the current root of the chain, and assume that will follow after this chain :
    CurrentRoot := LogEntryPool;
    aEntry.Next := CurrentRoot;

    // Try to put this entry's Root as the head of the list, retry if this fails :
  until (InterlockedCompareExchangePointer({var}Pointer(LogEntryPool), {Exchange}aEntry.LogRoot, {Comperand}CurrentRoot) = CurrentRoot);
end;

procedure FreeLogEntryPool;
var
  Loop, Next: PLogStack;
begin
  // Note : This is not thread-safe, which is alright as long as we only call it at shutdown
  Loop := LogEntryPool;
  LogEntryPool := nil;
  LogEntryPoolCount := 0;

  while Assigned(Loop) do
  begin
    Next := Loop.Next;
    Dispose(Loop);
    Loop := Next;
  end;
end;

{$IFDEF DXBX_TRACE_MEM}
var
  Prev_Mem: Int64 = 0;

procedure TraceMemory;
var
  Current, Delta: Int64;
  MemStatus: TMEMORYSTATUSEX;
begin
  MemStatus.dwLength := SizeOf(MemStatus);
  GlobalMemoryStatusEx({var}MemStatus);
  Current := MemStatus.ullTotalVirtual - MemStatus.ullAvailVirtual;

  Delta := Current - Prev_Mem;
  if Delta > 4096 then
  begin
    Prev_Mem := Current;
    WriteLog(Format('Traced a memory increase of %.0n bytes!', [0.0+Delta]));
  end;
end;
{$ENDIF}

function LogBegin(const aSymbolName: string; const aCategory: string = ''): PLogStack;
begin
{$IFDEF DXBX_TRACE_MEM}
  TraceMemory;
{$ENDIF}

  // Start the chain with a new entry that points to itself :
  Result := GetLogEntry(nil);
  Result.LogRoot := Result;
  // Set the Category and SymbolName and return the next entry to be filled (or finished) :
  Result.SetValue(aCategory);
  Result := Result.SetName(aSymbolName, '');
end;

function RLogStack.SetName(const aName, aType: string): PLogStack;
begin
  // Set the name (or type if no name was given) :
  if aName <> '' then
    LogName := aName
  else
    LogName := aType;
  // Retrieve the following entry and return that :
  Next := GetLogEntry(LogRoot);
  Result := Next;
end;

procedure RLogStack.SetValue(const aValue: string; const aDetails: string = '');
begin
  if aDetails <> '' then
    LogValue := aValue + ' (' + aDetails + ')'
  else
    LogValue := aValue;
end;

procedure RLogStack.SetValue(const aValue: UIntPtr; const aDetails: string = '');
begin
  SetValue('0x' + IntToHex(aValue, SizeOf(UIntPtr) * NIBBLES_PER_BYTE), aDetails);
end;

procedure RLogStack.SetHexValue(const aValue: DWORD; const aNrNibbles: Integer);
begin
  // TODO : Handle signed/unsigned difference
  if {(aValue < 0) or} (aValue > 9) then // Only add decimal representation when that's different from hexadecimal
    SetValue('0x' + IntToHex(aValue, aNrNibbles * NIBBLES_PER_BYTE), IntToStr(aValue))
  else
    SetValue('0x' + IntToHex(aValue, aNrNibbles * NIBBLES_PER_BYTE));
end;

function RLogStack._(const aValue: AnsiString; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'AnsiString');
  SetValue(string(aValue));
end;

function RLogStack._(const aValue: UnicodeString; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'UnicodeString');
  SetValue(string(aValue));
end;

function RLogStack._(const aValue: int; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'int');
  SetValue(IntToStr(aValue));
end;

function RLogStack._(const aValue: float; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'float');
  SetValue(FloatToString(aValue));
end;

function RLogStack._(const aValue: SHORT; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'SHORT');
  SetHexValue(aValue, SizeOf(aValue));
end;

function RLogStack._(const aValue: WORD; const aName: string): PlogStack;
begin
  Result := SetName(aName, 'WORD');
  SetHexValue(aValue, SizeOf(aValue));
end;

function RLogStack._(const aValue: DWORD; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'DWORD');
  SetHexValue(aValue, SizeOf(aValue));
end;

function RLogStack._(const aValue: BOOL; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'BOOL');
  if aValue = BOOL_FALSE then
    SetValue(UIntPtr(aValue), 'FALSE')
  else
    SetValue(UIntPtr(aValue), 'TRUE');
end;

function RLogStack._(const aValue: _BOOLEAN; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, '_BOOLEAN');
  if aValue = False then
    SetValue(UIntPtr(aValue), 'False')
  else
    SetValue(UIntPtr(aValue), 'True');
end;

function RLogStack._(const aValue: LARGE_INTEGER; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'LARGE_INTEGER');
  SetValue(IntToStr(aValue.QuadPart));
end;

function RLogStack._(const aValue: PVOID; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'PVOID');
  SetValue(UIntPtr(aValue));
end;

function RLogStack._(const aValue: LPCSTR; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'LPCSTR');
  SetValue(UIntPtr(aValue), '"' + string(PAnsiCharMaxLenToString(aValue, LOG_MAX_STRING_LENGTH)) + '"');
end;

function RLogStack._(const aValue: LPCWSTR; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'LPCWSTR');
  SetValue(UIntPtr(aValue), '"' + string(PWideCharMaxLenToString(aValue, LOG_MAX_STRING_LENGTH)) + '"');
end;

function RLogStack._(const aValue: PPOINT; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'PPOINT');
  if Assigned(aValue) then
    SetValue(UIntPtr(aValue), Format('%d,%d', [aValue.X, aValue.Y]))
  else
    SetValue(UIntPtr(aValue));
end;

function RLogStack._(const aValue: PRECT; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'PRECT');
  if Assigned(aValue) then
    SetValue(UIntPtr(aValue), Format('%d,%d %d,%d', [aValue.Left, aValue.Top, aValue.Right, aValue.Bottom]))
  else
    SetValue(UIntPtr(aValue));
end;

{$IFDEF DXBX_DLL}

function RLogStack._(const aValue: PLARGE_INTEGER; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'PLARGE_INTEGER');
  if Assigned(aValue) then
    SetValue(UIntPtr(aValue), IntToStr(aValue.QuadPart))
  else
    SetValue(UIntPtr(aValue));
end;

function RLogStack._(const aValue: PANSI_STRING; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'PANSI_STRING');
  if Assigned(aValue) then
    SetValue(UIntPtr(aValue), '"' + string(PSTRING_String(aValue)) + '"')
  else
    SetValue(UIntPtr(aValue));
end;

function RLogStack._(const aValue: POBJECT_ATTRIBUTES; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'POBJECT_ATTRIBUTES');
  if Assigned(aValue) then
    SetValue(UIntPtr(aValue), '"' + string(POBJECT_ATTRIBUTES_String(aValue)) + '"')
  else
    SetValue(UIntPtr(aValue));
end;

function RLogStack._(const aValue: FILE_INFORMATION_CLASS; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'FILE_INFORMATION_CLASS');
  SetValue(UIntPtr(aValue), FileInformationClassToString(aValue));
end;

function RLogStack._ACCESS_MASK(const aValue: ACCESS_MASK; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'ACCESS_MASK');
  SetValue(UIntPtr(aValue), AccessMaskToString(aValue));
end;

function RLogStack._FileAttributes(const aValue: ULONG; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'FileAttributes');
  SetValue(UIntPtr(aValue), FileAttributesToString(aValue));
end;

function RLogStack._CreateDisposition(const aValue: ULONG; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'CreateDisposition');
  SetValue(UIntPtr(aValue), CreateDispositionToString(aValue));
end;

function RLogStack._CreateOptions(const aValue: ULONG; const aName: string = ''): PLogStack;
begin
  Result := SetName(aName, 'CreateOptions');
  SetValue(UIntPtr(aValue), CreateOptionsToString(aValue));
end;

{$ENDIF DXBX_DLL}

procedure RLogStack.LogEnd();
var
  Loop: PLogStack;
  Str: string; // TODO : Retrieve a StringBuilder from a (thread-safe) pool to save on reallocating concatenations
  NameWidth: Integer;
begin
  // This LogStackEntry won't be processed, but instead it prints the entire stack :
  // First start with the header (accesible via LogRoot) :
  Str := LogRoot.LogName; // TODO : Handle redirects (marked with '>>') better
  if LogRoot.LogValue <> '' then
    Str := LogRoot.LogValue + ' : ' + Str;

  Loop := LogRoot.Next;
  if Loop = @Self then
    Str := Str + '();'
  else
  begin
    Str := Str + #13#10'(';
    // Determine how wide the name should be (and give it a minimum width, to keep the layout relatively steady) :
    NameWidth := 18;
    while Loop <> @Self do
    begin
      if NameWidth < Length(Loop.LogName) then
        NameWidth := Length(Loop.LogName);

      Loop := Loop.Next;
    end;

    // Print the complete stack of arguments :
    Loop := LogRoot.Next;
    while Loop <> @Self do
    begin
      Str := Str + Format(#13#10'   %-*s : %s', [NameWidth, Loop.LogName, Loop.LogValue]);
      Loop := Loop.Next;
    end;
    Str := Str + #13#10');';
  end;

  // Push it to the normal printing function :
  DbgPrintf(Str);

  // Return the entire chain back to the pool :
  ReturnLogEntry(@Self);
end;

initialization

  // Because we bypass Delphi's BeginThread, the memory manager has no idea it has to run thread-safe,
  // and thus skips all locking. When doing lots of allocations (like when our logging is turned on),
  // this will cause memory corruption with access violations as a result.
  // Simply setting IsMultiThread to True fixes all this!
  IsMultiThread := True;

  InitializeCriticalSection({var}DxbxLogLock);

  SetSpecifiedLogFlags(@g_DefaultLogFlags_Enabled, @g_DefaultLogFlags_Disabled);

finalization

  CloseLogs;
  FreeLogEntryPool;

end.
