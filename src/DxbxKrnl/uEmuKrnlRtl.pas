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
unit uEmuKrnlRtl;

{$INCLUDE Dxbx.inc}

{$DEFINE XBOX_CRITICAL_SECTION}

interface

uses
  // Delphi
  SysUtils,
  // Jedi Win32API
  JwaWinType,
  JwaWinBase,
  JwaWinNT,
  JwaNative,
  JwaNTStatus,
  // OpenXDK
  XboxKrnl,
  // Dxbx
  uTypes,
  uDxbxUtils,
  uLog,
  uDxbxKrnlUtils,
  uEmuFS,
  uEmuKrnl,
  uDxbxKrnl;

type
{$IFDEF XBOX_CRITICAL_SECTION}
  PRTL_CRITICAL_SECTION = XboxKrnl.PRTL_CRITICAL_SECTION;
{$ELSE}
  PRTL_CRITICAL_SECTION = JwaWinNT.PRTL_CRITICAL_SECTION;
{$ENDIF}

function xboxkrnl_RtlAnsiStringToUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PSTRING;
  AllocateDestinationString: UCHAR
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlAppendStringToString(
  DestinationString: PSTRING;
  AppendThisString: PSTRING
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlAppendUnicodeStringToString(
  DestinationString: PUNICODE_STRING;
  SourceString: PUNICODE_STRING
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlAppendUnicodeToString(
  Destination: PUNICODE_STRING;
  Source: LPCWSTR
  ): NTSTATUS; stdcall;
procedure xboxkrnl_RtlAssert(
  FailedAssertion: PVOID;
  FileName: PVOID;
  LineNumber: ULONG;
  Message_: P_CHAR
  ); stdcall;
procedure xboxkrnl_RtlCaptureContext(
  ContextRecord: PCONTEXT
  ); stdcall;
function xboxkrnl_RtlCaptureStackBackTrace(
  FramesToSkip: ULONG;
  FramesToCapture: ULONG;
  BackTrace: PPVOID;
  BackTraceHash: PULONG
  ): USHORT; stdcall;
function xboxkrnl_RtlCharToInteger(
  Str: PCSZ;
  Base: ULONG;
  Value: PULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlCompareMemory(
  Source1: PVOID;
  Source2: PVOID;
  Length: SIZE_T
  ): SIZE_T; stdcall;
function xboxkrnl_RtlCompareMemoryUlong(
  Source: PVOID;
  Length: ULONG;
  Value: ULONG
  ): ULONG; stdcall;
function xboxkrnl_RtlCompareString(
  String1: PSTRING;
  String2: PSTRING;
  CaseInsensitive: _BOOLEAN
  ): LONG; stdcall;
function xboxkrnl_RtlCompareUnicodeString(
  String1: PUNICODE_STRING;
  String2: PUNICODE_STRING;
  CaseInsensitive: _BOOLEAN
  ): LONG; stdcall;
procedure xboxkrnl_RtlCopyString(
  DestinationString: PSTRING;
  SourceString: PSTRING
  ); stdcall;
procedure xboxkrnl_RtlCopyUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PUNICODE_STRING
  ); stdcall;
function xboxkrnl_RtlCreateUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PWSTR
  ): _BOOLEAN; stdcall;
function xboxkrnl_RtlDowncaseUnicodeChar(
  Source: WCHAR
  ): WCHAR; stdcall;
function xboxkrnl_RtlDowncaseUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PUNICODE_STRING;
  AllocateDestinationString: _BOOLEAN
  ): NTSTATUS; stdcall;
procedure xboxkrnl_RtlEnterCriticalSection(
  CriticalSection: PRTL_CRITICAL_SECTION
  ); stdcall;
procedure xboxkrnl_RtlEnterCriticalSectionAndRegion(
  CriticalSection: PRTL_CRITICAL_SECTION
  ); stdcall;
function xboxkrnl_RtlEqualString(
  String1: PSTRING;
  String2: PSTRING;
  CaseInsensitive: _BOOLEAN
  ): _BOOLEAN; stdcall;
function xboxkrnl_RtlEqualUnicodeString(
  String1: PUNICODE_STRING;
  String2: PUNICODE_STRING;
  CaseInsensitive: _BOOLEAN
  ): _BOOLEAN; stdcall;
function xboxkrnl_RtlExtendedIntegerMultiply(
  Multiplicand: LARGE_INTEGER;
  Multiplier: LONG
  ): _LARGE_INTEGER; stdcall;
function xboxkrnl_RtlExtendedLargeIntegerDivide(
  Dividend: LARGE_INTEGER;
  Divisor: ULONG;
  Remainder: PULONG
  ): _LARGE_INTEGER; stdcall;
function xboxkrnl_RtlExtendedMagicDivide(
  Dividend: LARGE_INTEGER;
  MagicDivisor: LARGE_INTEGER;
  ShiftCount: CCHAR
  ): _LARGE_INTEGER; stdcall;
procedure xboxkrnl_RtlFillMemory(
  Destination: PVOID;
  Length: SIZE_T;
  Fill: UCHAR
  ); stdcall;
procedure xboxkrnl_RtlFillMemoryUlong(
  Destination: PVOID;
  Length: ULONG;
  Fill: ULONG
  ); stdcall;
procedure xboxkrnl_RtlFreeAnsiString(
  AnsiString: PANSI_STRING
  ); stdcall;
procedure xboxkrnl_RtlFreeUnicodeString(
  UnicodeString: PUNICODE_STRING
  ); stdcall;
procedure xboxkrnl_RtlGetCallersAddress(
  CallersAddress: PPVOID;
  CallersCaller: PPVOID
  ); stdcall;
procedure xboxkrnl_RtlInitAnsiString(
  DestinationString: PANSI_STRING; // OUT
  SourceString: PCSZ
); stdcall;
procedure xboxkrnl_RtlInitUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: LPCWSTR
  ); stdcall;
procedure xboxkrnl_RtlInitializeCriticalSection(
  CriticalSection: PRTL_CRITICAL_SECTION
  ); stdcall;
function xboxkrnl_RtlIntegerToChar(
  Value: ULONG;
  Base: ULONG;
  Length: ULONG;
  Str: PAnsiChar
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlIntegerToUnicodeString(
  Value: ULONG;
  Base: ULONG;
  Str: PUNICODE_STRING
  ): NTSTATUS; stdcall;
procedure xboxkrnl_RtlLeaveCriticalSection(
  CriticalSection: PRTL_CRITICAL_SECTION
  ); stdcall;
procedure xboxkrnl_RtlLeaveCriticalSectionAndRegion(
  CriticalSection: PRTL_CRITICAL_SECTION
  ); stdcall;
function xboxkrnl_RtlLowerChar(
  Character: _CHAR
  ): _CHAR; stdcall;
procedure xboxkrnl_RtlMapGenericMask(
  AccessMask: PACCESS_MASK;
  GenericMapping: PGENERIC_MAPPING
  ); stdcall;
procedure xboxkrnl_RtlMoveMemory(
  Destination: PVOID;
  Source: PVOID;
  Length: SIZE_T
  ); stdcall;
function xboxkrnl_RtlMultiByteToUnicodeN(
  UnicodeString: PWCH; // OUT
  MaxBytesInUnicodeString: ULONG;
  BytesInUnicodeString: PULONG; // OUT OPTIONAL
  MultiByteString: PCH;
  BytesInMultiByteString: ULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlMultiByteToUnicodeSize(
  BytesInUnicodeString: PULONG; // OUT
  MultiByteString: PCH;
  BytesInMultiByteString: ULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlNtStatusToDosError(
  Status: NTSTATUS
  ): ULONG; stdcall;
procedure xboxkrnl_RtlRaiseException(
  ExceptionRecord: PEXCEPTION_RECORD
  ); stdcall;
procedure xboxkrnl_RtlRaiseStatus(
  Status: NTSTATUS
  ); stdcall;
function xboxkrnl_RtlTimeFieldsToTime(
  TimeFields: PTIME_FIELDS;
  Time: PLARGE_INTEGER // OUT
  ): LONGBOOL; stdcall;
procedure xboxkrnl_RtlTimeToTimeFields(
  Time: PLARGE_INTEGER;
  TimeFields: PTIME_FIELDS // OUT
  ); stdcall;
function xboxkrnl_RtlTryEnterCriticalSection(
  CriticalSection: PRTL_CRITICAL_SECTION
  ): _BOOLEAN; stdcall;
function xboxkrnl_RtlUlongByteSwap(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  Source: ULONG
  ): ULONG; register;
function xboxkrnl_RtlUnicodeStringToAnsiString(
  DestinationString: PSTRING; // OUT
  SourceString: PUNICODE_STRING;
  AllocateDestinationString: _BOOLEAN
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlUnicodeStringToInteger(
  Str: PUNICODE_STRING;
  Base: ULONG;
  Value: PULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlUnicodeToMultiByteN(
  MultiByteString: PCH; // OUT
  MaxBytesInMultiByteString: ULONG;
  BytesInMultiByteString: PULONG; // OUT OPTIONAL
  UnicodeString: PWCH;
  BytesInUnicodeString: ULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlUnicodeToMultiByteSize(
  BytesInMultiByteString: PULONG;
  UnicodeString: PWSTR;
  BytesInUnicodeString: ULONG
  ): NTSTATUS; stdcall;
procedure xboxkrnl_RtlUnwind(
  TargetFrame: PVOID;
  TargetIp: PVOID;
  ExceptionRecord: PEXCEPTION_RECORD;
  ReturnValue: PVOID
  ); stdcall;
function xboxkrnl_RtlUpcaseUnicodeChar(
  SourceCharacter: WCHAR
  ): WCHAR; stdcall;
function xboxkrnl_RtlUpcaseUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PUNICODE_STRING;
  AllocateDestinationString: _BOOLEAN
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlUpcaseUnicodeToMultiByteN(
  MbString: PAnsiChar;
  MbSize: ULONG;
  ResultSize: PULONG;
  UnicodeString: PWSTR;
  UnicodeSize: ULONG
  ): NTSTATUS; stdcall;
function xboxkrnl_RtlUpperChar(
  Character: ANSICHAR
  ): ANSICHAR; stdcall;
procedure xboxkrnl_RtlUpperString(
  DestinationString: PSTRING;
  SourceString: PSTRING
  ); stdcall;
function xboxkrnl_RtlUshortByteSwap(
  FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  Source: USHORT
  ): USHORT; register;
function xboxkrnl_RtlWalkFrameChain(
  Callers: PPVOID;
  Count: ULONG;
  Flags: ULONG
  ): ULONG; stdcall;
procedure xboxkrnl_RtlZeroMemory(
  Destination: PVOID;
  Length: SIZE_T
  ); stdcall;
procedure xboxkrnl_RtlRip(
  Part1: PCSZ;
  Part2: PCSZ;
  Part3: PCSZ
  ); stdcall;
function xboxkrnl_RtlSnprintf(
  Buffer: P_char;
  BufferSize: size_t;
  FormatString: P_char
  // args
  ): NTSTATUS; cdecl; // INCOMPLETE_SIGNATURE
function xboxkrnl_RtlSprintf(
  Buffer: P_char;
  FormatString: P_char
  // args
  ): NTSTATUS; cdecl; // INCOMPLETE_SIGNATURE
function xboxkrnl_RtlVsnprintf(
  Buffer: P_char;
  BufferSize: size_t;
  FormatString: P_char
  // varargs
  ): NTSTATUS; cdecl; // INCOMPLETE_SIGNATURE
function xboxkrnl_RtlVsprintf(
  Buffer: P_char;
  FormatString: P_char
  // varargs
  ): NTSTATUS; cdecl; // INCOMPLETE_SIGNATURE

implementation

uses
  uXboxLibraryUtils;

const lfUnit = lfCxbx or lfKernel;


{$IFDEF XBOX_CRITICAL_SECTION}

// Critical Section implementation from ReactOS, modified to use Xbox1 data structure.
// See http://code.google.com/p/reactos-mirror/source/browse/trunk/reactos/lib/rtl/critical.c

// Call this when changing any Xbox1 field that can be mapped to the 'free' Native section :
procedure X_MimickNative(CriticalSection: PRTL_CRITICAL_SECTION);
begin
  // CriticalSection.Overlapped.DebugInfo := nil; // May not be safe to do
  CriticalSection.Overlapped.LockCount := CriticalSection.LockCount;
  // CriticalSection.Overlapped.LockSemaphore - do not overwrite, as we use this one for native synchronization
  CriticalSection.Overlapped.OwningThread := CriticalSection.OwningThread;
end;

procedure X_RtlInitializeCriticalSection(CriticalSection: PRTL_CRITICAL_SECTION);
begin
  ZeroMemory(CriticalSection, SizeOf(CriticalSection));
  CriticalSection.LockCount := -1;

  X_MimickNative(CriticalSection);
end;

function X_RtlTryEnterCriticalSection(CriticalSection: PRTL_CRITICAL_SECTION): _BOOLEAN;
begin
  // It's not ours
  Result := FALSE;

  // Try to take control
  if (InterlockedCompareExchange({var}CriticalSection.LockCount, 0, -1) = -1) then
  begin
    // It's ours
    CriticalSection.OwningThread := GetCurrentThreadId();
    CriticalSection.RecursionCount := 1;

    X_MimickNative(CriticalSection);
    Result := TRUE;
  end
  else if (CriticalSection.OwningThread = GetCurrentThreadId()) then
  begin
    // It's already ours
    InterlockedIncrement({var}CriticalSection.LockCount);
    Inc(CriticalSection.RecursionCount);

    X_MimickNative(CriticalSection);
    Result := TRUE;
  end;
end;

function RtlpCreateCriticalSectionSem(CriticalSection: PRTL_CRITICAL_SECTION): HANDLE;
begin
  // Check if we have an event
  Result := CriticalSection.Overlapped.LockSemaphore;
  if (Result = 0) then
  begin
    // No, so create it
    if (NtCreateSemaphore(@Result, SEMAPHORE_ALL_ACCESS, NULL, 0, 1) <> 0) then
      // We failed, this is bad...
      Exit;

    if (InterlockedCompareExchange({var}Integer(CriticalSection.Overlapped.LockSemaphore), Result, 0) <> 0) then
    begin
      NtClose(Result);  // somebody beat us to it
      Result := CriticalSection.Overlapped.LockSemaphore;
    end;

    X_MimickNative(CriticalSection);
  end;
end;

procedure X_RtlpWaitForCriticalSection(CriticalSection: PRTL_CRITICAL_SECTION);
var
  sem: HANDLE;
begin
  sem := RtlpCreateCriticalSectionSem(CriticalSection);
  while WaitForSingleObject(sem, {timeout=}5) <> STATUS_WAIT_0 do
    ;
end;

procedure X_RtlEnterCriticalSection(CriticalSection: PRTL_CRITICAL_SECTION);
var
  CurrentThreadId: DWORD;
begin
  CurrentThreadId := GetCurrentThreadId();
  // Try to Lock it
  if (InterlockedIncrement({var}CriticalSection.LockCount) <> 0) then
  begin
    X_MimickNative(CriticalSection);

    // We've failed to lock it! Does this thread
    // actually own it?
    if (CriticalSection.OwningThread = CurrentThreadId) then
    begin
      // You own it, so you'll get it when you're done with it! No need to
      // use the interlocked functions as only the thread who already owns
      // the lock can modify this data. */
      Inc(CriticalSection.RecursionCount);
      Exit;
    end;

    // NOTE - CriticalSection->OwningThread can be NULL here because changing
    //        this information is not serialized. This happens when thread a
    //        acquires the lock (LockCount == 0) and thread b tries to
    //        acquire it as well (LockCount == 1) but thread a hasn't had a
    //        chance to set the OwningThread! So it's not an error when
    //        OwningThread is NULL here!

    // We don't own it, so we must wait for it
    X_RtlpWaitForCriticalSection(CriticalSection);
  end;

  // Lock successful. Changing this information has not to be serialized because
  // only one thread at a time can actually change it (the one who acquired
  // the lock)!
  CriticalSection.OwningThread := CurrentThreadId;
  CriticalSection.RecursionCount := 1;

  X_MimickNative(CriticalSection);
end;

procedure X_RtlpUnWaitCriticalSection(CriticalSection: PRTL_CRITICAL_SECTION);
begin
  NtReleaseSemaphore(RtlpCreateCriticalSectionSem(CriticalSection), 1, NULL);
end;

procedure X_RtlLeaveCriticalSection(CriticalSection: PRTL_CRITICAL_SECTION);
begin
  // Decrease the Recursion Count. No need to do this atomically because only
  // the thread who holds the lock can call this function (unless the program
  // is totally screwed...
  Dec(CriticalSection.RecursionCount);
  if (CriticalSection.RecursionCount > 0) then
    // Someone still owns us, but we are free. This needs to be done atomically.
    InterlockedDecrement({var}CriticalSection.LockCount)
  else
  begin
    // Nobody owns us anymore. No need to do this atomically. See comment
    // above.
    CriticalSection.OwningThread := 0;
    // Was someone wanting us? This needs to be done atomically.
    if (InterlockedDecrement({var}CriticalSection.LockCount) >= 0) then
    begin
      // Let him have us
      X_RtlpUnWaitCriticalSection(CriticalSection);
    end;
  end;

  X_MimickNative(CriticalSection);
end;

{$ENDIF XBOX_CRITICAL_SECTION}

//

function xboxkrnl_RtlAnsiStringToUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PSTRING;
  AllocateDestinationString: UCHAR
): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('RtlAnsiStringToUnicodeString').
      _(DestinationString, 'DestinationString').
      _(SourceString, 'SourceString').
      _(AllocateDestinationString, ' AllocateDestinationString').
    LogEnd();

  Result := JwaNative.RtlAnsiStringToUnicodeString(DestinationString, SourceString, Boolean(AllocateDestinationString));
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlAppendStringToString(
  DestinationString: PSTRING;
  AppendThisString: PSTRING
  ): NTSTATUS; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('RtlAppendStringToString').
      _(DestinationString, 'DestinationString').
      _(AppendThisString, 'AppendThisString').
    LogEnd();

  Result := JwaNative.RtlAppendStringToString(DestinationString, AppendThisString);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlAppendUnicodeStringToString(
  DestinationString: PUNICODE_STRING;
  SourceString: PUNICODE_STRING
  ): NTSTATUS; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('RtlAppendUnicodeStringToString').
      _(DestinationString, 'DestinationString').
      _(SourceString, 'SourceString').
    LogEnd();

  Result := JwaNative.RtlAppendUnicodeStringToString(DestinationString, SourceString);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlAppendUnicodeToString(
  Destination: PUNICODE_STRING;
  Source: LPCWSTR
  ): NTSTATUS; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('RtlAppendUnicodeToString').
      _(Destination, 'Destination').
      _(Source, 'Source').
    LogEnd();

  Result := JwaNative.RtlAppendUnicodeToString(Destination, Source);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlAssert(
  FailedAssertion: PVOID;
  FileName: PVOID;
  LineNumber: ULONG;
  Message_: P_CHAR
  ); stdcall;
// Branch:shogun  Branch:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlAssert' +
        #13#10'(' +
        #13#10'   FailedAssertion           : 0x%.08X ("%s")' +
        #13#10'   FileName                  : 0x%.08X ("%s")' +
        #13#10'   LineNumber                : 0x%.08X (%d)' +
        #13#10'   Message                   : 0x%.08X ("%s")' +
        #13#10');',
        [Pointer(FailedAssertion), PAnsiCharMaxLenToString(FailedAssertion, 260),
         Pointer(FileName), PAnsiCharMaxLenToString(FileName, 260),
         LineNumber, LineNumber,
         Pointer(Message_), PAnsiCharMaxLenToString(Message_, 260)]);

  JwaNative.RtlAssert(FailedAssertion, FileName, LineNumber, Message_);

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlCaptureContext(
  ContextRecord: PCONTEXT
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('RtlCaptureContext').
      _(ContextRecord, 'ContextRecord').
    LogEnd();

  JwaNative.RtlCaptureContext(ContextRecord);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCaptureStackBackTrace(
  FramesToSkip: ULONG;
  FramesToCapture: ULONG;
  BackTrace: PPVOID;
  BackTraceHash: PULONG
  ): USHORT; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('RtlCaptureStackBackTrace').
      _(FramesToSkip, 'FramesToSkip').
      _(FramesToCapture, 'FramesToCapture').
      _(BackTrace, 'BackTrace').
      _(BackTraceHash, 'BackTraceHash').
    LogEnd();

  Result := Unimplemented('RtlCaptureStackBackTrace');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCharToInteger(
  Str: PCSZ;
  Base: ULONG;
  Value: PULONG
  ): NTSTATUS; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('RtlCharToInteger').
      _(LPCSTR(Str), 'Str').
      _(Base, 'Base').
      _(Value, 'Value').
    LogEnd();

  Result := JwaNative.RtlCharToInteger(Str, Base, Value);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCompareMemory(
  Source1: PVOID;
  Source2: PVOID;
  Length: SIZE_T
  ): SIZE_T; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('RtlCompareMemory').
      _(Source1, 'Source1').
      _(Source2, 'Source2').
      _(Length).
    LogEnd();

  Result := JwaNative.RtlCompareMemory(Source1, Source2, Length);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCompareMemoryUlong(
  Source: PVOID;
  Length: ULONG;
  Value: ULONG
  ): ULONG; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('RtlCompareMemoryUlong').
      _(Source, 'Source').
      _(Length, 'Length').
      _(Value, 'Value').
    LogEnd();

  Result := JwaNative.RtlCompareMemoryUlong(Source, Length, Value);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCompareString(
  String1: PSTRING;
  String2: PSTRING;
  CaseInsensitive: _BOOLEAN
  ): LONG; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('RtlCompareString').
      _(String1, 'String1').
      _(String2, 'String2').
      _(CaseInsensitive, 'CaseInsensitive').
    LogEnd();

  Result := JwaNative.RtlCompareString(String1, String2, CaseInsensitive);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCompareUnicodeString(
  String1: PUNICODE_STRING;
  String2: PUNICODE_STRING;
  CaseInsensitive: _BOOLEAN
  ): LONG; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('RtlCompareUnicodeString').
      _(String1, 'String1').
      _(String2, 'String2').
      _(CaseInsensitive, 'CaseInsensitive').
    LogEnd();

  Result := JwaNative.RtlCompareUnicodeString(String1, String2, CaseInsensitive);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlCopyString(
  DestinationString: PSTRING;
  SourceString: PSTRING
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('RtlCopyString').
      _(DestinationString, 'DestinationString').
      _(SourceString, 'SourceString').
    LogEnd();

  JwaNative.RtlCopyString(DestinationString, SourceString);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlCopyUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PUNICODE_STRING
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('RtlCopyUnicodeString').
      _(DestinationString, 'DestinationString').
      _(SourceString, 'SourceString').
    LogEnd();

  JwaNative.RtlCopyUnicodeString(DestinationString, SourceString);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlCreateUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PWSTR
  ): _BOOLEAN; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('RtlCreateUnicodeString').
      _(DestinationString, 'DestinationString').
      _(SourceString, 'SourceString').
    LogEnd();

  Result := JwaNative.RtlCreateUnicodeString(DestinationString, SourceString);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlDowncaseUnicodeChar(
  Source: WCHAR
  ): WCHAR; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('RtlDowncaseUnicodeChar').
      _(Source, 'Source').
    LogEnd();

  Result := JwaNative.RtlDowncaseUnicodeChar(Source);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlDowncaseUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PUNICODE_STRING;
  AllocateDestinationString: _BOOLEAN
  ): NTSTATUS; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('RtlDowncaseUnicodeString').
      _(DestinationString, 'DestinationString').
      _(SourceString, 'SourceString').
      _(AllocateDestinationString, 'AllocateDestinationString').
    LogEnd();

  Result := JwaNative.RtlDowncaseUnicodeString(DestinationString, SourceString, AllocateDestinationString);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlEnterCriticalSection
(
  CriticalSection: PRTL_CRITICAL_SECTION
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfDxbx or lfExtreme) then
    LogBegin('RtlEnterCriticalSection').
      _(CriticalSection, 'CriticalSection').
    LogEnd();

{$IFDEF XBOX_CRITICAL_SECTION}
  // Cxbx : This seems redundant, but xbox software doesn't always do it
  if(CriticalSection.LockCount = -1) then
    X_RtlInitializeCriticalSection(CriticalSection);

  X_RtlEnterCriticalSection(CriticalSection);
{$ELSE}
  // Cxbx : This seems redundant, but xbox software doesn't always do it
  if(CriticalSection.LockCount = -1) then
    JwaNative.RtlInitializeCriticalSection(CriticalSection);

  JwaNative.RtlEnterCriticalSection(CriticalSection);
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlEnterCriticalSectionAndRegion(
  CriticalSection: PRTL_CRITICAL_SECTION
  ); stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:50
begin
  EmuSwapFS(fsWindows);
  if MayLog(lfUnit or lfDxbx or lfExtreme) then
    LogBegin('RtlEnterCriticalSectionAndRegion').
      _(CriticalSection, 'CriticalSection').
    LogEnd();

{$IFDEF XBOX_CRITICAL_SECTION}
  X_RtlEnterCriticalSection(CriticalSection); // TODO : Do something better (region-related?)
{$ELSE}
  JwaNative.RtlEnterCriticalSection(CriticalSection); // TODO : Do something better (region-related?)
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlEqualString
(
  String1: PSTRING;
  String2: PSTRING;
  CaseInsensitive: _BOOLEAN
): _BOOLEAN; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('RtlEqualString').
      _(String1, 'String1').
      _(String2, 'String2').
      _(CaseInsensitive, 'CaseInsensitive').
    LogEnd();

  Result := JwaNative.RtlEqualString(String1, String2, CaseInsensitive);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlEqualUnicodeString(
  String1: PUNICODE_STRING;
  String2: PUNICODE_STRING;
  CaseInsensitive: _BOOLEAN
  ): _BOOLEAN; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('RtlEqualUnicodeString').
      _(String1, 'String1').
      _(String2, 'String2').
      _(CaseInsensitive, 'CaseInsensitive').
    LogEnd();

  Result := JwaNative.RtlEqualUnicodeString(String1, String2, CaseInsensitive);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlExtendedIntegerMultiply(
  Multiplicand: LARGE_INTEGER;
  Multiplier: LONG
  ): _LARGE_INTEGER; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('RtlExtendedIntegerMultiply').
      _(Multiplicand.QuadPart, 'Multiplicand').
      _(Multiplier, 'Multiplier').
    LogEnd();

  Result := _LARGE_INTEGER(JwaNative.RtlExtendedIntegerMultiply(Multiplicand, Multiplier));
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlExtendedLargeIntegerDivide(
  Dividend: LARGE_INTEGER;
  Divisor: ULONG;
  Remainder: PULONG
  ): _LARGE_INTEGER; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlExtendedLargeIntegerDivide' +
        #13#10'(' +
        #13#10'   Dividend                  : 0x%.08X' +
        #13#10'   Divisor                   : 0x%.08X' +
        #13#10'   Remainder                 : 0x%.08X' +
        #13#10');',
        [Dividend.QuadPart,
        Divisor,
        Remainder]);

  Result := _LARGE_INTEGER(JwaNative.RtlExtendedLargeIntegerDivide(Dividend, Divisor, Remainder));
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlExtendedMagicDivide(
  Dividend: LARGE_INTEGER;
  MagicDivisor: LARGE_INTEGER;
  ShiftCount: CCHAR
  ): _LARGE_INTEGER; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlExtendedMagicDivide' +
        #13#10'(' +
        #13#10'   Dividend                  : 0x%.08X' +
        #13#10'   MagicDivisor              : 0x%.08X' +
        #13#10'   ShiftCount                : 0x%.08X' +
        #13#10');',
        [Dividend.QuadPart,
        MagicDivisor.QuadPart,
        ShiftCount]);

  Result := _LARGE_INTEGER(JwaNative.RtlExtendedMagicDivide(Dividend, MagicDivisor, JwaWinType.CCHAR(ShiftCount)));
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlFillMemory(
  Destination: PVOID;
  Length: SIZE_T;
  Fill: UCHAR
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlFillMemory' +
        #13#10'(' +
        #13#10'   Destination               : 0x%.08X' +
        #13#10'   Length                    : 0x%.08X' +
        #13#10'   Fill                      : 0x%.08X' +
        #13#10');',
        [Destination,
        Length,
        Fill]);

  JwaNative.RtlFillMemory(Destination, Length, Fill);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlFillMemoryUlong(
  Destination: PVOID;
  Length: ULONG;
  Fill: ULONG
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlFillMemoryUlong' +
        #13#10'(' +
        #13#10'   Destination               : 0x%.08X' +
        #13#10'   Length                    : 0x%.08X' +
        #13#10'   Fill                      : 0x%.08X' +
        #13#10');',
        [Destination,
        Length,
        Fill]);

  JwaNative.RtlFillMemoryUlong(Destination, Length, Fill);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlFreeAnsiString(
  AnsiString: PANSI_STRING
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlFreeAnsiString' +
        #13#10'(' +
        #13#10'   AnsiString                : 0x%.08X' +
        #13#10');',
        [AnsiString]);

  JwaNative.RtlFreeAnsiString(AnsiString);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlFreeUnicodeString(
  UnicodeString: PUNICODE_STRING
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlFreeUnicodeString' +
        #13#10'(' +
        #13#10'   UnicodeString                : 0x%.08X' +
        #13#10');',
        [UnicodeString]);

  JwaNative.RtlFreeUnicodeString(UnicodeString);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlGetCallersAddress(
  CallersAddress: PPVOID;
  CallersCaller: PPVOID
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlGetCallersAddress' +
        #13#10'(' +
        #13#10'   CallersAddress                : 0x%.08X' +
        #13#10'   CallersCaller                : 0x%.08X' +
        #13#10');',
        [CallersAddress,
        CallersCaller]);

  JwaNative.RtlGetCallersAddress(CallersAddress, CallersCaller);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlInitAnsiString
(
  DestinationString: PANSI_STRING; // OUT
  SourceString: PCSZ
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlInitAnsiString' +
        #13#10'(' +
        #13#10'   DestinationString   : 0x%.08X' +
        #13#10'   SourceString        : 0x%.08X ("%s")' +
        #13#10');',
        [DestinationString,
         SourceString, AnsiString(SourceString)]);

  JwaNative.RtlInitAnsiString(DestinationString, SourceString);

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlInitUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: LPCWSTR
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlInitUnicodeString' +
        #13#10'(' +
        #13#10'   DestinationString            : 0x%.08X' +
        #13#10'   SourceString                 : 0x%.08X' +
        #13#10');',
        [DestinationString,
        SourceString]);

  JwaNative.RtlInitUnicodeString(DestinationString, SourceString);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlInitializeCriticalSection
(
  CriticalSection: PRTL_CRITICAL_SECTION
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlInitializeCriticalSection' +
             #13#10'(' +
             #13#10'   CriticalSection              : 0x%.08X' +
             #13#10');',
             [CriticalSection]);

{$IFDEF XBOX_CRITICAL_SECTION}
  X_RtlInitializeCriticalSection(CriticalSection);
{$ELSE}
  JwaNative.RtlInitializeCriticalSection(CriticalSection);
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlIntegerToChar(
  Value: ULONG;
  Base: ULONG;
  Length: ULONG;
  Str: PAnsiChar
  ): NTSTATUS; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlIntegerToChar' +
        #13#10'(' +
        #13#10'   Value                        : 0x%.08X' +
        #13#10'   Base                         : 0x%.08X' +
        #13#10'   Length                       : 0x%.08X' +
        #13#10'   Str                          : 0x%.08X' +
        #13#10');',
        [Value,
        Base,
        Length,
        Str]);

  Result := JwaNative.RtlIntegerToChar(Value, Base, Length, Str);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlIntegerToUnicodeString(
  Value: ULONG;
  Base: ULONG;
  Str: PUNICODE_STRING
  ): NTSTATUS; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlIntegerToUnicodeString' +
        #13#10'(' +
        #13#10'   Value                        : 0x%.08X' +
        #13#10'   Base                         : 0x%.08X' +
        #13#10'   Str                          : 0x%.08X' +
        #13#10');',
        [Value,
        Base,
        Str]);

  Result := JwaNative.RtlIntegerToUnicodeString(Value, Base, Str);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlLeaveCriticalSection
(
  CriticalSection: PRTL_CRITICAL_SECTION
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit and lfDxbx and lfTrace) then
    DbgPrintf('EmuKrnl : RtlLeaveCriticalSection' +
        #13#10'(' +
        #13#10'   CriticalSection              : 0x%.08X' +
        #13#10');',
        [CriticalSection]);

{$IFDEF XBOX_CRITICAL_SECTION}
  X_RtlLeaveCriticalSection(CriticalSection);
{$ELSE}
  JwaNative.RtlLeaveCriticalSection(CriticalSection);
{$ENDIF}
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlLeaveCriticalSectionAndRegion(
  CriticalSection: PRTL_CRITICAL_SECTION
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:50
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit and lfDxbx and lfTrace) then
    DbgPrintf('EmuKrnl : RtlLeaveCriticalSectionAndRegion' +
        #13#10'(' +
        #13#10'   CriticalSection              : 0x%.08X' +
        #13#10');',
        [CriticalSection]);

{$IFDEF XBOX_CRITICAL_SECTION}
  X_RtlLeaveCriticalSection(CriticalSection); // TODO : Do something better (region-related?)
{$ELSE}
  JwaNative.RtlLeaveCriticalSection(CriticalSection); // TODO : Do something better (region-related?)
{$ENDIF}
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlLowerChar(Character: _CHAR): _CHAR; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlLowerChar(%c);', [Character]);

  Result := tolower(Character);

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlMapGenericMask(
  AccessMask: PACCESS_MASK;
  GenericMapping: PGENERIC_MAPPING
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlMapGenericMask' +
        #13#10'(' +
        #13#10'   AccessMask                   : 0x%.08X' +
        #13#10'   GenericMapping               : 0x%.08X' +
        #13#10');',
        [AccessMask,
        GenericMapping]);

  JwaNative.RtlMapGenericMask(AccessMask, GenericMapping);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlMoveMemory(
  Destination: PVOID;
  Source: PVOID;
  Length: SIZE_T
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlMoveMemory' +
        #13#10'(' +
        #13#10'   Destination                  : 0x%.08X' +
        #13#10'   Source                       : 0x%.08X' +
        #13#10'   Length                       : 0x%.08X' +
        #13#10');',
        [Destination,
        Source,
        Length]);

  JwaNative.RtlMoveMemory(Destination, Source, Length);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlMultiByteToUnicodeN(
  UnicodeString: PWCH; // OUT
  MaxBytesInUnicodeString: ULONG;
  BytesInUnicodeString: PULONG; // OUT OPTIONAL
  MultiByteString: PCH;
  BytesInMultiByteString: ULONG
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  MaxChars: ULONG;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuKrnl : RtlMultiByteToUnicodeN' +
           #13#10'(' +
           #13#10'   UnicodeString          : 0x%.08X' +
           #13#10'   MaxBytesInUnicodeString: 0x%.08X' +
           #13#10'   BytesInUnicodeString   : 0x%.08X' +
           #13#10'   MultiByteString        : 0x%.08X' +// ("%s")' +
           #13#10'   BytesInMultiByteString : 0x%.08X' +
           #13#10');',
           [Pointer(UnicodeString), MaxBytesInUnicodeString,
            BytesInUnicodeString,
            Pointer(MultiByteString), {AnsiString(MultiByteString), }BytesInMultiByteString]);

  MaxChars := MaxBytesInUnicodeString div SizeOf(WideChar);
  if MaxChars > BytesInMultiByteString then
    MaxChars := BytesInMultiByteString;

  if Assigned(BytesInUnicodeString) then
    BytesInUnicodeString^ := MaxChars;

  while MaxChars > 0 do
  begin
    // Just zero-extend all Ansi characters :
    UnicodeString^ := WideChar(MultiByteString^);
    Inc(UnicodeString);
    Inc(MultiByteString);
    Dec(MaxChars);
  end;

  Result := STATUS_SUCCESS;

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlMultiByteToUnicodeSize(
  BytesInUnicodeString: PULONG; // OUT
  MultiByteString: PCH;
  BytesInMultiByteString: ULONG
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuKrnl : RtlMultiByteToUnicodeN' +
           #13#10'(' +
           #13#10'   BytesInUnicodeString   : 0x%.08X' +
           #13#10'   MultiByteString        : 0x%.08X' +// ("%s")' +
           #13#10'   BytesInMultiByteString : 0x%.08X' +
           #13#10');',
           [BytesInUnicodeString,
            Pointer(MultiByteString), {AnsiString(MultiByteString), }BytesInMultiByteString]);

  BytesInUnicodeString^ := BytesInMultiByteString * SizeOf(WideChar);

  Result := STATUS_SUCCESS;

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlNtStatusToDosError
(
  Status: NTSTATUS
): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlNtStatusToDosError' +
             #13#10'(' +
             #13#10'   Status              : 0x%.08X' +
             #13#10');',
             [Status]);

  Result := JwaNative.RtlNtStatusToDosError(Status);

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlRaiseException(
  ExceptionRecord: PEXCEPTION_RECORD
  ); stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('RtlRaiseException');
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlRaiseStatus(
  Status: NTSTATUS
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlRaiseStatus' +
        #13#10'(' +
        #13#10'   Status                       : 0x%.08X' +
        #13#10');',
        [Status]);

  JwaNative.RtlRaiseStatus(Status);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlTimeFieldsToTime
(
  TimeFields: PTIME_FIELDS;
  Time: PLARGE_INTEGER // OUT
): LONGBOOL; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlTimeFieldsToTime' +
        #13#10'(' +
        #13#10'   TimeFields          : 0x%.08X' +
        #13#10'   Time                : 0x%.08X' +
        #13#10');',
        [TimeFields, Time]);

  Result := JwaNative.RtlTimeFieldsToTime(TimeFields, Time);

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlTimeToTimeFields
(
  Time: PLARGE_INTEGER;
  TimeFields: PTIME_FIELDS // OUT
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlTimeToTimeFields' +
        #13#10'(' +
        #13#10'   Time                : 0x%.08X (%d)' +
        #13#10'   TimeFields          : 0x%.08X' +
        #13#10');',
        [Time, QuadPart(Time), TimeFields]);

  JwaNative.RtlTimeToTimeFields(Time, TimeFields);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlTryEnterCriticalSection
(
  CriticalSection: PRTL_CRITICAL_SECTION
): _BOOLEAN; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlTryEnterCriticalSection' +
        #13#10'(' +
        #13#10'   CriticalSection     : 0x%.08X' +
        #13#10');',
        [CriticalSection]);

{$IFDEF XBOX_CRITICAL_SECTION}
  Result := X_RtlTryEnterCriticalSection(CriticalSection);
{$ELSE}
  Result := JwaNative.RtlTryEnterCriticalSection(CriticalSection);
{$ENDIF}

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUlongByteSwap(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}Source: ULONG
  ): ULONG; register; // fastcall simulation - See Translation guide
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlRaiseStatus' +
        #13#10'(' +
        #13#10'   Source                       : 0x%.08X' +
        #13#10');',
        [Source]);

  Result := JwaNative.RtlUlongByteSwap(Source);
  EmuSwapFS(fsXbox);
  asm int 3 end; // REMOVE THIS AFTER VALIDATING fastcall (caller fills EDX, ECX and stack)!
end;

function xboxkrnl_RtlUnicodeStringToAnsiString
(
  DestinationString: PSTRING; // OUT
  SourceString: PUNICODE_STRING;
  AllocateDestinationString: _BOOLEAN
): NTSTATUS; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlUnicodeStringToAnsiString' +
        #13#10'(' +
        #13#10'   DestinationString         : 0x%.08X' +
        #13#10'   SourceString              : 0x%.08X ("%s")' +
        #13#10'   AllocateDestinationString : 0x%.08X' +
        #13#10');',
        [DestinationString,
         SourceString, PUNICODE_STRING_String(SourceString),
         AllocateDestinationString]);

  Result := JwaNative.RtlUnicodeStringToAnsiString(DestinationString, SourceString, AllocateDestinationString);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUnicodeStringToInteger(
  Str: PUNICODE_STRING;
  Base: ULONG;
  Value: PULONG
  ): NTSTATUS; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlUnicodeStringToInteger' +
        #13#10'(' +
        #13#10'   Str                       : 0x%.08X ("%s")' +
        #13#10'   Base                      : 0x%.08X' +
        #13#10'   Value                     : 0x%.08X' +
        #13#10');',
        [Str, PUNICODE_STRING_String(Str), Base, Value]);

  Result := JwaNative.RtlUnicodeStringToInteger(Str, Base, Value);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUnicodeToMultiByteN(
  MultiByteString: PCH; // OUT
  MaxBytesInMultiByteString: ULONG;
  BytesInMultiByteString: PULONG; // OUT OPTIONAL
  UnicodeString: PWCH;
  BytesInUnicodeString: ULONG
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  MaxChars: ULONG;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuKrnl : RtlUnicodeToMultiByteN' +
      #13#10'(' +
      #13#10'   MultiByteString          : 0x%.08X' +
      #13#10'   MaxBytesInMultiByteString: 0x%.08X' +
      #13#10'   BytesInMultiByteString   : 0x%.08X' +
      #13#10'   UnicodeString            : 0x%.08X' +// ("%s")' +
      #13#10'   BytesInUnicodeString     : 0x%.08X' +
      #13#10');',
      [Pointer(MultiByteString), MaxBytesInMultiByteString,
       BytesInMultiByteString,
       Pointer(UnicodeString), {WideString(UnicodeString), }BytesInUnicodeString]);

  MaxChars := BytesInUnicodeString div SizeOf(WideChar);
  if MaxChars > MaxBytesInMultiByteString then
    MaxChars := MaxBytesInMultiByteString;

  if Assigned(BytesInMultiByteString) then
    BytesInMultiByteString^ := MaxChars;

  while MaxChars > 0 do
  begin
    if UnicodeString^ > #255 then
      MultiByteString^ := '?'
    else
      MultiByteString^ := AnsiChar(UnicodeString^);

    Inc(UnicodeString);
    Inc(MultiByteString);
    Dec(MaxChars);
  end;

  Result := STATUS_SUCCESS;

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUnicodeToMultiByteSize(
  BytesInMultiByteString: PULONG;
  UnicodeString: PWSTR;
  BytesInUnicodeString: ULONG
  ): NTSTATUS; stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlUnicodeToMultiByteSize' +
        #13#10'(' +
        #13#10'   BytesInMultiByteString    : 0x%.08X' +
        #13#10'   UnicodeString             : 0x%.08X' +// ("%s")' +
        #13#10'   BytesInUnicodeString      : 0x%.08X' +
        #13#10');',
        [BytesInMultiByteString, Pointer(UnicodeString), {PWideChar(UnicodeString), }BytesInUnicodeString]);

  BytesInMultiByteString^ := BytesInUnicodeString div SizeOf(WideChar);

  Result := STATUS_SUCCESS;

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlUnwind(
  TargetFrame: PVOID;
  TargetIp: PVOID;
  ExceptionRecord: PEXCEPTION_RECORD;
  ReturnValue: PVOID
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlUnwind' +
        #13#10'(' +
        #13#10'   TargetFrame                  : 0x%.08X' +
        #13#10'   TargetIp                     : 0x%.08X' +
        #13#10'   ExceptionRecord              : 0x%.08X' +
        #13#10'   ReturnValue                  : 0x%.08X' +
        #13#10');',
        [TargetFrame,
        TargetIp,
        ExceptionRecord,
        ReturnValue]);

  JwaNative.RtlUnwind(TargetFrame, TargetIp, ExceptionRecord, ReturnValue);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUpcaseUnicodeChar(
  SourceCharacter: WCHAR
  ): WCHAR; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlUpcaseUnicodeChar' +
        #13#10'(' +
        #13#10'   SourceCharacter              : 0x%.08X' +
        #13#10');',
        [SourceCharacter]);

  Result := JwaNative.RtlUpcaseUnicodeChar(SourceCharacter);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUpcaseUnicodeString(
  DestinationString: PUNICODE_STRING;
  SourceString: PUNICODE_STRING;
  AllocateDestinationString: _BOOLEAN
  ): NTSTATUS; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlUpcaseUnicodeString' +
        #13#10'(' +
        #13#10'   DestinationString            : 0x%.08X' +
        #13#10'   SourceString                 : 0x%.08X' +
        #13#10'   AllocateDestinationString    : 0x%.08X' +
        #13#10');',
        [DestinationString,
        SourceString,
        AllocateDestinationString]);

  Result := JwaNative.RtlUpcaseUnicodeString(DestinationString, SourceString, Boolean(AllocateDestinationString));
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUpcaseUnicodeToMultiByteN(
  MbString: PAnsiChar;
  MbSize: ULONG;
  ResultSize: PULONG;
  UnicodeString: PWSTR;
  UnicodeSize: ULONG
  ): NTSTATUS; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlUpcaseUnicodeToMultiByteN' +
        #13#10'(' +
        #13#10'   MbString                     : 0x%.08X' +
        #13#10'   MbSize                       : 0x%.08X' +
        #13#10'   ResultSize                   : 0x%.08X' +
        #13#10'   UnicodeString                : 0x%.08X' +
        #13#10'   UnicodeSize                  : 0x%.08X' +
        #13#10');',
        [MbString,
        MbSize,
        ResultSize,
        UnicodeString,
        UnicodeSize]);

  Result := JwaNative.RtlUpcaseUnicodeToMultiByteN(MbString, MbSize, {var}ResultSize^, UnicodeString, UnicodeSize);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUpperChar(
  Character: ANSICHAR
  ): ANSICHAR; stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlUpperChar' +
        #13#10'(' +
        #13#10'   Character                    : 0x%.08X' +
        #13#10');',
        [Character]);

  Result := JwaNative.RtlUpperChar(Character);
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlUpperString(
  DestinationString: PSTRING;
  SourceString: PSTRING
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlUpperString' +
        #13#10'(' +
        #13#10'   DestinationString            : 0x%.08X' +
        #13#10'   SourceString                 : 0x%.08X' +
        #13#10');',
        [DestinationString,
        SourceString]);

  JwaNative.RtlUpperString(DestinationString, SourceString);
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlUshortByteSwap(
  {0 EAX}FASTCALL_FIX_ARGUMENT_TAKING_EAX: DWORD;
  {2 EDX}FASTCALL_FIX_ARGUMENT_TAKING_EDX: DWORD;
  {1 ECX}Source: USHORT
  ): USHORT; register; // fastcall simulation - See Translation guide
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlUshortByteSwap' +
        #13#10'(' +
        #13#10'   Source                       : 0x%.08X' +
        #13#10');',
        [Source]);

  Result := JwaNative.RtlUshortByteSwap(Source);
  EmuSwapFS(fsXbox);
  asm int 3 end; // REMOVE THIS AFTER VALIDATING fastcall (caller fills EDX, ECX and stack)!
end;

function xboxkrnl_RtlWalkFrameChain(
  Callers: PPVOID;
  Count: ULONG;
  Flags: ULONG
  ): ULONG; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlWalkFrameChain');
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_RtlZeroMemory(
  Destination: PVOID;
  Length: SIZE_T
  ); stdcall;
// Source:JwaNative  Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuKrnl : RtlZeroMemory' +
        #13#10'(' +
        #13#10'   Destination                  : 0x%.08X' +
        #13#10'   Length                       : 0x%.08X' +
        #13#10');',
        [Destination,
        Length]);

  JwaNative.RtlZeroMemory(Destination, Length);
  EmuSwapFS(fsXbox);
end;

// RtlRip:
// Traps to the debugger with a certain message, then crashes.
//
// New to the XBOX.
procedure xboxkrnl_RtlRip(
  Part1: PCSZ;
  Part2: PCSZ;
  Part3: PCSZ
  ); stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('RtlRip');
  EmuSwapFS(fsXbox);
end;

// Returns length in bytes of buffer filling. -1 means buffer overflow
function xboxkrnl_RtlSnprintf(
  Buffer: P_char;
  BufferSize: size_t;
  FormatString: P_char
  // args
  ): NTSTATUS; cdecl;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlSnprintf');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlSprintf(
  Buffer: P_char;
  FormatString: P_char
  // args
  ): NTSTATUS; cdecl;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlSprintf');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlVsnprintf(
  Buffer: P_char;
  BufferSize: size_t;
  FormatString: P_char
  // varargs
  ): NTSTATUS; cdecl;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlVsnprintf');
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_RtlVsprintf(
  Buffer: P_char;
  FormatString: P_char
  // varargs
  ): NTSTATUS; cdecl;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Result := Unimplemented('RtlVsprintf');
  EmuSwapFS(fsXbox);
end;


end.
