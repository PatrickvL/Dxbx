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
unit uTime;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows, // TTimeZoneInformation
  MMSystem; // timeBeginPeriod

type
  TCTime = Int64;

const
  CTimeZero = 25569; //EncodeDate(1970, 1, 1);
  SecPerDay = 60 * 60 * 24;

function CTimeToDateTime(CTime: TCTime): TDateTime;
function DateTimeToCTime(DateTime: TDateTime): TCTime;
function WideStringToString(const ws: WideString; codePage: Word): AnsiString;
//function StringToWideString(const s: AnsiString; codePage: Word): WideString;  // NOT WORKING IN D2009
function Bias: Integer;

const
  MillisecondsPerSecond = 1000;

type
  DxbxTimer = record
    PerformanceCounterFrequency: LARGE_INTEGER;
    CurrentPerformanceCounter: LARGE_INTEGER;
    PreviousPerformanceCounter: LARGE_INTEGER;
    TicksBetweenTimeOuts: Integer;
    TicksPerSleep1: Integer;
    procedure InitTicks(const aTicksToWait: Integer);
    procedure InitMilliseconds(const aMillisecondsToWait: Single);
    procedure InitFPS(const aFramesPerSecond: Integer);
    procedure Wait;
  end;

implementation

procedure DxbxTimer.InitTicks(const aTicksToWait: Integer);
begin
  QueryPerformanceFrequency({var}TLargeInteger(PerformanceCounterFrequency));
  QueryPerformanceCounter({var}TLargeInteger(CurrentPerformanceCounter));
  PreviousPerformanceCounter.QuadPart := 0;
  TicksBetweenTimeOuts := aTicksToWait;

  // Calculate how many ticks pass during Sleep(1), by using the Performance counter
  // frequency (which is measured in ticks per second) and dividing that by 1000
  // to get the number of ticks per millisecond (which is the unit of time Sleep
  // works with) :
  TicksPerSleep1 := Integer(PerformanceCounterFrequency.QuadPart div MillisecondsPerSecond);

  // However, Sleep(1) actually sleeps around 1-2 ms (*if* timeBeginPeriod(1) is called,
  // otherwise it would Sleep almost 10 ms!). If we worked with the lower bound, any deviation
  // would mean we would overshoot the waiting period. Using the average (1,5 ms) might sound
  // better, but still has the same behaviour. Only when we use the upper bound (2 ms), the risk
  // of overshooting our waiting period is reduced to near-never. So factor that in here :
  TicksPerSleep1 := TicksPerSleep1 * 2;

  // Raise the accuracy of Sleep to it's maximum :
  timeBeginPeriod(1);
  // Dxbx Note : We really shouldn't do this, as you can read about on
  // http://blogs.msdn.com/b/larryosterman/archive/2009/09/02/what-s-the-difference-between-gettickcount-and-timegettime.aspx
  // Instead, we could use NtDelayExecutionThread as an alternative (and hopefully accurate)
  // timing mechanism (see below).

  // TODO : Add a finalizer with timeEndPeriod(1);
end;

procedure DxbxTimer.InitMilliseconds(const aMillisecondsToWait: Single);
begin
  // Determine the number of counts (ticks) per second :
  QueryPerformanceFrequency({var}TLargeInteger(PerformanceCounterFrequency));
  // Initialize the timer by calculating the number of ticks this takes :
  InitTicks(Trunc(aMillisecondsToWait * PerformanceCounterFrequency.QuadPart / MillisecondsPerSecond));
  // Say, there are 5,000,000 ticks per second, that would be 5,000 ticks per millisecond,
  // which would let 16.66666... milliseconds become 83,333 ticks.
end;

procedure DxbxTimer.InitFPS(const aFramesPerSecond: Integer);
begin
  // Initialize the FPS counter by calculating the (floating point) number of milliseconds that takes :
  InitMilliseconds(MillisecondsPerSecond / aFramesPerSecond); // 60 Hz becomes 16.66666... milliseconds
end;

procedure DxbxTimer.Wait;
var
  TicksPassed: Integer;
  TicksToWait: Integer;
//  i: Integer;
begin
  // Note : Even though we used timeBeginPeriod, Sleep is not really accurate.
  // So we do what VirtualDub does (http://www.virtualdub.org/blog/pivot/entry.php?id=272)
  // we make this thread run at THREAD_PRIORITY_HIGHEST, and take care of the frequency and
  // accuracy ourselves here.
  //
  // See http://www.geisswerks.com/ryan/FAQS/timing.html).

  if (PreviousPerformanceCounter.QuadPart <> 0) then
  begin
    while True do
    begin
      // Get current performance counter value :
      QueryPerformanceCounter({var}TLargeInteger(CurrentPerformanceCounter));

      // Calculate how many ticks have passed since last timer expiration :
      TicksPassed := Integer(CurrentPerformanceCounter.QuadPart - PreviousPerformanceCounter.QuadPart);

      // Safeguard against a time wrap (this can presumably happen sometimes with Performance counters) :
      if (TicksPassed <= 0) then
        Break;

      // Calculate how many ticks we still have to wait :
      TicksToWait := TicksBetweenTimeOuts - TicksPassed;
      // Stop this loop when the wait time's up :
      if (TicksToWait <= 0) then
        Break;

      // Check if the number of tick we still have to wait, is more than
      // the number of ticks that would pass when doing a Sleep(1) :
      if (TicksToWait > TicksPerSleep1) then
      begin
        // In this case, we can do the most efficient wait, by Sleep()ing
        // this thread, which costs the least amount of CPU overhead.
        // The number of milliseconds we should wait for can be calculated
        // by dividing the number of TicksToWait by the number of ticks
        // that passes during a Sleep(1). This way, we Sleep as long as we
        // possibly can, without risking 'overshooting' our waiting period.
        // To be safe, we *do* safeguard against EDivByZero, by testing
        // that the previously calculated TicksPerSleep1 is indeed positive :
        if TicksPerSleep1 > 0 then
          Sleep(TicksToWait div TicksPerSleep1)
        else
          // If somehow TicksPerSleep1 became 0 (or less!), just Sleep
          // 1 millisecond (or 2, depending on the system's deviation),
          // which results in a maximum frequency of about 500 updates
          // per second. The alternative Sleep(0), as done below, eats
          // up way to much CPU time, so that's not an option here.
          Sleep(1);
      end
      else
        // If the TicksToWait is less then the time for a Sleep(1),
        // then just give up this timeslice. If this happens a lot,
        // it will incur much CPU usage (with all the thread switching
        // going on), but it would at least spend a tiny amount of time:
        Sleep(0); // This causes thread to give up its timeslice
        // TODO : What about pausing the cpu a little (rep nop) instead?
    end; // while True
  end;

  PreviousPerformanceCounter := CurrentPerformanceCounter;
end; // Wait

function Bias: Integer;
var
  TimeZone: TTimeZoneInformation;
begin
  GetTimeZoneInformation(TimeZone);
  Result := TimeZone.Bias * 60;
end;

function CTimeToDateTime(CTime: TCTime): TDateTime;
begin
  Result := CTimeZero + (CTime - Bias) / SecPerDay;
end;

function DateTimeToCTime(DateTime: TDateTime): TCTime;
begin
  Result := Round((DateTime - CTimeZero) * SecPerDay + Bias);
end;

{:Converts Unicode string to Ansi string using specified code page.
  @param   ws       Unicode string.
  @param   codePage Code page to be used in conversion.
  @returns Converted ansi string.
}

function WideStringToString(const ws: WideString; codePage: Word): AnsiString;
var
  l: Integer;
begin
  if ws = '' then
    Result := ''
  else
  begin
    l := WideCharToMultiByte(codePage,
      WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
      @ws[1], -1, nil, 0, nil, nil);
    SetLength(Result, l - 1);
    if l > 1 then
      WideCharToMultiByte(codePage,
        WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
        @ws[1], -1, @Result[1], l - 1, nil, nil);
  end;
end; { WideStringToString }


{:Converts Ansi string to Unicode string using specified code page.
  @param   s        Ansi string.
  @param   codePage Code page to be used in conversion.
  @returns Converted wide string.
}

(*  // NOT WORKING IN D2009
function StringToWideString(const s: AnsiString; codePage: Word): WideString;
var
  l: Integer;
begin
  if s = '' then
    Result := ''
  else
  begin
    l := MultiByteToWideChar(codePage, MB_PRECOMPOSED, PChar(@s[1]), -1, nil, 0);
    SetLength(Result, l - 1);
    if l > 1 then
      MultiByteToWideChar(CodePage, MB_PRECOMPOSED, PChar(@s[1]),
        -1, PWideChar(@Result[1]), l - 1);
  end;
end; { StringToWideString } *)


end.
