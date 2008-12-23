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
unit uMutex;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  // Dxbx
  uLog;

type
  // IMPORTANT NOTE : Keep the data-layout of this record in-sync with
  // the Cxbx version (if you want to maintain CxbxKrnl.DLL compatibility) !
  // Mutex object (intended to be inherited from)
  Mutex = packed record
  private
    m_MutexLock: Integer; // Mutex lock
    m_OwnerProcess: Integer; // Current owner process (or zero)
    m_OwnerThread: Integer; // Current owner thread
    m_LockCount: Integer; // Lock count within this thread
  public
    procedure Create;
    procedure Lock;
    procedure Unlock;
  end;

implementation

procedure Mutex.Create;
begin
  InterlockedExchange({var}m_MutexLock, 0);
  InterlockedExchange({var}m_OwnerProcess, 0);
  InterlockedExchange({var}m_OwnerThread, 0);
  InterlockedExchange({var}m_LockCount, 0);
end;

procedure Mutex.Lock;
begin
//WriteLog('Mutex.Lock>');
  while True do
  begin
    // Grab the lock, letting us look at the variables
    while InterlockedCompareExchange({var}m_MutexLock, {Exchange} 1, {Comperand} 0) <> 0 do
      SwitchToThread;

    // Are we the the new owner?
    if m_OwnerProcess = 0 then
    begin
//WriteLog('Mutex.Lock claimed');
      // Take ownership
      InterlockedExchange({var}m_OwnerProcess, GetCurrentProcessId());
      InterlockedExchange({var}m_OwnerThread, GetCurrentThreadId());
      InterlockedExchange({var}m_LockCount, 1);

      // Unlock the mutex itself
      InterlockedExchange({var}m_MutexLock, 0);

      Exit;
    end;

    // If a different process owns this mutex right now, unlock
    // the mutex lock and wait.  The reading need not be
    // interlocked.
    if (m_OwnerProcess <> Integer(GetCurrentProcessId()))
    or (m_OwnerThread <> Integer(GetCurrentThreadId())) then
    begin
      // Unlock the mutex itself
      InterlockedExchange({var}m_MutexLock, 0);

      // Wait and try again
      SwitchToThread;
      Continue;
    end;

//WriteLog('Mutex.Lock Already locked - incrementing LockCount');

    // The mutex was already locked, but by us.  Just increment
    // the lock count and unlock the mutex itself.
    InterlockedIncrement({var}m_LockCount);
    InterlockedExchange({var}m_MutexLock, 0);

    Exit;
  end;
//WriteLog('Mutex.Lock<');
end;

procedure Mutex.Unlock;
begin
//WriteLog('Mutex.Unlock>');
  // Grab the lock, letting us look at the variables
  while InterlockedCompareExchange({var}m_MutexLock, {Exchange} 1, {Comperand} 0) <> 0 do
    SwitchToThread;

  // Decrement the lock count
  if InterlockedDecrement({var}m_LockCount) <= 0 then
  begin
    // Mark the mutex as now unused
    InterlockedExchange({var}m_OwnerProcess, 0);
    InterlockedExchange({var}m_OwnerThread, 0);
    InterlockedExchange({var}m_LockCount, 0);
  end;

  // Unlock the mutex itself
  InterlockedExchange({var}m_MutexLock, 0);
//WriteLog('Mutex.Unlock<');
end;

end.
