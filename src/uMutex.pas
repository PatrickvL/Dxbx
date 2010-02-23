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
  uTypes,
  uLog;

// Mutex object (intended to be inherited from)
type Mutex = object
  public
    procedure Create;
    procedure Lock;
    procedure Unlock;
  private
    m_MutexLock: LONG;      // Mutex lock
    m_OwnerProcess: LONG;   // Current owner process (or zero)
    m_OwnerThread: LONG;    // Current owner thread
    m_LockCount: LONG;      // Lock count within this thread
  end;

implementation

procedure Mutex.Create;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  InterlockedExchange({var}m_MutexLock, 0);
  InterlockedExchange({var}m_OwnerProcess, 0);
  InterlockedExchange({var}m_OwnerThread, 0);
  InterlockedExchange({var}m_LockCount, 0);
end;

procedure Mutex.Lock;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  while True do
  begin
    // Grab the lock, letting us look at the variables
    while InterlockedCompareExchange({var}m_MutexLock, {Exchange} 1, {Comperand} 0) <> 0 do
      SwitchToThread; // Cxbx has : Sleep(1);

    // Are we the the new owner?
    if m_OwnerProcess = 0 then
    begin
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
      SwitchToThread; // Cxbx has : Sleep(1);
      Continue;
    end;

    // The mutex was already locked, but by us.  Just increment
    // the lock count and unlock the mutex itself.
    InterlockedIncrement({var}m_LockCount);
    InterlockedExchange({var}m_MutexLock, 0);

    Exit;
  end;
end;

procedure Mutex.Unlock;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  // Grab the lock, letting us look at the variables
  while InterlockedCompareExchange({var}m_MutexLock, {Exchange} 1, {Comperand} 0) <> 0 do
    SwitchToThread; // Cxbx has : Sleep(1);

  // Decrement the lock count
  if InterlockedDecrement({var}m_LockCount) <= 0 then
  begin
    // Mark the mutex as now unused
    InterlockedExchange({var}m_OwnerProcess, 0);
    InterlockedExchange({var}m_OwnerThread, 0);
  end;

  // Unlock the mutex itself
  InterlockedExchange({var}m_MutexLock, 0);
end;

end.
