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
unit uResourceTracker;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows,
  // Dxbx
  uTypes,
  uMutex;
  
// exported globals

const g_bVBSkipStream: _bool = false;
//const g_bPBSkipPusher: _bool = false;

type
  PRTNode = ^RTNode;
  RTNode = packed record
    uiKey: uint32;
    pResource: Pvoid;
    pNext: PRTNode;
  end; // packed size = 12

type ResourceTracker = object(Mutex)
  public
//    constructor ResourceTracker();// : m_head(0), m_tail(0) {};
    destructor _ResourceTracker();

    // clear the tracker
    procedure clear();

    // insert a ptr using the pResource pointer as key
    procedure insert(pResource: Pvoid); overload;

    // insert a ptr using an explicit key
    procedure insert(uiKey: uint32; pResource: Pvoid); overload;

    // remove a ptr using the pResource pointer as key
    procedure remove(pResource: Pvoid); overload;

    // remove a ptr using an explicit key
    procedure remove(uiKey: uint32); overload;

    // check for existance of ptr using the pResource pointer as key
    function exists(pResource: Pvoid): _bool; overload;

    // check for existance of an explicit key
    function exists(uiKey: uint32): _bool; overload;

    // retrieves aresource using the resource ointer as key, explicit locking needed
    function get(pResource: Pvoid): Pvoid; overload;

    // retrieves a resource using an explicit key, explicit locking needed
    function get(uiKey: uint32): Pvoid; overload;

    // retrieves the number of entries in the tracker
    function get_count(): uint32;

    // for traversal
    function getHead(): PRTNode;
  private
    // list of "live" vertex buffers for debugging purposes
    m_head: PRTNode;
    m_tail: PRTNode;
  end; // size = 24

//
// all of our resource trackers
//

var g_VBTrackTotal: ResourceTracker;
var g_VBTrackDisable: ResourceTracker;
var g_PBTrackTotal: ResourceTracker;
var g_PBTrackDisable: ResourceTracker;
var g_PBTrackShowOnce: ResourceTracker;
var g_PatchedStreamsCache: ResourceTracker;
var g_DataToTexture: ResourceTracker;
var g_AlignCache: ResourceTracker;
  
implementation

{ ResourceTracker }

destructor ResourceTracker._ResourceTracker();
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  clear();
end;

procedure ResourceTracker.clear();
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  cur: PRTNode;
  tmp: PRTNode;
begin
  Self.Lock();

  cur := m_head;

  while (cur <> nil) do
  begin
    tmp := cur.pNext;

    Dispose(cur);

    cur := tmp;
  end;

  m_head := nil; m_tail := nil;

  Self.Unlock();
end;

procedure ResourceTracker.insert(pResource: Pvoid);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  insert(uint32(pResource), pResource);
end;

procedure ResourceTracker.insert(uiKey: uint32; pResource: Pvoid);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  Self.Lock();

  if (exists(uiKey)) then
  begin
    Self.Unlock();
    Exit;
  end;

  if (m_head = nil) then
  begin
    New(m_head);
    m_tail := m_head;
    m_tail.pResource := nil;
    m_tail.pNext := nil;
  end;

  m_tail.pResource := pResource;
  m_tail.uiKey := uiKey;

  New(m_tail.pNext);

  m_tail := m_tail.pNext;

  m_tail.pResource := nil;
  m_tail.uiKey := 0;
  m_tail.pNext := nil;

  Self.Unlock();
end;

procedure ResourceTracker.remove(pResource: Pvoid);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  remove(uint32(pResource));
end;

procedure ResourceTracker.remove(uiKey: uint32);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pre: PRTNode;
  cur: PRTNode;
begin
  Self.Lock();

  pre := nil;
  cur := m_head;

  while (cur <> nil) do
  begin
    if (cur.uiKey = uiKey) then
    begin
      if (pre <> nil) then
      begin
        pre.pNext := cur.pNext;
      end
      else
      begin
        m_head := cur.pNext;

        if (m_head.pNext = nil) then
        begin
          Dispose(m_head);

          m_head := nil;
          m_tail := nil;
        end;
      end;

      Dispose(cur);

      self.Unlock();

      Exit;
    end;

    pre := cur;
    cur := cur.pNext;
  end;
  Self.Unlock();
end;

function ResourceTracker.exists(pResource: Pvoid): _bool;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  Result := exists(uint32(pResource));
end;

function ResourceTracker.exists(uiKey: uint32): _bool;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  cur: PRTNode;
begin
  self.Lock();

  cur := m_head;

  while (cur <> nil)  do
  begin
    if (cur.uiKey = uiKey) then
    begin
      self.Unlock();
      Result := true;
      Exit;
    end;

    cur := cur.pNext;
  end;

  Self.Unlock();
  Result := false;
end;

function ResourceTracker.get(pResource: Pvoid): Pvoid;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  Result := get(uint32(pResource));
end;

function ResourceTracker.get(uiKey: uint32): Pvoid;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  cur: PRTNode;
begin
  cur := m_head;

  while (cur <> nil) do
  begin
    if (cur.uiKey = uiKey) then
    begin
      Result := cur.pResource;
      Exit;
    end;

    cur := cur.pNext;
  end;

  Result := nil;
end;

function ResourceTracker.get_count(): uint32;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  uiCount: uint32;
  cur: PRTNode;
begin
  uiCount := 0;

  Self.Lock();

  cur := m_head;

  while (cur <> nil) do
  begin
    Inc(uiCount);

    cur := cur.pNext;
  end;

  Self.Unlock();

  Result := uiCount;
end;

function ResourceTracker.getHead: PRTNode;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  Result := m_head;
end;

end.

