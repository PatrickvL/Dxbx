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

const
  g_bVBSkipStream: bool = false;
  g_bVBSkipPusher: bool = false;

type
  PRTNode = ^RTNode;
  RTNode = record
    uiKey: uint32;
    pResource: Pvoid;
    pNext: PRTNode;
  end;

  ResourceTracker = object(Mutex)
  public
    function exists(pResource: PVOID): BOOL; overload;
    function exists(uiKey: uint32): BOOL; overload;
    function get(pResource: PVOID): PVOID; overload;
    function get(uiKey: uint32): PVOID; overload;
    procedure insert(uiKey: uint32; pResource: PVOID); overload;
    procedure insert(pResource: PVOID); overload;
    procedure remove(uiKey: uint32); overload;
    procedure remove(pResource: PVOID); overload;
  end;

var
  g_VBTrackTotal: ResourceTracker;
  g_VBTrackDisable: ResourceTracker;
  g_PBTrackTotal: ResourceTracker;
  g_PBTrackDisable: ResourceTracker;
  g_PBTrackShowOnce: ResourceTracker;
  g_PatchedStreamsCache: ResourceTracker;
  g_DataToTexture: ResourceTracker;
  g_AlignCache: ResourceTracker;

implementation

//
// all of our resource trackers
//
(*
#include "Common/Win32/Mutex.h"

    public:
        ResourceTracker() : m_head(0), m_tail(0) {};
       ~ResourceTracker();

        // clear the tracker
        void clear();

        // insert a ptr using the pResource PVOID as key
        void insert(void *pResource);

        // insert a ptr using an explicit key
        void insert(uint32 uiKey, void *pResource);

        // remove a ptr using the pResource PVOID as key
        void remove(void *pResource);

        // remove a ptr using an explicit key
        void remove(uint32 uiKey);

        // check for existance of ptr using the pResource PVOID as key
        bool exists(void *pResource);

        // check for existance of an explicit key
        bool exists(uint32 uiKey);

        // retrieves aresource using the resource ointer as key, explicit locking needed
        void *get(void *pResource);

        // retrieves a resource using an explicit key, explicit locking needed
        void *get(uint32 uiKey);

        // retrieves the number of entries in the tracker
        uint32 get_count(void);

        // for traversal
        struct RTNode *getHead() { return m_head; }

    private:
        // list of "live" vertex buffers for debugging purposes
        struct RTNode *m_head;
        struct RTNode *m_tail;
}



ResourceTracker::~ResourceTracker()
{
    clear();
} *)



{ ResourceTracker }

function ResourceTracker.exists(pResource: PVOID): BOOL;
begin
(*  return exists((uint32)pResource); *)
end;

function ResourceTracker.exists(uiKey: uint32): BOOL;
begin
(*    this.Lock();

    RTNode *cur = m_head;

    while Assigned(cur)
    {
        if (cur.uiKey = uiKey) then
        {
            this.Unlock();
            return true;
        }

        cur = cur.pNext;
    }

    this.Unlock();

    return false; *)
end;

function ResourceTracker.get(pResource: PVOID): PVOID;
begin
(*    return get((uint32)pResource); *)
end;

function ResourceTracker.get(uiKey: uint32): PVOID;
begin
(*    RTNode *cur = m_head;

    while Assigned(cur)
    {
        if (cur.uiKey = uiKey) then
        {
            return cur.pResource;
        }

        cur = cur.pNext;
    }

    return 0; *)
end;

procedure ResourceTracker.insert(pResource: PVOID);
begin
(*    insert((uint32)pResource, pResource); *)
end;

procedure ResourceTracker.insert(uiKey: uint32; pResource: PVOID);
begin
  Self.Lock;

  if exists(uiKey) then
  begin
    Self.Unlock();
    Exit;
  end;

(*  if m_head = 0 then
  {
      m_tail = m_head = new RTNode();
      m_tail.pResource = 0;
      m_tail.pNext = 0;
  }

  m_tail.pResource := pResource;
  m_tail.uiKey := uiKey;

  (*m_tail.pNext := new RTNode(); * )

  m_tail := m_tail.pNext;

  m_tail.pResource := 0;
  m_tail.uiKey := 0;
  m_tail.pNext := 0;
*)
  Self.Unlock();
end;

procedure ResourceTracker.remove(pResource: PVOID);
begin
   remove(uint32(pResource));
end;

procedure ResourceTracker.remove(uiKey: uint32);
begin
  Self.Lock();

(*
    RTNode *pre = 0;
    RTNode *cur = m_head;

    while Assigned(cur)
    {
        if (cur.uiKey = uiKey) then
        {
            if Assigned(pre) then
            {
                pre.pNext = cur.pNext;
            }
            else
            {
                m_head = cur.pNext;

                if (m_head.pNext = nil) then
                {
                    delete m_head;

                    m_head = 0;
                    m_tail = 0;
                }
            }

            delete cur;

            this.Unlock();

            return;
        }

        pre = cur;
        cur = cur.pNext;
    }
*)
  Self.Unlock();
end;

end.

