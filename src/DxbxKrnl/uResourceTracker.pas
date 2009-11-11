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

  end;

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

        // insert a ptr using the pResource pointer as key
        void insert(void *pResource);

        // insert a ptr using an explicit key
        void insert(uint32 uiKey, void *pResource);

        // remove a ptr using the pResource pointer as key
        void remove(void *pResource);

        // remove a ptr using an explicit key
        void remove(uint32 uiKey);

        // check for existance of ptr using the pResource pointer as key
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


ResourceTracker g_VBTrackTotal;
ResourceTracker g_VBTrackDisable;
ResourceTracker g_PBTrackTotal;
ResourceTracker g_PBTrackDisable;
ResourceTracker g_PBTrackShowOnce;
ResourceTracker g_PatchedStreamsCache;
ResourceTracker g_DataToTexture;
ResourceTracker g_AlignCache;

ResourceTracker::~ResourceTracker()
{
    clear();
}

void ResourceTracker::clear()
{
    this->Lock();

    RTNode *cur = m_head;

    while(cur != 0)
    {
        RTNode *tmp = cur->pNext;

        delete cur;

        cur = tmp;
    }

    m_head = m_tail = 0;

    this->Unlock();
}

void ResourceTracker::insert(void *pResource)
{
    insert((uint32)pResource, pResource);
}

void ResourceTracker::insert(uint32 uiKey, void *pResource)
{
    this->Lock();

    if(exists(uiKey))
    {
        this->Unlock();
        return;
    }

    if(m_head == 0)
    {
        m_tail = m_head = new RTNode();
        m_tail->pResource = 0;
        m_tail->pNext = 0;
    }

    m_tail->pResource = pResource;
    m_tail->uiKey = uiKey;

    m_tail->pNext = new RTNode();

    m_tail = m_tail->pNext;

    m_tail->pResource = 0;
    m_tail->uiKey = 0;
    m_tail->pNext = 0;

    this->Unlock();

    return;
}

void ResourceTracker::remove(void *pResource)
{
    remove((uint32)pResource);
}

void ResourceTracker::remove(uint32 uiKey)
{
    this->Lock();

    RTNode *pre = 0;
    RTNode *cur = m_head;

    while(cur != 0)
    {
        if(cur->uiKey == uiKey)
        {
            if(pre != 0)
            {
                pre->pNext = cur->pNext;
            }
            else
            {
                m_head = cur->pNext;

                if(m_head->pNext == 0)
                {
                    delete m_head;

                    m_head = 0;
                    m_tail = 0;
                }
            }

            delete cur;

            this->Unlock();

            return;
        }

        pre = cur;
        cur = cur->pNext;
    }

    this->Unlock();

    return;
}

bool ResourceTracker::exists(void *pResource)
{
    return exists((uint32)pResource);
}

bool ResourceTracker::exists(uint32 uiKey)
{
    this->Lock();

    RTNode *cur = m_head;

    while(cur != 0)
    {
        if(cur->uiKey == uiKey)
        {
            this->Unlock();
            return true;
        }

        cur = cur->pNext;
    }

    this->Unlock();

    return false;
}

void *ResourceTracker::get(void *pResource)
{
    return get((uint32)pResource);
}

void *ResourceTracker::get(uint32 uiKey)
{
    RTNode *cur = m_head;

    while(cur != 0)
    {
        if(cur->uiKey == uiKey)
        {
            return cur->pResource;
        }

        cur = cur->pNext;
    }

    return 0;
}

uint32 ResourceTracker::get_count(void)
{
    uint32 uiCount = 0;

    this->Lock();

    RTNode *cur = m_head;

    while(cur != 0)
    {
        uiCount++;

        cur = cur->pNext;
    }

    this->Unlock();

    return uiCount;
}
*)
end.

