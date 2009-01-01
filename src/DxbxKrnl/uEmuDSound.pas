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

unit uEmuDSound;

{$INCLUDE ..\Dxbx.inc}

interface

implementation

uses
  JwaWinType
  , uEmu
  , uDxbxKrnlUtils
  , DirectSound
  , Windows
  , uEmuFS
  , uLog
  ;


type
  LPDIRECTSOUND8 = ^IDIRECTSOUND8;

// size of sound buffer cache (used for periodic sound buffer updates)
const SOUNDBUFFER_CACHE_SIZE = $100;
// size of sound stream cache (used for periodic sound stream updates)
const SOUNDSTREAM_CACHE_SIZE = $100;

var
   g_pDSound8 : IDIRECTSOUND8 = Nil;
   g_pDSound8RefCount : int = 0;

// periodically update sound buffers
procedure HackUpdateSoundBuffers();
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
(*var
  v : Integer; *)
begin
    (*for v:=0 to SOUNDBUFFER_CACHE_SIZE -1 do begin
        if(g_pDSoundBufferCache[v] = 0 or g_pDSoundBufferCache[v]^.EmuBuffer = 0) then
            continue;

        PVOID pAudioPtr, pAudioPtr2;
        DWORD dwAudioBytes, dwAudioBytes2;

        // unlock existing lock
        if(g_pDSoundBufferCache[v]^.EmuLockPtr1 <> 0) then
            g_pDSoundBufferCache[v]^.EmuDirectSoundBuffer8^.Unlock(g_pDSoundBufferCache[v]^.EmuLockPtr1, g_pDSoundBufferCache[v]^.EmuLockBytes1, g_pDSoundBufferCache[v]^.EmuLockPtr2, g_pDSoundBufferCache[v]^.EmuLockBytes2);

        HRESULT hRet := g_pDSoundBufferCache[v]^.EmuDirectSoundBuffer8^.Lock(0, g_pDSoundBufferCache[v]^.EmuBufferDesc^.dwBufferBytes, @pAudioPtr, @dwAudioBytes, @pAudioPtr2, @dwAudioBytes2, 0);

        if(SUCCEEDED(hRet)) then
        begin
            if(pAudioPtr <> 0) then
                memcpy(pAudioPtr,  g_pDSoundBufferCache[v]^.EmuBuffer, dwAudioBytes);

            if(pAudioPtr2 <> 0) then
                memcpy(pAudioPtr2, (PVOID)((DWORD)g_pDSoundBufferCache[v]^.EmuBuffer+dwAudioBytes), dwAudioBytes2);

            g_pDSoundBufferCache[v]^.EmuDirectSoundBuffer8^.Unlock(pAudioPtr, dwAudioBytes, pAudioPtr2, dwAudioBytes2);
         end;

        // TODO: relock old lock ??
     end; *)
 end;

// periodically update sound streams
procedure HackUpdateSoundStreams();
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    (*for(integer v:=0;v<SOUNDSTREAM_CACHE_SIZE;v++)
    begin
        if(g_pDSoundStreamCache[v] = 0 or g_pDSoundStreamCache[v]^.EmuBuffer = 0) then
            continue;

        PVOID pAudioPtr, pAudioPtr2;
        DWORD dwAudioBytes, dwAudioBytes2;

        HRESULT hRet := g_pDSoundStreamCache[v]^.EmuDirectSoundBuffer8^.Lock(0, g_pDSoundStreamCache[v]^.EmuBufferDesc^.dwBufferBytes, @pAudioPtr, @dwAudioBytes, @pAudioPtr2, @dwAudioBytes2, 0);

        if(SUCCEEDED(hRet)) then
        begin
            if(pAudioPtr <> 0) then
                memcpy(pAudioPtr,  g_pDSoundStreamCache[v]^.EmuBuffer, dwAudioBytes);

            if(pAudioPtr2 <> 0) then
                memcpy(pAudioPtr2, (PVOID)((DWORD)g_pDSoundStreamCache[v]^.EmuBuffer+dwAudioBytes), dwAudioBytes2);

            g_pDSoundStreamCache[v]^.EmuDirectSoundBuffer8^.Unlock(pAudioPtr, dwAudioBytes, pAudioPtr2, dwAudioBytes2);
         end;

        g_pDSoundStreamCache[v]^.EmuDirectSoundBuffer8^.SetCurrentPosition(0);
        g_pDSoundStreamCache[v]^.EmuDirectSoundBuffer8^.Play(0, 0, 0);
     end;

    Exit; *)
 end;

// resize an emulated directsound buffer, if necessary
(*  procedure EmuResizeIDirectSoundBuffer8(var pThis: XTL.X_CDirectSoundBuffer; dwBytes: DWORD);
    if(dwBytes = pThis^.EmuBufferDesc^.dwBufferBytes or dwBytes = 0) then
        Exit;

    DbgPrintf('EmuResizeIDirectSoundBuffer8 : Resizing not  ($ mod .08X^.$ mod .08X)', pThis^.EmuBufferDesc^.dwBufferBytes, dwBytes);

    DWORD dwPlayCursor, dwWriteCursor, dwStatus;

    HRESULT hRet := pThis^.EmuDirectSoundBuffer8^.GetCurrentPosition(@dwPlayCursor, @dwWriteCursor);

    if(FAILED(hRet)) then 
        CxbxKrnlCleanup('Unable to retrieve current position for resize reallocation not ');

    hRet := pThis^.EmuDirectSoundBuffer8^.GetStatus(@dwStatus);

    if(FAILED(hRet)) then 
        CxbxKrnlCleanup('Unable to retrieve current status for resize reallocation not ');

    // release old buffer
    while(pThis^.EmuDirectSoundBuffer8^.Release() > 0) begin   end;

    pThis^.EmuBufferDesc^.dwBufferBytes := dwBytes;

    hRet := g_pDSound8^.CreateSoundBuffer(pThis^.EmuBufferDesc, @pThis^.EmuDirectSoundBuffer8, 0);

    if(FAILED(hRet)) then 
        CxbxKrnlCleanup('IDirectSoundBuffer8 resize Failed not ');

    pThis^.EmuDirectSoundBuffer8^.SetCurrentPosition(dwPlayCursor);

    if(dwStatus and DSBSTATUS_PLAYING) then 
        pThis^.EmuDirectSoundBuffer8^.Play(0, 0, pThis^.EmuPlayFlags);
 end;        *)

// resize an emulated directsound stream, if necessary
(*  procedure EmuResizeIDirectSoundStream8(var pThis: XTL.X_CDirectSoundStream; dwBytes: DWORD);
    if(dwBytes = pThis^.EmuBufferDesc^.dwBufferBytes) then 
        Exit;

    DWORD dwPlayCursor, dwWriteCursor, dwStatus;

    HRESULT hRet := pThis^.EmuDirectSoundBuffer8^.GetCurrentPosition(@dwPlayCursor, @dwWriteCursor);

    if(FAILED(hRet)) then 
        CxbxKrnlCleanup('Unable to retrieve current position for resize reallocation not ');

    hRet := pThis^.EmuDirectSoundBuffer8^.GetStatus(@dwStatus);

    if(FAILED(hRet)) then 
        CxbxKrnlCleanup('Unable to retrieve current status for resize reallocation not ');

    // release old buffer
    while(pThis^.EmuDirectSoundBuffer8^.Release() > 0) begin   end;

    pThis^.EmuBufferDesc^.dwBufferBytes := dwBytes;

    hRet := g_pDSound8^.CreateSoundBuffer(pThis^.EmuBufferDesc, @pThis^.EmuDirectSoundBuffer8, 0);

    if(FAILED(hRet)) then 
        CxbxKrnlCleanup('IDirectSoundBuffer8 resize Failed not ');

    pThis^.EmuDirectSoundBuffer8^.SetCurrentPosition(dwPlayCursor);

    if(dwStatus and DSBSTATUS_PLAYING) then 
        pThis^.EmuDirectSoundBuffer8^.Play(0, 0, pThis^.EmuPlayFlags);
 end;     *)

// ******************************************************************
// * func: EmuDirectSoundCreate
// ******************************************************************
function XTL_EmuDirectSoundCreate(
    pguidDeviceId : Pointer;
    ppDirectSound : LPDIRECTSOUND8;
    pUnknown : IUNKNOWN) : HRESULT; stdcall;
// Branch:martin  Revision:80  Translator:Shadow_Tj  Done:80
var
  initialized : bool;
  hRet : HRESULT;
  v : integer;
begin
    EmuSwapFS( fsWindows );   // Win2k/XP FS

    DbgPrintf('EmuDSound (0x%X): EmuDirectSoundCreate' +
           '(' +
           '   pguidDeviceId             : 0x%.08X' +
           '   ppDirectSound             : 0x%.08X' +
           '   pUnknown                  : 0x%.08X' +
           ');',
           [GetCurrentThreadId(), pguidDeviceId, ppDirectSound, pUnknown]);

     initialized := false;

    hRet := DS_OK;

    if( not initialized) then 
    begin 
        hRet := DirectSoundCreate8(0, ppDirectSound^, Nil);

        if(FAILED(hRet)) then 
            CxbxKrnlCleanup('DirectSoundCreate8 Failed not ');

        g_pDSound8 := ppDirectSound^;

        hRet := g_pDSound8.SetCooperativeLevel(g_hEmuWindow, DSSCL_PRIORITY);

        if(FAILED(hRet)) then 
            CxbxKrnlCleanup('g_pDSound8^.SetCooperativeLevel Failed not ');

        v:=0;
        (*// clear sound buffer cache
        for v:=0 to SOUNDBUFFER_CACHE_SIZE -1  do
            g_pDSoundBufferCache[v] := 0;

        // clear sound stream cache
        for v:=0 to SOUNDSTREAM_CACHE_SIZE -1 do
            g_pDSoundStreamCache[v] := 0; *)

        initialized := true;
     end;

    g_pDSound8RefCount := 1;

    EmuSwapFS( fsXbox );   // XBox FS

    result:= hRet;
 end;      

// ******************************************************************
// * func: EmuIDirectSound8_AddRef
// ******************************************************************
Function XTL_EmuIDirectSound8_AddRef(
    pThis : LPDIRECTSOUND8 ) : ULONG; stdcall;
// Branch:martin  Revision:80  Translator:Shadow_Tj  Done:100
var
  uRet : ULONG;
begin
    EmuSwapFS( fsWindows );   // Win2k/XP FS

    DbgPrintf('EmuDSound (0x%X): EmuIDirectSound8_AddRef' +
           '(' +
           '   pThis                     : 0x%.08X' +
           ');',
           [GetCurrentThreadId(), pThis]);

    uRet := g_pDSound8RefCount;
    Inc(g_pDSound8RefCount);

    EmuSwapFS( fsXbox );   // XBox FS

    result:= uRet;
 end;        

// ******************************************************************
// * func: EmuIDirectSound8_Release
// ******************************************************************
function XTL_EmuIDirectSound8_Release(
    pThis : LPDIRECTSOUND8) : ULONG; stdcall;
// Branch:martin  Revision:80  Translator:Shadow_Tj  Done:100
var
  uRet : ULONG;
begin
    EmuSwapFS( fsWindows );   // Win2k/XP FS

    DbgPrintf('EmuDSound (0x%X): EmuIDirectSound8_Release' +
           '(' +
           '   pThis                     : 0x%.08X' +
           ');',
           [GetCurrentThreadId(), pThis]);

    uRet := g_pDSound8RefCount;
    inc(g_pDSound8RefCount);

    (* temporarily (?) disabled by cxbx
    if(uRet = 1) then
        pThis^.Release();
    //*)

    EmuSwapFS( fsXbox );   // XBox FS

    result:= uRet;
 end;

// ******************************************************************
// * func: EmuCDirectSound_GetSpeakerConfig
// ******************************************************************
(*HRESULT WINAPI XTL.EmuCDirectSound_GetSpeakerConfig
(
    X_CDirectSound         *pThis,
    PDWORD                  pdwSpeakerConfig
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound ($ mod X): EmuCDirectSound_GetSpeakerConfig'
           '('
           '   pThis                     : 0x%.08X'
           '   pdwSpeakerConfig          : 0x%.08X'
           ');',
           GetCurrentThreadId(), pThis, pdwSpeakerConfig);

    *pdwSpeakerConfig := 0; // STEREO

    EmuSwapFS();   // XBox FS

    result:= S_OK;
 end;      *)

// ******************************************************************
// * func: EmuIDirectSound8_DownloadEffectsImage
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSound8_DownloadEffectsImage
(
    LPDIRECTSOUND8          pThis,
    LPCVOID                 pvImageBuffer,
    DWORD                   dwImageSize,
    PVOID                   pImageLoc,      // TODO: Use this param
    PVOID                  *ppImageDesc     // TODO: Use this param
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound ($ mod X): EmuIDirectSound8_DownloadEffectsImage'
           '('
           '   pThis                     : 0x%.08X'
           '   pvImageBuffer             : 0x%.08X'
           '   dwImageSize               : 0x%.08X'
           '   pImageLoc                 : 0x%.08X'
           '   ppImageDesc               : 0x%.08X'
           ');',
           GetCurrentThreadId(), pThis, pvImageBuffer, dwImageSize, pImageLoc, ppImageDesc);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    result:= S_OK;
 end;          *)

// ******************************************************************
// * func: EmuDirectSoundDoWork
// ******************************************************************
procedure XTL_EmuDirectSoundDoWork(); stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    EmuSwapFS( fsWindows );   // Win2k/XP FS

    DbgPrintf('EmuDSound (0x%X): EmuDirectSoundDoWork();', GetCurrentThreadId());

    HackUpdateSoundBuffers();
    HackUpdateSoundStreams();

    EmuSwapFS( fsXbox );   // XBox FS

    Exit;
 end;

// ******************************************************************
// * func: EmuIDirectSound8_SetOrientation
// ******************************************************************
function XTL_EmuIDirectSound8_SetOrientation
(
    pThis : LPDIRECTSOUND8;
    xFront : FLOAT;
    yFront : FLOAT;
    zFront : FLOAT;
    xTop : FLOAT;
    yTop : FLOAT;
    zTop : FLOAT;
    dwApply : DWORD
) : HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    EmuSwapFS( fsWindows );   // Win2k/XP FS

    DbgPrintf('EmuDSound (0x%X): EmuIDirectSound8_SetOrientation' +
           '(' +
           '   pThis                     : 0x%.08X' +
           '   xFront                    :  % f' +
           '   yFront                    :  % f' +
           '   zFront                    :  % f' +
           '   xTop                      :  % f' +
           '   yTop                      :  % f' +
           '   zTop                      :  % f' +
           '   dwApply                   : 0x%.08X' +
           ');',
           [GetCurrentThreadId(), pThis, xFront, yFront, zFront, xTop, yTop, zTop, dwApply]);

    // TODO: Actually implement this

    EmuSwapFS( fsXbox );   // XBox FS

    result:= S_OK;
 end;

// ******************************************************************
// * func: EmuIDirectSound8_SetDistanceFactor
// ******************************************************************
function XTL_EmuIDirectSound8_SetDistanceFactor
(
    pThis : LPDIRECTSOUND8;
    fDistanceFactor : FLOAT;
    dwApply : DWORD
) : HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    EmuSwapFS( fsWindows );   // Win2k/XP FS

    DbgPrintf('EmuDSound (0x%X): EmuIDirectSound8_SetDistanceFactor' +
           '(' +
           '   pThis                     : 0x%.08X' +
           '   fDistanceFactor           :  % f' +
           '   dwApply                   : 0x%.08X' +
           ');',
           [GetCurrentThreadId(), pThis, fDistanceFactor, dwApply]);

    // TODO: Actually implement this

    EmuSwapFS( fsXbox );   // XBox FS

    result:= S_OK;
 end;

// ******************************************************************
// * func: EmuIDirectSound8_SetRolloffFactor
// ******************************************************************
function XTL_EmuIDirectSound8_SetRolloffFactor
(
    pThis : LPDIRECTSOUND8;
    fRolloffFactor : FLOAT;
    dwApply : DWORD
) : HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    EmuSwapFS( fsWindows );   // Win2k/XP FS

    DbgPrintf('EmuDSound (0x%X): EmuIDirectSound8_SetRolloffFactor' +
           '(' +
           '   pThis                     : 0x%.08X' +
           '   fRolloffFactor            :  % f' +
           '   dwApply                   : 0x%.08X' +
           ');',
           [GetCurrentThreadId(), pThis, fRolloffFactor, dwApply]);

    // TODO: Actually implement this

    EmuSwapFS( fsWindows );   // XBox FS

    result:= S_OK;
 end;

// ******************************************************************
// * func: EmuIDirectSound8_SetDopplerFactor
// ******************************************************************
function XTL_EmuIDirectSound8_SetDopplerFactor
(
    pThis : LPDIRECTSOUND8;
    fDopplerFactor : FLOAT;
    dwApply : DWORD
) : HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    EmuSwapFS( fsWindows );   // Win2k/XP FS

    DbgPrintf('EmuDSound (0x%X): EmuIDirectSound8_SetDopplerFactor' +
           '(' +
           '   pThis                     : 0x%.08X' +
           '   fDopplerFactor            :  % f' +
           '   dwApply                   : 0x%.08X' +
           ');',
           [GetCurrentThreadId(), pThis, fDopplerFactor, dwApply]);

    // TODO: Actually implement this

    EmuSwapFS( fsXbox );   // XBox FS

    result:= S_OK;
 end;

// ******************************************************************
// * func: EmuIDirectSound8_SetI3DL2Listener
// ******************************************************************
function XTL_EmuIDirectSound8_SetI3DL2Listener
(
    pThis : LPDIRECTSOUND8;
    pDummy : PVOID; // TODO: fill this out
    dwApply : DWORD
) : HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin 
    // debug trace
   {$IFDEF _DEBUG_TRACE}

    begin
        EmuSwapFS( fsWindows );   // Win2k/XP FS
        DbgPrintf('EmuDSound (0x%X): EmuIDirectSound8_SetI3DL2Listener' +
               '(' +
               '   pThis                     : 0x%.08X' +
               '   pDummy                    : 0x%.08X' +
               '   dwApply                   : 0x%.08X' +
               ');',
               [GetCurrentThreadId(), pThis, pDummy, dwApply]);
        EmuSwapFS( fsXbox );   // XBox FS
     end;
    {$ENDIF}

    // TODO: Actually do something

    result:= DS_OK;
 end;            

// ******************************************************************
// * func: EmuIDirectSound8_SetMixBinHeadroom
// ******************************************************************
function XTL_EmuIDirectSound8_SetMixBinHeadroom
(
    pThis : LPDIRECTSOUND8;
    dwMixBinMask : DWORD;
    dwHeadroom : DWORD
) : HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    // debug trace
    {$IFDEF _DEBUG_TRACE}
    begin
        EmuSwapFS( fsWindows );   // Win2k/XP FS
        DbgPrintf('EmuDSound (0x%X): EmuIDirectSound8_SetMixBinHeadroom' +
               '(' +
               '   pThis                     : 0x%.08X' +
               '   dwMixBinMask              : 0x%.08X' +
               '   dwHeadroom                : 0x%.08X' +
               ');',
               [GetCurrentThreadId(), pThis, dwMixBinMask, dwHeadroom]);
        EmuSwapFS( fsXbox );   // XBox FS
     end;
    {$ENDIF}

    // TODO: Actually do something

    result:= DS_OK;
 end;

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetMixBins
// ******************************************************************
function XTL_EmuIDirectSoundBuffer8_SetMixBins
(
    pThis : LPDIRECTSOUND8;
    pMixBins : PVOID
) : HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    // debug trace
    {$IFDEF _DEBUG_TRACE}
    begin 
        EmuSwapFS( fsWindows );   // Win2k/XP FS
        DbgPrintf('EmuDSound (0x%X): EmuIDirectSoundBuffer8_SetMixBins' +
               '(' +
               '   pThis                     : 0x%.08X' +
               '   pMixBins                  : 0x%.08X' +
               ');',
               [GetCurrentThreadId(), pThis, pMixBins]);
        EmuSwapFS( fsXbox );   // XBox FS
     end;
    {$ENDIF}

    // TODO: Actually do something

    result:= DS_OK;
 end;

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetMixBinVolumes
// ******************************************************************
function XTL_EmuIDirectSoundBuffer8_SetMixBinVolumes
(
    pThis : LPDIRECTSOUND8;
    pMixBins : PVOID
) : HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    // debug trace
    {$IFDEF _DEBUG_TRACE}
    begin 
        EmuSwapFS( fsWindows );   // Win2k/XP FS
        DbgPrintf('EmuDSound (0x%X): EmuIDirectSoundBuffer8_SetMixBinVolumes' +
               '(' +
               '   pThis                     : 0x%.08X' +
               '   pMixBins                  : 0x%.08X' +
               ');',
               [GetCurrentThreadId(), pThis, pMixBins]);
        EmuSwapFS( fsXbox );   // XBox FS
     end;
    {$ENDIF}

    // TODO: Actually do something

    result:= DS_OK;
 end;

// ******************************************************************
// * func: EmuIDirectSound8_SetPosition
// ******************************************************************
function XTL_EmuIDirectSound8_SetPosition
(
    pThis : LPDIRECTSOUND8;
    x : FLOAT;
    y : FLOAT;
    z : FLOAT;
    dwApply : DWORD
) : HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin 
    // debug trace
    {$IFDEF _DEBUG_TRACE}
    begin 
        EmuSwapFS( fsWindows );   // Win2k/XP FS
        DbgPrintf('EmuDSound (0x%X): EmuIDirectSound8_SetPosition' +
               '(' +
               '   pThis                     : 0x%.08X' +
               '   x                         :  % f' +
               '   y                         :  % f' +
               '   z                         :  % f' +
               '   dwApply                   : 0x%.08X' +
               ');',
               [GetCurrentThreadId(), pThis, x, y, z, dwApply]);
        EmuSwapFS( fsXbox );   // XBox FS
     end;
    {$ENDIF}

    // TODO: Actually do something

    result:= DS_OK;
 end;

// ******************************************************************
// * func: EmuIDirectSound8_SetPosition
// ******************************************************************
function XTL_EmuIDirectSound8_SetVelocity
(
    pThis : LPDIRECTSOUND8;
    x : FLOAT;
    y : FLOAT;
    z : FLOAT;
    dwApply : DWORD
) : HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    // debug trace
    {$IFDEF _DEBUG_TRACE}
    begin 
        EmuSwapFS( fsWindows );   // Win2k/XP FS
        DbgPrintf('EmuDSound (0x%X): EmuIDirectSound8_SetVelocity' +
               '(' +
               '   pThis                     : 0x%.08X' +
               '   x                         :  % f' +
               '   y                         :  % f' +
               '   z                         :  % f' +
               '   dwApply                   : 0x%.08X' +
               ');',
               [GetCurrentThreadId(), pThis, x, y, z, dwApply]);
        EmuSwapFS( fsXbox );   // XBox FS
     end;
    {$ENDIF}

    // TODO: Actually do something

    result:= DS_OK;
 end;            

// ******************************************************************
// * func: EmuIDirectSound8_SetAllParameters
// ******************************************************************
function XTL_EmuIDirectSound8_SetAllParameters
(
    pThis : LPDIRECTSOUND8;
    pTodo : Pointer;  // TODO: LPCDS3DLISTENER
    dwApply : DWORD
) : HRESULT; stdcall;
begin 
    // debug trace
    {$IFDEF _DEBUG_TRACE}
    begin 
        EmuSwapFS( fsWindows );   // Win2k/XP FS
        DbgPrintf('EmuDSound (0x%X): EmuIDirectSound8_SetAllParameters' +
               '(' +
               '   pThis                     : 0x%.08X' +
               '   pTodo                     : 0x%.08X' +
               '   dwApply                   : 0x%.08X' +
               ');',
               [GetCurrentThreadId(), pThis, pTodo, dwApply]);
        EmuSwapFS( fsXbox );   // XBox FS
     end;
    {$ENDIF}

    // TODO: Actually do something

    result:= DS_OK;
 end;         

// ******************************************************************
// * func: EmuCDirectSound_CommitDeferredSettings
// ******************************************************************
(*HRESULT WINAPI XTL.EmuCDirectSound_CommitDeferredSettings
(
    X_CDirectSound         *pThis
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound ($ mod X): EmuCDirectSound_CommitDeferredSettings"
           "("
           "   pThis                     : $ mod .08X"
           ");",
           GetCurrentThreadId(), pThis);

    // TODO: Translate params, then make the PC DirectSound call

    EmuSwapFS();   // XBox FS

    result:= DS_OK;
 end;              *)

// ******************************************************************
// * func: EmuDirectSoundCreateBuffer
// ******************************************************************
(*HRESULT WINAPI XTL.EmuDirectSoundCreateBuffer
(
    X_DSBUFFERDESC         *pdsbd,
    X_CDirectSoundBuffer  **ppBuffer
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound ($ mod X): EmuDirectSoundCreateBuffer"
           "("
           "   pdsbd                     : $ mod .08X"
           "   ppBuffer                  : $ mod .08X"
           ");",
           GetCurrentThreadId(), pdsbd, ppBuffer);

    DWORD dwEmuFlags := 0;

    DSBUFFERDESC *pDSBufferDesc := (DSBUFFERDESC)CxbxMalloc(SizeOf(DSBUFFERDESC));
    
    // convert from Xbox to PC DSound
    begin 
        DWORD dwAcceptableMask := $00000010 or $00000020 or $00000080 or $00000100 or $00002000 or $00040000 or $00080000;

        if(pdsbd^.dwFlags and (~dwAcceptableMask)) then 
            EmuWarning("Use of unsupported pdsbd^.dwFlags mask(s) ($ mod .08X)", pdsbd^.dwFlags and (~dwAcceptableMask));

        pDSBufferDesc^.dwSize := SizeOf(DSBUFFERDESC);
        pDSBufferDesc^.dwFlags := (pdsbd^.dwFlags and dwAcceptableMask) or DSBCAPS_CTRLVOLUME or DSBCAPS_GETCURRENTPOSITION2;
        pDSBufferDesc^.dwBufferBytes := pdsbd^.dwBufferBytes;

        if(pDSBufferDesc^.dwBufferBytes < DSBSIZE_MIN) then 
            pDSBufferDesc^.dwBufferBytes := DSBSIZE_MIN;
        else if(pDSBufferDesc^.dwBufferBytes > DSBSIZE_MAX) then  
            pDSBufferDesc^.dwBufferBytes := DSBSIZE_MAX;

        pDSBufferDesc^.dwReserved := 0;

        if(pdsbd^.lpwfxFormat <> 0) then 
        begin 
            pDSBufferDesc^.lpwfxFormat := (WAVEFORMATEX)CxbxMalloc(SizeOf(WAVEFORMATEX)+pdsbd^.lpwfxFormat^.cbSize);
            memcpy(pDSBufferDesc^.lpwfxFormat, pdsbd^.lpwfxFormat, SizeOf(WAVEFORMATEX));

            if(pDSBufferDesc^.lpwfxFormat^.wFormatTag = (*WAVE_FORMAT_XBOX_ADPCM*)(*0x0069) then
            begin 
                dwEmuFlags:= dwEmuFlags or DSB_FLAG_ADPCM;

                EmuWarning("WAVE_FORMAT_XBOX_ADPCM Unsupported not ");

                pDSBufferDesc^.lpwfxFormat^.wFormatTag := WAVE_FORMAT_PCM;
                pDSBufferDesc^.lpwfxFormat^.nBlockAlign := (pDSBufferDesc^.lpwfxFormat^.nChannels*pDSBufferDesc^.lpwfxFormat^.wBitsPerSample)/8;

                // the above calculation can yield zero for wBitsPerSample < 8, so we'll bound it to 1 byte minimum
                if(pDSBufferDesc^.lpwfxFormat^.nBlockAlign = 0) then 
                    pDSBufferDesc^.lpwfxFormat^.nBlockAlign := 1;

                pDSBufferDesc^.lpwfxFormat^.nAvgBytesPerSec := pDSBufferDesc^.lpwfxFormat^.nSamplesPerSec*pDSBufferDesc^.lpwfxFormat^.nBlockAlign;
                pDSBufferDesc^.lpwfxFormat^.wBitsPerSample := 8;

                (* TODO: Get ADPCM working!
                pDSBufferDesc^.lpwfxFormat^.cbSize := 32;
                const WAVE_FORMAT_ADPCM = 2;
                pDSBufferDesc^.lpwfxFormat^.wFormatTag := WAVE_FORMAT_ADPCM;
                *)
(*             end;
         end;

        pDSBufferDesc^.guid3DAlgorithm := DS3DALG_DEFAULT;
     end;

    // sanity check
    if(pDSBufferDesc^.lpwfxFormat^.nBlockAlign <> (pDSBufferDesc^.lpwfxFormat^.nChannels*pDSBufferDesc^.lpwfxFormat^.wBitsPerSample)/8) then 
    begin 
        pDSBufferDesc^.lpwfxFormat^.nBlockAlign := (2*pDSBufferDesc^.lpwfxFormat^.wBitsPerSample)/8;
        pDSBufferDesc^.lpwfxFormat^.nAvgBytesPerSec := pDSBufferDesc^.lpwfxFormat^.nSamplesPerSec * pDSBufferDesc^.lpwfxFormat^.nBlockAlign;
     end;

    // TODO: Garbage Collection
    *ppBuffer := new X_CDirectSoundBuffer();

    (ppBuffer)^.EmuDirectSoundBuffer8 := 0;
    (ppBuffer)^.EmuBuffer := 0;
    (ppBuffer)^.EmuBufferDesc := pDSBufferDesc;
    (ppBuffer)^.EmuLockPtr1 := 0;
    (ppBuffer)^.EmuLockBytes1 := 0;
    (ppBuffer)^.EmuLockPtr2 := 0;
    (ppBuffer)^.EmuLockBytes2 := 0;
    (ppBuffer)^.EmuFlags := dwEmuFlags;

    DbgPrintf("EmuDSound ($ mod X): EmuDirectSoundCreateBuffer, *ppBuffer := $ mod .08X, bytes := $ mod .08X", GetCurrentThreadId(), *ppBuffer, pDSBufferDesc^.dwBufferBytes);

    HRESULT hRet := g_pDSound8^.CreateSoundBuffer(pDSBufferDesc,  and ((ppBuffer)^.EmuDirectSoundBuffer8), 0);

    if(FAILED(hRet)) then 
        EmuWarning("CreateSoundBuffer Failed not ");

    // cache this sound buffer
    begin 
        integer v:=0;
        for(v:=0;v<SOUNDBUFFER_CACHE_SIZE;v++)
        begin 
            if(g_pDSoundBufferCache[v] = 0) then 
            begin 
                g_pDSoundBufferCache[v] := *ppBuffer;
                break;
             end;
         end;

        if(v = SOUNDBUFFER_CACHE_SIZE) then 
            CxbxKrnlCleanup("SoundBuffer cache out of slots not ");
     end;

    EmuSwapFS();   // XBox FS

    result:= hRet;
 end;      *)

// ******************************************************************
// * func: EmuIDirectSound8_CreateBuffer
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSound8_CreateBuffer
(
    LPDIRECTSOUND8          pThis,
    X_DSBUFFERDESC         *pdssd,
    X_CDirectSoundBuffer  **ppBuffer,
    PVOID                   pUnknown
)
begin 
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
       EmuSwapFS();   // Win2k/XP FS
       DbgPrintf("EmuDSound ($ mod X): EmuIDirectSound8_CreateBuffer"
               "("
               "   pThis                     : $ mod .08X"
               "   pdssd                     : $ mod .08X"
               "   ppBuffer                  : $ mod .08X"
               "   pUnknown                  : $ mod .08X"
               ");",
               GetCurrentThreadId(), pThis, pdssd, ppBuffer, pUnknown);
       EmuSwapFS();   // XBox FS
     end;
    //endif

    EmuDirectSoundCreateBuffer(pdssd, ppBuffer);

    result:= DS_OK;
 end;         *)

// ******************************************************************
// * func: EmuIDirectSound8_CreateSoundBuffer
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSound8_CreateSoundBuffer
(
    LPDIRECTSOUND8          pThis,
    X_DSBUFFERDESC         *pdsbd,
    X_CDirectSoundBuffer  **ppBuffer,
    LPUNKNOWN               pUnkOuter
)
begin 
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf("EmuDSound ($ mod X): EmuIDirectSound8_CreateSoundBuffer"
               "("
               "   pdsbd                     : $ mod .08X"
               "   ppBuffer                  : $ mod .08X"
               "   pUnkOuter                 : $ mod .08X"
               ");",
               GetCurrentThreadId(), pdsbd, ppBuffer, pUnkOuter);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    result:= EmuDirectSoundCreateBuffer(pdsbd, ppBuffer);
 end;       *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetBufferData
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetBufferData
(
    X_CDirectSoundBuffer   *pThis,
    Pointer                  pvBufferData,
    DWORD                   dwBufferBytes
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound ($ mod X): EmuIDirectSoundBuffer8_SetBufferData"
           "("
           "   pThis                     : $ mod .08X"
           "   pvBufferData              : $ mod .08X"
           "   dwBufferBytes             : $ mod .08X"
           ");",
           GetCurrentThreadId(), pThis, pvBufferData, dwBufferBytes);

    // update buffer data cache
    pThis^.EmuBuffer := pvBufferData;

    EmuResizeIDirectSoundBuffer8(pThis, dwBufferBytes);

    EmuSwapFS();   // XBox FS

    result:= S_OK;
 end;           *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetPlayRegion
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetPlayRegion
(
    X_CDirectSoundBuffer   *pThis,
    DWORD                   dwPlayStart,
    DWORD                   dwPlayLength
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound ($ mod X): EmuIDirectSoundBuffer8_SetPlayRegion"
           "("
           "   pThis                     : $ mod .08X"
           "   dwPlayStart               : $ mod .08X"
           "   dwPlayLength              : $ mod .08X"
           ");",
           GetCurrentThreadId(), pThis, dwPlayStart, dwPlayLength);

    // TODO: Translate params, then make the PC DirectSound call

    // TODO: Ensure that 4627 & 4361 are intercepting far enough back
    // (otherwise pThis is manipulated!)

    EmuSwapFS();   // XBox FS

    result:= DS_OK;
 end;            *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_Lock
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_Lock
(
    X_CDirectSoundBuffer   *pThis,
    DWORD                   dwOffset, 
    DWORD                   dwBytes, 
    Pointer                 *ppvAudioPtr1, 
    LPDWORD                 pdwAudioBytes1, 
    Pointer                 *ppvAudioPtr2, 
    LPDWORD                 pdwAudioBytes2, 
    DWORD                   dwFlags 
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound ($ mod X): EmuIDirectSoundBuffer8_Lock"
           "("
           "   pThis                     : $ mod .08X"
           "   dwOffset                  : $ mod .08X"
           "   dwBytes                   : $ mod .08X"
           "   ppvAudioPtr1              : $ mod .08X"
           "   pdwAudioBytes1            : $ mod .08X"
           "   ppvAudioPtr2              : $ mod .08X"
           "   pdwAudioBytes2            : $ mod .08X"
           "   dwFlags                   : $ mod .08X"
           ");",
           GetCurrentThreadId(), pThis, dwOffset, dwBytes, ppvAudioPtr1, pdwAudioBytes1,
           ppvAudioPtr2, pdwAudioBytes2, dwFlags);

    HRESULT hRet := D3D_OK;

    if(pThis^.EmuBuffer <> 0) then 
    begin 
        *ppvAudioPtr1 := pThis^.EmuBuffer;
        *pdwAudioBytes1 := dwBytes;
     end;
    else
    begin 
        if(dwBytes > pThis^.EmuBufferDesc^.dwBufferBytes) then 
            EmuResizeIDirectSoundBuffer8(pThis, dwBytes);

        if(pThis^.EmuLockPtr1 <> 0) then 
            pThis^.EmuDirectSoundBuffer8^.Unlock(pThis^.EmuLockPtr1, pThis^.EmuLockBytes1, pThis^.EmuLockPtr2, pThis^.EmuLockBytes2);

        // TODO: Verify dwFlags is the same as windows
        hRet := pThis^.EmuDirectSoundBuffer8^.Lock(dwOffset, dwBytes, ppvAudioPtr1, pdwAudioBytes1, ppvAudioPtr2, pdwAudioBytes2, dwFlags);

        if(FAILED(hRet)) then 
            CxbxKrnlCleanup("DirectSoundBuffer Lock Failed not ");

        pThis^.EmuLockPtr1 := *ppvAudioPtr1;
        pThis^.EmuLockBytes1 := *pdwAudioBytes1;
        pThis^.EmuLockPtr2 := (ppvAudioPtr2 <> 0) ? *ppvAudioPtr2 : 0;
        pThis^.EmuLockBytes2 := (pdwAudioBytes2 <> 0) ? *pdwAudioBytes2 : 0;
     end;

    EmuSwapFS();   // XBox FS

    result:= hRet;
 end;         *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetHeadroom
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetHeadroom
(
    X_CDirectSoundBuffer  *pThis,
    DWORD                  dwHeadroom
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound ($ mod X): EmuIDirectSoundBuffer8_SetHeadroom"
           "("
           "   pThis                     : $ mod .08X"
           "   dwHeadroom                : $ mod .08X"
           ");",
           GetCurrentThreadId(), pThis, dwHeadroom);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    result:= S_OK;
 end;         *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetLoopRegion
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetLoopRegion
(
    X_CDirectSoundBuffer   *pThis,
    DWORD                   dwLoopStart,
    DWORD                   dwLoopLength
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound ($ mod X): EmuIDirectSoundBuffer8_SetLoopRegion"
           "("
           "   pThis                     : $ mod .08X"
           "   dwLoopStart               : $ mod .08X"
           "   dwLoopLength              : $ mod .08X"
           ");",
           GetCurrentThreadId(), pThis, dwLoopStart, dwLoopLength);

    // TODO: Ensure that 4627 & 4361 are intercepting far enough back
    // (otherwise pThis is manipulated!)

    //EmuResizeIDirectSoundBuffer8(pThis, dwLoopLength);

    EmuSwapFS();   // XBox FS

    result:= DS_OK;
 end;      *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_Release
// ******************************************************************
(*ULONG WINAPI XTL.EmuIDirectSoundBuffer8_Release
(
    X_CDirectSoundBuffer   *pThis
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound ($ mod X): EmuIDirectSoundBuffer8_Release"
           "("
           "   pThis                     : $ mod .08X"
           ");",
           GetCurrentThreadId(), pThis);

    ULONG uRet := 0;

    if(pThis <> 0) then 
    begin 
        uRet := pThis^.EmuDirectSoundBuffer8^.Release();

        if(uRet = 0) then 
        begin 
            // remove cache entry
            for(integer v:=0;v<SOUNDBUFFER_CACHE_SIZE;v++)
            begin 
                if(g_pDSoundBufferCache[v] = pThis) then 
                    g_pDSoundBufferCache[v] := 0;
             end;

            if(pThis^.EmuBufferDesc^.lpwfxFormat <> 0) then 
                CxbxFree(pThis^.EmuBufferDesc^.lpwfxFormat);

            CxbxFree(pThis^.EmuBufferDesc);

            delete pThis;
         end;
     end;

    EmuSwapFS();   // XBox FS

    result:= uRet;
 end;            *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetPitch
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetPitch
(
    X_CDirectSoundBuffer   *pThis,
    LongInt                    lPitch
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound ($ mod X): EmuIDirectSoundBuffer8_SetPitch"
           "("
           "   pThis                     : $ mod .08X"
           "   lPitch                    : $ mod .08X"
           ");",
           GetCurrentThreadId(), pThis, lPitch);

    // TODO: Translate params, then make the PC DirectSound call

    EmuSwapFS();   // XBox FS

    result:= DS_OK;
 end;                *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_GetStatus
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_GetStatus
(
    X_CDirectSoundBuffer   *pThis,
    LPDWORD                 pdwStatus
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound ($ mod X): EmuIDirectSoundBuffer8_GetStatus"
           "("
           "   pThis                     : $ mod .08X"
           "   pdwStatus                 : $ mod .08X"
           ");",
           GetCurrentThreadId(), pThis, pdwStatus);

    HRESULT hRet := DS_OK;

    if(pThis <> 0 and pThis^.EmuBuffer = 0) then 
    begin 
        hRet := pThis^.EmuDirectSoundBuffer8^.GetStatus(pdwStatus);
     end;
    else
    begin 
        *pdwStatus := 0;
     end;

    EmuSwapFS();   // XBox FS

    result:= hRet;
 end;           *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetCurrentPosition
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetCurrentPosition
(
    X_CDirectSoundBuffer   *pThis,
    DWORD                   dwNewPosition
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound ($ mod X): EmuIDirectSoundBuffer8_SetCurrentPosition"
           "("
           "   pThis                     : $ mod .08X"
           "   dwNewPosition             : $ mod .08X"
           ");",
           GetCurrentThreadId(), pThis, dwNewPosition);

    // NOTE: TODO: This call *will* (by MSDN) fail on primary buffers!
    HRESULT hRet := pThis^.EmuDirectSoundBuffer8^.SetCurrentPosition(dwNewPosition);

    if(FAILED(hRet)) then 
        EmuWarning("SetCurrentPosition Failed not ");

    EmuSwapFS();   // XBox FS

    result:= hRet;
 end;    *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_GetCurrentPosition
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_GetCurrentPosition
(
    X_CDirectSoundBuffer   *pThis,
    PDWORD                  pdwCurrentPlayCursor,
    PDWORD                  pdwCurrentWriteCursor
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuIDirectSoundBuffer8_GetCurrentPosition"
           "("
           "   pThis                     : 0x mod .08X"
           "   pdwCurrentPlayCursor      : 0x mod .08X"
           "   pdwCurrentWriteCursor     : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis, pdwCurrentPlayCursor, pdwCurrentWriteCursor);

    HackUpdateSoundBuffers();
    HackUpdateSoundStreams();

    // NOTE: TODO: This call always seems to fail on primary buffers!
    HRESULT hRet := pThis^.EmuDirectSoundBuffer8^.GetCurrentPosition(pdwCurrentPlayCursor, pdwCurrentWriteCursor);

    if(FAILED(hRet)) then 
        EmuWarning("GetCurrentPosition Failed not ");

    if(pdwCurrentPlayCursor <> 0 and pdwCurrentWriteCursor <> 0) then 
    begin 
        DbgPrintf("*pdwCurrentPlayCursor :=  mod d, *pdwCurrentWriteCursor :=  mod d", *pdwCurrentPlayCursor, *pdwCurrentWriteCursor);
     end;

    EmuSwapFS();   // XBox FS

    result:= hRet;
 end;         *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_Play
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_Play
(
    X_CDirectSoundBuffer   *pThis,
    DWORD                   dwReserved1,
    DWORD                   dwReserved2,
    DWORD                   dwFlags
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuIDirectSoundBuffer8_Play"
           "("
           "   pThis                     : 0x mod .08X"
           "   dwReserved1               : 0x mod .08X"
           "   dwReserved2               : 0x mod .08X"
           "   dwFlags                   : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis, dwReserved1, dwReserved2, dwFlags);

    if(dwFlags and (~DSBPLAY_LOOPING)) then 
        CxbxKrnlCleanup("Unsupported Playing Flags");

    HackUpdateSoundBuffers();

    // close any existing locks
    if(pThis^.EmuLockPtr1 <> 0) then 
    begin 
        pThis^.EmuDirectSoundBuffer8^.Unlock
        (
            pThis^.EmuLockPtr1,
            pThis^.EmuLockBytes1,
            pThis^.EmuLockPtr2,
            pThis^.EmuLockBytes2
        );

        pThis^.EmuLockPtr1 := 0;
     end;
    
    HRESULT hRet;

    if(pThis^.EmuFlags and DSB_FLAG_ADPCM) then 
    begin 
        hRet := D3D_OK;
     end;
    else
    begin 
        hRet := pThis^.EmuDirectSoundBuffer8^.Play(0, 0, dwFlags);
     end;

    pThis^.EmuPlayFlags := dwFlags;

    EmuSwapFS();   // XBox FS

    result:= hRet;
 end;            *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_Stop
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_Stop
(
    X_CDirectSoundBuffer   *pThis
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuIDirectSoundBuffer8_Stop"
           "("
           "   pThis                     : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis);

    HRESULT hRet := pThis^.EmuDirectSoundBuffer8^.Stop();

    EmuSwapFS();   // XBox FS

    result:= hRet;
 end;                *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_StopEx
// ******************************************************************
(* "C" HRESULT __stdcall XTL.EmuIDirectSoundBuffer8_StopEx
(
    X_CDirectSoundBuffer *pBuffer,
    REFERENCE_TIME        rtTimeStamp,
    DWORD                 dwFlags
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuIDirectSoundBuffer8_StopEx"
           "("
           "   pBuffer                   : 0x mod .08X"
           "   rtTimeStamp               : 0x mod .08X"
           "   dwFlags                   : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pBuffer, rtTimeStamp, dwFlags);

    if(pBuffer^.EmuDirectSoundBuffer8 = 0) then 
        EmuWarning("pBuffer^.EmuDirectSoundBuffer8 := 0");

    EmuWarning("StopEx not yet implemented not ");

    EmuSwapFS();   // XBox FS

    result:= S_OK;
 end;           *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetVolume
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetVolume
(
    X_CDirectSoundBuffer   *pThis,
    LongInt                    lVolume
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuIDirectSoundBuffer8_SetVolume"
           "("
           "   pThis                     : 0x mod .08X"
           "   lVolume                   : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis, lVolume);

    // TODO: Ensure that 4627 & 4361 are intercepting far enough back
    // (otherwise pThis is manipulated!)

//    HRESULT hRet = pThis->EmuDirectSoundBuffer8->SetVolume(lVolume);

    EmuSwapFS();   // XBox FS

//    return hRet;
    result:= S_OK;
 end;   *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetFrequency
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetFrequency
(
    X_CDirectSoundBuffer   *pThis,
    DWORD                   dwFrequency
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuIDirectSoundBuffer8_SetFrequency"
           "("
           "   pThis                     : 0x mod .08X"
           "   dwFrequency               : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis, dwFrequency);

//    HRESULT hRet = pThis->EmuDirectSoundBuffer8->SetFrequency(dwFrequency);

    EmuSwapFS();   // XBox FS

//    return hRet;
    result:= S_OK;
 end;    *)

// ******************************************************************
// * func: EmuDirectSoundCreateStream
// ******************************************************************
(*HRESULT WINAPI XTL.EmuDirectSoundCreateStream
(
    X_DSSTREAMDESC         *pdssd,
    X_CDirectSoundStream  **ppStream
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuDirectSoundCreateStream"
           "("
           "   pdssd                     : 0x mod .08X"
           "   ppStream                  : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pdssd, ppStream);

    // TODO: Garbage Collection
    *ppStream := new X_CDirectSoundStream();
    
    DSBUFFERDESC *pDSBufferDesc := (DSBUFFERDESC)CxbxMalloc(SizeOf(DSBUFFERDESC));
    
    // convert from Xbox to PC DSound
    begin 
        DWORD dwAcceptableMask := 0x00000010; // TODO: Note 0x00040000 is being ignored (DSSTREAMCAPS_LOCDEFER)

        if(pdssd^.dwFlags and (~dwAcceptableMask)) then 
            EmuWarning("Use of unsupported pdssd^.dwFlags mask(s) (0x mod .08X)", pdssd^.dwFlags and (~dwAcceptableMask));

        pDSBufferDesc^.dwSize := SizeOf(DSBUFFERDESC);
//        pDSBufferDesc->dwFlags = (pdssd->dwFlags & dwAcceptableMask) | DSBCAPS_CTRLVOLUME | DSBCAPS_GETCURRENTPOSITION2;
        pDSBufferDesc^.dwFlags := DSBCAPS_CTRLVOLUME;
        pDSBufferDesc^.dwBufferBytes := DSBSIZE_MIN;

        pDSBufferDesc^.dwReserved := 0;

        if(pdssd^.lpwfxFormat <> 0) then 
        begin 
            pDSBufferDesc^.lpwfxFormat := (WAVEFORMATEX)CxbxMalloc(SizeOf(WAVEFORMATEX));
            memcpy(pDSBufferDesc^.lpwfxFormat, pdssd^.lpwfxFormat, SizeOf(WAVEFORMATEX));
         end;

        pDSBufferDesc^.guid3DAlgorithm := DS3DALG_DEFAULT;

        if(pDSBufferDesc^.lpwfxFormat <> 0 and pDSBufferDesc^.lpwfxFormat^.wFormatTag <> WAVE_FORMAT_PCM) then 
        begin 
            EmuWarning("Invalid WAVE_FORMAT not ");
			if(pDSBufferDesc^.lpwfxFormat^.wFormatTag = (*WAVE_FORMAT_XBOX_ADPCM*)(*0x0069) then
				EmuWarning("WAVE_FORMAT_XBOX_ADPCM Unsupported not ");

            (ppStream)^.EmuDirectSoundBuffer8 := 0;

            EmuSwapFS();   // XBox FS

            result:= DS_OK;
         end;

        // we only support 2 channels right now
        if(pDSBufferDesc^.lpwfxFormat^.nChannels > 2) then 
        begin 
            pDSBufferDesc^.lpwfxFormat^.nChannels := 2;
            pDSBufferDesc^.lpwfxFormat^.nBlockAlign := (2*pDSBufferDesc^.lpwfxFormat^.wBitsPerSample)/8;
            pDSBufferDesc^.lpwfxFormat^.nAvgBytesPerSec := pDSBufferDesc^.lpwfxFormat^.nSamplesPerSec * pDSBufferDesc^.lpwfxFormat^.nBlockAlign;
         end;
     end;

    (ppStream)^.EmuBuffer := 0;
    (ppStream)^.EmuBufferDesc := pDSBufferDesc;
    (ppStream)^.EmuLockPtr1 := 0;
    (ppStream)^.EmuLockBytes1 := 0;
    (ppStream)^.EmuLockPtr2 := 0;
    (ppStream)^.EmuLockBytes2 := 0;

    DbgPrintf("EmuDSound (0x mod X): EmuDirectSoundCreateStream, *ppStream := 0x mod .08X", GetCurrentThreadId(), *ppStream);

    HRESULT hRet := g_pDSound8^.CreateSoundBuffer(pDSBufferDesc,  and (ppStream)^.EmuDirectSoundBuffer8, 0);

    if(FAILED(hRet)) then 
        EmuWarning("CreateSoundBuffer Failed not ");

    // cache this sound stream
    begin 
        integer v:=0;
        for(v:=0;v<SOUNDSTREAM_CACHE_SIZE;v++)
        begin 
            if(g_pDSoundStreamCache[v] = 0) then 
            begin 
                g_pDSoundStreamCache[v] := *ppStream;
                break;
             end;
         end;

        if(v = SOUNDSTREAM_CACHE_SIZE) then 
            CxbxKrnlCleanup("SoundStream cache out of slots not ");
     end;

    EmuSwapFS();   // XBox FS

    result:= DS_OK;
 end;       *)

// ******************************************************************
// * func: EmuIDirectSound8_CreateStream
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSound8_CreateStream
(
    LPDIRECTSOUND8          pThis,
    X_DSSTREAMDESC         *pdssd,
    X_CDirectSoundStream  **ppStream,
    PVOID                   pUnknown
)
begin 
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf("EmuDSound (0x mod X): EmuIDirectSound8_CreateStream"
               "("
               "   pThis                     : 0x mod .08X"
               "   pdssd                     : 0x mod .08X"
               "   ppStream                  : 0x mod .08X"
               "   pUnknown                  : 0x mod .08X"
               ");",
               GetCurrentThreadId(), pThis, pdssd, ppStream, pUnknown);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    EmuDirectSoundCreateStream(pdssd, ppStream);

    result:= DS_OK;
 end;     *)

// ******************************************************************
// * func: EmuCMcpxStream_Dummy_0x10
// ******************************************************************
(*VOID WINAPI XTL.EmuCMcpxStream_Dummy_0x10(DWORD dwDummy1, DWORD dwDummy2)
begin 
    EmuWarning("EmuCMcpxStream_Dummy_0x10 is ignored not ");
    Exit;
 end;    *)

// ******************************************************************
// * func: EmuCDirectSoundStream_SetVolume
// ******************************************************************
(*ULONG WINAPI XTL.EmuCDirectSoundStream_SetVolume(X_CDirectSoundStream *pThis, LongInt lVolume)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuCDirectSoundStream_SetVolume"
           "("
           "   pThis                     : 0x mod .08X"
           "   lVolume                   :  mod d"
           ");",
           GetCurrentThreadId(), pThis, lVolume);

    // TODO: Actually SetVolume

    EmuSwapFS();   // XBox FS

    result:= DS_OK;
 end;       *)

// ******************************************************************
// * func: EmuCDirectSoundStream_SetRolloffFactor
// ******************************************************************
(*HRESULT WINAPI XTL.EmuCDirectSoundStream_SetRolloffFactor
(
    X_CDirectSoundStream *pThis,
    FLOAT                 fRolloffFactor,
    DWORD                 dwApply
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuCDirectSoundStream_SetRolloffFactor"
           "("
           "   pThis                     : 0x mod .08X"
           "   fRolloffFactor            :  mod f"
           "   dwApply                   : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis, fRolloffFactor, dwApply);

    // TODO: Actually SetRolloffFactor

    EmuSwapFS();   // XBox FS

    result:= DS_OK;
 end;       *)

// ******************************************************************
// * func: EmuCDirectSoundStream_AddRef
// ******************************************************************
(*ULONG WINAPI XTL.EmuCDirectSoundStream_AddRef(X_CDirectSoundStream *pThis)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuCDirectSoundStream_AddRef"
           "("
           "   pThis                     : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis);

    if(pThis <> 0) then 
        pThis^.EmuDirectSoundBuffer8^.AddRef();

    EmuSwapFS();   // XBox FS

    result:= DS_OK;
 end;         *)

// ******************************************************************
// * func: EmuCDirectSoundStream_Release
// ******************************************************************
(*ULONG WINAPI XTL.EmuCDirectSoundStream_Release(X_CDirectSoundStream *pThis)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuCDirectSoundStream_Release"
           "("
           "   pThis                     : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis);

    ULONG uRet := 0;

    if(pThis <> 0 and (pThis^.EmuDirectSoundBuffer8 <> 0)) then 
    begin 
        uRet := pThis^.EmuDirectSoundBuffer8^.Release();

        if(uRet = 0) then 
        begin 
            // remove cache entry
            for(integer v:=0;v<SOUNDSTREAM_CACHE_SIZE;v++)
            begin 
                if(g_pDSoundStreamCache[v] = pThis) then 
                    g_pDSoundStreamCache[v] := 0;
             end;

            if(pThis^.EmuBufferDesc^.lpwfxFormat <> 0) then 
                CxbxFree(pThis^.EmuBufferDesc^.lpwfxFormat);

            CxbxFree(pThis^.EmuBufferDesc);

            delete pThis;
         end;
     end;

    EmuSwapFS();   // XBox FS

    result:= uRet;
 end;      *)

// ******************************************************************
// * func: EmuCDirectSoundStream_GetStatus
// ******************************************************************
(*HRESULT WINAPI XTL.EmuCDirectSoundStream_GetStatus
(
    X_CDirectSoundStream   *pThis,
    DWORD                  *pdwStatus
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuCDirectSoundStream_GetStatus"
           "("
           "   pThis                     : 0x mod .08X"
           "   pdwStatus                 : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis, pdwStatus);

    EmuWarning("EmuCDirectSoundStream_GetStatus is not yet implemented");

    *pdwStatus := DSBSTATUS_PLAYING;

    EmuSwapFS();   // XBox FS

    result:= DS_OK;
 end;       *)

// ******************************************************************
// * func: EmuCDirectSoundStream_Process
// ******************************************************************
(*HRESULT WINAPI XTL.EmuCDirectSoundStream_Process
(
    X_CDirectSoundStream   *pThis,
    PXMEDIAPACKET           pInputBuffer,
    PXMEDIAPACKET           pOutputBuffer
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuCDirectSoundStream_Process"
           "("
           "   pThis                     : 0x mod .08X"
           "   pInputBuffer              : 0x mod .08X"
           "   pOutputBuffer             : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis, pInputBuffer, pOutputBuffer);

    if(pThis^.EmuDirectSoundBuffer8 <> 0) then 
    begin 
        // update buffer data cache
        pThis^.EmuBuffer := pInputBuffer^.pvBuffer;

        EmuResizeIDirectSoundStream8(pThis, pInputBuffer^.dwMaxSize);

        if(pInputBuffer^.pdwStatus <> 0) then 
            *pInputBuffer^.pdwStatus := S_OK;

        HackUpdateSoundStreams();
     end;
    else
    begin 
        if(pInputBuffer^.pdwStatus <> 0) then 
            *pInputBuffer^.pdwStatus := S_OK;
     end;

    EmuSwapFS();   // XBox FS

    result:= DS_OK;
 end;         *)

// ******************************************************************
// * func: EmuCDirectSoundStream_Discontinuity
// ******************************************************************
(*HRESULT WINAPI XTL.EmuCDirectSoundStream_Discontinuity(X_CDirectSoundStream *pThis)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuCDirectSoundStream_Discontinuity"
           "("
           "   pThis                     : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis);

    // TODO: Actually Process

    EmuSwapFS();   // XBox FS

    result:= DS_OK;
 end;          *)

// ******************************************************************
// * func: EmuCDirectSoundStream_Flush
// ******************************************************************
(*HRESULT WINAPI XTL.EmuCDirectSoundStream_Flush(X_CDirectSoundStream *pThis)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuCDirectSoundStream_Flush();",
           GetCurrentThreadId(), pThis);

    // TODO: Actually Flush

    EmuSwapFS();   // XBox FS

    result:= DS_OK;
 end;          *)

// ******************************************************************
// * func: EmuCDirectSound_SynchPlayback
// ******************************************************************
(*HRESULT WINAPI XTL.EmuCDirectSound_SynchPlayback(PVOID pUnknown)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuCDirectSound_SynchPlayback(0x mod .08X);",
           GetCurrentThreadId());

    EmuSwapFS();   // XBox FS

    result:= DS_OK;
 end;           *)

// ******************************************************************
// * func: EmuCDirectSoundStream_Pause
// ******************************************************************
(*HRESULT WINAPI XTL.EmuCDirectSoundStream_Pause
(
    PVOID   pStream,
    DWORD   dwPause
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuCDirectSoundStream_Pause"
           "("
           "   pStream                   : 0x mod .08X"
           "   dwPause                   : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pStream, dwPause);

    EmuSwapFS();   // XBox FS

    result:= DS_OK;
 end;        *)

// ******************************************************************
// * func: EmuIDirectSoundStream_SetHeadroom
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundStream_SetHeadroom
(
    PVOID   pThis,
    DWORD   dwHeadroom
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuIDirectSoundStream_SetHeadroom"
           "("
           "   pThis                     : 0x mod .08X"
           "   dwHeadroom                : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis, dwHeadroom);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    result:= S_OK;
 end;      *)

// ******************************************************************
// * func: EmuCDirectSoundStream_SetConeAngles
// ******************************************************************
(*HRESULT WINAPI XTL.EmuCDirectSoundStream_SetConeAngles
(
    PVOID   pThis,
    DWORD   dwInsideConeAngle,
    DWORD   dwOutsideConeAngle,
    DWORD   dwApply
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuCDirectSoundStream_SetConeAngles"
           "("
           "   pThis                     : 0x mod .08X"
           "   dwInsideConeAngle         : 0x mod .08X"
           "   dwOutsideConeAngle        : 0x mod .08X"
           "   dwApply                   : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis, dwInsideConeAngle, dwOutsideConeAngle, dwApply);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    result:= S_OK;
 end;              *)

// ******************************************************************
// * func: EmuCDirectSoundStream_SetConeOutsideVolume
// ******************************************************************
(*HRESULT WINAPI XTL.EmuCDirectSoundStream_SetConeOutsideVolume
(
    PVOID   pThis,
    LongInt    lConeOutsideVolume,
    DWORD   dwApply
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuCDirectSoundStream_SetConeOutsideVolume"
           "("
           "   pThis                     : 0x mod .08X"
           "   lConeOutsideVolume        :  mod d"
           "   dwApply                   : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis, lConeOutsideVolume, dwApply);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    result:= S_OK;
 end;     *)

// ******************************************************************
// * func: EmuCDirectSoundStream_SetAllParameters
// ******************************************************************
(*HRESULT WINAPI XTL.EmuCDirectSoundStream_SetAllParameters
(
    PVOID    pThis,
    PVOID    pUnknown,
    DWORD    dwApply
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuCDirectSoundStream_SetAllParameters"
           "("
           "   pThis                     : 0x mod .08X"
           "   pUnknown                  :  mod f"
           "   dwApply                   : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis, pUnknown, dwApply);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    result:= S_OK;
 end;      *)

// ******************************************************************
// * func: EmuCDirectSoundStream_SetMaxDistance
// ******************************************************************
(*HRESULT WINAPI XTL.EmuCDirectSoundStream_SetMaxDistance
(
    PVOID    pThis,
    D3DVALUE fMaxDistance,
    DWORD    dwApply
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuCDirectSoundStream_SetMaxDistance"
           "("
           "   pThis                     : 0x mod .08X"
           "   fMaxDistance              :  mod f"
           "   dwApply                   : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis, fMaxDistance, dwApply);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    result:= S_OK;
 end;          *)

// ******************************************************************
// * func: EmuCDirectSoundStream_SetMinDistance
// ******************************************************************
(*HRESULT WINAPI XTL.EmuCDirectSoundStream_SetMinDistance
(
    PVOID    pThis,
    D3DVALUE fMinDistance,
    DWORD    dwApply
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuCDirectSoundStream_SetMinDistance"
           "("
           "   pThis                     : 0x mod .08X"
           "   fMinDistance              :  mod f"
           "   dwApply                   : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis, fMinDistance, dwApply);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    result:= S_OK;
 end;        *)

// ******************************************************************
// * func: EmuCDirectSoundStream_SetVelocity
// ******************************************************************
(*HRESULT WINAPI XTL.EmuCDirectSoundStream_SetVelocity
(
    PVOID    pThis,
    D3DVALUE x,
    D3DVALUE y,
    D3DVALUE z,
    DWORD    dwApply
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuCDirectSoundStream_SetVelocity"
           "("
           "   pThis                     : 0x mod .08X"
           "   x                         :  mod f"
           "   y                         :  mod f"
           "   z                         :  mod f"
           "   dwApply                   : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis, x, y, z, dwApply);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    result:= S_OK;
 end;       *)

// ******************************************************************
// * func: EmuCDirectSoundStream_SetConeOrientation
// ******************************************************************
(*HRESULT WINAPI XTL.EmuCDirectSoundStream_SetConeOrientation
(
    PVOID    pThis,
    D3DVALUE x,
    D3DVALUE y,
    D3DVALUE z,
    DWORD    dwApply
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuCDirectSoundStream_SetConeOrientation"
           "("
           "   pThis                     : 0x mod .08X"
           "   x                         :  mod f"
           "   y                         :  mod f"
           "   z                         :  mod f"
           "   dwApply                   : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis, x, y, z, dwApply);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    result:= S_OK;
 end;       *)

// ******************************************************************
// * func: EmuCDirectSoundStream_SetPosition
// ******************************************************************
(*HRESULT WINAPI XTL.EmuCDirectSoundStream_SetPosition
(
    PVOID    pThis,
    D3DVALUE x,
    D3DVALUE y,
    D3DVALUE z,
    DWORD    dwApply
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuCDirectSoundStream_SetPosition"
           "("
           "   pThis                     : 0x mod .08X"
           "   x                         :  mod f"
           "   y                         :  mod f"
           "   z                         :  mod f"
           "   dwApply                   : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis, x, y, z, dwApply);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    result:= S_OK;
 end;        *)

// ******************************************************************
// * func: EmuCDirectSoundStream_SetFrequency
// ******************************************************************
(*HRESULT WINAPI XTL.EmuCDirectSoundStream_SetFrequency
(
    PVOID   pThis,
    DWORD   dwFrequency
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuCDirectSoundStream_SetFrequency"
           "("
           "   pThis                     : 0x mod .08X"
           "   dwFrequency               :  mod d"
           ");",
           GetCurrentThreadId(), pThis, dwFrequency);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    result:= S_OK;
 end;             *)

// ******************************************************************
// * func: EmuIDirectSoundStream_SetI3DL2Source
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundStream_SetI3DL2Source
(
    PVOID   pThis,
    PVOID   pds3db,
    DWORD   dwApply
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuIDirectSoundStream_SetI3DL2Source"
           "("
           "   pThis                     : 0x mod .08X"
           "   pds3db                    : 0x mod .08X"
           "   dwApply                   : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis, pds3db, dwApply);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    result:= S_OK;
 end;      *)

function XTL_EmuIDirectSoundStream_Unknown1 (pThis : PVOID; dwUnknown1 : DWORD ): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:10
begin
  EmuSwapFS(fsWindows);   // Win2k/XP FS

  (*DbgPrintf('EmuDSound (0%x mod X): EmuIDirectSoundStream_Unknown1' +
         '(' +
         '   pThis                     : 0x% mod.08X' +
         '   dwUnknown1                : 0x% mod.08X' +
         ');',
         GetCurrentThreadId(), pThis, dwUnknown1); *)

  // TODO: Actually implement this

  EmuSwapFS(fsXbox);   // XBox FS

  result:= S_OK;
 end;

// s+
// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetMaxDistance
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetMaxDistance
(
    LPDIRECTSOUNDBUFFER8    pThis,
    FLOAT                   flMaxDistance,
    DWORD                   dwApply
)
begin 
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf("EmuDSound (0x mod X): EmuIDirectSoundBuffer8_SetMaxDistance"
               "("
               "   pThis                     : 0x mod .08X"
               "   flMaxDistance             :  mod f"
               "   dwApply                   : 0x mod .08X"
               ");",
               GetCurrentThreadId(), pThis, flMaxDistance, dwApply);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    // TODO: Actually do something

    result:= DS_OK;
 end;   *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetMinDistance
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetMinDistance
(
    LPDIRECTSOUNDBUFFER8    pThis,
    FLOAT                   flMinDistance,
    DWORD                   dwApply
)
begin 
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf("EmuDSound (0x mod X): EmuIDirectSoundBuffer8_SetMinDistance"
               "("
               "   pThis                     : 0x mod .08X"
               "   flMinDistance             :  mod f"
               "   dwApply                   : 0x mod .08X"
               ");",
               GetCurrentThreadId(), pThis, flMinDistance, dwApply);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    // TODO: Actually do something

    result:= DS_OK;
 end;         *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetRolloffFactor
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetRolloffFactor
(
    LPDIRECTSOUNDBUFFER8    pThis,
    FLOAT                   flRolloffFactor,
    DWORD                   dwApply
)
begin 
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf("EmuDSound (0x mod X): EmuIDirectSoundBuffer8_SetRolloffFactor"
               "("
               "   pThis                     : 0x mod .08X"
               "   flRolloffFactor           :  mod f"
               "   dwApply                   : 0x mod .08X"
               ");",
               GetCurrentThreadId(), pThis, flRolloffFactor, dwApply);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    // TODO: Actually do something

    result:= DS_OK;
 end;       *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetDistanceFactor
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetDistanceFactor
(
    LPDIRECTSOUNDBUFFER8    pThis,
    FLOAT                   flDistanceFactor,
    DWORD                   dwApply
)
begin 
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf("EmuDSound (0x mod X): EmuIDirectSoundBuffer8_SetDistanceFactor"
               "("
               "   pThis                     : 0x mod .08X"
               "   flDistanceFactor          :  mod f"
               "   dwApply                   : 0x mod .08X"
               ");",
               GetCurrentThreadId(), pThis, flDistanceFactor, dwApply);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    // TODO: Actually do something

    result:= DS_OK;
 end;   *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetConeAngles
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetConeAngles
(
    LPDIRECTSOUNDBUFFER8    pThis,
    DWORD                   dwInsideConeAngle,
    DWORD                   dwOutsideConeAngle,
    DWORD                   dwApply
)
begin 
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf("EmuDSound (0x mod X): EmuIDirectSoundBuffer8_SetConeAngles"
               "("
               "   pThis                     : 0x mod .08X"
               "   dwInsideConeAngle         : 0x mod .08X"
               "   dwOutsideConeAngle        : 0x mod .08X"
               "   dwApply                   : 0x mod .08X"
               ");",
               GetCurrentThreadId(), pThis, dwInsideConeAngle,
               dwOutsideConeAngle, dwApply);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    // TODO: Actually do something

    result:= DS_OK;
 end;      *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetConeOrientation
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetConeOrientation
(
    LPDIRECTSOUNDBUFFER8    pThis,
    FLOAT                   x,
    FLOAT                   y,
    FLOAT                   z,
    DWORD                   dwApply
)
begin 
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf("EmuDSound (0x mod X): EmuIDirectSoundBuffer8_SetConeOrientation"
               "("
               "   pThis                     : 0x mod .08X"
               "   x                         :  mod f"
               "   y                         :  mod f"
               "   z                         :  mod f"
               "   dwApply                   : 0x mod .08X"
               ");",
               GetCurrentThreadId(), pThis, x, y, z, dwApply);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    // TODO: Actually do something

    result:= DS_OK;
 end;     *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetConeOutsideVolume
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetConeOutsideVolume
(
    LPDIRECTSOUNDBUFFER8    pThis,
    LongInt                    lConeOutsideVolume,
    DWORD                   dwApply
)
begin 
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf("EmuDSound (0x mod X): EmuIDirectSoundBuffer8_SetConeOutsideVolume"
               "("
               "   pThis                     : 0x mod .08X"
               "   lConeOutsideVolume        : 0x mod .08X"
               "   dwApply                   : 0x mod .08X"
               ");",
               GetCurrentThreadId(), pThis, lConeOutsideVolume, dwApply);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    // TODO: Actually do something

    result:= DS_OK;
 end;     *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetPosition
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetPosition
(
    LPDIRECTSOUNDBUFFER8    pThis,
    FLOAT                   x,
    FLOAT                   y,
    FLOAT                   z,
    DWORD                   dwApply
)
begin 
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf("EmuDSound (0x mod X): EmuIDirectSoundBuffer8_SetPosition"
               "("
               "   pThis                     : 0x mod .08X"
               "   x                         :  mod f"
               "   y                         :  mod f"
               "   z                         :  mod f"
               "   dwApply                   : 0x mod .08X"
               ");",
               GetCurrentThreadId(), pThis, x, y, z, dwApply);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    // TODO: Actually do something

    result:= DS_OK;
 end;    *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetVelocity
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetVelocity
(
    LPDIRECTSOUNDBUFFER8    pThis,
    FLOAT                   x,
    FLOAT                   y,
    FLOAT                   z,
    DWORD                   dwApply
)
begin 
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf("EmuDSound (0x mod X): EmuIDirectSoundBuffer8_SetVelocity"
               "("
               "   pThis                     : 0x mod .08X"
               "   x                         :  mod f"
               "   y                         :  mod f"
               "   z                         :  mod f"
               "   dwApply                   : 0x mod .08X"
               ");",
               GetCurrentThreadId(), pThis, x, y, z, dwApply);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    // TODO: Actually do something

    result:= DS_OK;
 end;     *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetDopplerFactor
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetDopplerFactor
(
    LPDIRECTSOUNDBUFFER8    pThis,
    FLOAT                   flDopplerFactor,
    DWORD                   dwApply
)
begin 
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf("EmuDSound (0x mod X): EmuIDirectSoundBuffer8_SetConeOutsideVolume"
               "("
               "   pThis                     : 0x mod .08X"
               "   flDopplerFactor           :  mod f"
               "   dwApply                   : 0x mod .08X"
               ");",
               GetCurrentThreadId(), pThis, flDopplerFactor, dwApply);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    // TODO: Actually do something

    result:= DS_OK;
 end;    *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetI3DL2Source
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetI3DL2Source
(
    LPDIRECTSOUNDBUFFER8    pThis,
    LPCDSI3DL2BUFFER        pds3db,
    DWORD                   dwApply
)
begin 
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf("EmuDSound (0x mod X): EmuIDirectSoundBuffer8_SetI3DL2Source"
               "("
               "   pThis                     : 0x mod .08X"
               "   pds3db                    : 0x mod .08X"
               "   dwApply                   : 0x mod .08X"
               ");",
               GetCurrentThreadId(), pThis, pds3db, dwApply);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    // TODO: Actually do something

    result:= DS_OK;
 end;    *)
// +s
// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetFormat
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetFormat
(
    X_CDirectSoundBuffer *pBuffer, 
    LPCWAVEFORMATEX pwfxFormat
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        printf("EmuDSound (0x mod X): EmuIDirectSoundBuffer8_SetFormat"
               "("
               "   pBuffer                   : 0x mod .08X"
               "   pwfxFormat                : 0x mod .08X"
               ");",
               GetCurrentThreadId(), pBuffer,pwfxFormat);
     end;
    //endif

    HRESULT hRet := DS_OK;

    EmuSwapFS();   // XBox FS

    result:= hRet;
 end;      *)

// ******************************************************************
// * func: EmuDirectSoundUseFullHRTF
// ******************************************************************
(*STDAPI_(STDAPI_(procedure: ) EmuDirectSoundUseFullHRTF;d
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuDirectSoundUseFullHRTF()", GetCurrentThreadId());

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS
 end;          *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetLFO
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetLFO
(
	LPDIRECTSOUNDBUFFER  pThis,
	LPCDSLFODESC         pLFODesc
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuIDirectSoundBuffer8_SetLFO"
           "("
           "   pThis                     : 0x mod .08X"
           "   pLFODesc                  : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis, pLFODesc);

	// TODO: Implement

    EmuSwapFS();   // XBox FS

    result:= S_OK;
 end;    *)

// ******************************************************************
// * func: EmuXAudioCreateAdpcmFormat
// ******************************************************************
(*VOID WINAPI XTL.EmuXAudioCreateAdpcmFormat
(
	WORD                   nChannels,
	DWORD                  nSamplesPerSec,
	LPXBOXADPCMWAVEFORMAT  pwfx
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuXAudioCreateAdpcmFormat"
           "("
           "   nChannels                 : 0x mod .04X"
           "   nSamplesPerSec            : 0x mod .08X"
           "   pwfx                      : 0x mod .08X"
           ");",
           GetCurrentThreadId(), nChannels, nSamplesPerSec, pwfx);

	// Fill out the pwfx structure with the appropriate data
	pwfx^.wfx.wFormatTag		:= WAVE_FORMAT_XBOX_ADPCM;
	pwfx^.wfx.nChannels			:= nChannels;
	pwfx^.wfx.nSamplesPerSec	:= nSamplesPerSec;
	pwfx^.wfx.nAvgBytesPerSec	:= (nSamplesPerSec*nChannels * 36)/64;
	pwfx^.wfx.nBlockAlign		:= nChannels * 36;
	pwfx^.wfx.wBitsPerSample	:= 4;
	pwfx^.wfx.cbSize			:= 2;
	pwfx^.wSamplesPerBlock		:= 64;

    EmuSwapFS();   // XBox FS
 end;     *)

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetRolloffCurve
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetRolloffCurve
(
	LPDIRECTSOUNDBUFFER  pThis,
	 FLOAT         *pflPoints,
	DWORD                dwPointCount,
	DWORD                dwApply
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuIDirectSoundBuffer8_SetRolloffCurve"
           "("
           "   pThis                     : 0x mod .08X"
           "   pflPoints                 : 0x mod .08X"
           "   dwPointCount              : 0x mod .08X"
		   "   dwApply                   : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pThis, pflPoints, dwPointCount, dwApply);

	// TODO: Implement

    EmuSwapFS();   // XBox FS

	result:= DS_OK;
 end;   *)

// ******************************************************************
// * func: EmuIDirectSoundStream_SetVolume
// ******************************************************************
(*HRESULT WINAPI XTL.EmuIDirectSoundStream_SetVolume
(
	LPDIRECTSOUNDSTREAM pStream,
	LongInt                lVolume
)
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf("EmuDSound (0x mod X): EmuIDirectSoundStream_SetVolume"
           "("
           "   pStream                   : 0x mod .08X"
           "   lVolume                   : 0x mod .08X"
           ");",
           GetCurrentThreadId(), pStream, lVolume);

	// TODO: Implement

    EmuSwapFS();   // XBox FS

	result:= DS_OK;
 end;   *)


end.
