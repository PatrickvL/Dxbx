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
   g_pDSound8: IDIRECTSOUND8 = nil;
   g_pDSound8RefCount: Int = 0;

// periodically update sound buffers
procedure HackUpdateSoundBuffers();
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
(*var
  v: Integer; *)
begin
    (*for v:=0 to SOUNDBUFFER_CACHE_SIZE -1 do begin
        if(g_pDSoundBufferCache[v] = 0 or g_pDSoundBufferCache[v].EmuBuffer = 0) then
            continue;

        PVOID pAudioPtr, pAudioPtr2;
        DWORD dwAudioBytes, dwAudioBytes2;

        // unlock existing lock
        if(g_pDSoundBufferCache[v].EmuLockPtr1 <> 0) then
            g_pDSoundBufferCache[v].EmuDirectSoundBuffer8.Unlock(g_pDSoundBufferCache[v].EmuLockPtr1, g_pDSoundBufferCache[v].EmuLockBytes1, g_pDSoundBufferCache[v].EmuLockPtr2, g_pDSoundBufferCache[v].EmuLockBytes2);

        HRESULT hRet := g_pDSoundBufferCache[v].EmuDirectSoundBuffer8.Lock(0, g_pDSoundBufferCache[v].EmuBufferDesc.dwBufferBytes, @pAudioPtr, @dwAudioBytes, @pAudioPtr2, @dwAudioBytes2, 0);

        if(SUCCEEDED(hRet)) then
        begin
            if(pAudioPtr <> 0) then
                memcpy(pAudioPtr,  g_pDSoundBufferCache[v].EmuBuffer, dwAudioBytes);

            if(pAudioPtr2 <> 0) then
                memcpy(pAudioPtr2, (PVOID)((DWORD)g_pDSoundBufferCache[v].EmuBuffer+dwAudioBytes), dwAudioBytes2);

            g_pDSoundBufferCache[v].EmuDirectSoundBuffer8.Unlock(pAudioPtr, dwAudioBytes, pAudioPtr2, dwAudioBytes2);
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
        if(g_pDSoundStreamCache[v] = 0 or g_pDSoundStreamCache[v].EmuBuffer = 0) then
            continue;

        PVOID pAudioPtr, pAudioPtr2;
        DWORD dwAudioBytes, dwAudioBytes2;

        HRESULT hRet := g_pDSoundStreamCache[v].EmuDirectSoundBuffer8.Lock(0, g_pDSoundStreamCache[v].EmuBufferDesc.dwBufferBytes, @pAudioPtr, @dwAudioBytes, @pAudioPtr2, @dwAudioBytes2, 0);

        if(SUCCEEDED(hRet)) then
        begin
            if(pAudioPtr <> 0) then
                memcpy(pAudioPtr,  g_pDSoundStreamCache[v].EmuBuffer, dwAudioBytes);

            if(pAudioPtr2 <> 0) then
                memcpy(pAudioPtr2, (PVOID)((DWORD)g_pDSoundStreamCache[v].EmuBuffer+dwAudioBytes), dwAudioBytes2);

            g_pDSoundStreamCache[v].EmuDirectSoundBuffer8.Unlock(pAudioPtr, dwAudioBytes, pAudioPtr2, dwAudioBytes2);
         end;

        g_pDSoundStreamCache[v].EmuDirectSoundBuffer8.SetCurrentPosition(0);
        g_pDSoundStreamCache[v].EmuDirectSoundBuffer8.Play(0, 0, 0);
     end;

    Exit; *)
 end;

// resize an emulated directsound buffer, if necessary
(*  procedure EmuResizeIDirectSoundBuffer8(var pThis: XTL.X_CDirectSoundBuffer; dwBytes: DWORD);
    if(dwBytes = pThis.EmuBufferDesc.dwBufferBytes or dwBytes = 0) then
        Exit;

    DbgPrintf('EmuResizeIDirectSoundBuffer8 : Resizing not  ($%.08X.$%.08X)', pThis.EmuBufferDesc.dwBufferBytes, dwBytes);

    DWORD dwPlayCursor, dwWriteCursor, dwStatus;

    HRESULT hRet := pThis.EmuDirectSoundBuffer8.GetCurrentPosition(@dwPlayCursor, @dwWriteCursor);

    if(FAILED(hRet)) then 
        CxbxKrnlCleanup('Unable to retrieve current position for resize reallocation not ');

    hRet := pThis.EmuDirectSoundBuffer8.GetStatus(@dwStatus);

    if(FAILED(hRet)) then 
        CxbxKrnlCleanup('Unable to retrieve current status for resize reallocation not ');

    // release old buffer
    while(pThis.EmuDirectSoundBuffer8.Release() > 0) begin   end;

    pThis.EmuBufferDesc.dwBufferBytes := dwBytes;

    hRet := g_pDSound8.CreateSoundBuffer(pThis.EmuBufferDesc, @pThis.EmuDirectSoundBuffer8, 0);

    if(FAILED(hRet)) then 
        CxbxKrnlCleanup('IDirectSoundBuffer8 resize Failed not ');

    pThis.EmuDirectSoundBuffer8.SetCurrentPosition(dwPlayCursor);

    if(dwStatus and DSBSTATUS_PLAYING) then 
        pThis.EmuDirectSoundBuffer8.Play(0, 0, pThis.EmuPlayFlags);
 end;        *)

// resize an emulated directsound stream, if necessary
(*  procedure EmuResizeIDirectSoundStream8(var pThis: XTL.X_CDirectSoundStream; dwBytes: DWORD);
    if(dwBytes = pThis.EmuBufferDesc.dwBufferBytes) then 
        Exit;

    DWORD dwPlayCursor, dwWriteCursor, dwStatus;

    HRESULT hRet := pThis.EmuDirectSoundBuffer8.GetCurrentPosition(@dwPlayCursor, @dwWriteCursor);

    if(FAILED(hRet)) then 
        CxbxKrnlCleanup('Unable to retrieve current position for resize reallocation not ');

    hRet := pThis.EmuDirectSoundBuffer8.GetStatus(@dwStatus);

    if(FAILED(hRet)) then 
        CxbxKrnlCleanup('Unable to retrieve current status for resize reallocation not ');

    // release old buffer
    while(pThis.EmuDirectSoundBuffer8.Release() > 0) begin   end;

    pThis.EmuBufferDesc.dwBufferBytes := dwBytes;

    hRet := g_pDSound8.CreateSoundBuffer(pThis.EmuBufferDesc, @pThis.EmuDirectSoundBuffer8, 0);

    if(FAILED(hRet)) then 
        CxbxKrnlCleanup('IDirectSoundBuffer8 resize Failed not ');

    pThis.EmuDirectSoundBuffer8.SetCurrentPosition(dwPlayCursor);

    if (dwStatus and DSBSTATUS_PLAYING) > 0 then 
        pThis.EmuDirectSoundBuffer8.Play(0, 0, pThis.EmuPlayFlags);
 end;     *)

function XTL_EmuDirectSoundCreate(
    pguidDeviceId: Pointer;
    ppDirectSound: LPDIRECTSOUND8;
    pUnknown: IUNKNOWN): HRESULT; stdcall;
// Branch:martin  Revision:80  Translator:Shadow_Tj  Done:80
{$J+}
const
  Initialized: Bool = False;
{$J-}
var
  hRet: HRESULT;
//  v: Integer;
begin
  EmuSwapFS(fsWindows);

  DbgPrintf('EmuDSound : EmuDirectSoundCreate' +
      #13#10'(' +
      #13#10'   pguidDeviceId             : 0x%.08X' +
      #13#10'   ppDirectSound             : 0x%.08X' +
      #13#10'   pUnknown                  : 0x%.08X' +
      #13#10');',
            [pguidDeviceId, ppDirectSound, pUnknown]);


  hRet := DS_OK;

  if not Initialized then
  begin 
    hRet := DirectSoundCreate8(nil, ppDirectSound^, nil);

    if FAILED(hRet) then
      CxbxKrnlCleanup('DirectSoundCreate8 Failed not ');

    g_pDSound8 := ppDirectSound^;

    hRet := g_pDSound8.SetCooperativeLevel(g_hEmuWindow, DSSCL_PRIORITY);

    if FAILED(hRet) then
      CxbxKrnlCleanup('g_pDSound8.SetCooperativeLevel Failed not ');

    (*// clear sound buffer cache
    v := 0;
    for v:=0 to SOUNDBUFFER_CACHE_SIZE -1  do
        g_pDSoundBufferCache[v] := 0;

    // clear sound stream cache
    for v:=0 to SOUNDSTREAM_CACHE_SIZE -1 do
        g_pDSoundStreamCache[v] := 0; *)

    Initialized := True;
  end;

  g_pDSound8RefCount := 1;

  EmuSwapFS(fsXbox);   // XBox FS

  Result := hRet;
end;

function XTL_EmuIDirectSound8_AddRef(pThis: LPDIRECTSOUND8): ULONG; stdcall;
// Branch:martin  Revision:80  Translator:Shadow_Tj  Done:100
var
  uRet: ULONG;
begin
  EmuSwapFS( fsWindows );   // Win2k/XP FS

  DbgPrintf('EmuDSound : EmuIDirectSound8_AddRef' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10');',
         [pThis]);

  uRet := g_pDSound8RefCount;
  Inc(g_pDSound8RefCount);

  EmuSwapFS( fsXbox );   // XBox FS

  Result := uRet;
 end;

// ******************************************************************
// * func: EmuIDirectSound8_Release
// ******************************************************************
function XTL_EmuIDirectSound8_Release(
    pThis: LPDIRECTSOUND8): ULONG; stdcall;
// Branch:martin  Revision:80  Translator:Shadow_Tj  Done:100
var
  uRet: ULONG;
begin
    EmuSwapFS( fsWindows );   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSound8_Release' +
           #13#10'(' +
           #13#10'   pThis                     : 0x%.08X' +
           #13#10');',
           [pThis]);

    uRet := g_pDSound8RefCount;
    inc(g_pDSound8RefCount);

    (* temporarily (?) disabled by cxbx
    if(uRet = 1) then
        pThis.Release();
    //*)

    EmuSwapFS( fsXbox );   // XBox FS

    Result := uRet;
 end;

(*HRESULT WINAPI XTL.EmuCDirectSound_GetSpeakerConfig
(
    X_CDirectSound         *pThis,
    PDWORD                  pdwSpeakerConfig
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuCDirectSound_GetSpeakerConfig'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10'   pdwSpeakerConfig          : 0x%.08X'
           #13#10');',
           [pThis, pdwSpeakerConfig);

    *pdwSpeakerConfig := 0; // STEREO

    EmuSwapFS();   // XBox FS

    Result := S_OK;
 end;      *)

(*HRESULT WINAPI XTL.EmuIDirectSound8_DownloadEffectsImage
(
    LPDIRECTSOUND8          pThis,
    LPCVOID                 pvImageBuffer,
    DWORD                   dwImageSize,
    PVOID                   pImageLoc,      // TODO: Use this param
    PVOID                  *ppImageDesc     // TODO: Use this param
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSound8_DownloadEffectsImage'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10'   pvImageBuffer             : 0x%.08X'
           #13#10'   dwImageSize               : 0x%.08X'
           #13#10'   pImageLoc                 : 0x%.08X'
           #13#10'   ppImageDesc               : 0x%.08X'
           #13#10');',
           [pThis, pvImageBuffer, dwImageSize, pImageLoc, ppImageDesc);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    Result := S_OK;
 end;          *)

procedure XTL_EmuDirectSoundDoWork(); stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    EmuSwapFS( fsWindows );   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuDirectSoundDoWork();');

    HackUpdateSoundBuffers();
    HackUpdateSoundStreams();

    EmuSwapFS( fsXbox );   // XBox FS

    Exit;
 end;

function XTL_EmuIDirectSound8_SetOrientation
(
    pThis: LPDIRECTSOUND8;
    xFront: FLOAT;
    yFront: FLOAT;
    zFront: FLOAT;
    xTop: FLOAT;
    yTop: FLOAT;
    zTop: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    EmuSwapFS( fsWindows );   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSound8_SetOrientation' +
           #13#10'(' +
           #13#10'   pThis                     : 0x%.08X' +
           #13#10'   xFront                    :  % f' +
           #13#10'   yFront                    :  % f' +
           #13#10'   zFront                    :  % f' +
           #13#10'   xTop                      :  % f' +
           #13#10'   yTop                      :  % f' +
           #13#10'   zTop                      :  % f' +
           #13#10'   dwApply                   : 0x%.08X' +
           #13#10');',
           [pThis, xFront, yFront, zFront, xTop, yTop, zTop, dwApply]);

    // TODO: Actually implement this

    EmuSwapFS( fsXbox );   // XBox FS

    Result := S_OK;
 end;

function XTL_EmuIDirectSound8_SetDistanceFactor
(
    pThis: LPDIRECTSOUND8;
    fDistanceFactor: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    EmuSwapFS( fsWindows );   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSound8_SetDistanceFactor' +
           #13#10'(' +
           #13#10'   pThis                     : 0x%.08X' +
           #13#10'   fDistanceFactor           :  % f' +
           #13#10'   dwApply                   : 0x%.08X' +
           #13#10');',
           [pThis, fDistanceFactor, dwApply]);

    // TODO: Actually implement this

    EmuSwapFS( fsXbox );   // XBox FS

    Result := S_OK;
 end;

function XTL_EmuIDirectSound8_SetRolloffFactor
(
    pThis: LPDIRECTSOUND8;
    fRolloffFactor: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    EmuSwapFS( fsWindows );   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSound8_SetRolloffFactor' +
           #13#10'(' +
           #13#10'   pThis                     : 0x%.08X' +
           #13#10'   fRolloffFactor            :  % f' +
           #13#10'   dwApply                   : 0x%.08X' +
           #13#10');',
           [pThis, fRolloffFactor, dwApply]);

    // TODO: Actually implement this

    EmuSwapFS( fsWindows );   // XBox FS

    Result := S_OK;
 end;

function XTL_EmuIDirectSound8_SetDopplerFactor
(
    pThis: LPDIRECTSOUND8;
    fDopplerFactor: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    EmuSwapFS( fsWindows );   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSound8_SetDopplerFactor' +
           #13#10'(' +
           #13#10'   pThis                     : 0x%.08X' +
           #13#10'   fDopplerFactor            :  % f' +
           #13#10'   dwApply                   : 0x%.08X' +
           #13#10');',
           [pThis, fDopplerFactor, dwApply]);

    // TODO: Actually implement this

    EmuSwapFS( fsXbox );   // XBox FS

    Result := S_OK;
 end;

function XTL_EmuIDirectSound8_SetI3DL2Listener
(
    pThis: LPDIRECTSOUND8;
    pDummy: PVOID; // TODO: fill this out
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    // debug trace
   {$IFDEF _DEBUG_TRACE}

    begin
        EmuSwapFS( fsWindows );   // Win2k/XP FS
        DbgPrintf('EmuDSound : EmuIDirectSound8_SetI3DL2Listener' +
               #13#10'(' +
               #13#10'   pThis                     : 0x%.08X' +
               #13#10'   pDummy                    : 0x%.08X' +
               #13#10'   dwApply                   : 0x%.08X' +
               #13#10');',
               [pThis, pDummy, dwApply]);
        EmuSwapFS( fsXbox );   // XBox FS
     end;
    {$ENDIF}

    // TODO: Actually do something

    Result := DS_OK;
 end;

function XTL_EmuIDirectSound8_SetMixBinHeadroom
(
    pThis: LPDIRECTSOUND8;
    dwMixBinMask: DWORD;
    dwHeadroom: DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    // debug trace
    {$IFDEF _DEBUG_TRACE}
    begin
        EmuSwapFS( fsWindows );   // Win2k/XP FS
        DbgPrintf('EmuDSound : EmuIDirectSound8_SetMixBinHeadroom' +
               #13#10'(' +
               #13#10'   pThis                     : 0x%.08X' +
               #13#10'   dwMixBinMask              : 0x%.08X' +
               #13#10'   dwHeadroom                : 0x%.08X' +
               #13#10');',
               [pThis, dwMixBinMask, dwHeadroom]);
        EmuSwapFS( fsXbox );   // XBox FS
     end;
    {$ENDIF}

    // TODO: Actually do something

    Result := DS_OK;
 end;

function XTL_EmuIDirectSoundBuffer8_SetMixBins
(
    pThis: LPDIRECTSOUND8;
    pMixBins: PVOID
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    // debug trace
    {$IFDEF _DEBUG_TRACE}
    begin
        EmuSwapFS( fsWindows );   // Win2k/XP FS
        DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetMixBins' +
               #13#10'(' +
               #13#10'   pThis                     : 0x%.08X' +
               #13#10'   pMixBins                  : 0x%.08X' +
               #13#10');',
               [pThis, pMixBins]);
        EmuSwapFS( fsXbox );   // XBox FS
     end;
    {$ENDIF}

    // TODO: Actually do something

    Result := DS_OK;
 end;

function XTL_EmuIDirectSoundBuffer8_SetMixBinVolumes
(
    pThis: LPDIRECTSOUND8;
    pMixBins: PVOID
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    // debug trace
    {$IFDEF _DEBUG_TRACE}
    begin
        EmuSwapFS( fsWindows );   // Win2k/XP FS
        DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetMixBinVolumes' +
               #13#10'(' +
               #13#10'   pThis                     : 0x%.08X' +
               #13#10'   pMixBins                  : 0x%.08X' +
               #13#10');',
               [pThis, pMixBins]);
        EmuSwapFS( fsXbox );   // XBox FS
     end;
    {$ENDIF}

    // TODO: Actually do something

    Result := DS_OK;
 end;

function XTL_EmuIDirectSound8_SetPosition
(
    pThis : LPDIRECTSOUND8;
    x : FLOAT;
    y : FLOAT;
    z : FLOAT;
    dwApply : DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    // debug trace
    {$IFDEF _DEBUG_TRACE}
    begin
        EmuSwapFS( fsWindows );   // Win2k/XP FS
        DbgPrintf('EmuDSound : EmuIDirectSound8_SetPosition' +
               #13#10'(' +
               #13#10'   pThis                     : 0x%.08X' +
               #13#10'   x                         :  % f' +
               #13#10'   y                         :  % f' +
               #13#10'   z                         :  % f' +
               #13#10'   dwApply                   : 0x%.08X' +
               #13#10');',
               [pThis, x, y, z, dwApply]);
        EmuSwapFS( fsXbox );   // XBox FS
     end;
    {$ENDIF}

    // TODO: Actually do something

    Result := DS_OK;
 end;

function XTL_EmuIDirectSound8_SetVelocity
(
    pThis : LPDIRECTSOUND8;
    x : FLOAT;
    y : FLOAT;
    z : FLOAT;
    dwApply : DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    // debug trace
    {$IFDEF _DEBUG_TRACE}
    begin
        EmuSwapFS( fsWindows );   // Win2k/XP FS
        DbgPrintf('EmuDSound : EmuIDirectSound8_SetVelocity' +
               #13#10'(' +
               #13#10'   pThis                     : 0x%.08X' +
               #13#10'   x                         :  % f' +
               #13#10'   y                         :  % f' +
               #13#10'   z                         :  % f' +
               #13#10'   dwApply                   : 0x%.08X' +
               #13#10');',
               [pThis, x, y, z, dwApply]);
        EmuSwapFS( fsXbox );   // XBox FS
     end;
    {$ENDIF}

    // TODO: Actually do something

    Result := DS_OK;
 end;

function XTL_EmuIDirectSound8_SetAllParameters(
    pThis: LPDIRECTSOUND8;
    pTodo: Pointer;  // TODO: LPCDS3DLISTENER
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  // debug trace
{$IFDEF _DEBUG_TRACE}
  begin
    EmuSwapFS(fsWindows);
    DbgPrintf('EmuDSound : EmuIDirectSound8_SetAllParameters' +
        #13#10'(' +
        #13#10'   pThis                     : 0x%.08X' +
        #13#10'   pTodo                     : 0x%.08X' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [pThis, pTodo, dwApply]);

    EmuSwapFS(fsXbox);
  end;
{$ENDIF}

  // Cxbx TODO: Actually do something

  Result := DS_OK;
end;

(*HRESULT WINAPI XTL.EmuCDirectSound_CommitDeferredSettings
(
    X_CDirectSound         *pThis
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS();   // Win2k/XP FS

  DbgPrintf('EmuDSound : EmuCDirectSound_CommitDeferredSettings'
         #13#10'('
         #13#10'   pThis                     : $%.08X'
         #13#10');',
         [pThis);

  // TODO: Translate params, then make the PC DirectSound call

  EmuSwapFS();   // XBox FS

  Result := DS_OK;
 end;
 *)

(*HRESULT WINAPI XTL.EmuDirectSoundCreateBuffer
(
    X_DSBUFFERDESC         *pdsbd,
    X_CDirectSoundBuffer  **ppBuffer
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuDirectSoundCreateBuffer'
           #13#10'('
           #13#10'   pdsbd                     : $%.08X'
           #13#10'   ppBuffer                  : $%.08X'
           #13#10');',
           [pdsbd, ppBuffer);

    DWORD dwEmuFlags := 0;

    DSBUFFERDESC *pDSBufferDesc := (DSBUFFERDESC)CxbxMalloc(SizeOf(DSBUFFERDESC));

    // convert from Xbox to PC DSound
    begin
        DWORD dwAcceptableMask := $00000010 or $00000020 or $00000080 or $00000100 or $00002000 or $00040000 or $00080000;

        if(pdsbd.dwFlags and (~dwAcceptableMask)) then
            EmuWarning('Use of unsupported pdsbd.dwFlags mask(s) ($%.08X)', pdsbd.dwFlags and (~dwAcceptableMask));

        pDSBufferDesc.dwSize := SizeOf(DSBUFFERDESC);
        pDSBufferDesc.dwFlags := (pdsbd.dwFlags and dwAcceptableMask) or DSBCAPS_CTRLVOLUME or DSBCAPS_GETCURRENTPOSITION2;
        pDSBufferDesc.dwBufferBytes := pdsbd.dwBufferBytes;

        if(pDSBufferDesc.dwBufferBytes < DSBSIZE_MIN) then
            pDSBufferDesc.dwBufferBytes := DSBSIZE_MIN;
        else if(pDSBufferDesc.dwBufferBytes > DSBSIZE_MAX) then
            pDSBufferDesc.dwBufferBytes := DSBSIZE_MAX;

        pDSBufferDesc.dwReserved := 0;

        if(pdsbd.lpwfxFormat <> 0) then
        begin
            pDSBufferDesc.lpwfxFormat := (WAVEFORMATEX)CxbxMalloc(SizeOf(WAVEFORMATEX)+pdsbd.lpwfxFormat.cbSize);
            memcpy(pDSBufferDesc.lpwfxFormat, pdsbd.lpwfxFormat, SizeOf(WAVEFORMATEX));

            if(pDSBufferDesc.lpwfxFormat.wFormatTag = (*WAVE_FORMAT_XBOX_ADPCM*)(*0x0069) then
            begin
                dwEmuFlags:= dwEmuFlags or DSB_FLAG_ADPCM;

                EmuWarning('WAVE_FORMAT_XBOX_ADPCM Unsupported not ');

                pDSBufferDesc.lpwfxFormat.wFormatTag := WAVE_FORMAT_PCM;
                pDSBufferDesc.lpwfxFormat.nBlockAlign := (pDSBufferDesc.lpwfxFormat.nChannels*pDSBufferDesc.lpwfxFormat.wBitsPerSample)/8;

                // the above calculation can yield zero for wBitsPerSample < 8, so we'll bound it to 1 byte minimum
                if(pDSBufferDesc.lpwfxFormat.nBlockAlign = 0) then
                    pDSBufferDesc.lpwfxFormat.nBlockAlign := 1;

                pDSBufferDesc.lpwfxFormat.nAvgBytesPerSec := pDSBufferDesc.lpwfxFormat.nSamplesPerSec*pDSBufferDesc.lpwfxFormat.nBlockAlign;
                pDSBufferDesc.lpwfxFormat.wBitsPerSample := 8;

                (* TODO: Get ADPCM working!
                pDSBufferDesc.lpwfxFormat.cbSize := 32;
                const WAVE_FORMAT_ADPCM = 2;
                pDSBufferDesc.lpwfxFormat.wFormatTag := WAVE_FORMAT_ADPCM;
                *)
(*             end;
         end;

        pDSBufferDesc.guid3DAlgorithm := DS3DALG_DEFAULT;
     end;

    // sanity check
    if(pDSBufferDesc.lpwfxFormat.nBlockAlign <> (pDSBufferDesc.lpwfxFormat.nChannels*pDSBufferDesc.lpwfxFormat.wBitsPerSample)/8) then
    begin
        pDSBufferDesc.lpwfxFormat.nBlockAlign := (2*pDSBufferDesc.lpwfxFormat.wBitsPerSample)/8;
        pDSBufferDesc.lpwfxFormat.nAvgBytesPerSec := pDSBufferDesc.lpwfxFormat.nSamplesPerSec * pDSBufferDesc.lpwfxFormat.nBlockAlign;
     end;

    // TODO: Garbage Collection
    *ppBuffer := new X_CDirectSoundBuffer();

    (ppBuffer).EmuDirectSoundBuffer8 := 0;
    (ppBuffer).EmuBuffer := 0;
    (ppBuffer).EmuBufferDesc := pDSBufferDesc;
    (ppBuffer).EmuLockPtr1 := 0;
    (ppBuffer).EmuLockBytes1 := 0;
    (ppBuffer).EmuLockPtr2 := 0;
    (ppBuffer).EmuLockBytes2 := 0;
    (ppBuffer).EmuFlags := dwEmuFlags;

    DbgPrintf('EmuDSound : EmuDirectSoundCreateBuffer, *ppBuffer := $%.08X, bytes := $%.08X', [*ppBuffer, pDSBufferDesc.dwBufferBytes);

    HRESULT hRet := g_pDSound8.CreateSoundBuffer(pDSBufferDesc,  and ((ppBuffer).EmuDirectSoundBuffer8), 0);

    if(FAILED(hRet)) then
        EmuWarning('CreateSoundBuffer Failed not ');

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
            CxbxKrnlCleanup('SoundBuffer cache out of slots not ');
     end;

    EmuSwapFS();   // XBox FS

    Result := hRet;
 end;      *)

(*HRESULT WINAPI XTL.EmuIDirectSound8_CreateBuffer
(
    LPDIRECTSOUND8          pThis,
    X_DSBUFFERDESC         *pdssd,
    X_CDirectSoundBuffer  **ppBuffer,
    PVOID                   pUnknown
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
       EmuSwapFS();   // Win2k/XP FS
       DbgPrintf('EmuDSound : EmuIDirectSound8_CreateBuffer'
               #13#10'('
               #13#10'   pThis                     : $%.08X'
               #13#10'   pdssd                     : $%.08X'
               #13#10'   ppBuffer                  : $%.08X'
               #13#10'   pUnknown                  : $%.08X'
               #13#10');',
               [pThis, pdssd, ppBuffer, pUnknown);
       EmuSwapFS();   // XBox FS
     end;
    //endif

    EmuDirectSoundCreateBuffer(pdssd, ppBuffer);

    Result := DS_OK;
 end;         *)

(*HRESULT WINAPI XTL.EmuIDirectSound8_CreateSoundBuffer
(
    LPDIRECTSOUND8          pThis,
    X_DSBUFFERDESC         *pdsbd,
    X_CDirectSoundBuffer  **ppBuffer,
    LPUNKNOWN               pUnkOuter
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    // debug trace
    #ifdef _DEBUG_TRACE
    begin
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf('EmuDSound : EmuIDirectSound8_CreateSoundBuffer'
               #13#10'('
               #13#10'   pdsbd                     : $%.08X'
               #13#10'   ppBuffer                  : $%.08X'
               #13#10'   pUnkOuter                 : $%.08X'
               #13#10');',
               [pdsbd, ppBuffer, pUnkOuter);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    Result := EmuDirectSoundCreateBuffer(pdsbd, ppBuffer);
 end;       *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetBufferData
(
    X_CDirectSoundBuffer   *pThis,
    Pointer                  pvBufferData,
    DWORD                   dwBufferBytes
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetBufferData'
           #13#10'('
           #13#10'   pThis                     : $%.08X'
           #13#10'   pvBufferData              : $%.08X'
           #13#10'   dwBufferBytes             : $%.08X'
           #13#10');',
           [pThis, pvBufferData, dwBufferBytes);

    // update buffer data cache
    pThis.EmuBuffer := pvBufferData;

    EmuResizeIDirectSoundBuffer8(pThis, dwBufferBytes);

    EmuSwapFS();   // XBox FS

    Result := S_OK;
 end;           *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetPlayRegion
(
    X_CDirectSoundBuffer   *pThis,
    DWORD                   dwPlayStart,
    DWORD                   dwPlayLength
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetPlayRegion'
           #13#10'('
           #13#10'   pThis                     : $%.08X'
           #13#10'   dwPlayStart               : $%.08X'
           #13#10'   dwPlayLength              : $%.08X'
           #13#10');',
           [pThis, dwPlayStart, dwPlayLength);

    // TODO: Translate params, then make the PC DirectSound call

    // TODO: Ensure that 4627 & 4361 are intercepting far enough back
    // (otherwise pThis is manipulated!)

    EmuSwapFS();   // XBox FS

    Result := DS_OK;
 end;            *)

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
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_Lock'
           #13#10'('
           #13#10'   pThis                     : $%.08X'
           #13#10'   dwOffset                  : $%.08X'
           #13#10'   dwBytes                   : $%.08X'
           #13#10'   ppvAudioPtr1              : $%.08X'
           #13#10'   pdwAudioBytes1            : $%.08X'
           #13#10'   ppvAudioPtr2              : $%.08X'
           #13#10'   pdwAudioBytes2            : $%.08X'
           #13#10'   dwFlags                   : $%.08X'
           #13#10');',
           [pThis, dwOffset, dwBytes, ppvAudioPtr1, pdwAudioBytes1,
           ppvAudioPtr2, pdwAudioBytes2, dwFlags);

    HRESULT hRet := D3D_OK;

    if(pThis.EmuBuffer <> 0) then
    begin
        *ppvAudioPtr1 := pThis.EmuBuffer;
        *pdwAudioBytes1 := dwBytes;
     end;
    else
    begin
        if(dwBytes > pThis.EmuBufferDesc.dwBufferBytes) then
            EmuResizeIDirectSoundBuffer8(pThis, dwBytes);

        if(pThis.EmuLockPtr1 <> 0) then
            pThis.EmuDirectSoundBuffer8.Unlock(pThis.EmuLockPtr1, pThis.EmuLockBytes1, pThis.EmuLockPtr2, pThis.EmuLockBytes2);

        // TODO: Verify dwFlags is the same as windows
        hRet := pThis.EmuDirectSoundBuffer8.Lock(dwOffset, dwBytes, ppvAudioPtr1, pdwAudioBytes1, ppvAudioPtr2, pdwAudioBytes2, dwFlags);

        if(FAILED(hRet)) then
            CxbxKrnlCleanup('DirectSoundBuffer Lock Failed not ');

        pThis.EmuLockPtr1 := *ppvAudioPtr1;
        pThis.EmuLockBytes1 := *pdwAudioBytes1;
        pThis.EmuLockPtr2 := (ppvAudioPtr2 <> 0) ? *ppvAudioPtr2 : 0;
        pThis.EmuLockBytes2 := (pdwAudioBytes2 <> 0) ? *pdwAudioBytes2 : 0;
     end;

    EmuSwapFS();   // XBox FS

    Result := hRet;
 end;         *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetHeadroom
(
    X_CDirectSoundBuffer  *pThis,
    DWORD                  dwHeadroom
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetHeadroom'
           #13#10'('
           #13#10'   pThis                     : $%.08X'
           #13#10'   dwHeadroom                : $%.08X'
           #13#10');',
           [pThis, dwHeadroom);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    Result := S_OK;
 end;         *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetLoopRegion
(
    X_CDirectSoundBuffer   *pThis,
    DWORD                   dwLoopStart,
    DWORD                   dwLoopLength
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetLoopRegion'
           #13#10'('
           #13#10'   pThis                     : $%.08X'
           #13#10'   dwLoopStart               : $%.08X'
           #13#10'   dwLoopLength              : $%.08X'
           #13#10');',
           [pThis, dwLoopStart, dwLoopLength);

    // TODO: Ensure that 4627 & 4361 are intercepting far enough back
    // (otherwise pThis is manipulated!)

    //EmuResizeIDirectSoundBuffer8(pThis, dwLoopLength);

    EmuSwapFS();   // XBox FS

    Result := DS_OK;
 end;      *)

(*ULONG WINAPI XTL.EmuIDirectSoundBuffer8_Release
(
    X_CDirectSoundBuffer   *pThis
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_Release'
           #13#10'('
           #13#10'   pThis                     : $%.08X'
           #13#10');',
           [pThis);

    ULONG uRet := 0;

    if(pThis <> 0) then
    begin
        uRet := pThis.EmuDirectSoundBuffer8.Release();

        if(uRet = 0) then
        begin
            // remove cache entry
            for(integer v:=0;v<SOUNDBUFFER_CACHE_SIZE;v++)
            begin
                if(g_pDSoundBufferCache[v] = pThis) then
                    g_pDSoundBufferCache[v] := 0;
             end;

            if(pThis.EmuBufferDesc.lpwfxFormat <> 0) then
                CxbxFree(pThis.EmuBufferDesc.lpwfxFormat);

            CxbxFree(pThis.EmuBufferDesc);

            delete pThis;
         end;
     end;

    EmuSwapFS();   // XBox FS

    Result := uRet;
 end;            *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetPitch
(
    X_CDirectSoundBuffer   *pThis,
    LongInt                    lPitch
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetPitch'
           #13#10'('
           #13#10'   pThis                     : $%.08X'
           #13#10'   lPitch                    : $%.08X'
           #13#10');',
           [pThis, lPitch);

    // TODO: Translate params, then make the PC DirectSound call

    EmuSwapFS();   // XBox FS

    Result := DS_OK;
 end;                *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_GetStatus
(
    X_CDirectSoundBuffer   *pThis,
    LPDWORD                 pdwStatus
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_GetStatus'
           #13#10'('
           #13#10'   pThis                     : $%.08X'
           #13#10'   pdwStatus                 : $%.08X'
           #13#10');',
           [pThis, pdwStatus);

    HRESULT hRet := DS_OK;

    if(pThis <> 0 and pThis.EmuBuffer = 0) then
    begin
        hRet := pThis.EmuDirectSoundBuffer8.GetStatus(pdwStatus);
     end;
    else
    begin
        *pdwStatus := 0;
     end;

    EmuSwapFS();   // XBox FS

    Result := hRet;
 end;           *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetCurrentPosition
(
    X_CDirectSoundBuffer   *pThis,
    DWORD                   dwNewPosition
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetCurrentPosition'
           #13#10'('
           #13#10'   pThis                     : $%.08X'
           #13#10'   dwNewPosition             : $%.08X'
           #13#10');',
           [pThis, dwNewPosition);

    // NOTE: TODO: This call *will* (by MSDN) fail on primary buffers!
    HRESULT hRet := pThis.EmuDirectSoundBuffer8.SetCurrentPosition(dwNewPosition);

    if(FAILED(hRet)) then
        EmuWarning('SetCurrentPosition Failed not ');

    EmuSwapFS();   // XBox FS

    Result := hRet;
 end;    *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_GetCurrentPosition
(
    X_CDirectSoundBuffer   *pThis,
    PDWORD                  pdwCurrentPlayCursor,
    PDWORD                  pdwCurrentWriteCursor
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_GetCurrentPosition'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10'   pdwCurrentPlayCursor      : 0x%.08X'
           #13#10'   pdwCurrentWriteCursor     : 0x%.08X'
           #13#10');',
           [pThis, pdwCurrentPlayCursor, pdwCurrentWriteCursor);

    HackUpdateSoundBuffers();
    HackUpdateSoundStreams();

    // NOTE: TODO: This call always seems to fail on primary buffers!
    HRESULT hRet := pThis.EmuDirectSoundBuffer8.GetCurrentPosition(pdwCurrentPlayCursor, pdwCurrentWriteCursor);

    if(FAILED(hRet)) then
        EmuWarning('GetCurrentPosition Failed not ');

    if(pdwCurrentPlayCursor <> 0 and pdwCurrentWriteCursor <> 0) then
    begin
        DbgPrintf('*pdwCurrentPlayCursor := %d, *pdwCurrentWriteCursor := %d', [*pdwCurrentPlayCursor, *pdwCurrentWriteCursor);
     end;

    EmuSwapFS();   // XBox FS

    Result := hRet;
 end;         *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_Play
(
    X_CDirectSoundBuffer   *pThis,
    DWORD                   dwReserved1,
    DWORD                   dwReserved2,
    DWORD                   dwFlags
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_Play'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10'   dwReserved1               : 0x%.08X'
           #13#10'   dwReserved2               : 0x%.08X'
           #13#10'   dwFlags                   : 0x%.08X'
           #13#10');',
           [pThis, dwReserved1, dwReserved2, dwFlags);

    if(dwFlags and (~DSBPLAY_LOOPING)) then
        CxbxKrnlCleanup('Unsupported Playing Flags');

    HackUpdateSoundBuffers();

    // close any existing locks
    if(pThis.EmuLockPtr1 <> 0) then
    begin
        pThis.EmuDirectSoundBuffer8.Unlock
        (
            pThis.EmuLockPtr1,
            pThis.EmuLockBytes1,
            pThis.EmuLockPtr2,
            pThis.EmuLockBytes2
        );

        pThis.EmuLockPtr1 := 0;
     end;

    HRESULT hRet;

    if(pThis.EmuFlags and DSB_FLAG_ADPCM) then
    begin
        hRet := D3D_OK;
     end;
    else
    begin
        hRet := pThis.EmuDirectSoundBuffer8.Play(0, 0, dwFlags);
     end;

    pThis.EmuPlayFlags := dwFlags;

    EmuSwapFS();   // XBox FS

    Result := hRet;
 end;            *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_Stop
(
    X_CDirectSoundBuffer   *pThis
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_Stop'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10');',
           [pThis);

    HRESULT hRet := pThis.EmuDirectSoundBuffer8.Stop();

    EmuSwapFS();   // XBox FS

    Result := hRet;
 end;                *)

(* 'C' HRESULT __stdcall XTL.EmuIDirectSoundBuffer8_StopEx
(
    X_CDirectSoundBuffer *pBuffer,
    REFERENCE_TIME        rtTimeStamp,
    DWORD                 dwFlags
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_StopEx'
           #13#10'('
           #13#10'   pBuffer                   : 0x%.08X'
           #13#10'   rtTimeStamp               : 0x%.08X'
           #13#10'   dwFlags                   : 0x%.08X'
           #13#10');',
           [pBuffer, rtTimeStamp, dwFlags);

    if(pBuffer.EmuDirectSoundBuffer8 = 0) then
        EmuWarning('pBuffer.EmuDirectSoundBuffer8 := 0');

    EmuWarning('StopEx not yet implemented not ');

    EmuSwapFS();   // XBox FS

    Result := S_OK;
 end;           *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetVolume
(
    X_CDirectSoundBuffer   *pThis,
    LongInt                    lVolume
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetVolume'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10'   lVolume                   : 0x%.08X'
           #13#10');',
           [pThis, lVolume);

    // TODO: Ensure that 4627 & 4361 are intercepting far enough back
    // (otherwise pThis is manipulated!)

//    HRESULT hRet = pThis->EmuDirectSoundBuffer8->SetVolume(lVolume);

    EmuSwapFS();   // XBox FS

//    return hRet;
    Result := S_OK;
 end;   *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetFrequency
(
    X_CDirectSoundBuffer   *pThis,
    DWORD                   dwFrequency
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetFrequency'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10'   dwFrequency               : 0x%.08X'
           #13#10');',
           [pThis, dwFrequency);

//    HRESULT hRet = pThis->EmuDirectSoundBuffer8->SetFrequency(dwFrequency);

    EmuSwapFS();   // XBox FS

//    return hRet;
    Result := S_OK;
 end;    *)

(*HRESULT WINAPI XTL.EmuDirectSoundCreateStream
(
    X_DSSTREAMDESC         *pdssd,
    X_CDirectSoundStream  **ppStream
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuDirectSoundCreateStream'
           #13#10'('
           #13#10'   pdssd                     : 0x%.08X'
           #13#10'   ppStream                  : 0x%.08X'
           #13#10');',
           [pdssd, ppStream);

    // TODO: Garbage Collection
    *ppStream := new X_CDirectSoundStream();

    DSBUFFERDESC *pDSBufferDesc := (DSBUFFERDESC)CxbxMalloc(SizeOf(DSBUFFERDESC));

    // convert from Xbox to PC DSound
    begin
        DWORD dwAcceptableMask := 0x00000010; // TODO: Note 0x00040000 is being ignored (DSSTREAMCAPS_LOCDEFER)

        if(pdssd.dwFlags and (~dwAcceptableMask)) then
            EmuWarning('Use of unsupported pdssd.dwFlags mask(s) (0x%.08X)', pdssd.dwFlags and (~dwAcceptableMask));

        pDSBufferDesc.dwSize := SizeOf(DSBUFFERDESC);
//        pDSBufferDesc->dwFlags = (pdssd->dwFlags & dwAcceptableMask) | DSBCAPS_CTRLVOLUME | DSBCAPS_GETCURRENTPOSITION2;
        pDSBufferDesc.dwFlags := DSBCAPS_CTRLVOLUME;
        pDSBufferDesc.dwBufferBytes := DSBSIZE_MIN;

        pDSBufferDesc.dwReserved := 0;

        if(pdssd.lpwfxFormat <> 0) then
        begin
            pDSBufferDesc.lpwfxFormat := (WAVEFORMATEX)CxbxMalloc(SizeOf(WAVEFORMATEX));
            memcpy(pDSBufferDesc.lpwfxFormat, pdssd.lpwfxFormat, SizeOf(WAVEFORMATEX));
         end;

        pDSBufferDesc.guid3DAlgorithm := DS3DALG_DEFAULT;

        if(pDSBufferDesc.lpwfxFormat <> 0 and pDSBufferDesc.lpwfxFormat.wFormatTag <> WAVE_FORMAT_PCM) then
        begin
            EmuWarning('Invalid WAVE_FORMAT not ');
			if(pDSBufferDesc.lpwfxFormat.wFormatTag = (*WAVE_FORMAT_XBOX_ADPCM*)(*0x0069) then
				EmuWarning('WAVE_FORMAT_XBOX_ADPCM Unsupported not ');

            (ppStream).EmuDirectSoundBuffer8 := 0;

            EmuSwapFS();   // XBox FS

            Result := DS_OK;
         end;

        // we only support 2 channels right now
        if(pDSBufferDesc.lpwfxFormat.nChannels > 2) then
        begin
            pDSBufferDesc.lpwfxFormat.nChannels := 2;
            pDSBufferDesc.lpwfxFormat.nBlockAlign := (2*pDSBufferDesc.lpwfxFormat.wBitsPerSample)/8;
            pDSBufferDesc.lpwfxFormat.nAvgBytesPerSec := pDSBufferDesc.lpwfxFormat.nSamplesPerSec * pDSBufferDesc.lpwfxFormat.nBlockAlign;
         end;
     end;

    (ppStream).EmuBuffer := 0;
    (ppStream).EmuBufferDesc := pDSBufferDesc;
    (ppStream).EmuLockPtr1 := 0;
    (ppStream).EmuLockBytes1 := 0;
    (ppStream).EmuLockPtr2 := 0;
    (ppStream).EmuLockBytes2 := 0;

    DbgPrintf('EmuDSound : EmuDirectSoundCreateStream, *ppStream := 0x%.08X', [*ppStream);

    HRESULT hRet := g_pDSound8.CreateSoundBuffer(pDSBufferDesc,  and (ppStream).EmuDirectSoundBuffer8, 0);

    if(FAILED(hRet)) then
        EmuWarning('CreateSoundBuffer Failed not ');

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
            CxbxKrnlCleanup('SoundStream cache out of slots not ');
     end;

    EmuSwapFS();   // XBox FS

    Result := DS_OK;
 end;       *)

(*HRESULT WINAPI XTL.EmuIDirectSound8_CreateStream
(
    LPDIRECTSOUND8          pThis,
    X_DSSTREAMDESC         *pdssd,
    X_CDirectSoundStream  **ppStream,
    PVOID                   pUnknown
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    // debug trace
    #ifdef _DEBUG_TRACE
    begin
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf('EmuDSound : EmuIDirectSound8_CreateStream'
               #13#10'('
               #13#10'   pThis                     : 0x%.08X'
               #13#10'   pdssd                     : 0x%.08X'
               #13#10'   ppStream                  : 0x%.08X'
               #13#10'   pUnknown                  : 0x%.08X'
               #13#10');',
               [pThis, pdssd, ppStream, pUnknown);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    EmuDirectSoundCreateStream(pdssd, ppStream);

    Result := DS_OK;
 end;     *)

(*
VOID WINAPI XTL.EmuCMcpxStream_Dummy_0x10(DWORD dwDummy1, DWORD dwDummy2)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuWarning('EmuCMcpxStream_Dummy_0x10 is ignored not ');
    Exit;
end;
*)

(*ULONG WINAPI XTL.EmuCDirectSoundStream_SetVolume(X_CDirectSoundStream *pThis, LongInt lVolume)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetVolume'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10'   lVolume                   : %d'
           #13#10');',
           [pThis, lVolume);

    // TODO: Actually SetVolume

    EmuSwapFS();   // XBox FS

    Result := DS_OK;
 end;       *)

(*HRESULT WINAPI XTL.EmuCDirectSoundStream_SetRolloffFactor
(
    X_CDirectSoundStream *pThis,
    FLOAT                 fRolloffFactor,
    DWORD                 dwApply
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetRolloffFactor'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10'   fRolloffFactor            : %f'
           #13#10'   dwApply                   : 0x%.08X'
           #13#10');',
           [pThis, fRolloffFactor, dwApply);

    // TODO: Actually SetRolloffFactor

    EmuSwapFS();   // XBox FS

    Result := DS_OK;
 end;       *)

(*ULONG WINAPI XTL.EmuCDirectSoundStream_AddRef(X_CDirectSoundStream *pThis)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuCDirectSoundStream_AddRef'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10');',
           [pThis);

    if(pThis <> 0) then
        pThis.EmuDirectSoundBuffer8.AddRef();

    EmuSwapFS();   // XBox FS

    Result := DS_OK;
 end;         *)

(*ULONG WINAPI XTL.EmuCDirectSoundStream_Release(X_CDirectSoundStream *pThis)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuCDirectSoundStream_Release'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10');',
           [pThis);

    ULONG uRet := 0;

    if(pThis <> 0 and (pThis.EmuDirectSoundBuffer8 <> 0)) then
    begin
        uRet := pThis.EmuDirectSoundBuffer8.Release();

        if(uRet = 0) then
        begin
            // remove cache entry
            for(integer v:=0;v<SOUNDSTREAM_CACHE_SIZE;v++)
            begin
                if(g_pDSoundStreamCache[v] = pThis) then
                    g_pDSoundStreamCache[v] := 0;
             end;

            if(pThis.EmuBufferDesc.lpwfxFormat <> 0) then
                CxbxFree(pThis.EmuBufferDesc.lpwfxFormat);

            CxbxFree(pThis.EmuBufferDesc);

            delete pThis;
         end;
     end;

    EmuSwapFS();   // XBox FS

    Result := uRet;
 end;      *)

(*HRESULT WINAPI XTL.EmuCDirectSoundStream_GetStatus
(
    X_CDirectSoundStream   *pThis,
    DWORD                  *pdwStatus
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuCDirectSoundStream_GetStatus'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10'   pdwStatus                 : 0x%.08X'
           #13#10');',
           [pThis, pdwStatus);

    EmuWarning('EmuCDirectSoundStream_GetStatus is not yet implemented');

    *pdwStatus := DSBSTATUS_PLAYING;

    EmuSwapFS();   // XBox FS

    Result := DS_OK;
 end;       *)

(*HRESULT WINAPI XTL.EmuCDirectSoundStream_Process
(
    X_CDirectSoundStream   *pThis,
    PXMEDIAPACKET           pInputBuffer,
    PXMEDIAPACKET           pOutputBuffer
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuCDirectSoundStream_Process'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10'   pInputBuffer              : 0x%.08X'
           #13#10'   pOutputBuffer             : 0x%.08X'
           #13#10');',
           [pThis, pInputBuffer, pOutputBuffer);

    if(pThis.EmuDirectSoundBuffer8 <> 0) then
    begin
        // update buffer data cache
        pThis.EmuBuffer := pInputBuffer.pvBuffer;

        EmuResizeIDirectSoundStream8(pThis, pInputBuffer.dwMaxSize);

        if(pInputBuffer.pdwStatus <> 0) then
            *pInputBuffer.pdwStatus := S_OK;

        HackUpdateSoundStreams();
     end;
    else
    begin
        if(pInputBuffer.pdwStatus <> 0) then
            *pInputBuffer.pdwStatus := S_OK;
     end;

    EmuSwapFS();   // XBox FS

    Result := DS_OK;
 end;         *)

(*HRESULT WINAPI XTL.EmuCDirectSoundStream_Discontinuity(X_CDirectSoundStream *pThis)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuCDirectSoundStream_Discontinuity'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10');',
           [pThis);

    // TODO: Actually Process

    EmuSwapFS();   // XBox FS

    Result := DS_OK;
 end;          *)

(*HRESULT WINAPI XTL.EmuCDirectSoundStream_Flush(X_CDirectSoundStream *pThis)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuCDirectSoundStream_Flush();',
           [pThis);

    // TODO: Actually Flush

    EmuSwapFS();   // XBox FS

    Result := DS_OK;
 end;          *)

(*HRESULT WINAPI XTL.EmuCDirectSound_SynchPlayback(PVOID pUnknown)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuCDirectSound_SynchPlayback (0x%.08X);', [pUnknown]);

    EmuSwapFS();   // XBox FS

    Result := DS_OK;
 end;           *)

(*HRESULT WINAPI XTL.EmuCDirectSoundStream_Pause
(
    PVOID   pStream,
    DWORD   dwPause
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuCDirectSoundStream_Pause'
           #13#10'('
           #13#10'   pStream                   : 0x%.08X'
           #13#10'   dwPause                   : 0x%.08X'
           #13#10');',
           [pStream, dwPause);

    EmuSwapFS();   // XBox FS

    Result := DS_OK;
 end;        *)

(*HRESULT WINAPI XTL.EmuIDirectSoundStream_SetHeadroom
(
    PVOID   pThis,
    DWORD   dwHeadroom
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSoundStream_SetHeadroom'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10'   dwHeadroom                : 0x%.08X'
           #13#10');',
           [pThis, dwHeadroom);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    Result := S_OK;
 end;      *)

(*HRESULT WINAPI XTL.EmuCDirectSoundStream_SetConeAngles
(
    PVOID   pThis,
    DWORD   dwInsideConeAngle,
    DWORD   dwOutsideConeAngle,
    DWORD   dwApply
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetConeAngles'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10'   dwInsideConeAngle         : 0x%.08X'
           #13#10'   dwOutsideConeAngle        : 0x%.08X'
           #13#10'   dwApply                   : 0x%.08X'
           #13#10');',
           [pThis, dwInsideConeAngle, dwOutsideConeAngle, dwApply);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    Result := S_OK;
 end;              *)

(*HRESULT WINAPI XTL.EmuCDirectSoundStream_SetConeOutsideVolume
(
    PVOID   pThis,
    LongInt    lConeOutsideVolume,
    DWORD   dwApply
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetConeOutsideVolume'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10'   lConeOutsideVolume        : %d'
           #13#10'   dwApply                   : 0x%.08X'
           #13#10');',
           [pThis, lConeOutsideVolume, dwApply);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    Result := S_OK;
 end;     *)

(*HRESULT WINAPI XTL.EmuCDirectSoundStream_SetAllParameters
(
    PVOID    pThis,
    PVOID    pUnknown,
    DWORD    dwApply
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetAllParameters'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10'   pUnknown                  : %f'
           #13#10'   dwApply                   : 0x%.08X'
           #13#10');',
           [pThis, pUnknown, dwApply);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    Result := S_OK;
 end;      *)

(*HRESULT WINAPI XTL.EmuCDirectSoundStream_SetMaxDistance
(
    PVOID    pThis,
    D3DVALUE fMaxDistance,
    DWORD    dwApply
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetMaxDistance'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10'   fMaxDistance              : %f'
           #13#10'   dwApply                   : 0x%.08X'
           #13#10');',
           [pThis, fMaxDistance, dwApply);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    Result := S_OK;
 end;          *)

(*HRESULT WINAPI XTL.EmuCDirectSoundStream_SetMinDistance
(
    PVOID    pThis,
    D3DVALUE fMinDistance,
    DWORD    dwApply
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetMinDistance'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10'   fMinDistance              : %f'
           #13#10'   dwApply                   : 0x%.08X'
           #13#10');',
           [pThis, fMinDistance, dwApply);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    Result := S_OK;
 end;        *)

(*HRESULT WINAPI XTL.EmuCDirectSoundStream_SetVelocity
(
    PVOID    pThis,
    D3DVALUE x,
    D3DVALUE y,
    D3DVALUE z,
    DWORD    dwApply
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetVelocity'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10'   x                         : %f'
           #13#10'   y                         : %f'
           #13#10'   z                         : %f'
           #13#10'   dwApply                   : 0x%.08X'
           #13#10');',
           [pThis, x, y, z, dwApply);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    Result := S_OK;
 end;       *)

(*HRESULT WINAPI XTL.EmuCDirectSoundStream_SetConeOrientation
(
    PVOID    pThis,
    D3DVALUE x,
    D3DVALUE y,
    D3DVALUE z,
    DWORD    dwApply
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetConeOrientation'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10'   x                         : %f'
           #13#10'   y                         : %f'
           #13#10'   z                         : %f'
           #13#10'   dwApply                   : 0x%.08X'
           #13#10');',
           [pThis, x, y, z, dwApply);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    Result := S_OK;
 end;       *)

(*HRESULT WINAPI XTL.EmuCDirectSoundStream_SetPosition
(
    PVOID    pThis,
    D3DVALUE x,
    D3DVALUE y,
    D3DVALUE z,
    DWORD    dwApply
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetPosition'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10'   x                         : %f'
           #13#10'   y                         : %f'
           #13#10'   z                         : %f'
           #13#10'   dwApply                   : 0x%.08X'
           #13#10');',
           [pThis, x, y, z, dwApply);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    Result := S_OK;
 end;        *)

(*HRESULT WINAPI XTL.EmuCDirectSoundStream_SetFrequency
(
    PVOID   pThis,
    DWORD   dwFrequency
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetFrequency'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10'   dwFrequency               : %d'
           #13#10');',
           [pThis, dwFrequency);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    Result := S_OK;
 end;             *)

(*HRESULT WINAPI XTL.EmuIDirectSoundStream_SetI3DL2Source
(
    PVOID   pThis,
    PVOID   pds3db,
    DWORD   dwApply
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSoundStream_SetI3DL2Source'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10'   pds3db                    : 0x%.08X'
           #13#10'   dwApply                   : 0x%.08X'
           #13#10');',
           [pThis, pds3db, dwApply);

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS

    Result := S_OK;
 end;      *)

function XTL_EmuIDirectSoundStream_Unknown1 (pThis : PVOID; dwUnknown1 : DWORD ): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:10
begin
  EmuSwapFS(fsWindows);   // Win2k/XP FS

  (*DbgPrintf('EmuDSound : EmuIDirectSoundStream_Unknown1' +
         #13#10'(' +
         #13#10'   pThis                     : 0x%.08X' +
         #13#10'   dwUnknown1                : 0x%.08X' +
         #13#10');',
         [pThis, dwUnknown1]); *)

  // Cxbx TODO: Actually implement this

  EmuSwapFS(fsXbox);   // XBox FS

  Result := S_OK;
 end;

// s+
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetMaxDistance
(
    LPDIRECTSOUNDBUFFER8    pThis,
    FLOAT                   flMaxDistance,
    DWORD                   dwApply
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    // debug trace
    #ifdef _DEBUG_TRACE
    begin
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetMaxDistance'
               #13#10'('
               #13#10'   pThis                     : 0x%.08X'
               #13#10'   flMaxDistance             : %f'
               #13#10'   dwApply                   : 0x%.08X'
               #13#10');',
               [pThis, flMaxDistance, dwApply);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    // TODO: Actually do something

    Result := DS_OK;
 end;   *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetMinDistance
(
    LPDIRECTSOUNDBUFFER8    pThis,
    FLOAT                   flMinDistance,
    DWORD                   dwApply
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin 
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetMinDistance'
               #13#10'('
               #13#10'   pThis                     : 0x%.08X'
               #13#10'   flMinDistance             : %f'
               #13#10'   dwApply                   : 0x%.08X'
               #13#10');',
               [pThis, flMinDistance, dwApply);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    // TODO: Actually do something

    Result := DS_OK;
 end;         *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetRolloffFactor
(
    LPDIRECTSOUNDBUFFER8    pThis,
    FLOAT                   flRolloffFactor,
    DWORD                   dwApply
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetRolloffFactor'
               #13#10'('
               #13#10'   pThis                     : 0x%.08X'
               #13#10'   flRolloffFactor           : %f'
               #13#10'   dwApply                   : 0x%.08X'
               #13#10');',
               [pThis, flRolloffFactor, dwApply);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    // TODO: Actually do something

    Result := DS_OK;
 end;       *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetDistanceFactor
(
    LPDIRECTSOUNDBUFFER8    pThis,
    FLOAT                   flDistanceFactor,
    DWORD                   dwApply
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetDistanceFactor'
               #13#10'('
               #13#10'   pThis                     : 0x%.08X'
               #13#10'   flDistanceFactor          : %f'
               #13#10'   dwApply                   : 0x%.08X'
               #13#10');',
               [pThis, flDistanceFactor, dwApply);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    // TODO: Actually do something

    Result := DS_OK;
 end;   *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetConeAngles
(
    LPDIRECTSOUNDBUFFER8    pThis,
    DWORD                   dwInsideConeAngle,
    DWORD                   dwOutsideConeAngle,
    DWORD                   dwApply
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetConeAngles'
               #13#10'('
               #13#10'   pThis                     : 0x%.08X'
               #13#10'   dwInsideConeAngle         : 0x%.08X'
               #13#10'   dwOutsideConeAngle        : 0x%.08X'
               #13#10'   dwApply                   : 0x%.08X'
               #13#10');',
               [pThis, dwInsideConeAngle,
               dwOutsideConeAngle, dwApply);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    // TODO: Actually do something

    Result := DS_OK;
 end;      *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetConeOrientation
(
    LPDIRECTSOUNDBUFFER8    pThis,
    FLOAT                   x,
    FLOAT                   y,
    FLOAT                   z,
    DWORD                   dwApply
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin 
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetConeOrientation'
               #13#10'('
               #13#10'   pThis                     : 0x%.08X'
               #13#10'   x                         : %f'
               #13#10'   y                         : %f'
               #13#10'   z                         : %f'
               #13#10'   dwApply                   : 0x%.08X'
               #13#10');',
               [pThis, x, y, z, dwApply);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    // TODO: Actually do something

    Result := DS_OK;
 end;     *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetConeOutsideVolume
(
    LPDIRECTSOUNDBUFFER8    pThis,
    LongInt                    lConeOutsideVolume,
    DWORD                   dwApply
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetConeOutsideVolume'
               #13#10'('
               #13#10'   pThis                     : 0x%.08X'
               #13#10'   lConeOutsideVolume        : 0x%.08X'
               #13#10'   dwApply                   : 0x%.08X'
               #13#10');',
               [pThis, lConeOutsideVolume, dwApply);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    // TODO: Actually do something

    Result := DS_OK;
 end;     *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetPosition
(
    LPDIRECTSOUNDBUFFER8    pThis,
    FLOAT                   x,
    FLOAT                   y,
    FLOAT                   z,
    DWORD                   dwApply
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin 
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetPosition'
               #13#10'('
               #13#10'   pThis                     : 0x%.08X'
               #13#10'   x                         : %f'
               #13#10'   y                         : %f'
               #13#10'   z                         : %f'
               #13#10'   dwApply                   : 0x%.08X'
               #13#10');',
               [pThis, x, y, z, dwApply);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    // TODO: Actually do something

    Result := DS_OK;
 end;    *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetVelocity
(
    LPDIRECTSOUNDBUFFER8    pThis,
    FLOAT                   x,
    FLOAT                   y,
    FLOAT                   z,
    DWORD                   dwApply
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetVelocity'
               #13#10'('
               #13#10'   pThis                     : 0x%.08X'
               #13#10'   x                         : %f'
               #13#10'   y                         : %f'
               #13#10'   z                         : %f'
               #13#10'   dwApply                   : 0x%.08X'
               #13#10');',
               [pThis, x, y, z, dwApply);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    // TODO: Actually do something

    Result := DS_OK;
 end;     *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetDopplerFactor
(
    LPDIRECTSOUNDBUFFER8    pThis,
    FLOAT                   flDopplerFactor,
    DWORD                   dwApply
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    // debug trace
    #ifdef _DEBUG_TRACE
    begin
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetConeOutsideVolume'
               #13#10'('
               #13#10'   pThis                     : 0x%.08X'
               #13#10'   flDopplerFactor           : %f'
               #13#10'   dwApply                   : 0x%.08X'
               #13#10');',
               [pThis, flDopplerFactor, dwApply);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    // TODO: Actually do something

    Result := DS_OK;
 end;    *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetI3DL2Source
(
    LPDIRECTSOUNDBUFFER8    pThis,
    LPCDSI3DL2BUFFER        pds3db,
    DWORD                   dwApply
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin 
    // debug trace
    #ifdef _DEBUG_TRACE
    begin 
        EmuSwapFS();   // Win2k/XP FS
        DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetI3DL2Source'
               #13#10'('
               #13#10'   pThis                     : 0x%.08X'
               #13#10'   pds3db                    : 0x%.08X'
               #13#10'   dwApply                   : 0x%.08X'
               #13#10');',
               [pThis, pds3db, dwApply);
        EmuSwapFS();   // XBox FS
     end;
    //endif

    // TODO: Actually do something

    Result := DS_OK;
 end;    *)

// +s
(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetFormat
(
    X_CDirectSoundBuffer *pBuffer,
    LPCWAVEFORMATEX pwfxFormat
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    // debug trace
    #ifdef _DEBUG_TRACE
    begin
        printf('EmuDSound : EmuIDirectSoundBuffer8_SetFormat'
               #13#10'('
               #13#10'   pBuffer                   : 0x%.08X'
               #13#10'   pwfxFormat                : 0x%.08X'
               #13#10');',
               [pBuffer,pwfxFormat);
     end;
    //endif

    HRESULT hRet := DS_OK;

    EmuSwapFS();   // XBox FS

    Result := hRet;
 end;      *)

(*STDAPI_(STDAPI_(procedure: ) EmuDirectSoundUseFullHRTF;d
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuDirectSoundUseFullHRTF()');

    // TODO: Actually implement this

    EmuSwapFS();   // XBox FS
 end;          *)

(*HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetLFO
(
	LPDIRECTSOUNDBUFFER  pThis,
	LPCDSLFODESC         pLFODesc
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetLFO'
           #13#10'('
           #13#10'   pThis                     : 0x%.08X'
           #13#10'   pLFODesc                  : 0x%.08X'
           #13#10');',
           [pThis, pLFODesc);

	// TODO: Implement

    EmuSwapFS();   // XBox FS

    Result := S_OK;
 end;    *)

(*procedure XTL_EmuXAudioCreateAdpcmFormat
(
	WORD                   nChannels,
	DWORD                  nSamplesPerSec,
	LPXBOXADPCMWAVEFORMAT  pwfx
); stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuXAudioCreateAdpcmFormat'
           #13#10'('
           #13#10'   nChannels                 : 0x%.04X'
           #13#10'   nSamplesPerSec            : 0x%.08X'
           #13#10'   pwfx                      : 0x%.08X'
           #13#10');',
           [nChannels, nSamplesPerSec, pwfx);

	// Fill out the pwfx structure with the appropriate data
	pwfx.wfx.wFormatTag		:= WAVE_FORMAT_XBOX_ADPCM;
	pwfx.wfx.nChannels			:= nChannels;
	pwfx.wfx.nSamplesPerSec	:= nSamplesPerSec;
	pwfx.wfx.nAvgBytesPerSec	:= (nSamplesPerSec*nChannels * 36)/64;
	pwfx.wfx.nBlockAlign		:= nChannels * 36;
	pwfx.wfx.wBitsPerSample	:= 4;
	pwfx.wfx.cbSize			:= 2;
	pwfx.wSamplesPerBlock		:= 64;

    EmuSwapFS();   // XBox FS
 end;     *)

(*
HRESULT WINAPI XTL.EmuIDirectSoundBuffer8_SetRolloffCurve
(
	LPDIRECTSOUNDBUFFER  pThis,
	 FLOAT         *pflPoints,
	DWORD                dwPointCount,
	DWORD                dwApply
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS();   // Win2k/XP FS

  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetRolloffCurve'
       #13#10'('
       #13#10'   pThis                     : 0x%.08X'
       #13#10'   pflPoints                 : 0x%.08X'
       #13#10'   dwPointCount              : 0x%.08X'
       #13#10'   dwApply                   : 0x%.08X'
       #13#10');',
       [pThis, pflPoints, dwPointCount, dwApply);

  // Cxbx TODO: Implement

  EmuSwapFS();   // XBox FS

	Result := DS_OK;
end;
*)

(*HRESULT WINAPI XTL.EmuIDirectSoundStream_SetVolume
(
	LPDIRECTSOUNDSTREAM pStream,
	LongInt                lVolume
)
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin 
    EmuSwapFS();   // Win2k/XP FS

    DbgPrintf('EmuDSound : EmuIDirectSoundStream_SetVolume'
           #13#10'('
           #13#10'   pStream                   : 0x%.08X'
           #13#10'   lVolume                   : 0x%.08X'
           #13#10');',
           [pStream, lVolume);

	// TODO: Implement

    EmuSwapFS();   // XBox FS

	Result := DS_OK;
 end;   *)


end.
