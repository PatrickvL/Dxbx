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

{$INCLUDE Dxbx.inc}

interface

implementation

uses
  // Delphi
  Windows
  , MMSystem
  // Jedi Win32API
  , JwaWinType
  // DirectX
  , DirectSound
  , DirectMusic
  // Dxbx
  , uTypes
  , uLog
  , uEmu
  , uEmuAlloc
  , uEmuFS
  , uDxbxKrnlUtils
  ;

const
  // EmuIDirectSoundBuffer8_Play flags
  X_DSBPLAY_LOOPING = $00000001;
  X_DSBPLAY_FROMSTART = $00000002;

  // EmuIDirectSoundBuffer8_Pause flags
  X_DSBPAUSE_RESUME = $00000000;
  X_DSBPAUSE_PAUSE = $00000001;
  X_DSBPAUSE_SYNCHPLAYBACK = $00000002;

type
  LPWAVEFORMATEX = MMSystem.PWaveFormatEx; // alias
  LPCDSI3DL2BUFFER = PVoid;

type X_DSBUFFERDESC = packed record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwBufferBytes: DWORD;
    lpwfxFormat: LPWAVEFORMATEX;
    lpMixBins: LPVOID;      // Cxbx TODO: Implement
    dwInputMixBin: DWORD;
  end;
  PX_DSBUFFERDESC = ^X_DSBUFFERDESC;

type X_DSSTREAMDESC = packed record
    dwFlags: DWORD;
    dwMaxAttachedPackets: DWORD;
    lpwfxFormat: LPWAVEFORMATEX;
    lpfnCallback: PVOID;   // Cxbx TODO: Correct Parameter
    lpvContext: LPVOID;
    lpMixBins: PVOID;      // Cxbx TODO: Correct Parameter
  end;
  PX_DSSTREAMDESC = ^X_DSSTREAMDESC;

  REFERENCE_TIME = LONGLONG;
  PPREFERENCE_TIME = ^REFERENCE_TIME;
  LPREFERENCE_TIME = ^REFERENCE_TIME;

type _XMEDIAPACKET = packed record
    pvBuffer: LPVOID;
    dwMaxSize: DWORD;
    pdwCompletedSize: PDWORD;
    pdwStatus: PDWORD;
    case Integer of // union {
    0: (hCompletionEvent: HANDLE;);
    1: (
        pContext: PVOID;
    // end;
      prtTimestamp: PREFERENCE_TIME;
    ); // end of union
  end;
  XMEDIAPACKET = _XMEDIAPACKET;
  PXMEDIAPACKET = ^XMEDIAPACKET;
  LPXMEDIAPACKET = ^XMEDIAPACKET;

type _DSLFODESC = packed record
    dwLFO: DWORD;
    dwDelay: DWORD;
    dwDelta: DWORD;
    lPitchModulation: LONG;
    lFilterCutOffRange: LONG;
    lAmplitudeModulation: LONG;
  end;
  DSLFODESC = _DSLFODESC;
  LPCDSLFODESC = ^DSLFODESC;

type xbox_adpcmwaveformat_tag = packed record
    wfx: TWAVEFORMATEX;            // WAVEFORMATEX data
    wSamplesPerBlock: WORD;       // Number of samples per encoded block.  It must be 64.
  end;
  XBOXADPCMWAVEFORMAT = xbox_adpcmwaveformat_tag;
  PXBOXADPCMWAVEFORMAT = ^XBOXADPCMWAVEFORMAT;
  LPXBOXADPCMWAVEFORMAT = PXBOXADPCMWAVEFORMAT;

type X_DSOUTPUTLEVELS = packed record
    dwAnalogLeftTotalPeak: DWORD;// analog peak
    dwAnalogRightTotalPeak: DWORD;
    dwAnalogLeftTotalRMS: DWORD;// analog RMS
    dwAnalogRightTotalRMS: DWORD;
    dwDigitalFrontLeftPeak: DWORD;// digital peak levels
    dwDigitalFrontCenterPeak: DWORD;
    dwDigitalFrontRightPeak: DWORD;
    dwDigitalBackLeftPeak: DWORD;
    dwDigitalBackRightPeak: DWORD;
    dwDigitalLowFrequencyPeak: DWORD;
    dwDigitalFrontLeftRMS: DWORD;// digital RMS levels
    dwDigitalFrontCenterRMS: DWORD;
    dwDigitalFrontRightRMS: DWORD;
    dwDigitalBackLeftRMS: DWORD;
    dwDigitalBackRightRMS: DWORD;
    dwDigitalLowFrequencyRMS: DWORD;
  end;

(*
typedef struct IDirectSoundStream IDirectSoundStream;
typedef IDirectSoundStream *LPDIRECTSOUNDSTREAM;
*)

type X_CDirectSound = packed record
    // Cxbx TODO: Fill this in?
  end;
  PX_CDirectSound = ^X_CDirectSound;

type X_CDirectSoundBuffer = packed record
    UnknownA: array [0..$20-1] of Byte; // Offset: 0x00
    {union}case Integer of
    0: (
      pMpcxBuffer: PVOID);          // Offset: 0x20
    1: (
      EmuDirectSoundBuffer8: ^IDirectSoundBuffer;
    // endcase; fall through :
    UnknownB: array [0..$0C-1] of Byte; // Offset: 0x24
    EmuBuffer: PVOID;                   // Offset: 0x28
    EmuBufferDesc: PDSBUFFERDESC;       // Offset: 0x2C
    EmuLockPtr1: PVOID;                 // Offset: 0x30
    EmuLockBytes1: DWORD;               // Offset: 0x34
    EmuLockPtr2: PVOID;                 // Offset: 0x38
    EmuLockBytes2: DWORD;               // Offset: 0x3C
    EmuPlayFlags: DWORD;                // Offset: 0x40
    EmuFlags: DWORD                     // Offset: 0x44
    ); // end of union
  end;
  PX_CDirectSoundBuffer = ^X_CDirectSoundBuffer;
  PPX_CDirectSoundBuffer = ^PX_CDirectSoundBuffer;

const DSB_FLAG_ADPCM = $00000001;
const WAVE_FORMAT_XBOX_ADPCM = $0069;

type X_CMcpxStream = class(TObject)
  (*
    public:
        // construct vtable (or grab ptr to existing)
        X_CMcpxStream(class X_CDirectSoundStream *pParentStream) : pVtbl(&vtbl), pParentStream(pParentStream) {end;

    private:
        // vtable (cached by each instance, via constructor)
        struct _vtbl
        {
            DWORD Unknown1;                                                 // 0x00 - ???
            DWORD Unknown2;                                                 // 0x04 - ???
            DWORD Unknown3;                                                 // 0x08 - ???
            DWORD Unknown4;                                                 // 0x0C - ???

            //
            // Cxbx TODO: Function needs X_CMcpxStream "this" pointer (ecx!)
            //

            VOID (WINAPI *Dummy_0x10)(DWORD dwDummy1, DWORD dwDummy2);   // 0x10
        end;
        *pVtbl;

        // global vtbl for this class
        static _vtbl vtbl;

        // debug mode guard for detecting naughty data accesses
        #ifdef _DEBUG
        DWORD DebugGuard[256];
        #endif

    public:

        class X_CDirectSoundStream *pParentStream;
  *)
  end;

type X_CDirectSoundStream = class(TObject)
  (*
    public:
        // construct vtable (or grab ptr to existing)
        X_CDirectSoundStream() : pVtbl(&vtbl) { pMcpxStream = new X_CMcpxStream(this); end;

    private:
        // vtable (cached by each instance, via constructor)
        struct _vtbl
        {
            ULONG (WINAPI *AddRef)(X_CDirectSoundStream *pThis);            // 0x00
            ULONG (WINAPI *Release)(X_CDirectSoundStream *pThis);           // 0x04
            DWORD Unknown;                                                  // 0x08

            HRESULT (WINAPI *GetStatus)                                     // 0x0C
            (
                X_CDirectSoundStream   *pThis,
                DWORD                  *pdwStatus
            );

            HRESULT (WINAPI *Process)                                       // 0x10
            (
                X_CDirectSoundStream   *pThis,
                PXMEDIAPACKET           pInputBuffer,
                PXMEDIAPACKET           pOutputBuffer
            );

            HRESULT (WINAPI *Discontinuity)(X_CDirectSoundStream *pThis);   // 0x14

            HRESULT (WINAPI *Flush)(X_CDirectSoundStream *pThis);           // 0x18

            DWORD Unknown2;                                                 // 0x1C - ???
            DWORD Unknown3;                                                 // 0x20 - ???
            DWORD Unknown4;                                                 // 0x24 - ???
            DWORD Unknown5;                                                 // 0x28 - ???
            DWORD Unknown6;                                                 // 0x2C - ???
            DWORD Unknown7;                                                 // 0x30 - ???
            DWORD Unknown8;                                                 // 0x34 - ???
            DWORD Unknown9;                                                 // 0x38 - ???
        end;
        *pVtbl;

        // global vtbl for this class
        static _vtbl vtbl;

        DWORD Spacer[8];
        PVOID pMcpxStream;

        // debug mode guard for detecting naughty data accesses
        #ifdef _DEBUG
        DWORD DebugGuard[256];
        #endif               *)

    public
        // cached data
        EmuDirectSoundBuffer8: IDirectSoundBuffer;
        EmuBuffer: PVOID;
        EmuBufferDesc: PDSBUFFERDESC;
        EmuLockPtr1: PVOID;
        EmuLockBytes1: DWORD;
        EmuLockPtr2: PVOID;
        EmuLockBytes2: DWORD;
        EmuPlayFlags: DWORD;
    end;
  PX_CDirectSoundStream = ^X_CDirectSoundStream;
  PPX_CDirectSoundStream = ^PX_CDirectSoundStream;


  LPDIRECTSOUND8 = {^}IDIRECTSOUND8;
  PLPDIRECTSOUND8 = ^LPDIRECTSOUND8;

// size of sound buffer cache (used for periodic sound buffer updates)
const SOUNDBUFFER_CACHE_SIZE = $100;

// size of sound stream cache (used for periodic sound stream updates)
const SOUNDSTREAM_CACHE_SIZE = $100;

// Static Variable(s)
var g_pDSound8: IDIRECTSOUND8 = nil;
var g_pDSound8RefCount: Int = 0;
var g_pDSoundBufferCache: array [0..SOUNDBUFFER_CACHE_SIZE-1] of PX_CDirectSoundBuffer;
var g_pDSoundStreamCache: array [0..SOUNDSTREAM_CACHE_SIZE-1] of X_CDirectSoundStream;
var g_bDSoundCreateCalled: Boolean = False;

// periodically update sound buffers
procedure HackUpdateSoundBuffers();
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  v: Integer;
  pAudioPtr, pAudioPtr2 : PVOID;
  dwAudioBytes, dwAudioBytes2 : DWORD;
  hRet : HRESULT;
begin
  for v := 0 to SOUNDBUFFER_CACHE_SIZE -1 do begin
    if (g_pDSoundBufferCache[v] = nil) or (g_pDSoundBufferCache[v].EmuBuffer = nil) then
        continue;

    // unlock existing lock
    if (g_pDSoundBufferCache[v].EmuLockPtr1 <> nil) then
        g_pDSoundBufferCache[v].EmuDirectSoundBuffer8.Unlock(g_pDSoundBufferCache[v].EmuLockPtr1, g_pDSoundBufferCache[v].EmuLockBytes1, g_pDSoundBufferCache[v].EmuLockPtr2, g_pDSoundBufferCache[v].EmuLockBytes2);

    hRet := g_pDSoundBufferCache[v].EmuDirectSoundBuffer8.Lock(0, g_pDSoundBufferCache[v].EmuBufferDesc.dwBufferBytes, @pAudioPtr, @dwAudioBytes, @pAudioPtr2, @dwAudioBytes2, 0);

    if (SUCCEEDED(hRet)) then
    begin
        if (pAudioPtr <> nil) then
          memcpy(g_pDSoundBufferCache[v].EmuBuffer, pAudioPtr, dwAudioBytes);

        if (pAudioPtr2 <> nil) then
            memcpy(PVoid(DWord(g_pDSoundBufferCache[v].EmuBuffer)+dwAudioBytes), pAudioPtr2, dwAudioBytes2);

        g_pDSoundBufferCache[v].EmuDirectSoundBuffer8.Unlock(pAudioPtr, dwAudioBytes, pAudioPtr2, dwAudioBytes2);
     end;

    // Cxbx TODO: relock old lock ??
   end;
end;

// periodically update sound streams
procedure HackUpdateSoundStreams();
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  v : Integer;
  pAudioPtr, pAudioPtr2 : PVOID;
  dwAudioBytes, dwAudioBytes2 : DWORD;
  hRet : HRESULT;
begin
  for v := 0 to SOUNDSTREAM_CACHE_SIZE - 1 do begin
    if (g_pDSoundStreamCache[v] = nil) or (g_pDSoundStreamCache[v].EmuBuffer = nil) then
        continue;

    hRet := g_pDSoundStreamCache[v].EmuDirectSoundBuffer8.Lock(0, g_pDSoundStreamCache[v].EmuBufferDesc.dwBufferBytes, @pAudioPtr, @dwAudioBytes, @pAudioPtr2, @dwAudioBytes2, 0);

    if (SUCCEEDED(hRet)) then
    begin
      if (pAudioPtr <> nil) then
        memcpy(pAudioPtr,  g_pDSoundStreamCache[v].EmuBuffer, dwAudioBytes);

      if (pAudioPtr2 <> nil) then
        memcpy(PVOID((DWORD(g_pDSoundStreamCache[v].EmuBuffer)+dwAudioBytes)), pAudioPtr2, dwAudioBytes2);

      g_pDSoundStreamCache[v].EmuDirectSoundBuffer8.Unlock(pAudioPtr, dwAudioBytes, pAudioPtr2, dwAudioBytes2);
    end;

    g_pDSoundStreamCache[v].EmuDirectSoundBuffer8.SetCurrentPosition(0);
    g_pDSoundStreamCache[v].EmuDirectSoundBuffer8.Play(0, 0, 0);
  end;
end;

// resize an emulated directsound buffer, if necessary
procedure EmuResizeIDirectSoundBuffer8(pThis: PX_CDirectSoundBuffer; dwBytes: DWORD);
var
  dwPlayCursor: DWORD;
  dwWriteCursor: DWORD;
  dwStatus: DWORD;
  hRet: HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  if (dwBytes = pThis.EmuBufferDesc.dwBufferBytes) or (dwBytes = 0) then
      Exit;

{$IFDEF DEBUG}
  DbgPrintf('EmuResizeIDirectSoundBuffer8 : Resizing! ($%.08X.$%.08X)', [pThis.EmuBufferDesc.dwBufferBytes, dwBytes]);
{$ENDIF}

  hRet := pThis.EmuDirectSoundBuffer8.GetCurrentPosition(@dwPlayCursor, @dwWriteCursor);

  if (FAILED(hRet)) then
      CxbxKrnlCleanup('Unable to retrieve current position for resize reallocation!');

  hRet := pThis.EmuDirectSoundBuffer8.GetStatus(dwStatus);

  if (FAILED(hRet)) then
      CxbxKrnlCleanup('Unable to retrieve current status for resize reallocation!');

  // release old buffer
  while(pThis.EmuDirectSoundBuffer8._Release() > 0) do
  begin
  end;

  pThis.EmuBufferDesc.dwBufferBytes := dwBytes;

  hRet := g_pDSound8.CreateSoundBuffer(pThis.EmuBufferDesc^, pThis.EmuDirectSoundBuffer8^, nil);

  if (FAILED(hRet)) then
      CxbxKrnlCleanup('IDirectSoundBuffer8 resize Failed!');

  pThis.EmuDirectSoundBuffer8.SetCurrentPosition(dwPlayCursor);

  if (dwStatus and DSBSTATUS_PLAYING) > 0 then
      pThis.EmuDirectSoundBuffer8.Play(0, 0, pThis.EmuPlayFlags);
end;


// resize an emulated directsound stream, if necessary
procedure EmuResizeIDirectSoundStream8(var pThis: X_CDirectSoundStream; dwBytes: DWORD);
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  dwPlayCursor: DWORD;
  dwWriteCursor: DWORD;
  dwStatus: DWORD;
  hRet: HRESULT;
begin
  if (dwBytes = pThis.EmuBufferDesc.dwBufferBytes) then
    Exit;

  hRet := pThis.EmuDirectSoundBuffer8.GetCurrentPosition(@dwPlayCursor, @dwWriteCursor);

  if (FAILED(hRet)) then
      CxbxKrnlCleanup('Unable to retrieve current position for resize reallocation!');

  hRet := pThis.EmuDirectSoundBuffer8.GetStatus(dwStatus);

  if (FAILED(hRet)) then
      CxbxKrnlCleanup('Unable to retrieve current status for resize reallocation!');

  // release old buffer
  while(pThis.EmuDirectSoundBuffer8._Release() > 0) do
  begin
  end;

  pThis.EmuBufferDesc.dwBufferBytes := dwBytes;

  hRet := g_pDSound8.CreateSoundBuffer(pThis.EmuBufferDesc^, pThis.EmuDirectSoundBuffer8, nil);

  if (FAILED(hRet)) then
      CxbxKrnlCleanup('IDirectSoundBuffer8 resize Failed!');

  pThis.EmuDirectSoundBuffer8.SetCurrentPosition(dwPlayCursor);

  if (dwStatus and DSBSTATUS_PLAYING) > 0 then
      pThis.EmuDirectSoundBuffer8.Play(0, 0, pThis.EmuPlayFlags);
end;


function XTL_EmuDirectSoundCreate(
    pguidDeviceId: Pointer;
    ppDirectSound: PLPDIRECTSOUND8;
    pUnknown: IUNKNOWN): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
{$WRITEABLECONST ON}
const
  Initialized: Bool = False;
{$WRITEABLECONST OFF}
var
  v: Integer;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuDirectSoundCreate' +
      #13#10'(' +
      #13#10'   pguidDeviceId             : 0x%.08X' +
      #13#10'   ppDirectSound             : 0x%.08X' +
      #13#10'   pUnknown                  : 0x%.08X' +
      #13#10');',
      [pguidDeviceId, ppDirectSound, pUnknown]);
{$ENDIF}


  if Initialized then
    Result := DS_OK
  else
  begin
    PPointer(ppDirectSound)^ := nil; // Prevent Delphi _Release on invalid pointer contents
    Result := DirectSoundCreate8(nil, {out}ppDirectSound^, nil);

    if FAILED(Result) then
      CxbxKrnlCleanup('DirectSoundCreate8 Failed!');

    g_pDSound8 := ppDirectSound^;

    Result := g_pDSound8.SetCooperativeLevel(g_hEmuWindow, DSSCL_PRIORITY);

    if FAILED(Result) then
      CxbxKrnlCleanup('g_pDSound8.SetCooperativeLevel Failed!');


    // clear sound buffer cache
    for v := 0 to SOUNDBUFFER_CACHE_SIZE - 1  do
    begin
      g_pDSoundBufferCache[v] := nil;
    end;

    // clear sound stream cache
    for v := 0 to SOUNDSTREAM_CACHE_SIZE - 1 do
    begin
      g_pDSoundStreamCache[v] := nil;
    end;

    Initialized := True;
  end;

  g_pDSound8RefCount := 1;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirectSound8_AddRef(pThis: LPDIRECTSOUND8): ULONG; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  uRet: ULONG;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSound8_AddRef' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10');',
         [pThis]);
{$ENDIF}

  uRet := g_pDSound8RefCount;
  Inc(g_pDSound8RefCount);

  EmuSwapFS(fsXbox);

  Result := uRet;
 end;

function XTL_EmuIDirectSound8_Release(
    pThis: LPDIRECTSOUND8): ULONG; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  uRet: ULONG;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSound8_Release' +
         #13#10'(' +
         #13#10'   pThis                     : 0x%.08X' +
         #13#10');',
         [pThis]);
{$ENDIF}

  uRet := g_pDSound8RefCount;
  inc(g_pDSound8RefCount);

  { temporarily (?) disabled by cxbx
  if (uRet = 1) then
      pThis._Release();
  //}

  EmuSwapFS(fsXbox);

  Result := uRet;
end;

function XTL_EmuCDirectSound_GetSpeakerConfig(
    pThis: PX_CDirectSound;
    pdwSpeakerConfig: PDWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSound_GetSpeakerConfig' +
         #13#10'(' +
         #13#10'   pThis                     : 0x%.08X' +
         #13#10'   pdwSpeakerConfig          : 0x%.08X' +
         #13#10');',
         [pThis, pdwSpeakerConfig]);
{$ENDIF}

  pdwSpeakerConfig^ := 0; // STEREO

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;


function XTL_EmuIDirectSound8_DownloadEffectsImage(
    pThis: LPDIRECTSOUND8;
    pvImageBuffer: LPCVOID;
    dwImageSize: DWORD;
    pImageLoc: PVOID;      // Cxbx TODO: Use this param
    ppImageDesc: PVOID   // Cxbx TODO: Use this param
): HResult; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSound8_DownloadEffectsImage' +
         #13#10'(' +
         #13#10'   pThis                     : 0x%.08X' +
         #13#10'   pvImageBuffer             : 0x%.08X' +
         #13#10'   dwImageSize               : 0x%.08X' +
         #13#10'   pImageLoc                 : 0x%.08X' +
         #13#10'   ppImageDesc               : 0x%.08X' +
         #13#10');',
         [pThis, pvImageBuffer, dwImageSize, pImageLoc, ppImageDesc]);
{$ENDIF}

  // Cxbx TODO: Actually implement this

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

procedure XTL_EmuDirectSoundDoWork(); stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuDirectSoundDoWork();');
{$ENDIF}

  HackUpdateSoundBuffers();
  HackUpdateSoundStreams();

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirectSound8_SetOrientation(
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
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
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
{$ENDIF}

  // Cxbx TODO: Actually implement this

  EmuSwapFS(fsXbox);

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
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSound8_SetDistanceFactor' +
         #13#10'(' +
         #13#10'   pThis                     : 0x%.08X' +
         #13#10'   fDistanceFactor           :  % f' +
         #13#10'   dwApply                   : 0x%.08X' +
         #13#10');',
         [pThis, fDistanceFactor, dwApply]);
{$ENDIF}

  // Cxbx TODO: Actually implement this

  EmuSwapFS(fsXbox);

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
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSound8_SetRolloffFactor' +
         #13#10'(' +
         #13#10'   pThis                     : 0x%.08X' +
         #13#10'   fRolloffFactor            :  % f' +
         #13#10'   dwApply                   : 0x%.08X' +
         #13#10');',
         [pThis, fRolloffFactor, dwApply]);
{$ENDIF}

  // Cxbx TODO: Actually implement this

  EmuSwapFS(fsWindows);

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
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSound8_SetDopplerFactor' +
         #13#10'(' +
         #13#10'   pThis                     : 0x%.08X' +
         #13#10'   fDopplerFactor            :  % f' +
         #13#10'   dwApply                   : 0x%.08X' +
         #13#10');',
         [pThis, fDopplerFactor, dwApply]);
{$ENDIF}

  // Cxbx TODO: Actually implement this

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIDirectSound8_SetI3DL2Listener
(
    pThis: LPDIRECTSOUND8;
    pDummy: PVOID; // Cxbx TODO: fill this out
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
{$IFDEF _DEBUG_TRACE}
  EmuSwapFS(fsWindows);
  DbgPrintf('EmuDSound : EmuIDirectSound8_SetI3DL2Listener' +
     #13#10'(' +
     #13#10'   pThis                     : 0x%.08X' +
     #13#10'   pDummy                    : 0x%.08X' +
     #13#10'   dwApply                   : 0x%.08X' +
     #13#10');',
     [pThis, pDummy, dwApply]);
  EmuSwapFS(fsXbox);
{$ENDIF}

  // Cxbx TODO: Actually do something

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
{$IFDEF _DEBUG_TRACE}
  begin
    EmuSwapFS(fsWindows);
    DbgPrintf('EmuDSound : EmuIDirectSound8_SetMixBinHeadroom' +
        #13#10'(' +
        #13#10'   pThis                     : 0x%.08X' +
        #13#10'   dwMixBinMask              : 0x%.08X' +
        #13#10'   dwHeadroom                : 0x%.08X' +
        #13#10');',
              [pThis, dwMixBinMask, dwHeadroom]);
    EmuSwapFS(fsXbox);
  end;
{$ENDIF}

  // Cxbx TODO: Actually do something

  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetMixBins
(
    pThis: LPDIRECTSOUND8;
    pMixBins: PVOID
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
{$IFDEF _DEBUG_TRACE}
  begin
      EmuSwapFS(fsWindows);
      DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetMixBins' +
             #13#10'(' +
             #13#10'   pThis                     : 0x%.08X' +
             #13#10'   pMixBins                  : 0x%.08X' +
             #13#10');',
             [pThis, pMixBins]);
      EmuSwapFS(fsXbox);
   end;
  {$ENDIF}

  // Cxbx TODO: Actually do something

  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetMixBinVolumes
(
    pThis: LPDIRECTSOUND8;
    pMixBins: PVOID
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
{$IFDEF _DEBUG_TRACE}
  begin
      EmuSwapFS(fsWindows);
      DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetMixBinVolumes' +
             #13#10'(' +
             #13#10'   pThis                     : 0x%.08X' +
             #13#10'   pMixBins                  : 0x%.08X' +
             #13#10');',
             [pThis, pMixBins]);
      EmuSwapFS(fsXbox);
   end;
  {$ENDIF}

  // Cxbx TODO: Actually do something

  Result := DS_OK;
end;

function XTL_EmuIDirectSound8_SetPosition(
    pThis: LPDIRECTSOUND8;
    x: FLOAT;
    y: FLOAT;
    z: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
{$IFDEF _DEBUG_TRACE}
  begin
      EmuSwapFS(fsWindows);
      DbgPrintf('EmuDSound : EmuIDirectSound8_SetPosition' +
             #13#10'(' +
             #13#10'   pThis                     : 0x%.08X' +
             #13#10'   x                         :  % f' +
             #13#10'   y                         :  % f' +
             #13#10'   z                         :  % f' +
             #13#10'   dwApply                   : 0x%.08X' +
             #13#10');',
             [pThis, x, y, z, dwApply]);
      EmuSwapFS(fsXbox);
   end;
  {$ENDIF}

  // Cxbx TODO: Actually do something

  Result := DS_OK;
end;

function XTL_EmuIDirectSound8_SetVelocity(
    pThis: LPDIRECTSOUND8;
    x: FLOAT;
    y: FLOAT;
    z: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
{$IFDEF _DEBUG_TRACE}
  begin
      EmuSwapFS(fsWindows);
      DbgPrintf('EmuDSound : EmuIDirectSound8_SetVelocity' +
             #13#10'(' +
             #13#10'   pThis                     : 0x%.08X' +
             #13#10'   x                         :  % f' +
             #13#10'   y                         :  % f' +
             #13#10'   z                         :  % f' +
             #13#10'   dwApply                   : 0x%.08X' +
             #13#10');',
             [pThis, x, y, z, dwApply]);
      EmuSwapFS(fsXbox);
   end;
  {$ENDIF}

  // Cxbx TODO: Actually do something

  Result := DS_OK;
end;

function XTL_EmuIDirectSound8_SetAllParameters(
    pThis: LPDIRECTSOUND8;
    pTodo: Pointer;  // Dxbx TODO: LPCDS3DLISTENER
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
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

function XTL_EmuCDirectSound_CommitDeferredSettings(
    pThis: PX_CDirectSound
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSound_CommitDeferredSettings' +
         #13#10'(' +
         #13#10'   pThis                     : $%.08X' +
         #13#10');',
         [pThis]);
{$ENDIF}

  // Cxbx TODO: Translate params, then make the PC DirectSound call

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuDirectSoundCreateBuffer
(
    pdsbd: PX_DSBUFFERDESC;
    ppBuffer: PPX_CDirectSoundBuffer
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
  dwEmuFlags: DWORD;
  pDSBufferDesc: DirectSound.PDSBUFFERDESC;
  dwAcceptableMask: DWORD;
  v: integer;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuDirectSoundCreateBuffer'+
         #13#10'('+
         #13#10'   pdsbd                     : $%.08X'+
         #13#10'   ppBuffer                  : $%.08X'+
         #13#10');',
         [pdsbd, ppBuffer]);
{$ENDIF}

  dwEmuFlags := 0;

  pDSBufferDesc := DirectSound.PDSBUFFERDESC(CxbxMalloc(SizeOf(DSBUFFERDESC)));

  // convert from Xbox to PC DSound
  begin
      dwAcceptableMask := $00000010 or $00000020 or $00000080 or $00000100 or $00002000 or $00040000 or $00080000;

      if (pdsbd.dwFlags and (not dwAcceptableMask)) > 0 then
          EmuWarning('Use of unsupported pdsbd.dwFlags mask(s) ($%.08X)', [pdsbd.dwFlags and Not(dwAcceptableMask)]);

      pDSBufferDesc.dwSize := SizeOf(DSBUFFERDESC);
      pDSBufferDesc.dwFlags := (pdsbd.dwFlags and dwAcceptableMask) or DSBCAPS_CTRLVOLUME or DSBCAPS_GETCURRENTPOSITION2;
      pDSBufferDesc.dwBufferBytes := pdsbd.dwBufferBytes;

      if (pDSBufferDesc.dwBufferBytes < DSBSIZE_MIN) then
          pDSBufferDesc.dwBufferBytes := DSBSIZE_MIN
      else if (pDSBufferDesc.dwBufferBytes > DSBSIZE_MAX) then
          pDSBufferDesc.dwBufferBytes := DSBSIZE_MAX;

      pDSBufferDesc.dwReserved := 0;

      if Assigned(pdsbd.lpwfxFormat) then
      begin
          pDSBufferDesc.lpwfxFormat := CxbxMalloc(SizeOf(TWAVEFORMATEX) + pdsbd.lpwfxFormat.cbSize);
          memcpy(pDSBufferDesc.lpwfxFormat, pdsbd.lpwfxFormat, SizeOf(TWAVEFORMATEX));

          if (pDSBufferDesc.lpwfxFormat.wFormatTag = WAVE_FORMAT_XBOX_ADPCM) then
          begin
              dwEmuFlags := dwEmuFlags or DSB_FLAG_ADPCM;

              EmuWarning('WAVE_FORMAT_XBOX_ADPCM Unsupported!');

              pDSBufferDesc.lpwfxFormat.wFormatTag := WAVE_FORMAT_PCM;
              pDSBufferDesc.lpwfxFormat.nBlockAlign := (pDSBufferDesc.lpwfxFormat.nChannels*pDSBufferDesc.lpwfxFormat.wBitsPerSample) div 8;

              // the above calculation can yield zero for wBitsPerSample < 8, so we'll bound it to 1 byte minimum
              if (pDSBufferDesc.lpwfxFormat.nBlockAlign = 0) then
                  pDSBufferDesc.lpwfxFormat.nBlockAlign := 1;

              pDSBufferDesc.lpwfxFormat.nAvgBytesPerSec := pDSBufferDesc.lpwfxFormat.nSamplesPerSec*pDSBufferDesc.lpwfxFormat.nBlockAlign;
              pDSBufferDesc.lpwfxFormat.wBitsPerSample := 8;

              { Cxbx TODO: Get ADPCM working!  MARKED OUT CXBX
              pDSBufferDesc.lpwfxFormat.cbSize := 32;
              const WAVE_FORMAT_ADPCM = 2;
              pDSBufferDesc.lpwfxFormat.wFormatTag := WAVE_FORMAT_ADPCM;
              }
          end;
      end;

      pDSBufferDesc.guid3DAlgorithm := DS3DALG_DEFAULT;
   end;

  // sanity check
  if (pDSBufferDesc.lpwfxFormat.nBlockAlign <> (pDSBufferDesc.lpwfxFormat.nChannels*pDSBufferDesc.lpwfxFormat.wBitsPerSample)/8) then
  begin
      pDSBufferDesc.lpwfxFormat.nBlockAlign := (2*pDSBufferDesc.lpwfxFormat.wBitsPerSample) div 8;
      pDSBufferDesc.lpwfxFormat.nAvgBytesPerSec := pDSBufferDesc.lpwfxFormat.nSamplesPerSec * pDSBufferDesc.lpwfxFormat.nBlockAlign;
   end;

  // Cxbx TODO: Garbage Collection
  New({var PX_CDirectSoundBuffer}ppBuffer^);

  ppBuffer^.EmuDirectSoundBuffer8 := nil;
  ppBuffer^.EmuBuffer := nil;
  ppBuffer^.EmuBufferDesc := pDSBufferDesc;
  ppBuffer^.EmuLockPtr1 := nil;
  ppBuffer^.EmuLockBytes1 := 0;
  ppBuffer^.EmuLockPtr2 := nil;
  ppBuffer^.EmuLockBytes2 := 0;
  ppBuffer^.EmuFlags := dwEmuFlags;

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuDirectSoundCreateBuffer, *ppBuffer := $%.08X, bytes := $%.08X', [ppBuffer^, pDSBufferDesc.dwBufferBytes]);
{$ENDIF}

  hRet := g_pDSound8.CreateSoundBuffer(pDSBufferDesc^, (ppBuffer^.EmuDirectSoundBuffer8)^, nil);

  if (FAILED(hRet)) then
      EmuWarning('CreateSoundBuffer Failed!');

  // cache this sound buffer
  begin
    for v := 0 to SOUNDBUFFER_CACHE_SIZE - 1 do
    begin
      if not Assigned(g_pDSoundBufferCache[v]) then
      begin
        g_pDSoundBufferCache[v] := ppBuffer^;
        break;
      end;
    end;

    if (v = SOUNDBUFFER_CACHE_SIZE) then
        CxbxKrnlCleanup('SoundBuffer cache out of slots!');
  end;

  EmuSwapFS(fsXbox);
  Result := hRet;
end;

function XTL_EmuIDirectSound8_CreateBuffer
(
    pThis: LPDIRECTSOUND8;
    pdssd: PX_DSBUFFERDESC;
    ppBuffer: PPX_CDirectSoundBuffer;
    pUnknown: PVOID
): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  // debug trace
  begin
    EmuSwapFS(fsWindows);
{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSound8_CreateBuffer'+
            #13#10'('+
            #13#10'   pThis                     : $%.08X'+
            #13#10'   pdssd                     : $%.08X'+
            #13#10'   ppBuffer                  : $%.08X'+
            #13#10'   pUnknown                  : $%.08X'+
            #13#10');',
            [pThis, pdssd, ppBuffer, pUnknown]);
{$ENDIF}
    EmuSwapFS(fsXbox);
  end;

  XTL_EmuDirectSoundCreateBuffer(pdssd, ppBuffer);

  Result := DS_OK;
end;

function XTL_EmuIDirectSound8_CreateSoundBuffer
(
    pThis: LPDIRECTSOUND8;
    pdsbd: PX_DSBUFFERDESC;
    ppBuffer: PPX_CDirectSoundBuffer;
    pUnkOuter: PUNKNOWN
): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
    EmuSwapFS(fsWindows);
{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSound8_CreateSoundBuffer'+
           #13#10'('+
           #13#10'   pdsbd                     : $%.08X'+
           #13#10'   ppBuffer                  : $%.08X'+
           #13#10'   pUnkOuter                 : $%.08X'+
           #13#10');',
           [pdsbd, ppBuffer, pUnkOuter]);
{$ENDIF}
    EmuSwapFS(fsXbox);
  end;
  {$endif}

  Result := XTL_EmuDirectSoundCreateBuffer(pdsbd, ppBuffer);
end;


function XTL_EmuIDirectSoundBuffer8_SetBufferData (pThis: PX_CDirectSoundBuffer; pvBufferData: Pointer; dwBufferBytes: DWORD): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetBufferData'+
         #13#10'('+
         #13#10'   pThis                     : $%.08X'+
         #13#10'   pvBufferData              : $%.08X'+
         #13#10'   dwBufferBytes             : $%.08X'+
         #13#10');',
         [pThis, pvBufferData, dwBufferBytes]);
{$ENDIF}

  // update buffer data cache
  pThis.EmuBuffer := pvBufferData;

  EmuResizeIDirectSoundBuffer8(pThis, dwBufferBytes);

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetPlayRegion(pThis: PX_CDirectSoundBuffer; dwPlayStart: DWORD; dwPlayLength: DWORD): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetPlayRegion'+
         #13#10'('+
         #13#10'   pThis                     : $%.08X'+
         #13#10'   dwPlayStart               : $%.08X'+
         #13#10'   dwPlayLength              : $%.08X'+
         #13#10');',
         [pThis, dwPlayStart, dwPlayLength]);
{$ENDIF}

  // Cxbx TODO: Translate params, then make the PC DirectSound call

  // Cxbx TODO: Ensure that 4627 & 4361 are intercepting far enough back
  // (otherwise pThis is manipulated!)

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_Lock(pThis: PX_CDirectSoundBuffer; dwOffset: DWORD; dwBytes: DWORD;
    ppvAudioPtr1: System.PPointer; pdwAudioBytes1: LPDWORD; ppvAudioPtr2: System.PPointer; pdwAudioBytes2: LPDWORD;
    dwFlags:DWORD): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_Lock'+
         #13#10'('+
         #13#10'   pThis                     : $%.08X'+
         #13#10'   dwOffset                  : $%.08X'+
         #13#10'   dwBytes                   : $%.08X'+
         #13#10'   ppvAudioPtr1              : $%.08X'+
         #13#10'   pdwAudioBytes1            : $%.08X'+
         #13#10'   ppvAudioPtr2              : $%.08X'+
         #13#10'   pdwAudioBytes2            : $%.08X'+
         #13#10'   dwFlags                   : $%.08X'+
         #13#10');',
         [pThis, dwOffset, dwBytes, ppvAudioPtr1, pdwAudioBytes1,
         ppvAudioPtr2, pdwAudioBytes2, dwFlags]);
{$ENDIF}

  hRet := DS_OK;

  if Assigned(pThis.EmuBuffer) then
  begin
      ppvAudioPtr1^ := pThis.EmuBuffer;
      pdwAudioBytes1^ := dwBytes;
  end
  else
  begin
    if (dwBytes > pThis.EmuBufferDesc.dwBufferBytes) then
        EmuResizeIDirectSoundBuffer8(pThis, dwBytes);

    if Assigned(pThis.EmuLockPtr1) then
        pThis.EmuDirectSoundBuffer8.Unlock(pThis.EmuLockPtr1, pThis.EmuLockBytes1, pThis.EmuLockPtr2, pThis.EmuLockBytes2);

    // Cxbx TODO: Verify dwFlags is the same as windows
    hRet := pThis.EmuDirectSoundBuffer8.Lock(dwOffset, dwBytes, ppvAudioPtr1, pdwAudioBytes1, ppvAudioPtr2, pdwAudioBytes2, dwFlags);

    if (FAILED(hRet)) then
        CxbxKrnlCleanup('DirectSoundBuffer Lock Failed!');

    pThis.EmuLockPtr1 := ppvAudioPtr1^;
    pThis.EmuLockBytes1 := pdwAudioBytes1^;

    if Assigned(ppvAudioPtr2) then
      pThis.EmuLockPtr2 := ppvAudioPtr2^
    else
      pThis.EmuLockPtr2 := nil;

    if Assigned(pdwAudioBytes2) then
      pThis.EmuLockBytes2 := pdwAudioBytes2^
    else
      pThis.EmuLockBytes2 := 0;
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuIDirectSoundBuffer8_SetHeadroom( pThis: PX_CDirectSoundBuffer; dwHeadroom: DWORD ):HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetHeadroom'+
         #13#10'('+
         #13#10'   pThis                     : $%.08X'+
         #13#10'   dwHeadroom                : $%.08X'+
         #13#10');',
         [pThis, dwHeadroom]);
{$ENDIF}

  // Cxbx TODO: Actually implement this

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetLoopRegion(pThis: PX_CDirectSoundBuffer; dwLoopStart: DWORD; dwLoopLength: DWORD) : HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetLoopRegion'+
         #13#10'('+
         #13#10'   pThis                     : $%.08X'+
         #13#10'   dwLoopStart               : $%.08X'+
         #13#10'   dwLoopLength              : $%.08X'+
         #13#10');',
         [pThis, dwLoopStart, dwLoopLength]);
{$ENDIF}

  // Cxbx TODO: Ensure that 4627 & 4361 are intercepting far enough back
  // (otherwise pThis is manipulated!)

  //EmuResizeIDirectSoundBuffer8(pThis, dwLoopLength);

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_Release(pThis: PX_CDirectSoundBuffer): ULONG;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  uRet: ULONG;
  v: Integer;
begin
  EmuSwapFS(fsWindows);


{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_Release'+
         #13#10'('+
         #13#10'   pThis                     : $%.08X'+
         #13#10');',
         [pThis]);
{$ENDIF}

  uRet := 0;

  if Assigned(pThis) then
  begin
    uRet := pThis.EmuDirectSoundBuffer8._Release();

    if (uRet = 0) then
    begin
      // remove cache entry
      for v := 0 to SOUNDBUFFER_CACHE_SIZE - 1 do
      begin
          if (g_pDSoundBufferCache[v] = pThis) then
              g_pDSoundBufferCache[v] := nil;
       end;

      if (pThis.EmuBufferDesc.lpwfxFormat <> nil) then
          CxbxFree(pThis.EmuBufferDesc.lpwfxFormat);

      CxbxFree(pThis.EmuBufferDesc);

      dispose(pThis);
    end;
  end;

  EmuSwapFS(fsXbox);

  Result := uRet;
end;

function XTL_EmuIDirectSoundBuffer8_SetPitch(pThis: PX_CDirectSoundBuffer; lPitch: LongInt): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetPitch'+
         #13#10'('+
         #13#10'   pThis                     : $%.08X'+
         #13#10'   lPitch                    : $%.08X'+
         #13#10');',
         [pThis, lPitch]);
{$ENDIF}

  // Cxbx TODO: Translate params, then make the PC DirectSound call
  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_GetStatus(pThis: PX_CDirectSoundBuffer; pdwStatus: LPDWORD) : HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_GetStatus'+
         #13#10'('+
         #13#10'   pThis                     : $%.08X'+
         #13#10'   pdwStatus                 : $%.08X'+
         #13#10');',
         [pThis, pdwStatus]);
{$ENDIF}

  hRet := DS_OK;

  if Assigned(pThis) and not Assigned(pThis.EmuBuffer) then
  begin
    hRet := pThis.EmuDirectSoundBuffer8.GetStatus(pdwStatus^);
  end
  else
  begin
    pdwStatus^ := 0;
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuIDirectSoundBuffer8_SetCurrentPosition(pThis: PX_CDirectSoundBuffer; dwNewPosition: DWORD): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetCurrentPosition'+
         #13#10'('+
         #13#10'   pThis                     : $%.08X'+
         #13#10'   dwNewPosition             : $%.08X'+
         #13#10');',
         [pThis, dwNewPosition]);
{$ENDIF}

  // NOTE: Cxbx TODO: This call *will* (by MSDN) fail on primary buffers!
  hRet := pThis.EmuDirectSoundBuffer8.SetCurrentPosition(dwNewPosition);

  if (FAILED(hRet)) then
      EmuWarning('SetCurrentPosition Failed!');

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuIDirectSoundBuffer8_GetCurrentPosition(pThis: PX_CDirectSoundBuffer; pdwCurrentPlayCursor: PDWORD; pdwCurrentWriteCursor: PDWORD): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_GetCurrentPosition'+
         #13#10'('+
         #13#10'   pThis                     : 0x%.08X'+
         #13#10'   pdwCurrentPlayCursor      : 0x%.08X'+
         #13#10'   pdwCurrentWriteCursor     : 0x%.08X'+
         #13#10');',
         [pThis, pdwCurrentPlayCursor, pdwCurrentWriteCursor]);
{$ENDIF}

  HackUpdateSoundBuffers();
  HackUpdateSoundStreams();

  // NOTE: Cxbx TODO: This call always seems to fail on primary buffers!
  hRet := pThis.EmuDirectSoundBuffer8.GetCurrentPosition(pdwCurrentPlayCursor, pdwCurrentWriteCursor);

  if (FAILED(hRet)) then
    EmuWarning('GetCurrentPosition Failed!');

  if Assigned(pdwCurrentPlayCursor) and Assigned(pdwCurrentWriteCursor) then
  begin
{$IFDEF DEBUG}
    DbgPrintf('*pdwCurrentPlayCursor := %d, *pdwCurrentWriteCursor := %d', [pdwCurrentPlayCursor^, pdwCurrentWriteCursor^]);
{$ENDIF}
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuIDirectSoundBuffer8_Play(pThis: PX_CDirectSoundBuffer; dwReserved1: DWORD; dwReserved2: DWORD; dwFlags: DWORD): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_Play'+
         #13#10'('+
         #13#10'   pThis                     : 0x%.08X'+
         #13#10'   dwReserved1               : 0x%.08X'+
         #13#10'   dwReserved2               : 0x%.08X'+
         #13#10'   dwFlags                   : 0x%.08X'+
         #13#10');',
         [pThis, dwReserved1, dwReserved2, dwFlags]);
{$ENDIF}
  if (dwFlags and (not DSBPLAY_LOOPING)) > 0 then
      CxbxKrnlCleanup('Unsupported Playing Flags');

  HackUpdateSoundBuffers();

  // close any existing locks
  if Assigned(pThis.EmuLockPtr1) then
  begin
    pThis.EmuDirectSoundBuffer8.Unlock
    (
      pThis.EmuLockPtr1,
      pThis.EmuLockBytes1,
      pThis.EmuLockPtr2,
      pThis.EmuLockBytes2
    );

    pThis.EmuLockPtr1 := nil;
  end;

  if (pThis.EmuFlags and DSB_FLAG_ADPCM) > 0 then
  begin
    hRet := S_OK;
  end
  else
  begin
    hRet := pThis.EmuDirectSoundBuffer8.Play(0, 0, dwFlags);
  end;

  pThis.EmuPlayFlags := dwFlags;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuIDirectSoundBuffer8_Stop(pThis: PX_CDirectSoundBuffer): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_Stop'+
         #13#10'('+
         #13#10'   pThis                     : 0x%.08X'+
         #13#10');',
         [pThis]);
{$ENDIF}

  hRet := pThis.EmuDirectSoundBuffer8.Stop();

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuIDirectSoundBuffer8_StopEx(pBuffer: PX_CDirectSoundBuffer; rtTimeStamp: REFERENCE_TIME; dwFlags: DWORD): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_StopEx'+
         #13#10'('+
         #13#10'   pBuffer                   : 0x%.08X'+
         #13#10'   rtTimeStamp               : 0x%.08X'+
         #13#10'   dwFlags                   : 0x%.08X'+
         #13#10');',
         [pBuffer, rtTimeStamp, dwFlags]);
{$ENDIF}

  if not Assigned(pBuffer.EmuDirectSoundBuffer8) then
      EmuWarning('pBuffer.EmuDirectSoundBuffer8 := 0');

  EmuWarning('StopEx not yet implemented!');

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetVolume(pThis: PX_CDirectSoundBuffer; lVolume: LongInt): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetVolume'+
         #13#10'('+
         #13#10'   pThis                     : 0x%.08X'+
         #13#10'   lVolume                   : 0x%.08X'+
         #13#10');',
         [pThis, lVolume]);
{$ENDIF}

  // Cxbx TODO: Ensure that 4627 & 4361 are intercepting far enough back
  // (otherwise pThis is manipulated!)

//    HRESULT hRet = pThis.EmuDirectSoundBuffer8.SetVolume(lVolume);

  EmuSwapFS(fsXbox);

//    return hRet;
  Result := S_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetFrequency(pThis: PX_CDirectSoundBuffer; dwFrequency: DWORD): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetFrequency'+
         #13#10'('+
         #13#10'   pThis                     : 0x%.08X'+
         #13#10'   dwFrequency               : 0x%.08X'+
         #13#10');',
         [pThis, dwFrequency]);
{$ENDIF}

//    HRESULT hRet = pThis.EmuDirectSoundBuffer8.SetFrequency(dwFrequency);

  EmuSwapFS(fsXbox);

//    return hRet;
  Result := S_OK;
end;

function XTL_EmuDirectSoundCreateStream
(
    pdssd: PX_DSSTREAMDESC;
    ppStream: PPX_CDirectSoundStream
): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  pDSBufferDesc: DirectSound.PDSBUFFERDESC;
  dwAcceptableMask: DWORD;
  hRet: HRESULT;
  v: integer;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuDirectSoundCreateStream'+
         #13#10'('+
         #13#10'   pdssd                     : 0x%.08X'+
         #13#10'   ppStream                  : 0x%.08X'+
         #13#10');',
         [pdssd, ppStream]);
{$ENDIF}

  // Cxbx TODO: Garbage Collection
  new(ppStream);

  pDSBufferDesc := DirectSound.PDSBUFFERDESC(CxbxMalloc(SizeOf(DSBUFFERDESC)));

  // convert from Xbox to PC DSound
  begin
    dwAcceptableMask := $00000010; // Cxbx TODO: Note 0x00040000 is being ignored (DSSTREAMCAPS_LOCDEFER)

    if (pdssd.dwFlags and (not dwAcceptableMask)) > 0 then
        EmuWarning('Use of unsupported pdssd.dwFlags mask(s) (0x%.08X)', [pdssd.dwFlags and (not dwAcceptableMask)]);

    pDSBufferDesc.dwSize := SizeOf(DSBUFFERDESC);
// MERKED OUT CXBX        pDSBufferDesc.dwFlags = (pdssd.dwFlags and dwAcceptableMask) or DSBCAPS_CTRLVOLUME or DSBCAPS_GETCURRENTPOSITION2;
    pDSBufferDesc.dwFlags := DSBCAPS_CTRLVOLUME;
    pDSBufferDesc.dwBufferBytes := DSBSIZE_MIN;

    pDSBufferDesc.dwReserved := 0;

    if Assigned(pdssd.lpwfxFormat) then
    begin
        pDSBufferDesc.lpwfxFormat := CxbxMalloc(SizeOf(TWAVEFORMATEX));
        memcpy(pDSBufferDesc.lpwfxFormat, pdssd.lpwfxFormat, SizeOf(TWAVEFORMATEX));
    end;

    pDSBufferDesc.guid3DAlgorithm := DS3DALG_DEFAULT;

    if Assigned(pDSBufferDesc.lpwfxFormat) and (pDSBufferDesc.lpwfxFormat.wFormatTag <> WAVE_FORMAT_PCM) then
    begin
      EmuWarning('Invalid WAVE_FORMAT!');
      if (pDSBufferDesc.lpwfxFormat.wFormatTag = WAVE_FORMAT_XBOX_ADPCM) then
        EmuWarning('WAVE_FORMAT_XBOX_ADPCM Unsupported!');

      ppStream^.EmuDirectSoundBuffer8 := nil;

      EmuSwapFS(fsXbox);

      Result := DS_OK;
      Exit;
    end;

    // we only support 2 channels right now
    if (pDSBufferDesc.lpwfxFormat.nChannels > 2) then
    begin
      pDSBufferDesc.lpwfxFormat.nChannels := 2;
      pDSBufferDesc.lpwfxFormat.nBlockAlign := (2*pDSBufferDesc.lpwfxFormat.wBitsPerSample) div 8;
      pDSBufferDesc.lpwfxFormat.nAvgBytesPerSec := pDSBufferDesc.lpwfxFormat.nSamplesPerSec * pDSBufferDesc.lpwfxFormat.nBlockAlign;
    end;
  end;

  ppStream^.EmuBuffer := nil;
  ppStream^.EmuBufferDesc := pDSBufferDesc;
  ppStream^.EmuLockPtr1 := nil;
  ppStream^.EmuLockBytes1 := 0;
  ppStream^.EmuLockPtr2 := nil;
  ppStream^.EmuLockBytes2 := 0;

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuDirectSoundCreateStream, *ppStream := 0x%.08X', [ppStream^]);
{$ENDIF}

  hRet := g_pDSound8.CreateSoundBuffer(pDSBufferDesc^, ppStream^.EmuDirectSoundBuffer8, nil);

  if (FAILED(hRet)) then
    EmuWarning('CreateSoundBuffer Failed!');

  // cache this sound stream
  begin
    for v := 0 to SOUNDSTREAM_CACHE_SIZE - 1 do
    begin
      if not Assigned(g_pDSoundStreamCache[v]) then
      begin
        g_pDSoundStreamCache[v] := @ppStream;
        break;
      end;
    end;

    if (v = SOUNDSTREAM_CACHE_SIZE) then
        CxbxKrnlCleanup('SoundStream cache out of slots!');
  end;

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuIDirectSound8_CreateStream
(
    pThis: LPDIRECTSOUND8;
    pdssd: PX_DSSTREAMDESC;
    ppStream: PPX_CDirectSoundStream;
    pUnknown: PVOID
): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
    EmuSwapFS(fsWindows);
{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSound8_CreateStream'+
           #13#10'('+
           #13#10'   pThis                     : 0x%.08X'+
           #13#10'   pdssd                     : 0x%.08X'+
           #13#10'   ppStream                  : 0x%.08X'+
           #13#10'   pUnknown                  : 0x%.08X'+
           #13#10');',
           [pThis, pdssd, ppStream, pUnknown]);
{$ENDIF}
    EmuSwapFS(fsXbox);
  end;
  {$endif}

  XTL_EmuDirectSoundCreateStream(pdssd, ppStream);

  Result := DS_OK;
end;

procedure XTL_EmuCMcpxStream_Dummy_0x10(dwDummy1: DWORD; dwDummy2: DWORD);
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuWarning('EmuCMcpxStream_Dummy_0x10 is ignored!');
end;

function XTL_EmuCDirectSoundStream_SetVolume(pThis: X_CDirectSoundStream; lVolume: LongInt): ULONG;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetVolume'+
         #13#10'('+
         #13#10'   pThis                     : 0x%.08X'+
         #13#10'   lVolume                   : %d'+
         #13#10');',
         [pThis, lVolume]);
{$ENDIF}

  // Cxbx TODO: Actually SetVolume

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuCDirectSoundStream_SetRolloffFactor
(
    pThis: X_CDirectSoundStream;
    fRolloffFactor: FLOAT;
    dwApply: DWORD
): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetRolloffFactor'+
           #13#10'('+
           #13#10'   pThis                     : 0x%.08X'+
           #13#10'   fRolloffFactor            : %f'+
           #13#10'   dwApply                   : 0x%.08X'+
           #13#10');',
           [pThis, fRolloffFactor, dwApply]);
{$ENDIF}

    // Cxbx TODO: Actually SetRolloffFactor

    EmuSwapFS(fsXbox);

    Result := DS_OK;
end;

function XTL_EmuCDirectSoundStream_AddRef(pThis: X_CDirectSoundStream): ULONG;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_AddRef'+
         #13#10'('+
         #13#10'   pThis                     : 0x%.08X'+
         #13#10');',
         [pThis]);
{$ENDIF}

  if Assigned(pThis) then
    pThis.EmuDirectSoundBuffer8._AddRef();

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuCDirectSoundStream_Release(pThis: PX_CDirectSoundStream): ULONG;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  uRet: ULONG;
  v: Integer;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_Release'+
         #13#10'('+
         #13#10'   pThis                     : 0x%.08X'+
         #13#10');',
         [pThis]);
{$ENDIF}

  uRet := 0;

  if Assigned(pThis) and Assigned(pThis.EmuDirectSoundBuffer8) then
  begin
    uRet := pThis.EmuDirectSoundBuffer8._Release();

    if (uRet = 0) then
    begin
        // remove cache entry
        for v := 0 to SOUNDSTREAM_CACHE_SIZE - 1 do
        begin
            if (g_pDSoundStreamCache[v] = pThis^) then
                g_pDSoundStreamCache[v] := nil;
        end;

        if Assigned(pThis.EmuBufferDesc.lpwfxFormat) then
            CxbxFree(pThis.EmuBufferDesc.lpwfxFormat);

        CxbxFree(pThis.EmuBufferDesc);

        Dispose(pThis);
     end;
  end;

  EmuSwapFS(fsXbox);

  Result := uRet;
end;

function XTL_EmuCDirectSoundStream_GetStatus
(
    pThis: X_CDirectSoundStream;
    pdwStatus: PDWORD
): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_GetStatus'+
         #13#10'('+
         #13#10'   pThis                     : 0x%.08X'+
         #13#10'   pdwStatus                 : 0x%.08X'+
         #13#10');',
         [pThis, pdwStatus]);
{$ENDIF}

  EmuWarning('EmuCDirectSoundStream_GetStatus is not yet implemented');

  pdwStatus^ := DSBSTATUS_PLAYING;

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuCDirectSoundStream_Process
(
    pThis: X_CDirectSoundStream;
    pInputBuffer: PXMEDIAPACKET;
    pOutputBuffer: PXMEDIAPACKET
): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_Process'+
         #13#10'('+
         #13#10'   pThis                     : 0x%.08X'+
         #13#10'   pInputBuffer              : 0x%.08X'+
         #13#10'   pOutputBuffer             : 0x%.08X'+
         #13#10');',
         [pThis, pInputBuffer, pOutputBuffer]);
{$ENDIF}

  if Assigned(pThis.EmuDirectSoundBuffer8) then
  begin
    // update buffer data cache
    pThis.EmuBuffer := pInputBuffer.pvBuffer;

    EmuResizeIDirectSoundStream8(pThis, pInputBuffer.dwMaxSize);

    if Assigned(pInputBuffer.pdwStatus) then
        pInputBuffer.pdwStatus^ := S_OK;

    HackUpdateSoundStreams();
  end
  else
  begin
    if Assigned(pInputBuffer.pdwStatus) then
      pInputBuffer.pdwStatus^ := S_OK;
  end;

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuCDirectSoundStream_Discontinuity(pThis: X_CDirectSoundStream): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuCDirectSoundStream_Discontinuity'+
           #13#10'('+
           #13#10'   pThis                     : 0x%.08X'+
           #13#10');',
           [pThis]);
{$ENDIF}

    // Cxbx TODO: Actually Process

    EmuSwapFS(fsXbox);

    Result := DS_OK;
end;


function XTL_EmuCDirectSoundStream_Flush(pThis: X_CDirectSoundStream): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
   DbgPrintf('EmuDSound : EmuCDirectSoundStream_Flush();',[pThis]);
{$ENDIF}

    // Cxbx TODO: Actually Flush

    EmuSwapFS(fsXbox);

    Result := DS_OK;
end;

function XTL_EmuCDirectSound_SynchPlayback(pUnknown: PVOID): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSound_SynchPlayback (0x%.08X);', [pUnknown]);
{$ENDIF}

  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function XTL_EmuCDirectSoundStream_Pause(pStream: PVOID; dwPause: DWORD): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_Pause'+
         #13#10'('+
         #13#10'   pStream                   : 0x%.08X'+
         #13#10'   dwPause                   : 0x%.08X'+
         #13#10');',
         [pStream, dwPause]);
{$ENDIF}

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuIDirectSoundStream_SetHeadroom(pThis: PVOID; dwHeadroom: DWORD): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundStream_SetHeadroom'+
         #13#10'('+
         #13#10'   pThis                     : 0x%.08X'+
         #13#10'   dwHeadroom                : 0x%.08X'+
         #13#10');',
         [pThis, dwHeadroom]);
{$ENDIF}

  // Cxbx TODO: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuCDirectSoundStream_SetConeAngles(pThis: PVOID; dwInsideConeAngle: DWORD; dwOutsideConeAngle: DWORD; dwApply: DWORD): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetConeAngles'+
         #13#10'('+
         #13#10'   pThis                     : 0x%.08X'+
         #13#10'   dwInsideConeAngle         : 0x%.08X'+
         #13#10'   dwOutsideConeAngle        : 0x%.08X'+
         #13#10'   dwApply                   : 0x%.08X'+
         #13#10');',
         [pThis, dwInsideConeAngle, dwOutsideConeAngle, dwApply]);
{$ENDIF}

  // Cxbx TODO: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuCDirectSoundStream_SetConeOutsideVolume(pThis: PVOID; lConeOutsideVolume: LongInt; dwApply: DWORD): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetConeOutsideVolume'+
         #13#10'('+
         #13#10'   pThis                     : 0x%.08X'+
         #13#10'   lConeOutsideVolume        : %d'+
         #13#10'   dwApply                   : 0x%.08X'+
         #13#10');',
         [pThis, lConeOutsideVolume, dwApply]);
{$ENDIF}

  // Cxbx TODO: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuCDirectSoundStream_SetAllParameters(pThis: PVOID; pUnknown: PVOID; dwApply: DWORD): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetAllParameters'+
         #13#10'('+
         #13#10'   pThis                     : 0x%.08X'+
         #13#10'   pUnknown                  : %f'+
         #13#10'   dwApply                   : 0x%.08X'+
         #13#10');',
         [pThis, pUnknown, dwApply]);
{$ENDIF}

  // Cxbx TODO: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuCDirectSoundStream_SetMaxDistance(pThis: PVOID; fMaxDistance: D3DVALUE; dwApply: DWORD): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetMaxDistance'+
         #13#10'('+
         #13#10'   pThis                     : 0x%.08X'+
         #13#10'   fMaxDistance              : %f'+
         #13#10'   dwApply                   : 0x%.08X'+
         #13#10');',
         [pThis, fMaxDistance, dwApply]);
{$ENDIF}

  // Cxbx TODO: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuCDirectSoundStream_SetMinDistance(pThis: PVOID; fMinDistance: D3DVALUE; dwApply: DWORD): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetMinDistance'+
         #13#10'('+
         #13#10'   pThis                     : 0x%.08X'+
         #13#10'   fMinDistance              : %f'+
         #13#10'   dwApply                   : 0x%.08X'+
         #13#10');',
         [pThis, fMinDistance, dwApply]);
{$ENDIF}

  // Cxbx TODO: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuCDirectSoundStream_SetVelocity(pThis: PVOID; x: D3DVALUE; y: D3DVALUE; z: D3DVALUE; dwApply: DWORD): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetVelocity'+
         #13#10'('+
         #13#10'   pThis                     : 0x%.08X'+
         #13#10'   x                         : %f'+
         #13#10'   y                         : %f'+
         #13#10'   z                         : %f'+
         #13#10'   dwApply                   : 0x%.08X'+
         #13#10');',
         [pThis, x, y, z, dwApply]);
{$ENDIF}

  // Cxbx TODO: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuCDirectSoundStream_SetConeOrientation(pThis: PVOID; x: D3DVALUE; y: D3DVALUE; z: D3DVALUE; dwApply: DWORD): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetConeOrientation'+
         #13#10'('+
         #13#10'   pThis                     : 0x%.08X'+
         #13#10'   x                         : %f'+
         #13#10'   y                         : %f'+
         #13#10'   z                         : %f'+
         #13#10'   dwApply                   : 0x%.08X'+
         #13#10');',
         [pThis, x, y, z, dwApply]);
{$ENDIF}

  // Cxbx TODO: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuCDirectSoundStream_SetPosition(pThis: PVOID; x: D3DVALUE; y: D3DVALUE; z: D3DVALUE; dwApply: DWORD): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetPosition'+
         #13#10'('+
         #13#10'   pThis                     : 0x%.08X'+
         #13#10'   x                         : %f'+
         #13#10'   y                         : %f'+
         #13#10'   z                         : %f'+
         #13#10'   dwApply                   : 0x%.08X'+
         #13#10');',
         [pThis, x, y, z, dwApply]);
{$ENDIF}

  // Cxbx TODO: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuCDirectSoundStream_SetFrequency(pThis: PVOID; dwFrequency: DWORD): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetFrequency'+
         #13#10'('+
         #13#10'   pThis                     : 0x%.08X'+
         #13#10'   dwFrequency               : %d'+
         #13#10');',
         [pThis, dwFrequency]);
{$ENDIF}

  // Cxbx TODO: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuIDirectSoundStream_SetI3DL2Source(pThis: PVOID; pds3db: PVOID; dwApply: DWORD): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundStream_SetI3DL2Source'+
         #13#10'('+
         #13#10'   pThis                     : 0x%.08X'+
         #13#10'   pds3db                    : 0x%.08X'+
         #13#10'   dwApply                   : 0x%.08X'+
         #13#10');',
         [pThis, pds3db, dwApply]);
{$ENDIF}

  // Cxbx TODO: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuIDirectSoundStream_Unknown1(pThis: PVOID; dwUnknown1: DWORD): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundStream_Unknown1' +
         #13#10'(' +
         #13#10'   pThis                     : 0x%.08X' +
         #13#10'   dwUnknown1                : 0x%.08X' +
         #13#10');',
         [pThis, dwUnknown1]);
{$ENDIF}

  // Cxbx TODO: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetMaxDistance
(
    pThis: IDIRECTSOUNDBUFFER8;
    flMaxDistance: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
      EmuSwapFS(fsWindows);
{$IFDEF DEBUG}
      DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetMaxDistance'+
             #13#10'('+
             #13#10'   pThis                     : 0x%.08X'+
             #13#10'   flMaxDistance             : %f'+
             #13#10'   dwApply                   : 0x%.08X'+
             #13#10');',
             [pThis, flMaxDistance, dwApply]);
{$ENDIF}
      EmuSwapFS(fsXbox);
   end;
  {$endif}

  // Cxbx TODO: Actually do something

  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetMinDistance
(
    pThis: IDIRECTSOUNDBUFFER8;
    flMinDistance: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
      EmuSwapFS(fsWindows);
{$IFDEF DEBUG}
      DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetMinDistance'+
             #13#10'('+
             #13#10'   pThis                     : 0x%.08X'+
             #13#10'   flMinDistance             : %f'+
             #13#10'   dwApply                   : 0x%.08X'+
             #13#10');',
             [pThis, flMinDistance, dwApply]);
{$ENDIF}
      EmuSwapFS(fsXbox);
   end;
  {$endif}

  // Cxbx TODO: Actually do something

  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetRolloffFactor
(
    pThis: IDIRECTSOUNDBUFFER8;
    flRolloffFactor: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
      EmuSwapFS(fsWindows);
{$IFDEF DEBUG}
      DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetRolloffFactor'+
             #13#10'('+
             #13#10'   pThis                     : 0x%.08X'+
             #13#10'   flRolloffFactor           : %f'+
             #13#10'   dwApply                   : 0x%.08X'+
             #13#10');',
             [pThis, flRolloffFactor, dwApply]);
{$ENDIF}
      EmuSwapFS(fsXbox);
   end;
  {$endif}

  // Cxbx TODO: Actually do something

  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetDistanceFactor
(
    pThis: IDIRECTSOUNDBUFFER8;
    flDistanceFactor: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
      EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
      DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetDistanceFactor'+
             #13#10'('+
             #13#10'   pThis                     : 0x%.08X'+
             #13#10'   flDistanceFactor          : %f'+
             #13#10'   dwApply                   : 0x%.08X'+
             #13#10');',
             [pThis, flDistanceFactor, dwApply]);
{$ENDIF}
      EmuSwapFS(fsXbox);
   end;
  {$endif}

  // Cxbx TODO: Actually do something

  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetConeAngles
(
    pThis: IDIRECTSOUNDBUFFER8;
    dwInsideConeAngle: DWORD;
    dwOutsideConeAngle: DWORD;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
      EmuSwapFS(fsWindows);
{$IFDEF DEBUG}
      DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetConeAngles'+
             #13#10'('+
             #13#10'   pThis                     : 0x%.08X'+
             #13#10'   dwInsideConeAngle         : 0x%.08X'+
             #13#10'   dwOutsideConeAngle        : 0x%.08X'+
             #13#10'   dwApply                   : 0x%.08X'+
             #13#10');',
             [pThis, dwInsideConeAngle,
             dwOutsideConeAngle, dwApply]);
{$ENDIF}
      EmuSwapFS(fsXbox);
   end;
  {$endif}

  // Cxbx TODO: Actually do something

  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetConeOrientation
(
    pThis: IDIRECTSOUNDBUFFER8;
    x: FLOAT;
    y: FLOAT;
    z: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
      EmuSwapFS(fsWindows);
{$IFDEF DEBUG}
      DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetConeOrientation'+
             #13#10'('+
             #13#10'   pThis                     : 0x%.08X'+
             #13#10'   x                         : %f'+
             #13#10'   y                         : %f'+
             #13#10'   z                         : %f'+
             #13#10'   dwApply                   : 0x%.08X'+
             #13#10');',
             [pThis, x, y, z, dwApply]);
{$ENDIF}
      EmuSwapFS(fsXbox);
   end;
  {$endif}

  // Cxbx TODO: Actually do something

  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetConeOutsideVolume
(
    pThis: IDIRECTSOUNDBUFFER8;
    lConeOutsideVolume: LongInt;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
      EmuSwapFS(fsWindows);
{$IFDEF DEBUG}
      DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetConeOutsideVolume'+
             #13#10'('+
             #13#10'   pThis                     : 0x%.08X'+
             #13#10'   lConeOutsideVolume        : 0x%.08X'+
             #13#10'   dwApply                   : 0x%.08X'+
             #13#10');',
             [pThis, lConeOutsideVolume, dwApply]);
{$ENDIF}
      EmuSwapFS(fsXbox);
   end;
  {$endif}

  // Cxbx TODO: Actually do something

  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetPosition
(
    pThis: IDIRECTSOUNDBUFFER8;
    x: FLOAT;
    y: FLOAT;
    z: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
      EmuSwapFS(fsWindows);
{$IFDEF DEBUG}
      DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetPosition'+
             #13#10'('+
             #13#10'   pThis                     : 0x%.08X'+
             #13#10'   x                         : %f'+
             #13#10'   y                         : %f'+
             #13#10'   z                         : %f'+
             #13#10'   dwApply                   : 0x%.08X'+
             #13#10');',
             [pThis, x, y, z, dwApply]);
{$ENDIF}
      EmuSwapFS(fsXbox);
   end;
  {$endif}

  // Cxbx TODO: Actually do something

  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetVelocity
(
    pThis: IDIRECTSOUNDBUFFER8;
    x: FLOAT;
    y: FLOAT;
    z: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
      EmuSwapFS(fsWindows);
{$IFDEF DEBUG}
      DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetVelocity'+
             #13#10'('+
             #13#10'   pThis                     : 0x%.08X'+
             #13#10'   x                         : %f'+
             #13#10'   y                         : %f'+
             #13#10'   z                         : %f'+
             #13#10'   dwApply                   : 0x%.08X'+
             #13#10');',
             [pThis, x, y, z, dwApply]);
{$ENDIF}
      EmuSwapFS(fsXbox);
   end;
  {$endif}

  // Cxbx TODO: Actually do something

  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetDopplerFactor
(
    pThis: IDIRECTSOUNDBUFFER8;
    flDopplerFactor: FLOAT;
    dwApply: DWORD
): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
      EmuSwapFS(fsWindows);
{$IFDEF DEBUG}
      DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetConeOutsideVolume'+
             #13#10'('+
             #13#10'   pThis                     : 0x%.08X'+
             #13#10'   flDopplerFactor           : %f'+
             #13#10'   dwApply                   : 0x%.08X'+
             #13#10');',
             [pThis, flDopplerFactor, dwApply]);
{$ENDIF}
      EmuSwapFS(fsXbox);
   end;
  {$endif}

  // Cxbx TODO: Actually do something

  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetI3DL2Source
(
    pThis: IDIRECTSOUNDBUFFER8;
    pds3db: LPCDSI3DL2BUFFER;
    dwApply: DWORD
): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
    EmuSwapFS(fsWindows);
{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetI3DL2Source'+
           #13#10'('+
           #13#10'   pThis                     : 0x%.08X'+
           #13#10'   pds3db                    : 0x%.08X'+
           #13#10'   dwApply                   : 0x%.08X'+
           #13#10');',
           [pThis, pds3db, dwApply]);
{$ENDIF}
    EmuSwapFS(fsXbox);
  end;
  {$endif}

  // Cxbx TODO: Actually do something

  Result := DS_OK;
end;


(*function XTL_EmuIDirectSoundBuffer8_SetFormat
(
    pBuffer: PX_CDirectSoundBuffer;
    pwfxFormat: LPCWAVEFORMATEX;
): HRESULT;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
    EmuSwapFS(fsWindows);

    // debug trace
    #ifdef _DEBUG_TRACE
    begin
{$IFDEF DEBUG}
        printf('EmuDSound : EmuIDirectSoundBuffer8_SetFormat'
               #13#10'('
               #13#10'   pBuffer                   : 0x%.08X'
               #13#10'   pwfxFormat                : 0x%.08X'
               #13#10');',
               [pBuffer,pwfxFormat);
{$ENDIF}
     end;
    //endif

    HRESULT hRet := DS_OK;

    EmuSwapFS(fsXbox);

    Result := hRet;
end;
*)

procedure XTL_EmuDirectSoundUseFullHRTF;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuDirectSoundUseFullHRTF()');
{$ENDIF}

    // Cxbx TODO: Actually implement this

    EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirectSoundBuffer8_SetLFO
(
  pThis: IDIRECTSOUNDBUFFER;
  pLFODesc: LPCDSLFODESC
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetLFO'+
         #13#10'('+
         #13#10'   pThis                     : 0x%.08X'+
         #13#10'   pLFODesc                  : 0x%.08X'+
         #13#10');',
         [pThis, pLFODesc]);
{$ENDIF}

  // Cxbx TODO: Implement
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

procedure XTL_EmuXAudioCreateAdpcmFormat
(
  nChannels: WORD;
  nSamplesPerSec: DWORD;
  pwfx: LPXBOXADPCMWAVEFORMAT
); stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
    EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuXAudioCreateAdpcmFormat'+
           #13#10'('+
           #13#10'   nChannels                 : 0x%.04X'+
           #13#10'   nSamplesPerSec            : 0x%.08X'+
           #13#10'   pwfx                      : 0x%.08X'+
           #13#10');',
           [nChannels, nSamplesPerSec, pwfx]);
{$ENDIF}

  // Fill out the pwfx structure with the appropriate data
  pwfx.wfx.wFormatTag    := WAVE_FORMAT_XBOX_ADPCM;
  pwfx.wfx.nChannels      := nChannels;
  pwfx.wfx.nSamplesPerSec  := nSamplesPerSec;
  pwfx.wfx.nAvgBytesPerSec  := (nSamplesPerSec*nChannels * 36) div 64;
  pwfx.wfx.nBlockAlign    := nChannels * 36;
  pwfx.wfx.wBitsPerSample  := 4;
  pwfx.wfx.cbSize      := 2;
  pwfx.wSamplesPerBlock    := 64;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirectSoundBuffer8_SetRolloffCurve
(
  pThis: IDIRECTSOUNDBUFFER;
  pflPoints: PFLOAT;
  dwPointCount: DWORD;
  dwApply: DWORD
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetRolloffCurve'+
       #13#10'('+
       #13#10'   pThis                     : 0x%.08X'+
       #13#10'   pflPoints                 : 0x%.08X'+
       #13#10'   dwPointCount              : 0x%.08X'+
       #13#10'   dwApply                   : 0x%.08X'+
       #13#10');',
       [pThis, pflPoints, dwPointCount, dwApply]);
{$ENDIF}

  // Cxbx TODO: Implement

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;


(*function XTL_EmuIDirectSoundStream_SetVolume(
  pStream: LPDIRECTSOUNDSTREAM;
  lVolume: LongInt
): HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundStream_SetVolume' +
           #13#10'(' +
           #13#10'   pStream                   : 0x%.08X' +
           #13#10'   lVolume                   : 0x%.08X' +
           #13#10');',
           [pStream, lVolume]);
{$ENDIF}

  // Cxbx TODO: Implement

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;
*)

exports
  XTL_EmuDirectSoundCreateBuffer,
  XTL_EmuDirectSoundDoWork,
  XTL_EmuDirectSoundCreate;

end.
