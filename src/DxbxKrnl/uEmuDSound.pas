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

{}implementation{}

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
  , uEmuKrnl
  , uTypes
  , uLog
  , uEmu
  , uEmuAlloc
  , uEmuFS
  , uXboxLibraryUtils
  , uDxbxKrnlUtils
  , uEmuD3D8Types
  ;

// EmuIDirectSoundBuffer8_Play flags
const X_DSBPLAY_LOOPING = $00000001;
const X_DSBPLAY_FROMSTART = $00000002;

// EmuIDirectSoundBuffer8_Pause flags
const X_DSBPAUSE_RESUME = $00000000;
const X_DSBPAUSE_PAUSE = $00000001;
const X_DSBPAUSE_SYNCHPLAYBACK = $00000002;

type
  WAVEFORMATEX = TWAVEFORMATEX;
  LPWAVEFORMATEX = MMSystem.PWaveFormatEx; // alias
  LPCWAVEFORMATEX = MMSystem.PWaveFormatEx;
  LPCDSI3DL2BUFFER = Pvoid;

type X_DSBUFFERDESC = record
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
    dwSize: DWORD;
    dwFlags: DWORD;
    dwBufferBytes: DWORD;
    lpwfxFormat: LPWAVEFORMATEX;
    lpMixBins: LPVOID;      // TODO -oCXBX: Implement
    dwInputMixBin: DWORD;
  end;
  PX_DSBUFFERDESC = ^X_DSBUFFERDESC;

type X_DSSTREAMDESC = record
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
    dwFlags: DWORD;
    dwMaxAttachedPackets: DWORD;
    lpwfxFormat: LPWAVEFORMATEX;
    lpfnCallback: PVOID;   // TODO -oCXBX: Correct Parameter
    lpvContext: LPVOID;
    lpMixBins: PVOID;      // TODO -oCXBX: Correct Parameter
  end;
  PX_DSSTREAMDESC = ^X_DSSTREAMDESC;

type REFERENCE_TIME = LONGLONG;
  PPREFERENCE_TIME = ^REFERENCE_TIME;
  LPREFERENCE_TIME = ^REFERENCE_TIME;

type _XMEDIAPACKET = record
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
    pvBuffer: LPVOID;
    dwMaxSize: DWORD;
    pdwCompletedSize: PDWORD;
    pdwStatus: PDWORD;
    case Integer of // union {
    0: (hCompletionEvent: HANDLE);
    1: (
        pContext: PVOID;
    // end;
      prtTimestamp: PREFERENCE_TIME;
    ); // end of union
  end;
  XMEDIAPACKET = _XMEDIAPACKET;
  PXMEDIAPACKET = ^XMEDIAPACKET;
  LPXMEDIAPACKET = ^XMEDIAPACKET;

type _XMEDIAINFO = record
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
    dwFlags: DWORD;
    dwInputSize: DWORD;
    dwOutputSize: DWORD;
    dwMaxLookahead: DWORD;
end;
XMEDIAINFO = _XMEDIAINFO; PXEIDIAINFO = ^XMEDIAINFO; LPXMEDIAINFO = ^XMEDIAINFO;

// XMEDIAINFO Flags
const XMO_STREAMF_FIXED_SAMPLE_SIZE           = $00000001;      // The object supports only a fixed sample size
const XMO_STREAMF_FIXED_PACKET_ALIGNMENT      = $00000002;      // The object supports only a fixed packet alignment
const XMO_STREAMF_INPUT_ASYNC                 = $00000004;      // The object supports receiving input data asynchronously
const XMO_STREAMF_OUTPUT_ASYNC                = $00000008;      // The object supports providing output data asynchronously
const XMO_STREAMF_IN_PLACE                    = $00000010;      // The object supports in-place modification of data
const XMO_STREAMF_MASK                        = $0000001F;

type X_DSFILTERDESC = record
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
    dwMode: DWORD;
    dwQCoefficient: DWORD;
    adwCoefficients: array [0..4-1] of DWORD;
end;
PX_DSFILTERDESC = ^X_DSFILTERDESC;

// X_DSFILTERDESC modes
const DSFILTER_MODE_BYPASS        = $00000000;      // The filter is bypassed
const DSFILTER_MODE_DLS2          = $00000001;      // DLS2 mode
const DSFILTER_MODE_PARAMEQ       = $00000002;      // Parametric equalizer mode
const DSFILTER_MODE_MULTI         = $00000003;      // Multifunction mode

type _DSLFODESC = record
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
    dwLFO: DWORD;
    dwDelay: DWORD;
    dwDelta: DWORD;
    lPitchModulation: LONG;
    lFilterCutOffRange: LONG;
    lAmplitudeModulation: LONG;
  end;
  DSLFODESC = _DSLFODESC;
  LPCDSLFODESC = ^DSLFODESC;

type xbox_adpcmwaveformat_tag = record
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
    wfx: WAVEFORMATEX;            // WAVEFORMATEX data
    wSamplesPerBlock: WORD;       // Number of samples per encoded block.  It must be 64.
  end;
  XBOXADPCMWAVEFORMAT = xbox_adpcmwaveformat_tag;
  PXBOXADPCMWAVEFORMAT = ^XBOXADPCMWAVEFORMAT;
  LPXBOXADPCMWAVEFORMAT = PXBOXADPCMWAVEFORMAT;

type X_DSOUTPUTLEVELS = record
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
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
  PX_DSOUTPUTLEVELS = ^X_DSOUTPUTLEVELS;

type X_DSCAPS = record
    dwFree2DBuffers: DWORD;
    dwFree3DBuffers: DWORD;
    dwFreeBufferSGEs: DWORD;
    dwMemoryAllocated: DWORD;
end;
PX_DSCAPS = ^X_DSCAPS;

type XTL_PIDirectSoundStream = type PInterface;

type LPDIRECTSOUND = type PInterface;
type LPDIRECTSOUNDSTREAM = XTL_PIDirectSoundStream;

type X_CDirectSound = record
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
    // TODO -oCXBX: Fill this in?
  end;
  PX_CDirectSound = ^X_CDirectSound;

type X_CDirectSoundBuffer = record
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
    UnknownA: array [0..$20-1] of BYTE; // Offset: 0x00
    {union}case Integer of
    0: (
      pMpcxBuffer: PVOID);              // Offset: 0x20
    1: (
      EmuDirectSoundBuffer8: XTL_PIDirectSoundBuffer;
    // endcase; fall through :
    UnknownB: array [0..$0C-1] of BYTE; // Offset: 0x24
    EmuBuffer: PVOID;                   // Offset: 0x30
    EmuBufferDesc: PDSBUFFERDESC;       // Offset: 0x34
    EmuLockPtr1: PVOID;                 // Offset: 0x38
    EmuLockBytes1: DWORD;               // Offset: 0x3C
    EmuLockPtr2: PVOID;                 // Offset: 0x40
    EmuLockBytes2: DWORD;               // Offset: 0x44
    EmuPlayFlags: DWORD;                // Offset: 0x48
    EmuFlags: DWORD;                    // Offset: 0x4C
    EmuListener: XTL_PIDirectSoundListener; // Offset: 0x50 Dxbx addition - to support 3D sound emulation through IDirectSound3DListener
    ); // end of union
  end;
  PX_CDirectSoundBuffer = ^X_CDirectSoundBuffer;
  PPX_CDirectSoundBuffer = ^PX_CDirectSoundBuffer;

  XTL_LPDIRECTSOUNDBUFFER8 = PX_CDirectSoundBuffer; // Dxbx addition : Let code below access X_CDirectSoundBuffer


const DSB_FLAG_ADPCM = $00000001;
const WAVE_FORMAT_XBOX_ADPCM = $0069;
const DSB_FLAG_RECIEVEDATA = $00001000;

type
  X_CDirectSoundStream = class; // forward

  X_CMediaObject = class(TObject) // Cxbx incorrectly calls this X_CMcpxStream
  // Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
  public
    // construct vtable (or grab ptr to existing)
    constructor Create(pParentStream: {*}X_CDirectSoundStream); //begin {pVtbl := @vtbl;} Self.pParentStream := pParentStream; end;

  private
    // Dxbx : 'virtual' creates vtable (cached by each instance, via constructor)
    function AddRef({pThis: PX_CMediaObject}): ULONG; virtual; stdcall;          // VMT 0x00
    function Release({pThis: PX_CMediaObject}): ULONG; virtual; stdcall;         // VMT 0x04

    function GetInfo                                                            // VMT 0x08
    (
        {pThis: PX_CMediaObject;}
        pInfo: LPXMEDIAINFO
    ): HRESULT; virtual; stdcall;

    function GetStatus                                                          // VMT 0x0C
    (
        {pThis: PX_CMediaObject;}
        pdwStatus: PDWORD
    ): HRESULT; virtual; stdcall;

    //
    // TODO -oCXBX: Function needs X_CMediaObject "this" pointer (ecx!)
    //
    function Process                                                            // VMT 0x10
    (
        {pThis: PX_CMediaObject;}
        pInputBuffer: PXMEDIAPACKET;
        pOutputBuffer: PXMEDIAPACKET
    ): HRESULT; virtual; stdcall;

    function Discontinuity({pThis: PX_CMediaObject}): HRESULT; virtual; stdcall; // VMT 0x14

    function Flush({pThis: PX_CMediaObject}): HRESULT; virtual; stdcall;         // VMT 0x18

    // Dxbx : global vtbl for this class...is compiled in automatically by Delphi, so leave it out :
    // vtbl: _vtbl;

  // debug mode guard for detecting naughty data accesses
{$ifdef DEBUG}
  private
    DebugGuard: array[0..256-1] of DWORD;
{$endif}

  public
    pParentStream: X_CDirectSoundStream;
  end;
  PX_CMediaObject = X_CMediaObject; // Dxbx note : Delphi's classes are already pointer-types
  PPX_CMediaObject = ^PX_CMediaObject;

  X_CDirectSoundStream = class(TObject)
  // Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
  public
    // construct vtable (or grab ptr to existing)
    constructor Create(); // begin { pVtbl := @vtbl;} pXMediaObject := X_CMediaObject.Create(Self); end;

  private
    // Dxbx : 'virtual' creates vtable (cached by each instance, via constructor)
    function AddRef({pThis: PX_CDirectSoundStream}): ULONG; virtual; stdcall;          // VMT 0x00
    function Release({pThis: PX_CDirectSoundStream}): ULONG; virtual; stdcall;         // VMT 0x04

    function GetInfo                                                                    // VMT 0x08
    (
        {pThis: PX_CDirectSoundStream;}
        pInfo: LPXMEDIAINFO
    ): HRESULT; virtual; stdcall;

    function GetStatus                                                                  // VMT 0x0C
    (
        {pThis: PX_CDirectSoundStream;}
        pdwStatus: PDWORD
    ): HRESULT; virtual; stdcall;

    function Process                                                                    // VMT 0x10
    (
        {pThis: PX_CDirectSoundStream;}
        pInputBuffer: PXMEDIAPACKET;
        pOutputBuffer: PXMEDIAPACKET
    ): HRESULT; virtual; stdcall;

    function Discontinuity({pThis: PX_CDirectSoundStream}): HRESULT; virtual; stdcall; // VMT 0x14

    function Flush({pThis: PX_CDirectSoundStream}): HRESULT; virtual; stdcall;         // VMT 0x18

    procedure Unknown2; virtual;                                                      // VMT 0x1C - ???
    procedure Unknown3; virtual;                                                      // VMT 0x20 - ???
    procedure Unknown4; virtual;                                                      // VMT 0x24 - ???
    procedure Unknown5; virtual;                                                      // VMT 0x28 - ???
    procedure Unknown6; virtual;                                                      // VMT 0x2C - ???
    procedure Unknown7; virtual;                                                      // VMT 0x30 - ???
    procedure Unknown8; virtual;                                                      // VMT 0x34 - ???
    procedure Unknown9; virtual;                                                      // VMT 0x38 - ???

    // Dxbx : global vtbl for this class...is compiled in automatically by Delphi, so leave it out :
    // vtbl: _vtbl;
  private
    Spacer: array[0..8-1] of DWORD;
    pXMediaObject: PVOID;

    // debug mode guard for detecting naughty data accesses
{$ifdef DEBUG}
    DebugGuard: array[0..256-1] of DWORD;
{$endif}

  public
    // cached data
    EmuDirectSoundBuffer8: XTL_PIDirectSoundBuffer;
    EmuBuffer: PVOID;
    EmuBufferDesc: PDSBUFFERDESC;
    EmuLockPtr1: PVOID;
    EmuLockBytes1: DWORD;
    EmuLockPtr2: PVOID;
    EmuLockBytes2: DWORD;
    EmuPlayFlags: DWORD;
  end;
  PX_CDirectSoundStream = X_CDirectSoundStream; // Dxbx note : Delphi's classes are already pointer-types
  PPX_CDirectSoundStream = ^PX_CDirectSoundStream;


// size of sound buffer cache (used for periodic sound buffer updates)
const SOUNDBUFFER_CACHE_SIZE = $100;

// size of sound stream cache (used for periodic sound stream updates)
const SOUNDSTREAM_CACHE_SIZE = $100;

// Static Variable(s)
var g_pDSound8: XTL_LPDIRECTSOUND8 = NULL;
var g_pDSound8RefCount: int = 0;
var g_pDSoundBufferCache: array [0..SOUNDBUFFER_CACHE_SIZE-1] of PX_CDirectSoundBuffer;
var g_pDSoundStreamCache: array [0..SOUNDSTREAM_CACHE_SIZE-1] of PX_CDirectSoundStream;
var g_bDSoundCreateCalled: Boolean = false; // Dxbx note : Boolean is simpler than Cxbx's int.

{implementation}

function iif(const aValue: Boolean; const aTrue, aFalse: DirectSound.PDSBUFFERDESC): DirectSound.PDSBUFFERDESC; overload;
begin
  if aValue then
    Result := aTrue
  else
    Result := aFalse;
end;

// periodically update sound buffers
procedure HackUpdateSoundBuffers();
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  v: int;
  pAudioPtr, pAudioPtr2: PVOID;
  dwAudioBytes, dwAudioBytes2: DWORD;
  hRet: HRESULT;
begin
  for v := 0 to SOUNDBUFFER_CACHE_SIZE -1 do 
  begin
    if (g_pDSoundBufferCache[v] = nil) or (g_pDSoundBufferCache[v].EmuBuffer = nil) then
      continue;

    // unlock existing lock
    if (g_pDSoundBufferCache[v].EmuLockPtr1 <> nil) then
        IDirectSoundBuffer(g_pDSoundBufferCache[v].EmuDirectSoundBuffer8).Unlock(g_pDSoundBufferCache[v].EmuLockPtr1, g_pDSoundBufferCache[v].EmuLockBytes1, g_pDSoundBufferCache[v].EmuLockPtr2, g_pDSoundBufferCache[v].EmuLockBytes2);

    hRet := IDirectSoundBuffer(g_pDSoundBufferCache[v].EmuDirectSoundBuffer8).Lock(0, g_pDSoundBufferCache[v].EmuBufferDesc.dwBufferBytes, @pAudioPtr, @dwAudioBytes, @pAudioPtr2, @dwAudioBytes2, 0);

    if (SUCCEEDED(hRet)) then
    begin
      if (pAudioPtr <> nil) then
        memcpy(pAudioPtr, g_pDSoundBufferCache[v].EmuBuffer, dwAudioBytes);

      if (pAudioPtr2 <> nil) then
        memcpy(pAudioPtr2, PVOID(DWORD(g_pDSoundBufferCache[v].EmuBuffer)+dwAudioBytes), dwAudioBytes2);

      IDirectSoundBuffer(g_pDSoundBufferCache[v].EmuDirectSoundBuffer8).Unlock(pAudioPtr, dwAudioBytes, pAudioPtr2, dwAudioBytes2);
     end;

    // TODO -oCXBX: relock old lock ??
   end;
end;

// periodically update sound streams
procedure HackUpdateSoundStreams();
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  v: int;
  pAudioPtr, pAudioPtr2: PVOID;
  dwAudioBytes, dwAudioBytes2: DWORD;
  hRet: HRESULT;
begin
  for v := 0 to SOUNDSTREAM_CACHE_SIZE-1 do
  begin
    if (g_pDSoundStreamCache[v] = nil) or (g_pDSoundStreamCache[v].EmuBuffer = NULL) then
      continue;

    hRet := IDirectSoundBuffer(g_pDSoundStreamCache[v].EmuDirectSoundBuffer8).Lock(0, g_pDSoundStreamCache[v].EmuBufferDesc.dwBufferBytes, @pAudioPtr, @dwAudioBytes, @pAudioPtr2, @dwAudioBytes2, 0);

    if (SUCCEEDED(hRet)) then
    begin
      if (pAudioPtr <> nil) then
        memcpy(pAudioPtr,  g_pDSoundStreamCache[v].EmuBuffer, dwAudioBytes);

      if (pAudioPtr2 <> nil) then
        memcpy(pAudioPtr2, PVOID((DWORD(g_pDSoundStreamCache[v].EmuBuffer)+dwAudioBytes)), dwAudioBytes2);

      IDirectSoundBuffer(g_pDSoundStreamCache[v].EmuDirectSoundBuffer8).Unlock(pAudioPtr, dwAudioBytes, pAudioPtr2, dwAudioBytes2);
    end;

    IDirectSoundBuffer(g_pDSoundStreamCache[v].EmuDirectSoundBuffer8).SetCurrentPosition(0);
    IDirectSoundBuffer(g_pDSoundStreamCache[v].EmuDirectSoundBuffer8).Play(0, 0, 0);
  end;
end;

// resize an emulated directsound buffer, if necessary
procedure EmuResizeIDirectSoundBuffer8(pThis: PX_CDirectSoundBuffer; dwBytes: DWORD);
var
  dwPlayCursor: DWORD;
  dwWriteCursor: DWORD;
  dwStatus: DWORD;
  hRet: HRESULT;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  if (dwBytes = pThis.EmuBufferDesc.dwBufferBytes) {or (dwBytes = 0)} then // Dxbx addition : Allow resize to zero
    Exit;

{$IFDEF DEBUG}
  DbgPrintf('EmuResizeIDirectSoundBuffer8 : Resizing! (0x%.08X->0x%.08X)', [pThis.EmuBufferDesc.dwBufferBytes, dwBytes]);
{$ENDIF}

  if Assigned(pThis.EmuDirectSoundBuffer8) then // Dxbx addition : Allow resize from nil
  begin
    hRet := IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8).GetCurrentPosition(@dwPlayCursor, @dwWriteCursor);

    if (FAILED(hRet)) then
      DxbxKrnlCleanup('Unable to retrieve current position for resize reallocation!');

    hRet := IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8).GetStatus({out}dwStatus);

    if (FAILED(hRet)) then
      DxbxKrnlCleanup('Unable to retrieve current status for resize reallocation!');

    // release old buffer
    while(IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8)._Release() > 0) do begin end;
    pThis.EmuDirectSoundBuffer8 := nil; // Dxbx addition : nil out after free
  end;

  pThis.EmuBufferDesc.dwBufferBytes := dwBytes;

  if dwBytes > 0 then // Dxbx addition : Allow resize to zero
  begin
    hRet := IDirectSound8(g_pDSound8).CreateSoundBuffer(pThis.EmuBufferDesc^, PIDirectSoundBuffer(@(pThis.EmuDirectSoundBuffer8)), NULL);

    if (FAILED(hRet)) then
      DxbxKrnlCleanup('IDirectSoundBuffer8 resize Failed!');

    IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8).SetCurrentPosition(dwPlayCursor);

    if (dwStatus and DSBSTATUS_PLAYING) > 0 then
      IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8).Play(0, 0, pThis.EmuPlayFlags);
  end;
end;

// resize an emulated directsound stream, if necessary
procedure EmuResizeIDirectSoundStream8(pThis: PX_CDirectSoundStream; dwBytes: DWORD);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  dwPlayCursor: DWORD;
  dwWriteCursor: DWORD;
  dwStatus: DWORD;
  hRet: HRESULT;
begin
  if (dwBytes = pThis.EmuBufferDesc.dwBufferBytes) then
    Exit;

  if Assigned(pThis.EmuDirectSoundBuffer8) then // Dxbx addition : Allow resize from nil
  begin
    hRet := IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8).GetCurrentPosition(@dwPlayCursor, @dwWriteCursor);

    if (FAILED(hRet)) then
      DxbxKrnlCleanup('Unable to retrieve current position for resize reallocation!');

    hRet := IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8).GetStatus({out}dwStatus);

    if (FAILED(hRet)) then
      DxbxKrnlCleanup('Unable to retrieve current status for resize reallocation!');

    // release old buffer
    while(IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8)._Release() > 0) do begin end;
    pThis.EmuDirectSoundBuffer8 := nil; // Dxbx addition : nil out after free
  end;

  pThis.EmuBufferDesc.dwBufferBytes := dwBytes;

  if dwBytes > 0 then // Dxbx addition : Allow resize to zero
  begin
    hRet := IDirectSound8(g_pDSound8).CreateSoundBuffer(pThis.EmuBufferDesc^, PIDirectSoundBuffer(@(pThis.EmuDirectSoundBuffer8)), NULL);

    if (FAILED(hRet)) then
      DxbxKrnlCleanup('IDirectSoundBuffer8 resize Failed!');

    IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8).SetCurrentPosition(dwPlayCursor);

    if (dwStatus and DSBSTATUS_PLAYING) > 0 then
      IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8).Play(0, 0, pThis.EmuPlayFlags);
  end;
end;


function AssureDirectSoundCreate(const FromOriginalDSoundCreate: Boolean = False): HRESULT;
var
  v: int;
begin
  Result := DS_OK;

  if (nil=g_pDSound8) then
  begin
    if (not FromOriginalDSoundCreate) then
    begin
      if g_bDSoundCreateCalled then
        EmuWarning('Initializing DirectSound pointer even though DirectSoundCreate was already called!?')
      else
        EmuWarning('Initializing DirectSound pointer since DirectSoundCreate was not called!');
    end;

    // Create the DirectSound buffer before continuing...
    Result := DirectSoundCreate8(NULL, PIDirectSound8(@g_pDSound8), NULL);
    if FAILED(Result) then
      DxbxKrnlCleanup('DirectSoundCreate8 Failed!');

    Result := IDirectSound8(g_pDSound8).SetCooperativeLevel(g_hEmuWindow, DSSCL_PRIORITY);

    if FAILED(Result) then
      DxbxKrnlCleanup('g_pDSound8->SetCooperativeLevel Failed!');

    // clear sound buffer cache
    for v := 0 to SOUNDBUFFER_CACHE_SIZE-1 do
      g_pDSoundBufferCache[v] := nil;

    // clear sound stream cache
    for v := 0 to SOUNDSTREAM_CACHE_SIZE-1 do
      g_pDSoundStreamCache[v] := nil;

    g_pDSound8RefCount := 1;
  end
  else
    if FromOriginalDSoundCreate then
      EmuWarning('DirectSound already initialized! Ignoring');
end;

function XTL_EmuDirectSoundCreate
(
    pguidDeviceId: LPVOID;
    ppDirectSound: XTL_PLPDIRECTSOUND8;
    pUnknown: LPUNKNOWN
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
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

  Result := DS_OK;

  // Set this flag when this function is called
  g_bDSoundCreateCalled := true;

  AssureDirectSoundCreate(True); // Dxbx addition - use one implementation for DirectSoundCreate

  // This way we can be sure that this function returns a valid
  // DirectSound8 pointer even if we initialized it elsewhere!
  if (nil=ppDirectSound^) and Assigned(g_pDSound8) then
    ppDirectSound^ := g_pDSound8;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirectSound8_AddRef
(
    pThis: XTL_LPDIRECTSOUND8
): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
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

  uRet := g_pDSound8RefCount; Inc(g_pDSound8RefCount);

  EmuSwapFS(fsXbox);

  Result := uRet;
end;

function XTL_EmuIDirectSound8_Release
(
    pThis: XTL_LPDIRECTSOUND8
): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
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

  uRet := g_pDSound8RefCount; Dec(g_pDSound8RefCount);

  (* temporarily (?) disabled by cxbx
  if (uRet = 1) then
    pThis._Release();
  *)

  EmuSwapFS(fsXbox);

  Result := uRet;
end;

function XTL_EmuCDirectSound_GetSpeakerConfig
(
    pThis: PX_CDirectSound;
    pdwSpeakerConfig: PDWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
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

function XTL_EmuIDirectSound8_EnableHeadphones
(
    pThis: XTL_LPDIRECTSOUND8;
    fEnabled: BOOL
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSound8_EnableHeadphones' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   fEnabled                  : 0x%.08X' +
      #13#10');',
      [pThis, fEnabled]);
{$ENDIF}

  EmuWarning('EmuIDirectSound8_EnableHeadphones ignored');

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIDirectSound8_SynchPlayback
(
    pThis: XTL_LPDIRECTSOUND8
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSound8_SynchPlayback' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10');',
      [pThis]);
{$ENDIF}

  EmuWarning('EmuIDirectSound8_SynchPlayback ignored');

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIDirectSound8_DownloadEffectsImage
(
    pThis: XTL_LPDIRECTSOUND8;
    pvImageBuffer: LPCVOID;
    dwImageSize: DWORD;
    pImageLoc: PVOID;      // TODO -oCXBX: Use this param
    ppImageDesc: PVOID   // TODO -oCXBX: Use this param
): HResult; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
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

  // TODO -oCXBX: Actually implement this

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

procedure XTL_EmuDirectSoundDoWork(); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuDirectSoundDoWork();');
{$ENDIF}

  HackUpdateSoundBuffers();
  HackUpdateSoundStreams();

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirectSound8_SetOrientation
(
    pThis: XTL_LPDIRECTSOUND8;
    xFront: FLOAT;
    yFront: FLOAT;
    zFront: FLOAT;
    xTop: FLOAT;
    yTop: FLOAT;
    zTop: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSound8_SetOrientation' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   xFront                    : %f' +
      #13#10'   yFront                    : %f' +
      #13#10'   zFront                    : %f' +
      #13#10'   xTop                      : %f' +
      #13#10'   yTop                      : %f' +
      #13#10'   zTop                      : %f' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [pThis, xFront, yFront, zFront, xTop, yTop, zTop, dwApply]);
{$ENDIF}

  // TODO -oCXBX: Actually implement this

  // TODO -oDxbx : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  // IDirectSound3DListener(pThis^.EmuListener).SetOrientation(xFront, yFront, zFront, xTop, yTop, zTop, dwApply);

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIDirectSound8_SetDistanceFactor
(
    pThis: XTL_LPDIRECTSOUND8;
    fDistanceFactor: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSound8_SetDistanceFactor' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   fDistanceFactor           : %f' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [pThis, fDistanceFactor, dwApply]);
{$ENDIF}

  // TODO -oCXBX: Actually implement this

  // TODO -oDxbx : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  // IDirectSound3DListener(pThis^.EmuListener).SetDistanceFactor(fDistanceFactor, dwApply);

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIDirectSound8_SetRolloffFactor
(
    pThis: XTL_LPDIRECTSOUND8;
    fRolloffFactor: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSound8_SetRolloffFactor' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   fRolloffFactor            : %f' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [pThis, fRolloffFactor, dwApply]);
{$ENDIF}

  // TODO -oCXBX: Actually implement this

  // TODO -oDxbx : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  // IDirectSound3DListener(pThis^.EmuListener).SetRolloffFactor(fRolloffFactor, dwApply);

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIDirectSound8_SetDopplerFactor
(
    pThis: XTL_LPDIRECTSOUND8;
    fDopplerFactor: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSound8_SetDopplerFactor' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   fDopplerFactor            : %f' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [pThis, fDopplerFactor, dwApply]);
{$ENDIF}

  // TODO -oCXBX: Actually implement this

  // TODO -oDxbx : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  // IDirectSound3DListener(pThis^.EmuListener).SetDopplerFactor(fDopplerFactor, dwApply);

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIDirectSound8_SetI3DL2Listener
(
    pThis: XTL_LPDIRECTSOUND8;
    pDummy: PVOID; // TODO -oCXBX: fill this out
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);
{$IFDEF _DEBUG_TRACE}
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSound8_SetI3DL2Listener' +
       #13#10'(' +
       #13#10'   pThis                     : 0x%.08X' +
       #13#10'   pDummy                    : 0x%.08X' +
       #13#10'   dwApply                   : 0x%.08X' +
       #13#10');',
       [pThis, pDummy, dwApply]);
{$ENDIF}
  end;
{$ENDIF}

  // TODO -oCXBX: Actually do something

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuIDirectSound8_SetMixBinHeadroom
(
    pThis: XTL_LPDIRECTSOUND8;
    dwMixBinMask: DWORD;
    dwHeadroom: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);
{$IFDEF _DEBUG_TRACE}
  begin
    DbgPrintf('EmuDSound : EmuIDirectSound8_SetMixBinHeadroom' +
        #13#10'(' +
        #13#10'   pThis                     : 0x%.08X' +
        #13#10'   dwMixBinMask              : 0x%.08X' +
        #13#10'   dwHeadroom                : 0x%.08X' +
        #13#10');',
        [pThis, dwMixBinMask, dwHeadroom]);
  end;
{$ENDIF}

  // TODO -oCXBX: Actually do something

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetMixBins
(
    pThis: XTL_LPDIRECTSOUND8;
    pMixBins: PVOID
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);
{$IFDEF _DEBUG_TRACE}
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetMixBins' +
        #13#10'(' +
        #13#10'   pThis                     : 0x%.08X' +
        #13#10'   pMixBins                  : 0x%.08X' +
        #13#10');',
        [pThis, pMixBins]);
{$ENDIF}
  end;
{$ENDIF}

  // TODO -oCXBX: Actually do something

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetMixBinVolumes
(
    pThis: XTL_LPDIRECTSOUND8;
    pMixBins: PVOID
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);
{$IFDEF _DEBUG_TRACE}
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetMixBinVolumes' +
        #13#10'(' +
        #13#10'   pThis                     : 0x%.08X' +
        #13#10'   pMixBins                  : 0x%.08X' +
        #13#10');',
        [pThis, pMixBins]);
{$ENDIF}
  end;
{$ENDIF}

  // TODO -oCXBX: Actually do something

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuIDirectSound8_SetPosition(
    pThis: XTL_LPDIRECTSOUND8;
    x: FLOAT;
    y: FLOAT;
    z: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF _DEBUG_TRACE}
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSound8_SetPosition' +
        #13#10'(' +
        #13#10'   pThis                     : 0x%.08X' +
        #13#10'   x                         : %f' +
        #13#10'   y                         : %f' +
        #13#10'   z                         : %f' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [pThis, x, y, z, dwApply]);
{$ENDIF}
  end;
  {$ENDIF}

  // TODO -oCXBX: Actually do something

  // TODO -oDxbx : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  // IDirectSound3DListener(pThis^.EmuListener).SetPosition(x, y, z, dwApply);

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuIDirectSound8_SetVelocity(
    pThis: XTL_LPDIRECTSOUND8;
    x: FLOAT;
    y: FLOAT;
    z: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);
{$IFDEF _DEBUG_TRACE}
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSound8_SetVelocity' +
        #13#10'(' +
        #13#10'   pThis                     : 0x%.08X' +
        #13#10'   x                         : %f' +
        #13#10'   y                         : %f' +
        #13#10'   z                         : %f' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [pThis, x, y, z, dwApply]);
{$ENDIF}
  end;
{$ENDIF}

  // TODO -oCXBX: Actually do something

  // TODO -oDxbx : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  // IDirectSound3DListener(pThis^.EmuListener).SetVelocity(x, y, z, dwApply);

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuIDirectSound8_SetAllParameters(
    pThis: XTL_LPDIRECTSOUND8;
    pTodo: LPVOID;  // TODO -oDxbx : LPCDS3DLISTENER
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF _DEBUG_TRACE}
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSound8_SetAllParameters' +
        #13#10'(' +
        #13#10'   pThis                     : 0x%.08X' +
        #13#10'   pTodo                     : 0x%.08X' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [pThis, pTodo, dwApply]);
{$ENDIF}
  end;
{$ENDIF}

  // TODO -oCXBX: Actually do something

  // TODO -oDxbx : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  // IDirectSound3DListener(pThis^.EmuListener).SetAllParameters(nil{???}, dwApply);

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuCDirectSound_CommitDeferredSettings(
    pThis: PX_CDirectSound
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSound_CommitDeferredSettings' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10');',
      [pThis]);
{$ENDIF}

  // TODO -oCXBX: Translate params, then make the PC DirectSound call

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

// DirectSound Buffer creation flags - Most match to native DirectSound8 :

const X_DSBCAPS_CTRL3D              = $00000010;     // The buffer supports 3D
const X_DSBCAPS_CTRLFREQUENCY       = $00000020;     // The buffer supports frequency changes
const X_DSBCAPS_CTRLVOLUME          = $00000080;     // The buffer supports volume changes
const X_DSBCAPS_CTRLPOSITIONNOTIFY  = $00000100;     // The buffer supports position notifications
const X_DSBCAPS_MIXIN               = $00002000;     // The buffer is to be used as the destination of a submix operation
const X_DSBCAPS_LOCDEFER            = $00040000;     // The buffer does not acquire resources at creation
const X_DSBCAPS_FXIN                = $00080000;     // The buffer is to be used as the destination of a post-effects submix operation

const X_DSSTREAMCAPS_CTRL3D = X_DSBCAPS_CTRL3D;
const X_DSSTREAMCAPS_LOCDEFER = X_DSBCAPS_LOCDEFER;

function XTL_EmuDirectSoundCreateBuffer
(
    pdsbd: PX_DSBUFFERDESC;
    ppBuffer: PPX_CDirectSoundBuffer
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
  dwEmuFlags: DWORD;
  pDSBufferDesc: DirectSound.PDSBUFFERDESC;
  pDSBufferDescSpecial: DirectSound.PDSBUFFERDESC;
  bIsSpecial: _bool;
  dwAcceptableMask: DWORD;
  v: int;
begin
  EmuSwapFS(fsWindows);
  pDSBufferDescSpecial := nil; // Dxbx not : Prevent W1036 Variable might not have been initialized

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuDirectSoundCreateBuffer' +
      #13#10'(' +
      #13#10'   pdsbd                     : 0x%.08X' +
      #13#10'   ppBuffer                  : 0x%.08X' +
      #13#10');',
      [pdsbd, ppBuffer]);
{$ENDIF}

  dwEmuFlags := 0;

  pDSBufferDesc := DirectSound.PDSBUFFERDESC(DxbxMalloc(sizeof(DSBUFFERDESC)));
  pDSBufferDescSpecial := NULL;
  bIsSpecial := false;

  // convert from Xbox to PC DSound
  begin
    dwAcceptableMask := X_DSBCAPS_CTRL3D or X_DSBCAPS_CTRLFREQUENCY or X_DSBCAPS_CTRLVOLUME or X_DSBCAPS_CTRLPOSITIONNOTIFY or X_DSBCAPS_MIXIN or X_DSBCAPS_LOCDEFER;

    if (pdsbd.dwFlags and (not dwAcceptableMask)) > 0 then
      EmuWarning('Use of unsupported pdsbd.dwFlags mask(s) (0x%.08X)', [pdsbd.dwFlags and (not dwAcceptableMask)]);

    pDSBufferDesc.dwSize := sizeof(DirectSound.DSBUFFERDESC);
    pDSBufferDesc.dwFlags := (pdsbd.dwFlags and dwAcceptableMask) or DSBCAPS_CTRLVOLUME or DSBCAPS_GETCURRENTPOSITION2;
    pDSBufferDesc.dwBufferBytes := pdsbd.dwBufferBytes;

    if (pDSBufferDesc.dwBufferBytes < DSBSIZE_MIN) then
      pDSBufferDesc.dwBufferBytes := DSBSIZE_MIN
    else if (pDSBufferDesc.dwBufferBytes > DSBSIZE_MAX) then
      pDSBufferDesc.dwBufferBytes := DSBSIZE_MAX;

    pDSBufferDesc.dwReserved := 0;

    if (pdsbd.lpwfxFormat <> NULL) then
    begin
      pDSBufferDesc.lpwfxFormat := {PWAVEFORMATEX}DxbxMalloc(sizeof(WAVEFORMATEX) + pdsbd.lpwfxFormat.cbSize);
      memcpy(pDSBufferDesc.lpwfxFormat, pdsbd.lpwfxFormat, sizeof(WAVEFORMATEX));

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

        { TODO -oCXBX: Get ADPCM working!  MARKED OUT CXBX
        pDSBufferDesc.lpwfxFormat.cbSize := 32;
        const WAVE_FORMAT_ADPCM = 2;
        pDSBufferDesc.lpwfxFormat.wFormatTag := WAVE_FORMAT_ADPCM;
        }
      end;
    end
    else
    begin
      bIsSpecial := true;
      dwEmuFlags := dwEmuFlags or DSB_FLAG_RECIEVEDATA;

      EmuWarning('Creating dummy WAVEFORMATEX (pdsbd->lpwfxFormat = NULL)...');

      // HACK: This is a special sound buffer, create dummy WAVEFORMATEX data.
      // It's supposed to recieve data rather than generate it.  Buffers created
      // with flags DSBCAPS_MIXIN, DSBCAPS_FXIN, and DSBCAPS_FXIN2 will have no
      // WAVEFORMATEX structure by default.

      // TODO -oCXBX: A better response to this scenario if possible.

      pDSBufferDescSpecial := DirectSound.PDSBUFFERDESC(DxbxMalloc(sizeof(DSBUFFERDESC)));
      pDSBufferDescSpecial.lpwfxFormat := PWAVEFORMATEX(DxbxMalloc(sizeof(WAVEFORMATEX)));

      //memset(pDSBufferDescSpecial.lpwfxFormat, 0, sizeof(WAVEFORMATEX));
      //memset(pDSBufferDescSpecial, 0, sizeof(DSBUFFERDESC));

      pDSBufferDescSpecial.lpwfxFormat.wFormatTag := WAVE_FORMAT_PCM;
      pDSBufferDescSpecial.lpwfxFormat.nChannels := 2;
      pDSBufferDescSpecial.lpwfxFormat.nSamplesPerSec := 22050;
      pDSBufferDescSpecial.lpwfxFormat.nBlockAlign := 4;
      pDSBufferDescSpecial.lpwfxFormat.nAvgBytesPerSec := pDSBufferDescSpecial.lpwfxFormat.nSamplesPerSec *
                                                          pDSBufferDescSpecial.lpwfxFormat.nBlockAlign;
      pDSBufferDescSpecial.lpwfxFormat.wBitsPerSample := 16;

      pDSBufferDescSpecial.dwSize := sizeof(DSBUFFERDESC);
      pDSBufferDescSpecial.dwFlags := DSBCAPS_CTRLPAN or DSBCAPS_CTRLVOLUME or DSBCAPS_CTRLFREQUENCY;
      pDSBufferDescSpecial.dwBufferBytes := 3 * pDSBufferDescSpecial.lpwfxFormat.nAvgBytesPerSec;

      // MARKED OUT CXBX
//    pDSBufferDesc.lpwfxFormat := (WAVEFORMATEX*)DxbxMalloc(sizeof(WAVEFORMATEX)/*+pdsbd.lpwfxFormat.cbSize*/);

////  pDSBufferDesc.lpwfxFormat.cbSize := sizeof( WAVEFORMATEX );
//    pDSBufferDesc.lpwfxFormat.nChannels := 1;
//    pDSBufferDesc.lpwfxFormat.wFormatTag := WAVE_FORMAT_PCM;
//    pDSBufferDesc.lpwfxFormat.nSamplesPerSec := 22050;
//    pDSBufferDesc.lpwfxFormat.nBlockAlign := 4;
//    pDSBufferDesc.lpwfxFormat.nAvgBytesPerSec := 4 * 22050;
//    pDSBufferDesc.lpwfxFormat.wBitsPerSample := 16;

      // Give this buffer 3 seconds of data if needed
      {if(pdsbd.dwBufferBytes = 0)
        pDSBufferDesc.dwBufferBytes := 3 * pDSBufferDesc.lpwfxFormat.nAvgBytesPerSec;}
    end;

    pDSBufferDesc.guid3DAlgorithm := DS3DALG_DEFAULT;
  end;

  // sanity check
  if (not bIsSpecial) then
  begin
    if (pDSBufferDesc.lpwfxFormat.nBlockAlign <> (pDSBufferDesc.lpwfxFormat.nChannels*pDSBufferDesc.lpwfxFormat.wBitsPerSample) div 8) then
    begin
      pDSBufferDesc.lpwfxFormat.nBlockAlign := (2*pDSBufferDesc.lpwfxFormat.wBitsPerSample) div 8;
      pDSBufferDesc.lpwfxFormat.nAvgBytesPerSec := pDSBufferDesc.lpwfxFormat.nSamplesPerSec * pDSBufferDesc.lpwfxFormat.nBlockAlign;
    end;
  end;

  // TODO -oCXBX: Garbage Collection
  new({var PX_CDirectSoundBuffer}ppBuffer^);

  ppBuffer^.EmuDirectSoundBuffer8 := nil;
  ppBuffer^.EmuBuffer := nil;
  ppBuffer^.EmuBufferDesc := iif(bIsSpecial, pDSBufferDescSpecial, pDSBufferDesc);
  ppBuffer^.EmuLockPtr1 := nil;
  ppBuffer^.EmuLockBytes1 := 0;
  ppBuffer^.EmuLockPtr2 := nil;
  ppBuffer^.EmuLockBytes2 := 0;
  ppBuffer^.EmuFlags := dwEmuFlags;
  ppBuffer^.EmuListener := nil; // Dxbx addition : Prevent automatic interface release-problems on non-nil!

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuDirectSoundCreateBuffer, *ppBuffer := 0x%.08X, bytes := 0x%.08X', [ppBuffer^, pDSBufferDesc.dwBufferBytes]);
{$ENDIF}

  AssureDirectSoundCreate(); // Dxbx addition - use one implementation for DirectSoundCreate8

  hRet := IDirectSound8(g_pDSound8).CreateSoundBuffer(iif(bIsSpecial, pDSBufferDescSpecial, pDSBufferDesc)^, @(ppBuffer^.EmuDirectSoundBuffer8), NULL);

  if (FAILED(hRet)) then
  begin
    EmuWarning('CreateSoundBuffer Failed!');
    ppBuffer^.EmuDirectSoundBuffer8 := NULL;
  end
  else

  // Dxbx addition : Create a IDirectSound3DListener too, so that all 3-D sound effects can be implemented through that,
  // since on the Xbox1 there is a 1:1 correspondence between the IDirectSound8 object and the listener :
  if (ppBuffer^.EmuBufferDesc.dwFlags and X_DSBCAPS_CTRL3D) > 0 then
  begin
    if FAILED(IDirectSoundBuffer(ppBuffer^.EmuDirectSoundBuffer8).
         QueryInterface(IDirectSound3DListener, {out}IDirectSound3DListener(ppBuffer^.EmuListener))) then
    begin
      ppBuffer^.EmuListener := nil;
  (* From http://www.ews64.com/mcdirectsound.html :
     If you want to add 3D sound there is more to do. First of all, you have to add
     the parameter DSBCAPS_CTRL3D to the flags of the primary buffer
     (e.g. dsbdesc.dwFlags = DSBCAPS_CTRL3D | DSBCAPS_PRIMARYBUFFER).
     With the handle you get from the CreateSoundBuffer command (e.g. lpDSB) you can
     get access to the IDirectSound3DListener interface with the QueryInterface command
     (e.g. lpDSB->QueryInterface(IID_IDirectSound3DListener, (void** )&lpDS3DListener)).
     This interface has functions to change the position, speed and other preferences of the 3D listener.
     The lpDS3DListener from the last example must have the type LPDIRECTSOUND3DLISTENER.

     Additionally, the initialisation of the secondary buffers has to be changed.
     The flag DSBCAPS_CTRL3D has to be set and the following line has to be added:
     lpDSB->QueryInterface(IID_IDirectSound3DBuffer, (void** )&lpDS3DB);.
     You have to make sure that you do not use DSBCAPS_CTRL3D and DSBCAPS_CTRLPAN as flags at the same time;
     otherwise you will get an error.
     Either you use 3D sound, or you use the panning capabilities of DirectSound.

     Finally you must set preferences like lpDS3DB->SetMaxDistance(...)
     or lpDS3DB->SetVelocity(...) that are described in the DirectX SDK.
  *)
    end;
  end;

  // cache this sound buffer
  begin
    for v := 0 to SOUNDBUFFER_CACHE_SIZE-1 do
    begin
      if (g_pDSoundBufferCache[v] = nil) then
      begin
        g_pDSoundBufferCache[v] := ppBuffer^;
        break;
      end;
    end;

    if (v = SOUNDBUFFER_CACHE_SIZE) then
      DxbxKrnlCleanup('SoundBuffer cache out of slots!');
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuIDirectSound8_CreateBuffer
(
    pThis: XTL_LPDIRECTSOUND8;
    pdssd: PX_DSBUFFERDESC;
    ppBuffer: PPX_CDirectSoundBuffer;
    pUnknown: PVOID
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);
  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
  {$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSound8_CreateBuffer' +
         #13#10'(' +
         #13#10'   pThis                     : 0x%.08X' +
         #13#10'   pdssd                     : 0x%.08X' +
         #13#10'   ppBuffer                  : 0x%.08X' +
         #13#10'   pUnknown                  : 0x%.08X' +
         #13#10');',
         [pThis, pdssd, ppBuffer, pUnknown]);
  {$ENDIF}
  end;
  {$endif}

  EmuSwapFS(fsXbox);

  Result := XTL_EmuDirectSoundCreateBuffer(pdssd, ppBuffer);

//  Result := DS_OK; // TODO -oDxbx : Should we fake this?
end;

function XTL_EmuIDirectSound8_CreateSoundBuffer
(
    pThis: XTL_LPDIRECTSOUND8;
    pdsbd: PX_DSBUFFERDESC;
    ppBuffer: PPX_CDirectSoundBuffer;
    pUnkOuter: LPUNKNOWN
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSound8_CreateSoundBuffer' +
           #13#10'(' +
           #13#10'   pdsbd                     : 0x%.08X' +
           #13#10'   ppBuffer                  : 0x%.08X' +
           #13#10'   pUnkOuter                 : 0x%.08X' +
           #13#10');',
           [pdsbd, ppBuffer, pUnkOuter]);
{$ENDIF}
  end;
  {$endif}

  EmuSwapFS(fsXbox);

  Result := XTL_EmuDirectSoundCreateBuffer(pdsbd, ppBuffer);
end;


function XTL_EmuIDirectSoundBuffer8_SetBufferData
(
    pThis: PX_CDirectSoundBuffer; 
    pvBufferData: LPVOID; 
    dwBufferBytes: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetBufferData' +
         #13#10'(' +
         #13#10'   pThis                     : 0x%.08X' +
         #13#10'   pvBufferData              : 0x%.08X' +
         #13#10'   dwBufferBytes             : 0x%.08X' +
         #13#10');',
         [pThis, pvBufferData, dwBufferBytes]);
{$ENDIF}

  // update buffer data cache
  pThis.EmuBuffer := pvBufferData;

  EmuResizeIDirectSoundBuffer8(pThis, dwBufferBytes);

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetPlayRegion
(
    pThis: PX_CDirectSoundBuffer; 
    dwPlayStart: DWORD; 
    dwPlayLength: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetPlayRegion' +
         #13#10'(' +
         #13#10'   pThis                     : 0x%.08X' +
         #13#10'   dwPlayStart               : 0x%.08X' +
         #13#10'   dwPlayLength              : 0x%.08X' +
         #13#10');',
         [pThis, dwPlayStart, dwPlayLength]);
{$ENDIF}

  // TODO -oCXBX: Translate params, then make the PC DirectSound call

  // TODO -oCXBX: Ensure that 4627 & 4361 are intercepting far enough back
  // (otherwise pThis is manipulated!)

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_Lock
(
    pThis: PX_CDirectSoundBuffer; 
    dwOffset: DWORD; 
    dwBytes: DWORD;
    ppvAudioPtr1: PLPVOID; 
    pdwAudioBytes1: LPDWORD; 
    ppvAudioPtr2: PLPVOID; 
    pdwAudioBytes2: LPDWORD;
    dwFlags: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_Lock' +
         #13#10'(' +
         #13#10'   pThis                     : 0x%.08X' +
         #13#10'   dwOffset                  : 0x%.08X' +
         #13#10'   dwBytes                   : 0x%.08X' +
         #13#10'   ppvAudioPtr1              : 0x%.08X' +
         #13#10'   pdwAudioBytes1            : 0x%.08X' +
         #13#10'   ppvAudioPtr2              : 0x%.08X' +
         #13#10'   pdwAudioBytes2            : 0x%.08X' +
         #13#10'   dwFlags                   : 0x%.08X' +
         #13#10');',
         [pThis, dwOffset, dwBytes, ppvAudioPtr1, pdwAudioBytes1,
         ppvAudioPtr2, pdwAudioBytes2, dwFlags]);
{$ENDIF}

  hRet := DS_OK; // Dxbx note : Cxbx uses D3D_OK here.

  if (pThis.EmuBuffer <> nil) then
  begin
    ppvAudioPtr1^ := pThis.EmuBuffer;
    pdwAudioBytes1^ := dwBytes;
  end
  else
  begin
    if (dwBytes > pThis.EmuBufferDesc.dwBufferBytes) then
      EmuResizeIDirectSoundBuffer8(pThis, dwBytes);

    if (pThis.EmuLockPtr1 <> nil) then
      IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8).Unlock(pThis.EmuLockPtr1, pThis.EmuLockBytes1, pThis.EmuLockPtr2, pThis.EmuLockBytes2);

    // TODO -oCXBX: Verify dwFlags is the same as windows
    hRet := IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8).Lock(dwOffset, dwBytes, ppvAudioPtr1, pdwAudioBytes1, ppvAudioPtr2, pdwAudioBytes2, dwFlags);

    if (FAILED(hRet)) then
        DxbxKrnlCleanup('DirectSoundBuffer Lock Failed!');

    pThis.EmuLockPtr1 := ppvAudioPtr1^;
    pThis.EmuLockBytes1 := pdwAudioBytes1^;

    if (ppvAudioPtr2 <> NULL) then
      pThis.EmuLockPtr2 := ppvAudioPtr2^
    else
      pThis.EmuLockPtr2 := nil;

    if (pdwAudioBytes2 <> NULL) then
      pThis.EmuLockBytes2 := pdwAudioBytes2^
    else
      pThis.EmuLockBytes2 := 0;
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuIDirectSoundBuffer8_SetHeadroom
( 
    pThis: PX_CDirectSoundBuffer; 
    dwHeadroom: DWORD
):HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetHeadroom' +
         #13#10'(' +
         #13#10'   pThis                     : 0x%.08X' +
         #13#10'   dwHeadroom                : 0x%.08X' +
         #13#10');',
         [pThis, dwHeadroom]);
{$ENDIF}

  // TODO -oCXBX: Actually implement this

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetLoopRegion
(
    pThis: PX_CDirectSoundBuffer;
    dwLoopStart: DWORD;
    dwLoopLength: DWORD
) : HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetLoopRegion' +
         #13#10'(' +
         #13#10'   pThis                     : 0x%.08X' +
         #13#10'   dwLoopStart               : 0x%.08X' +
         #13#10'   dwLoopLength              : 0x%.08X' +
         #13#10');',
         [pThis, dwLoopStart, dwLoopLength]);
{$ENDIF}

  // TODO -oCXBX: Ensure that 4627 & 4361 are intercepting far enough back
  // (otherwise pThis is manipulated!)

  //EmuResizeIDirectSoundBuffer8(pThis, dwLoopLength);

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_Release
(
    pThis: PX_CDirectSoundBuffer
): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  uRet: ULONG;
  v: int;
begin
  EmuSwapFS(fsWindows);


{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_Release' +
         #13#10'(' +
         #13#10'   pThis                     : 0x%.08X' +
         #13#10');',
         [pThis]);
{$ENDIF}

  uRet := 0;

  if (pThis <> nil) then
  begin
    if (0=(pThis.EmuFlags and DSB_FLAG_RECIEVEDATA)) then
    begin
      uRet := IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8)._Release();

      if (uRet = 0) then
      begin
        pThis.EmuDirectSoundBuffer8 := nil; // Dxbx addition : nil out after free
        IDirectSound3DListener(pThis.EmuListener) := nil; // Dxbx addition : Implicitly release of Listener

        // remove cache entry
        for v := 0 to SOUNDBUFFER_CACHE_SIZE-1 do
        begin
          if (g_pDSoundBufferCache[v] = pThis) then
            g_pDSoundBufferCache[v] := nil;
        end;

        if (pThis.EmuBufferDesc.lpwfxFormat <> NULL) then
          DxbxFree(pThis.EmuBufferDesc.lpwfxFormat);

        DxbxFree(pThis.EmuBufferDesc);

        dispose(pThis);
      end;
    end;
  end;

  EmuSwapFS(fsXbox);

  Result := uRet;
end;

function XTL_EmuIDirectSoundBuffer8_SetPitch
(
    pThis: PX_CDirectSoundBuffer; 
    lPitch: LONG
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetPitch' +
         #13#10'(' +
         #13#10'   pThis                     : 0x%.08X' +
         #13#10'   lPitch                    : 0x%.08X' +
         #13#10');',
         [pThis, lPitch]);
{$ENDIF}

  // TODO -oCXBX: Translate params, then make the PC DirectSound call
  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_GetStatus
(
    pThis: PX_CDirectSoundBuffer;
    pdwStatus: LPDWORD
) : HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_GetStatus' +
         #13#10'(' +
         #13#10'   pThis                     : 0x%.08X' +
         #13#10'   pdwStatus                 : 0x%.08X' +
         #13#10');',
         [pThis, pdwStatus]);
{$ENDIF}

  hRet := DS_OK;

  if (pThis <> nil) and (pThis.EmuBuffer = nil) then
  begin
    hRet := IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8).GetStatus({out}pdwStatus^);
  end
  else
  begin
    pdwStatus^ := 0;
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuIDirectSoundBuffer8_SetCurrentPosition
(
    pThis: PX_CDirectSoundBuffer;
    dwNewPosition: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetCurrentPosition' +
         #13#10'(' +
         #13#10'   pThis                     : 0x%.08X' +
         #13#10'   dwNewPosition             : 0x%.08X' +
         #13#10');',
         [pThis, dwNewPosition]);
{$ENDIF}

  // NOTE: TODO -oCXBX: This call *will* (by MSDN) fail on primary buffers!
  hRet := IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8).SetCurrentPosition(dwNewPosition);

  if (FAILED(hRet)) then
    EmuWarning('SetCurrentPosition Failed!');

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuIDirectSoundBuffer8_GetCurrentPosition
(
    pThis: PX_CDirectSoundBuffer;
    pdwCurrentPlayCursor: PDWORD;
    pdwCurrentWriteCursor: PDWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_GetCurrentPosition' +
         #13#10'(' +
         #13#10'   pThis                     : 0x%.08X' +
         #13#10'   pdwCurrentPlayCursor      : 0x%.08X' +
         #13#10'   pdwCurrentWriteCursor     : 0x%.08X' +
         #13#10');',
         [pThis, pdwCurrentPlayCursor, pdwCurrentWriteCursor]);
{$ENDIF}

  HackUpdateSoundBuffers();
  HackUpdateSoundStreams();

  // NOTE: TODO -oCXBX: This call always seems to fail on primary buffers!
  hRet := IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8).GetCurrentPosition(pdwCurrentPlayCursor, pdwCurrentWriteCursor);

  if (FAILED(hRet)) then
    EmuWarning('GetCurrentPosition Failed!');

  if (pdwCurrentPlayCursor <> nil) and (pdwCurrentWriteCursor <> nil) then
  begin
{$IFDEF DEBUG}
    DbgPrintf('*pdwCurrentPlayCursor := %d, *pdwCurrentWriteCursor := %d', [pdwCurrentPlayCursor^, pdwCurrentWriteCursor^]);
{$ENDIF}
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuIDirectSoundBuffer8_Play
(
    pThis: PX_CDirectSoundBuffer;
    dwReserved1: DWORD;
    dwReserved2: DWORD;
    dwFlags: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_Play' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   dwReserved1               : 0x%.08X' +
      #13#10'   dwReserved2               : 0x%.08X' +
      #13#10'   dwFlags                   : 0x%.08X' +
      #13#10');',
      [pThis, dwReserved1, dwReserved2, dwFlags]);
{$ENDIF}
  if (dwFlags and (not (DSBPLAY_LOOPING or X_DSBPLAY_FROMSTART))) > 0 then
    DxbxKrnlCleanup('Unsupported Playing Flags');

  // rewind buffer
  if ((dwFlags and X_DSBPLAY_FROMSTART) <> X_DSBPLAY_FROMSTART) then
  begin
    if (FAILED(IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8).SetCurrentPosition(0))) then
      EmuWarning('Rewinding buffer failed!');

    dwFlags := dwFlags and (not X_DSBPLAY_FROMSTART);
  end;

  HackUpdateSoundBuffers();

  // close any existing locks
  if (pThis.EmuLockPtr1 <> nil) then
  begin
    IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8).Unlock
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
    hRet := DS_OK; // Dxbx note : Cxbx uses D3D_OK here.
  end
  else
  begin
    hRet := IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8).Play(0, 0, dwFlags);
  end;

  pThis.EmuPlayFlags := dwFlags;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuIDirectSoundBuffer8_Stop
(
    pThis: PX_CDirectSoundBuffer
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_Stop' +
         #13#10'(' +
         #13#10'   pThis                     : 0x%.08X' +
         #13#10');',
         [pThis]);
{$ENDIF}

  hRet := IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8).Stop();

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function XTL_EmuIDirectSoundBuffer8_StopEx
(
    pBuffer: PX_CDirectSoundBuffer; 
    rtTimeStamp: REFERENCE_TIME; 
    dwFlags: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_StopEx' +
         #13#10'(' +
         #13#10'   pBuffer                   : 0x%.08X' +
         #13#10'   rtTimeStamp               : 0x%.08X' +
         #13#10'   dwFlags                   : 0x%.08X' +
         #13#10');',
         [pBuffer, rtTimeStamp, dwFlags]);
{$ENDIF}

  if (pBuffer.EmuDirectSoundBuffer8 = nil) then
    EmuWarning('pBuffer.EmuDirectSoundBuffer8 == 0');

  EmuWarning('StopEx not yet implemented!');

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetVolume
(
    pThis: PX_CDirectSoundBuffer;
    lVolume: LONG
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetVolume' +
         #13#10'(' +
         #13#10'   pThis                     : 0x%.08X' +
         #13#10'   lVolume                   : 0x%.08X' +
         #13#10');',
         [pThis, lVolume]);
{$ENDIF}

  // TODO -oCXBX: Ensure that 4627 & 4361 are intercepting far enough back
  // (otherwise pThis is manipulated!)
//    HRESULT hRet = IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8).SetVolume(lVolume);

  EmuSwapFS(fsXbox);

//    return hRet;
  Result := S_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetOutputBuffer
(
    pThis: XTL_LPDIRECTSOUND8;
    pOutputBuffer: XTL_LPDIRECTSOUND8
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('KeBoostPriorityThread');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirectSoundBuffer8_SetFrequency
(
    pThis: PX_CDirectSoundBuffer;
    dwFrequency: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetFrequency' +
         #13#10'(' +
         #13#10'   pThis                     : 0x%.08X' +
         #13#10'   dwFrequency               : 0x%.08X' +
         #13#10');',
         [pThis, dwFrequency]);
{$ENDIF}

//    HRESULT hRet = IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8).SetFrequency(dwFrequency);

  EmuSwapFS(fsXbox);

//    return hRet;
  Result := S_OK;
end;

function XTL_EmuDirectSoundCreateStream
(
    pdssd: PX_DSSTREAMDESC;
    ppStream: PPX_CDirectSoundStream
): HRESULT; stdcall;
// Branch:shogun  Revision:161  Translator:Shadow_Tj  Done:100
var
  pDSBufferDesc: DirectSound.PDSBUFFERDESC;
  dwAcceptableMask: DWORD;
  hRet: HRESULT;
  v: int;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuDirectSoundCreateStream' +
      #13#10'(' +
      #13#10'   pdssd                     : 0x%.08X' +
      #13#10'   ppStream                  : 0x%.08X' +
      #13#10');',
      [pdssd, ppStream]);
{$ENDIF}

  // TODO -oCXBX: Garbage Collection
  ppStream^ := X_CDirectSoundStream.Create;

  pDSBufferDesc := DirectSound.PDSBUFFERDESC(DxbxMalloc(sizeof(DSBUFFERDESC)));

  // convert from Xbox to PC DSound
  begin
    dwAcceptableMask := X_DSSTREAMCAPS_CTRL3D; // TODO -oCXBX: Note 0x00040000 is being ignored (X_DSSTREAMCAPS_LOCDEFER)

    if (pdssd.dwFlags and (not dwAcceptableMask)) > 0 then
        EmuWarning('Use of unsupported pdssd.dwFlags mask(s) (0x%.08X)', [pdssd.dwFlags and (not dwAcceptableMask)]);

    pDSBufferDesc.dwSize := sizeof(DSBUFFERDESC);
// MARKED OUT CXBX        pDSBufferDesc.dwFlags = (pdssd.dwFlags and dwAcceptableMask) or DSBCAPS_CTRLVOLUME or DSBCAPS_GETCURRENTPOSITION2;
    pDSBufferDesc.dwFlags := DSBCAPS_CTRLVOLUME;
    pDSBufferDesc.dwBufferBytes := DSBSIZE_MIN;

    pDSBufferDesc.dwReserved := 0;

    if (pdssd.lpwfxFormat <> NULL) then
    begin
      pDSBufferDesc.lpwfxFormat := PWAVEFORMATEX(DxbxMalloc(sizeof(WAVEFORMATEX)));
      memcpy(pDSBufferDesc.lpwfxFormat, pdssd.lpwfxFormat, sizeof(WAVEFORMATEX));
    end;

    pDSBufferDesc.guid3DAlgorithm := DS3DALG_DEFAULT;

    if (pDSBufferDesc.lpwfxFormat <> NULL) and (pDSBufferDesc.lpwfxFormat.wFormatTag <> WAVE_FORMAT_PCM) then
    begin
      EmuWarning('Invalid WAVE_FORMAT!');
      if (pDSBufferDesc.lpwfxFormat.wFormatTag = WAVE_FORMAT_XBOX_ADPCM) then
        EmuWarning('WAVE_FORMAT_XBOX_ADPCM Unsupported!');

      ppStream^.EmuDirectSoundBuffer8 := nil;

      EmuSwapFS(fsXbox);

      Result := DS_OK;
      Exit;
    end;

    if (pDSBufferDesc.lpwfxFormat <> NULL) then
    begin
      // we only support 2 channels right now
      if (pDSBufferDesc.lpwfxFormat.nChannels > 2) then
      begin
        pDSBufferDesc.lpwfxFormat.nChannels := 2;
        pDSBufferDesc.lpwfxFormat.nBlockAlign := (2*pDSBufferDesc.lpwfxFormat.wBitsPerSample) div 8;
        pDSBufferDesc.lpwfxFormat.nAvgBytesPerSec := pDSBufferDesc.lpwfxFormat.nSamplesPerSec * pDSBufferDesc.lpwfxFormat.nBlockAlign;
      end;
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

  AssureDirectSoundCreate(); // Dxbx addition - use one implementation for DirectSoundCreate8

  hRet := IDirectSound8(g_pDSound8).CreateSoundBuffer(pDSBufferDesc^, PIDirectSoundBuffer(@(ppStream^.EmuDirectSoundBuffer8)), NULL);

  if (FAILED(hRet)) then
    EmuWarning('CreateSoundBuffer Failed!');

  // cache this sound stream
  begin
    for v := 0 to SOUNDSTREAM_CACHE_SIZE-1 do
    begin
      if (g_pDSoundStreamCache[v] = nil) then
      begin
        g_pDSoundStreamCache[v] := ppStream^;
        break;
      end;
    end;

    if (v = SOUNDSTREAM_CACHE_SIZE) then
        DxbxKrnlCleanup('SoundStream cache out of slots!');
  end;

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuIDirectSound8_CreateStream
(
    pThis: XTL_LPDIRECTSOUND8;
    pdssd: PX_DSSTREAMDESC;
    ppStream: PPX_CDirectSoundStream;
    pUnknown: PVOID
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSound8_CreateStream' +
           #13#10'(' +
           #13#10'   pThis                     : 0x%.08X' +
           #13#10'   pdssd                     : 0x%.08X' +
           #13#10'   ppStream                  : 0x%.08X' +
           #13#10'   pUnknown                  : 0x%.08X' +
           #13#10');',
           [pThis, pdssd, ppStream, pUnknown]);
{$ENDIF}
  end;
  {$endif}

  EmuSwapFS(fsXbox);

  Result := XTL_EmuDirectSoundCreateStream(pdssd, ppStream);

//  Result := DS_OK; // TODO -oDxbx : Should we fake this?
end;

{ X_CMediaObject }

constructor X_CMediaObject.Create(pParentStream: X_CDirectSoundStream);
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
begin
  Self.pParentStream := pParentStream;
end;

function {XTL_Emu}X_CMediaObject.AddRef({pThis: PX_CMediaObject}): ULONG; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
var
  pThis: PX_CMediaObject;
begin
  pThis := Self;
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : X_CMediaObject.AddRef' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10');',
      [pThis]);
{$ENDIF}

//  if (pThis <> nil) then
//    if (pThis.EmuDirectSoundBuffer8 <> nil) then // Cxbx HACK: Ignore unsupported codecs.
//      IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8)._AddRef();

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function {XTL_Emu}X_CMediaObject.Release({pThis: PX_CMediaObject}): ULONG; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
var
  pThis: PX_CMediaObject;
begin
  pThis := Self;
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : X_CMediaObject.Release' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10');',
      [pThis]);
{$ENDIF}

  EmuSwapFS(fsXbox);

  Result := 0;
end;

function {XTL_Emu}X_CMediaObject.GetInfo
(
    {pThis: PX_CMediaObject;}
    pInfo: LPXMEDIAINFO
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
var
  pThis: PX_CMediaObject;
begin
  pThis := Self;
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : X_CMediaObject.GetInfo' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   pInfo                     : 0x%.08X' +
      #13#10');',
      [pThis, pInfo]);
{$ENDIF}

  // TODO -oDXBX: A (real) implementation?
  EmuWarning('X_CMediaObject.GetInfo is not yet supported!');

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function {XTL_Emu}X_CMediaObject.GetStatus
(
    {pThis: PX_CMediaObject;}
    pdwStatus: PDWORD
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
var
  pThis: PX_CMediaObject;
begin
  pThis := Self;
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : X_CMediaObject.GetStatus' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   pdwStatus                 : 0x%.08X' +
      #13#10');',
      [pThis, pdwStatus]);
{$ENDIF}

  EmuWarning('X_CMediaObject.GetStatus is not yet implemented');

  pdwStatus^ := DSBSTATUS_PLAYING;

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function {XTL_Emu}X_CMediaObject.Process
(
    {pThis: PX_CMediaObject;}
    pInputBuffer: PXMEDIAPACKET;
    pOutputBuffer: PXMEDIAPACKET
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
var
  pThis: PX_CMediaObject;
begin
  pThis := Self;
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : X_CMediaObject.Process' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   pInputBuffer              : 0x%.08X' +
      #13#10'   pOutputBuffer             : 0x%.08X' +
      #13#10');',
      [pThis, pInputBuffer, pOutputBuffer]);
{$ENDIF}

  // TODO -oDXBX: Actually Process

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function {XTL_Emu}X_CMediaObject.Discontinuity({pThis: PX_CMediaObject}): HRESULT; stdcall; // virtual;
// Was Dummy_0x10
// Branch:shogun  Revision:163  Translator:PatrickvL  Done:100
var
  pThis: PX_CMediaObject;
begin
  pThis := Self;
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : X_CMediaObject.Discontinuity' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10');',
      [pThis]);
{$ENDIF}

  // Causes deadlock in Halo...
  // TODO -oCxbx: Verify that this is a Vista related problem (I HATE Vista!)
  //    EmuWarning('EmuCMcpxStream_Dummy_0x10 is ignored!');

  // TODO -oDXBX: Actually Process

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;


function {XTL_Emu}X_CMediaObject.Flush({pThis: PX_CMediaObject}): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
var
  pThis: PX_CMediaObject;
begin
  pThis := Self;
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : X_CMediaObject.Flush();',
            [pThis]);
{$ENDIF}

  // TODO -oDXBX: Actually Flush

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

//

function XTL_EmuCDirectSoundStream_SetVolume(pThis: PX_CDirectSoundStream; lVolume: LONG): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetVolume' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   lVolume                   : %d' +
      #13#10');',
      [pThis, lVolume]);
{$ENDIF}

  // TODO -oCXBX: Actually SetVolume

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuCDirectSoundStream_SetRolloffFactor
(
    pThis: PX_CDirectSoundStream;
    fRolloffFactor: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetRolloffFactor' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   fRolloffFactor            : %f' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [pThis, fRolloffFactor, dwApply]);
{$ENDIF}

  // TODO -oCXBX: Actually SetRolloffFactor

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

{ X_CDirectSoundStream }

constructor X_CDirectSoundStream.Create();
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
begin
  pXMediaObject := X_CMediaObject.Create(Self);
end;

function {XTL_Emu}X_CDirectSoundStream.AddRef({pThis: PX_CDirectSoundStream}): ULONG; stdcall; // virtual
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
var
  pThis: PX_CDirectSoundStream;
begin
  pThis := Self;
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_AddRef' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10');',
      [pThis]);
{$ENDIF}

  if (pThis <> nil) then
    if (pThis.EmuDirectSoundBuffer8 <> nil) then // Cxbx HACK: Ignore unsupported codecs.
      IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8)._AddRef();

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function {XTL_Emu}X_CDirectSoundStream.Release({pThis: PX_CDirectSoundStream}): ULONG; stdcall; // virtual
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
var
  uRet: ULONG;
  v: int;
  pThis: PX_CDirectSoundStream;
begin
  pThis := Self;
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_Release' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10');',
      [pThis]);
{$ENDIF}

  uRet := 0;

  if (pThis <> nil) and (pThis.EmuDirectSoundBuffer8 <> nil) then
  begin
    uRet := IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8)._Release();

    if (uRet = 0) then
    begin
      pThis.EmuDirectSoundBuffer8 := nil; // Dxbx addition : nil out after free
      // remove cache entry
      for v := 0 to SOUNDSTREAM_CACHE_SIZE-1 do
      begin
        if (g_pDSoundStreamCache[v] = pThis) then
          g_pDSoundStreamCache[v] := nil;
      end;

      if (pThis.EmuBufferDesc.lpwfxFormat <> NULL) then
        DxbxFree(pThis.EmuBufferDesc.lpwfxFormat);

      DxbxFree(pThis.EmuBufferDesc);

      pThis.Free;
    end;
  end;

  EmuSwapFS(fsXbox);

  Result := uRet;
end;

function {XTL_Emu}X_CDirectSoundStream.GetInfo
(
    {pThis: PX_CDirectSoundStream;}
    pInfo: LPXMEDIAINFO
): HRESULT; stdcall; // virtual
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  pThis: PX_CDirectSoundStream;
begin
  pThis := Self;
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_GetInfo' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   pInfo                     : 0x%.08X' +
      #13#10');',
      [pThis, pInfo]);
{$ENDIF}

  // TODO -oCXBX: A (real) implementation?
  EmuWarning('EmuCDirectSoundStream_GetInfo is not yet supported!');

  if Assigned(pInfo) then
  begin
    pInfo.dwFlags := XMO_STREAMF_FIXED_SAMPLE_SIZE;
    pInfo.dwInputSize := $40000;
    pInfo.dwOutputSize := $40000;
    pInfo.dwMaxLookahead := $4000;
  end;

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function {XTL_Emu}X_CDirectSoundStream.GetStatus
(
    {pThis: PX_CDirectSoundStream;}
    pdwStatus: PDWORD
): HRESULT; stdcall; // virtual
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pThis: PX_CDirectSoundStream;
begin
  pThis := Self;
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_GetStatus' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   pdwStatus                 : 0x%.08X' +
      #13#10');',
      [pThis, pdwStatus]);
{$ENDIF}

  EmuWarning('EmuCDirectSoundStream_GetStatus is not yet implemented');

  pdwStatus^ := DSBSTATUS_PLAYING;

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function {XTL_Emu}X_CDirectSoundStream.Process
(
    {pThis: PX_CDirectSoundStream;}
    pInputBuffer: PXMEDIAPACKET;
    pOutputBuffer: PXMEDIAPACKET
): HRESULT; stdcall; // virtual
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pThis: PX_CDirectSoundStream;
begin
  pThis := Self;
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_Process' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   pInputBuffer              : 0x%.08X' +
      #13#10'   pOutputBuffer             : 0x%.08X' +
      #13#10');',
      [pThis, pInputBuffer, pOutputBuffer]);
{$ENDIF}

  if (pThis.EmuDirectSoundBuffer8 <> NULL) then
  begin
    // update buffer data cache
    pThis.EmuBuffer := pInputBuffer.pvBuffer;

    EmuResizeIDirectSoundStream8(pThis, pInputBuffer.dwMaxSize);

    if (pInputBuffer.pdwStatus <> nil) then
      pInputBuffer.pdwStatus^ := S_OK;

    HackUpdateSoundStreams();
  end
  else
  begin
    if (pInputBuffer.pdwStatus <> nil) then
      pInputBuffer.pdwStatus^ := S_OK;
  end;

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function {XTL_Emu}X_CDirectSoundStream.Discontinuity({pThis: PX_CDirectSoundStream}): HRESULT; stdcall; // virtual
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pThis: PX_CDirectSoundStream;
begin
  pThis := Self;
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_Discontinuity' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10');',
      [pThis]);
{$ENDIF}

  // TODO -oCXBX: Actually Process

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;


function {XTL_Emu}X_CDirectSoundStream.Flush({pThis: PX_CDirectSoundStream}): HRESULT; stdcall; // virtual
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
var
  pThis: PX_CDirectSoundStream;
begin
  pThis := Self;
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_Flush' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10');',
      [pThis]);
{$ENDIF}

  // TODO -oCXBX: Actually Flush

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

procedure X_CDirectSoundStream.Unknown2; // virtual;
begin
end;

procedure X_CDirectSoundStream.Unknown3; // virtual;
begin
end;

procedure X_CDirectSoundStream.Unknown4; // virtual;
begin
end;

procedure X_CDirectSoundStream.Unknown5; // virtual;
begin
end;

procedure X_CDirectSoundStream.Unknown6; // virtual;
begin
end;

procedure X_CDirectSoundStream.Unknown7; // virtual;
begin
end;

procedure X_CDirectSoundStream.Unknown8; // virtual;
begin
end;

procedure X_CDirectSoundStream.Unknown9; // virtual;
begin
end;

//

function XTL_EmuCDirectSound_SynchPlayback(pUnknown: PVOID): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSound_SynchPlayback (0x%.08X);', [pUnknown]);
{$ENDIF}

  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function XTL_EmuCDirectSoundStream_Pause
(
    pStream: PVOID;
    dwPause: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_Pause' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10'   dwPause                   : 0x%.08X' +
      #13#10');',
      [pStream, dwPause]);
{$ENDIF}

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuIDirectSoundStream_SetHeadroom
(
    pThis: PVOID; 
    dwHeadroom: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundStream_SetHeadroom' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   dwHeadroom                : 0x%.08X' +
      #13#10');',
      [pThis, dwHeadroom]);
{$ENDIF}

  // TODO -oCXBX: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuCDirectSoundStream_SetConeAngles
(
    pThis: PVOID;
    dwInsideConeAngle: DWORD;
    dwOutsideConeAngle: DWORD;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetConeAngles' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   dwInsideConeAngle         : 0x%.08X' +
      #13#10'   dwOutsideConeAngle        : 0x%.08X' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [pThis, dwInsideConeAngle, dwOutsideConeAngle, dwApply]);
{$ENDIF}

  // TODO -oCXBX: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuCDirectSoundStream_SetConeOutsideVolume
(
    pThis: PVOID;
    lConeOutsideVolume: LONG;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetConeOutsideVolume' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   lConeOutsideVolume        : %d' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [pThis, lConeOutsideVolume, dwApply]);
{$ENDIF}

  // TODO -oCXBX: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuCDirectSoundStream_SetAllParameters
(
    pThis: PVOID;
    pUnknown: PVOID;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetAllParameters' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   pUnknown                  : %f' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [pThis, pUnknown, dwApply]);
{$ENDIF}

  // TODO -oCXBX: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuCDirectSoundStream_SetMaxDistance
(
    pThis: PVOID;
    fMaxDistance: D3DVALUE;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetMaxDistance' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   fMaxDistance              : %f' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [pThis, fMaxDistance, dwApply]);
{$ENDIF}

  // TODO -oCXBX: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuCDirectSoundStream_SetMinDistance
(
    pThis: PVOID;
    fMinDistance: D3DVALUE;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetMinDistance' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   fMinDistance              : %f' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [pThis, fMinDistance, dwApply]);
{$ENDIF}

  // TODO -oCXBX: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuCDirectSoundStream_SetVelocity
(
    pThis: PVOID;
    x: D3DVALUE;
    y: D3DVALUE;
    z: D3DVALUE;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetVelocity' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   x                         : %f' +
      #13#10'   y                         : %f' +
      #13#10'   z                         : %f' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [pThis, x, y, z, dwApply]);
{$ENDIF}

  // TODO -oCXBX: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuCDirectSoundStream_SetConeOrientation
(
    pThis: PVOID;
    x: D3DVALUE;
    y: D3DVALUE;
    z: D3DVALUE;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetConeOrientation' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   x                         : %f' +
      #13#10'   y                         : %f' +
      #13#10'   z                         : %f' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [pThis, x, y, z, dwApply]);
{$ENDIF}

  // TODO -oCXBX: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuCDirectSoundStream_SetPosition
(
    pThis: PVOID;
    x: D3DVALUE;
    y: D3DVALUE;
    z: D3DVALUE;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetPosition' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   x                         : %f' +
      #13#10'   y                         : %f' +
      #13#10'   z                         : %f' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [pThis, x, y, z, dwApply]);
{$ENDIF}

  // TODO -oCXBX: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuCDirectSoundStream_SetFrequency
(
    pThis: PVOID;
    dwFrequency: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetFrequency' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   dwFrequency               : %d' +
      #13#10');',
      [pThis, dwFrequency]);
{$ENDIF}

  // TODO -oCXBX: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuIDirectSoundStream_SetI3DL2Source
(
    pThis: PVOID;
    pds3db: PVOID;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundStream_SetI3DL2Source' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   pds3db                    : 0x%.08X' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [pThis, pds3db, dwApply]);
{$ENDIF}

  // TODO -oCXBX: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuCDirectSoundStream_SetMixBins
(
    pThis: PVOID;
    pMixBins: PVOID
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetMixBins' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   pMixBins                  : 0x%.08X' +
      #13#10');',
      [pThis, pMixBins]);
{$ENDIF}

  // TODO -oCXBX: Actually implement this.

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIDirectSoundStream_Unknown1
(
    pThis: PVOID; 
    dwUnknown1: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
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

  // TODO -oCXBX: Actually implement this
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetMaxDistance
(
    pThis: XTL_LPDIRECTSOUNDBUFFER8;
    flMaxDistance: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetMaxDistance' +
        #13#10'(' +
        #13#10'   pThis                     : 0x%.08X' +
        #13#10'   flMaxDistance             : %f' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [pThis, flMaxDistance, dwApply]);
{$ENDIF}
  end;
  {$endif}

  // TODO -oCXBX: Actually do something

  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetMinDistance
(
    pThis: XTL_LPDIRECTSOUNDBUFFER8;
    flMinDistance: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetMinDistance' +
        #13#10'(' +
        #13#10'   pThis                     : 0x%.08X' +
        #13#10'   flMinDistance             : %f' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [pThis, flMinDistance, dwApply]);
{$ENDIF}
  end;
  {$endif}

  // TODO -oCXBX: Actually do something

  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetRolloffFactor
(
    pThis: XTL_LPDIRECTSOUNDBUFFER8;
    flRolloffFactor: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetRolloffFactor' +
        #13#10'(' +
        #13#10'   pThis                     : 0x%.08X' +
        #13#10'   flRolloffFactor           : %f' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [pThis, flRolloffFactor, dwApply]);
{$ENDIF}
  end;
  {$endif}

  // TODO -oCXBX: Actually do something

  // Dxbx addition : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  if Assigned(pThis^.EmuListener) then
    IDirectSound3DListener(pThis^.EmuListener).SetRolloffFactor(flRolloffFactor, dwApply);

  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetDistanceFactor
(
    pThis: XTL_LPDIRECTSOUNDBUFFER8;
    flDistanceFactor: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin

{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetDistanceFactor' +
        #13#10'(' +
        #13#10'   pThis                     : 0x%.08X' +
        #13#10'   flDistanceFactor          : %f' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [pThis, flDistanceFactor, dwApply]);
{$ENDIF}
  end;
  {$endif}

  // TODO -oCXBX: Actually do something

  // Dxbx addition : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  if Assigned(pThis^.EmuListener) then
    IDirectSound3DListener(pThis^.EmuListener).SetDistanceFactor(flDistanceFactor, dwApply);

  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetConeAngles
(
    pThis: XTL_LPDIRECTSOUNDBUFFER8;
    dwInsideConeAngle: DWORD;
    dwOutsideConeAngle: DWORD;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetConeAngles' +
        #13#10'(' +
        #13#10'   pThis                     : 0x%.08X' +
        #13#10'   dwInsideConeAngle         : 0x%.08X' +
        #13#10'   dwOutsideConeAngle        : 0x%.08X' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [pThis, dwInsideConeAngle,
        dwOutsideConeAngle, dwApply]);
{$ENDIF}
  end;
  {$endif}

  // TODO -oCXBX: Actually do something

  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetConeOrientation
(
    pThis: XTL_LPDIRECTSOUNDBUFFER8;
    x: FLOAT;
    y: FLOAT;
    z: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetConeOrientation' +
        #13#10'(' +
        #13#10'   pThis                     : 0x%.08X' +
        #13#10'   x                         : %f' +
        #13#10'   y                         : %f' +
        #13#10'   z                         : %f' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [pThis, x, y, z, dwApply]);
{$ENDIF}
  end;
  {$endif}

  // TODO -oCXBX: Actually do something

  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetConeOutsideVolume
(
    pThis: XTL_LPDIRECTSOUNDBUFFER8;
    lConeOutsideVolume: LONG;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetConeOutsideVolume' +
        #13#10'(' +
        #13#10'   pThis                     : 0x%.08X' +
        #13#10'   lConeOutsideVolume        : 0x%.08X' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [pThis, lConeOutsideVolume, dwApply]);
{$ENDIF}
  end;
  {$endif}

  // TODO -oCXBX: Actually do something

  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetPosition
(
    pThis: XTL_LPDIRECTSOUNDBUFFER8;
    x: FLOAT;
    y: FLOAT;
    z: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetPosition' +
        #13#10'(' +
        #13#10'   pThis                     : 0x%.08X' +
        #13#10'   x                         : %f' +
        #13#10'   y                         : %f' +
        #13#10'   z                         : %f' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [pThis, x, y, z, dwApply]);
{$ENDIF}
  end;
  {$endif}

  // TODO -oCXBX: Actually do something

  // Dxbx addition : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  if Assigned(pThis^.EmuListener) then
    IDirectSound3DListener(pThis^.EmuListener).SetPosition(x, y, z, dwApply);

  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetVelocity
(
    pThis: XTL_LPDIRECTSOUNDBUFFER8;
    x: FLOAT;
    y: FLOAT;
    z: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetVelocity' +
        #13#10'(' +
        #13#10'   pThis                     : 0x%.08X' +
        #13#10'   x                         : %f' +
        #13#10'   y                         : %f' +
        #13#10'   z                         : %f' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [pThis, x, y, z, dwApply]);
{$ENDIF}
  end;
  {$endif}

  // TODO -oCXBX: Actually do something

  // Dxbx addition : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  if Assigned(pThis^.EmuListener) then
    IDirectSound3DListener(pThis^.EmuListener).SetVelocity(x, y, z, dwApply);

  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetDopplerFactor
(
    pThis: XTL_LPDIRECTSOUNDBUFFER8;
    flDopplerFactor: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);
  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
{$IFDEF DEBUG}
      DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetDopplerFactor' + // Cxbx incorrectly logs SetConeOutsideVolume
             #13#10'(' +
             #13#10'   pThis                     : 0x%.08X' +
             #13#10'   flDopplerFactor           : %f' +
             #13#10'   dwApply                   : 0x%.08X' +
             #13#10');',
             [pThis, flDopplerFactor, dwApply]);
{$ENDIF}
   end;
  {$endif}

  // TODO -oCXBX: Actually do something

  // Dxbx addition : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  if Assigned(pThis^.EmuListener) then
    IDirectSound3DListener(pThis^.EmuListener).SetDopplerFactor(flDopplerFactor, dwApply);

  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetI3DL2Source
(
    pThis: XTL_LPDIRECTSOUNDBUFFER8;
    pds3db: LPCDSI3DL2BUFFER;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);
// debug trace
{$ifdef _DEBUG_TRACE}
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetI3DL2Source' +
           #13#10'(' +
           #13#10'   pThis                     : 0x%.08X' +
           #13#10'   pds3db                    : 0x%.08X' +
           #13#10'   dwApply                   : 0x%.08X' +
           #13#10');',
           [pThis, pds3db, dwApply]);
{$ENDIF}
  end;
  {$endif}

  // TODO -oCXBX: Actually do something

  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function XTL_EmuIDirectSoundBuffer8_SetMode
(
    pBuffer: PX_CDirectSoundBuffer;
    dwMode: DWORD;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetMode' +
      #13#10'(' +
      #13#10'   pBuffer             : 0x%.08X' +
      #13#10'   dwMode              : 0x%.08X' +
      #13#10'   dwApply             : 0x%.08X' +
      #13#10');',
      [pBuffer, dwMode, dwApply]);
{$ENDIF}

  RESULT := DS_OK;

  EmuWarning('EmuIDirectSoundBuffer8_SetMode ignored');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirectSoundBuffer8_SetFormat
(
    pBuffer: PX_CDirectSoundBuffer;
    pwfxFormat: LPCWAVEFORMATEX
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  // debug trace
  {$ifdef _DEBUG_TRACE}
  begin
{$IFDEF DEBUG}
    DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetFormat' +
        #13#10'(' +
        #13#10'   pBuffer                   : 0x%.08X' +
        #13#10'   pwfxFormat                : 0x%.08X' +
        #13#10');',
        [pBuffer,pwfxFormat]);
{$ENDIF}
  end;
  {$endif}

  Result := DS_OK;

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuDirectSoundUseFullHRTF
(
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuDirectSoundUseFullHRTF();');
{$ENDIF}

  // TODO -oCXBX: Actually implement this

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirectSoundBuffer8_SetLFO
(
  pThis: XTL_PIDIRECTSOUNDBUFFER;
  pLFODesc: LPCDSLFODESC
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetLFO' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   pLFODesc                  : 0x%.08X' +
      #13#10');',
      [pThis, pLFODesc]);
{$ENDIF}

  // TODO -oCXBX: Implement
  EmuSwapFS(fsXbox);
  Result := S_OK;
end;

procedure XTL_EmuXAudioCreateAdpcmFormat
(
  nChannels: WORD;
  nSamplesPerSec: DWORD;
  pwfx: LPXBOXADPCMWAVEFORMAT
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuXAudioCreateAdpcmFormat' +
      #13#10'(' +
      #13#10'   nChannels                 : 0x%.04X' +
      #13#10'   nSamplesPerSec            : 0x%.08X' +
      #13#10'   pwfx                      : 0x%.08X' +
      #13#10');',
      [nChannels, nSamplesPerSec, pwfx]);
{$ENDIF}

  // Fill out the pwfx structure with the appropriate data
  pwfx.wfx.wFormatTag       := WAVE_FORMAT_XBOX_ADPCM;
  pwfx.wfx.nChannels        := nChannels;
  pwfx.wfx.nSamplesPerSec   := nSamplesPerSec;
  pwfx.wfx.nAvgBytesPerSec  := (nSamplesPerSec*nChannels * 36) div 64;
  pwfx.wfx.nBlockAlign      := nChannels * 36;
  pwfx.wfx.wBitsPerSample   := 4;
  pwfx.wfx.cbSize           := 2;
  pwfx.wSamplesPerBlock     := 64;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirectSoundBuffer8_SetRolloffCurve
(
  pThis: XTL_LPDIRECTSOUNDBUFFER8; // Cxbx uses XTL_PIDIRECTSOUNDBUFFER
  pflPoints: PFLOAT;
  dwPointCount: DWORD;
  dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetRolloffCurve' +
      #13#10'(' +
      #13#10'   pThis                     : 0x%.08X' +
      #13#10'   pflPoints                 : 0x%.08X' +
      #13#10'   dwPointCount              : 0x%.08X' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [pThis, pflPoints, dwPointCount, dwApply]);
{$ENDIF}

  // TODO -oCXBX: Implement

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;


function XTL_EmuIDirectSoundStream_SetVolume
(
  pStream: LPDIRECTSOUNDSTREAM;
  lVolume: LONG
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
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

  // TODO -oCXBX: Implement

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function XTL_EmuIDirectSound_EnableHeadphones
(
    pThis: LPDIRECTSOUND;
    fEnabled: BOOL
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSound_EnableHeadphones' +
      #13#10'(' +
      #13#10'   pThis           : 0x%.08X' +
      #13#10'   fEnabled        : 0x%.08X' +
      #13#10');',
      [pThis, fEnabled]);
{$ENDIF}

  EmuSwapFS(fsXbox);

  result := DS_OK;
end;

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_AddRef
// ******************************************************************
function XTL_EmuIDirectSoundBuffer8_AddRef
(
    pThis: PX_CDirectSoundBuffer
): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer_AddRef' +
      #13#10'(' +
      #13#10'   pThis                   : 0x%.08X' +
      #13#10');',
      [pThis]);
{$ENDIF}

  Result := 0;

  if (pThis <> nil) then
  begin
    // HACK: Skip this on unsupported flags
    if(pThis.EmuFlags and DSB_FLAG_RECIEVEDATA) > 0 then
    begin
      EmuWarning('Not adding reference to a potentially bad pointer!');
    end
    else
    begin
      if(pThis.EmuDirectSoundBuffer8 <> nil) then // HACK: Ignore unsupported codecs.
        Result := IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8)._AddRef();
    end;
  end;

  EmuSwapFS(fsXbox);
end;

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_Pause
// ******************************************************************
function XTL_EmuIDirectSoundBuffer8_Pause
(
    pThis: PX_CDirectSoundBuffer;
    dwPause: DWORD          
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
//var
//  dwFlags: DWord;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer_Pause' +
      #13#10'(' +
      #13#10'   pThis                   : 0x%.08X' +
      #13#10'   dwPause                 : 0x%.08X' +
      #13#10');',
      [pThis, dwPause]);
{$ENDIF}

  // This function wasn't part of the XDK until 4721.
  Result := S_OK;

  // Unstable!
  (*if (pThis <> NULL) then
  begin
    if(pThis.EmuDirectSoundBuffer8) then
    begin
      if (dwPause = X_DSBPAUSE_PAUSE) then
        result := pThis.EmuDirectSoundBuffer8.Stop();
      if (dwPause = X_DSBPAUSE_RESUME) then
      begin
        dwFlags := iif((pThis.EmuPlayFlags & X_DSBPLAY_LOOPING) > 0, DSBPLAY_LOOPING, 0);
        result := IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8).Play(0, 0, dwFlags);
      end;
      if (dwPause = X_DSBPAUSE_SYNCHPLAYBACK) then
        EmuWarning('DSBPAUSE_SYNCHPLAYBACK is not yet supported!');
    end;
  end;*)

  EmuSwapFS(fsXbox);
end;

//MARKED OUT CXBX
//// ******************************************************************
//// * func: EmuIDirectSoundBuffer_Pause
//// ******************************************************************
//function {extern 'C'} XTL_EmuIDirectSoundBuffer_PauseEx
//(
//    pThis: PX_CDirectSoundBuffer;
//    rtTimestamp: REFERENCE_TIME;
//    dwPause: DWORD
//): HRESULT; stdcall;
//// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
//var
//  ret: HRESULT;
//  dwFlags: DWORD;
//begin
//  EmuSwapFS(fsWindows);
//
//{$IFDEF DEBUG}
//  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer_PauseEx' +
//      #13#10'(' +
//      #13#10'   pThis                   : 0x%.08X' +
//      #13#10'   rtTimestamp             : 0x%.08X' +
//      #13#10'   dwPause                 : 0x%.08X' +
//      #13#10');',
//      [pThis, rtTimestamp, dwPause);
//{$ENDIF}
//
//  // This function wasn't part of the XDK until 4721.
//  // TODO: Implement time stamp feature (a thread maybe?)
//  EmuWarning('IDirectSoundBuffer_PauseEx not fully implemented!');
//
//  ret := DS_OK;
//
//  if(pThis <> NULL) then
//  begin
//    if(pThis.EmuDirectSoundBuffer8) then
//    begin
//      if(dwPause = X_DSBPAUSE_PAUSE) then
//        ret := pThis.EmuDirectSoundBuffer8.Stop();
//      if(dwPause = X_DSBPAUSE_RESUME) then
//      begin
//        dwFlags := iif((pThis.EmuPlayFlags & X_DSBPLAY_LOOPING) > 0, DSBPLAY_LOOPING, 0);
//        ret := IDirectSoundBuffer(pThis.EmuDirectSoundBuffer8).Play(0, 0, dwFlags);
//      end;
//      if(dwPause = X_DSBPAUSE_SYNCHPLAYBACK) then
//        EmuWarning('DSBPAUSE_SYNCHPLAYBACK is not yet supported!');
//    end;
//  end;
//
//  EmuSwapFS(fsXbox);
//
//  result := ret;
//end;

// ******************************************************************
// * func: EmuIDirectSound8_GetOutputLevels
// ******************************************************************
function XTL_EmuIDirectSound8_GetOutputLevels
(
  pThis: XTL_PLPDIRECTSOUND8;
  pOutputLevels: PX_DSOUTPUTLEVELS;
  bResetPeakValues: BOOL
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSound8_GetOutputLevels' +
      #13#10'(' +
      #13#10'   pThis                   : 0x%.08X' +
      #13#10'   pOutputLevels           : 0x%.08X' +
      #13#10'   bResetPeakValues        : 0x%.08X' +
      #13#10');',
      [pThis, pOutputLevels, bResetPeakValues]);
{$ENDIF}

  // TODO -oCXBX: Anything?  Either way, I've never seen a game to date use this...

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

// ******************************************************************
// * func: EmuCDirectSoundStream_SetEG
// ******************************************************************
function XTL_EmuCDirectSoundStream_SetEG
(
  pThis: LPVOID;
  pEnvelopeDesc: LPVOID
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetEG' +
      #13#10'(' +
      #13#10'   pThis                   : 0x%.08X' +
      #13#10'   pEnvelopeDesc           : 0x%.08X' +
      #13#10');',
      [pThis, pEnvelopeDesc]);
{$ENDIF}

  // TODO -oCXBX: Implement this...

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

// ******************************************************************
// * func: EmuIDirectSoundStream_Flush
// ******************************************************************
function XTL_EmuIDirectSoundStream_Flush(): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundStream_Flush();');
{$ENDIF}

  // TODO -oCXBX: Actually implement

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

// ******************************************************************
// * func: EmuIDirectSoundStream_FlushEx
// ******************************************************************
function {extern 'C'} XTL_EmuIDirectSoundStream_FlushEx
(
  pThis: PX_CDirectSoundStream;
  rtTimeStamp: REFERENCE_TIME;
  dwFlags: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundStream_FlushEx' +
      #13#10'(' +
      #13#10'   pThis                   : 0x%.08X' +
      #13#10'   rtTimeStamp             : 0x%.08X' +
      #13#10'   dwFlags                 : 0x%.08X' +
      #13#10');',
      [pThis, rtTimeStamp, dwFlags]);
{$ENDIF}

  // TODO -oCXBX: Actually implement

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

// ******************************************************************
// * func: EmuCDirectSoundStream_SetMode
// ******************************************************************
function XTL_EmuCDirectSoundStream_SetMode
(
    pStream: PX_CDirectSoundStream;
    dwMode: DWORD;
    dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetFormat' +
      #13#10'(' +
      #13#10'   pStream             : 0x%.08X' +
      #13#10'   dwMode              : 0x%.08X' +
      #13#10'   dwApply             : 0x%.08X' +
      #13#10');',
      [pStream, dwMode, dwApply]);
{$ENDIF}

  Result := DS_OK;

  EmuWarning('EmuCDirectSoundStream_SetFormat ignored');

  EmuSwapFS(fsXbox);
end;

// ******************************************************************
// * func: EmuXAudioDownloadEffectsImage
// ******************************************************************
function XTL_EmuXAudioDownloadEffectsImage
(
    pszImageName: LPCSTR;
    pImageLoc: LPVOID;
    dwFlags: DWORD;
    ppImageDesc: PLPVOID
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuXAudioDownloadEffectsImage' +
      #13#10'(' +
      #13#10'   pszImageName        : 0x%.08X' +
      #13#10'   pImageLoc           : 0x%.08X' +
      #13#10'   dwFlags             : 0x%.08X' +
      #13#10'   ppImageDesc         : 0x%.08X' +
      #13#10');',
      [UIntPtr(pszImageName), pImageLoc, dwFlags, ppImageDesc]);
{$ENDIF}

   EmuSwapFS(fsXbox);

   Result := S_OK;
end;

// ******************************************************************
// * func: EmuIDirectSoundBuffer8_SetFilter
// ******************************************************************
function XTL_EmuIDirectSoundBuffer8_SetFilter
(
  pThis: LPVOID;
  pFilterDesc: PX_DSFILTERDESC
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_SetFilter' +
      #13#10'(' +
      #13#10'   pThis               : 0x%.08X' +
      #13#10'   pFilterDesc         : 0x%.08X' +
      #13#10');',
      [pThis, pFilterDesc]);
{$ENDIF}

  // TODO -oCXBX: Implement

  EmuWarning('IDirectSoundBuffer8_SetFilter not yet supported!');

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

// ******************************************************************
// * func: EmuCDirectSoundStream_SetFilter
// ******************************************************************
function XTL_EmuCDirectSoundStream_SetFilter
(
  pThis: PX_CDirectSoundStream;
  pFilterDesc: PX_DSFILTERDESC
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuCDirectSoundStream_SetFilter' +
      #13#10'(' +
      #13#10'   pThis               : 0x%.08X' +
      #13#10'   pFilterDesc         : 0x%.08X' +
      #13#10');',
      [pThis, pFilterDesc]);
{$ENDIF}

  // TODO -oCXBX: Implement

  EmuWarning('CDirectSoundStream_SetFilter not yet supported!');

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;


// ******************************************************************
// * func: EmuIDirectSoundBuffer8_PlayEx
// ******************************************************************
function {extern 'C'} XTL_EmuIDirectSoundBuffer8_PlayEx
(
    pBuffer: PX_CDirectSoundBuffer;
    rtTimeStamp: REFERENCE_TIME;
    dwFlags: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundBuffer8_PlayEx' +
      #13#10'(' +
      #13#10'   pBuffer                   : 0x%.08X' +
      #13#10'   rtTimeStamp               : 0x%.08X' +
      #13#10'   dwFlags                   : 0x%.08X' +
      #13#10');',
      [pBuffer, rtTimeStamp, dwFlags]);
{$ENDIF}

  if(pBuffer.EmuDirectSoundBuffer8 = nil) then
    EmuWarning('pBuffer.EmuDirectSoundBuffer8 == 0');

  EmuWarning('PlayEx not yet implemented!');

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

// ******************************************************************
// * func: EmuIDirectSound8_GetCaps
// ******************************************************************
function XTL_EmuIDirectSound8_GetCaps
(
    pThis: PX_CDirectSound;
    pDSCaps: PX_DSCAPS
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  DSCapsPC: DSCAPS;
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSound8_GetCaps' +
      #13#10'(' +
      #13#10'   pThis               : 0x%.08X' +
      #13#10'   pDSCaps             : 0x%.08X' +
      #13#10');',
      [pThis, pDSCaps]);
{$ENDIF}

  AssureDirectSoundCreate(); // Dxbx addition - use one implementation for DirectSoundCreate8

  // Get PC's DirectSound capabilities
  ZeroMemory(@DSCapsPC, sizeof(DSCAPS));
  DSCapsPC.dwSize := sizeof(DSCAPS);

  hRet := IDirectSound8(g_pDSound8).GetCaps({out}DSCapsPC);
  if(FAILED(hRet)) then
    EmuWarning('Failed to get PC DirectSound caps!');

  // Convert PC -> Xbox
  if Assigned(pDSCaps) then
  begin
    // WARNING: This may not be accurate under Windows Vista...
    pDSCaps.dwFree2DBuffers := DSCapsPC.dwFreeHwMixingAllBuffers;
    pDSCaps.dwFree3DBuffers := DSCapsPC.dwFreeHw3DAllBuffers;
    pDSCaps.dwFreeBufferSGEs := 256;              // TODO -oCXBX: Verify max on a real Xbox
    pDSCaps.dwMemoryAllocated := DSCapsPC.dwFreeHwMemBytes;  // TODO -oCXBX: Bytes or MegaBytes?
  end;

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

// ******************************************************************
// * func: EmuIDirectSoundStream_SetPitch
// ******************************************************************
function XTL_EmuIDirectSoundStream_SetPitch
(
    pThis: PX_CDirectSoundStream;
    lPitch: LONG
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuIDirectSoundStream_SetPitch' +
      #13#10'(' +
      #13#10'   pThis               : 0x%.08X' +
      #13#10'   lPitch              : 0x%.08X' +
      #13#10');',
      [pThis, lPitch]);
{$ENDIF}

  Result := S_OK;

  EmuWarning('IDirectSoundStream_SetPitch not yet implemented!');

  EmuSwapFS(fsXbox);
end;

// ******************************************************************
// * func: EmuDirectSoundGetSampleTime
// ******************************************************************
{static} var dwStart: DWORD = 0;
function XTL_EmuDirectSoundGetSampleTime(): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  dwRet: DWORD;
begin
  EmuSwapFS(fsWindows);

{$IFDEF DEBUG}
  DbgPrintf('EmuDSound : EmuDirectSoundGetSampleTime();');
{$ENDIF}

  // FIXME: This is the best I could think of for now.
  // Check the XDK documentation for the description of what this function
  // can actually do.  BTW, this function accesses the NVIDIA SoundStorm APU
  // register directly (0xFE80200C).

  // TODO -oCXBX: Handle reset at certain event?
  // TODO -oCXBX: Wait until a DirectSoundBuffer/Stream is being played?
  if dwStart = 0 then dwStart := GetTickCount(); // Dxbx note : Assign static var only once
  dwRet := GetTickCount() - dwStart;

  EmuSwapFS(fsXbox);

  Result := 0; // TODO -oDXBX: Should we (and Cxbx) really return dwRet here?
end;

{.$MESSAGE 'PatrickvL reviewed up to here'}

exports
  XTL_EmuIDirectSoundBuffer8_PlayEx name PatchPrefix + 'IDirectSoundBuffer_PlayEx',

  XTL_EmuIDirectSoundStream_FlushEx,

  // XTL_EmuCDirectSound_CommitDeferredSettings name PatchPrefix + 'CDirectSound.CommitDeferredSettings', // Disabled, is CDirectSound_CommitDeferredSettingsA (Xref) in Cxbx!
  XTL_EmuCDirectSound_CommitDeferredSettings name PatchPrefix + '_IDirectSound_CommitDeferredSettings@4', // Use it for interface method instead (Cxbx names this wrong)
  XTL_EmuCDirectSound_GetSpeakerConfig name PatchPrefix + 'CDirectSound.GetSpeakerConfig',
  XTL_EmuCDirectSound_SynchPlayback name PatchPrefix + 'CDirectSound.SynchPlayback',

//  XTL_EmuCDirectSoundStream_AddRef name PatchPrefix + 'DirectSound.CDirectSoundStream.AddRef',
//  XTL_EmuCDirectSoundStream_Discontinuity name PatchPrefix + 'DirectSound.CDirectSoundStream.Discontinuity',
//  XTL_EmuCDirectSoundStream_Flush name PatchPrefix + 'DirectSound.CDirectSoundStream.Flush',
//  XTL_EmuCDirectSoundStream_GetInfo name PatchPrefix + 'DirectSound.CDirectSoundStream.GetInfo',
//  XTL_EmuCDirectSoundStream_GetStatus name PatchPrefix + 'DirectSound.CDirectSoundStream.GetStatus',
  XTL_EmuCDirectSoundStream_Pause name PatchPrefix + 'DirectSound.CDirectSoundStream.Pause',
//  XTL_EmuCDirectSoundStream_Process name PatchPrefix + 'DirectSound.CDirectSoundStream.Process',
//  XTL_EmuCDirectSoundStream_Release name PatchPrefix + 'DirectSound.CDirectSoundStream.Release',
  XTL_EmuCDirectSoundStream_SetAllParameters name PatchPrefix + 'DirectSound.CDirectSoundStream.SetAllParameters',
  XTL_EmuCDirectSoundStream_SetConeAngles name PatchPrefix + 'DirectSound.CDirectSoundStream.SetConeAngles',
  XTL_EmuCDirectSoundStream_SetConeOrientation name PatchPrefix + 'DirectSound.CDirectSoundStream.SetConeOrientation',
  XTL_EmuCDirectSoundStream_SetConeOutsideVolume name PatchPrefix + 'DirectSound.CDirectSoundStream.SetConeOutsideVolume',
  XTL_EmuCDirectSoundStream_SetEG name PatchPrefix + 'DirectSound.CDirectSoundStream.SetEG',
  XTL_EmuCDirectSoundStream_SetFilter name PatchPrefix + 'DirectSound.CDirectSoundStream.SetFilter',
  XTL_EmuCDirectSoundStream_SetFrequency name PatchPrefix + 'DirectSound.CDirectSoundStream.SetFrequency',
  XTL_EmuCDirectSoundStream_SetMaxDistance name PatchPrefix + 'DirectSound.CDirectSoundStream.SetMaxDistance',
  XTL_EmuCDirectSoundStream_SetMinDistance name PatchPrefix + 'DirectSound.CDirectSoundStream.SetMinDistance',
  XTL_EmuCDirectSoundStream_SetMixBins name PatchPrefix + 'DirectSound.CDirectSoundStream.SetMixBins',
  XTL_EmuCDirectSoundStream_SetMode name PatchPrefix + 'DirectSound.CDirectSoundStream.SetMode',
  XTL_EmuCDirectSoundStream_SetPosition name PatchPrefix + 'DirectSound.CDirectSoundStream.SetPosition',
  XTL_EmuCDirectSoundStream_SetRolloffFactor name PatchPrefix + 'DirectSound.CDirectSoundStream.SetRolloffFactor',
  XTL_EmuCDirectSoundStream_SetVelocity name PatchPrefix + 'DirectSound.CDirectSoundStream.SetVelocity',
  XTL_EmuCDirectSoundStream_SetVolume name PatchPrefix + 'DirectSound.CDirectSoundStream.SetVolume',

//  XTL_EmuX_CMediaObject_Dummy_0x10,

  XTL_EmuDirectSoundCreate,
  XTL_EmuDirectSoundCreateBuffer,
  XTL_EmuDirectSoundCreateStream,
  XTL_EmuDirectSoundDoWork,
  XTL_EmuDirectSoundGetSampleTime,
  XTL_EmuDirectSoundUseFullHRTF,

  XTL_EmuIDirectSound_EnableHeadphones,

  XTL_EmuIDirectSound8_AddRef name PatchPrefix + 'IDirectSound_AddRef',
  XTL_EmuIDirectSound8_CreateBuffer name PatchPrefix + 'IDirectSound_CreateBuffer',
  XTL_EmuIDirectSound8_CreateSoundBuffer name PatchPrefix + 'IDirectSound_CreateSoundBuffer',
  XTL_EmuIDirectSound8_CreateStream name PatchPrefix + 'IDirectSound_CreateStream',
  XTL_EmuIDirectSound8_DownloadEffectsImage name PatchPrefix + 'IDirectSound_DownloadEffectsImage',
  XTL_EmuIDirectSound8_EnableHeadphones name PatchPrefix + 'IDirectSound_EnableHeadphones',
  XTL_EmuIDirectSound8_GetCaps name PatchPrefix + 'IDirectSound_GetCaps',
  XTL_EmuIDirectSound8_GetOutputLevels name PatchPrefix + 'IDirectSound_GetOutputLevels',
  XTL_EmuIDirectSound8_Release name PatchPrefix + '_IDirectSound_Release@4', // Was 'IDirectSound_Release'
  XTL_EmuIDirectSound8_SetAllParameters name PatchPrefix + 'IDirectSound_SetAllParameters',
  XTL_EmuIDirectSound8_SetDopplerFactor name PatchPrefix + 'IDirectSound_SetDopplerFactor',
  XTL_EmuIDirectSound8_SetI3DL2Listener name PatchPrefix + 'IDirectSound_SetI3DL2Listener',
  XTL_EmuIDirectSound8_SetMixBinHeadroom name PatchPrefix + 'IDirectSound_SetMixBinHeadroom',

  XTL_EmuIDirectSound8_SetOrientation name PatchPrefix + 'IDirectSound_SetOrientation',
  XTL_EmuIDirectSound8_SetDistanceFactor name PatchPrefix + 'IDirectSound_SetDistanceFactor',

  XTL_EmuIDirectSound8_SetPosition name PatchPrefix + 'IDirectSound_SetPosition',
  XTL_EmuIDirectSound8_SetRolloffFactor name PatchPrefix + 'IDirectSound_SetRolloffFactor',
  XTL_EmuIDirectSound8_SetVelocity name PatchPrefix + 'IDirectSound_SetVelocity',
  XTL_EmuIDirectSound8_SynchPlayback name PatchPrefix + 'IDirectSound_SynchPlayback',

  XTL_EmuIDirectSoundBuffer8_AddRef name PatchPrefix + 'IDirectSoundBuffer_AddRef',
  XTL_EmuIDirectSoundBuffer8_GetCurrentPosition name PatchPrefix + 'IDirectSoundBuffer_GetCurrentPosition',
  XTL_EmuIDirectSoundBuffer8_GetStatus name PatchPrefix + 'IDirectSoundBuffer_GetStatus',
  XTL_EmuIDirectSoundBuffer8_Lock name PatchPrefix + 'IDirectSoundBuffer_Lock',
  XTL_EmuIDirectSoundBuffer8_Pause name PatchPrefix + 'IDirectSoundBuffer_Pause',
  XTL_EmuIDirectSoundBuffer8_Play name PatchPrefix + 'IDirectSoundBuffer_Play',
  XTL_EmuIDirectSoundBuffer8_Release name PatchPrefix + '_IDirectSoundBuffer_Release@4',
  XTL_EmuIDirectSoundBuffer8_SetBufferData name PatchPrefix + 'IDirectSoundBuffer_SetBufferData',
  XTL_EmuIDirectSoundBuffer8_SetConeAngles name PatchPrefix + 'IDirectSoundBuffer_SetConeAngles',
  XTL_EmuIDirectSoundBuffer8_SetConeOrientation name PatchPrefix + 'IDirectSoundBuffer_SetConeOrientation',
  XTL_EmuIDirectSoundBuffer8_SetConeOutsideVolume name PatchPrefix + 'IDirectSoundBuffer_SetConeOutsideVolume',
  XTL_EmuIDirectSoundBuffer8_SetCurrentPosition name PatchPrefix + 'IDirectSoundBuffer_SetCurrentPosition',
  XTL_EmuIDirectSoundBuffer8_SetDistanceFactor name PatchPrefix + 'IDirectSoundBuffer_SetDistanceFactor',
  XTL_EmuIDirectSoundBuffer8_SetDopplerFactor name PatchPrefix + 'IDirectSoundBuffer_SetDopplerFactor',
  XTL_EmuIDirectSoundBuffer8_SetFilter name PatchPrefix + 'IDirectSoundBuffer_SetFilter',
  XTL_EmuIDirectSoundBuffer8_SetFormat name PatchPrefix + 'IDirectSoundBuffer_SetFormat',
  XTL_EmuIDirectSoundBuffer8_SetFrequency name PatchPrefix + 'IDirectSoundBuffer_SetFrequency',
  XTL_EmuIDirectSoundBuffer8_SetHeadroom name PatchPrefix + 'IDirectSoundBuffer_SetHeadroom',
  XTL_EmuIDirectSoundBuffer8_SetI3DL2Source name PatchPrefix + 'IDirectSoundBuffer_SetI3DL2Source',
  XTL_EmuIDirectSoundBuffer8_SetLFO name PatchPrefix + 'IDirectSoundBuffer_SetLFO',
  XTL_EmuIDirectSoundBuffer8_SetLoopRegion name PatchPrefix + 'IDirectSoundBuffer_SetLoopRegion',
  XTL_EmuIDirectSoundBuffer8_SetMaxDistance name PatchPrefix + 'IDirectSoundBuffer_SetMaxDistance',
  XTL_EmuIDirectSoundBuffer8_SetMinDistance name PatchPrefix + 'IDirectSoundBuffer_SetMinDistance',
  XTL_EmuIDirectSoundBuffer8_SetMixBins name PatchPrefix + 'IDirectSoundBuffer_SetMixBins',
  XTL_EmuIDirectSoundBuffer8_SetMixBinVolumes name PatchPrefix + 'IDirectSoundBuffer_SetMixBinVolumes',
  XTL_EmuIDirectSoundBuffer8_SetMode name PatchPrefix + 'IDirectSoundBuffer_SetMode',
  XTL_EmuIDirectSoundBuffer8_SetPitch name PatchPrefix + 'IDirectSoundBuffer_SetPitch',
  XTL_EmuIDirectSoundBuffer8_SetPlayRegion name PatchPrefix + 'IDirectSoundBuffer_SetPlayRegion',
  XTL_EmuIDirectSoundBuffer8_SetPosition name PatchPrefix + 'IDirectSoundBuffer_SetPosition',
  XTL_EmuIDirectSoundBuffer8_SetRolloffCurve name PatchPrefix + 'IDirectSoundBuffer_SetRolloffCurve',
  XTL_EmuIDirectSoundBuffer8_SetRolloffFactor name PatchPrefix + 'IDirectSoundBuffer_SetRolloffFactor',
  XTL_EmuIDirectSoundBuffer8_SetVelocity name PatchPrefix + 'IDirectSoundBuffer_SetVelocity',
  XTL_EmuIDirectSoundBuffer8_SetVolume name PatchPrefix + 'IDirectSoundBuffer_SetVolume',
  XTL_EmuIDirectSoundBuffer8_SetOutputBuffer name PatchPrefix + 'IDirectSoundBuffer_SetOutputBuffer',
  XTL_EmuIDirectSoundBuffer8_Stop name PatchPrefix + 'IDirectSoundBuffer_Stop',
  XTL_EmuIDirectSoundBuffer8_StopEx name PatchPrefix + 'IDirectSoundBuffer_StopEx',

  XTL_EmuIDirectSoundStream_Flush,
  XTL_EmuIDirectSoundStream_SetHeadroom,
  XTL_EmuIDirectSoundStream_SetI3DL2Source,
  XTL_EmuIDirectSoundStream_SetPitch,
  XTL_EmuIDirectSoundStream_SetVolume,
  XTL_EmuIDirectSoundStream_Unknown1,

  XTL_EmuXAudioCreateAdpcmFormat,
  XTL_EmuXAudioDownloadEffectsImage;

end.
