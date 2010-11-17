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

uses
  uXBSound;


var g_XBSound: XBSound;

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
  , uEmuShared
  , uTypes
  , uLog
  , uDxbxUtils // ReadSystemTimeIntoLargeInteger
  , uDxbxKrnlUtils
  , uEmuAlloc // DxbxMalloc
  , uEmuFS // EmuSwapFS
  , uEmuKrnl // Unimplemented
  , uEmuKrnlKe // xboxkrnl_KeInterruptTimePtr
  , uXboxLibraryUtils // PatchPrefix
  , uEmuD3D8Types // XTL_PIDirectSoundBuffer, XTL_PIDirectSoundListener
  , uEmu // EmuWarning
  ;

// Flags for wFormatTag field of WAVEFORMAT
const X_WAVE_FORMAT_PCM = 1;
const X_WAVE_FORMAT_XBOX_ADPCM = $0069;
const X_WAVE_FORMAT_VOXWARE_VR12 = $0077;
const X_WAVE_FORMAT_VOXWARE_SC03 = $007A;
const X_WAVE_FORMAT_VOXWARE_SC06 = $007B;
const X_WAVE_FORMAT_EXTENSIBLE = $FFFE;

// Status constants for XMediaObject
const XMO_STATUSF_ACCEPT_INPUT_DATA           = $00000001;
const XMO_STATUSF_ACCEPT_OUTPUT_DATA          = $00000002;
const XMO_STATUSF_MASK                        = $00000003;

// Flags for dwFlags field of XMEDIAINFO
const XMO_STREAMF_FIXED_SAMPLE_SIZE           = $00000001;      // The object supports only a fixed sample size
const XMO_STREAMF_FIXED_PACKET_ALIGNMENT      = $00000002;      // The object supports only a fixed packet alignment
const XMO_STREAMF_INPUT_ASYNC                 = $00000004;      // The object supports receiving input data asynchronously
const XMO_STREAMF_OUTPUT_ASYNC                = $00000008;      // The object supports providing output data asynchronously
const XMO_STREAMF_IN_PLACE                    = $00000010;      // The object supports in-place modification of data
const XMO_STREAMF_MASK                        = $0000001F;

// Values for pdwStatus field of XMediaPacket
const XMEDIAPACKET_STATUS_SUCCESS             = S_OK;
const XMEDIAPACKET_STATUS_PENDING             = E_PENDING;
const XMEDIAPACKET_STATUS_FLUSHED             = E_ABORT;
const XMEDIAPACKET_STATUS_FAILURE             = E_FAIL;

// Speaker flags
const X_DSSPEAKER_STEREO            = $00000000;
const X_DSSPEAKER_MONO              = $00000001;
const X_DSSPEAKER_SURROUND          = $00000002;
const X_DSSPEAKER_ENABLE_AC3        = $00010000;
const X_DSSPEAKER_ENABLE_DTS        = $00020000;
const X_DSSPEAKER_USE_DEFAULT       = $FFFFFFFF;

// Flags for dwFlags argument of TIDirectSoundBuffer.Play/PlayEx
const X_DSBPLAY_LOOPING = $00000001;
const X_DSBPLAY_FROMSTART = $00000002;
const X_DSBPLAY_SYNCHPLAYBACK = $00000004;

// EmuIDirectSoundBuffer_Pause flags
const X_DSBPAUSE_RESUME = $00000000;
const X_DSBPAUSE_PAUSE = $00000001;
const X_DSBPAUSE_SYNCHPLAYBACK = $00000002;

// Dxbx note : Shouldn't these be native? Then where's their definition?
const DSB_FLAG_ADPCM = $00000001;
const DSB_FLAG_RECIEVEDATA = $00001000;

// DirectSound Buffer creation flags - Most match to native DirectSound8 :
const X_DSBCAPS_CTRL3D              = $00000010;     // The buffer supports 3D
const X_DSBCAPS_CTRLFREQUENCY       = $00000020;     // The buffer supports frequency changes
const X_DSBCAPS_CTRLVOLUME          = $00000080;     // The buffer supports volume changes
const X_DSBCAPS_CTRLPOSITIONNOTIFY  = $00000100;     // The buffer supports position notifications
const X_DSBCAPS_MIXIN               = $00002000;     // The buffer is to be used as the destination of a submix operation
const X_DSBCAPS_MUTE3DATMAXDISTANCE = $00020000;
const X_DSBCAPS_LOCDEFER            = $00040000;     // The buffer does not acquire resources at creation
const X_DSBCAPS_FXIN                = $00080000;     // The buffer is to be used as the destination of a post-effects submix operation
const X_DSBCAPS_FXIN2               = $00100000;

// DirectSound Stream creation flags
const X_DSSTREAMCAPS_CTRL3D              = X_DSBCAPS_CTRL3D;              // The stream supports 3D
const X_DSSTREAMCAPS_CTRLFREQUENCY       = X_DSBCAPS_CTRLFREQUENCY;       // The stream supports frequency changes
const X_DSSTREAMCAPS_CTRLVOLUME          = X_DSBCAPS_CTRLVOLUME;          // The stream supports volume changes
const X_DSSTREAMCAPS_MUTE3DATMAXDISTANCE = X_DSBCAPS_MUTE3DATMAXDISTANCE; // The 3D stream is muted at max distance and beyond
const X_DSSTREAMCAPS_LOCDEFER            = X_DSBCAPS_LOCDEFER;            // The stream does not acquire resources at creation
const X_DSSTREAMCAPS_NOCOALESCE          = $20000000;
const X_DSSTREAMCAPS_ACCURATENOTIFY      = $40000000;

// X_DSFILTERDESC modes
const X_DSFILTER_MODE_BYPASS        = $00000000;      // The filter is bypassed
const X_DSFILTER_MODE_DLS2          = $00000001;      // DLS2 mode
const X_DSFILTER_MODE_PARAMEQ       = $00000002;      // Parametric equalizer mode
const X_DSFILTER_MODE_MULTI         = $00000003;      // Multifunction mode

// size of sound buffer cache (used for periodic sound buffer updates)
const SOUNDBUFFER_CACHE_SIZE = $100;

// size of sound stream cache (used for periodic sound stream updates)
const SOUNDSTREAM_CACHE_SIZE = $100;

type
  PIID = PGUID;
  REFIID = PIID; // ??

  // Dxbx note : WAVEFORMATEX on Xbox is identical to Native
     WAVEFORMATEX = TWAVEFORMATEX;
   LPWAVEFORMATEX = MMSystem.PWaveFormatEx; // alias
  LPCWAVEFORMATEX = MMSystem.PWaveFormatEx;

  LPUNKNOWN = Pvoid;

  LPCDSI3DL2BUFFER = Pvoid;
  LPCDSI3DL2LISTENER = Pvoid;

  LPCDSMIXBINS = Pvoid;
  LPCDSEFFECTIMAGELOC = Pvoid;
  LPCDSBPOSITIONNOTIFY = Pvoid;

  LPFNXMEDIAOBJECTCALLBACK = procedure (pStreamContext: LPVOID; pPacketContext: LPVOID; dwStatus: DWORD); stdcall;

  TIDirectSoundStream = class; // forward

   PX_CDirectSoundStream = TIDirectSoundStream; // Dxbx note : Delphi's classes are already pointer-types
  PPX_CDirectSoundStream = ^PX_CDirectSoundStream;

  PLPDIRECTSOUNDSTREAM = PPX_CDirectSoundStream; // ^LPDIRECTSOUNDSTREAM;

  LPCDS3DLISTENER = Pvoid;
  LPDSOUTPUTLEVELS = Pvoid;

  LPDSVOICEPROPS = Pvoid;
  LPDSEFFECTIMAGEDESC = Pvoid;
  PLPDSEFFECTIMAGEDESC = ^LPDSEFFECTIMAGEDESC;

    REFERENCE_TIME = LONGLONG;
   PREFERENCE_TIME = ^REFERENCE_TIME;
  LPREFERENCE_TIME = ^REFERENCE_TIME;


  _X_DSBUFFERDESC = record
  // Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
    {0x00}dwSize: DWORD;
    {0x04}dwFlags: DWORD;
    {0x08}dwBufferBytes: DWORD;
    {0x0C}lpwfxFormat: LPWAVEFORMATEX;
    {0x10}lpMixBins: LPCDSMIXBINS;      // TODO -oCXBX: Implement
    {0x14}dwInputMixBin: DWORD;
  end; {=0x18}
   X_DSBUFFERDESC = _X_DSBUFFERDESC;
  PX_DSBUFFERDESC = ^X_DSBUFFERDESC;
  LPCDSBUFFERDESC = PX_DSBUFFERDESC;


  _X_DSSTREAMDESC = record
  // Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
    {0x00}dwFlags: DWORD;
    {0x04}dwMaxAttachedPackets: DWORD;
    {0x08}lpwfxFormat: LPWAVEFORMATEX;
    {0x0C}lpfnCallback: LPFNXMEDIAOBJECTCALLBACK;
    {0x10}lpvContext: LPVOID;
    {0x14}lpMixBins: LPCDSMIXBINS;
  end; {=0x18}
   X_DSSTREAMDESC = _X_DSSTREAMDESC;
  PX_DSSTREAMDESC = ^X_DSSTREAMDESC;
  LPCDSSTREAMDESC = PX_DSSTREAMDESC;


  _XMEDIAPACKET = record
  // Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
    {0x00}pvBuffer: LPVOID;
    {0x04}dwMaxSize: DWORD;
    {0x08}pdwCompletedSize: PDWORD;
    {0x0C}pdwStatus: PDWORD;
    {union} case Integer of
    0: ({0x10}hCompletionEvent: HANDLE);
    1: ({0x10}pContext: PVOID;
    // end;
    {0x14}prtTimestamp: PREFERENCE_TIME;
    ); // close last union case
  end; {=0x18}
    XMEDIAPACKET = _XMEDIAPACKET;
   PXMEDIAPACKET = ^XMEDIAPACKET;
  LPXMEDIAPACKET = ^XMEDIAPACKET;


  _XMEDIAINFO = record
  // Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
    {0x00}dwFlags: DWORD;
    {0x04}dwInputSize: DWORD;
    {0x08}dwOutputSize: DWORD;
    {0x0C}dwMaxLookahead: DWORD;
  end; {=0x10}
    XMEDIAINFO = _XMEDIAINFO;
   PXEIDIAINFO = ^XMEDIAINFO;
  LPXMEDIAINFO = ^XMEDIAINFO;


  _X_DSFILTERDESC = record
  // Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
    {0x00}dwMode: DWORD;
    {0x04}dwQCoefficient: DWORD;
    {0x08}adwCoefficients: array [0..4-1] of DWORD;
  end; {=0x18}
   X_DSFILTERDESC = _X_DSFILTERDESC;
  PX_DSFILTERDESC = ^X_DSFILTERDESC;
  LPCDSFILTERDESC = PX_DSFILTERDESC;


  _DSLFODESC = record
  // Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
    {0x00}dwLFO: DWORD;
    {0x04}dwDelay: DWORD;
    {0x08}dwDelta: DWORD;
    {0x0C}lPitchModulation: LONG;
    {0x10}lFilterCutOffRange: LONG;
    {0x14}lAmplitudeModulation: LONG;
  end; {=0x18}
     DSLFODESC = _DSLFODESC;
    PDSLFODESC = ^DSLFODESC;
  LPCDSLFODESC = PDSLFODESC;


  _DSENVELOPEDESC = record
    // TODO -oDxbx: Fill this in
  end;
  DSENVELOPEDESC = _DSENVELOPEDESC;
  LPCDSENVELOPEDESC = ^DSENVELOPEDESC;


  _DS3DBUFFER = record
    // TODO -oDxbx: Fill this in
  end;
  DS3DBUFFER = _DS3DBUFFER;
  LPCDS3DBUFFER = ^DS3DBUFFER;


  xbox_adpcmwaveformat_tag = record
  // Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
    wfx: WAVEFORMATEX;            // WAVEFORMATEX data
    wSamplesPerBlock: WORD;       // Number of samples per encoded block.  It must be 64.
  end;
    XBOXADPCMWAVEFORMAT = xbox_adpcmwaveformat_tag;
   PXBOXADPCMWAVEFORMAT = ^XBOXADPCMWAVEFORMAT;
  LPXBOXADPCMWAVEFORMAT = PXBOXADPCMWAVEFORMAT;


  X_DSOUTPUTLEVELS = record
  // Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
    {0x00}dwAnalogLeftTotalPeak: DWORD;// analog peak
    {0x04}dwAnalogRightTotalPeak: DWORD;
    {0x08}dwAnalogLeftTotalRMS: DWORD;// analog RMS
    {0x0C}dwAnalogRightTotalRMS: DWORD;
    {0x10}dwDigitalFrontLeftPeak: DWORD;// digital peak levels
    {0x14}dwDigitalFrontCenterPeak: DWORD;
    {0x18}dwDigitalFrontRightPeak: DWORD;
    {0x1C}dwDigitalBackLeftPeak: DWORD;
    {0x20}dwDigitalBackRightPeak: DWORD;
    {0x24}dwDigitalLowFrequencyPeak: DWORD;
    {0x28}dwDigitalFrontLeftRMS: DWORD;// digital RMS levels
    {0x2C}dwDigitalFrontCenterRMS: DWORD;
    {0x30}dwDigitalFrontRightRMS: DWORD;
    {0x34}dwDigitalBackLeftRMS: DWORD;
    {0x38}dwDigitalBackRightRMS: DWORD;
    {0x3C}dwDigitalLowFrequencyRMS: DWORD;
  end; {=0x40}
  PX_DSOUTPUTLEVELS = ^X_DSOUTPUTLEVELS;


  _X_DSCAPS = record
    {0x00}dwFree2DBuffers: DWORD;
    {0x04}dwFree3DBuffers: DWORD;
    {0x08}dwFreeBufferSGEs: DWORD;
    {0x0C}dwMemoryAllocated: DWORD;
  end; {=0x10}
   X_DSCAPS = _X_DSCAPS;
  PX_DSCAPS = ^X_DSCAPS;
   LPDSCAPS = PX_DSCAPS;


  TIDirectSoundBuffer = class; // forward

   PX_CDirectSoundBuffer = TIDirectSoundBuffer; // Dxbx note : Delphi's classes are already pointer-types
  PPX_CDirectSoundBuffer = ^PX_CDirectSoundBuffer;

  PLPDIRECTSOUNDBUFFER = PPX_CDirectSoundBuffer;
   LPDIRECTSOUNDBUFFER = XTL_LPDIRECTSOUNDBUFFER;

  // Dxbx note : 'virtual' creates vtable (cached by each instance, via constructor).
  // Also, inheritance arranges that a new class has the same members at the same offsets
  // (which can over overriden) and can be extended with new functions.

  // Delphi implementation of IDirectSound interface.
  // Note : On Xbox, this interface contains the following Listener methods, that Native DirectSound
  // should do in a global Listener (if there is/can be made such a thing anyway) :
  //    SetAllParameters, SetDistanceFactor, SetDopplerFactor, SetOrientation,
  //    SetPosition, SetRolloffFactor, SetVelocity and CommitDeferredSettings.
  TIDirectSound = class(TObject)
  // Branch:Dxbx  Translator:PatrickvL  Done:100
  public
    // IUnknown interface :
    {VMT 0x00}function AddRef(): ULONG; virtual; stdcall;
    {VMT 0x04}function Release(): ULONG; virtual; stdcall;
    // IDirectSound interface :
    {VMT 0x08}function GetCaps(pdsc: LPDSCAPS): HRESULT; virtual; stdcall;
    {VMT 0x0C}function CreateSoundBuffer(pdsbd: LPCDSBUFFERDESC; ppBuffer: PLPDIRECTSOUNDBUFFER; pUnkOuter: LPUNKNOWN): HRESULT; virtual; stdcall;
    {VMT 0x10}function CreateSoundStream(pdssd: LPCDSSTREAMDESC; ppStream: PLPDIRECTSOUNDSTREAM; pUnkOuter: LPUNKNOWN): HRESULT; virtual; stdcall;
    {VMT 0x14}function GetSpeakerConfig(pdwSpeakerConfig: LPDWORD): HRESULT; virtual; stdcall;
    {VMT 0x18}function SetCooperativeLevel(hWnd: HWND; dwLevel: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x1C}function Compact(): HRESULT; virtual; stdcall;
    {VMT 0x20}function DownloadEffectsImage(pvImageBuffer: LPCVOID; dwImageSize: DWORD; pImageLoc: LPCDSEFFECTIMAGELOC; ppImageDesc: PLPDSEFFECTIMAGEDESC): HRESULT; virtual; stdcall;
    {VMT 0x24}function GetEffectData(dwEffectIndex, dwOffset: DWORD; pvData: LPVOID; dwDataSize: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x28}function SetEffectData(dwEffectIndex, dwOffset: DWORD; pvData: LPCVOID; dwDataSize, dwApply: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x2C}function CommitEffectData(): HRESULT; virtual; stdcall;
    {VMT 0x30}function EnableHeadphones(fEnabled: BOOL): HRESULT; virtual; stdcall;
    {VMT 0x34}function SetMixBinHeadroom(dwMixBin, dwHeadroom: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x38}function SetAllParameters(pds3dl: LPCDS3DLISTENER; dwApply: DWORD): HRESULT; virtual; stdcall; // IDirectSound3DListener
    {VMT 0x3C}function SetOrientation(xFront, yFront, zFront, xTop, yTop, zTop: FLOAT; dwApply: DWORD): HRESULT; virtual; stdcall; // IDirectSound3DListener
    {VMT 0x40}function SetPosition(x, y, z: FLOAT; dwApply: DWORD): HRESULT; virtual; stdcall; // IDirectSound3DListener
    {VMT 0x44}function SetVelocity(x, y, z: FLOAT; dwApply: DWORD): HRESULT; virtual; stdcall; // IDirectSound3DListener
    {VMT 0x48}function SetDistanceFactor(flDistanceFactor: FLOAT; dwApply: DWORD): HRESULT; virtual; stdcall; // IDirectSound3DListener
    {VMT 0x4C}function SetDopplerFactor(flDopplerFactor: FLOAT; dwApply: DWORD): HRESULT; virtual; stdcall; // IDirectSound3DListener
    {VMT 0x50}function SetRolloffFactor(flRolloffFactor: FLOAT; dwApply: DWORD): HRESULT; virtual; stdcall; // IDirectSound3DListener
    {VMT 0x54}function SetI3DL2Listener(pds3dl: LPCDSI3DL2LISTENER; dwApply: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x58}function CommitDeferredSettings(): HRESULT; virtual; stdcall; // IDirectSound3DListener
    {VMT 0x5C}function GetTime(prtCurrent: PREFERENCE_TIME): HRESULT; virtual; stdcall;
    {VMT 0x60}function GetOutputLevels(pOutputLevels: LPDSOUTPUTLEVELS; fResetPeakValues: BOOL): HRESULT; virtual; stdcall;
    {VMT 0x64}function SynchPlayback(): HRESULT; virtual; stdcall;
  end;


  TXMediaObject = class(TObject) // Cxbx incorrectly calls this X_CMcpxStream
  // Branch:Dxbx  Translator:PatrickvL  Done:100
  public
    // IUnknown interface :
    {VMT 0x00}function AddRef(): ULONG; virtual; stdcall;
    {VMT 0x04}function Release(): ULONG; virtual; stdcall;
    // XMediaObject interface :
    {VMT 0x08}function GetInfo(pInfo: LPXMEDIAINFO): HRESULT; virtual; stdcall;
    {VMT 0x0C}function GetStatus(pdwStatus: PDWORD): HRESULT; virtual; stdcall;
    {VMT 0x10}function Process(pInputBuffer: PXMEDIAPACKET; pOutputBuffer: PXMEDIAPACKET): HRESULT; virtual; stdcall;
    {VMT 0x14}function Discontinuity(): HRESULT; virtual; stdcall;
    {VMT 0x18}function Flush(): HRESULT; virtual; stdcall;
{$ifdef DEBUG}
  protected
    // debug mode guard for detecting naughty data accesses
    DebugGuard: array[0..256-1] of DWORD;
{$endif}
  public
//    pParentStream: TIDirectSoundStream;
  end;

(*
  TXFileMediaObject = class(TXMediaObject)
  // Branch:Dxbx  Translator:PatrickvL  Done:100
  public
    // XFileMediaObject interface :
    {VMT 0x1C}function Seek(lOffset: LONG; dwOrigin: DWORD; pdwAbsolute: LPDWORD): HRESULT; virtual; stdcall;
    {VMT 0x20}function GetLength(pdwLength: LPDWORD): HRESULT; virtual; stdcall;
    {VMT 0x24}procedure DoWork(); virtual; stdcall;
  end;

  TXWaveFileMediaObject = class(TXFileMediaObject)
  // Branch:Dxbx  Translator:PatrickvL  Done:100
  public
    // XWaveFileMediaObject interface :
    {VMT 0x28}function GetFormat(ppwfxFormat: PLPCWAVEFORMATEX): HRESULT; virtual; stdcall;
    {VMT 0x2C}function GetLoopRegion(pdwLoopStart: LPDWORD; pdwLoopLength: LPDWORD): HRESULT; virtual; stdcall;
  end;

  TXWmaFileMediaObject = class(TXFileMediaObject)
  // Branch:Dxbx  Translator:PatrickvL  Done:100
  public
    // XWmaFileMediaObject interface :
    {VMT 0x28}function GetFileHeader(pFileHeader: PWMAXMOFileHeader): HRESULT; virtual; stdcall;
    {VMT 0x2C}function GetFileContentDescription(pContentDesc: PWMAXMOFileContDesc): HRESULT; virtual; stdcall;
    {VMT 0x30}function SeekToTime(dwSeek: DWORD; pdwAcutalSeek: LPDWORD): HRESULT; virtual; stdcall;
  end;

  TXAc97MediaObject = class(TXMediaObject)
  // Branch:Dxbx  Translator:PatrickvL  Done:100
  public
    // XAc97MediaObject interface :
    {VMT 0x1C}function SetMode(dwMode: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x20}function GetCurrentPosition(pdwMode: LPDWORD): HRESULT; virtual; stdcall;
  end;
*)

  // Delphi implementation of IDirectSoundBuffer interface.
  // Note : On Xbox, this interface contains the following methods, that Native DirectSound
  // should do in a IDirectSound3DBuffer:
  //    SetAllParameters, SetConeAngles, SetConeOrientation, SetConeOutsideVolume,
  //    SetMaxDistance, SetMinDistance, SetMode, SetPosition and SetVelocity.
  // Note : On Xbox, this interface contains the following methods, that Native DirectSound
  // should do in a IDirectSound3DListener:
  //    SetAllParameters, SetConeAngles, SetConeOrientation, SetConeOutsideVolume,
  //    SetMaxDistance, SetMinDistance, SetMode, SetPosition and SetVelocity.
  TIDirectSoundBuffer = class(TObject)
  // Branch:Dxbx  Translator:PatrickvL  Done:100
  public
//    {0x}function QueryInterfaceC(const PIID iid; PLPVOID ppvInterface): HRESULT; stdcall; virtual;
//    {0x}function QueryInterface IDirectSoundBuffer_QueryInterfaceC(): HRESULT; stdcall; virtual;
    // IUnknown interface :
    {VMT 0x00}function AddRef(): ULONG; virtual; stdcall;
    {VMT 0x04}function Release(): ULONG; virtual; stdcall;
    // IDirectSoundBuffer interface :
    {VMT 0x08}function SetFormat(pwfxFormat: LPCWAVEFORMATEX): HRESULT; virtual; stdcall; // IDirectSoundBuffer
    {VMT 0x0C}function SetFrequency(dwFrequency: DWORD): HRESULT; virtual; stdcall; // IDirectSoundBuffer
    {VMT 0x10}function SetVolume(lVolume: LONG): HRESULT; virtual; stdcall; // IDirectSoundBuffer
    {VMT 0x14}function SetPitch(lPitch: LONG): HRESULT; virtual; stdcall; // Unsupported
    {VMT 0x18}function SetLFO(pLFODesc: LPCDSLFODESC): HRESULT; virtual; stdcall; // Unsupported
    {VMT 0x1C}function SetEG(pEnvelopeDesc: LPCDSENVELOPEDESC): HRESULT; virtual; stdcall; // Unsupported
    {VMT 0x20}function SetFilter(pFilterDesc: LPCDSFILTERDESC): HRESULT; virtual; stdcall; // Unsupported
    {VMT 0x24}function SetHeadroom(dwHeadroom: DWORD): HRESULT; virtual; stdcall; // Unsupported
    {VMT 0x28}function SetOutputBuffer(pOutputBuffer: LPDIRECTSOUNDBUFFER): HRESULT; virtual; stdcall; // Unsupported
    {VMT 0x2C}function SetMixBins(pMixBins: LPCDSMIXBINS): HRESULT; virtual; stdcall; // Unsupported
    {VMT 0x30}function SetMixBinVolumes(pMixBins: LPCDSMIXBINS): HRESULT; virtual; stdcall; // Unsupported
    {VMT 0x34}function SetAllParameters(pds3db: LPCDS3DBUFFER; dwApply: DWORD): HRESULT; virtual; stdcall; // IDirectSound3DBuffer
    {VMT 0x38}function SetConeAngles(dwInsideConeAngle, dwOutsideConeAngle, dwApply: DWORD): HRESULT; virtual; stdcall; // IDirectSound3DBuffer
    {VMT 0x3C}function SetConeOrientation(x, y, z: FLOAT; dwApply: DWORD): HRESULT; virtual; stdcall; // IDirectSound3DBuffer
    {VMT 0x40}function SetConeOutsideVolume(lConeOutsideVolume: LONG; dwApply: DWORD): HRESULT; virtual; stdcall; // IDirectSound3DBuffer
    {VMT 0x44}function SetMaxDistance(flMaxDistance: FLOAT; dwApply: DWORD): HRESULT; virtual; stdcall; // IDirectSound3DBuffer
    {VMT 0x48}function SetMinDistance(flMinDistance: FLOAT; dwApply: DWORD): HRESULT; virtual; stdcall; // IDirectSound3DBuffer
    {VMT 0x4C}function SetMode(dwMode, dwApply: DWORD): HRESULT; virtual; stdcall; // IDirectSound3DBuffer
    {VMT 0x50}function SetPosition(x, y, z: FLOAT; dwApply: DWORD): HRESULT; virtual; stdcall; // IDirectSound3DBuffer / IDirectSound3DListener
    {VMT 0x54}function SetVelocity(x, y, z: FLOAT; dwApply: DWORD): HRESULT; virtual; stdcall; // IDirectSound3DBuffer / IDirectSound3DListener
    {VMT 0x58}function SetDistanceFactor(flDistanceFactor: FLOAT; dwApply: DWORD): HRESULT; virtual; stdcall; // IDirectSound3DListener
    {VMT 0x5C}function SetDopplerFactor(flDopplerFactor: FLOAT; dwApply: DWORD): HRESULT; virtual; stdcall; // IDirectSound3DListener
    {VMT 0x60}function SetRolloffFactor(flRolloffFactor: FLOAT; dwApply: DWORD): HRESULT; virtual; stdcall; // IDirectSound3DListener
    {VMT 0x64}function SetRolloffCurve(pflPoints: PFLOAT; dwPointCount, dwApply: DWORD): HRESULT; virtual; stdcall; // Unsupported
    {VMT 0x68}function SetI3DL2Source(pds3db: LPCDSI3DL2BUFFER; dwApply: DWORD): HRESULT; virtual; stdcall; // Unsupported
    {VMT 0x6C}function Play(dwReserved1, dwReserved2, dwFlags: DWORD): HRESULT; virtual; stdcall; // IDirectSoundBuffer
    {VMT 0x70}function PlayEx(rtTimeStamp: REFERENCE_TIME; dwFlags: DWORD): HRESULT; virtual; stdcall; // Unsupported
    {VMT 0x74}function Stop(): HRESULT; virtual; stdcall; // IDirectSoundBuffer
    {VMT 0x78}function StopEx(rtTimeStamp: REFERENCE_TIME; dwFlags: DWORD): HRESULT; virtual; stdcall; // Unsupported
    {VMT 0x7C}function Pause(dwPause: DWORD): HRESULT; virtual; stdcall; // Unsupported
    {VMT 0x80}function PauseEx(rtTimestamp: REFERENCE_TIME; dwPause: DWORD): HRESULT; virtual; stdcall; // Unsupported
    {VMT 0x84}function SetPlayRegion(dwPlayStart, dwPlayLength: DWORD): HRESULT; virtual; stdcall; // Unsupported
    {VMT 0x88}function SetLoopRegion(dwLoopStart, dwLoopLength: DWORD): HRESULT; virtual; stdcall; // Unsupported
    {VMT 0x8C}function GetStatus(pdwStatus: LPDWORD): HRESULT; virtual; stdcall; // IDirectSoundBuffer
    {VMT 0x90}function GetCurrentPosition(pdwPlayCursor: LPDWORD; pdwWriteCursor: LPDWORD): HRESULT; virtual; stdcall; // IDirectSoundBuffer
    {VMT 0x94}function SetCurrentPosition(dwPlayCursor: DWORD): HRESULT; virtual; stdcall; // IDirectSoundBuffer
    {VMT 0x98}function SetBufferData(pvBufferData: LPVOID; dwBufferBytes: DWORD): HRESULT; virtual; stdcall; // Unsupported
    {VMT 0x9C}function Lock(dwOffset, dwBytes: DWORD; ppvAudioPtr1: PLPVOID; pdwAudioBytes1: LPDWORD; ppvAudioPtr2: PLPVOID; pdwAudioBytes2: LPDWORD; dwFlags: DWORD): HRESULT; virtual; stdcall; // IDirectSoundBuffer
    {VMT 0xA0}function Unlock(pvLock1: LPVOID; dwLockSize1: DWORD; pvLock2: LPVOID; dwLockSize2: DWORD): HRESULT; virtual; stdcall; // IDirectSoundBuffer
    {VMT 0xA4}function Restore(): HRESULT; virtual; stdcall; // IDirectSoundBuffer
    {VMT 0xA8}function SetNotificationPositions(dwNotifyCount: DWORD; paNotifies: LPCDSBPOSITIONNOTIFY): HRESULT; virtual; stdcall; // Unsupported
    {VMT 0xAC}function GetVoiceProperties(pVoiceProps: LPDSVOICEPROPS): HRESULT; virtual; stdcall; // Unsupported
  public
  // Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
    // TODO -oDxbx: In Cxbx, the following variables start at {0x00} - is that still correct/necessary?
    {0x00}UnknownA: array [0..$20-1] of BYTE;
    {0x20}EmuDirectSoundBuffer8: XTL_PIDirectSoundBuffer;
    {0x24}UnknownB: array [0..$0C-1] of BYTE;
    {0x30}EmuBuffer: PVOID;
    {0x34}EmuBufferDesc: PDSBUFFERDESC;
    {0x38}EmuLockPtr1: PVOID;
    {0x3C}EmuLockBytes1: DWORD;
    {0x40}EmuLockPtr2: PVOID;
    {0x44}EmuLockBytes2: DWORD;
    {0x48}EmuPlayFlags: DWORD;
    {0x4C}EmuFlags: DWORD;
    {0x50}EmuListener: XTL_PIDirectSoundListener; // Dxbx addition - to support 3D sound emulation through IDirectSound3DListener
  end;

  TIDirectSoundStream = class(TXMediaObject)
  // Branch:Dxbx  Translator:PatrickvL  Done:100
  public
    // IUnknown interface :
    {VMT 0x00}function AddRef(): ULONG; override; stdcall;
    {VMT 0x04}function Release(): ULONG; override; stdcall;
    // XMediaObject interface :
    {VMT 0x08}function GetInfo(pInfo: LPXMEDIAINFO): HRESULT; override; stdcall;
    {VMT 0x0C}function GetStatus(pdwStatus: PDWORD): HRESULT; override; stdcall;
    {VMT 0x10}function Process(pInputBuffer: PXMEDIAPACKET; pOutputBuffer: PXMEDIAPACKET): HRESULT; override; stdcall;
    {VMT 0x14}function Discontinuity(): HRESULT; override; stdcall;
    {VMT 0x18}function Flush(): HRESULT; override; stdcall;
    // IDirectSoundStream interface :
    {VMT 0x1C}function SetFormat(pwfxFormat: LPCWAVEFORMATEX): HRESULT; virtual; stdcall;
    {VMT 0x20}function SetFrequency(dwFrequency: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x24}function SetVolume(lVolume: LONG): HRESULT; virtual; stdcall;
    {VMT 0x28}function SetPitch(lPitch: LONG): HRESULT; virtual; stdcall;
    {VMT 0x2C}function SetLFO(pLFODesc: LPCDSLFODESC): HRESULT; virtual; stdcall;
    {VMT 0x30}function SetEG(pEnvelopeDesc: LPCDSENVELOPEDESC): HRESULT; virtual; stdcall;
    {VMT 0x34}function SetFilter(pFilterDesc: LPCDSFILTERDESC): HRESULT; virtual; stdcall;
    {VMT 0x38}function SetHeadroom(dwHeadroom: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x3C}function SetOutputBuffer(pOutputBuffer: LPDIRECTSOUNDBUFFER): HRESULT; virtual; stdcall;
    {VMT 0x40}function SetMixBins(pMixBins: LPCDSMIXBINS): HRESULT; virtual; stdcall;
    {VMT 0x44}function SetMixBinVolumes(pMixBins: LPCDSMIXBINS): HRESULT; virtual; stdcall;
    {VMT 0x48}function SetAllParameters(pds3db: LPCDS3DBUFFER; dwApply: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x4C}function SetConeAngles(dwInsideConeAngle, dwOutsideConeAngle, dwApply: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x50}function SetConeOrientation(x, y, z: FLOAT; dwApply: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x54}function SetConeOutsideVolume(lConeOutsideVolume: LONG; dwApply: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x58}function SetMaxDistance(flMaxDistance: FLOAT; dwApply: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x5C}function SetMinDistance(flMinDistance: FLOAT; dwApply: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x60}function SetMode(dwMode, dwApply: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x64}function SetPosition(x, y, z: FLOAT; dwApply: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x68}function SetVelocity(x, y, z: FLOAT; dwApply: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x6C}function SetDistanceFactor(flDistanceFactor: FLOAT; dwApply: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x70}function SetDopplerFactor(flDopplerFactor: FLOAT; dwApply: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x74}function SetRolloffFactor(flRolloffFactor: FLOAT; dwApply: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x78}function SetRolloffCurve(pflPoints: PFLOAT; dwPointCount, dwApply: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x7C}function SetI3DL2Source(pds3db: LPCDSI3DL2BUFFER; dwApply: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x80}function Pause(dwPause: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x84}function PauseEx(rtTimestamp: REFERENCE_TIME; dwPause: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x88}function FlushEx(rtTimeStamp: REFERENCE_TIME; dwFlags: DWORD): HRESULT; virtual; stdcall;
    {VMT 0x8C}function GetVoiceProperties(pVoiceProps: LPDSVOICEPROPS): HRESULT; virtual; stdcall;
  public
    // cached data
    EmuDirectSoundBuffer8: XTL_PIDirectSoundBuffer;
    // TODO -oDxbx: In Cxbx, the following variables start at {0x30} - is that still correct/necessary?
    EmuBuffer: PVOID;
    EmuBufferDesc: PDSBUFFERDESC;
    EmuLockPtr1: PVOID;
    EmuLockBytes1: DWORD;
    EmuLockPtr2: PVOID;
    EmuLockBytes2: DWORD;
    EmuPlayFlags: DWORD;
    EmuFlags: DWORD;
    EmuListener: XTL_PIDirectSoundListener; // Dxbx addition - to support 3D sound emulation through IDirectSound3DListener

  // EmuDirectSound3DBuffer8: XTL_PIDirectSound3DBuffer; // TODO : Initialize this
  end;

  XTL_PIDirectSoundStream = type PInterface;

  LPDIRECTSOUND = type PInterface;

  LPDIRECTSOUNDSTREAM = XTL_PIDirectSoundStream;

  ErrorType = (etCleanup, etWarning);

// Static Variable(s)
var g_pDSound8: XTL_LPDIRECTSOUND8 = NULL;
var g_pDSound8RefCount: int = 0;
var g_pDSoundBufferCache: array [0..SOUNDBUFFER_CACHE_SIZE-1] of PX_CDirectSoundBuffer;
var g_pDSoundStreamCache: array [0..SOUNDSTREAM_CACHE_SIZE-1] of PX_CDirectSoundStream;
var g_bDSoundCreateCalled: Boolean = false; // Dxbx note : Boolean is simpler than Cxbx's int.
var g_SoundVolume: Long = DSBVOLUME_MAX;
{implementation}

const lfUnit = lfCxbx or lfSound;

function iif(const aValue: Boolean; const aTrue, aFalse: DirectSound.PDSBUFFERDESC): DirectSound.PDSBUFFERDESC; overload;
begin
  if aValue then
    Result := aTrue
  else
    Result := aFalse;
end;

function GetSoundError(aResult: HRESULT): String;
begin
  case aResult of
    DSERR_ALLOCATED:
        Result := 'were already being used by another caller';
    DSERR_CONTROLUNAVAIL:
        Result := 'The control (vol, pan, etc.) requested by the caller is not available';
    DSERR_INVALIDPARAM:
        Result := 'An invalid parameter was passed to the returning function';
    DSERR_INVALIDCALL:
        Result := 'This call is not valid for the current state of this object';
    DSERR_GENERIC:
        Result := 'An undetermined error occurred inside the DirectSound subsystem';
    DSERR_PRIOLEVELNEEDED:
        Result := 'The caller does not have the priority level required for the function to succeed';
    DSERR_OUTOFMEMORY:
        Result := 'Not enough free memory is available to complete the operation';
    DSERR_BADFORMAT:
        Result := 'The specified WAVE format is not supported';
    DSERR_UNSUPPORTED:
        Result := 'The function called is not supported at this time';
    DSERR_NODRIVER:
        Result := 'No sound driver is available for use';
    DSERR_ALREADYINITIALIZED:
        Result := 'This object is already initialized';
    DSERR_NOAGGREGATION:
        Result := 'This object does not support aggregation';
    DSERR_BUFFERLOST:
        Result := 'The buffer memory has been lost, and must be restored';
    DSERR_OTHERAPPHASPRIO:
        Result := 'Another app has a higher priority level, preventing this call from succeeding';
    DSERR_UNINITIALIZED:
        Result := 'This object has not been initialized';
    DSERR_NOINTERFACE:
        Result := 'The requested COM interface is not available';
    DSERR_ACCESSDENIED:
        Result := 'Access is denied';
    DSERR_BUFFERTOOSMALL:
        Result := 'Tried to create a DSBCAPS_CTRLFX buffer shorter than DSBSIZE_FX_MIN milliseconds';
    DSERR_DS8_REQUIRED:
        Result := 'Attempt to use DirectSound 8 functionality on an older DirectSound object';
    DSERR_SENDLOOP:
        Result := 'A circular loop of send effects was detected';
    DSERR_BADSENDBUFFERGUID:
        Result := 'The GUID specified in an audiopath file does not match a valid MIXIN buffer';
    DSERR_OBJECTNOTFOUND:
        Result := 'The object requested was not found (numerically equal to DMUS_E_NOT_FOUND)';
    DSERR_FXUNAVAILABLE:
      begin
        Result := 'The effects requested could not be found on the system, or they were found' + #13#10 +
        'but in the wrong order, or in the wrong hardware/software locations.';
      end;
  end;
end;

procedure ShowSoundError(aText: string; aResult: HRESULT; aErrorType: ErrorType);
begin
  case aErrorType of
    etCleanup: DxbxKrnlCleanup(aText + #13#10 + GetsoundError(aResult));
    etWarning: EmuWarning(aText + #13#10 + GetSoundError(aResult));
  end;
end;

// periodically update sound buffers
procedure DxbxHackUpdateSoundBuffers();
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  v: int;
  pAudioPtr, pAudioPtr2: PVOID;
  dwAudioBytes, dwAudioBytes2: DWORD;
  hRet: HRESULT;
begin
  for v := 0 to SOUNDBUFFER_CACHE_SIZE-1 do
  begin
    if (g_pDSoundBufferCache[v] = nil)
    or (g_pDSoundBufferCache[v].EmuDirectSoundBuffer8 = nil) then
      continue;

    // unlock existing lock
    if (g_pDSoundBufferCache[v].EmuLockPtr1 <> nil) then
        IDirectSoundBuffer(g_pDSoundBufferCache[v].EmuDirectSoundBuffer8).
          Unlock(g_pDSoundBufferCache[v].EmuLockPtr1, g_pDSoundBufferCache[v].EmuLockBytes1, g_pDSoundBufferCache[v].EmuLockPtr2, g_pDSoundBufferCache[v].EmuLockBytes2);

    hRet := IDirectSoundBuffer(g_pDSoundBufferCache[v].EmuDirectSoundBuffer8).
      Lock(0, g_pDSoundBufferCache[v].EmuBufferDesc.dwBufferBytes, @pAudioPtr, @dwAudioBytes, @pAudioPtr2, @dwAudioBytes2, 0);

    if (SUCCEEDED(hRet)) then
    begin
      if (g_pDSoundBufferCache[v].EmuBuffer <> nil) then // Dxbx addition
      begin
        if (pAudioPtr <> nil) then
          memcpy(pAudioPtr, g_pDSoundBufferCache[v].EmuBuffer, dwAudioBytes);

        if (pAudioPtr2 <> nil) then
          memcpy(pAudioPtr2, PVOID(DWORD(g_pDSoundBufferCache[v].EmuBuffer)+dwAudioBytes), dwAudioBytes2);
      end;

      IDirectSoundBuffer(g_pDSoundBufferCache[v].EmuDirectSoundBuffer8).Unlock(pAudioPtr, dwAudioBytes, pAudioPtr2, dwAudioBytes2);
    end;

    // TODO -oCXBX: relock old lock ??
  end;
end;


// periodically update sound streams
procedure DxbxHackUpdateSoundStreams();
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  v: int;
  pAudioPtr, pAudioPtr2: PVOID;
  dwAudioBytes, dwAudioBytes2: DWORD;
  hRet: HRESULT;
begin
  for v := 0 to SOUNDSTREAM_CACHE_SIZE-1 do
  begin
    if (g_pDSoundStreamCache[v] = nil)
    or (g_pDSoundStreamCache[v].EmuDirectSoundBuffer8 = nil) then
      continue;

    // unlock existing lock
    if (g_pDSoundStreamCache[v].EmuLockPtr1 <> nil) then
        IDirectSoundBuffer(g_pDSoundStreamCache[v].EmuDirectSoundBuffer8).
          Unlock(g_pDSoundStreamCache[v].EmuLockPtr1, g_pDSoundStreamCache[v].EmuLockBytes1, g_pDSoundStreamCache[v].EmuLockPtr2, g_pDSoundStreamCache[v].EmuLockBytes2);

    hRet := IDirectSoundBuffer(g_pDSoundStreamCache[v].EmuDirectSoundBuffer8).
      Lock(0, g_pDSoundStreamCache[v].EmuBufferDesc.dwBufferBytes, @pAudioPtr, @dwAudioBytes, @pAudioPtr2, @dwAudioBytes2, 0);

    if (SUCCEEDED(hRet)) then
    begin
      if (g_pDSoundStreamCache[v].EmuBuffer <> nil) then // Dxbx addition
      begin
        if (pAudioPtr <> nil) then
          memcpy(pAudioPtr, g_pDSoundStreamCache[v].EmuBuffer, dwAudioBytes);

        if (pAudioPtr2 <> nil) then
          memcpy(pAudioPtr2, PVOID(DWORD(g_pDSoundStreamCache[v].EmuBuffer)+dwAudioBytes), dwAudioBytes2);
      end;

      IDirectSoundBuffer(g_pDSoundStreamCache[v].EmuDirectSoundBuffer8).Unlock(pAudioPtr, dwAudioBytes, pAudioPtr2, dwAudioBytes2);
    end;

    IDirectSoundBuffer(g_pDSoundStreamCache[v].EmuDirectSoundBuffer8).SetCurrentPosition(0);

    if g_XBSound.GetMute then
      IDirectSoundBuffer(g_pDSoundStreamCache[v].EmuDirectSoundBuffer8).SetVolume(DSBVOLUME_MIN)
    else
      IDirectSoundBuffer(g_pDSoundStreamCache[v].EmuDirectSoundBuffer8).SetVolume(g_SoundVolume);


    IDirectSoundBuffer(g_pDSoundStreamCache[v].EmuDirectSoundBuffer8).Play(0, 0, 0);



  end;
end;

// resize an emulated directsound buffer, if necessary
procedure Dxbx_TIDirectSoundBuffer_Resize
(
  pBuffer: PX_CDirectSoundBuffer;
  dwBytes: DWORD
);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  dwPlayCursor: DWORD;
  dwWriteCursor: DWORD;
  dwStatus: DWORD;
  hRet: HRESULT;
begin
  if (dwBytes = pBuffer.EmuBufferDesc.dwBufferBytes) {or (dwBytes = 0)} then // Dxbx addition : Allow resize to zero
    Exit;

  if MayLog(lfUnit) then
    DbgPrintf('Dxbx_TIDirectSoundBuffer_Resize : Resizing! (0x%.08X->0x%.08X)', [pBuffer.EmuBufferDesc.dwBufferBytes, dwBytes]);

  if Assigned(pBuffer.EmuDirectSoundBuffer8) then // Dxbx addition : Allow resize from nil
  begin
    hRet := IDirectSoundBuffer(pBuffer.EmuDirectSoundBuffer8).GetCurrentPosition(@dwPlayCursor, @dwWriteCursor);
    if (FAILED(hRet)) then
      DxbxKrnlCleanup('Unable to retrieve current position for resize reallocation!');

    hRet := IDirectSoundBuffer(pBuffer.EmuDirectSoundBuffer8).GetStatus({out}dwStatus);

    if (FAILED(hRet)) then
      DxbxKrnlCleanup('Unable to retrieve current status for resize reallocation!');

    // release old buffer
    while(IDirectSoundBuffer(pBuffer.EmuDirectSoundBuffer8)._Release() > 0) do begin end;
    pBuffer.EmuDirectSoundBuffer8 := nil; // Dxbx addition : nil out after free
  end;

  pBuffer.EmuBufferDesc.dwBufferBytes := dwBytes;

  if dwBytes > 0 then // Dxbx addition : Allow resize to zero
  begin
    hRet := IDirectSound8(g_pDSound8).CreateSoundBuffer(pBuffer.EmuBufferDesc^, PIDirectSoundBuffer(@(pBuffer.EmuDirectSoundBuffer8)), NULL);

    if (FAILED(hRet)) then
      ShowSoundError('IDirectSoundBuffer8 resize Failed!', hRet, etCleanup);

    IDirectSoundBuffer(pBuffer.EmuDirectSoundBuffer8).SetCurrentPosition(dwPlayCursor);

    if (dwStatus and DSBSTATUS_PLAYING) > 0 then
    begin
      if g_XBSound.GetMute then
        IDirectSoundBuffer(pBuffer.EmuDirectSoundBuffer8).SetVolume(DSBVOLUME_MIN)
      else
        IDirectSoundBuffer(pBuffer.EmuDirectSoundBuffer8).SetVolume(g_SoundVolume);
      IDirectSoundBuffer(pBuffer.EmuDirectSoundBuffer8).Play(0, 0, pBuffer.EmuPlayFlags);
    end
  end;
end;

// resize an emulated directsound stream, if necessary
procedure Dxbx_TIDirectSoundStream_Resize
(
  pStream: PX_CDirectSoundStream;
  dwBytes: DWORD
);
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  dwPlayCursor: DWORD;
  dwWriteCursor: DWORD;
  dwStatus: DWORD;
  hRet: HRESULT;
begin
  if (dwBytes = pStream.EmuBufferDesc.dwBufferBytes) then
    Exit;

  if Assigned(pStream.EmuDirectSoundBuffer8) then // Dxbx addition : Allow resize from nil
  begin
    hRet := IDirectSoundBuffer(pStream.EmuDirectSoundBuffer8).GetCurrentPosition(@dwPlayCursor, @dwWriteCursor);

    if (FAILED(hRet)) then
      DxbxKrnlCleanup('Unable to retrieve current position for resize reallocation!');

    hRet := IDirectSoundBuffer(pStream.EmuDirectSoundBuffer8).GetStatus({out}dwStatus);

    if (FAILED(hRet)) then
      DxbxKrnlCleanup('Unable to retrieve current status for resize reallocation!');

    // release old buffer
    while(IDirectSoundBuffer(pStream.EmuDirectSoundBuffer8)._Release() > 0) do begin end;
    pStream.EmuDirectSoundBuffer8 := nil; // Dxbx addition : nil out after free
  end;

  pStream.EmuBufferDesc.dwBufferBytes := dwBytes;

  if dwBytes > 0 then // Dxbx addition : Allow resize to zero
  begin
    hRet := IDirectSound8(g_pDSound8).CreateSoundBuffer(pStream.EmuBufferDesc^, PIDirectSoundBuffer(@(pStream.EmuDirectSoundBuffer8)), NULL);

    if (FAILED(hRet)) then
      ShowSoundError('IDirectSoundBuffer8 resize Failed!', hRet, etCleanup);

    IDirectSoundBuffer(pStream.EmuDirectSoundBuffer8).SetCurrentPosition(dwPlayCursor);

    if (dwStatus and DSBSTATUS_PLAYING) > 0 then
    begin
      if g_XBSound.GetMute then
        IDirectSoundBuffer(pStream.EmuDirectSoundBuffer8).SetVolume(DSBVOLUME_MIN)
      else
        IDirectSoundBuffer(pStream.EmuDirectSoundBuffer8).SetVolume(g_SoundVolume);
      IDirectSoundBuffer(pStream.EmuDirectSoundBuffer8).Play(0, 0, pStream.EmuPlayFlags);
    end;
  end;
end;


function DxbxAssureDirectSoundCreate(const FromOriginalDSoundCreate: Boolean = False): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  v: int;
begin
  Result := DS_OK;

  g_EmuShared.GetXBSound(@g_XBSound);

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
    Result := DirectSoundCreate8(NULL, @g_pDSound8, NULL);
    if FAILED(Result) then
      ShowSoundError('DirectSoundCreate8 Failed!', Result, etCleanup);

    Result := IDirectSound8(g_pDSound8).SetCooperativeLevel(g_hEmuWindow, DSSCL_PRIORITY);
    if FAILED(Result) then
      ShowSoundError('g_pDSound8->SetCooperativeLevel Failed!', Result, etCleanup);

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

//
// TXMediaObject - no patches, but a real class implementation as a VMT-replacement for the Xbox version
//

function TXMediaObject.AddRef(): ULONG; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : XMediaObject.AddRef').
      _(Self, 'pXMediaObject').
    LogEnd();

//  if (Self <> nil) then
//    if (Self.EmuDirectSoundBuffer8 <> nil) then // Cxbx HACK: Ignore unsupported codecs.
//      IDirectSoundBuffer(Self.EmuDirectSoundBuffer8)._AddRef();

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TXMediaObject.Release(): ULONG; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : XMediaObject.Release').
      _(Self, 'pXMediaObject').
    LogEnd();

  EmuSwapFS(fsXbox);

  Result := 0;
end;

function TXMediaObject.GetInfo
(
    pInfo: LPXMEDIAINFO
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : XMediaObject.GetInfo').
      _(Self, 'pXMediaObject').
      _(pInfo, 'pInfo').
    LogEnd();

  // TODO -oDXBX: A (real) implementation?
  EmuWarning('XMediaObject.GetInfo is not yet supported!');

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TXMediaObject.GetStatus
(
    pdwStatus: PDWORD
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : XMediaObject.GetStatus').
      _(Self, 'pXMediaObject').
      _(pdwStatus, 'pdwStatus').
    LogEnd();

  Unimplemented('XMediaObject.GetStatus');

  pdwStatus^ := DSBSTATUS_PLAYING;

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TXMediaObject.Process
(
    pInputBuffer: PXMEDIAPACKET;
    pOutputBuffer: PXMEDIAPACKET
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : XMediaObject.Process').
      _(Self, 'pXMediaObject').
      _(pInputBuffer, 'pInputBuffer').
      _(pOutputBuffer, 'pOutputBuffer').
    LogEnd();

  // TODO -oDXBX: Actually Process

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TXMediaObject.Discontinuity(): HRESULT; stdcall; // virtual;
// Was Dummy_0x10
// Branch:shogun  Revision:163  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : XMediaObject.Discontinuity').
      _(Self, 'pXMediaObject').
    LogEnd();

{$IFDEF GAME_HACKS_ENABLED}
  // Causes deadlock in Halo...
  // TODO -oCxbx: Verify that this is a Vista related problem (I HATE Vista!)
  //    EmuWarning('EmuCMcpxStream_Dummy_0x10 is ignored!');
{$ENDIF}

  // TODO -oDXBX: Actually Process

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TXMediaObject.Flush(): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : XMediaObject.Flush').
      _(Self, 'pXMediaObject').
    LogEnd();

  // TODO -oDXBX: Actually Flush

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

// Patches on DirectSound* functions

function XTL_EmuDirectSoundCreate
(
    pguidDeviceId: LPGUID;
    ppDirectSound: XTL_PLPDIRECTSOUND8;
    pUnkOuter: LPUNKNOWN
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : DirectSoundCreate').
      _(pguidDeviceId, 'pguidDeviceId').
      _(ppDirectSound, 'ppDirectSound').
      _(pUnkOuter, 'pUnkOuter').
    LogEnd();

  Result := DS_OK;

  // Set this flag when this function is called
  g_bDSoundCreateCalled := true;

  DxbxAssureDirectSoundCreate(True); // Dxbx addition - use one implementation for DirectSoundCreate

  // This way we can be sure that this function returns a valid
  // DirectSound8 pointer even if we initialized it elsewhere!
  if (nil=ppDirectSound^) and Assigned(g_pDSound8) then
    ppDirectSound^ := g_pDSound8;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuDirectSoundCreateBuffer
(
    pdsbd: PX_DSBUFFERDESC;
    ppBuffer: PPX_CDirectSoundBuffer
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
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

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : DirectSoundCreateBuffer').
      _(pdsbd, 'pdsbd').
      _(ppBuffer, 'ppBuffer').
    LogEnd();

  dwEmuFlags := 0;


  // Dxbx note : When and how should we create a IDirectSound3DBuffer ?
  // It will be needed to emulate SetConeAngles and other functions!

  pDSBufferDesc := DirectSound.PDSBUFFERDESC(XboxAlloc(sizeof(DSBUFFERDESC)));
  pDSBufferDescSpecial := NULL; // Dxbx not : Prevent W1036 Variable might not have been initialized
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
      pDSBufferDesc.lpwfxFormat := {PWAVEFORMATEX}XboxAlloc(sizeof(WAVEFORMATEX) + pdsbd.lpwfxFormat.cbSize);
      memcpy(pDSBufferDesc.lpwfxFormat, pdsbd.lpwfxFormat, sizeof(WAVEFORMATEX));

      if (pDSBufferDesc.lpwfxFormat.wFormatTag = X_WAVE_FORMAT_XBOX_ADPCM) then
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

      pDSBufferDescSpecial := DirectSound.PDSBUFFERDESC(XboxAlloc(sizeof(DSBUFFERDESC)));
      pDSBufferDescSpecial.lpwfxFormat := PWAVEFORMATEX(XboxAlloc(sizeof(WAVEFORMATEX)));

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
//    pDSBufferDesc.lpwfxFormat := (WAVEFORMATEX*)XboxAlloc(sizeof(WAVEFORMATEX)/*+pdsbd.lpwfxFormat.cbSize*/);

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
  ppBuffer^ := TIDirectSoundBuffer.Create;
  ppBuffer^.EmuDirectSoundBuffer8 := nil;
  ppBuffer^.EmuBuffer := nil;
  ppBuffer^.EmuBufferDesc := iif(bIsSpecial, pDSBufferDescSpecial, pDSBufferDesc);
  ppBuffer^.EmuLockPtr1 := nil;
  ppBuffer^.EmuLockBytes1 := 0;
  ppBuffer^.EmuLockPtr2 := nil;
  ppBuffer^.EmuLockBytes2 := 0;
  ppBuffer^.EmuFlags := dwEmuFlags;
  ppBuffer^.EmuListener := nil; // Dxbx addition : Prevent automatic interface release-problems on non-nil!

  if MayLog(lfUnit) then
    DbgPrintf('EmuDSound : DirectSoundCreateBuffer, *ppBuffer := 0x%.08X, bytes := 0x%.08X', [ppBuffer^, pDSBufferDesc.dwBufferBytes]);

  DxbxAssureDirectSoundCreate(); // Dxbx addition - use one implementation for DirectSoundCreate8

  hRet := IDirectSound8(g_pDSound8).CreateSoundBuffer(iif(bIsSpecial, pDSBufferDescSpecial, pDSBufferDesc)^, @(ppBuffer^.EmuDirectSoundBuffer8), NULL);

  if (FAILED(hRet)) then
  begin
    ShowSoundError('CreateSoundBuffer Failed!', hRet, etWarning);
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
      EmuWarning('QueryInterface Failed!');
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

function XTL_EmuDirectSoundCreateStream
(
    pdssd: PX_DSSTREAMDESC;
    ppStream: PPX_CDirectSoundStream
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:161  Translator:PatrickvL  Done:100
var
  pDSBufferDesc: DirectSound.PDSBUFFERDESC;
  dwAcceptableMask: DWORD;
  hRet: HRESULT;
  v: int;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : DirectSoundCreateStream').
      _(pdssd, 'pdssd').
      _(ppStream, 'ppStream').
    LogEnd();

  // TODO -oCXBX: Garbage Collection
  ppStream^ := TIDirectSoundStream.Create;
  pDSBufferDesc := DirectSound.PDSBUFFERDESC(XboxAlloc(sizeof(DSBUFFERDESC)));

  // convert from Xbox to PC DSound
  begin
    dwAcceptableMask := X_DSBCAPS_CTRL3D or X_DSBCAPS_CTRLFREQUENCY or X_DSBCAPS_CTRLVOLUME or X_DSBCAPS_CTRLPOSITIONNOTIFY or X_DSBCAPS_MIXIN or X_DSBCAPS_LOCDEFER;

    if (pdssd.dwFlags and (not dwAcceptableMask)) > 0 then
        EmuWarning('Use of unsupported pdssd.dwFlags mask(s) (0x%.08X)', [pdssd.dwFlags and (not dwAcceptableMask)]);

    pDSBufferDesc.dwSize := sizeof(DSBUFFERDESC);
    pDSBufferDesc.dwFlags := (pdssd.dwFlags and dwAcceptableMask) or DSBCAPS_CTRLVOLUME or DSBCAPS_GETCURRENTPOSITION2;
    pDSBufferDesc.dwBufferBytes := DSBSIZE_MIN;

    pDSBufferDesc.dwReserved := 0;

    if (pdssd.lpwfxFormat <> NULL) then
    begin
      pDSBufferDesc.lpwfxFormat := PWAVEFORMATEX(XboxAlloc(sizeof(WAVEFORMATEX)));
      memcpy(pDSBufferDesc.lpwfxFormat, pdssd.lpwfxFormat, sizeof(WAVEFORMATEX));
    end;

    pDSBufferDesc.guid3DAlgorithm := DS3DALG_DEFAULT;

    if (pDSBufferDesc.lpwfxFormat <> NULL) and (pDSBufferDesc.lpwfxFormat.wFormatTag <> WAVE_FORMAT_PCM) then
    begin
      EmuWarning('Invalid WAVE_FORMAT!');
      if (pDSBufferDesc.lpwfxFormat.wFormatTag = X_WAVE_FORMAT_XBOX_ADPCM) then
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

  if MayLog(lfUnit) then
    DbgPrintf('EmuDSound : DirectSoundCreateStream, *ppStream := 0x%.08X', [ppStream^]);

  DxbxAssureDirectSoundCreate(); // Dxbx addition - use one implementation for DirectSoundCreate8

  hRet := IDirectSound8(g_pDSound8).CreateSoundBuffer(pDSBufferDesc^, PIDirectSoundBuffer(@(ppStream^.EmuDirectSoundBuffer8)), NULL);

  if (FAILED(hRet)) then
    ShowSoundError('CreateSoundBuffer Failed!', hRet, etWarning);

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

procedure XTL_EmuDirectSoundDoWork(); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuDSound : DirectSoundDoWork();');

  DxbxHackUpdateSoundBuffers();
  DxbxHackUpdateSoundStreams();

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuDirectSoundUseFullHRTF(); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuDSound : DirectSoundUseFullHRTF();');

  // TODO -oCXBX: Actually implement this

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuDirectSoundUseLightHRTF(); stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuDSound : DirectSoundUseLightHRTF();');

  // TODO -oDxbx: Actually implement this

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuDirectSoundUseFullHRTF4Channel(); stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuDSound : DirectSoundUseFullHRTF4Channel();');

  // TODO -oDxbx: Actually implement this

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuDirectSoundUseLightHRTF4Channel(); stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuDSound : DirectSoundUseLightHRTF4Channel();');

  // TODO -oDxbx: Actually implement this

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuDirectSoundOverrideSpeakerConfig(
  dwSpeakerConfig: DWORD
); stdcall;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuDSound : DirectSoundOverrideSpeakerConfig();');

  // TODO -oDxbx: Actually implement this

  EmuSwapFS(fsXbox);
end;

{static} var GetSampleTime_Start: LARGE_INTEGER = (QuadPart:0);
function XTL_EmuDirectSoundGetSampleTime(): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  Delta: LARGE_INTEGER;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuDSound : DirectSoundGetSampleTime();');

  // Originally, this function accesses the NVIDIA SoundStorm APU
  // register directly (0xFE80200C).
  // The SDK documentation says this sample counter updates with a frequency
  // of 48 KHz, and could reset unexpectedly. It relies on the existence of
  // the DirectSound object.

  // TODO -oCXBX: Wait until a DirectSoundBuffer/Stream is being played?
  if GetSampleTime_Start.QuadPart = 0 then ReadSystemTimeIntoLargeInteger(xboxkrnl_KeInterruptTimePtr, @GetSampleTime_Start);

  ReadSystemTimeIntoLargeInteger(xboxkrnl_KeInterruptTimePtr, @Delta);
  Delta.QuadPart := Delta.QuadPart - GetSampleTime_Start.QuadPart;

  // Handle reset when underflow occurs :
  if Delta.QuadPart < 0 then
  begin
    GetSampleTime_Start.QuadPart := 0;
    Result := 0;
  end
  else
  begin
    // Convert "units of approximately 100 nanoseconds" to 48 KHz :
    Result := DWORD((Delta.QuadPart * 48000) div (10 * 1000 * 1000));
  end;

  EmuSwapFS(fsXbox);
end;

//
// TIDirectSound - no patches, but a real class implementation as a VMT-replacement for the Xbox version
//

function TIDirectSound.AddRef(): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  uRet: ULONG;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSound.AddRef').
      _(Self, 'pDirectSound').
    LogEnd();

  uRet := g_pDSound8RefCount; Inc(g_pDSound8RefCount);

  EmuSwapFS(fsXbox);

  Result := uRet;
end;

function TIDirectSound.Release(): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  uRet: ULONG;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSound.Release').
      _(Self, 'pDirectSound').
    LogEnd();

  uRet := g_pDSound8RefCount; Dec(g_pDSound8RefCount);

  (* temporarily (?) disabled by cxbx
  if (uRet = 1) then
    Self._Release();
  *)

  EmuSwapFS(fsXbox);

  Result := uRet;
end;

function TIDirectSound.GetCaps
(
    pdsc: LPDSCAPS
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  DSCapsPC: DSCAPS;
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSound.GetCaps').
      _(Self, 'pDirectSound').
      _(pdsc, 'pDSCaps').
    LogEnd();

  DxbxAssureDirectSoundCreate(); // Dxbx addition - use one implementation for DirectSoundCreate8

  // Get PC's DirectSound capabilities
  ZeroMemory(@DSCapsPC, sizeof(DSCAPS));
  DSCapsPC.dwSize := sizeof(DSCAPS);

  hRet := IDirectSound8(g_pDSound8).GetCaps({out}DSCapsPC);
  if(FAILED(hRet)) then
    EmuWarning('Failed to get PC DirectSound caps!');

  // Convert PC -> Xbox
  if Assigned(pdsc) then
  begin
    // WARNING: This may not be accurate under Windows Vista...
    pdsc.dwFree2DBuffers := DSCapsPC.dwFreeHwMixingAllBuffers;
    pdsc.dwFree3DBuffers := DSCapsPC.dwFreeHw3DAllBuffers;
    pdsc.dwFreeBufferSGEs := 256;              // TODO -oCXBX: Verify max on a real Xbox
    pdsc.dwMemoryAllocated := DSCapsPC.dwFreeHwMemBytes;  // TODO -oCXBX: Bytes or MegaBytes?
  end;

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSound.CreateSoundBuffer
(
    pdsbd: PX_DSBUFFERDESC;
    ppBuffer: PPX_CDirectSoundBuffer;
    pUnkOuter: LPUNKNOWN
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuDSound : TIDirectSound.CreateSoundBuffer').
      _(Self, 'pDirectSound').
      _(pdsbd, 'pdsbd').
      _(ppBuffer, 'ppBuffer').
      _(pUnkOuter, 'pUnkOuter').
    LogEnd();

  EmuSwapFS(fsXbox);

  Result := XTL_EmuDirectSoundCreateBuffer(pdsbd, ppBuffer);
end;

function TIDirectSound.CreateSoundStream
(
    pdssd: LPCDSSTREAMDESC;
    ppStream: PLPDIRECTSOUNDSTREAM;
    pUnkOuter: LPUNKNOWN
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuDSound : TIDirectSound.CreateSoundStream').
      _(Self, 'pDirectSound').
      _(pdssd, 'pdssd').
      _(ppStream, 'ppStream').
      _(pUnkOuter, 'pUnkOuter').
    LogEnd();

  EmuSwapFS(fsXbox);

  Result := XTL_EmuDirectSoundCreateStream(pdssd, ppStream);
end;

function TIDirectSound.GetSpeakerConfig
(
    pdwSpeakerConfig: LPDWORD
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSound.GetSpeakerConfig').
      _(Self, 'pDirectSound').
      _(pdwSpeakerConfig, 'pdwSpeakerConfig').
    LogEnd();

  Result := IDirectSound(g_pDSound8).GetSpeakerConfig({out}pdwSpeakerConfig^);

  // Convert return value from native to Xbox :
  if Result = DS_OK then
    case pdwSpeakerConfig^ of
      DSSPEAKER_DIRECTOUT:
        pdwSpeakerConfig^ := X_DSSPEAKER_USE_DEFAULT; // ??
      DSSPEAKER_HEADPHONE:
        pdwSpeakerConfig^ := X_DSSPEAKER_STEREO;
      DSSPEAKER_MONO:
        pdwSpeakerConfig^ := X_DSSPEAKER_MONO;
      DSSPEAKER_QUAD: // Not supported on Xbox
        pdwSpeakerConfig^ := X_DSSPEAKER_STEREO; // ??
      DSSPEAKER_STEREO:
        pdwSpeakerConfig^ := X_DSSPEAKER_STEREO;
      DSSPEAKER_SURROUND:
        pdwSpeakerConfig^ := X_DSSPEAKER_SURROUND;
      // DSSPEAKER_5POINT1: // obsolete 5.1 setting
      // DSSPEAKER_7POINT1: // obsolete 7.1 setting
      DSSPEAKER_7POINT1_SURROUND: // correct 7.1 Home Theater setting
        pdwSpeakerConfig^ := X_DSSPEAKER_SURROUND or X_DSSPEAKER_ENABLE_DTS; // ??
      DSSPEAKER_7POINT1_WIDE: // = DSSPEAKER_7POINT1;
        pdwSpeakerConfig^ := X_DSSPEAKER_STEREO or X_DSSPEAKER_ENABLE_DTS; // ??
      DSSPEAKER_5POINT1_SURROUND: // correct 5.1 setting
        pdwSpeakerConfig^ := X_DSSPEAKER_SURROUND or X_DSSPEAKER_ENABLE_AC3; // ??
      DSSPEAKER_5POINT1_BACK: // = DSSPEAKER_5POINT1;
        pdwSpeakerConfig^ := X_DSSPEAKER_STEREO or X_DSSPEAKER_ENABLE_AC3; // ??
    else
      pdwSpeakerConfig^ := X_DSSPEAKER_STEREO;
    end;

  EmuSwapFS(fsXbox);
end;

function TIDirectSound.SetCooperativeLevel
(
    hWnd: HWND;
    dwLevel: DWORD
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:Shadow_tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSound.SetCooperativeLevel').
      _(Self, 'pDirectSound').
      _(hWnd, 'hWnd').
      _(dwLevel, 'dwLevel').
    LogEnd();

  // TODO -oDxbx : Should we allow this call? And must we pass hWnd or g_hEmuWindow ?
  Result := IDirectSound8(g_pDSound8).SetCooperativeLevel(hWnd, dwLevel);

  EmuSwapFS(fsXbox);
end;

function TIDirectSound.Compact(): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:Shadow_tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSound.Compact').
      _(Self, 'pDirectSound').
    LogEnd();

  Result := IDirectSound8(g_pDSound8).Compact;

  EmuSwapFS(fsXbox);
end;

function TIDirectSound.DownloadEffectsImage
(
    pvImageBuffer: LPCVOID;
    dwImageSize: DWORD;
    pImageLoc: LPCDSEFFECTIMAGELOC; // TODO -oCXBX: Use this param
    ppImageDesc: PLPDSEFFECTIMAGEDESC // TODO -oCXBX: Use this param
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSound.DownloadEffectsImage').
      _(Self, 'pDirectSound').
      _(pvImageBuffer, 'pvImageBuffer').
      _(dwImageSize, 'dwImageSize').
      _(pImageLoc, 'pImageLoc').
      _(ppImageDesc, 'ppImageDesc').
    LogEnd();

  // TODO -oCXBX: Actually implement this

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSound.GetEffectData
(
    dwEffectIndex: DWORD;
    dwOffset: DWORD;
    pvData: LPVOID;
    dwDataSize: DWORD
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('TIDirectSound.GetEffectData');

  EmuSwapFS(fsXbox);
end;

function TIDirectSound.SetEffectData
(
  dwEffectIndex: DWORD;
  dwOffset: DWORD;
  pvData: LPCVOID;
  dwDataSize: DWORD;
  dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('TIDirectSound.SetEffectData');

  EmuSwapFS(fsXbox);
end;

function TIDirectSound.CommitEffectData(): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('TIDirectSound.CommitEffectData');

  EmuSwapFS(fsXbox);
end;

function TIDirectSound.EnableHeadphones
(
    fEnabled: BOOL
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSound.EnableHeadphones').
      _(Self, 'pDirectSound').
      _(fEnabled, 'fEnabled').
    LogEnd();

  EmuWarning('EmuIDirectSound_EnableHeadphones ignored');

// TODO -oDxbx : Try this :
(*
  if fEnabled then
    Result := IDirectSound(g_pDSound8).SetSpeakerConfig(DSSPEAKER_HEADPHONE)
  else
    Result := IDirectSound(g_pDSound8).SetSpeakerConfig(DSSPEAKER_STEREO);
*)

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSound.SetMixBinHeadroom
(
    dwMixBin: DWORD;
    dwHeadroom: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuDSound : TIDirectSound.SetMixBinHeadroom').
      _(Self, 'pDirectSound').
      _(dwMixBin, 'dwMixBinMask').
      _(dwHeadroom, 'dwHeadroom').
    LogEnd();

  // TODO -oCXBX: Actually do something

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSound.SetAllParameters
(
    pds3dl: LPCDS3DLISTENER;
    dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuDSound : TIDirectSound.SetAllParameters').
      _(Self, 'pDirectSound').
      _(pds3dl, 'pds3dl').
      _(dwApply, 'dwApply').
    LogEnd();

  // TODO -oCXBX: Actually do something

  // TODO -oDxbx : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  //IDirectSound3DListener(Self.EmuListener).SetAllParameters(nil{???}, dwApply);

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSound.SetOrientation
(
    xFront: FLOAT;
    yFront: FLOAT;
    zFront: FLOAT;
    xTop: FLOAT;
    yTop: FLOAT;
    zTop: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSound.SetOrientation').
      _(Self, 'pDirectSound').
      _(xFront, 'xFront').
      _(yFront, 'yFront').
      _(zFront, 'zFront').
      _(xTop, 'xTop').
      _(yTop, 'yTop').
      _(zTop, 'zTop').
      _(dwApply, 'dwApply').
    LogEnd();

  // TODO -oCXBX: Actually implement this

  // TODO -oDxbx : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  // IDirectSound3DListener(Self.EmuListener).SetOrientation(xFront, yFront, zFront, xTop, yTop, zTop, dwApply);

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSound.SetPosition
(
    x: FLOAT;
    y: FLOAT;
    z: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuDSound : TIDirectSound.SetPosition').
      _(Self, 'pDirectSound').
      _(x, 'x').
      _(y, 'y').
      _(z, 'z').
      _(dwApply, 'dwApply').
    LogEnd();

  // TODO -oCXBX: Actually do something

  // TODO -oDxbx : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  // IDirectSound3DListener(Self.EmuListener).SetPosition(x, y, z, dwApply);

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSound.SetVelocity
(
    x: FLOAT;
    y: FLOAT;
    z: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuDSound : TIDirectSound.SetVelocity').
      _(Self, 'pDirectSound').
      _(x, 'x').
      _(y, 'y').
      _(z, 'z').
      _(dwApply, 'dwApply').
    LogEnd();

  // TODO -oCXBX: Actually do something

  // TODO -oDxbx : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  // IDirectSound3DListener(Self.EmuListener).SetVelocity(x, y, z, dwApply);

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSound.SetDistanceFactor
(
    flDistanceFactor: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSound.SetDistanceFactor').
      _(Self, 'pDirectSound').
      _(flDistanceFactor, 'flDistanceFactor').
      _(dwApply, 'dwApply').
    LogEnd();

  // TODO -oCXBX: Actually implement this

  // TODO -oDxbx : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  // IDirectSound3DListener(Self.EmuListener).SetDistanceFactor(fDistanceFactor, dwApply);

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSound.SetDopplerFactor
(
    flDopplerFactor: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSound.SetDopplerFactor').
      _(Self, 'pDirectSound').
      _(flDopplerFactor, 'flDopplerFactor').
      _(dwApply, 'dwApply').
    LogEnd();

  // TODO -oCXBX: Actually implement this

  // TODO -oDxbx : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  // IDirectSound3DListener(Self.EmuListener).SetDopplerFactor(fDopplerFactor, dwApply);

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSound.SetRolloffFactor
(
    flRolloffFactor: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSound.SetRolloffFactor').
      _(Self, 'pDirectSound').
      _(flRolloffFactor, 'flRolloffFactor').
      _(dwApply, 'dwApply').
    LogEnd();

  // TODO -oCXBX: Actually implement this

  // TODO -oDxbx : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  // IDirectSound3DListener(Self.EmuListener).SetRolloffFactor(fRolloffFactor, dwApply);

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSound.SetI3DL2Listener
(
    pds3dl: LPCDSI3DL2LISTENER;
    dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuDSound : TIDirectSound.SetI3DL2Listener').
      _(Self, 'pDirectSound').
      _(pds3dl, 'pds3dl').
      _(dwApply, 'dwApply').
    LogEnd();

  // TODO -oCXBX: Actually do something

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSound.CommitDeferredSettings(): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSound.CommitDeferredSettings').
      _(Self, 'pDirectSound').
    LogEnd();

  // TODO -oCXBX: Translate params, then make the PC DirectSound call

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSound.GetTime
(
    prtCurrent: PREFERENCE_TIME
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:100
var
  CurrentTime: LARGE_INTEGER;
begin
  EmuSwapFS(fsWindows);

  if Assigned(prtCurrent) then
  begin
    // Dxbx note : The SDK documentation says "time returned by the master clock is a 64-bit value"
    // "measured in units of approximately 100 nanoseconds", which reminds me of the InterruptTimer.
    // So just return that for now :
    ReadSystemTimeIntoLargeInteger(xboxkrnl_KeInterruptTimePtr, @CurrentTime);
    prtCurrent^ := CurrentTime.QuadPart;
  end;

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSound.GetOutputLevels
(
    pOutputLevels: LPDSOUTPUTLEVELS;
    fResetPeakValues: BOOL
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSound.GetOutputLevels').
      _(Self, 'pDirectSound').
      _(pOutputLevels, 'pOutputLevels').
      _(fResetPeakValues, 'bResetPeakValues').
    LogEnd();

  // TODO -oCXBX: Anything?  Either way, I've never seen a game to date use this...

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSound.SynchPlayback(): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSound.SynchPlayback').
      _(Self, 'pDirectSound').
    LogEnd();

  EmuWarning('EmuIDirectSound_SynchPlayback ignored');

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

//
// Patches on IDirectSound_* functions (mimicking an interface, but still being plain C functions)
//

function XTL_EmuIDirectSound_QueryInterfaceC
(
    pThis: XTL_LPDIRECTSOUND8
    // MISSING_ARGUMENTS!
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('TIDirectSound.QueryInterfaceC');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirectSound_QueryInterface
(
    pThis: XTL_LPDIRECTSOUND8
    // MISSING_ARGUMENTS!
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('TIDirectSound.QueryInterface');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirectSound_AddRef;
asm  jmp TIDirectSound.AddRef; end;

procedure XTL_EmuIDirectSound_Release;
asm jmp TIDirectSound.Release; end;

procedure XTL_EmuIDirectSound_GetCaps;
asm jmp TIDirectSound.GetCaps; end;

procedure XTL_EmuIDirectSound_CreateSoundBuffer;
asm jmp TIDirectSound.CreateSoundBuffer; end;

procedure XTL_EmuIDirectSound_CreateSoundStream;
asm jmp TIDirectSound.CreateSoundStream; end;

procedure XTL_EmuIDirectSound_GetSpeakerConfig;
asm jmp TIDirectSound.GetSpeakerConfig; end;

procedure XTL_EmuIDirectSound_SetCooperativeLevel;
asm jmp TIDirectSound.SetCooperativeLevel; end;

procedure XTL_EmuIDirectSound_Compact;
asm jmp TIDirectSound.Compact; end;

procedure XTL_EmuIDirectSound_DownloadEffectsImage;
asm jmp TIDirectSound.DownloadEffectsImage; end;

procedure XTL_EmuIDirectSound_GetEffectData;
asm jmp TIDirectSound.GetEffectData; end;

procedure XTL_EmuIDirectSound_SetEffectData;
asm jmp TIDirectSound.SetEffectData; end;

procedure XTL_EmuIDirectSound_CommitEffectData;
asm jmp TIDirectSound.CommitEffectData; end;

procedure XTL_EmuIDirectSound_EnableHeadphones;
asm jmp TIDirectSound.EnableHeadphones; end;

procedure XTL_EmuIDirectSound_SetMixBinHeadroom;
asm jmp TIDirectSound.SetMixBinHeadroom; end;

procedure XTL_EmuIDirectSound_SetAllParameters;
asm jmp TIDirectSound.SetAllParameters; end;

procedure XTL_EmuIDirectSound_SetOrientation;
asm jmp TIDirectSound.SetOrientation; end;

procedure XTL_EmuIDirectSound_SetPosition;
asm jmp TIDirectSound.SetPosition; end;

procedure XTL_EmuIDirectSound_SetVelocity;
asm jmp TIDirectSound.SetVelocity; end;

procedure XTL_EmuIDirectSound_SetDistanceFactor;
asm jmp TIDirectSound.SetDistanceFactor; end;

procedure XTL_EmuIDirectSound_SetDopplerFactor;
asm jmp TIDirectSound.SetDopplerFactor; end;

procedure XTL_EmuIDirectSound_SetRolloffFactor;
asm jmp TIDirectSound.SetRolloffFactor; end;

procedure XTL_EmuIDirectSound_SetI3DL2Listener;
asm jmp TIDirectSound.SetI3DL2Listener; end;

procedure XTL_EmuIDirectSound_CommitDeferredSettings;
asm jmp TIDirectSound.CommitDeferredSettings; end;

procedure XTL_EmuIDirectSound_GetTime;
asm jmp TIDirectSound.GetTime; end;

procedure XTL_EmuIDirectSound_GetOutputLevels;
asm jmp TIDirectSound.GetOutputLevels; end;

procedure XTL_EmuIDirectSound_SynchPlayback;
asm jmp TIDirectSound.SynchPlayback; end;

//
// Patches on CDirectSound class functions
//

procedure XTL_EmuDirectSound_CDirectSound_AddRef;
asm  jmp TIDirectSound.AddRef; end;

procedure XTL_EmuDirectSound_CDirectSound_Release;
asm jmp TIDirectSound.Release; end;

procedure XTL_EmuDirectSound_CDirectSound_GetCaps;
asm jmp TIDirectSound.GetCaps; end;

procedure XTL_EmuDirectSound_CDirectSound_CreateSoundBuffer;
asm jmp TIDirectSound.CreateSoundBuffer; end;

procedure XTL_EmuDirectSound_CDirectSound_CreateSoundStream;
asm jmp TIDirectSound.CreateSoundStream; end;

procedure XTL_EmuDirectSound_CDirectSound_GetSpeakerConfig;
asm jmp TIDirectSound.GetSpeakerConfig; end;

procedure XTL_EmuDirectSound_CDirectSound_SetCooperativeLevel;
asm jmp TIDirectSound.SetCooperativeLevel; end;

procedure XTL_EmuDirectSound_CDirectSound_Compact;
asm jmp TIDirectSound.Compact; end;

procedure XTL_EmuDirectSound_CDirectSound_DownloadEffectsImage;
asm jmp TIDirectSound.DownloadEffectsImage; end;

procedure XTL_EmuDirectSound_CDirectSound_GetEffectData;
asm jmp TIDirectSound.GetEffectData; end;

procedure XTL_EmuDirectSound_CDirectSound_SetEffectData;
asm jmp TIDirectSound.SetEffectData; end;

procedure XTL_EmuDirectSound_CDirectSound_CommitEffectData;
asm jmp TIDirectSound.CommitEffectData; end;

procedure XTL_EmuDirectSound_CDirectSound_EnableHeadphones;
asm jmp TIDirectSound.EnableHeadphones; end;

procedure XTL_EmuDirectSound_CDirectSound_SetMixBinHeadroom;
asm jmp TIDirectSound.SetMixBinHeadroom; end;

procedure XTL_EmuDirectSound_CDirectSound_SetAllParameters;
asm jmp TIDirectSound.SetAllParameters; end;

procedure XTL_EmuDirectSound_CDirectSound_SetOrientation;
asm jmp TIDirectSound.SetOrientation; end;

procedure XTL_EmuDirectSound_CDirectSound_SetPosition;
asm jmp TIDirectSound.SetPosition; end;

procedure XTL_EmuDirectSound_CDirectSound_SetVelocity;
asm jmp TIDirectSound.SetVelocity; end;

procedure XTL_EmuDirectSound_CDirectSound_SetDistanceFactor;
asm jmp TIDirectSound.SetDistanceFactor; end;

procedure XTL_EmuDirectSound_CDirectSound_SetDopplerFactor;
asm jmp TIDirectSound.SetDopplerFactor; end;

procedure XTL_EmuDirectSound_CDirectSound_SetRolloffFactor;
asm jmp TIDirectSound.SetRolloffFactor; end;

procedure XTL_EmuDirectSound_CDirectSound_SetI3DL2Listener;
asm jmp TIDirectSound.SetI3DL2Listener; end;

procedure XTL_EmuDirectSound_CDirectSound_CommitDeferredSettings;
asm jmp TIDirectSound.CommitDeferredSettings; end;

procedure XTL_EmuDirectSound_CDirectSound_GetTime;
asm jmp TIDirectSound.GetTime; end;


procedure XTL_EmuDirectSound_CDirectSound_GetOutputLevels;
asm jmp TIDirectSound.GetOutputLevels; end;

procedure XTL_EmuDirectSound_CDirectSound_SynchPlayback;
asm jmp TIDirectSound.SynchPlayback; end;

//
// Patches on IDirectSoundBuffer_* functions (mimicking an interface, but still being plain C functions)
//

function XTL_EmuIDirectSoundBuffer_QueryInterfaceC
(
    pThis: XTL_LPDIRECTSOUND8
    // MISSING_ARGUMENTS!
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('TIDirectSoundBuffer.QueryInterfaceC');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirectSoundBuffer_QueryInterface
(
    pThis: XTL_LPDIRECTSOUND8
    // MISSING_ARGUMENTS!
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('TIDirectSoundBuffer.QueryInterface');

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.AddRef(): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.AddRef').
      _(Self, 'pBuffer').
    LogEnd();

  Result := 0;

  if (Self <> nil) then
  begin
    // HACK: Skip this on unsupported flags
    if(Self.EmuFlags and DSB_FLAG_RECIEVEDATA) > 0 then
    begin
      EmuWarning('Not adding reference to a potentially bad pointer!');
    end
    else
    begin
      if(Self.EmuDirectSoundBuffer8 <> nil) then // HACK: Ignore unsupported codecs.
        Result := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8)._AddRef();
    end;
  end;

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.Release(): ULONG; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  uRet: ULONG;
  v: int;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.Release').
      _(Self, 'pBuffer').
    LogEnd();

  //TODO DXBX: - This is not good to release... it crashes rayamn menu items hard.
  uRet := 0;
  if (Self <> nil) then
  begin
    if (0=(Self.EmuFlags and DSB_FLAG_RECIEVEDATA)) then
    begin
        if Assigned(Self.EmuDirectSoundBuffer8) then
          uRet := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8)._Release();

        if (uRet = 0) then
        begin
          Self.EmuDirectSoundBuffer8 := nil; // Dxbx addition : nil out after free
          IDirectSound3DListener(Self.EmuListener) := nil; // Dxbx addition : Implicitly release of Listener

          // remove cache entry
          for v := 0 to SOUNDBUFFER_CACHE_SIZE-1 do
          begin
            if (g_pDSoundBufferCache[v] = Self) then
              g_pDSoundBufferCache[v] := nil;
          end;

          if (Self.EmuBufferDesc.lpwfxFormat <> NULL) then
            XboxFree(Self.EmuBufferDesc.lpwfxFormat);

          XboxFree(Self.EmuBufferDesc);

          Self.Free;
      end;
    end;
  end;

  EmuSwapFS(fsXbox);

  Result := uRet;
end;

function TIDirectSoundBuffer.SetFormat
(
    pwfxFormat: LPCWAVEFORMATEX
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetFormat').
      _(Self, 'pBuffer').
      _(pwfxFormat, 'pwfxFormat').
    LogEnd();

  Result := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).SetFormat(pwfxFormat); // TODO -oDxbx : Test this!
//  Result := DS_OK;

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.SetFrequency
(
    dwFrequency: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetFrequency').
      _(Self, 'pBuffer').
      _(dwFrequency, 'dwFrequency').
    LogEnd();

  if Assigned(Self.EmuDirectSoundBuffer8) then
    Result := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).SetFrequency(dwFrequency) // TODO -oDxbx : Test this!
  else
    Result := DS_OK;

  EmuSwapFS(fsXbox);
end;


function TIDirectSoundBuffer.SetVolume
(
    lVolume: LONG
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetVolume').
      _(Self, 'pBuffer').
      _(lVolume, 'lVolume').
    LogEnd();

  Result := DS_OK;
  if Assigned(Self.EmuDirectSoundBuffer8) then
  begin
    if g_XBSound.GetMute then
      lVolume := DSBVOLUME_MIN
    else
      g_SoundVolume := lVolume;


    Result := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).SetVolume(lVolume);
    DxbxHackUpdateSoundBuffers;
  end;

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.SetPitch
(
    lPitch: LONG
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  lFrequency: Double;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetPitch').
      _(Self, 'pBuffer').
      _(lPitch, 'lPitch').
    LogEnd();

  // TODO -oCXBX: Translate params, then make the PC DirectSound call
  if Assigned(Self.EmuDirectSoundBuffer8) then
  begin
    // pBuffer->SetPitch(-8192); // Play the sound two octaves lower (12 kHz)
    // pBuffer->SetPitch(-4096); // Play the sound one octaves lower (24 kHz)
    // pBuffer->SetPitch(    0); // Play the sound at 48 kHz (no pitch bend, 48 kHz)
    // pBuffer->SetPitch( 4096); // Play the sound one octave higher (96 kHz)
    // pBuffer->SetPitch( 8192); // Play the sound two octaves higher (192 kHz)
    //
    // A way to figure out the value for lPitch would be to calculate the base pitch of the voice
    // (log2(nSamplesPerSecond/48000)), add (or subtract) the number of octaves you want to change
    // (for example, -1 if you want to go down one octave), and convert the value to register format (multiply by 4096).
    //
    // Internally, SetFrequency calls SetPitch, so they are equivalent methods and will override each other.
    // Calling SetPitch with the value zero is the same as calling SetFrequency with the value of 48000.

    // TODO -oDxbx : Is this the right way to convert pitch into frequency?  Probably not....
    // Source : http://en.wikipedia.org/wiki/Note
    lFrequency := lPitch;
    lFrequency := (lFrequency / 4096.0);
    lFrequency := (lFrequency * lFrequency) * 440.0;
    lFrequency := lFrequency + 48000.0;

    Result := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).SetFrequency(Round(lFrequency));
  end
  else
    Result := DS_OK;

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.SetLFO
(
  pLFODesc: LPCDSLFODESC
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetLFO').
      _(Self, 'pBuffer').
      _(pLFODesc, 'pLFODesc').
    LogEnd();

  // TODO -oCXBX: Implement
  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function TIDirectSoundBuffer.SetEG
(
    pEnvelopeDesc: LPCDSENVELOPEDESC
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('TIDirectSoundBuffer.SetEG');

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.SetFilter
(
  pFilterDesc: PX_DSFILTERDESC
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetFilter').
      _(Self, 'pBuffer').
      _(pFilterDesc, 'pFilterDesc').
    LogEnd();

  // TODO -oCXBX: Implement

  EmuWarning('IDirectSoundBuffer_SetFilter not yet supported!');

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSoundBuffer.SetHeadroom
(
    dwHeadroom: DWORD
):HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetHeadroom').
      _(Self, 'pBuffer').
      _(dwHeadroom, 'dwHeadroom').
    LogEnd();

  // TODO -oCXBX: Actually implement this

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSoundBuffer.SetOutputBuffer
(
    pOutputBuffer: LPDIRECTSOUNDBUFFER
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('IDirectSoundBuffer_SetOutputBuffer');

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.SetMixBins
(
    pMixBins: LPCDSMIXBINS
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetMixBins').
      _(Self, 'pBuffer').
      _(pMixBins, 'pMixBins').
    LogEnd();

  // TODO -oCXBX: Actually do something

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSoundBuffer.SetMixBinVolumes
(
    pMixBins: LPCDSMIXBINS
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetMixBinVolumes').
      _(Self, 'pBuffer').
      _(pMixBins, 'pMixBins').
    LogEnd();

  if g_XBSound.GetMute then
    //??
  else
    ; // TODO -oCXBX: Actually do something

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSoundBuffer.SetAllParameters
(
    pds3db: LPCDS3DBUFFER;
    dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('TIDirectSoundBuffer.SetAllParameters');

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.SetConeAngles
(
    dwInsideConeAngle: DWORD;
    dwOutsideConeAngle: DWORD;
    dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetConeAngles').
      _(Self, 'pBuffer').
      _(dwInsideConeAngle, 'dwInsideConeAngle').
      _(dwOutsideConeAngle, 'dwOutsideConeAngle').
      _(dwApply, 'dwApply').
    LogEnd();

// TODO -oDxbx : Make this work & test it!
//  Result := IDirectSound3DBuffer(Self.EmuDirectSound3DBuffer8).SetConeAngles(dwInsideConeAngle, dwOutsideConeAngle, dwApply);
  Result := DS_OK;

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.SetConeOrientation
(
    x: FLOAT;
    y: FLOAT;
    z: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetConeOrientation').
      _(Self, 'pBuffer').
      _(x, 'x').
      _(y, 'y').
      _(z, 'z').
      _(dwApply, 'dwApply').
    LogEnd();

// TODO -oDxbx : Make this work & test it!
//  Result := IDirectSound3DBuffer(Self.EmuDirectSound3DBuffer8).SetConeOrientation(x, y, z, dwApply);
  Result := DS_OK;

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.SetConeOutsideVolume
(
    lConeOutsideVolume: LONG;
    dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetConeOutsideVolume').
      _(Self, 'pBuffer').
      _(lConeOutsideVolume, 'lConeOutsideVolume').
      _(dwApply, 'dwApply').
    LogEnd();

// TODO -oDxbx : Make this work & test it!
//  Result := IDirectSound3DBuffer(Self.EmuDirectSound3DBuffer8).SetConeOutsideVolume(lConeOutsideVolume, dwApply);
  Result := DS_OK;

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.SetMaxDistance
(
    flMaxDistance: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetMaxDistance').
      _(Self, 'pBuffer').
      _(flMaxDistance, 'flMaxDistance').
      _(dwApply, 'dwApply').
    LogEnd();

// TODO -oDxbx : Make this work & test it!
//  Result := IDirectSound3DBuffer(Self.EmuDirectSound3DBuffer8).SetMaxDistance(flMaxDistance, dwApply);
  Result := DS_OK;

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.SetMinDistance
(
    flMinDistance: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetMinDistance').
      _(Self, 'pBuffer').
      _(flMinDistance, 'flMinDistance').
      _(dwApply, 'dwApply').
    LogEnd();

// TODO -oDxbx : Make this work & test it!
//  Result := IDirectSound3DBuffer(Self.EmuDirectSound3DBuffer8).SetMinDistance(flMinDistance, dwApply);
  Result := DS_OK;

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.SetMode
(
    dwMode: DWORD;
    dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetMode').
      _(Self, 'pBuffer').
      _(dwMode, 'dwMode').
      _(dwApply, 'dwApply').
    LogEnd();

// TODO -oDxbx : Make this work & test it!
//  Result := IDirectSound3DBuffer(Self.EmuDirectSound3DBuffer8).SetMode(dwMode, dwApply);
  Result := DS_OK;

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.SetPosition
(
    x: FLOAT;
    y: FLOAT;
    z: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetPosition').
      _(Self, 'pBuffer').
      _(x, 'x').
      _(y, 'y').
      _(z, 'z').
      _(dwApply, 'dwApply').
    LogEnd();

  // Dxbx addition : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  if Assigned(Self.EmuListener) then
    IDirectSound3DListener(Self.EmuListener).SetPosition(x, y, z, dwApply);

// TODO -oDxbx : Find out if we can/need to use the Listener, or this attempt :
//  Result := IDirectSound3DBuffer(Self.EmuDirectSound3DBuffer8).SetPosition(x, y, z, dwApply);
  Result := DS_OK;

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.SetVelocity
(
    x: FLOAT;
    y: FLOAT;
    z: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetVelocity').
      _(Self, 'pBuffer').
      _(x, 'x').
      _(y, 'y').
      _(z, 'z').
      _(dwApply, 'dwApply').
    LogEnd();

  // Dxbx addition : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  if Assigned(Self.EmuListener) then
    // TODO -oDxbx: Test this!
    IDirectSound3DListener(Self.EmuListener).SetVelocity(x, y, z, dwApply);

// TODO -oDxbx : Find out if we can/need to use the Listener, or this attempt :
//  Result := IDirectSound3DBuffer(Self.EmuDirectSound3DBuffer8).SetVelocity(x, y, z, dwApply);
  Result := DS_OK;

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.SetDistanceFactor
(
    flDistanceFactor: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetDistanceFactor').
      _(Self, 'pBuffer').
      _(flDistanceFactor, 'flDistanceFactor').
      _(dwApply, 'dwApply').
    LogEnd();

  // Dxbx addition : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  if Assigned(Self.EmuListener) then
    // TODO -oDxbx: Test this!
    Result := IDirectSound3DListener(Self.EmuListener).SetDistanceFactor(flDistanceFactor, dwApply)
  else
    Result := DS_OK;

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.SetDopplerFactor
(
    flDopplerFactor: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetDopplerFactor').
      _(Self, 'pBuffer').
      _(flDopplerFactor, 'flDopplerFactor').
      _(dwApply, 'dwApply').
    LogEnd();

  // Dxbx addition : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  if Assigned(Self.EmuListener) then
    Result := IDirectSound3DListener(Self.EmuListener).SetDopplerFactor(flDopplerFactor, dwApply)
  else
    Result := DS_OK;

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.SetRolloffFactor
(
    flRolloffFactor: FLOAT;
    dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetRolloffFactor').
      _(Self, 'pBuffer').
      _(flRolloffFactor, 'flRolloffFactor').
      _(dwApply, 'dwApply').
    LogEnd();

  // Dxbx addition : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  if Assigned(Self.EmuListener) then
    Result := IDirectSound3DListener(Self.EmuListener).SetRolloffFactor(flRolloffFactor, dwApply)
  else
    Result := DS_OK;

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.SetRolloffCurve
(
  pflPoints: PFLOAT;
  dwPointCount: DWORD;
  dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetRolloffCurve').
      _(Self, 'pBuffer').
      _(pflPoints, 'pflPoints').
      _(dwPointCount, 'dwPointCount').
      _(dwApply, 'dwApply').
    LogEnd();

  // TODO -oCXBX: Implement

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSoundBuffer.SetI3DL2Source
(
    pds3db: LPCDSI3DL2BUFFER;
    dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit or lfTrace) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetI3DL2Source').
      _(Self, 'pBuffer').
      _(pds3db, 'pds3db').
      _(dwApply, 'dwApply').
    LogEnd();

  // TODO -oCXBX: Actually do something

  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function TIDirectSoundBuffer.Play
(
    dwReserved1: DWORD;
    dwReserved2: DWORD;
    dwFlags: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.Play').
      _(Self, 'pBuffer').
      _(dwReserved1, 'dwReserved1').
      _(dwReserved2, 'dwReserved2').
      _(dwFlags, 'dwFlags').
    LogEnd();

  if (dwFlags and (not (DSBPLAY_LOOPING or X_DSBPLAY_FROMSTART))) > 0 then
    DxbxKrnlCleanup('Unsupported Playing Flags');

  // rewind buffer
  if ((dwFlags and X_DSBPLAY_FROMSTART) <> X_DSBPLAY_FROMSTART) then
  begin
    if (FAILED(IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).SetCurrentPosition(0))) then
      EmuWarning('Rewinding buffer failed!');

    dwFlags := dwFlags and (not X_DSBPLAY_FROMSTART);
  end;

  DxbxHackUpdateSoundBuffers();

  // close any existing locks
  if (Self.EmuLockPtr1 <> nil) then
  begin
    IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).Unlock
    (
      Self.EmuLockPtr1,
      Self.EmuLockBytes1,
      Self.EmuLockPtr2,
      Self.EmuLockBytes2
    );

    Self.EmuLockPtr1 := nil;
  end;

  if (Self.EmuFlags and DSB_FLAG_ADPCM) > 0 then
  begin
    hRet := DS_OK; // Dxbx note : Cxbx uses D3D_OK here.
  end
  else
  begin
    if g_XBSound.GetMute then
      IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).SetVolume(DSBVOLUME_MIN)
    else
      IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).SetVolume(g_SoundVolume);
    hRet := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).Play(0, 0, dwFlags);

  end;

  Self.EmuPlayFlags := dwFlags;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function TIDirectSoundBuffer.PlayEx
(
    rtTimeStamp: REFERENCE_TIME;
    dwFlags: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.PlayEx').
      _(Self, 'pBuffer').
      _(rtTimeStamp, 'rtTimeStamp').
      _(dwFlags, 'dwFlags').
    LogEnd();

  if(Self.EmuDirectSoundBuffer8 = nil) then
    EmuWarning('pBuffer.EmuDirectSoundBuffer8 == 0');

  Unimplemented('PlayEx');

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSoundBuffer.Stop(): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.Stop').
      _(Self, 'pBuffer').
    LogEnd();

  hRet := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).Stop();

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function TIDirectSoundBuffer.StopEx
(
    rtTimeStamp: REFERENCE_TIME;
    dwFlags: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.StopEx').
      _(Self, 'pBuffer').
      _(rtTimeStamp, 'rtTimeStamp').
      _(dwFlags, 'dwFlags').
    LogEnd();

  if (Self.EmuDirectSoundBuffer8 = nil) then
    EmuWarning('pBuffer.EmuDirectSoundBuffer8 == 0');

  Result := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).Stop();

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.Pause
(
    dwPause: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
//var
//  dwFlags: DWord;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.Pause').
      _(Self, 'pBuffer').
      _(dwPause, 'dwPause').
    LogEnd();

  // This function wasn't part of the XDK until 4721.
  Result := DS_OK;

  // Unstable!
  (*if (Self <> NULL) then
  begin
    if(Self.EmuDirectSoundBuffer8) then
    begin
      if (dwPause = X_DSBPAUSE_PAUSE) then
        result := Self.EmuDirectSoundBuffer8.Stop();
      if (dwPause = X_DSBPAUSE_RESUME) then
      begin
        dwFlags := iif((Self.EmuPlayFlags and X_DSBPLAY_LOOPING) > 0, DSBPLAY_LOOPING, 0);
        result := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).Play(0, 0, dwFlags);
      end;
      if (dwPause = X_DSBPAUSE_SYNCHPLAYBACK) then
        EmuWarning('DSBPAUSE_SYNCHPLAYBACK is not yet supported!');
    end;
  end;*)

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.PauseEx
(
    rtTimestamp: REFERENCE_TIME;
    dwPause: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  ret: HRESULT;
  //dwFlags: DWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.PauseEx').
      _(Self, 'pBuffer').
      _(rtTimestamp, 'rtTimestamp').
      _(dwPause, 'dwPause').
    LogEnd();

  // This function wasn't part of the XDK until 4721.
  // TODO: Implement time stamp feature (a thread maybe?)
  EmuWarning('IDirectSoundBuffer_PauseEx not fully implemented!');

  ret := DS_OK;

  // Unstable!
  (*  if(Self <> NULL) then
  begin
    if Assigned(Self.EmuDirectSoundBuffer8) then
    begin
      if(dwPause = X_DSBPAUSE_PAUSE) then
        ret := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).Stop();
      if(dwPause = X_DSBPAUSE_RESUME) then
      begin
        dwFlags := iif((Self.EmuPlayFlags and X_DSBPLAY_LOOPING) > 0, DSBPLAY_LOOPING, 0);
        ret := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).Play(0, 0, dwFlags);
      end;
      if(dwPause = X_DSBPAUSE_SYNCHPLAYBACK) then
        EmuWarning('DSBPAUSE_SYNCHPLAYBACK is not yet supported!');
    end;
  end; *)

  EmuSwapFS(fsXbox);

  result := ret;
end;

function TIDirectSoundBuffer.SetPlayRegion
(
    dwPlayStart: DWORD;
    dwPlayLength: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetPlayRegion').
      _(Self, 'pBuffer').
      _(dwPlayStart, 'dwPlayStart').
      _(dwPlayLength, 'dwPlayLength').
    LogEnd();

  // TODO -oCXBX: Translate params, then make the PC DirectSound call

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSoundBuffer.SetLoopRegion
(
    dwLoopStart: DWORD;
    dwLoopLength: DWORD
) : HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetLoopRegion').
      _(Self, 'pBuffer').
      _(dwLoopStart, 'dwLoopStart').
      _(dwLoopLength, 'dwLoopLength').
    LogEnd();

  //Dxbx_TIDirectSoundBuffer_Resize(Self, dwLoopLength);

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSoundBuffer.GetStatus
(
    pdwStatus: LPDWORD
) : HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.GetStatus').
      _(Self, 'pBuffer').
      _(pdwStatus, 'pdwStatus').
    LogEnd();

  hRet := DS_OK;

  if (Self <> nil) and (Self.EmuDirectSoundBuffer8 <> nil) then
  begin
    hRet := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).GetStatus({out}pdwStatus^);
  end
  else
  begin
//    pdwStatus^ := 0;
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function TIDirectSoundBuffer.GetCurrentPosition
(
    pdwPlayCursor: LPDWORD;
    pdwWriteCursor: LPDWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.GetCurrentPosition').
      _(Self, 'pBuffer').
      _(pdwPlayCursor, 'pdwCurrentPlayCursor').
      _(pdwWriteCursor, 'pdwCurrentWriteCursor').
    LogEnd();

  DxbxHackUpdateSoundBuffers();
  DxbxHackUpdateSoundStreams();

  // NOTE: TODO -oCXBX: This call always seems to fail on primary buffers!
  hRet := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).GetCurrentPosition(pdwPlayCursor, pdwWriteCursor);

  if (FAILED(hRet)) then
    ShowSoundError('GetCurrentPosition Failed!', hRet, etWarning);

  if (pdwPlayCursor <> nil) and (pdwWriteCursor <> nil) then
  begin
    if MayLog(lfUnit) then
      DbgPrintf('*pdwCurrentPlayCursor := %d, *pdwCurrentWriteCursor := %d', [pdwPlayCursor^, pdwWriteCursor^]);
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function TIDirectSoundBuffer.SetCurrentPosition
(
    dwPlayCursor: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetCurrentPosition').
      _(Self, 'pBuffer').
      _(dwPlayCursor, 'dwPlayCursor').
    LogEnd();

  // NOTE: TODO -oCXBX: This call *will* (by MSDN) fail on primary buffers!
  hRet := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).SetCurrentPosition(dwPlayCursor);

  if (FAILED(hRet)) then
    ShowSoundError('SetCurrentPosition Failed!', hRet, etWarning);

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function TIDirectSoundBuffer.SetBufferData
(
    pvBufferData: LPVOID;
    dwBufferBytes: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.SetBufferData').
      _(Self, 'pBuffer').
      _(pvBufferData, 'pvBufferData').
      _(dwBufferBytes, 'dwBufferBytes').
    LogEnd();

  // update buffer data cache
  Self.EmuBuffer := pvBufferData;

  Dxbx_TIDirectSoundBuffer_Resize(Self, dwBufferBytes);

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSoundBuffer.Lock
(
    dwOffset: DWORD;
    dwBytes: DWORD;
    ppvAudioPtr1: PLPVOID;
    pdwAudioBytes1: LPDWORD;
    ppvAudioPtr2: PLPVOID;
    pdwAudioBytes2: LPDWORD;
    dwFlags: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  hRet: HRESULT;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.Lock').
      _(Self, 'pBuffer').
      _(dwOffset, 'dwOffset').
      _(dwBytes, 'dwBytes').
      _(ppvAudioPtr1, 'ppvAudioPtr1').
      _(pdwAudioBytes1, 'pdwAudioBytes1').
      _(ppvAudioPtr2, 'ppvAudioPtr2').
      _(pdwAudioBytes2, 'pdwAudioBytes2').
      _(dwFlags, 'dwFlags').
    LogEnd();

  hRet := DS_OK; // Dxbx note : Cxbx uses D3D_OK here.

  if (Self.EmuBuffer <> nil) then
  begin
    ppvAudioPtr1^ := Self.EmuBuffer;
    pdwAudioBytes1^ := dwBytes;
  end
  else
  begin
    if (dwBytes > Self.EmuBufferDesc.dwBufferBytes) then
      Dxbx_TIDirectSoundBuffer_Resize(Self, dwBytes);

    if (Self.EmuLockPtr1 <> nil) then
      IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).Unlock(Self.EmuLockPtr1, Self.EmuLockBytes1, Self.EmuLockPtr2, Self.EmuLockBytes2);

    // TODO -oCXBX: Verify dwFlags is the same as windows
    hRet := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).Lock(dwOffset, dwBytes, ppvAudioPtr1, pdwAudioBytes1, ppvAudioPtr2, pdwAudioBytes2, dwFlags);

    if (FAILED(hRet)) then
      DxbxKrnlCleanup('DirectSoundBuffer Lock Failed!');

    Self.EmuLockPtr1 := ppvAudioPtr1^;
    Self.EmuLockBytes1 := pdwAudioBytes1^;

    if (ppvAudioPtr2 <> NULL) then
      Self.EmuLockPtr2 := ppvAudioPtr2^
    else
      Self.EmuLockPtr2 := nil;

    if (pdwAudioBytes2 <> NULL) then
      Self.EmuLockBytes2 := pdwAudioBytes2^
    else
      Self.EmuLockBytes2 := 0;
  end;

  EmuSwapFS(fsXbox);

  Result := hRet;
end;

function TIDirectSoundBuffer.Unlock
(
    pvLock1: LPVOID;
    dwLockSize1: DWORD;
    pvLock2: LPVOID;
    dwLockSize2: DWORD
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('TIDirectSound.Unlock');

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.Restore(): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('TIDirectSoundBuffer.Restore');

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.SetNotificationPositions
(
    dwNotifyCount: DWORD;
    paNotifies: LPCDSBPOSITIONNOTIFY
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('TIDirectSoundBuffer.SetNotificationPositions');

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.GetVoiceProperties
(
    pVoiceProps: LPDSVOICEPROPS
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundBuffer.GetVoiceProperties').
      _(Self, 'pBuffer').
      _(pVoiceProps, 'pVoiceProps').
    LogEnd();

  // TODO -oCXBX: Actually implement

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

procedure XTL_EmuIDirectSoundBuffer_AddRef;
asm  jmp TIDirectSoundBuffer.AddRef; end;

procedure XTL_EmuIDirectSoundBuffer_Release;
asm jmp TIDirectSoundBuffer.Release; end;

procedure XTL_EmuIDirectSoundBuffer_GetCurrentPosition;
asm jmp TIDirectSoundBuffer.GetCurrentPosition; end;

procedure XTL_EmuIDirectSoundBuffer_GetStatus;
asm jmp TIDirectSoundBuffer.GetStatus; end;

procedure XTL_EmuIDirectSoundBuffer_GetVoiceProperties;
asm jmp TIDirectSoundBuffer.GetVoiceProperties; end;

procedure XTL_EmuIDirectSoundBuffer_Lock;
asm jmp TIDirectSoundBuffer.Lock; end;

procedure XTL_EmuIDirectSoundBuffer_Pause;
asm jmp TIDirectSoundBuffer.Pause; end;

procedure XTL_EmuIDirectSoundBuffer_PauseEx;
asm jmp TIDirectSoundBuffer.PauseEx; end;

procedure XTL_EmuIDirectSoundBuffer_Play;
asm jmp TIDirectSoundBuffer.Play; end;

procedure XTL_EmuIDirectSoundBuffer_PlayEx;
asm jmp TIDirectSoundBuffer.PlayEx; end;

procedure XTL_EmuIDirectSoundBuffer_Restore;
asm jmp TIDirectSoundBuffer.Restore; end;

procedure XTL_EmuIDirectSoundBuffer_SetAllParameters;
asm jmp TIDirectSoundBuffer.SetAllParameters; end;

procedure XTL_EmuIDirectSoundBuffer_SetBufferData;
asm jmp TIDirectSoundBuffer.SetBufferData; end;

procedure XTL_EmuIDirectSoundBuffer_SetConeAngles;
asm jmp TIDirectSoundBuffer.SetConeAngles; end;

procedure XTL_EmuIDirectSoundBuffer_SetConeOrientation;
asm jmp TIDirectSoundBuffer.SetConeOrientation; end;

procedure XTL_EmuIDirectSoundBuffer_SetConeOutsideVolume;
asm jmp TIDirectSoundBuffer.SetConeOutsideVolume; end;

procedure XTL_EmuIDirectSoundBuffer_SetCurrentPosition;
asm jmp TIDirectSoundBuffer.SetCurrentPosition; end;

procedure XTL_EmuIDirectSoundBuffer_SetDistanceFactor;
asm jmp TIDirectSoundBuffer.SetDistanceFactor; end;

procedure XTL_EmuIDirectSoundBuffer_SetDopplerFactor;
asm jmp TIDirectSoundBuffer.SetDopplerFactor; end;

procedure XTL_EmuIDirectSoundBuffer_SetEG;
asm jmp TIDirectSoundBuffer.SetEG; end;

procedure XTL_EmuIDirectSoundBuffer_SetFilter;
asm jmp TIDirectSoundBuffer.SetFilter; end;

procedure XTL_EmuIDirectSoundBuffer_SetFormat;
asm jmp TIDirectSoundBuffer.SetFormat; end;

procedure XTL_EmuIDirectSoundBuffer_SetFrequency;
asm jmp TIDirectSoundBuffer.SetFrequency; end;

procedure XTL_EmuIDirectSoundBuffer_SetHeadroom;
asm jmp TIDirectSoundBuffer.SetHeadroom; end;

procedure XTL_EmuIDirectSoundBuffer_SetI3DL2Source;
asm jmp TIDirectSoundBuffer.SetI3DL2Source; end;

procedure XTL_EmuIDirectSoundBuffer_SetLFO;
asm jmp TIDirectSoundBuffer.SetLFO; end;

procedure XTL_EmuIDirectSoundBuffer_SetLoopRegion;
asm jmp TIDirectSoundBuffer.SetLoopRegion; end;

procedure XTL_EmuIDirectSoundBuffer_SetMaxDistance;
asm jmp TIDirectSoundBuffer.SetMaxDistance; end;

procedure XTL_EmuIDirectSoundBuffer_SetMinDistance;
asm jmp TIDirectSoundBuffer.SetMinDistance; end;

procedure XTL_EmuIDirectSoundBuffer_SetMixBins;
asm jmp TIDirectSoundBuffer.SetMixBins; end;

procedure XTL_EmuIDirectSoundBuffer_SetMixBinVolumes;
asm jmp TIDirectSoundBuffer.SetMixBinVolumes; end;

procedure XTL_EmuIDirectSoundBuffer_SetMode;
asm jmp TIDirectSoundBuffer.SetMode; end;

procedure XTL_EmuIDirectSoundBuffer_SetNotificationPositions;
asm jmp TIDirectSoundBuffer.SetNotificationPositions; end;

procedure XTL_EmuIDirectSoundBuffer_SetOutputBuffer;
asm jmp TIDirectSoundBuffer.SetOutputBuffer; end;

procedure XTL_EmuIDirectSoundBuffer_SetPitch;
asm jmp TIDirectSoundBuffer.SetPitch; end;

procedure XTL_EmuIDirectSoundBuffer_SetPlayRegion;
asm jmp TIDirectSoundBuffer.SetPlayRegion; end;

procedure XTL_EmuIDirectSoundBuffer_SetPosition;
asm jmp TIDirectSoundBuffer.SetPosition; end;

procedure XTL_EmuIDirectSoundBuffer_SetRolloffCurve;
asm jmp TIDirectSoundBuffer.SetRolloffCurve; end;

procedure XTL_EmuIDirectSoundBuffer_SetRolloffFactor;
asm jmp TIDirectSoundBuffer.SetRolloffFactor; end;

procedure XTL_EmuIDirectSoundBuffer_SetVelocity;
asm jmp TIDirectSoundBuffer.SetVelocity; end;

procedure XTL_EmuIDirectSoundBuffer_SetVolume;
asm jmp TIDirectSoundBuffer.SetVolume; end;

procedure XTL_EmuIDirectSoundBuffer_Stop;
asm jmp TIDirectSoundBuffer.Stop; end;

procedure XTL_EmuIDirectSoundBuffer_StopEx;
asm jmp TIDirectSoundBuffer.StopEx; end;

procedure XTL_EmuIDirectSoundBuffer_Unlock;
asm jmp TIDirectSoundBuffer.Unlock; end;

//
//
//

procedure XTL_EmuDirectSound_CDirectSoundBuffer_GetCurrentPosition;
asm jmp TIDirectSoundBuffer.GetCurrentPosition; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_GetStatus;
asm jmp TIDirectSoundBuffer.GetStatus; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_GetVoiceProperties;
asm jmp TIDirectSoundBuffer.GetVoiceProperties; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_Lock;
asm jmp TIDirectSoundBuffer.Lock; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_Pause;
asm jmp TIDirectSoundBuffer.Pause; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_PauseEx;
asm jmp TIDirectSoundBuffer.PauseEx; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_Play;
asm jmp TIDirectSoundBuffer.Play; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_PlayEx;
asm jmp TIDirectSoundBuffer.PlayEx; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_Restore;
asm jmp TIDirectSoundBuffer.Restore; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetAllParameters;
asm jmp TIDirectSoundBuffer.SetAllParameters; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetBufferData;
asm jmp TIDirectSoundBuffer.SetBufferData; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetConeAngles;
asm jmp TIDirectSoundBuffer.SetConeAngles; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetConeOrientation;
asm jmp TIDirectSoundBuffer.SetConeOrientation; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetConeOutsideVolume;
asm jmp TIDirectSoundBuffer.SetConeOutsideVolume; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetCurrentPosition;
asm jmp TIDirectSoundBuffer.SetCurrentPosition; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetDistanceFactor;
asm jmp TIDirectSoundBuffer.SetDistanceFactor; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetDopplerFactor;
asm jmp TIDirectSoundBuffer.SetDopplerFactor; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetEG;
asm jmp TIDirectSoundBuffer.SetEG; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetFilter;
asm jmp TIDirectSoundBuffer.SetFilter; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetFormat;
asm jmp TIDirectSoundBuffer.SetFormat; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetFrequency;
asm jmp TIDirectSoundBuffer.SetFrequency; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetHeadroom;
asm jmp TIDirectSoundBuffer.SetHeadroom; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetI3DL2Source;
asm jmp TIDirectSoundBuffer.SetI3DL2Source; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetLFO;
asm jmp TIDirectSoundBuffer.SetLFO; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetLoopRegion;
asm jmp TIDirectSoundBuffer.SetLoopRegion; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetMaxDistance;
asm jmp TIDirectSoundBuffer.SetMaxDistance; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetMinDistance;
asm jmp TIDirectSoundBuffer.SetMinDistance; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetMixBins;
asm jmp TIDirectSoundBuffer.SetMixBins; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetMixBinVolumes;
asm jmp TIDirectSoundBuffer.SetMixBinVolumes; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetMode;
asm jmp TIDirectSoundBuffer.SetMode; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetNotificationPositions;
asm jmp TIDirectSoundBuffer.SetNotificationPositions; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetOutputBuffer;
asm jmp TIDirectSoundBuffer.SetOutputBuffer; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetPitch;
asm jmp TIDirectSoundBuffer.SetPitch; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetPlayRegion;
asm jmp TIDirectSoundBuffer.SetPlayRegion; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetPosition;
asm jmp TIDirectSoundBuffer.SetPosition; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetRolloffCurve;
asm jmp TIDirectSoundBuffer.SetRolloffCurve; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetRolloffFactor;
asm jmp TIDirectSoundBuffer.SetRolloffFactor; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetVelocity;
asm jmp TIDirectSoundBuffer.SetVelocity; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_SetVolume;
asm jmp TIDirectSoundBuffer.SetVolume; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_Stop;
asm jmp TIDirectSoundBuffer.Stop; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_StopEx;
asm jmp TIDirectSoundBuffer.StopEx; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_Unlock;
asm jmp TIDirectSoundBuffer.Unlock; end;

procedure XTL_EmuDirectSound_CDirectSoundBuffer_Release;
asm jmp TIDirectSoundBuffer.Release; end;


//
// TIDirectSoundStream - no patches, but a real class implementation as a VMT-replacement for the Xbox version
//

function TIDirectSoundStream.AddRef(): ULONG; stdcall; // virtual
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.AddRef').
      _(Self, 'pStream').
    LogEnd();

  if (Self <> nil) then
    if (Self.EmuDirectSoundBuffer8 <> nil) then // Cxbx HACK: Ignore unsupported codecs.
      IDirectSoundBuffer(Self.EmuDirectSoundBuffer8)._AddRef();

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSoundStream.Release(): ULONG; stdcall; // virtual
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
var
  uRet: ULONG;
  v: int;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.Release').
      _(Self, 'pStream').
    LogEnd();

  uRet := 0;

  if (Self <> nil) and (Self.EmuDirectSoundBuffer8 <> nil) then
  begin
    uRet := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8)._Release();

    if (uRet = 0) then
    begin
      Self.EmuDirectSoundBuffer8 := nil; // Dxbx addition : nil out after free
      // remove cache entry
      for v := 0 to SOUNDSTREAM_CACHE_SIZE-1 do
      begin
        if (g_pDSoundStreamCache[v] = Self) then
          g_pDSoundStreamCache[v] := nil;
      end;

      if (Self.EmuBufferDesc.lpwfxFormat <> NULL) then
        XboxFree(Self.EmuBufferDesc.lpwfxFormat);

      XboxFree(Self.EmuBufferDesc);

      Self.Free;
    end;
  end;

  EmuSwapFS(fsXbox);

  Result := uRet;
end;

function TIDirectSoundStream.GetInfo
(
    pInfo: LPXMEDIAINFO
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.GetInfo').
      _(Self, 'pStream').
      _(pInfo, 'pInfo').
    LogEnd();

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

function TIDirectSoundStream.GetStatus
(
    pdwStatus: PDWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.GetStatus').
      _(Self, 'pStream').
      _(pdwStatus, 'pdwStatus').
    LogEnd();

  Unimplemented('EmuCDirectSoundStream_GetStatus');

  pdwStatus^ := DSBSTATUS_PLAYING;

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSoundStream.Process
(
    pInputBuffer: PXMEDIAPACKET;
    pOutputBuffer: PXMEDIAPACKET
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.Process').
      _(Self, 'pStream').
      _(pInputBuffer, 'pInputBuffer').
      _(pOutputBuffer, 'pOutputBuffer').
    LogEnd();

  if  (Self <> nil)
  and (Self.EmuDirectSoundBuffer8 <> NULL) then
  begin
    // update buffer data cache
    Self.EmuBuffer := pInputBuffer.pvBuffer;

    Dxbx_TIDirectSoundStream_Resize(Self, pInputBuffer.dwMaxSize);

    if (pInputBuffer.pdwStatus <> nil) then
      pInputBuffer.pdwStatus^ := S_OK;

    DxbxHackUpdateSoundStreams();
  end
  else
  begin
    if (pInputBuffer.pdwStatus <> nil) then
      pInputBuffer.pdwStatus^ := S_OK;
  end;

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSoundStream.Discontinuity(): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.Discontinuity').
      _(Self, 'pStream').
    LogEnd();

  // TODO -oCXBX: Actually Process

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;


function TIDirectSoundStream.Flush(): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.Flush').
      _(Self, 'pStream').
    LogEnd();

  // TODO -oCXBX: Actually Flush

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSoundStream.SetFormat
(
  pwfxFormat: LPCWAVEFORMATEX
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.SetFormat').
      _(Self, 'pStream').
      _(pwfxFormat, 'pwfxFormat').
    LogEnd();

  if  (Self <> nil)
  and (Self.EmuDirectSoundBuffer8 <> NULL) then
    Result := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).SetFormat(pwfxFormat)
  else
    Result := DS_OK;

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundStream.SetFrequency
(
  dwFrequency: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.SetFrequency').
      _(Self, 'pStream').
      _(dwFrequency, 'dwFrequency').
    LogEnd();

  if  (Self <> nil)
  and (Self.EmuDirectSoundBuffer8 <> NULL) then
    Result := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).SetFrequency(dwFrequency)
  else
    Result := DS_OK;

  EmuSwapFS(fsXbox);

end;

function TIDirectSoundStream.SetVolume
(
  lVolume: LONG
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.SetVolume').
      _(Self, 'pStream').
      _(lVolume, 'lVolume').
    LogEnd();

  Result := DS_OK;
  if  (Self <> nil)
  and (Self.EmuDirectSoundBuffer8 <> NULL)
  and (not g_XBSound.GetMute) then
  begin
    g_SoundVolume := lVolume;
    Result := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).SetVolume(lVolume);
    DxbxHackUpdateSoundStreams;
  end;

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundStream.SetPitch
(
  lPitch: LONG
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.SetPitch').
      _(Self, 'pStream').
      _(lPitch, 'lPitch').
    LogEnd();

  Result := DS_OK;

  Unimplemented('IDirectSoundStream_SetPitch');

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundStream.SetLFO
(
  pLFODesc: LPCDSLFODESC
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('TIDirectSoundStream.SetLFO');

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundStream.SetEG
(
  pEnvelopeDesc: LPCDSENVELOPEDESC
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.SetEG').
      _(Self, 'pStream').
      _(pEnvelopeDesc, 'pEnvelopeDesc').
    LogEnd();

  // TODO -oCXBX: Implement this...

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSoundStream.SetFilter
(
  pFilterDesc: LPCDSFILTERDESC
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.SetFilter').
      _(Self, 'pStream').
      _(pFilterDesc, 'pFilterDesc').
    LogEnd();

  // TODO -oCXBX: Implement

  EmuWarning('CDirectSoundStream_SetFilter not yet supported!');

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSoundStream.SetHeadroom
(
  dwHeadroom: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.SetHeadroom').
      _(Self, 'pStream').
      _(dwHeadroom, 'dwHeadroom').
    LogEnd();

  // TODO -oCXBX: Actually implement this
  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSoundStream.SetOutputBuffer
(
  pOutputBuffer: LPDIRECTSOUNDBUFFER
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('TIDirectSoundStream.SetOutputBuffer');

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundStream.SetMixBins
(
  pMixBins: LPCDSMIXBINS
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.SetMixBins').
      _(Self, 'pStream').
      _(pMixBins, 'pMixBins').
    LogEnd();

  // TODO -oCXBX: Actually implement this.

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;


function TIDirectSoundStream.SetMixBinVolumes
(
  pMixBins: LPCDSMIXBINS
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('TIDirectSoundStream.SetMixBinVolumes');

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundStream.SetAllParameters
(
  pds3db: LPCDS3DBUFFER;
  dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.SetAllParameters').
      _(Self, 'pStream').
      _(pds3db, 'pds3db').
      _(dwApply, 'dwApply').
    LogEnd();

  // TODO -oCXBX: Actually implement this
  // Result := IDirectSound3DBuffer(Self.EmuDirectSound3DBuffer8).SetAllParameters(pds3db, dwApply);
  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function TIDirectSoundStream.SetConeAngles
(
  dwInsideConeAngle,
  dwOutsideConeAngle,
  dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.SetConeAngles').
      _(Self, 'pStream').
      _(dwInsideConeAngle, 'dwInsideConeAngle').
      _(dwOutsideConeAngle, 'dwOutsideConeAngle').
      _(dwApply, 'dwApply').
    LogEnd();

  // TODO -oCXBX: Actually implement this
  // Result := IDirectSound3DBuffer(Self.EmuDirectSound3DBuffer8).SetConeAngles(dwInsideConeAngle, dwOutsideConeAngle, dwApply);
  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function TIDirectSoundStream.SetConeOrientation
(
  x,
  y,
  z: FLOAT;
  dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.SetConeOrientation').
      _(Self, 'pStream').
      _(x, 'x').
      _(y, 'y').
      _(z, 'z').
      _(dwApply, 'dwApply').
    LogEnd();

  // TODO -oCXBX: Actually implement this
  // Result := IDirectSound3DBuffer(Self.EmuDirectSound3DBuffer8).SetConeOrientation(x, y, z, dwApply);
  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function TIDirectSoundStream.SetConeOutsideVolume
(
  lConeOutsideVolume: LONG;
  dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.SetConeOutsideVolume').
      _(Self, 'pStream').
      _(lConeOutsideVolume, 'lConeOutsideVolume').
      _(dwApply, 'dwApply').
    LogEnd();

  // TODO -oCXBX: Actually implement this
  // Result := IDirectSound3DBuffer(Self.EmuDirectSound3DBuffer8).SetConeOutsideVolume(lConeOutsideVolume, dwApply);
  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function TIDirectSoundStream.SetMaxDistance
(
  flMaxDistance: FLOAT;
  dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.SetMaxDistance').
      _(Self, 'pStream').
      _(flMaxDistance, 'flMaxDistance').
      _(dwApply, 'dwApply').
    LogEnd();

  // TODO -oCXBX: Actually implement this
  // Result := IDirectSound3DBuffer(Self.EmuDirectSound3DBuffer8).SetMaxDistance(flMaxDistance, dwApply);
  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function TIDirectSoundStream.SetMinDistance
(
  flMinDistance: FLOAT;
  dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.SetMinDistance').
      _(Self, 'pStream').
      _(flMinDistance, 'flMinDistance').
      _(dwApply, 'dwApply').
    LogEnd();

  // TODO -oCXBX: Actually implement this
  // Result := IDirectSound3DBuffer(Self.EmuDirectSound3DBuffer8).SetMinDistance(flMinDistance, dwApply);
  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function TIDirectSoundStream.SetMode
(
  dwMode,
  dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.SetFormat').
      _(Self, 'pStream').
      _(dwMode, 'dwMode').
      _(dwApply, 'dwApply').
    LogEnd();

  Result := DS_OK;

  EmuWarning('EmuCDirectSoundStream_SetFormat ignored');

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundStream.SetPosition
(
  x,
  y,
  z: FLOAT;
  dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.SetPosition').
      _(Self, 'pStream').
      _(x, 'x').
      _(y, 'y').
      _(z, 'z').
      _(dwApply, 'dwApply').
    LogEnd();

  // TODO -oCXBX: Actually implement this
  // Result := IDirectSound3DBuffer(Self.EmuDirectSound3DBuffer8).SetPosition(x, y, z, dwApply);
  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function TIDirectSoundStream.SetVelocity
(
  x,
  y,
  z: FLOAT;
  dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.SetVelocity').
      _(Self, 'pStream').
      _(x, 'x').
      _(y, 'y').
      _(z, 'z').
      _(dwApply, 'dwApply').
    LogEnd();

  // TODO -oCXBX: Actually implement this
  // Result := IDirectSound3DBuffer(Self.EmuDirectSound3DBuffer8).SetVelocity(x, y, z, dwApply);

  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function TIDirectSoundStream.SetDistanceFactor(flDistanceFactor: FLOAT; dwApply: DWORD): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('TIDirectSoundStream.SetDistanceFactor');

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundStream.SetDopplerFactor
(
  flDopplerFactor: FLOAT;
  dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('TIDirectSoundStream.SetDopplerFactor');

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundStream.SetRolloffFactor
(
  flRolloffFactor: FLOAT;
  dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.SetRolloffFactor').
      _(Self, 'pStream').
      _(flRolloffFactor, 'flRolloffFactor').
      _(dwApply, 'dwApply').
    LogEnd();

  // TODO -oCXBX: Actually SetRolloffFactor
  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function TIDirectSoundStream.SetRolloffCurve
(
  pflPoints: PFLOAT;
  dwPointCount,
  dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('TIDirectSoundStream.SetRolloffCurve');

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundStream.SetI3DL2Source
(
  pds3db: LPCDSI3DL2BUFFER;
  dwApply: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.SetI3DL2Source').
      _(Self, 'pStream').
      _(pds3db, 'pds3db').
      _(dwApply, 'dwApply').
    LogEnd();

  // TODO -oCXBX: Actually implement this
  EmuSwapFS(fsXbox);
  Result := DS_OK;
end;

function TIDirectSoundStream.Pause
(
  dwPause: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.Pause').
      _(Self, 'pStream').
      _(dwPause, 'dwPause').
    LogEnd();

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSoundStream.PauseEx
(
  rtTimestamp: REFERENCE_TIME;
  dwPause: DWORD
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('TIDirectSoundStream.PauseEx');

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundStream.FlushEx
(
  rtTimeStamp: REFERENCE_TIME;
  dwFlags: DWORD
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.FlushEx').
      _(Self, 'pStream').
      _(rtTimeStamp, 'rtTimeStamp').
      _(dwFlags, 'dwFlags').
    LogEnd();

  // TODO -oCXBX: Actually implement

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSoundStream.GetVoiceProperties
(
  pVoiceProps: LPDSVOICEPROPS
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : TIDirectSoundStream.GetVoiceProperties').
      _(Self, 'pStream').
      _(pVoiceProps, 'pVoiceProps').
    LogEnd();

  // TODO -oCXBX: Actually implement
  if Assigned(pVoiceProps) then
  begin
//    pVoiceProps.???
  end;

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

//
// Patches on IDirectSoundStream_* functions (mimicking an interface, but still being plain C functions)
//

function XTL_EmuIDirectSoundStream_QueryInterfaceC
(
    pThis: LPDIRECTSOUNDSTREAM;
    iid: PIID;
    ppvInterface: PLPVOID
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('XTL_EmuIDirectSoundStream_QueryInterfaceC');

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIDirectSoundStream_QueryInterface
(
    pThis: LPDIRECTSOUNDSTREAM;
    iid: REFIID;
    ppvInterface: PLPVOID
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('XTL_EmuIDirectSoundStream_QueryInterface');

  EmuSwapFS(fsXbox);
end;

procedure XTL_EmuIDirectSoundStream_SetFormat;
asm jmp TIDirectSoundStream.SetFormat; end;

procedure XTL_EmuIDirectSoundStream_SetFrequency;
asm jmp TIDirectSoundStream.SetFrequency; end;

procedure XTL_EmuIDirectSoundStream_SetVolume;
asm jmp TIDirectSoundStream.SetVolume; end;

procedure XTL_EmuIDirectSoundStream_SetPitch;
asm jmp TIDirectSoundStream.SetPitch; end;

procedure XTL_EmuIDirectSoundStream_SetLFO;
asm jmp TIDirectSoundStream.SetLFO; end;

procedure XTL_EmuIDirectSoundStream_SetEG;
asm jmp TIDirectSoundStream.SetEG; end;

procedure XTL_EmuIDirectSoundStream_SetFilter;
asm jmp TIDirectSoundStream.SetFilter; end;

procedure XTL_EmuIDirectSoundStream_SetHeadroom;
asm jmp TIDirectSoundStream.SetHeadroom; end;

procedure XTL_EmuIDirectSoundStream_SetOutputBuffer;
asm jmp TIDirectSoundStream.SetOutputBuffer; end;

procedure XTL_EmuIDirectSoundStream_SetMixBins;
asm jmp TIDirectSoundStream.SetMixBins; end;

procedure XTL_EmuIDirectSoundStream_SetMixBinVolumes;
asm jmp TIDirectSoundStream.SetMixBinVolumes; end;

procedure XTL_EmuIDirectSoundStream_SetAllParameters;
asm jmp TIDirectSoundStream.SetAllParameters; end;

procedure XTL_EmuIDirectSoundStream_SetConeAngles;
asm jmp TIDirectSoundStream.SetConeAngles; end;

procedure XTL_EmuIDirectSoundStream_SetConeOrientation;
asm jmp TIDirectSoundStream.SetConeOrientation; end;

procedure XTL_EmuIDirectSoundStream_SetConeOutsideVolume;
asm jmp TIDirectSoundStream.SetConeOutsideVolume; end;

procedure XTL_EmuIDirectSoundStream_SetMaxDistance;
asm jmp TIDirectSoundStream.SetMaxDistance; end;

procedure XTL_EmuIDirectSoundStream_SetMinDistance;
asm jmp TIDirectSoundStream.SetMinDistance; end;

procedure XTL_EmuIDirectSoundStream_SetMode;
asm jmp TIDirectSoundStream.SetMode; end;

procedure XTL_EmuIDirectSoundStream_SetPosition;
asm jmp TIDirectSoundStream.SetPosition; end;

procedure XTL_EmuIDirectSoundStream_SetVelocity;
asm jmp TIDirectSoundStream.SetVelocity; end;

procedure XTL_EmuIDirectSoundStream_SetDistanceFactor;
asm jmp TIDirectSoundStream.SetDistanceFactor; end;

procedure XTL_EmuIDirectSoundStream_SetDopplerFactor;
asm jmp TIDirectSoundStream.SetDopplerFactor; end;

procedure XTL_EmuIDirectSoundStream_SetRolloffFactor;
asm jmp TIDirectSoundStream.SetRolloffFactor; end;

procedure XTL_EmuIDirectSoundStream_SetRolloffCurve;
asm jmp TIDirectSoundStream.SetRolloffCurve; end;

procedure XTL_EmuIDirectSoundStream_SetI3DL2Source;
asm jmp TIDirectSoundStream.SetI3DL2Source; end;

procedure XTL_EmuIDirectSoundStream_Pause;
asm jmp TIDirectSoundStream.Pause; end;

procedure XTL_EmuIDirectSoundStream_PauseEx;
asm jmp TIDirectSoundStream.PauseEx; end;

procedure XTL_EmuIDirectSoundStream_FlushEx;
asm jmp TIDirectSoundStream.FlushEx; end;

procedure XTL_EmuIDirectSoundStream_GetVoiceProperties;
asm jmp TIDirectSoundStream.GetVoiceProperties; end;

procedure XTL_EmuIDirectSoundStream_Release;
asm jmp TIDirectSoundStream.Release; end;

//
// Patches on CDirectSoundStream class functions
//

procedure XTL_EmuDirectSound_CDirectSoundStream_AddRef;
asm jmp TIDirectSoundStream.AddRef; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_Discontinuity;
asm jmp TIDirectSoundStream.Discontinuity; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetFormat;
asm jmp TIDirectSoundStream.SetFormat; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetFrequency;
asm jmp TIDirectSoundStream.SetFrequency; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetVolume;
asm jmp TIDirectSoundStream.SetVolume; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetPitch;
asm jmp TIDirectSoundStream.SetPitch; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetLFO;
asm jmp TIDirectSoundStream.SetLFO; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetEG;
asm jmp TIDirectSoundStream.SetEG; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetFilter;
asm jmp TIDirectSoundStream.SetFilter; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetHeadroom;
asm jmp TIDirectSoundStream.SetHeadroom; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetOutputBuffer;
asm jmp TIDirectSoundStream.SetOutputBuffer; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetMixBins;
asm jmp TIDirectSoundStream.SetMixBins; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetMixBinVolumes;
asm jmp TIDirectSoundStream.SetMixBinVolumes; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetAllParameters;
asm jmp TIDirectSoundStream.SetAllParameters; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetConeAngles;
asm jmp TIDirectSoundStream.SetConeAngles; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetConeOrientation;
asm jmp TIDirectSoundStream.SetConeOrientation; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetConeOutsideVolume;
asm jmp TIDirectSoundStream.SetConeOutsideVolume; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetMaxDistance;
asm jmp TIDirectSoundStream.SetMaxDistance; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetMinDistance;
asm jmp TIDirectSoundStream.SetMinDistance; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetMode;
asm jmp TIDirectSoundStream.SetMode; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetPosition;
asm jmp TIDirectSoundStream.SetPosition; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetVelocity;
asm jmp TIDirectSoundStream.SetVelocity; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetDistanceFactor;
asm jmp TIDirectSoundStream.SetDistanceFactor; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetDopplerFactor;
asm jmp TIDirectSoundStream.SetDopplerFactor; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetRolloffFactor;
asm jmp TIDirectSoundStream.SetRolloffFactor; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetRolloffCurve;
asm jmp TIDirectSoundStream.SetRolloffCurve; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_SetI3DL2Source;
asm jmp TIDirectSoundStream.SetI3DL2Source; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_Pause;
asm jmp TIDirectSoundStream.Pause; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_PauseEx;
asm jmp TIDirectSoundStream.PauseEx; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_Process;
asm jmp TIDirectSoundStream.Process; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_Release;
asm jmp TIDirectSoundStream.Release; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_GetStatus;
asm jmp TIDirectSoundStream.GetStatus; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_GetInfo;
asm jmp TIDirectSoundStream.GetInfo; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_Flush;
asm jmp TIDirectSoundStream.Flush; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_FlushEx;
asm jmp TIDirectSoundStream.FlushEx; end;

procedure XTL_EmuDirectSound_CDirectSoundStream_GetVoiceProperties;
asm jmp TIDirectSoundStream.GetVoiceProperties; end;

//
// Patches on XAudio* functions
//

procedure XTL_EmuXAudioCreateAdpcmFormat
(
  nChannels: WORD;
  nSamplesPerSec: DWORD;
  pwfx: LPXBOXADPCMWAVEFORMAT
); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : XAudioCreateAdpcmFormat').
      _(nChannels, 'nChannels').
      _(nSamplesPerSec, 'nSamplesPerSec').
      _(pwfx, 'pwfx').
    LogEnd();

  // Fill out the pwfx structure with the appropriate data
  pwfx.wfx.wFormatTag       := X_WAVE_FORMAT_XBOX_ADPCM;
  pwfx.wfx.nChannels        := nChannels;
  pwfx.wfx.nSamplesPerSec   := nSamplesPerSec;
  pwfx.wfx.nAvgBytesPerSec  := (nSamplesPerSec*nChannels * 36) div 64;
  pwfx.wfx.nBlockAlign      := nChannels * 36;
  pwfx.wfx.wBitsPerSample   := 4;
  pwfx.wfx.cbSize           := 2;
  pwfx.wSamplesPerBlock     := 64;

  EmuSwapFS(fsXbox);
end;

function XTL_EmuXAudioDownloadEffectsImage
(
    pszImageName: LPCSTR;
    pImageLoc: LPVOID;
    dwFlags: DWORD;
    ppImageDesc: PLPVOID
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuDSound : XAudioDownloadEffectsImage').
      _(pszImageName, 'pszImageName').
      _(pImageLoc, 'pImageLoc').
      _(dwFlags, 'dwFlags').
      _(ppImageDesc, 'ppImageDesc').
    LogEnd();

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

{.$MESSAGE 'PatrickvL reviewed up to here'}

exports // Keep this list sorted, with newlines between patch groups :

  XTL_EmuDirectSound_CDirectSound_AddRef,
  XTL_EmuDirectSound_CDirectSound_CommitDeferredSettings,
  XTL_EmuDirectSound_CDirectSound_CommitEffectData,
  XTL_EmuDirectSound_CDirectSound_Compact,
  XTL_EmuDirectSound_CDirectSound_CreateSoundBuffer,
  XTL_EmuDirectSound_CDirectSound_CreateSoundStream,
  XTL_EmuDirectSound_CDirectSound_DownloadEffectsImage,
  XTL_EmuDirectSound_CDirectSound_EnableHeadphones,
  XTL_EmuDirectSound_CDirectSound_GetCaps,
  XTL_EmuDirectSound_CDirectSound_GetEffectData,
  XTL_EmuDirectSound_CDirectSound_GetOutputLevels,
  XTL_EmuDirectSound_CDirectSound_GetSpeakerConfig,
  XTL_EmuDirectSound_CDirectSound_GetTime,
//  XTL_EmuDirectSound_CDirectSound_QueryInterface,
//  XTL_EmuDirectSound_CDirectSound_QueryInterfaceC,
  XTL_EmuDirectSound_CDirectSound_Release,
  XTL_EmuDirectSound_CDirectSound_SetAllParameters,
  XTL_EmuDirectSound_CDirectSound_SetCooperativeLevel,
  XTL_EmuDirectSound_CDirectSound_SetDistanceFactor,
  XTL_EmuDirectSound_CDirectSound_SetDopplerFactor,
  XTL_EmuDirectSound_CDirectSound_SetEffectData,
  XTL_EmuDirectSound_CDirectSound_SetI3DL2Listener,
  XTL_EmuDirectSound_CDirectSound_SetMixBinHeadroom,
  XTL_EmuDirectSound_CDirectSound_SetOrientation,
  XTL_EmuDirectSound_CDirectSound_SetPosition,
  XTL_EmuDirectSound_CDirectSound_SetRolloffFactor,
  XTL_EmuDirectSound_CDirectSound_SetVelocity,
  XTL_EmuDirectSound_CDirectSound_SynchPlayback,

  XTL_EmuDirectSound_CDirectSoundBuffer_GetCurrentPosition,
  XTL_EmuDirectSound_CDirectSoundBuffer_GetStatus,
  XTL_EmuDirectSound_CDirectSoundBuffer_GetVoiceProperties,
  XTL_EmuDirectSound_CDirectSoundBuffer_Lock,
  XTL_EmuDirectSound_CDirectSoundBuffer_Pause,
  XTL_EmuDirectSound_CDirectSoundBuffer_PauseEx,
  XTL_EmuDirectSound_CDirectSoundBuffer_Play,
  XTL_EmuDirectSound_CDirectSoundBuffer_PlayEx,
  XTL_EmuDirectSound_CDirectSoundBuffer_Release,
  XTL_EmuDirectSound_CDirectSoundBuffer_Restore,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetAllParameters,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetBufferData,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetConeAngles,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetConeOrientation,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetConeOutsideVolume,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetCurrentPosition,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetDistanceFactor,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetDopplerFactor,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetEG,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetFilter,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetFormat,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetFrequency,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetHeadroom,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetI3DL2Source,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetLFO,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetLoopRegion,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetMaxDistance,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetMinDistance,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetMixBins,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetMixBinVolumes,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetMode,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetNotificationPositions,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetOutputBuffer,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetPitch,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetPlayRegion,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetPosition,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetRolloffCurve,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetRolloffFactor,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetVelocity,
  XTL_EmuDirectSound_CDirectSoundBuffer_SetVolume,
  XTL_EmuDirectSound_CDirectSoundBuffer_Stop,
  XTL_EmuDirectSound_CDirectSoundBuffer_StopEx,
  XTL_EmuDirectSound_CDirectSoundBuffer_Unlock,

  XTL_EmuDirectSound_CDirectSoundStream_AddRef,
  XTL_EmuDirectSound_CDirectSoundStream_Discontinuity,
  XTL_EmuDirectSound_CDirectSoundStream_Flush,
  XTL_EmuDirectSound_CDirectSoundStream_FlushEx,
  XTL_EmuDirectSound_CDirectSoundStream_GetInfo,
  XTL_EmuDirectSound_CDirectSoundStream_GetStatus,
  XTL_EmuDirectSound_CDirectSoundStream_GetVoiceProperties,
//  XTL_EmuDirectSound_CDirectSoundStream_Initialize ,
  XTL_EmuDirectSound_CDirectSoundStream_Pause,
  XTL_EmuDirectSound_CDirectSoundStream_PauseEx,
  XTL_EmuDirectSound_CDirectSoundStream_Process,
  XTL_EmuDirectSound_CDirectSoundStream_Release,
  XTL_EmuDirectSound_CDirectSoundStream_SetAllParameters,
  XTL_EmuDirectSound_CDirectSoundStream_SetConeAngles,
  XTL_EmuDirectSound_CDirectSoundStream_SetConeOrientation,
  XTL_EmuDirectSound_CDirectSoundStream_SetConeOutsideVolume,
  XTL_EmuDirectSound_CDirectSoundStream_SetDistanceFactor,
  XTL_EmuDirectSound_CDirectSoundStream_SetDopplerFactor,
  XTL_EmuDirectSound_CDirectSoundStream_SetEG,
  XTL_EmuDirectSound_CDirectSoundStream_SetFilter,
  XTL_EmuDirectSound_CDirectSoundStream_SetFormat,
  XTL_EmuDirectSound_CDirectSoundStream_SetFrequency,
  XTL_EmuDirectSound_CDirectSoundStream_SetHeadroom,
  XTL_EmuDirectSound_CDirectSoundStream_SetI3DL2Source,
  XTL_EmuDirectSound_CDirectSoundStream_SetLFO,
  XTL_EmuDirectSound_CDirectSoundStream_SetMaxDistance,
  XTL_EmuDirectSound_CDirectSoundStream_SetMinDistance,
  XTL_EmuDirectSound_CDirectSoundStream_SetMixBins,
  XTL_EmuDirectSound_CDirectSoundStream_SetMixBinVolumes,
  XTL_EmuDirectSound_CDirectSoundStream_SetMode,
  XTL_EmuDirectSound_CDirectSoundStream_SetOutputBuffer,
  XTL_EmuDirectSound_CDirectSoundStream_SetPitch,
  XTL_EmuDirectSound_CDirectSoundStream_SetPosition,
  XTL_EmuDirectSound_CDirectSoundStream_SetRolloffCurve,
  XTL_EmuDirectSound_CDirectSoundStream_SetRolloffFactor,
  XTL_EmuDirectSound_CDirectSoundStream_SetVelocity,
  XTL_EmuDirectSound_CDirectSoundStream_SetVolume, // Among others, used by Smashing Drive

  XTL_EmuDirectSoundCreate,
  XTL_EmuDirectSoundCreateBuffer,
  XTL_EmuDirectSoundCreateStream,
  XTL_EmuDirectSoundDoWork,
  XTL_EmuDirectSoundGetSampleTime,
  XTL_EmuDirectSoundOverrideSpeakerConfig,
  XTL_EmuDirectSoundUseFullHRTF,
  XTL_EmuDirectSoundUseFullHRTF4Channel,
  XTL_EmuDirectSoundUseLightHRTF,
  XTL_EmuDirectSoundUseLightHRTF4Channel,

  XTL_EmuIDirectSound_AddRef,
  XTL_EmuIDirectSound_CommitDeferredSettings,
  XTL_EmuIDirectSound_CommitEffectData,
  XTL_EmuIDirectSound_Compact,
  XTL_EmuIDirectSound_CreateSoundBuffer,
  XTL_EmuIDirectSound_CreateSoundStream,
  XTL_EmuIDirectSound_DownloadEffectsImage,
  XTL_EmuIDirectSound_EnableHeadphones,
  XTL_EmuIDirectSound_GetCaps,
  XTL_EmuIDirectSound_GetEffectData,
  XTL_EmuIDirectSound_GetOutputLevels,
  XTL_EmuIDirectSound_GetSpeakerConfig,
  XTL_EmuIDirectSound_GetTime,
  XTL_EmuIDirectSound_QueryInterface,
  XTL_EmuIDirectSound_QueryInterfaceC,
  XTL_EmuIDirectSound_Release,
  XTL_EmuIDirectSound_SetAllParameters,
  XTL_EmuIDirectSound_SetCooperativeLevel,
  XTL_EmuIDirectSound_SetDistanceFactor,
  XTL_EmuIDirectSound_SetDopplerFactor,
  XTL_EmuIDirectSound_SetEffectData,
  XTL_EmuIDirectSound_SetI3DL2Listener,
  XTL_EmuIDirectSound_SetMixBinHeadroom,
  XTL_EmuIDirectSound_SetOrientation,
  XTL_EmuIDirectSound_SetPosition,
  XTL_EmuIDirectSound_SetRolloffFactor,
  XTL_EmuIDirectSound_SetVelocity,
  XTL_EmuIDirectSound_SynchPlayback,

  XTL_EmuIDirectSoundBuffer_AddRef,
  XTL_EmuIDirectSoundBuffer_GetCurrentPosition,
  XTL_EmuIDirectSoundBuffer_GetStatus,
  XTL_EmuIDirectSoundBuffer_GetVoiceProperties,
  XTL_EmuIDirectSoundBuffer_Lock,
  XTL_EmuIDirectSoundBuffer_Pause,
  XTL_EmuIDirectSoundBuffer_PauseEx,
  XTL_EmuIDirectSoundBuffer_Play,
  XTL_EmuIDirectSoundBuffer_PlayEx,
  XTL_EmuIDirectSoundBuffer_QueryInterface,
  XTL_EmuIDirectSoundBuffer_QueryInterfaceC,
  XTL_EmuIDirectSoundBuffer_Release,

  XTL_EmuIDirectSoundBuffer_Restore,
  XTL_EmuIDirectSoundBuffer_SetAllParameters,
  XTL_EmuIDirectSoundBuffer_SetBufferData,
  XTL_EmuIDirectSoundBuffer_SetConeAngles,
  XTL_EmuIDirectSoundBuffer_SetConeOrientation,
  XTL_EmuIDirectSoundBuffer_SetConeOutsideVolume,
  XTL_EmuIDirectSoundBuffer_SetCurrentPosition,
  XTL_EmuIDirectSoundBuffer_SetDistanceFactor,
  XTL_EmuIDirectSoundBuffer_SetDopplerFactor,
  XTL_EmuIDirectSoundBuffer_SetEG,
  XTL_EmuIDirectSoundBuffer_SetFilter,
  XTL_EmuIDirectSoundBuffer_SetFormat,
  XTL_EmuIDirectSoundBuffer_SetFrequency,
  XTL_EmuIDirectSoundBuffer_SetHeadroom,
  XTL_EmuIDirectSoundBuffer_SetI3DL2Source,
  XTL_EmuIDirectSoundBuffer_SetLFO,
  XTL_EmuIDirectSoundBuffer_SetLoopRegion,
  XTL_EmuIDirectSoundBuffer_SetMaxDistance,
  XTL_EmuIDirectSoundBuffer_SetMinDistance,
  XTL_EmuIDirectSoundBuffer_SetMixBins,
  XTL_EmuIDirectSoundBuffer_SetMixBinVolumes,
  XTL_EmuIDirectSoundBuffer_SetMode,
  XTL_EmuIDirectSoundBuffer_SetNotificationPositions,
  XTL_EmuIDirectSoundBuffer_SetOutputBuffer,
  XTL_EmuIDirectSoundBuffer_SetPitch,
  XTL_EmuIDirectSoundBuffer_SetPlayRegion,
  XTL_EmuIDirectSoundBuffer_SetPosition,
  XTL_EmuIDirectSoundBuffer_SetRolloffCurve,
  XTL_EmuIDirectSoundBuffer_SetRolloffFactor,
  XTL_EmuIDirectSoundBuffer_SetVelocity,
  XTL_EmuIDirectSoundBuffer_SetVolume,
  XTL_EmuIDirectSoundBuffer_Stop,
  XTL_EmuIDirectSoundBuffer_StopEx,
  XTL_EmuIDirectSoundBuffer_Unlock,

  XTL_EmuIDirectSoundStream_FlushEx,
  XTL_EmuIDirectSoundStream_GetVoiceProperties,
  XTL_EmuIDirectSoundStream_Pause,
  XTL_EmuIDirectSoundStream_PauseEx,
  XTL_EmuIDirectSoundStream_QueryInterface,
  XTL_EmuIDirectSoundStream_QueryInterfaceC,
  XTL_EmuIDirectSoundStream_SetAllParameters,
  XTL_EmuIDirectSoundStream_SetConeAngles,
  XTL_EmuIDirectSoundStream_SetConeOrientation,
  XTL_EmuIDirectSoundStream_SetConeOutsideVolume,
  XTL_EmuIDirectSoundStream_SetDistanceFactor,
  XTL_EmuIDirectSoundStream_SetDopplerFactor,
  XTL_EmuIDirectSoundStream_SetEG,
  XTL_EmuIDirectSoundStream_SetFilter,
  XTL_EmuIDirectSoundStream_SetFormat,
  XTL_EmuIDirectSoundStream_SetFrequency,
  XTL_EmuIDirectSoundStream_SetHeadroom,
  XTL_EmuIDirectSoundStream_SetI3DL2Source,
  XTL_EmuIDirectSoundStream_SetLFO,
  XTL_EmuIDirectSoundStream_SetMaxDistance,
  XTL_EmuIDirectSoundStream_SetMinDistance,
  XTL_EmuIDirectSoundStream_SetMixBins,
  XTL_EmuIDirectSoundStream_SetMixBinVolumes,
  XTL_EmuIDirectSoundStream_SetMode,
  XTL_EmuIDirectSoundStream_SetOutputBuffer,
  XTL_EmuIDirectSoundStream_SetPitch,
  XTL_EmuIDirectSoundStream_SetPosition,
  XTL_EmuIDirectSoundStream_SetRolloffCurve,
  XTL_EmuIDirectSoundStream_SetRolloffFactor,
  XTL_EmuIDirectSoundStream_SetVelocity,
  XTL_EmuIDirectSoundStream_SetVolume,

  XTL_EmuXAudioCreateAdpcmFormat,
  XTL_EmuXAudioDownloadEffectsImage;

end.

