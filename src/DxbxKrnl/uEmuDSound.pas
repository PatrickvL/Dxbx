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
  , uTypes
  , uLog
  , uDxbxKrnlUtils
  , uEmuAlloc // DxbxMalloc
  , uEmuFS // EmuSwapFS
  , uEmuKrnl // Unimplemented
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
  end;

  XTL_PIDirectSoundStream = type PInterface;

  LPDIRECTSOUND = type PInterface;

  LPDIRECTSOUNDSTREAM = XTL_PIDirectSoundStream;

// Static Variable(s)
var g_pDSound8: XTL_LPDIRECTSOUND8 = NULL;
var g_pDSound8RefCount: int = 0;
var g_pDSoundBufferCache: array [0..SOUNDBUFFER_CACHE_SIZE-1] of PX_CDirectSoundBuffer;
var g_pDSoundStreamCache: array [0..SOUNDSTREAM_CACHE_SIZE-1] of PX_CDirectSoundStream;
var g_bDSoundCreateCalled: Boolean = false; // Dxbx note : Boolean is simpler than Cxbx's int.

{implementation}

const lfUnit = lfCxbx or lfSound;

function iif(const aValue: Boolean; const aTrue, aFalse: DirectSound.PDSBUFFERDESC): DirectSound.PDSBUFFERDESC; overload;
begin
  if aValue then
    Result := aTrue
  else
    Result := aFalse;
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
    if (g_pDSoundStreamCache[v] = nil) or (g_pDSoundStreamCache[v].EmuBuffer = nil) then
      continue;





    hRet := IDirectSoundBuffer(g_pDSoundStreamCache[v].EmuDirectSoundBuffer8).Lock(0, g_pDSoundStreamCache[v].EmuBufferDesc.dwBufferBytes, @pAudioPtr, @dwAudioBytes, @pAudioPtr2, @dwAudioBytes2, 0);

    if (SUCCEEDED(hRet)) then
    begin
      if (pAudioPtr <> nil) then
        memcpy(pAudioPtr, g_pDSoundStreamCache[v].EmuBuffer, dwAudioBytes);

      if (pAudioPtr2 <> nil) then
        memcpy(pAudioPtr2, PVOID(DWORD(g_pDSoundStreamCache[v].EmuBuffer)+dwAudioBytes), dwAudioBytes2);

      IDirectSoundBuffer(g_pDSoundStreamCache[v].EmuDirectSoundBuffer8).Unlock(pAudioPtr, dwAudioBytes, pAudioPtr2, dwAudioBytes2);
    end;

    IDirectSoundBuffer(g_pDSoundStreamCache[v].EmuDirectSoundBuffer8).SetCurrentPosition(0);
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
      DxbxKrnlCleanup('IDirectSoundBuffer8 resize Failed!');

    IDirectSoundBuffer(pBuffer.EmuDirectSoundBuffer8).SetCurrentPosition(dwPlayCursor);

    if (dwStatus and DSBSTATUS_PLAYING) > 0 then
      IDirectSoundBuffer(pBuffer.EmuDirectSoundBuffer8).Play(0, 0, pBuffer.EmuPlayFlags);
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
      DxbxKrnlCleanup('IDirectSoundBuffer8 resize Failed!');

    IDirectSoundBuffer(pStream.EmuDirectSoundBuffer8).SetCurrentPosition(dwPlayCursor);

    if (dwStatus and DSBSTATUS_PLAYING) > 0 then
      IDirectSoundBuffer(pStream.EmuDirectSoundBuffer8).Play(0, 0, pStream.EmuPlayFlags);
  end;
end;


function DxbxAssureDirectSoundCreate(const FromOriginalDSoundCreate: Boolean = False): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:100
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

//
// TXMediaObject - no patches, but a real class implementation as a VMT-replacement for the Xbox version
//

function TXMediaObject.AddRef(): ULONG; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuDSound : XMediaObject.AddRef' +
      #13#10'(' +
      #13#10'   pXMediaObject             : 0x%.08X' +
      #13#10');',
      [Self]);

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
    DbgPrintf('EmuDSound : XMediaObject.Release' +
      #13#10'(' +
      #13#10'   pXMediaObject             : 0x%.08X' +
      #13#10');',
      [Self]);

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
    DbgPrintf('EmuDSound : XMediaObject.GetInfo' +
      #13#10'(' +
      #13#10'   pXMediaObject             : 0x%.08X' +
      #13#10'   pInfo                     : 0x%.08X' +
      #13#10');',
      [Self, pInfo]);

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
    DbgPrintf('EmuDSound : XMediaObject.GetStatus' +
      #13#10'(' +
      #13#10'   pXMediaObject             : 0x%.08X' +
      #13#10'   pdwStatus                 : 0x%.08X' +
      #13#10');',
      [Self, pdwStatus]);

  EmuWarning('XMediaObject.GetStatus is not yet implemented');

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
    DbgPrintf('EmuDSound : XMediaObject.Process' +
      #13#10'(' +
      #13#10'   pXMediaObject             : 0x%.08X' +
      #13#10'   pInputBuffer              : 0x%.08X' +
      #13#10'   pOutputBuffer             : 0x%.08X' +
      #13#10');',
      [Self, pInputBuffer, pOutputBuffer]);

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
    DbgPrintf('EmuDSound : XMediaObject.Discontinuity' +
      #13#10'(' +
      #13#10'   pXMediaObject             : 0x%.08X' +
      #13#10');',
      [Self]);

  // Causes deadlock in Halo...
  // TODO -oCxbx: Verify that this is a Vista related problem (I HATE Vista!)
  //    EmuWarning('EmuCMcpxStream_Dummy_0x10 is ignored!');

  // TODO -oDXBX: Actually Process

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TXMediaObject.Flush(): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuDSound : XMediaObject.Flush' +
      #13#10'(' +
      #13#10'   pXMediaObject             : 0x%.08X' +
      #13#10');',
      [Self]);

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
    DbgPrintf('EmuDSound : DirectSoundCreate' +
      #13#10'(' +
      #13#10'   pguidDeviceId             : 0x%.08X' +
      #13#10'   ppDirectSound             : 0x%.08X' +
      #13#10'   pUnkOuter                 : 0x%.08X' +
      #13#10');',
      [pguidDeviceId, ppDirectSound, pUnkOuter]);

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
    DbgPrintf('EmuDSound : DirectSoundCreateBuffer' +
      #13#10'(' +
      #13#10'   pdsbd                     : 0x%.08X' +
      #13#10'   ppBuffer                  : 0x%.08X' +
      #13#10');',
      [pdsbd, ppBuffer]);

  dwEmuFlags := 0;


  // Dxbx note : When and how should we create a IDirectSound3DBuffer ?
  // It will be needed to emulate SetConeAngles and other functions!

  pDSBufferDesc := DirectSound.PDSBUFFERDESC(DxbxMalloc(sizeof(DSBUFFERDESC)));
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
      pDSBufferDesc.lpwfxFormat := {PWAVEFORMATEX}DxbxMalloc(sizeof(WAVEFORMATEX) + pdsbd.lpwfxFormat.cbSize);
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
    DbgPrintf('EmuDSound : DirectSoundCreateStream' +
      #13#10'(' +
      #13#10'   pdssd                     : 0x%.08X' +
      #13#10'   ppStream                  : 0x%.08X' +
      #13#10');',
      [pdssd, ppStream]);

  // TODO -oCXBX: Garbage Collection
  ppStream^ := TIDirectSoundStream.Create;

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

{static} var dwStart: DWORD = 0;
function XTL_EmuDirectSoundGetSampleTime(): DWORD; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
var
  dwRet: DWORD;
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuDSound : DirectSoundGetSampleTime();');

  // FIXME: This is the best I could think of for now.
  // Check the XDK documentation for the description of what this function
  // can actually do.  BTW, this function accesses the NVIDIA SoundStorm APU
  // register directly (0xFE80200C).

  // TODO -oCXBX: Handle reset at certain event?
  // TODO -oCXBX: Wait until a DirectSoundBuffer/Stream is being played?
  if dwStart = 0 then dwStart := GetTickCount(); // Dxbx note : Assign static var only once
  dwRet := GetTickCount() - dwStart;

  EmuSwapFS(fsXbox);

  Result := dwRet; // TODO -oDXBX: Should we (and Cxbx) really return dwRet here?
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
    DbgPrintf('EmuDSound : TIDirectSound.AddRef' +
      #13#10'(' +
      #13#10'   pDirectSound              : 0x%.08X' +
      #13#10');',
      [Self]);

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
    DbgPrintf('EmuDSound : TIDirectSound.Release' +
      #13#10'(' +
      #13#10'   pDirectSound              : 0x%.08X' +
      #13#10');',
      [Self]);

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
    DbgPrintf('EmuDSound : TIDirectSound.GetCaps' +
      #13#10'(' +
      #13#10'   pDirectSound        : 0x%.08X' +
      #13#10'   pDSCaps             : 0x%.08X' +
      #13#10');',
      [Self, pdsc]);

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
    DbgPrintf('EmuDSound : TIDirectSound.CreateSoundBuffer' +
           #13#10'(' +
           #13#10'   pDirectSound              : 0x%.08X' +
           #13#10'   pdsbd                     : 0x%.08X' +
           #13#10'   ppBuffer                  : 0x%.08X' +
           #13#10'   pUnkOuter                 : 0x%.08X' +
           #13#10');',
           [Self, pdsbd, ppBuffer, pUnkOuter]);

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
    DbgPrintf('EmuDSound : TIDirectSound.CreateSoundStream' +
           #13#10'(' +
           #13#10'   pDirectSound              : 0x%.08X' +
           #13#10'   pdssd                     : 0x%.08X' +
           #13#10'   ppStream                  : 0x%.08X' +
           #13#10'   pUnkOuter                 : 0x%.08X' +
           #13#10');',
           [Self, pdssd, ppStream, pUnkOuter]);

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
    DbgPrintf('EmuDSound : TIDirectSound.GetSpeakerConfig' +
      #13#10'(' +
      #13#10'   pDirectSound              : 0x%.08X' +
      #13#10'   pdwSpeakerConfig          : 0x%.08X' +
      #13#10');',
      [Self, pdwSpeakerConfig]);

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
    DbgPrintf('EmuDSound : TIDirectSound.SetCooperativeLevel' +
      #13#10'(' +
      #13#10'   pDirectSound              : 0x%.08X' +
      #13#10'   hWnd                      : 0x%.08X' +
      #13#10'   dwLevel                   : 0x%.08X' +
      #13#10');',
      [Self, hWnd, dwLevel]);

  // TODO -oDxbx : Should we allow this call? And must we pass hWnd or g_hEmuWindow ?
  Result := IDirectSound8(g_pDSound8).SetCooperativeLevel(hWnd, dwLevel);

  EmuSwapFS(fsXbox);
end;

function TIDirectSound.Compact(): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:Shadow_tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuDSound : TIDirectSound.Compact' +
      #13#10'(' +
      #13#10'   pDirectSound              : 0x%.08X' +
      #13#10');',
      [Self]);

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
    DbgPrintf('EmuDSound : TIDirectSound.DownloadEffectsImage' +
      #13#10'(' +
      #13#10'   pDirectSound              : 0x%.08X' +
      #13#10'   pvImageBuffer             : 0x%.08X' +
      #13#10'   dwImageSize               : 0x%.08X' +
      #13#10'   pImageLoc                 : 0x%.08X' +
      #13#10'   ppImageDesc               : 0x%.08X' +
      #13#10');',
      [Self, pvImageBuffer, dwImageSize, pImageLoc, ppImageDesc]);

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
    DbgPrintf('EmuDSound : TIDirectSound.EnableHeadphones' +
      #13#10'(' +
      #13#10'   pDirectSound              : 0x%.08X' +
      #13#10'   fEnabled                  : 0x%.08X' +
      #13#10');',
      [Self, fEnabled]);

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
    DbgPrintf('EmuDSound : TIDirectSound.SetMixBinHeadroom' +
        #13#10'(' +
        #13#10'   pDirectSound              : 0x%.08X' +
        #13#10'   dwMixBinMask              : 0x%.08X' +
        #13#10'   dwHeadroom                : 0x%.08X' +
        #13#10');',
        [Self, dwMixBin, dwHeadroom]);

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
    DbgPrintf('EmuDSound : TIDirectSound.SetAllParameters' +
        #13#10'(' +
        #13#10'   pDirectSound              : 0x%.08X' +
        #13#10'   pds3dl                    : 0x%.08X' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [Self, pds3dl, dwApply]);

  // TODO -oCXBX: Actually do something

  // TODO -oDxbx : Call upon the PrimaryBuffer (but do check for the existence of a Listener!) :
  // IDirectSound3DListener(Self.EmuListener).SetAllParameters(nil{???}, dwApply);

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
    DbgPrintf('EmuDSound : TIDirectSound.SetOrientation' +
      #13#10'(' +
      #13#10'   pDirectSound              : 0x%.08X' +
      #13#10'   xFront                    : %f' +
      #13#10'   yFront                    : %f' +
      #13#10'   zFront                    : %f' +
      #13#10'   xTop                      : %f' +
      #13#10'   yTop                      : %f' +
      #13#10'   zTop                      : %f' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [Self, xFront, yFront, zFront, xTop, yTop, zTop, dwApply]);

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
    DbgPrintf('EmuDSound : TIDirectSound.SetPosition' +
        #13#10'(' +
        #13#10'   pDirectSound              : 0x%.08X' +
        #13#10'   x                         : %f' +
        #13#10'   y                         : %f' +
        #13#10'   z                         : %f' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [Self, x, y, z, dwApply]);

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
    DbgPrintf('EmuDSound : TIDirectSound.SetVelocity' +
        #13#10'(' +
        #13#10'   pDirectSound              : 0x%.08X' +
        #13#10'   x                         : %f' +
        #13#10'   y                         : %f' +
        #13#10'   z                         : %f' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [Self, x, y, z, dwApply]);

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
    DbgPrintf('EmuDSound : TIDirectSound.SetDistanceFactor' +
      #13#10'(' +
      #13#10'   pDirectSound              : 0x%.08X' +
      #13#10'   flDistanceFactor          : %f' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [Self, flDistanceFactor, dwApply]);

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
    DbgPrintf('EmuDSound : TIDirectSound.SetDopplerFactor' +
      #13#10'(' +
      #13#10'   pDirectSound              : 0x%.08X' +
      #13#10'   flDopplerFactor           : %f' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [Self, flDopplerFactor, dwApply]);

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
    DbgPrintf('EmuDSound : TIDirectSound.SetRolloffFactor' +
      #13#10'(' +
      #13#10'   pDirectSound              : 0x%.08X' +
      #13#10'   flRolloffFactor           : %f' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [Self, flRolloffFactor, dwApply]);

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
    DbgPrintf('EmuDSound : TIDirectSound.SetI3DL2Listener' +
       #13#10'(' +
       #13#10'   pDirectSound              : 0x%.08X' +
       #13#10'   pds3dl                    : 0x%.08X' +
       #13#10'   dwApply                   : 0x%.08X' +
       #13#10');',
       [Self, pds3dl, dwApply]);

  // TODO -oCXBX: Actually do something

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSound.CommitDeferredSettings(): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuDSound : TIDirectSound.CommitDeferredSettings' +
      #13#10'(' +
      #13#10'   pDirectSound              : 0x%.08X' +
      #13#10');',
      [Self]);

  // TODO -oCXBX: Translate params, then make the PC DirectSound call

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSound.GetTime
(
    prtCurrent: PREFERENCE_TIME
): HRESULT; stdcall; // virtual;
// Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Result := Unimplemented('TIDirectSound.GetTime');

  EmuSwapFS(fsXbox);
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
    DbgPrintf('EmuDSound : TIDirectSound.GetOutputLevels' +
      #13#10'(' +
      #13#10'   pDirectSound            : 0x%.08X' +
      #13#10'   pOutputLevels           : 0x%.08X' +
      #13#10'   bResetPeakValues        : 0x%.08X' +
      #13#10');',
      [Self, pOutputLevels, fResetPeakValues]);

  // TODO -oCXBX: Anything?  Either way, I've never seen a game to date use this...

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSound.SynchPlayback(): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuDSound : TIDirectSound.SynchPlayback' +
      #13#10'(' +
      #13#10'   pDirectSound              : 0x%.08X' +
      #13#10');',
      [Self]);

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

procedure XTL_EmuCDirectSound_AddRef;
asm  jmp TIDirectSound.AddRef; end;

procedure XTL_EmuCDirectSound_Release;
asm jmp TIDirectSound.Release; end;

procedure XTL_EmuCDirectSound_GetCaps;
asm jmp TIDirectSound.GetCaps; end;

procedure XTL_EmuCDirectSound_CreateSoundBuffer;
asm jmp TIDirectSound.CreateSoundBuffer; end;

procedure XTL_EmuCDirectSound_CreateSoundStream;
asm jmp TIDirectSound.CreateSoundStream; end;

procedure XTL_EmuCDirectSound_GetSpeakerConfig;
asm jmp TIDirectSound.GetSpeakerConfig; end;

procedure XTL_EmuCDirectSound_SetCooperativeLevel;
asm jmp TIDirectSound.SetCooperativeLevel; end;

procedure XTL_EmuCDirectSound_Compact;
asm jmp TIDirectSound.Compact; end;

procedure XTL_EmuCDirectSound_DownloadEffectsImage;
asm jmp TIDirectSound.DownloadEffectsImage; end;

procedure XTL_EmuCDirectSound_GetEffectData;
asm jmp TIDirectSound.GetEffectData; end;

procedure XTL_EmuCDirectSound_SetEffectData;
asm jmp TIDirectSound.SetEffectData; end;

procedure XTL_EmuCDirectSound_CommitEffectData;
asm jmp TIDirectSound.CommitEffectData; end;

procedure XTL_EmuCDirectSound_EnableHeadphones;
asm jmp TIDirectSound.EnableHeadphones; end;

procedure XTL_EmuCDirectSound_SetMixBinHeadroom;
asm jmp TIDirectSound.SetMixBinHeadroom; end;

procedure XTL_EmuCDirectSound_SetAllParameters;
asm jmp TIDirectSound.SetAllParameters; end;

procedure XTL_EmuCDirectSound_SetOrientation;
asm jmp TIDirectSound.SetOrientation; end;

procedure XTL_EmuCDirectSound_SetPosition;
asm jmp TIDirectSound.SetPosition; end;

procedure XTL_EmuCDirectSound_SetVelocity;
asm jmp TIDirectSound.SetVelocity; end;

procedure XTL_EmuCDirectSound_SetDistanceFactor;
asm jmp TIDirectSound.SetDistanceFactor; end;

procedure XTL_EmuCDirectSound_SetDopplerFactor;
asm jmp TIDirectSound.SetDopplerFactor; end;

procedure XTL_EmuCDirectSound_SetRolloffFactor;
asm jmp TIDirectSound.SetRolloffFactor; end;

procedure XTL_EmuCDirectSound_SetI3DL2Listener;
asm jmp TIDirectSound.SetI3DL2Listener; end;

procedure XTL_EmuCDirectSound_CommitDeferredSettings;
asm jmp TIDirectSound.CommitDeferredSettings; end;

procedure XTL_EmuCDirectSound_GetTime;
asm jmp TIDirectSound.GetTime; end;

procedure XTL_EmuCDirectSound_GetOutputLevels;
asm jmp TIDirectSound.GetOutputLevels; end;

procedure XTL_EmuCDirectSound_SynchPlayback;
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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.AddRef' +
      #13#10'(' +
      #13#10'   pBuffer                 : 0x%.08X' +
      #13#10');',
      [Self]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.Release' +
         #13#10'(' +
         #13#10'   pBuffer                   : 0x%.08X' +
         #13#10');',
         [Self]);

  uRet := 0;

  if (Self <> nil) then
  begin
    if (0=(Self.EmuFlags and DSB_FLAG_RECIEVEDATA)) then
    begin
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
          DxbxFree(Self.EmuBufferDesc.lpwfxFormat);

        DxbxFree(Self.EmuBufferDesc);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetFormat' +
        #13#10'(' +
        #13#10'   pBuffer                   : 0x%.08X' +
        #13#10'   pwfxFormat                : 0x%.08X' +
        #13#10');',
        [Self, pwfxFormat]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetFrequency' +
         #13#10'(' +
         #13#10'   pBuffer                   : 0x%.08X' +
         #13#10'   dwFrequency               : 0x%.08X' +
         #13#10');',
         [Self, dwFrequency]);

  Result := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).SetFrequency(dwFrequency); // TODO -oDxbx : Test this!
//  Result := DS_OK;

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetVolume' +
         #13#10'(' +
         #13#10'   pBuffer                   : 0x%.08X' +
         #13#10'   lVolume                   : 0x%.08X' +
         #13#10');',
         [Self, lVolume]);

  Result := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).SetVolume(lVolume);
//  Result := DS_OK;

  EmuSwapFS(fsXbox);
end;

function TIDirectSoundBuffer.SetPitch
(
    lPitch: LONG
): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetPitch' +
         #13#10'(' +
         #13#10'   pBuffer                   : 0x%.08X' +
         #13#10'   lPitch                    : 0x%.08X' +
         #13#10');',
         [Self, lPitch]);

  // TODO -oCXBX: Translate params, then make the PC DirectSound call
  Result := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).SetVolume(lPitch);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetLFO' +
      #13#10'(' +
      #13#10'   pBuffer                   : 0x%.08X' +
      #13#10'   pLFODesc                  : 0x%.08X' +
      #13#10');',
      [Self, pLFODesc]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetFilter' +
      #13#10'(' +
      #13#10'   pBuffer             : 0x%.08X' +
      #13#10'   pFilterDesc         : 0x%.08X' +
      #13#10');',
      [Self, pFilterDesc]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetHeadroom' +
         #13#10'(' +
         #13#10'   pBuffer                   : 0x%.08X' +
         #13#10'   dwHeadroom                : 0x%.08X' +
         #13#10');',
         [Self, dwHeadroom]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetMixBins' +
        #13#10'(' +
        #13#10'   pBuffer                   : 0x%.08X' +
        #13#10'   pMixBins                  : 0x%.08X' +
        #13#10');',
        [Self, pMixBins]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetMixBinVolumes' +
        #13#10'(' +
        #13#10'   pBuffer                   : 0x%.08X' +
        #13#10'   pMixBins                  : 0x%.08X' +
        #13#10');',
        [Self, pMixBins]);

  // TODO -oCXBX: Actually do something

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetConeAngles' +
        #13#10'(' +
        #13#10'   pBuffer                   : 0x%.08X' +
        #13#10'   dwInsideConeAngle         : 0x%.08X' +
        #13#10'   dwOutsideConeAngle        : 0x%.08X' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [Self, dwInsideConeAngle,
        dwOutsideConeAngle, dwApply]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetConeOrientation' +
        #13#10'(' +
        #13#10'   pBuffer                   : 0x%.08X' +
        #13#10'   x                         : %f' +
        #13#10'   y                         : %f' +
        #13#10'   z                         : %f' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [Self, x, y, z, dwApply]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetConeOutsideVolume' +
        #13#10'(' +
        #13#10'   pBuffer                   : 0x%.08X' +
        #13#10'   lConeOutsideVolume        : 0x%.08X' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [Self, lConeOutsideVolume, dwApply]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetMaxDistance' +
        #13#10'(' +
        #13#10'   pBuffer                   : 0x%.08X' +
        #13#10'   flMaxDistance             : %f' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [Self, flMaxDistance, dwApply]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetMinDistance' +
        #13#10'(' +
        #13#10'   pBuffer                   : 0x%.08X' +
        #13#10'   flMinDistance             : %f' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [Self, flMinDistance, dwApply]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetMode' +
      #13#10'(' +
      #13#10'   pBuffer             : 0x%.08X' +
      #13#10'   dwMode              : 0x%.08X' +
      #13#10'   dwApply             : 0x%.08X' +
      #13#10');',
      [Self, dwMode, dwApply]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetPosition' +
        #13#10'(' +
        #13#10'   pBuffer                   : 0x%.08X' +
        #13#10'   x                         : %f' +
        #13#10'   y                         : %f' +
        #13#10'   z                         : %f' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [Self, x, y, z, dwApply]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetVelocity' +
        #13#10'(' +
        #13#10'   pBuffer                   : 0x%.08X' +
        #13#10'   x                         : %f' +
        #13#10'   y                         : %f' +
        #13#10'   z                         : %f' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [Self, x, y, z, dwApply]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetDistanceFactor' +
        #13#10'(' +
        #13#10'   pBuffer                   : 0x%.08X' +
        #13#10'   flDistanceFactor          : %f' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [Self, flDistanceFactor, dwApply]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetDopplerFactor' +
         #13#10'(' +
         #13#10'   pBuffer                   : 0x%.08X' +
         #13#10'   flDopplerFactor           : %f' +
         #13#10'   dwApply                   : 0x%.08X' +
         #13#10');',
         [Self, flDopplerFactor, dwApply]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetRolloffFactor' +
        #13#10'(' +
        #13#10'   pBuffer                   : 0x%.08X' +
        #13#10'   flRolloffFactor           : %f' +
        #13#10'   dwApply                   : 0x%.08X' +
        #13#10');',
        [Self, flRolloffFactor, dwApply]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetRolloffCurve' +
      #13#10'(' +
      #13#10'   pBuffer                   : 0x%.08X' +
      #13#10'   pflPoints                 : 0x%.08X' +
      #13#10'   dwPointCount              : 0x%.08X' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [Self, pflPoints, dwPointCount, dwApply]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetI3DL2Source' +
           #13#10'(' +
           #13#10'   pBuffer                   : 0x%.08X' +
           #13#10'   pds3db                    : 0x%.08X' +
           #13#10'   dwApply                   : 0x%.08X' +
           #13#10');',
           [Self, pds3db, dwApply]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.Play' +
      #13#10'(' +
      #13#10'   pBuffer                   : 0x%.08X' +
      #13#10'   dwReserved1               : 0x%.08X' +
      #13#10'   dwReserved2               : 0x%.08X' +
      #13#10'   dwFlags                   : 0x%.08X' +
      #13#10');',
      [Self, dwReserved1, dwReserved2, dwFlags]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.PlayEx' +
      #13#10'(' +
      #13#10'   pBuffer                   : 0x%.08X' +
      #13#10'   rtTimeStamp               : 0x%.08X' +
      #13#10'   dwFlags                   : 0x%.08X' +
      #13#10');',
      [Self, rtTimeStamp, dwFlags]);

  if(Self.EmuDirectSoundBuffer8 = nil) then
    EmuWarning('pBuffer.EmuDirectSoundBuffer8 == 0');

  EmuWarning('PlayEx not yet implemented!');

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.Stop' +
         #13#10'(' +
         #13#10'   pBuffer                   : 0x%.08X' +
         #13#10');',
         [Self]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.StopEx' +
         #13#10'(' +
         #13#10'   pBuffer                   : 0x%.08X' +
         #13#10'   rtTimeStamp               : 0x%.08X' +
         #13#10'   dwFlags                   : 0x%.08X' +
         #13#10');',
         [Self, rtTimeStamp, dwFlags]);

  if (Self.EmuDirectSoundBuffer8 = nil) then
    EmuWarning('pBuffer.EmuDirectSoundBuffer8 == 0');

  EmuWarning('StopEx not yet implemented!');

  EmuSwapFS(fsXbox);

  Result := DS_OK;
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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.Pause' +
      #13#10'(' +
      #13#10'   pBuffer                 : 0x%.08X' +
      #13#10'   dwPause                 : 0x%.08X' +
      #13#10');',
      [Self, dwPause]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.PauseEx' +
      #13#10'(' +
      #13#10'   pBuffer                 : 0x%.08X' +
      #13#10'   rtTimestamp             : 0x%.08X' +
      #13#10'   dwPause                 : 0x%.08X' +
      #13#10');',
      [Self, rtTimestamp, dwPause]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetPlayRegion' +
         #13#10'(' +
         #13#10'   pBuffer                   : 0x%.08X' +
         #13#10'   dwPlayStart               : 0x%.08X' +
         #13#10'   dwPlayLength              : 0x%.08X' +
         #13#10');',
         [Self, dwPlayStart, dwPlayLength]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetLoopRegion' +
         #13#10'(' +
         #13#10'   pBuffer                   : 0x%.08X' +
         #13#10'   dwLoopStart               : 0x%.08X' +
         #13#10'   dwLoopLength              : 0x%.08X' +
         #13#10');',
         [Self, dwLoopStart, dwLoopLength]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.GetStatus' +
         #13#10'(' +
         #13#10'   pBuffer                   : 0x%.08X' +
         #13#10'   pdwStatus                 : 0x%.08X' +
         #13#10');',
         [Self, pdwStatus]);

  hRet := DS_OK;

  if (Self <> nil) and (Self.EmuBuffer = nil) then
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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.GetCurrentPosition' +
         #13#10'(' +
         #13#10'   pBuffer                   : 0x%.08X' +
         #13#10'   pdwCurrentPlayCursor      : 0x%.08X' +
         #13#10'   pdwCurrentWriteCursor     : 0x%.08X' +
         #13#10');',
         [Self, pdwPlayCursor, pdwWriteCursor]);

  DxbxHackUpdateSoundBuffers();
  DxbxHackUpdateSoundStreams();

  // NOTE: TODO -oCXBX: This call always seems to fail on primary buffers!
  hRet := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).GetCurrentPosition(pdwPlayCursor, pdwWriteCursor);

  if (FAILED(hRet)) then
    EmuWarning('GetCurrentPosition Failed!');

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetCurrentPosition' +
         #13#10'(' +
         #13#10'   pBuffer                   : 0x%.08X' +
         #13#10'   dwNewPosition             : 0x%.08X' +
         #13#10');',
         [Self, dwPlayCursor]);

  // NOTE: TODO -oCXBX: This call *will* (by MSDN) fail on primary buffers!
  hRet := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).SetCurrentPosition(dwPlayCursor);

  if (FAILED(hRet)) then
    EmuWarning('SetCurrentPosition Failed!');

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.SetBufferData' +
         #13#10'(' +
         #13#10'   pBuffer                   : 0x%.08X' +
         #13#10'   pvBufferData              : 0x%.08X' +
         #13#10'   dwBufferBytes             : 0x%.08X' +
         #13#10');',
         [Self, pvBufferData, dwBufferBytes]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.Lock' +
         #13#10'(' +
         #13#10'   pBuffer                   : 0x%.08X' +
         #13#10'   dwOffset                  : 0x%.08X' +
         #13#10'   dwBytes                   : 0x%.08X' +
         #13#10'   ppvAudioPtr1              : 0x%.08X' +
         #13#10'   pdwAudioBytes1            : 0x%.08X' +
         #13#10'   ppvAudioPtr2              : 0x%.08X' +
         #13#10'   pdwAudioBytes2            : 0x%.08X' +
         #13#10'   dwFlags                   : 0x%.08X' +
         #13#10');',
         [Self, dwOffset, dwBytes, ppvAudioPtr1, pdwAudioBytes1,
         ppvAudioPtr2, pdwAudioBytes2, dwFlags]);

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
    DbgPrintf('EmuDSound : TIDirectSoundBuffer.GetVoiceProperties' +
      #13#10'(' +
      #13#10'   pBuffer                 : 0x%.08X' +
      #13#10'   pVoiceProps             : 0x%.08X' +
      #13#10');',
      [Self, pVoiceProps]);

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

procedure XTL_EmuCDirectSoundBuffer_GetCurrentPosition;
asm jmp TIDirectSoundBuffer.GetCurrentPosition; end;

procedure XTL_EmuCDirectSoundBuffer_GetStatus;
asm jmp TIDirectSoundBuffer.GetStatus; end;

procedure XTL_EmuCDirectSoundBuffer_GetVoiceProperties;
asm jmp TIDirectSoundBuffer.GetVoiceProperties; end;

procedure XTL_EmuCDirectSoundBuffer_Lock;
asm jmp TIDirectSoundBuffer.Lock; end;

procedure XTL_EmuCDirectSoundBuffer_Pause;
asm jmp TIDirectSoundBuffer.Pause; end;

procedure XTL_EmuCDirectSoundBuffer_PauseEx;
asm jmp TIDirectSoundBuffer.PauseEx; end;

procedure XTL_EmuCDirectSoundBuffer_Play;
asm jmp TIDirectSoundBuffer.Play; end;

procedure XTL_EmuCDirectSoundBuffer_PlayEx;
asm jmp TIDirectSoundBuffer.PlayEx; end;

procedure XTL_EmuCDirectSoundBuffer_Restore;
asm jmp TIDirectSoundBuffer.Restore; end;

procedure XTL_EmuCDirectSoundBuffer_SetAllParameters;
asm jmp TIDirectSoundBuffer.SetAllParameters; end;

procedure XTL_EmuCDirectSoundBuffer_SetBufferData;
asm jmp TIDirectSoundBuffer.SetBufferData; end;

procedure XTL_EmuCDirectSoundBuffer_SetConeAngles;
asm jmp TIDirectSoundBuffer.SetConeAngles; end;

procedure XTL_EmuCDirectSoundBuffer_SetConeOrientation;
asm jmp TIDirectSoundBuffer.SetConeOrientation; end;

procedure XTL_EmuCDirectSoundBuffer_SetConeOutsideVolume;
asm jmp TIDirectSoundBuffer.SetConeOutsideVolume; end;

procedure XTL_EmuCDirectSoundBuffer_SetCurrentPosition;
asm jmp TIDirectSoundBuffer.SetCurrentPosition; end;

procedure XTL_EmuCDirectSoundBuffer_SetDistanceFactor;
asm jmp TIDirectSoundBuffer.SetDistanceFactor; end;

procedure XTL_EmuCDirectSoundBuffer_SetDopplerFactor;
asm jmp TIDirectSoundBuffer.SetDopplerFactor; end;

procedure XTL_EmuCDirectSoundBuffer_SetEG;
asm jmp TIDirectSoundBuffer.SetEG; end;

procedure XTL_EmuCDirectSoundBuffer_SetFilter;
asm jmp TIDirectSoundBuffer.SetFilter; end;

procedure XTL_EmuCDirectSoundBuffer_SetFormat;
asm jmp TIDirectSoundBuffer.SetFormat; end;

procedure XTL_EmuCDirectSoundBuffer_SetFrequency;
asm jmp TIDirectSoundBuffer.SetFrequency; end;

procedure XTL_EmuCDirectSoundBuffer_SetHeadroom;
asm jmp TIDirectSoundBuffer.SetHeadroom; end;

procedure XTL_EmuCDirectSoundBuffer_SetI3DL2Source;
asm jmp TIDirectSoundBuffer.SetI3DL2Source; end;

procedure XTL_EmuCDirectSoundBuffer_SetLFO;
asm jmp TIDirectSoundBuffer.SetLFO; end;

procedure XTL_EmuCDirectSoundBuffer_SetLoopRegion;
asm jmp TIDirectSoundBuffer.SetLoopRegion; end;

procedure XTL_EmuCDirectSoundBuffer_SetMaxDistance;
asm jmp TIDirectSoundBuffer.SetMaxDistance; end;

procedure XTL_EmuCDirectSoundBuffer_SetMinDistance;
asm jmp TIDirectSoundBuffer.SetMinDistance; end;

procedure XTL_EmuCDirectSoundBuffer_SetMixBins;
asm jmp TIDirectSoundBuffer.SetMixBins; end;

procedure XTL_EmuCDirectSoundBuffer_SetMixBinVolumes;
asm jmp TIDirectSoundBuffer.SetMixBinVolumes; end;

procedure XTL_EmuCDirectSoundBuffer_SetMode;
asm jmp TIDirectSoundBuffer.SetMode; end;

procedure XTL_EmuCDirectSoundBuffer_SetNotificationPositions;
asm jmp TIDirectSoundBuffer.SetNotificationPositions; end;

procedure XTL_EmuCDirectSoundBuffer_SetOutputBuffer;
asm jmp TIDirectSoundBuffer.SetOutputBuffer; end;

procedure XTL_EmuCDirectSoundBuffer_SetPitch;
asm jmp TIDirectSoundBuffer.SetPitch; end;

procedure XTL_EmuCDirectSoundBuffer_SetPlayRegion;
asm jmp TIDirectSoundBuffer.SetPlayRegion; end;

procedure XTL_EmuCDirectSoundBuffer_SetPosition;
asm jmp TIDirectSoundBuffer.SetPosition; end;

procedure XTL_EmuCDirectSoundBuffer_SetRolloffCurve;
asm jmp TIDirectSoundBuffer.SetRolloffCurve; end;

procedure XTL_EmuCDirectSoundBuffer_SetRolloffFactor;
asm jmp TIDirectSoundBuffer.SetRolloffFactor; end;

procedure XTL_EmuCDirectSoundBuffer_SetVelocity;
asm jmp TIDirectSoundBuffer.SetVelocity; end;

procedure XTL_EmuCDirectSoundBuffer_SetVolume;
asm jmp TIDirectSoundBuffer.SetVolume; end;

procedure XTL_EmuCDirectSoundBuffer_Stop;
asm jmp TIDirectSoundBuffer.Stop; end;

procedure XTL_EmuCDirectSoundBuffer_StopEx;
asm jmp TIDirectSoundBuffer.StopEx; end;

procedure XTL_EmuCDirectSoundBuffer_Unlock;
asm jmp TIDirectSoundBuffer.Unlock; end;

//
// TIDirectSoundStream - no patches, but a real class implementation as a VMT-replacement for the Xbox version
//

function TIDirectSoundStream.AddRef(): ULONG; stdcall; // virtual
// Branch:shogun  Revision:20100412  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuDSound : TIDirectSoundStream.AddRef' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10');',
      [Self]);

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
    DbgPrintf('EmuDSound : TIDirectSoundStream.Release' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10');',
      [Self]);

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
        DxbxFree(Self.EmuBufferDesc.lpwfxFormat);

      DxbxFree(Self.EmuBufferDesc);

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
    DbgPrintf('EmuDSound : TIDirectSoundStream.GetInfo' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10'   pInfo                     : 0x%.08X' +
      #13#10');',
      [Self, pInfo]);

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
    DbgPrintf('EmuDSound : TIDirectSoundStream.GetStatus' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10'   pdwStatus                 : 0x%.08X' +
      #13#10');',
      [Self, pdwStatus]);

  EmuWarning('EmuCDirectSoundStream_GetStatus is not yet implemented');

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
    DbgPrintf('EmuDSound : TIDirectSoundStream.Process' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10'   pInputBuffer              : 0x%.08X' +
      #13#10'   pOutputBuffer             : 0x%.08X' +
      #13#10');',
      [Self, pInputBuffer, pOutputBuffer]);

  if  (Self <> nil)
  and (Self.EmuDirectSoundBuffer8 <> NULL) then
  begin
    // update buffer data cache
    Self.EmuBuffer := pInputBuffer.pvBuffer;

    Dxbx_TIDirectSoundStream_Resize(Self, pInputBuffer.dwMaxSize);

    if (pInputBuffer.pdwStatus <> nil) then
      pInputBuffer.pdwStatus^ := DS_OK;

    DxbxHackUpdateSoundStreams();
  end
  else
  begin
    if (pInputBuffer.pdwStatus <> nil) then
      pInputBuffer.pdwStatus^ := DS_OK;
  end;

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;

function TIDirectSoundStream.Discontinuity(): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuDSound : TIDirectSoundStream.Discontinuity' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10');',
      [Self]);

  // TODO -oCXBX: Actually Process

  EmuSwapFS(fsXbox);

  Result := DS_OK;
end;


function TIDirectSoundStream.Flush(): HRESULT; stdcall; // virtual;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuDSound : TIDirectSoundStream.Flush' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10');',
      [Self]);

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
    DbgPrintf('EmuDSound : TIDirectSoundStream.SetFormat' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10'   pwfxFormat                : 0x%.08X' +
      #13#10');',
      [Self, pwfxFormat]);

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
// Branch:shogun  Revision:0.8.1-Pre2  PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    DbgPrintf('EmuDSound : TIDirectSoundStream.SetFrequency' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10'   dwFrequency               : %d' +
      #13#10');',
      [Self, dwFrequency]);

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
    DbgPrintf('EmuDSound : TIDirectSoundStream.SetVolume' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10'   lVolume                   : %d' +
      #13#10');',
      [Self, lVolume]);

  if  (Self <> nil)
  and (Self.EmuDirectSoundBuffer8 <> NULL) then
    Result := IDirectSoundBuffer(Self.EmuDirectSoundBuffer8).SetVolume(lVolume)
  else
    Result := DS_OK;

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
    DbgPrintf('EmuDSound : TIDirectSoundStream.SetPitch' +
      #13#10'(' +
      #13#10'   pStream             : 0x%.08X' +
      #13#10'   lPitch              : 0x%.08X' +
      #13#10');',
      [Self, lPitch]);

  Result := DS_OK;

  EmuWarning('IDirectSoundStream_SetPitch not yet implemented!');

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
    DbgPrintf('EmuDSound : TIDirectSoundStream.SetEG' +
      #13#10'(' +
      #13#10'   pStream                 : 0x%.08X' +
      #13#10'   pEnvelopeDesc           : 0x%.08X' +
      #13#10');',
      [Self, pEnvelopeDesc]);

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
    DbgPrintf('EmuDSound : TIDirectSoundStream.SetFilter' +
      #13#10'(' +
      #13#10'   pStream             : 0x%.08X' +
      #13#10'   pFilterDesc         : 0x%.08X' +
      #13#10');',
      [Self, pFilterDesc]);

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
    DbgPrintf('EmuDSound : TIDirectSoundStream.SetHeadroom' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10'   dwHeadroom                : 0x%.08X' +
      #13#10');',
      [Self, dwHeadroom]);

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
    DbgPrintf('EmuDSound : TIDirectSoundStream.SetMixBins' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10'   pMixBins                  : 0x%.08X' +
      #13#10');',
      [Self, pMixBins]);

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
    DbgPrintf('EmuDSound : TIDirectSoundStream.SetAllParameters' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10'   pds3db                    : 0x%.08X' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [Self, pds3db, dwApply]);

  // TODO -oCXBX: Actually implement this
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
    DbgPrintf('EmuDSound : TIDirectSoundStream.SetConeAngles' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10'   dwInsideConeAngle         : 0x%.08X' +
      #13#10'   dwOutsideConeAngle        : 0x%.08X' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [Self, dwInsideConeAngle, dwOutsideConeAngle, dwApply]);

  // TODO -oCXBX: Actually implement this
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
    DbgPrintf('EmuDSound : TIDirectSoundStream.SetConeOrientation' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10'   x                         : %f' +
      #13#10'   y                         : %f' +
      #13#10'   z                         : %f' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [Self, x, y, z, dwApply]);

  // TODO -oCXBX: Actually implement this
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
    DbgPrintf('EmuDSound : TIDirectSoundStream.SetConeOutsideVolume' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10'   lConeOutsideVolume        : %d' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [Self, lConeOutsideVolume, dwApply]);

  // TODO -oCXBX: Actually implement this
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
    DbgPrintf('EmuDSound : TIDirectSoundStream.SetMaxDistance' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10'   flMaxDistance             : %f' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [Self, flMaxDistance, dwApply]);

  // TODO -oCXBX: Actually implement this
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
    DbgPrintf('EmuDSound : TIDirectSoundStream.SetMinDistance' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10'   flMinDistance             : %f' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [Self, flMinDistance, dwApply]);

  // TODO -oCXBX: Actually implement this
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
    DbgPrintf('EmuDSound : TIDirectSoundStream.SetFormat' +
      #13#10'(' +
      #13#10'   pStream             : 0x%.08X' +
      #13#10'   dwMode              : 0x%.08X' +
      #13#10'   dwApply             : 0x%.08X' +
      #13#10');',
      [Self, dwMode, dwApply]);

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
    DbgPrintf('EmuDSound : TIDirectSoundStream.SetPosition' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10'   x                         : %f' +
      #13#10'   y                         : %f' +
      #13#10'   z                         : %f' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [Self, x, y, z, dwApply]);

  // TODO -oCXBX: Actually implement this
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
    DbgPrintf('EmuDSound : TIDirectSoundStream.SetVelocity' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10'   x                         : %f' +
      #13#10'   y                         : %f' +
      #13#10'   z                         : %f' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [Self, x, y, z, dwApply]);

  // TODO -oCXBX: Actually implement this
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
    DbgPrintf('EmuDSound : TIDirectSoundStream.SetRolloffFactor' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10'   flRolloffFactor           : %f' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [Self, flRolloffFactor, dwApply]);

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
    DbgPrintf('EmuDSound : TIDirectSoundStream.SetI3DL2Source' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10'   pds3db                    : 0x%.08X' +
      #13#10'   dwApply                   : 0x%.08X' +
      #13#10');',
      [Self, pds3db, dwApply]);

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
    DbgPrintf('EmuDSound : TIDirectSoundStream.Pause' +
      #13#10'(' +
      #13#10'   pStream                   : 0x%.08X' +
      #13#10'   dwPause                   : 0x%.08X' +
      #13#10');',
      [Self, dwPause]);

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
    DbgPrintf('EmuDSound : TIDirectSoundStream.FlushEx' +
      #13#10'(' +
      #13#10'   pStream                 : 0x%.08X' +
      #13#10'   rtTimeStamp             : 0x%.08X' +
      #13#10'   dwFlags                 : 0x%.08X' +
      #13#10');',
      [Self, rtTimeStamp, dwFlags]);

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
    DbgPrintf('EmuDSound : TIDirectSoundStream.GetVoiceProperties' +
      #13#10'(' +
      #13#10'   pStream                 : 0x%.08X' +
      #13#10'   pVoiceProps             : 0x%.08X' +
      #13#10');',
      [Self, pVoiceProps]);

  // TODO -oCXBX: Actually implement

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

//
// Patches on CDirectSoundStream class functions
//

procedure XTL_EmuCDirectSoundStream_SetFormat;
asm jmp TIDirectSoundStream.SetFormat; end;

procedure XTL_EmuCDirectSoundStream_SetFrequency;
asm jmp TIDirectSoundStream.SetFrequency; end;

procedure XTL_EmuCDirectSoundStream_SetVolume;
asm jmp TIDirectSoundStream.SetVolume; end;

procedure XTL_EmuCDirectSoundStream_SetPitch;
asm jmp TIDirectSoundStream.SetPitch; end;

procedure XTL_EmuCDirectSoundStream_SetLFO;
asm jmp TIDirectSoundStream.SetLFO; end;

procedure XTL_EmuCDirectSoundStream_SetEG;
asm jmp TIDirectSoundStream.SetEG; end;

procedure XTL_EmuCDirectSoundStream_SetFilter;
asm jmp TIDirectSoundStream.SetFilter; end;

procedure XTL_EmuCDirectSoundStream_SetHeadroom;
asm jmp TIDirectSoundStream.SetHeadroom; end;

procedure XTL_EmuCDirectSoundStream_SetOutputBuffer;
asm jmp TIDirectSoundStream.SetOutputBuffer; end;

procedure XTL_EmuCDirectSoundStream_SetMixBins;
asm jmp TIDirectSoundStream.SetMixBins; end;

procedure XTL_EmuCDirectSoundStream_SetMixBinVolumes;
asm jmp TIDirectSoundStream.SetMixBinVolumes; end;

procedure XTL_EmuCDirectSoundStream_SetAllParameters;
asm jmp TIDirectSoundStream.SetAllParameters; end;

procedure XTL_EmuCDirectSoundStream_SetConeAngles;
asm jmp TIDirectSoundStream.SetConeAngles; end;

procedure XTL_EmuCDirectSoundStream_SetConeOrientation;
asm jmp TIDirectSoundStream.SetConeOrientation; end;

procedure XTL_EmuCDirectSoundStream_SetConeOutsideVolume;
asm jmp TIDirectSoundStream.SetConeOutsideVolume; end;

procedure XTL_EmuCDirectSoundStream_SetMaxDistance;
asm jmp TIDirectSoundStream.SetMaxDistance; end;

procedure XTL_EmuCDirectSoundStream_SetMinDistance;
asm jmp TIDirectSoundStream.SetMinDistance; end;

procedure XTL_EmuCDirectSoundStream_SetMode;
asm jmp TIDirectSoundStream.SetMode; end;

procedure XTL_EmuCDirectSoundStream_SetPosition;
asm jmp TIDirectSoundStream.SetPosition; end;

procedure XTL_EmuCDirectSoundStream_SetVelocity;
asm jmp TIDirectSoundStream.SetVelocity; end;

procedure XTL_EmuCDirectSoundStream_SetDistanceFactor;
asm jmp TIDirectSoundStream.SetDistanceFactor; end;

procedure XTL_EmuCDirectSoundStream_SetDopplerFactor;
asm jmp TIDirectSoundStream.SetDopplerFactor; end;

procedure XTL_EmuCDirectSoundStream_SetRolloffFactor;
asm jmp TIDirectSoundStream.SetRolloffFactor; end;

procedure XTL_EmuCDirectSoundStream_SetRolloffCurve;
asm jmp TIDirectSoundStream.SetRolloffCurve; end;

procedure XTL_EmuCDirectSoundStream_SetI3DL2Source;
asm jmp TIDirectSoundStream.SetI3DL2Source; end;

procedure XTL_EmuCDirectSoundStream_Pause;
asm jmp TIDirectSoundStream.Pause; end;

procedure XTL_EmuCDirectSoundStream_PauseEx;
asm jmp TIDirectSoundStream.PauseEx; end;

procedure XTL_EmuCDirectSoundStream_FlushEx;
asm jmp TIDirectSoundStream.FlushEx; end;

procedure XTL_EmuCDirectSoundStream_GetVoiceProperties;
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
    DbgPrintf('EmuDSound : XAudioCreateAdpcmFormat' +
      #13#10'(' +
      #13#10'   nChannels                 : 0x%.04X' +
      #13#10'   nSamplesPerSec            : 0x%.08X' +
      #13#10'   pwfx                      : 0x%.08X' +
      #13#10');',
      [nChannels, nSamplesPerSec, pwfx]);

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
    DbgPrintf('EmuDSound : XAudioDownloadEffectsImage' +
      #13#10'(' +
      #13#10'   pszImageName        : 0x%.08X ("%s")' +
      #13#10'   pImageLoc           : 0x%.08X' +
      #13#10'   dwFlags             : 0x%.08X' +
      #13#10'   ppImageDesc         : 0x%.08X' +
      #13#10');',
      [UIntPtr(pszImageName), PAnsiChar(pszImageName), pImageLoc, dwFlags, ppImageDesc]);

   EmuSwapFS(fsXbox);

   Result := DS_OK;
end;

{.$MESSAGE 'PatrickvL reviewed up to here'}

exports // Keep this list sorted, with newlines between patch groups :

  XTL_EmuCDirectSound_AddRef name PatchPrefix + 'DirectSound.CDirectSound.AddRef',
  XTL_EmuCDirectSound_CommitDeferredSettings name PatchPrefix + 'DirectSound.CDirectSound.CommitDeferredSettings@4',
  XTL_EmuCDirectSound_CommitEffectData name PatchPrefix + 'DirectSound.CDirectSound.CommitEffectData',
  XTL_EmuCDirectSound_Compact name PatchPrefix + 'DirectSound.CDirectSound.Compact',
  XTL_EmuCDirectSound_CreateSoundBuffer name PatchPrefix + 'DirectSound.CDirectSound.CreateSoundBuffer',
  XTL_EmuCDirectSound_CreateSoundStream name PatchPrefix + 'DirectSound.CDirectSound.CreateSoundStream',
  XTL_EmuCDirectSound_DownloadEffectsImage name PatchPrefix + 'DirectSound.CDirectSound.DownloadEffectsImage',
  XTL_EmuCDirectSound_EnableHeadphones name PatchPrefix + 'DirectSound.CDirectSound.EnableHeadphones',
  XTL_EmuCDirectSound_GetCaps name PatchPrefix + 'DirectSound.CDirectSound.GetCaps',
  XTL_EmuCDirectSound_GetEffectData name PatchPrefix + 'DirectSound.CDirectSound.GetEffectData',
  XTL_EmuCDirectSound_GetOutputLevels name PatchPrefix + 'DirectSound.CDirectSound.GetOutputLevels',
  XTL_EmuCDirectSound_GetSpeakerConfig name PatchPrefix + 'DirectSound.CDirectSound.GetSpeakerConfig',
  XTL_EmuCDirectSound_GetTime name PatchPrefix + 'DirectSound.CDirectSound.GetTime',
  //XTL_EmuCDirectSound_QueryInterface name PatchPrefix + 'DirectSound.CDirectSound.QueryInterface',
  //XTL_EmuCDirectSound_QueryInterfaceC name PatchPrefix + 'DirectSound.CDirectSound.QueryInterfaceC',
  XTL_EmuCDirectSound_Release name PatchPrefix + '_IDirectSound_Release@4', // Was 'IDirectSound_Release'
  XTL_EmuCDirectSound_SetAllParameters name PatchPrefix + 'DirectSound.CDirectSound.SetAllParameters',
  XTL_EmuCDirectSound_SetCooperativeLevel name PatchPrefix + 'DirectSound.CDirectSound.SetCooperativeLevel',
  XTL_EmuCDirectSound_SetDistanceFactor name PatchPrefix + 'DirectSound.CDirectSound.SetDistanceFactor',
  XTL_EmuCDirectSound_SetDopplerFactor name PatchPrefix + 'DirectSound.CDirectSound.SetDopplerFactor',
  XTL_EmuCDirectSound_SetEffectData name PatchPrefix + 'DirectSound.CDirectSound.SetEffectData',
  XTL_EmuCDirectSound_SetI3DL2Listener name PatchPrefix + 'DirectSound.CDirectSound.SetI3DL2Listener',
  XTL_EmuCDirectSound_SetMixBinHeadroom name PatchPrefix + 'DirectSound.CDirectSound.SetMixBinHeadroom',
  XTL_EmuCDirectSound_SetOrientation name PatchPrefix + 'DirectSound.CDirectSound.SetOrientation',
  XTL_EmuCDirectSound_SetPosition name PatchPrefix + 'DirectSound.CDirectSound.SetPosition',
  XTL_EmuCDirectSound_SetRolloffFactor name PatchPrefix + 'DirectSound.CDirectSound.SetRolloffFactor',
  XTL_EmuCDirectSound_SetVelocity name PatchPrefix + 'DirectSound.CDirectSound.SetVelocity',
  XTL_EmuCDirectSound_SynchPlayback name PatchPrefix + 'DirectSound.CDirectSound.SynchPlayback',

  XTL_EmuCDirectSoundBuffer_GetCurrentPosition name PatchPrefix + 'DirectSound.CDirectSoundBuffer.GetCurrentPosition',
  XTL_EmuCDirectSoundBuffer_GetStatus name PatchPrefix + 'DirectSound.CDirectSoundBuffer.GetStatus',
  XTL_EmuCDirectSoundBuffer_GetVoiceProperties name PatchPrefix + 'DirectSound.CDirectSoundBuffer.GetVoiceProperties',
  XTL_EmuCDirectSoundBuffer_Lock name PatchPrefix + 'DirectSound.CDirectSoundBuffer.Lock',
  XTL_EmuCDirectSoundBuffer_Pause name PatchPrefix + 'DirectSound.CDirectSoundBuffer.Pause',
  //XTL_EmuCDirectSoundBuffer_PauseEx name PatchPrefix + 'DirectSound.CDirectSoundBuffer.PauseEx',
  XTL_EmuCDirectSoundBuffer_Play name PatchPrefix + 'DirectSound.CDirectSoundBuffer.Play',
  XTL_EmuCDirectSoundBuffer_PlayEx name PatchPrefix + 'DirectSound.CDirectSoundBuffer.PlayEx',
  XTL_EmuCDirectSoundBuffer_Restore name PatchPrefix + 'DirectSound.CDirectSoundBuffer.Restore',
  XTL_EmuCDirectSoundBuffer_SetAllParameters name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetAllParameters',
  XTL_EmuCDirectSoundBuffer_SetBufferData name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetBufferData',
  XTL_EmuCDirectSoundBuffer_SetConeAngles name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetConeAngles',
  XTL_EmuCDirectSoundBuffer_SetConeOrientation name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetConeOrientation',
  XTL_EmuCDirectSoundBuffer_SetConeOutsideVolume name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetConeOutsideVolume',
  XTL_EmuCDirectSoundBuffer_SetCurrentPosition name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetCurrentPosition',
  XTL_EmuCDirectSoundBuffer_SetDistanceFactor name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetDistanceFactor',
  XTL_EmuCDirectSoundBuffer_SetDopplerFactor name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetDopplerFactor',
  XTL_EmuCDirectSoundBuffer_SetEG name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetEG',
  XTL_EmuCDirectSoundBuffer_SetFilter name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetFilter',
  XTL_EmuCDirectSoundBuffer_SetFormat name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetFormat',
  XTL_EmuCDirectSoundBuffer_SetFrequency name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetFrequency',
  XTL_EmuCDirectSoundBuffer_SetHeadroom name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetHeadroom',
  XTL_EmuCDirectSoundBuffer_SetI3DL2Source name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetI3DL2Source',
  XTL_EmuCDirectSoundBuffer_SetLFO name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetLFO',
  XTL_EmuCDirectSoundBuffer_SetLoopRegion name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetLoopRegion',
  XTL_EmuCDirectSoundBuffer_SetMaxDistance name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetMaxDistance',
  XTL_EmuCDirectSoundBuffer_SetMinDistance name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetMinDistance',
  XTL_EmuCDirectSoundBuffer_SetMixBins name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetMixBins',
  XTL_EmuCDirectSoundBuffer_SetMixBinVolumes name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetMixBinVolumes',
  XTL_EmuCDirectSoundBuffer_SetMode name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetMode',
//  XTL_EmuCDirectSoundBuffer_SetNotificationPositions name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetNotificationPositions',
  XTL_EmuCDirectSoundBuffer_SetOutputBuffer name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetOutputBuffer',
  XTL_EmuCDirectSoundBuffer_SetPitch name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetPitch',
  XTL_EmuCDirectSoundBuffer_SetPlayRegion name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetPlayRegion',
  XTL_EmuCDirectSoundBuffer_SetPosition name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetPosition',
  XTL_EmuCDirectSoundBuffer_SetRolloffCurve name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetRolloffCurve',
  XTL_EmuCDirectSoundBuffer_SetRolloffFactor name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetRolloffFactor',
  XTL_EmuCDirectSoundBuffer_SetVelocity name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetVelocity',
  XTL_EmuCDirectSoundBuffer_SetVolume name PatchPrefix + 'DirectSound.CDirectSoundBuffer.SetVolume',
  XTL_EmuCDirectSoundBuffer_Stop name PatchPrefix + 'DirectSound.CDirectSoundBuffer.Stop',
  XTL_EmuCDirectSoundBuffer_StopEx name PatchPrefix + 'DirectSound.CDirectSoundBuffer.StopEx',
  XTL_EmuCDirectSoundBuffer_Unlock name PatchPrefix + 'DirectSound.CDirectSoundBuffer.Unlock',

//  XTL_EmuCDirectSoundStream_AddRef name PatchPrefix + 'DirectSound.CDirectSoundStream.AddRef',
//  XTL_EmuCDirectSoundStream_Discontinuity name PatchPrefix + 'DirectSound.CDirectSoundStream.Discontinuity',
//  XTL_EmuCDirectSoundStream_Flush name PatchPrefix + 'DirectSound.CDirectSoundStream.Flush',
  XTL_EmuCDirectSoundStream_FlushEx name PatchPrefix + 'DirectSound.CDirectSoundStream.FlushEx',
//  XTL_EmuCDirectSoundStream_GetInfo name PatchPrefix + 'DirectSound.CDirectSoundStream.GetInfo',
//  XTL_EmuCDirectSoundStream_GetStatus name PatchPrefix + 'DirectSound.CDirectSoundStream.GetStatus',
  XTL_EmuCDirectSoundStream_GetVoiceProperties name PatchPrefix + 'DirectSound.CDirectSoundStream.GetVoiceProperties',
  XTL_EmuCDirectSoundStream_Pause name PatchPrefix + 'DirectSound.CDirectSoundStream.Pause',
  XTL_EmuCDirectSoundStream_PauseEx name PatchPrefix + 'DirectSound.CDirectSoundStream.PauseEx',
//  XTL_EmuCDirectSoundStream_Process name PatchPrefix + 'DirectSound.CDirectSoundStream.Process',
//  XTL_EmuCDirectSoundStream_Release name PatchPrefix + 'DirectSound.CDirectSoundStream.Release',
  XTL_EmuCDirectSoundStream_SetAllParameters name PatchPrefix + 'DirectSound.CDirectSoundStream.SetAllParameters',
  XTL_EmuCDirectSoundStream_SetConeAngles name PatchPrefix + 'DirectSound.CDirectSoundStream.SetConeAngles',
  XTL_EmuCDirectSoundStream_SetConeOrientation name PatchPrefix + 'DirectSound.CDirectSoundStream.SetConeOrientation',
  XTL_EmuCDirectSoundStream_SetConeOutsideVolume name PatchPrefix + 'DirectSound.CDirectSoundStream.SetConeOutsideVolume',
  XTL_EmuCDirectSoundStream_SetDistanceFactor name PatchPrefix + 'DirectSound.CDirectSoundStream.SetDistanceFactor',
  XTL_EmuCDirectSoundStream_SetDopplerFactor name PatchPrefix + 'DirectSound.CDirectSoundStream.SetDopplerFactor',
  XTL_EmuCDirectSoundStream_SetEG name PatchPrefix + 'DirectSound.CDirectSoundStream.SetEG',
  XTL_EmuCDirectSoundStream_SetFilter name PatchPrefix + 'DirectSound.CDirectSoundStream.SetFilter',
  XTL_EmuCDirectSoundStream_SetFormat name PatchPrefix + 'DirectSound.CDirectSoundStream.SetFormat',
  XTL_EmuCDirectSoundStream_SetFrequency name PatchPrefix + 'DirectSound.CDirectSoundStream.SetFrequency',
  XTL_EmuCDirectSoundStream_SetHeadroom name PatchPrefix + 'DirectSound.CDirectSoundStream.SetHeadroom',
  XTL_EmuCDirectSoundStream_SetI3DL2Source name PatchPrefix + 'DirectSound.CDirectSoundStream.SetI3DL2Source',
  XTL_EmuCDirectSoundStream_SetLFO name PatchPrefix + 'DirectSound.CDirectSoundStream.SetLFO',
  XTL_EmuCDirectSoundStream_SetMaxDistance name PatchPrefix + 'DirectSound.CDirectSoundStream.SetMaxDistance',
  XTL_EmuCDirectSoundStream_SetMinDistance name PatchPrefix + 'DirectSound.CDirectSoundStream.SetMinDistance',
  XTL_EmuCDirectSoundStream_SetMixBins name PatchPrefix + 'DirectSound.CDirectSoundStream.SetMixBins',
  XTL_EmuCDirectSoundStream_SetMixBinVolumes name PatchPrefix + 'DirectSound.CDirectSoundStream.SetMixBinVolumes',
  XTL_EmuCDirectSoundStream_SetMode name PatchPrefix + 'DirectSound.CDirectSoundStream.SetMode',
  XTL_EmuCDirectSoundStream_SetOutputBuffer name PatchPrefix + 'DirectSound.CDirectSoundStream.SetOutputBuffer',
  XTL_EmuCDirectSoundStream_SetPitch name PatchPrefix + 'DirectSound.CDirectSoundStream.SetPitch',
  XTL_EmuCDirectSoundStream_SetPosition name PatchPrefix + 'DirectSound.CDirectSoundStream.SetPosition',
  XTL_EmuCDirectSoundStream_SetRolloffCurve name PatchPrefix + 'DirectSound.CDirectSoundStream.SetRolloffCurve',
  XTL_EmuCDirectSoundStream_SetRolloffFactor name PatchPrefix + 'DirectSound.CDirectSoundStream.SetRolloffFactor',
  XTL_EmuCDirectSoundStream_SetVelocity name PatchPrefix + 'DirectSound.CDirectSoundStream.SetVelocity',
  XTL_EmuCDirectSoundStream_SetVolume name PatchPrefix + 'DirectSound.CDirectSoundStream.SetVolume',

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
  XTL_EmuIDirectSound_Release name PatchPrefix + '_IDirectSound_Release@4', // Was 'IDirectSound_Release'
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
  //XTL_EmuIDirectSoundBuffer_PauseEx,
  XTL_EmuIDirectSoundBuffer_Play,
  XTL_EmuIDirectSoundBuffer_PlayEx,
  XTL_EmuIDirectSoundBuffer_QueryInterface,
  XTL_EmuIDirectSoundBuffer_QueryInterfaceC,
  XTL_EmuIDirectSoundBuffer_Release name PatchPrefix + '_IDirectSoundBuffer_Release@4',
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
//  XTL_EmuIDirectSoundBuffer_SetNotificationPositions,
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

