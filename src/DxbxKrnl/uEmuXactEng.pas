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
unit uEmuXactEng;

{$INCLUDE Dxbx.inc}

interface

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

implementation

const lfUnit = lfCxbx or lfSound;

type
  _X_XACTEngine = record
    // Fill this in ?
  end;
  X_XACTEngine = _X_XACTEngine;
  PX_XACTEngine = ^X_XACTEngine;
  PPX_XACTEngine = ^PX_XACTEngine;

  _X_XACTWaveBank = record
    // fill this in ?
  end;
  X_XACTWaveBank = _X_XACTWaveBank;
  PX_XACTWaveBank = ^X_XACTWaveBank;
  PPX_XACTWaveBank = ^PX_XACTWaveBank;

  _X_XACTSoundBank = record
    // fill this in ?
  end;
  X_XACTSoundBank = _X_XACTSoundBank;
  PX_XACTSoundBank = ^X_XACTSoundBank;
  PPX_XACTSoundBank = ^PX_XACTSoundBank;

  _X_XACTSoundCue = record
    // fill this in ?
  end;
  X_XACTSoundCue = _X_XACTSoundCue;
  PX_XACTSoundCue = ^X_XACTSoundCue;
  PPX_XACTSoundCue = ^PX_XACTSoundCue;

  _X_XACTSoundSource = record
    // fill this in ?
  end;
  X_XACTSoundSource = _X_XACTSoundSource;
  PX_XACTSoundSource = ^X_XACTSoundSource;
  PPX_XACTSoundSource = ^PX_XACTSoundSource;

  _X_XACT_RUNTIME_PARAMETERS = record
    dwMax2DHwVoices: DWORD;
    dwMax3DHwVoices: DWORD;
    dwMaxConcurrentStreams: DWORD;
    dwMaxNotifications: DWORD;
    dwInteractiveAudioLookaheadTime: DWORD; // Time in ms
  end;
  X_XACT_RUNTIME_PARAMETERS = _X_XACT_RUNTIME_PARAMETERS;
  PX_XACT_RUNTIME_PARAMETERS = ^X_XACT_RUNTIME_PARAMETERS;


  _X_XACT_WAVEBANK_STREAMING_PARAMETERS = record
    hFile: HANDLE;                          // file handle associated with wavebank data
    dwOffset: DWORD;                       // offset within file of wavebank header
    dwPacketSizeInMilliSecs: DWORD;        // stream packet size to use for each stream (in ms)
  end;
  X_XACT_WAVEBANK_STREAMING_PARAMETERS = _X_XACT_WAVEBANK_STREAMING_PARAMETERS;
  PX_XACT_WAVEBANK_STREAMING_PARAMETERS = ^X_XACT_WAVEBANK_STREAMING_PARAMETERS;
  PPX_XACT_WAVEBANK_STREAMING_PARAMETERS = ^PX_XACT_WAVEBANK_STREAMING_PARAMETERS;


  _X_XACT_NOTIFICATION_DESCRIPTION = record
    dwType: DWORD;
    pSoundBank: PX_XACTSOUNDBANK;
    pSoundCue: PX_XACTSOUNDCUE;
    dwSoundCueIndex: DWORD;
    pvContext: PVOID;
    hEvent: HANDLE;
  end;
  X_XACT_NOTIFICATION_DESCRIPTION = _X_XACT_NOTIFICATION_DESCRIPTION;
  PX_XACT_NOTIFICATION_DESCRIPTION = ^X_XACT_NOTIFICATION_DESCRIPTION;

function XTL_EmuIXACTEngineCreate
(
  pParams: PX_XACT_RUNTIME_PARAMETERS;
  ppEngine: PPX_XACTEngine
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('XTL_EmuIXACTEngineCreate').
      _(pParams, 'pParams').
      _(ppEngine, 'ppEngine').
    LogEnd();


  // TODO -oCxbx: Any other form of initialization?

  ppEngine^ := PX_XACTEngine(XboxAlloc( sizeof( X_XACTEngine ) ) );

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

procedure XTL_EmuIXACTEngineDoWork(); stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfunit) then
    DbgPrintf('EmuXACTEngineDoWork()');

  // TODO -oCxbx: Anything else required here?
  // AFAIK, this function just calls DirectSoundDoWork()

  //EmuSwapFS(fsXbox);
  //EmuDirectSoundDoWork();
  //EmuSwapFS(fsWindows);

  EmuSwapFS(fsXbox);
end;

function XTL_EmuIXACTEngine_RegisterWaveBank
(
  pThis: PX_XACTEngine;
  pvData: LPVOID;
  dwSize: DWORD;
  ppWaveBank: PPX_XACTWaveBank
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIXACTEngine_RegisterWaveBank').
      _(pThis, 'pThis').
      _(pvData, 'pvData').
      _(dwSize, 'dwSize').
      _(ppWaveBank, 'ppWaveBank').
    LogEnd();

  // TODO -oCxbx: Implement

  ppWaveBank^ := PX_XACTWaveBank( XboxAlloc( sizeof( X_XACTWaveBank ) ) );

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIXACTEngine_RegisterStreamedWaveBank
(
  pThis: PX_XACTEngine;
  pParams: PX_XACT_WAVEBANK_STREAMING_PARAMETERS;
  ppWaveBank: PPX_XACTWaveBank
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIXACTEngine_RegisterStreamedWaveBank').
      _(pThis, 'pThis').
      _(pParams, 'pParams').
      _(ppWaveBank, 'ppWaveBank').
    LogEnd();

  // TODO -oCxbx: Implement

  ppWaveBank^ := PX_XACTWaveBank(XboxAlloc( sizeof( X_XACTWaveBank ) ) );

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIXACTEngine_CreateSoundBank
(
  pThis: PX_XACTEngine;
  pvData: LPVOID;
  dwSize: DWORD;
  ppSoundBank: PPX_XACTSoundBank
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIXACTEngine_CreateSoundBank').
      _(pThis, 'pThis').
      _(pvData, 'pvData').
      _(dwSize, 'dwSize').
      _(ppSoundBank, 'ppSoundBank').
    LogEnd();

  // TODO -oCxbx: Implement

  ppSoundBank^ := PX_XACTSoundBank( XboxAlloc( sizeof( X_XACTSoundBank ) ) );

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIXACTEngine_DownloadEffectsImage
(
  pThis: PX_XACTEngine;
  pvData: PVOID;
  dwSize: DWORD;
  pEffectLoc: LPVOID;
  ppImageDesc: PLPVOID
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfunit) then
    LogBegin('EmuIXACTEngine_DownloadEffectsImage').
      _(pThis, 'pThis').
      _(dwSize, 'dwSize').
      _(pEffectLoc, 'pEffectLoc').
      _(ppImageDesc, 'ppImageDesc').
    LogEnd();

  // TODO -oCxbx: Implement

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIXACTEngine_CreateSoundSource
(
  pThis: PX_XACTEngine;
  dwFlags: DWORD;
  ppSoundSource: PPX_XACTSoundSource
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('XTL_EmuIXACTEngine_CreateSoundSource').
      _(pThis, 'pThis').
      _(dwFlags, 'dwFlags').
      _(ppSoundSource, 'ppSoundSource').
    LogEnd();

  ppSoundSource^ := PX_XACTSoundSource( XboxAlloc( sizeof( X_XACTSoundSource ) ) );
  // TODO : Where's the destructor for this?

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIXACTEngine_EnableHeadphones
(
  pThis: PX_XACTEngine;
  fEnabled: BOOL
): HRESULT;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIXACTEngine_EnableHeadphones').
      _(pThis, 'pThis').
      _(fEnabled, 'fEnabled').
    LogEnd();

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIXACTEngine_SetListenerOrientation
(
  pThis: PX_XACTEngine;
  xFront: float;
  yFront: float;
  zFront: float;
  xTop: float;
  yTop: float;
  zTop: float;
  dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);  // Win2k/XP FS

  if MayLog(lfUnit) then
    LogBegin('EmuIXACTEngine_SetListenerOrientation').
      _(pThis, 'pThis').
      _(xFront, 'xFront').
      _(yFront, 'yFront').
      _(zFront, 'zFront').
      _(xTop, 'xTop').
      _(yTop, 'yTop').
      _(zTop, 'zTop').
      _(dwApply, 'dwApply').
    LogEnd();

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIXACTEngine_SetListenerPosition
(
  pThis: PX_XACTEngine;
  x: float;
  y: float;
  z: float;
  dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIXACTEngine_SetListenerPosition').
      _(pThis, 'pThis').
      _(x, 'x').
      _(y, 'y').
      _(z, 'z').
      _(dwApply, 'dwApply').
    LogEnd();

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIXACTEngine_SetListenerVelocity
(
  pThis: PX_XACTEngine;
  x: float;
  y: float;
  z: float;
  dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIXACTEngine_SetListenerVelocity').
      _(pThis, 'pThis').
      _(x, 'x').
      _(y, 'y').
      _(z, 'z').
      _(dwApply, 'dwApply').
    LogEnd();

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIXACTEngine_SetMasterVolume
(
  pThis: PX_XACTEngine;
  wCategory: WORD;
  lVolume: LONG
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIXACTEngine_SetMasterVolume').
      _(pThis, 'pThis').
      _(wCategory, 'wCategory').
      _(lVolume, 'lVolume').
    LogEnd();

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIXACTEngine_CommitDeferredSettings
(
  pThis: PX_XACTEngine
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if Maylog(lfUnit) then
    LogBegin('EmuIXACTEngine_CommitDeferredSettings').
      _(pThis, 'pThis').
    LogEnd();

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIXACTSoundBank_GetSoundCueIndexFromFriendlyName
(
  pThis: PX_XACTEngine;
  pFriendlyName: PCSTR;
  pdwSoundCueIndex: PDWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIXACTSoundBank_GetSoundCueIndexFromFriendlyName').
      _(pThis, 'pThis').
      _(pFriendlyName, 'pFriendlyName').
      _(pdwSoundCueIndex, 'pdwSoundCueIndex').
    LogEnd();

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIXACTSoundBank_Play
(
  pThis: PX_XACTSoundBank;
  dwSoundCueIndex: DWORD;
  pSoundSource: PX_XACTSoundSource;
  dwFlags: DWORD;
  ppSoundCue: PPX_XACTSoundCue
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIXACTSoundBank_Play').
      _(pThis, 'pThis').
      _(dwSoundCueIndex, 'dwSoundCueIndex').
      _(pSoundSource, 'pSoundSource').
      _(dwFlags, 'dwFlags').
      _(ppSoundCue, 'ppSoundCue').
    LogEnd();

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIXACTSoundBank_Stop
(
  pThis: PX_XACTSoundBank;
  dwSoundCueIndex: DWORD;
  dwFlags: DWORD;
  pSoundCue: PX_XACTSoundCue
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);  // Win2k/XP FS

  if maylog(lfUnit) then
    LogBegin('EmuIXACTSoundBank_Stop').
      _(pThis, 'pThis').
      _(dwSoundCueIndex, 'dwSoundCueIndex').
      _(dwFlags, 'dwFlags').
      _(pSoundCue, 'pSoundCue').
    LogEnd();

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIXACTSoundSource_SetPosition
(
  pThis: PX_XACTSoundSource;
  x: FLOAT;
  y: FLOAT;
  z: FLOAT;
  dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIXACTSoundSource_SetPosition').
      _(pThis, 'pThis').
      _(x, 'x').
      _(y, 'y').
      _(z, 'z').
      _(dwApply, 'dwApply').
    LogEnd();

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIXACTSoundSource_SetVelocity
(
  pThis: PX_XACTSoundSource;
  x: FLOAT;
  y: FLOAT;
  z: FLOAT;
  dwApply: DWORD
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIXACTSoundSource_SetVelocity').
      _(pThis, 'pThis').
      _(x, 'x').
      _(y, 'y').
      _(z, 'z').
      _(dwApply, 'dwApply').
    LogEnd();

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIXACTEngine_RegisterNotification
(
  pThis: PX_XACTEngine;
  pNotificationDesc: PX_XACT_NOTIFICATION_DESCRIPTION
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIXACTEngine_RegisterNotification').
      _(pThis, 'pThis').
      _(pNotificationDesc, 'pNotificationDesc').
    LogEnd();

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIXACTEngine_GetNotification
(
  pThis: PX_XACTEngine;
  pNotificationDesc: PX_XACT_NOTIFICATION_DESCRIPTION;
  pNotification: LPVOID
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:PatrickvL  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('XTL_EmuIXACTEngine_GetNotification').
      _(pThis, 'pThis').
      _(pNotificationDesc, 'pNotificationDesc').
      _(pNotification, 'pNotification').
    LogEnd();

  // TODO -oCxbx: The contents of XACT_NOTIFICATION can vary from one XDK to the next.
  // The definition for 4627 is different than 5558.

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

function XTL_EmuIXACTEngine_UnRegisterWaveBank
(
  pThis: PX_XACTEngine;
  pWaveBank: PX_XACTWaveBank
): HRESULT; stdcall;
// Branch:shogun  Revision:0.8.1-Pre2  Translator:Shadow_Tj  Done:100
begin
  EmuSwapFS(fsWindows);

  if MayLog(lfUnit) then
    LogBegin('EmuIXACTEngine_UnRegisterWaveBank').
      _(pThis, 'pThis').
      _(pWaveBank, 'pWaveBank').
    LogEnd();

  // Even though the documentation doesn't tell us much, I'm
  // assuming that after this function is called, the pointer
  // to IXACTWaveBank is released.

//  if(pWaveBank)
//    XboxFree(pWaveBank);

  EmuSwapFS(fsXbox);

  Result := S_OK;
end;

exports // Keep this list sorted, with newlines between patch groups :

  XTL_EmuIXACTEngine_CommitDeferredSettings,
  XTL_EmuIXACTEngine_CreateSoundBank,
  XTL_EmuIXACTEngine_CreateSoundSource,
  XTL_EmuIXACTEngine_DownloadEffectsImage,
  XTL_EmuIXACTEngine_EnableHeadphones,
  XTL_EmuIXACTEngine_GetNotification,
  XTL_EmuIXACTEngine_RegisterNotification,
  XTL_EmuIXACTEngine_RegisterStreamedWaveBank,
  XTL_EmuIXACTEngine_RegisterWaveBank,
  XTL_EmuIXACTEngine_SetListenerOrientation,
  XTL_EmuIXACTEngine_SetListenerPosition,
  XTL_EmuIXACTEngine_SetListenerVelocity,
  XTL_EmuIXACTEngine_SetMasterVolume,
  XTL_EmuIXACTEngine_UnRegisterWaveBank,

  XTL_EmuIXACTEngineCreate,
  XTL_EmuIXACTEngineDoWork,

  XTL_EmuIXACTSoundBank_GetSoundCueIndexFromFriendlyName,
  XTL_EmuIXACTSoundBank_Play,
  XTL_EmuIXACTSoundBank_Stop,

  XTL_EmuIXACTSoundSource_SetPosition,
  XTL_EmuIXACTSoundSource_SetVelocity;

end.
