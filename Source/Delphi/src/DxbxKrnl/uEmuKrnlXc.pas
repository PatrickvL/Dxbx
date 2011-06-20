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
unit uEmuKrnlXc;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  SysUtils,
  // Jedi Win32API
  JwaWinType,
  JwaWinBase,
  JwaWinNT,
  JwaNative,
  JwaNTStatus,
  // OpenXDK
  XboxKrnl,
  // Dxbx
  uTypes,
  uLog,
  uEmuFS,
  uEmuKrnl,
  uDxbxKrnl;

procedure xboxkrnl_XcSHAInit(
  pbSHAContext: PUCHAR
  ); stdcall;
procedure xboxkrnl_XcSHAUpdate(
  pbSHAContext: PUCHAR;
  pbInput: PUCHAR;
  dwInputLength: ULONG
  ); stdcall;
procedure xboxkrnl_XcSHAFinal(
  pbSHAContext: PUCHAR;
  pbDigest: PUCHAR
  ); stdcall;
procedure xboxkrnl_XcRC4Key(
    pbKeyStruct: PUCHAR;
    dwKeyLength: ULONG;
    pbKey: PUCHAR
); stdcall;

procedure xboxkrnl_XcRC4Crypt(
    pbKeyStruct: PUCHAR;
    dwInputLength: ULONG;
    pbInput: PUCHAR
); stdcall;

procedure xboxkrnl_XcHMAC(
    pbKeyMaterial: PBYTE;
    cbKeyMaterial: ULONG;
    pbData: PBYTE;
    cbData: ULONG;
    pbData2: PBYTE;
    {OUT}cbData2: ULONG;
    {OUT}HmacData: PBYTE  // length must be A_SHA_DIGEST_LEN
); stdcall;

function xboxkrnl_XcPKEncPublic(
    pbPubKey: PUCHAR;
    pbInput: PUCHAR;
    {OUT}pbOutput:  PUCHAR
): ULONG; stdcall;

function xboxkrnl_XcPKDecPrivate(
    pbPrvKey: PUCHAR;
    pbInput: PUCHAR;
    {OUT}pbOutput: PUCHAR
): ULONG; stdcall;

function xboxkrnl_XcPKGetKeyLen(
    pbPubKey: PUCHAR
): ULONG; stdcall;

function xboxkrnl_XcVerifyPKCS1Signature(
    pbSig: PUCHAR;
    pbPubKey: PUCHAR;
    pbDigest: PUCHAR
): BOOL; stdcall;

function xboxkrnl_XcModExp(
    {OUT} pA: PULONG;
    pB: PULONG;
    pC: PULONG;
    pD: PULONG;
    dwN: ULONG
): ULONG; stdcall;

procedure xboxkrnl_XcDESKeyParity(
    {OUT}pbKey: PUCHAR;
    dwKeyLength: ULONG
); stdcall;

procedure xboxkrnl_XcKeyTable(
    dwCipher: ULONG;
    {OUT}pbKeyTable: PUCHAR;
    pbKey: PUCHAR
); stdcall;

procedure xboxkrnl_XcBlockCrypt(
    dwCipher: ULONG;
    {OUT}pbOutput: PUCHAR;
    pbInput: PUCHAR;
    pbKeyTable: PUCHAR;
    dwOp: ULONG
); stdcall;

procedure xboxkrnl_XcBlockCryptCBC(
	dwCipher: ULONG;
	dwInputLength: ULONG;
	{OUT}pbOutput: PUCHAR;
	pbInput: PUCHAR;
	pbKeyTable: PUCHAR;
	dwOp: ULONG;
	pbFeedback: PUCHAR
); stdcall;

function xboxkrnl_XcCryptService(
    dwOp: ULONG;
    pArgs: Pvoid
): ULONG; stdcall;

procedure xboxkrnl_XcUpdateCrypto(
    (*pNewVector: PCRYPTO_VECTOR;
    {OUT}pROMVector: OPTIONAL PCRYPTO_VECTOR *)
); stdcall;

implementation

procedure xboxkrnl_XcSHAInit(
  pbSHAContext: PUCHAR
  ); stdcall;
// Source:shogun  Branch:163  Translator:JoaoHadouken  Done:0
begin
  EmuSwapFS(fsWindows);
  LogBegin('EmuKrnl : XcSHAInit').
    _(pbSHAContext, 'pbSHAContext').
  LogEnd();

  Unimplemented('XcSHAInit');

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_XcSHAUpdate(
  pbSHAContext: PUCHAR;
  pbInput: PUCHAR;
  dwInputLength: ULONG
  ); stdcall;
// Source:shogun  Branch:163  Translator:JoaoHadouken  Done:0
begin
  EmuSwapFS(fsWindows);
  LogBegin('EmuKrnl : XcSHAUpdate').
    _(pbSHAContext, 'pbSHAContext').
    _(pbInput, 'pbInput').
    _(dwInputLength, 'dwInputLength').
  LogEnd();

  Unimplemented('XcSHAUpdate');

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_XcSHAFinal(
  pbSHAContext: PUCHAR;
  pbDigest: PUCHAR
  ); stdcall;
// Source:shogun  Branch:163  Translator:JoaoHadouken  Done:100
var
  v: int;
begin
  EmuSwapFS(fsWindows);
  LogBegin('EmuKrnl : XcSHAFinal').
    _(pbSHAContext, 'pbSHAContext').
    _(pbDigest, 'pbDigest').
  LogEnd();

  if Assigned(pbDigest) then
    // for now, we dont care about the digest
    for v:=0 to 20-1 do
    begin
      PBytes(pbDigest)[v] := 0;
    end;

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_XcRC4Key(
    pbKeyStruct: PUCHAR;
    dwKeyLength: ULONG;
    pbKey: PUCHAR
); stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Unimplemented('XcRC4Key');

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_XcRC4Crypt(
    pbKeyStruct: PUCHAR;
    dwInputLength: ULONG;
    pbInput: PUCHAR
); stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Unimplemented('XcRC4Crypt');

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_XcHMAC(
    pbKeyMaterial: PBYTE;
    cbKeyMaterial: ULONG;
    pbData: PBYTE;
    cbData: ULONG;
    pbData2: PBYTE;
    {OUT}cbData2: ULONG;
    {OUT}HmacData: PBYTE  // length must be A_SHA_DIGEST_LEN
); stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Unimplemented('XcHMAC');

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_XcPKEncPublic(
    pbPubKey: PUCHAR;
    pbInput: PUCHAR;
    {OUT}pbOutput:  PUCHAR
): ULONG; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('XcPKEncPublic');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_XcPKDecPrivate(
    pbPrvKey: PUCHAR;
    pbInput: PUCHAR;
    {OUT}pbOutput: PUCHAR
): ULONG; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('XcPKDecPrivate');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_XcPKGetKeyLen(
    pbPubKey: PUCHAR
): ULONG; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('XcPKGetKeyLen');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

function xboxkrnl_XcVerifyPKCS1Signature(
    pbSig: PUCHAR;
    pbPubKey: PUCHAR;
    pbDigest: PUCHAR
): BOOL; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Unimplemented('XcVerifyPKCS1Signature');
  Result := BOOL(S_OK);

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_XcModExp(
    {OUT} pA: PULONG;
    pB: PULONG;
    pC: PULONG;
    pD: PULONG;
    dwN: ULONG
): ULONG; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Unimplemented('XcModExp');
  Result := S_OK;

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_XcDESKeyParity(
    {OUT}pbKey: PUCHAR;
    dwKeyLength: ULONG
); stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Unimplemented('XcDESKeyParity');

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_XcKeyTable(
    dwCipher: ULONG;
    {OUT}pbKeyTable: PUCHAR;
    pbKey: PUCHAR
); stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Unimplemented('XcKeyTable');

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_XcBlockCrypt(
    dwCipher: ULONG;
    {OUT}pbOutput: PUCHAR;
    pbInput: PUCHAR;
    pbKeyTable: PUCHAR;
    dwOp: ULONG
); stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Unimplemented('XcBlockCrypt');

  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_XcBlockCryptCBC(
	dwCipher: ULONG;
	dwInputLength: ULONG;
	{OUT}pbOutput: PUCHAR;
	pbInput: PUCHAR;
	pbKeyTable: PUCHAR;
	dwOp: ULONG;
	pbFeedback: PUCHAR
); stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Unimplemented('XcBlockCryptCBC');

  EmuSwapFS(fsXbox);
end;

function xboxkrnl_XcCryptService(
    dwOp: ULONG;
    pArgs: Pvoid
): ULONG; stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('XcCryptService');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

procedure xboxkrnl_XcUpdateCrypto(
    (*pNewVector: PCRYPTO_VECTOR;
    {OUT}pROMVector: OPTIONAL PCRYPTO_VECTOR *)
); stdcall;
// Source:?  Branch:dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);

  Unimplemented('XcUpdateCrypto');

  EmuSwapFS(fsXbox);
end;

end.
