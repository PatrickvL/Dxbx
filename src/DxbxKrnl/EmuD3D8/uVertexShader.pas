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

unit uVertexShader;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Windows
  // Jedi
  , JwaWinType
  // DirectX
  , D3DX8
  , Direct3D8
  // Dxbx
  , uEmu
  , uTypes
  , uEmuD3D8Types
  , uEmuAlloc;


const
  VSH_INSTRUCTION_SIZE = 4;
  VSH_INSTRUCTION_SIZE_BYTES = VSH_INSTRUCTION_SIZE * sizeof(DWORD);
  VSH_MAX_INTERMEDIATE_COUNT = 1024; // The maximum number of intermediate format slots

  VERSION_VS	=                      $F0; // vs.1.1, not an official value
  VERSION_XVS	=                      $20; // Xbox vertex shader
  VERSION_XVSS	=                    $73; // Xbox vertex state shader
  VERSION_XVSW	=                    $77; // Xbox vertex read/write shader
  VSH_XBOX_MAX_INSTRUCTION_COUNT	=  136;  // The maximum Xbox shader instruction count


type
  LPD3DXBUFFER = ID3DXBuffer; // Dxbx TODO : Move to better location.
  PLPD3DXBUFFER = ^LPD3DXBUFFER;

  _VSH_SHADER_HEADER = packed record
    aType: uint08;
    Version: uint08;
    NumInst: uint08;
    Unknown0: uint08;
  end;
  VSH_SHADER_HEADER = _VSH_SHADER_HEADER;
  PVSH_SHADER_HEADER = ^VSH_SHADER_HEADER;

  _VSH_TYPE_PATCH_DATA = packed record
    NbrTypes: DWORD;
    Types: array [0..255] of UINT;
  end;
  VSH_TYPE_PATCH_DATA = _VSH_TYPE_PATCH_DATA;

  _VSH_STREAM_PATCH_DATA = packed record
    NbrStreams: DWORD;
    pStreamPatches: array [0..255] of STREAM_DYNAMIC_PATCH;
  end;
  VSH_STREAM_PATCH_DATA = _VSH_STREAM_PATCH_DATA;

  _VSH_PATCH_DATA = packed record
    NeedPatching: boolean;
    ConvertedStride: DWORD;
    TypePatchData: VSH_TYPE_PATCH_DATA;
    StreamPatchData: VSH_STREAM_PATCH_DATA;
  end;
  VSH_PATCH_DATA = _VSH_PATCH_DATA;
  PVSH_PATCH_DATA = ^VSH_PATCH_DATA;

  _VSH_IMD_INSTRUCTION_TYPE = (IMD_MAC,IMD_ILU);
  VSH_IMD_INSTRUCTION_TYPE = _VSH_IMD_INSTRUCTION_TYPE;

  _VSH_IMD_OUTPUT_TYPE = (IMD_OUTPUT_C,
                          IMD_OUTPUT_R,
                          IMD_OUTPUT_O,
                          IMD_OUTPUT_A0X);
  VSH_IMD_OUTPUT_TYPE = _VSH_IMD_OUTPUT_TYPE;

  _VSH_IMD_OUTPUT = packed record
    aType: VSH_IMD_OUTPUT_TYPE;
    Mask: array [0..3] of boolean;
    Address: UInt16;
  end;
  VSH_IMD_OUTPUT = _VSH_IMD_OUTPUT;

  _VSH_ILU = (ILU_NOP = 0,
              ILU_MOV,
              ILU_RCP,
              ILU_RCC,
              ILU_RSQ,
              ILU_EXP,
              ILU_LOG,
              ILU_LIT);
  VSH_ILU = _VSH_ILU;

  _VSH_MAC = (MAC_NOP,
              MAC_MOV,
              MAC_MUL,
              AC_ADD,
              MAC_MAD,
              MAC_DP3,
              MAC_DPH,
              MAC_DP4,
              MAC_DST,
              MAC_MIN,
              MAC_MAX,
              MAC_SLT,
              MAC_SGE,
              MAC_ARL);
  VSH_MAC = _VSH_MAC;

  _VSH_PARAMETER_TYPE = (PARAM_UNKNOWN = 0,
                         PARAM_R,
                         PARAM_V,
                         PARAM_C);
  VSH_PARAMETER_TYPE = _VSH_PARAMETER_TYPE;

  _VSH_SWIZZLE = (SWIZZLE_X = 0,
                  SWIZZLE_Y,
                  SWIZZLE_Z,
                  SWIZZLE_W);
  VSH_SWIZZLE = _VSH_SWIZZLE;

  _VSH_PARAMETER = packed record
      ParameterType: VSH_PARAMETER_TYPE;   // Parameter type, R, V or C
      Neg: boolean;             // TRUE if negated, FALSE if not
      Swizzle: array [0..3] of VSH_SWIZZLE;      // The four swizzles
      Address: Uint16;         // Register address
  end;
  VSH_PARAMETER = _VSH_PARAMETER;

  _VSH_IMD_PARAMETER = packed record
    Active: boolean;
    Parameter: VSH_PARAMETER;
    IsA0X: boolean;
  end;
  VSH_IMD_PARAMETER = _VSH_IMD_PARAMETER;

  _VSH_INTERMEDIATE_FORMAT = packed record
    IsCombined: boolean;
    InstructionType: VSH_IMD_INSTRUCTION_TYPE;
    MAC: VSH_MAC;
    ILU: VSH_ILU;
    Output: VSH_IMD_OUTPUT;
    Parameters: array [0..2] of VSH_IMD_PARAMETER;
  end;
  VSH_INTERMEDIATE_FORMAT = _VSH_INTERMEDIATE_FORMAT;

  _VSH_XBOX_SHADER = packed record
    ShaderHeader: VSH_SHADER_HEADER;
    IntermediateCount: uint16;
    Intermediate: array [0..VSH_MAX_INTERMEDIATE_COUNT -1] of VSH_INTERMEDIATE_FORMAT ;
  end;
  VSH_XBOX_SHADER = _VSH_XBOX_SHADER;
  PVSH_XBOX_SHADER = ^VSH_XBOX_SHADER;

function XTL_IsValidCurrentShader: Boolean; stdcall; // forward
procedure XTL_FreeVertexDynamicPatch(pVertexShader: PVERTEX_SHADER) stdcall;

function VshHandleIsVertexShader(aHandle: DWORD): Boolean;
function VshHandleGetVertexShader(aHandle: DWORD): PX_D3DVertexShader;
function VshGetVertexDynamicPatch(Handle: DWORD): PVERTEX_DYNAMIC_PATCH;

function XTL_EmuRecompileVshDeclaration
(
  pDeclaration: PDWORD;
  ppRecompiledDeclaration: PPDWORD;
  pDeclarationSize: PDWORD;
  IsFixedFunction: Boolean;
  pVertexDynamicPatch: PVERTEX_DYNAMIC_PATCH
): DWORD;

function XTL_EmuRecompileVshFunction
(
    pFunction: PDWORD;
    ppRecompiled: PLPD3DXBUFFER;
    pOriginalSize: PDWORD;
    bNoReservedConstants: boolean
) : HRESULT; stdcall;

function VshGetDeclarationSize(pDeclaration: PDWord): DWORD;
function VshRecompileToken(pToken: PDWord; IsFixedFunction: boolean; pPatchData: PVSH_PATCH_DATA): DWORD;


const
  DEF_VSH_END = $FFFFFFFF;
  DEF_VSH_NOP = $00000000;

implementation

uses
  // Dxbx
  uEmuFS
  , uLog
  , uEmuD3D8;

function VshGetDeclarationSize(pDeclaration: PDWord): DWORD;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  Pos: DWORD;
begin
  Pos := 0;
  while PDWord(DWord(pDeclaration) + Pos)^ <> DEF_VSH_END do
    Inc(Pos);

  Result := (Pos + 1) * SizeOf(DWORD);
end;

function VshGetTokenType(Token: DWORD): DWORD;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  Result := (Token and D3DVSD_TOKENTYPEMASK) shr D3DVSD_TOKENTYPESHIFT;
end;

procedure VshConvertToken_NOP(pToken: PDWORD);
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  // D3DVSD_NOP
  if(pToken^ <> DEF_VSH_NOP) then
  begin
    EmuWarning('Token NOP found, but extra parameters are given!');
  end;
  DbgPrintf('D3DVSD_NOP(),');
end;

procedure VshConvertToken_STREAM(pToken: PDWORD; pPatchData: PVSH_PATCH_DATA);
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
var
  StreamNumber: DWORD;
begin
  // D3DVSD_STREAM_TESS
(*  if pToken^ and D3DVSD_STREAMTESSMASK then
  begin
    DbgPrintf('D3DVSD_STREAM_TESS(),');
  end
  // D3DVSD_STREAM
  else
  begin
    StreamNumber := VshGetVertexStream(pToken^);
    DbgPrintf('D3DVSD_STREAM(%s),', [StreamNumber]);

    // new stream
    // copy current data to structure
    if(VshAddStreamPatch(pPatchData)) then
    begin
      pPatchData.ConvertedStride := 0;
      pPatchData.TypePatchData.NbrTypes := 0;
      pPatchData.NeedPatching := False;
    end;

    Inc(pPatchData.StreamPatchData.NbrStreams);
  end; *)
end;

procedure VshConvertToken_STREAMDATA(pToken: PDWORD; IsFixedFunction: boolean; pPatchData: PVSH_PATCH_DATA );
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
(*    using namespace XTL;

    // D3DVSD_SKIP
    if(*pToken & 0x10000000)
    {
        VshConvertToken_STREAMDATA_SKIP(pToken);
    }
    // D3DVSD_SKIPBYTES
    else if(*pToken & 0x18000000)
    {
        VshConvertToken_STREAMDATA_SKIPBYTES(pToken);
    }
    // D3DVSD_REG
    else
    {
        VshConvertToken_STREAMDATA_REG(pToken, IsFixedFunction, pPatchData);
    } *)
end;

procedure VshConverToken_TESSELATOR(pToken: PDWORD; IsFixedFunction: boolean);
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
(*    using namespace XTL;

    // TODO: Investigate why Xb2PCRegisterType is only used for fixed function vertex shaders
    // D3DVSD_TESSUV
    if(*pToken & 0x10000000)
    {
        XTL::DWORD VertexRegister    = VshGetVertexRegister(*pToken);
        XTL::DWORD NewVertexRegister = VertexRegister;

        DbgVshPrintf("\tD3DVSD_TESSUV(");

        if(IsFixedFunction)
        {
            NewVertexRegister = Xb2PCRegisterType(VertexRegister);
        }
        else
        {
            DbgVshPrintf("%d", NewVertexRegister);
        }

        DbgVshPrintf("),\n");

        *pToken = D3DVSD_TESSUV(NewVertexRegister);
    }
    // D3DVSD_TESSNORMAL
    else
    {
        XTL::DWORD VertexRegisterIn  = VshGetVertexRegisterIn(*pToken);
        XTL::DWORD VertexRegisterOut = VshGetVertexRegister(*pToken);

        XTL::DWORD NewVertexRegisterIn  = VertexRegisterIn;
        XTL::DWORD NewVertexRegisterOut = VertexRegisterOut;

        DbgVshPrintf("\tD3DVSD_TESSNORMAL(");

        if(IsFixedFunction)
        {
            NewVertexRegisterIn = Xb2PCRegisterType(VertexRegisterIn);
        }
        else
        {
            DbgVshPrintf("%d", NewVertexRegisterIn);
        }

        DbgVshPrintf(", ");

        if(IsFixedFunction)
        {
            NewVertexRegisterOut = Xb2PCRegisterType(VertexRegisterOut);
        }
        else
        {
            DbgVshPrintf("%d", NewVertexRegisterOut);
        }

        DbgVshPrintf("),\n");
        *pToken = D3DVSD_TESSNORMAL(NewVertexRegisterIn, NewVertexRegisterOut);
    }      *)
end;

function VshConvertToken_CONSTMEM(pToken: PDWORD): DWORD;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:0
begin
(*    // D3DVSD_CONST
    DbgPrintf('tD3DVSD_CONST(');

    DWORD ConstantAddress = ((*pToken >> D3DVSD_CONSTADDRESSSHIFT) & 0xFF);
    DWORD Count           = (*pToken & D3DVSD_CONSTCOUNTMASK) >> D3DVSD_CONSTCOUNTSHIFT;

    DbgVshPrintf("%d, %d),\n", ConstantAddress, Count);

    //pToken = D3DVSD_CONST(ConstantAddress, Count);

    for (uint i = 0; i < Count; i++)
    {
        DbgVshPrintf("\t0x%08X,\n", pToken);
    }
    return Count;*)
end;


function VshRecompileToken(pToken: PDWord; IsFixedFunction: boolean; pPatchData: PVSH_PATCH_DATA): DWORD;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  Step: DWORD;
begin
  //using namespace XTL;
  Step := 1;

  case TD3DVSDTokenType(VshGetTokenType(pToken^)) of
    D3DVSD_TOKEN_NOP: VshConvertToken_NOP(pToken);
    D3DVSD_TOKEN_STREAM: VshConvertToken_STREAM(pToken, pPatchData);
    D3DVSD_TOKEN_STREAMDATA: VshConvertToken_STREAMDATA(pToken, IsFixedFunction, pPatchData);
    D3DVSD_TOKEN_TESSELLATOR: VshConverToken_TESSELATOR(pToken, IsFixedFunction);
    D3DVSD_TOKEN_CONSTMEM: Step := VshConvertToken_CONSTMEM(pToken);
  else
    DbgPrintf('Unknown token type: %d', [VshGetTokenType(pToken^)]);
  end;

  Result := Step;
end;

function VshAddStreamPatch(pPatchData: PVSH_PATCH_DATA): boolean;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
var
  CurrentStream: int;
  pStreamPatch: STREAM_DYNAMIC_PATCH;
begin
  CurrentStream := pPatchData.StreamPatchData.NbrStreams - 1;

  if (CurrentStream >= 0) then
  begin
    DbgPrintf('NeedPatching: %s', [pPatchData.NeedPatching]);

    pStreamPatch := pPatchData.StreamPatchData.pStreamPatches[CurrentStream];

    pStreamPatch.ConvertedStride := pPatchData.ConvertedStride;
    pStreamPatch.NbrTypes := pPatchData.TypePatchData.NbrTypes;
    pStreamPatch.NeedPatch := pPatchData.NeedPatching;
    pStreamPatch.pTypes := CxbxMalloc(pPatchData.TypePatchData.NbrTypes * sizeof(VSH_TYPE_PATCH_DATA));
    move(pPatchData.TypePatchData.Types, pStreamPatch.pTypes, pPatchData.TypePatchData.NbrTypes * sizeof(VSH_TYPE_PATCH_DATA));

    Result := TRUE;
    Exit;
  end;

  Result := FALSE;
end;

function XTL_EmuRecompileVshDeclaration
(
  pDeclaration: PDWORD;
  ppRecompiledDeclaration: PPDWORD;
  pDeclarationSize: PDWORD;
  IsFixedFunction: Boolean;
  pVertexDynamicPatch: PVERTEX_DYNAMIC_PATCH
) : DWORD;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  DeclarationSize: DWORD;
  pRecompiled: PDWord;
  PatchData: VSH_PATCH_DATA;
  Step: DWORD;
  StreamsSize: DWORD;
begin
  // First of all some info:
  // We have to figure out which flags are set and then
  // we have to patch their params

  // some token values
  // 0xFFFFFFFF - end of the declaration
  // 0x00000000 - nop (means that this value is ignored)

  // Calculate size of declaration
  DeclarationSize := VshGetDeclarationSize(pDeclaration);
  ppRecompiledDeclaration^ := PDWORD(CxbxMalloc(DeclarationSize));
  pRecompiled := ppRecompiledDeclaration^;

  memcpy(pRecompiled, pDeclaration, DeclarationSize);
  pDeclarationSize^ := DeclarationSize;

  // Cxbx TODO: Put these in one struct
  ZeroMemory(@PatchData, SizeOf(PatchData));

  DbgPrintf('DWORD dwVSHDecl[] = '#13#10'{');

  while pRecompiled^ <> DEF_VSH_END do
  begin
    Step := VshRecompileToken(pRecompiled, IsFixedFunction, @PatchData);
    Inc(DWord(pRecompiled), Step);
  end;
  DbgPrintf(#9'D3DVSD_END()'#13#10'};');

  VshAddStreamPatch(@PatchData);

  DbgPrintf('NbrStreams: %d', [PatchData.StreamPatchData.NbrStreams]);

  // Copy the patches to the vertex shader struct
  StreamsSize := PatchData.StreamPatchData.NbrStreams * sizeof(STREAM_DYNAMIC_PATCH);
  pVertexDynamicPatch.NbrStreams := PatchData.StreamPatchData.NbrStreams;
  pVertexDynamicPatch.pStreamPatches := PSTREAM_DYNAMIC_PATCH(CxbxMalloc(StreamsSize));
  memcpy(pVertexDynamicPatch.pStreamPatches,
         @(PatchData.StreamPatchData.pStreamPatches[0]),
         StreamsSize);

  Result := D3D_OK;
end;

function XTL_EmuRecompileVshFunction
(
    pFunction: PDWORD;
    ppRecompiled: PLPD3DXBUFFER;
    pOriginalSize: PDWORD;
    bNoReservedConstants: boolean
) : HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:50
var
  pShaderHeader: PVSH_SHADER_HEADER;
(*  pToken: PDWord; *)
  EOI: boolean;
  pShader: PVSH_XBOX_SHADER;
  hRet: HRESULT;
begin
  pShaderHeader := PVSH_SHADER_HEADER(pFunction);
  EOI := false;
  pShader := PVSH_XBOX_SHADER(CxbxMalloc(sizeof(VSH_XBOX_SHADER)));
  hRet := 0;

  // TODO: support this situation..
  if not Assigned(pFunction) then
  begin
    Result := E_FAIL;
    Exit;
  end;

  ppRecompiled := null;
  pOriginalSize := nil;
  if(not Assigned (pShader)) then
  begin
    EmuWarning('Couldn`t allocate memory for vertex shader conversion buffer');
    hRet := E_OUTOFMEMORY;
  end;
  memset(pShader, 0, sizeof(VSH_XBOX_SHADER));
  pShader.ShaderHeader := pShaderHeader^;
  case (pShaderHeader.Version) of
    VERSION_XVS: ;
    VERSION_XVSS:
      begin
        EmuWarning('Might not support vertex state shaders?');
        hRet := E_FAIL;
      end;
    VERSION_XVSW:
      begin
        EmuWarning('Might not support vertex read/write shaders?');
        hRet := E_FAIL;
      end;
    else
      begin
        EmuWarning('Unknown vertex shader version 0x%02X', [pShaderHeader.Version]);
        hRet := E_FAIL;
      end;
  end;

  if(SUCCEEDED(hRet)) then
  begin
       (*for (pToken = (DWORD*)(*((uint08*)(*pFunction + sizeof(VSH_SHADER_HEADER)); !EOI; pToken += VSH_INSTRUCTION_SIZE)
       {
           VSH_SHADER_INSTRUCTION Inst;

           VshParseInstruction(pToken, @Inst);
           VshConvertToIntermediate(@Inst, pShader);
           EOI = (boolean)VshGetField(pToken, FLD_FINAL);
       }

       // The size of the shader is
       *pOriginalSize = (DWORD)pToken - (DWORD)pFunction;

       char* pShaderDisassembly = (char*)(*CxbxMalloc(pShader.IntermediateCount * 50); // Should be plenty
       DbgVshPrintf('-- Before conversion --');
       VshWriteShader(pShader, pShaderDisassembly, FALSE);
       DbgVshPrintf('%s', pShaderDisassembly);
       DbgVshPrintf('-----------------------');

       VshConvertShader(pShader, bNoReservedConstants);
       VshWriteShader(pShader, pShaderDisassembly, TRUE);

       DbgVshPrintf('-- After conversion ---');
       DbgVshPrintf('%s', pShaderDisassembly);
       DbgVshPrintf('-----------------------');

       hRet = D3DXAssembleShader(pShaderDisassembly,
                                 strlen(pShaderDisassembly),
                                 D3DXASM_SKIPVALIDATION,
                                 NULL,
                                 ppRecompiled,
                                 NULL);

       if (FAILED(hRet))
       {
           EmuWarning('Couldn''t assemble recompiled vertex shader');
       }

       CxbxFree(pShaderDisassembly);*)
  end;
  CxbxFree(pShader);
  Result := hRet;
end;

procedure XTL_FreeVertexDynamicPatch(pVertexShader: PVERTEX_SHADER) stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  i: DWord;
begin
  for i := 0 to pVertexShader.VertexDynamicPatch.NbrStreams - 1 do
    CxbxFree(pVertexShader.VertexDynamicPatch.pStreamPatches[i].pTypes);

  CxbxFree(pVertexShader.VertexDynamicPatch.pStreamPatches);
  pVertexShader.VertexDynamicPatch.pStreamPatches := nil;
  pVertexShader.VertexDynamicPatch.NbrStreams := 0;
end;

// Checks for failed vertex shaders, and shaders that would need patching
function XTL_IsValidCurrentShader: Boolean; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  aHandle: DWORD;
  pVertexShader: PVERTEX_SHADER;
  pD3DVertexShader: PX_D3DVertexShader;
begin
  EmuSwapFS(fsWindows);
  XTL_EmuIDirect3DDevice8_GetVertexShader(@aHandle);
  EmuSwapFS(fsXbox);
  if (VshHandleIsVertexShader(aHandle)) then
  begin
    pD3DVertexShader := PX_D3DVertexShader(aHandle and $7FFFFFFF);
    pVertexShader := PVERTEX_SHADER(pD3DVertexShader.Handle);
    if (pVertexShader.Status <> 0) then
    begin
      Result := FALSE;
      Exit;
    end;
    { Cxbx has this disabled :
    for i := 0 to pVertexShader.VertexDynamicPatch.NbrStreams - 1 do
    begin
      if (pVertexShader.VertexDynamicPatch.pStreamPatches[i].NeedPatch) then
      begin
       // Just for caching purposes
        pVertexShader.Status := $80000001;
        Result := FALSE;
        Exit;
      end;
    end;
    }
  end;

  Result := True;
end;

function VshHandleIsVertexShader(aHandle: DWORD): Boolean;
// Branch:martin  Revision:39  Translator:Shadow_Tj  Done:100
begin
  Result := (aHandle and $8000000) <> 0;
end;

function VshHandleGetVertexShader(aHandle: DWORD): PX_D3DVertexShader;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
begin
  Result := PX_D3DVertexShader(aHandle and $7FFFFFFF);
end;

function VshGetVertexDynamicPatch(Handle: DWORD): PVERTEX_DYNAMIC_PATCH;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:100
var
  pD3DVertexShader: PX_D3DVertexShader;
  pVertexShader: PVERTEX_SHADER;
  i: uint32;
begin
  pD3DVertexShader := VshHandleGetVertexShader(Handle);
  pVertexShader := PVERTEX_SHADER(pD3DVertexShader.Handle);

  for i := 0 to pVertexShader.VertexDynamicPatch.NbrStreams -1 do
  begin
    if (pVertexShader.VertexDynamicPatch.pStreamPatches[i].NeedPatch) then
    begin
      Result := @pVertexShader.VertexDynamicPatch;
      Exit;
    end;
  end;
  result := null;
end;

exports
  XTL_IsValidCurrentShader,
  XTL_EmuRecompileVshFunction,
  XTL_FreeVertexDynamicPatch;

end.

