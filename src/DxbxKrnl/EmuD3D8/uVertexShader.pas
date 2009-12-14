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
  // DirectX
  , D3DX8
  , Direct3D8

  // Dxbx
  , uTypes
  , uEmuD3D8Types
  , uEmuAlloc;


type
  LPD3DXBUFFER = ID3DXBuffer; // Dxbx TODO : Move to better location.

  _VSH_SHADER_HEADER = packed record
    aType: uint08;
    Version: uint08;
    NumInst: uint08;
    Unknown0: uint08;
  end;

  VSH_SHADER_HEADER = _VSH_SHADER_HEADER;

const
  VSH_INSTRUCTION_SIZE = 4;
  VSH_INSTRUCTION_SIZE_BYTES = VSH_INSTRUCTION_SIZE * sizeof(DWORD);


function XTL_IsValidCurrentShader: Boolean; stdcall; // forward
procedure XTL_FreeVertexDynamicPatch(pVertexShader: PVERTEX_SHADER) stdcall;

function VshHandleIsVertexShader(aHandle: DWORD): Boolean;
function VshHandleGetVertexShader(aHandle: DWORD): PX_D3DVertexShader;
function XTL_EmuRecompileVshDeclaration
(
  pDeclaration: PDWORD;
  ppRecompiledDeclaration: PDWORD;
  pDeclarationSize: PDWORD;
  IsFixedFunction: Boolean;
  pVertexDynamicPatch: VERTEX_DYNAMIC_PATCH
) : DWORD;

function XTL_EmuRecompileVshFunction
(
    pFunction: DWORD;
    ppRecompiled: LPD3DXBUFFER;
    pOriginalSize: DWORD;
    bNoReservedConstants: boolean
) : HRESULT; stdcall;

function VshGetDeclarationSize(pDeclaration: PDWord): DWORD;


(*#define DEF_VSH_END 0xFFFFFFFF
(*#define DEF_VSH_NOP 0x00000000 *)


implementation

uses
  // Dxbx
  uEmuFS
  , uEmuD3D8;

function VshGetDeclarationSize(pDeclaration: PDWord): DWORD;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
var
  Pos: DWORD;
begin
  Pos := 0;
(*  while pDeclaration+Pos <> DEF_VSH_END do
    inc(Pos); *)

  Result := (Pos + 1) * sizeof(DWORD);
end;

function XTL_EmuRecompileVshDeclaration
(
  pDeclaration: PDWORD;
  ppRecompiledDeclaration: PDWORD;
  pDeclarationSize: PDWORD;
  IsFixedFunction: Boolean;
  pVertexDynamicPatch: VERTEX_DYNAMIC_PATCH
) : DWORD;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
var
  DeclarationSize: DWORD;
begin
  // First of all some info:
  // We have to figure out which flags are set and then
  // we have to patch their params

  // some token values
  // 0xFFFFFFFF - end of the declaration
  // 0x00000000 - nop (means that this value is ignored)

  // Calculate size of declaration
  DeclarationSize := VshGetDeclarationSize(pDeclaration);
  *ppRecompiledDeclaration = (DWORD *)(*CxbxMalloc(DeclarationSize);
  DWORD *pRecompiled = *ppRecompiledDeclaration;
  memcpy(pRecompiled, pDeclaration, DeclarationSize);
  *pDeclarationSize = DeclarationSize;

  // TODO: Put these in one struct
  VSH_PATCH_DATA       PatchData = { 0 };

  DbgVshPrintf("DWORD dwVSHDecl[] =\n{\n");

  while (*pRecompiled != DEF_VSH_END)
  {
      DWORD Step = VshRecompileToken(pRecompiled, IsFixedFunction, &PatchData);
      pRecompiled += Step;
  }
  DbgVshPrintf("\tD3DVSD_END()\n};\n");

  VshAddStreamPatch(&PatchData);

  DbgVshPrintf("NbrStreams: %d\n", PatchData.StreamPatchData.NbrStreams);

  // Copy the patches to the vertex shader struct
  DWORD StreamsSize = PatchData.StreamPatchData.NbrStreams * sizeof(STREAM_DYNAMIC_PATCH);
  pVertexDynamicPatch->NbrStreams = PatchData.StreamPatchData.NbrStreams;
  pVertexDynamicPatch->pStreamPatches = (STREAM_DYNAMIC_PATCH *)(*CxbxMalloc(StreamsSize);
  memcpy(pVertexDynamicPatch->pStreamPatches,
         PatchData.StreamPatchData.pStreamPatches,
         StreamsSize);

  result := D3D_OK;
end;

function XTL_EmuRecompileVshFunction
(
    pFunction: DWORD;
    ppRecompiled: LPD3DXBUFFER;
    pOriginalSize: DWORD;
    bNoReservedConstants: boolean
) : HRESULT; stdcall;
// Branch:martin  Revision:39  Translator:PatrickvL  Done:0
begin
(*    VSH_SHADER_HEADER   *pShaderHeader = (VSH_SHADER_HEADER*)(*pFunction;
    DWORD               *pToken;
    boolean             EOI = false;
    VSH_XBOX_SHADER     *pShader = (VSH_XBOX_SHADER*)(*CxbxMalloc(sizeof(VSH_XBOX_SHADER));
    HRESULT             hRet = 0;

    // TODO: support this situation..
    if(pFunction == NULL)
        return E_FAIL;

    *ppRecompiled = NULL;
    *pOriginalSize = 0;
    if(!pShader)
    {
        EmuWarning("Couldn't allocate memory for vertex shader conversion buffer");
        hRet = E_OUTOFMEMORY;
    }
    memset(pShader, 0, sizeof(VSH_XBOX_SHADER));
    pShader->ShaderHeader = *pShaderHeader;
    switch(pShaderHeader->Version)
    {
        case VERSION_XVS:
            break;
        case VERSION_XVSS:
            EmuWarning("Might not support vertex state shaders?");
            hRet = E_FAIL;
            break;
        case VERSION_XVSW:
            EmuWarning("Might not support vertex read/write shaders?");
            hRet = E_FAIL;
            break;
        default:
            EmuWarning("Unknown vertex shader version 0x%02X\n", pShaderHeader->Version);
            hRet = E_FAIL;
            break;
    }

    if(SUCCEEDED(hRet))
    {

        for (pToken = (DWORD*)(*((uint08*)(*pFunction + sizeof(VSH_SHADER_HEADER)); !EOI; pToken += VSH_INSTRUCTION_SIZE)
        {
            VSH_SHADER_INSTRUCTION Inst;

            VshParseInstruction(pToken, &Inst);
            VshConvertToIntermediate(&Inst, pShader);
            EOI = (boolean)VshGetField(pToken, FLD_FINAL);
        }

        // The size of the shader is
        *pOriginalSize = (DWORD)pToken - (DWORD)pFunction;

        char* pShaderDisassembly = (char*)(*CxbxMalloc(pShader->IntermediateCount * 50); // Should be plenty
        DbgVshPrintf("-- Before conversion --\n");
        VshWriteShader(pShader, pShaderDisassembly, FALSE);
        DbgVshPrintf("%s", pShaderDisassembly);
        DbgVshPrintf("-----------------------\n");

        VshConvertShader(pShader, bNoReservedConstants);
        VshWriteShader(pShader, pShaderDisassembly, TRUE);

        DbgVshPrintf("-- After conversion ---\n");
        DbgVshPrintf("%s", pShaderDisassembly);
        DbgVshPrintf("-----------------------\n");

        hRet = D3DXAssembleShader(pShaderDisassembly,
                                  strlen(pShaderDisassembly),
                                  D3DXASM_SKIPVALIDATION,
                                  NULL,
                                  ppRecompiled,
                                  NULL);

        if (FAILED(hRet))
        {
            EmuWarning("Couldn't assemble recompiled vertex shader\n");
        }

        CxbxFree(pShaderDisassembly);
    }
    CxbxFree(pShader);

    return hRet;
}                        *)
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
    (* Cxbx has this disabled :
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
    *)
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

exports
  XTL_IsValidCurrentShader,
  XTL_EmuRecompileVshFunction,
  XTL_FreeVertexDynamicPatch;

end.

