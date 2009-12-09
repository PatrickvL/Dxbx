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
unit uConsts;

{$INCLUDE Dxbx.inc}

interface

const
  DLL_IMAGE_BASE = $10000000;
  MAXIMUM_XBOX_THREADS = 256;

  CCXBXKRNLDLLNAME = 'CxbxKrnl.dll';
  CCXBXDLLNAME = 'Cxbx.dll';
  CDXBXKRNLDLLNAME = 'DxbxKrnl.dll';
  CDXBXDLLNAME = 'Dxbx.dll';

  // Application Versions
{$IFDEF DXBX_DEBUG}
  _DXBX_VERSION = '0.0.0.10 Debug';
{$ELSE}
  _DXBX_VERSION = '0.0.0.19';
{$ENDIF}
  _XDK_TRACKER_VERSION = '2.0.2.0';

  // Dialog Filters
  DIALOG_FILTER_TEXT = 'Text Documents (*.txt)|*.txt';
  DIALOG_FILTER_EXE = 'Windows Executables (*.exe)|*.exe';
  DIALOG_FILTER_XBE = 'Xbox Executables (*.xbe)|*.xbe';

  // Magic values

  XPR0_MAGIC_VALUE = $30525058; // MAKEFOURCC('XPR0'), bHasResourceOffsetsTable = FALSE;
  XPR1_MAGIC_VALUE = $31525058; // MAKEFOURCC('XPR1'), bHasResourceOffsetsTable = TRUE;
  XPR_MAGIC_VALUE = XPR0_MAGIC_VALUE;

  _MagicNumber = 'XBEH'; // Xbe File Format
  XBE_MAGIC_VALUE = $48454258; // "XBEH"

  // XOR keys

  XOR_EP_DEBUG = $94859D4B; // Entry Point (Debug)
  XOR_EP_RETAIL = $A8FC57AB; // Entry Point (Retail)
  XOR_KT_DEBUG = $EFB1F152; // Kernel Thunk (Debug)
  XOR_KT_RETAIL = $5B6D40B6; // Kernel Thunk (Retail)

  XOR_ENTRY_POINT_CHIHIRO = $40B5C16E; // Thanks to martin_sw
  XOR_KERNEL_THUNK_CHIHIRO = $2290059D; // Thanks to martin_sw

  // XBE constants

  XBE_INIT_FLAG_MountUtilityDrive = $00000001;
  XBE_INIT_FLAG_FormatUtilityDrive = $00000002;
  XBE_INIT_FLAG_Limit64MB = $00000004;
  XBE_INIT_FLAG_DontSetupHarddisk = $00000008;

  XBE_SECTIONHEADER_FLAG_Writable = $00000001;
  XBE_SECTIONHEADER_FLAG_Preload = $00000002;
  XBE_SECTIONHEADER_FLAG_Executable = $00000004;
  XBE_SECTIONHEADER_FLAG_InsertedFile = $00000008;
  XBE_SECTIONHEADER_FLAG_HeadPageRO = $00000010;
  XBE_SECTIONHEADER_FLAG_TailPageRO = $00000020;

  XBE_SECTIONNAME_MAXLENGTH = 9;

  XBE_LIBRARYVERSION_FLAG_ApprovedNo = $00;
  XBE_LIBRARYVERSION_FLAG_ApprovedPossibly = $20;
  XBE_LIBRARYVERSION_FLAG_ApprovedYes = $40;
  XBE_LIBRARYVERSION_FLAG_ApprovedMask = $60;
  XBE_LIBRARYVERSION_FLAG_DebugBuild = $80;

  XBE_LIBRARYNAME_MAXLENGTH = 8;

  // game region flags for Xbe certificate
  XBEIMAGE_GAME_REGION_NA = $00000001;
  XBEIMAGE_GAME_REGION_JAPAN = $00000002;
  XBEIMAGE_GAME_REGION_RESTOFWORLD = $00000004;
  XBEIMAGE_GAME_REGION_MANUFACTURING = $80000000;

  XBEIMAGE_GAME_REGION_ALL = XBEIMAGE_GAME_REGION_NA + XBEIMAGE_GAME_REGION_JAPAN + XBEIMAGE_GAME_REGION_RESTOFWORLD;

  // XBE media type flags for Xbe certificate
  
  XBEIMAGE_MEDIA_TYPE_HARD_DISK = $00000001;
  XBEIMAGE_MEDIA_TYPE_DVD_X2 = $00000002;
  XBEIMAGE_MEDIA_TYPE_DVD_CD = $00000004;
  XBEIMAGE_MEDIA_TYPE_CD = $00000008;
  XBEIMAGE_MEDIA_TYPE_DVD_5_RO = $00000010;
  XBEIMAGE_MEDIA_TYPE_DVD_9_RO = $00000020;
  XBEIMAGE_MEDIA_TYPE_DVD_5_RW = $00000040;
  XBEIMAGE_MEDIA_TYPE_DVD_9_RW = $00000080;
  XBEIMAGE_MEDIA_TYPE_DONGLE = $00000100;
  XBEIMAGE_MEDIA_TYPE_MEDIA_BOARD = $00000200;
  XBEIMAGE_MEDIA_TYPE_NONSECURE_HARD_DISK = $40000000;
  XBEIMAGE_MEDIA_TYPE_NONSECURE_MODE  = $80000000;
  XBEIMAGE_MEDIA_TYPE_MEDIA_MASK = $00FFFFFF;

  // Alignments

  PE_FILE_ALIGNMENT = $00000020; // Note : Delphi linker uses $200 ! Should we switch?
  PE_SECTION_ALIGNMENT = $00000020; // Note : Delphi linker uses $1000 ! Should we switch?
  PE_HEADER_ALIGNMENT = $1000; // - Actually, there's no such thing; JclDebug calls this 'ModuleCodeOffset'

  // XPR constants
  
  XPR_IMAGE_HDR_SIZE = 2048; // 2K
  XPR_IMAGE_WH = 128; // width and height
  XPR_IMAGE_DATA_SIZE = (XPR_IMAGE_WH * XPR_IMAGE_WH) div 2; // DXT1 is 4 bits per pixel

  XBE_SECTIONNAME_GAMEICON = '$$XTIMAGE';
  XBE_SECTIONNAME_SAVEICON = '$$XSIMAGE';

  // Copied from System.pas :
  EXCEPTION_CONTINUE_SEARCH = 0;
  EXCEPTION_EXECUTE_HANDLER = 1;
  EXCEPTION_CONTINUE_EXECUTION = -1;

  cXDK_TRACKER_DATA_FILE = 'GameData.dat';
  cXDk_TRACKER_XML_VERSION = '1.1';

  // Websites
  cWEBSITE_CXBX = 'http://www.caustik.com/cxbx/';
  cWEBSITE_SHADOWTJ = 'http://www.shadowtj.org';
  cWEBSITE_FORUM = 'http://forums.ngemu.com/dxbx-official-discussion/';

  cOpen = 'open';

  CCXBXKRNLINIT = 'CxbxKrnlInit';
  CXBXKRNL_KERNELTHUNKTABLE = 'CxbxKrnl_KernelThunkTable';
  CSETXBEPATHMANGLEDNAME = '?SetXbePath@EmuShared@@QAEXPBD@Z';

  // Limits
  _RecentXbeLimit: Integer = 10;
  _RecentExeLimit: Integer = 10;

implementation

end.
