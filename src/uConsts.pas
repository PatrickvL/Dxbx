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

uses
  // Delphi
  Messages; // WM_USER

const
  OPCODE_NOP = $90;
  OPCODE_RETN = $C2;
  OPCODE_RET = $C3;
  OPCODE_RETFN = $CA;
  OPCODE_RETF = $CB;
  OPCODE_INT3 = $CC;
  OPCODE_CALL = $E8;
  OPCODE_JMP = $E9;

  XBOX_MEMORY_SIZE = 128*1024*1024;

  XBE_IMAGE_BASE = $00010000;
  XBE_HEADER_SIZE = $1000;

  DLL_IMAGE_BASE = $10000000;
  MAXIMUM_XBOX_THREADS = 256;

  CDXBXKRNLDLLNAME = 'DxbxKrnl.dll';
  CDXBXDLLNAME = 'Dxbx.dll';

  // Application Versions
  _DXBX_VERSION = '0.6 ' + {$IFDEF DEBUG}'Debug'{$ELSE}'Release'{$ENDIF};
  _XDK_TRACKER_VERSION = '2.1';
  _XBE_EXPLORER_VERSION = '1.1';

  // Dialog Filters
  DIALOG_FILTER_TEXT = 'Text Documents (*.txt)|*.txt';
  DIALOG_FILTER_XBE = 'Xbox Executables (*.xbe)|*.xbe';

  // Magic values

  XPR0_MAGIC_VALUE = $30525058; // MAKEFOURCC('XPR0'), bHasResourceOffsetsTable = FALSE;
  XPR1_MAGIC_VALUE = $31525058; // MAKEFOURCC('XPR1'), bHasResourceOffsetsTable = TRUE;

  XPR_MAGIC_VALUE = XPR0_MAGIC_VALUE;

  _MagicNumber = 'XBEH'; // Xbe File Format
  XBE_MAGIC_VALUE = $48454258; // "XBEH"

  // XOR keys

  XOR_MAX_VIRTUAL_ADDRESS = $01000000;
  
  XOR_EP_DEBUG = $94859D4B; // Entry Point (Debug)
  XOR_EP_RETAIL = $A8FC57AB; // Entry Point (Retail)
  XOR_KT_DEBUG = $EFB1F152; // Kernel Thunk (Debug)
  XOR_KT_RETAIL = $5B6D40B6; // Kernel Thunk (Retail)

  XOR_ENTRY_POINT_CHIHIRO = $40B5C16E; // Thanks to martin_sw
  XOR_KERNEL_THUNK_CHIHIRO = $2290059D; // Thanks to martin_sw

  // XBE constants

  // Initialization flags
  XBE_INIT_FLAG_MountUtilityDrive = $00000001;
  XBE_INIT_FLAG_FormatUtilityDrive = $00000002;
  XBE_INIT_FLAG_Limit64MB = $00000004;
  XBE_INIT_FLAG_DontSetupHarddisk = $00000008;

  // Section flags
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

  XBE_TITLENAME_MAXLENGTH = 40;

  XBE_DebugUnicodeFileName_MAXLENGTH = 40; // ?? 256;

  // game region flags for Xbe certificate
  XBEIMAGE_GAME_REGION_US_CANADA = $00000001;
  XBEIMAGE_GAME_REGION_JAPAN = $00000002;
  XBEIMAGE_GAME_REGION_RESTOFWORLD = $00000004;
  XBEIMAGE_GAME_REGION_MANUFACTURING = $80000000;

  XBEIMAGE_GAME_REGION_ALL = XBEIMAGE_GAME_REGION_US_CANADA + XBEIMAGE_GAME_REGION_JAPAN + XBEIMAGE_GAME_REGION_RESTOFWORLD;

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

  WM_USER_PARENTNOTIFY = WM_USER + 123;

  cXDK_TRACKER_DATA_FILE = 'GameData.dat';
  cXDk_TRACKER_XML_VERSION = '1.1';

  // Websites
  cWEBSITE_CXBX = 'http://www.caustik.com/cxbx/';
  cWEBSITE_SHADOWTJ = 'http://www.shadowtj.org';
  cWEBSITE_FORUM = 'http://forums.ngemu.com/dxbx-official-discussion/';

  cOpen = 'open';

  CSETXBEPATHMANGLEDNAME = '?SetXbePath@EmuShared@@QAEXPBD@Z';

  // Limits
  _RecentXbeLimit: Integer = 10;

  DxbxDefaultXbeVolumeLetter = 'C'; // For now, assume 'C' is the default volume for an Xbe

  // TitleIDs - you can find these at http://www.xbox-games.org/ :
  TITLEID_Azurik = $4D530007;
  TITLEID_CrazyTaxi = $53450004;
  TITLEID_DeadToRights_NTSC = $4E4D0005;
  TITLEID_DeadToRights_PAL = $4541002C;
  TITLEID_Halo = $4D530004;
  TITLEID_Halo2 = $4D530064;
  TITLEID_PetitCopter = $41510001;
  TITLEID_TurokNTSC = $41430004;
  TITLEID_UnrealChampionship = $49470024;
  TITLEID_UnrealChampionship2 = $4D570021;
  TITLEID_UnrealII = $4947003C;

  // Aliases, used in code
  TITLEID_DeadToRights = TITLEID_DeadToRights_NTSC; // Caustic/Blueshogun probably used the NTSC version
  TITLEID_Unreal = TITLEID_UnrealII; // ??

implementation

end.
