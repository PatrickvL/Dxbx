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
unit uKernelThunk;

interface

uses
  // Delphi
  Types, // for DWord
  SysUtils, // for IntToStr
  // Dxbx
  uTypes,
  uLog; // for WriteLog

function CxbxKrnl_KernelThunkTable: PThunkTable; stdcall;

implementation

procedure Unimplemented(const aIndex: Word); // stdcall ??
begin
  WriteLog('Unimplemented method in thunk table index : ' + IntToStr(aIndex));
end;

// These method will at least give us a message saying which indexes aren't implemented :
procedure Panic_000; begin Unimplemented($000); end;
procedure Panic_001; begin Unimplemented($001); end;
procedure Panic_002; begin Unimplemented($002); end;
procedure Panic_003; begin Unimplemented($003); end;
procedure Panic_004; begin Unimplemented($004); end;
procedure Panic_005; begin Unimplemented($005); end;
procedure Panic_006; begin Unimplemented($006); end;
procedure Panic_007; begin Unimplemented($007); end;
procedure Panic_008; begin Unimplemented($008); end;
procedure Panic_009; begin Unimplemented($009); end;
procedure Panic_00A; begin Unimplemented($00A); end;
procedure Panic_00B; begin Unimplemented($00B); end;
procedure Panic_00C; begin Unimplemented($00C); end;
procedure Panic_00D; begin Unimplemented($00D); end;
procedure Panic_00E; begin Unimplemented($00E); end;
procedure Panic_00F; begin Unimplemented($00F); end;
procedure Panic_010; begin Unimplemented($010); end;
procedure Panic_011; begin Unimplemented($011); end;
procedure Panic_012; begin Unimplemented($012); end;
procedure Panic_013; begin Unimplemented($013); end;
procedure Panic_014; begin Unimplemented($014); end;
procedure Panic_015; begin Unimplemented($015); end;
procedure Panic_016; begin Unimplemented($016); end;
procedure Panic_017; begin Unimplemented($017); end;
procedure Panic_018; begin Unimplemented($018); end;
procedure Panic_019; begin Unimplemented($019); end;
procedure Panic_01A; begin Unimplemented($01A); end;
procedure Panic_01B; begin Unimplemented($01B); end;
procedure Panic_01C; begin Unimplemented($01C); end;
procedure Panic_01D; begin Unimplemented($01D); end;
procedure Panic_01E; begin Unimplemented($01E); end;
procedure Panic_01F; begin Unimplemented($01F); end;
procedure Panic_020; begin Unimplemented($020); end;
procedure Panic_021; begin Unimplemented($021); end;
procedure Panic_022; begin Unimplemented($022); end;
procedure Panic_023; begin Unimplemented($023); end;
procedure Panic_024; begin Unimplemented($024); end;
procedure Panic_025; begin Unimplemented($025); end;
procedure Panic_026; begin Unimplemented($026); end;
procedure Panic_027; begin Unimplemented($027); end;
procedure Panic_028; begin Unimplemented($028); end;
procedure Panic_029; begin Unimplemented($029); end;
procedure Panic_02A; begin Unimplemented($02A); end;
procedure Panic_02B; begin Unimplemented($02B); end;
procedure Panic_02C; begin Unimplemented($02C); end;
procedure Panic_02D; begin Unimplemented($02D); end;
procedure Panic_02E; begin Unimplemented($02E); end;
procedure Panic_02F; begin Unimplemented($02F); end;
procedure Panic_030; begin Unimplemented($030); end;
procedure Panic_031; begin Unimplemented($031); end;
procedure Panic_032; begin Unimplemented($032); end;
procedure Panic_033; begin Unimplemented($033); end;
procedure Panic_034; begin Unimplemented($034); end;
procedure Panic_035; begin Unimplemented($035); end;
procedure Panic_036; begin Unimplemented($036); end;
procedure Panic_037; begin Unimplemented($037); end;
procedure Panic_038; begin Unimplemented($038); end;
procedure Panic_039; begin Unimplemented($039); end;
procedure Panic_03A; begin Unimplemented($03A); end;
procedure Panic_03B; begin Unimplemented($03B); end;
procedure Panic_03C; begin Unimplemented($03C); end;
procedure Panic_03D; begin Unimplemented($03D); end;
procedure Panic_03E; begin Unimplemented($03E); end;
procedure Panic_03F; begin Unimplemented($03F); end;
procedure Panic_040; begin Unimplemented($040); end;
procedure Panic_041; begin Unimplemented($041); end;
procedure Panic_042; begin Unimplemented($042); end;
procedure Panic_043; begin Unimplemented($043); end;
procedure Panic_044; begin Unimplemented($044); end;
procedure Panic_045; begin Unimplemented($045); end;
procedure Panic_046; begin Unimplemented($046); end;
procedure Panic_047; begin Unimplemented($047); end;
procedure Panic_048; begin Unimplemented($048); end;
procedure Panic_049; begin Unimplemented($049); end;
procedure Panic_04A; begin Unimplemented($04A); end;
procedure Panic_04B; begin Unimplemented($04B); end;
procedure Panic_04C; begin Unimplemented($04C); end;
procedure Panic_04D; begin Unimplemented($04D); end;
procedure Panic_04E; begin Unimplemented($04E); end;
procedure Panic_04F; begin Unimplemented($04F); end;
procedure Panic_050; begin Unimplemented($050); end;
procedure Panic_051; begin Unimplemented($051); end;
procedure Panic_052; begin Unimplemented($052); end;
procedure Panic_053; begin Unimplemented($053); end;
procedure Panic_054; begin Unimplemented($054); end;
procedure Panic_055; begin Unimplemented($055); end;
procedure Panic_056; begin Unimplemented($056); end;
procedure Panic_057; begin Unimplemented($057); end;
procedure Panic_058; begin Unimplemented($058); end;
procedure Panic_059; begin Unimplemented($059); end;
procedure Panic_05A; begin Unimplemented($05A); end;
procedure Panic_05B; begin Unimplemented($05B); end;
procedure Panic_05C; begin Unimplemented($05C); end;
procedure Panic_05D; begin Unimplemented($05D); end;
procedure Panic_05E; begin Unimplemented($05E); end;
procedure Panic_05F; begin Unimplemented($05F); end;
procedure Panic_060; begin Unimplemented($060); end;
procedure Panic_061; begin Unimplemented($061); end;
procedure Panic_062; begin Unimplemented($062); end;
procedure Panic_063; begin Unimplemented($063); end;
procedure Panic_064; begin Unimplemented($064); end;
procedure Panic_065; begin Unimplemented($065); end;
procedure Panic_066; begin Unimplemented($066); end;
procedure Panic_067; begin Unimplemented($067); end;
procedure Panic_068; begin Unimplemented($068); end;
procedure Panic_069; begin Unimplemented($069); end;
procedure Panic_06A; begin Unimplemented($06A); end;
procedure Panic_06B; begin Unimplemented($06B); end;
procedure Panic_06C; begin Unimplemented($06C); end;
procedure Panic_06D; begin Unimplemented($06D); end;
procedure Panic_06E; begin Unimplemented($06E); end;
procedure Panic_06F; begin Unimplemented($06F); end;
procedure Panic_070; begin Unimplemented($070); end;
procedure Panic_071; begin Unimplemented($071); end;
procedure Panic_072; begin Unimplemented($072); end;
procedure Panic_073; begin Unimplemented($073); end;
procedure Panic_074; begin Unimplemented($074); end;
procedure Panic_075; begin Unimplemented($075); end;
procedure Panic_076; begin Unimplemented($076); end;
procedure Panic_077; begin Unimplemented($077); end;
procedure Panic_078; begin Unimplemented($078); end;
procedure Panic_079; begin Unimplemented($079); end;
procedure Panic_07A; begin Unimplemented($07A); end;
procedure Panic_07B; begin Unimplemented($07B); end;
procedure Panic_07C; begin Unimplemented($07C); end;
procedure Panic_07D; begin Unimplemented($07D); end;
procedure Panic_07E; begin Unimplemented($07E); end;
procedure Panic_07F; begin Unimplemented($07F); end;
procedure Panic_080; begin Unimplemented($080); end;
procedure Panic_081; begin Unimplemented($081); end;
procedure Panic_082; begin Unimplemented($082); end;
procedure Panic_083; begin Unimplemented($083); end;
procedure Panic_084; begin Unimplemented($084); end;
procedure Panic_085; begin Unimplemented($085); end;
procedure Panic_086; begin Unimplemented($086); end;
procedure Panic_087; begin Unimplemented($087); end;
procedure Panic_088; begin Unimplemented($088); end;
procedure Panic_089; begin Unimplemented($089); end;
procedure Panic_08A; begin Unimplemented($08A); end;
procedure Panic_08B; begin Unimplemented($08B); end;
procedure Panic_08C; begin Unimplemented($08C); end;
procedure Panic_08D; begin Unimplemented($08D); end;
procedure Panic_08E; begin Unimplemented($08E); end;
procedure Panic_08F; begin Unimplemented($08F); end;
procedure Panic_090; begin Unimplemented($090); end;
procedure Panic_091; begin Unimplemented($091); end;
procedure Panic_092; begin Unimplemented($092); end;
procedure Panic_093; begin Unimplemented($093); end;
procedure Panic_094; begin Unimplemented($094); end;
procedure Panic_095; begin Unimplemented($095); end;
procedure Panic_096; begin Unimplemented($096); end;
procedure Panic_097; begin Unimplemented($097); end;
procedure Panic_098; begin Unimplemented($098); end;
procedure Panic_099; begin Unimplemented($099); end;
procedure Panic_09A; begin Unimplemented($09A); end;
procedure Panic_09B; begin Unimplemented($09B); end;
procedure Panic_09C; begin Unimplemented($09C); end;
procedure Panic_09D; begin Unimplemented($09D); end;
procedure Panic_09E; begin Unimplemented($09E); end;
procedure Panic_09F; begin Unimplemented($09F); end;
procedure Panic_0A0; begin Unimplemented($0A0); end;
procedure Panic_0A1; begin Unimplemented($0A1); end;
procedure Panic_0A2; begin Unimplemented($0A2); end;
procedure Panic_0A3; begin Unimplemented($0A3); end;
procedure Panic_0A4; begin Unimplemented($0A4); end;
procedure Panic_0A5; begin Unimplemented($0A5); end;
procedure Panic_0A6; begin Unimplemented($0A6); end;
procedure Panic_0A7; begin Unimplemented($0A7); end;
procedure Panic_0A8; begin Unimplemented($0A8); end;
procedure Panic_0A9; begin Unimplemented($0A9); end;
procedure Panic_0AA; begin Unimplemented($0AA); end;
procedure Panic_0AB; begin Unimplemented($0AB); end;
procedure Panic_0AC; begin Unimplemented($0AC); end;
procedure Panic_0AD; begin Unimplemented($0AD); end;
procedure Panic_0AE; begin Unimplemented($0AE); end;
procedure Panic_0AF; begin Unimplemented($0AF); end;
procedure Panic_0B0; begin Unimplemented($0B0); end;
procedure Panic_0B1; begin Unimplemented($0B1); end;
procedure Panic_0B2; begin Unimplemented($0B2); end;
procedure Panic_0B3; begin Unimplemented($0B3); end;
procedure Panic_0B4; begin Unimplemented($0B4); end;
procedure Panic_0B5; begin Unimplemented($0B5); end;
procedure Panic_0B6; begin Unimplemented($0B6); end;
procedure Panic_0B7; begin Unimplemented($0B7); end;
procedure Panic_0B8; begin Unimplemented($0B8); end;
procedure Panic_0B9; begin Unimplemented($0B9); end;
procedure Panic_0BA; begin Unimplemented($0BA); end;
procedure Panic_0BB; begin Unimplemented($0BB); end;
procedure Panic_0BC; begin Unimplemented($0BC); end;
procedure Panic_0BD; begin Unimplemented($0BD); end;
procedure Panic_0BE; begin Unimplemented($0BE); end;
procedure Panic_0BF; begin Unimplemented($0BF); end;
procedure Panic_0C0; begin Unimplemented($0C0); end;
procedure Panic_0C1; begin Unimplemented($0C1); end;
procedure Panic_0C2; begin Unimplemented($0C2); end;
procedure Panic_0C3; begin Unimplemented($0C3); end;
procedure Panic_0C4; begin Unimplemented($0C4); end;
procedure Panic_0C5; begin Unimplemented($0C5); end;
procedure Panic_0C6; begin Unimplemented($0C6); end;
procedure Panic_0C7; begin Unimplemented($0C7); end;
procedure Panic_0C8; begin Unimplemented($0C8); end;
procedure Panic_0C9; begin Unimplemented($0C9); end;
procedure Panic_0CA; begin Unimplemented($0CA); end;
procedure Panic_0CB; begin Unimplemented($0CB); end;
procedure Panic_0CC; begin Unimplemented($0CC); end;
procedure Panic_0CD; begin Unimplemented($0CD); end;
procedure Panic_0CE; begin Unimplemented($0CE); end;
procedure Panic_0CF; begin Unimplemented($0CF); end;
procedure Panic_0D0; begin Unimplemented($0D0); end;
procedure Panic_0D1; begin Unimplemented($0D1); end;
procedure Panic_0D2; begin Unimplemented($0D2); end;
procedure Panic_0D3; begin Unimplemented($0D3); end;
procedure Panic_0D4; begin Unimplemented($0D4); end;
procedure Panic_0D5; begin Unimplemented($0D5); end;
procedure Panic_0D6; begin Unimplemented($0D6); end;
procedure Panic_0D7; begin Unimplemented($0D7); end;
procedure Panic_0D8; begin Unimplemented($0D8); end;
procedure Panic_0D9; begin Unimplemented($0D9); end;
procedure Panic_0DA; begin Unimplemented($0DA); end;
procedure Panic_0DB; begin Unimplemented($0DB); end;
procedure Panic_0DC; begin Unimplemented($0DC); end;
procedure Panic_0DD; begin Unimplemented($0DD); end;
procedure Panic_0DE; begin Unimplemented($0DE); end;
procedure Panic_0DF; begin Unimplemented($0DF); end;
procedure Panic_0E0; begin Unimplemented($0E0); end;
procedure Panic_0E1; begin Unimplemented($0E1); end;
procedure Panic_0E2; begin Unimplemented($0E2); end;
procedure Panic_0E3; begin Unimplemented($0E3); end;
procedure Panic_0E4; begin Unimplemented($0E4); end;
procedure Panic_0E5; begin Unimplemented($0E5); end;
procedure Panic_0E6; begin Unimplemented($0E6); end;
procedure Panic_0E7; begin Unimplemented($0E7); end;
procedure Panic_0E8; begin Unimplemented($0E8); end;
procedure Panic_0E9; begin Unimplemented($0E9); end;
procedure Panic_0EA; begin Unimplemented($0EA); end;
procedure Panic_0EB; begin Unimplemented($0EB); end;
procedure Panic_0EC; begin Unimplemented($0EC); end;
procedure Panic_0ED; begin Unimplemented($0ED); end;
procedure Panic_0EE; begin Unimplemented($0EE); end;
procedure Panic_0EF; begin Unimplemented($0EF); end;
procedure Panic_0F0; begin Unimplemented($0F0); end;
procedure Panic_0F1; begin Unimplemented($0F1); end;
procedure Panic_0F2; begin Unimplemented($0F2); end;
procedure Panic_0F3; begin Unimplemented($0F3); end;
procedure Panic_0F4; begin Unimplemented($0F4); end;
procedure Panic_0F5; begin Unimplemented($0F5); end;
procedure Panic_0F6; begin Unimplemented($0F6); end;
procedure Panic_0F7; begin Unimplemented($0F7); end;
procedure Panic_0F8; begin Unimplemented($0F8); end;
procedure Panic_0F9; begin Unimplemented($0F9); end;
procedure Panic_0FA; begin Unimplemented($0FA); end;
procedure Panic_0FB; begin Unimplemented($0FB); end;
procedure Panic_0FC; begin Unimplemented($0FC); end;
procedure Panic_0FD; begin Unimplemented($0FD); end;
procedure Panic_0FE; begin Unimplemented($0FE); end;
procedure Panic_0FF; begin Unimplemented($0FF); end;
procedure Panic_100; begin Unimplemented($100); end;
procedure Panic_101; begin Unimplemented($101); end;
procedure Panic_102; begin Unimplemented($102); end;
procedure Panic_103; begin Unimplemented($103); end;
procedure Panic_104; begin Unimplemented($104); end;
procedure Panic_105; begin Unimplemented($105); end;
procedure Panic_106; begin Unimplemented($106); end;
procedure Panic_107; begin Unimplemented($107); end;
procedure Panic_108; begin Unimplemented($108); end;
procedure Panic_109; begin Unimplemented($109); end;
procedure Panic_10A; begin Unimplemented($10A); end;
procedure Panic_10B; begin Unimplemented($10B); end;
procedure Panic_10C; begin Unimplemented($10C); end;
procedure Panic_10D; begin Unimplemented($10D); end;
procedure Panic_10E; begin Unimplemented($10E); end;
procedure Panic_10F; begin Unimplemented($10F); end;
procedure Panic_110; begin Unimplemented($110); end;
procedure Panic_111; begin Unimplemented($111); end;
procedure Panic_112; begin Unimplemented($112); end;
procedure Panic_113; begin Unimplemented($113); end;
procedure Panic_114; begin Unimplemented($114); end;
procedure Panic_115; begin Unimplemented($115); end;
procedure Panic_116; begin Unimplemented($116); end;
procedure Panic_117; begin Unimplemented($117); end;
procedure Panic_118; begin Unimplemented($118); end;
procedure Panic_119; begin Unimplemented($119); end;
procedure Panic_11A; begin Unimplemented($11A); end;
procedure Panic_11B; begin Unimplemented($11B); end;
procedure Panic_11C; begin Unimplemented($11C); end;
procedure Panic_11D; begin Unimplemented($11D); end;
procedure Panic_11E; begin Unimplemented($11E); end;
procedure Panic_11F; begin Unimplemented($11F); end;
procedure Panic_120; begin Unimplemented($120); end;
procedure Panic_121; begin Unimplemented($121); end;
procedure Panic_122; begin Unimplemented($122); end;
procedure Panic_123; begin Unimplemented($123); end;
procedure Panic_124; begin Unimplemented($124); end;
procedure Panic_125; begin Unimplemented($125); end;
procedure Panic_126; begin Unimplemented($126); end;
procedure Panic_127; begin Unimplemented($127); end;
procedure Panic_128; begin Unimplemented($128); end;
procedure Panic_129; begin Unimplemented($129); end;
procedure Panic_12A; begin Unimplemented($12A); end;
procedure Panic_12B; begin Unimplemented($12B); end;
procedure Panic_12C; begin Unimplemented($12C); end;
procedure Panic_12D; begin Unimplemented($12D); end;
procedure Panic_12E; begin Unimplemented($12E); end;
procedure Panic_12F; begin Unimplemented($12F); end;
procedure Panic_130; begin Unimplemented($130); end;
procedure Panic_131; begin Unimplemented($131); end;
procedure Panic_132; begin Unimplemented($132); end;
procedure Panic_133; begin Unimplemented($133); end;
procedure Panic_134; begin Unimplemented($134); end;
procedure Panic_135; begin Unimplemented($135); end;
procedure Panic_136; begin Unimplemented($136); end;
procedure Panic_137; begin Unimplemented($137); end;
procedure Panic_138; begin Unimplemented($138); end;
procedure Panic_139; begin Unimplemented($139); end;
procedure Panic_13A; begin Unimplemented($13A); end;
procedure Panic_13B; begin Unimplemented($13B); end;
procedure Panic_13C; begin Unimplemented($13C); end;
procedure Panic_13D; begin Unimplemented($13D); end;
procedure Panic_13E; begin Unimplemented($13E); end;
procedure Panic_13F; begin Unimplemented($13F); end;
procedure Panic_140; begin Unimplemented($140); end;
procedure Panic_141; begin Unimplemented($141); end;
procedure Panic_142; begin Unimplemented($142); end;
procedure Panic_143; begin Unimplemented($143); end;
procedure Panic_144; begin Unimplemented($144); end;
procedure Panic_145; begin Unimplemented($145); end;
procedure Panic_146; begin Unimplemented($146); end;
procedure Panic_147; begin Unimplemented($147); end;
procedure Panic_148; begin Unimplemented($148); end;
procedure Panic_149; begin Unimplemented($149); end;
procedure Panic_14A; begin Unimplemented($14A); end;
procedure Panic_14B; begin Unimplemented($14B); end;
procedure Panic_14C; begin Unimplemented($14C); end;
procedure Panic_14D; begin Unimplemented($14D); end;
procedure Panic_14E; begin Unimplemented($14E); end;
procedure Panic_14F; begin Unimplemented($14F); end;
procedure Panic_150; begin Unimplemented($150); end;
procedure Panic_151; begin Unimplemented($151); end;
procedure Panic_152; begin Unimplemented($152); end;
procedure Panic_153; begin Unimplemented($153); end;
procedure Panic_154; begin Unimplemented($154); end;
procedure Panic_155; begin Unimplemented($155); end;
procedure Panic_156; begin Unimplemented($156); end;
procedure Panic_157; begin Unimplemented($157); end;
procedure Panic_158; begin Unimplemented($158); end;
procedure Panic_159; begin Unimplemented($159); end;
procedure Panic_15A; begin Unimplemented($15A); end;
procedure Panic_15B; begin Unimplemented($15B); end;
procedure Panic_15C; begin Unimplemented($15C); end;
procedure Panic_15D; begin Unimplemented($15D); end;
procedure Panic_15E; begin Unimplemented($15E); end;
procedure Panic_15F; begin Unimplemented($15F); end;
procedure Panic_160; begin Unimplemented($160); end;
procedure Panic_161; begin Unimplemented($161); end;
procedure Panic_162; begin Unimplemented($162); end;
procedure Panic_163; begin Unimplemented($163); end;
procedure Panic_164; begin Unimplemented($164); end;
procedure Panic_165; begin Unimplemented($165); end;
procedure Panic_166; begin Unimplemented($166); end;
procedure Panic_167; begin Unimplemented($167); end;
procedure Panic_168; begin Unimplemented($168); end;
procedure Panic_169; begin Unimplemented($169); end;
procedure Panic_16A; begin Unimplemented($16A); end;
procedure Panic_16B; begin Unimplemented($16B); end;
procedure Panic_16C; begin Unimplemented($16C); end;
procedure Panic_16D; begin Unimplemented($16D); end;
procedure Panic_16E; begin Unimplemented($16E); end;

var
  KernelThunkTable: packed array[0..NUMBER_OF_THUNKS - 1] of Pointer = (
    @Panic_000,                          // $0000 (0)
    @Panic_001,                          // $0001 (1)
    @Panic_002,                          // $0002 (2)
    @Panic_003,                          // $0003 (3)
    @Panic_004,                          // $0004 (4)
    @Panic_005,                          // $0005 (5)
    @Panic_006,                          // $0006 (6)
    @Panic_007,                          // $0007 (7)
    @Panic_008,                          // $0008 (8)
    @Panic_009,                          // $0009 (9)
    @Panic_00A,                          // $000A (10)
    @Panic_00B,                          // $000B (11)
    @Panic_00C,                          // $000C (12)
    @Panic_00D,                          // $000D (13)
    @Panic_00E,                          // $000E (14)
    @Panic_00F,                          // $000F (15)
    @Panic_010,                          // $0010 (16)
    @Panic_011,                          // $0011 (17)
    @Panic_012,                          // $0012 (18)
    @Panic_013,                          // $0013 (19)
    @Panic_014,                          // $0014 (20)
    @Panic_015,                          // $0015 (21)
    @Panic_016,                          // $0016 (22)
    @Panic_017,                          // $0017 (23)
    @Panic_018,                          // $0018 (24)
    @Panic_019,                          // $0019 (25)
    @Panic_01A,                          // $001A (26)
    @Panic_01B,                          // $001B (27)
    @Panic_01C,                          // $001C (28)
    @Panic_01D,                          // $001D (29)
    @Panic_01E,                          // $001E (30)
    @Panic_01F,                          // $001F (31)
    @Panic_020,                          // $0020 (32)
    @Panic_021,                          // $0021 (33)
    @Panic_022,                          // $0022 (34)
    @Panic_023,                          // $0023 (35)
    @Panic_024,                          // $0024 (36)
    @Panic_025,                          // $0025 (37)
    @Panic_026,                          // $0026 (38)
    @Panic_027,                          // $0027 (39)
    @Panic_028,                          // $0028 (40)
    @Panic_029,                          // $0029 (41)
    @Panic_02A,                          // $002A (42)
    @Panic_02B,                          // $002B (43)
    @Panic_02C,                          // $002C (44)
    @Panic_02D,                          // $002D (45)
    @Panic_02E,                          // $002E (46)
    @Panic_02F,                          // $002F (47)
    @Panic_030,                          // $0030 (48)
    @Panic_031,                          // $0031 (49)
    @Panic_032,                          // $0032 (50)
    @Panic_033,                          // $0033 (51)
    @Panic_034,                          // $0034 (52)
    @Panic_035,                          // $0035 (53)
    @Panic_036,                          // $0036 (54)
    @Panic_037,                          // $0037 (55)
    @Panic_038,                          // $0038 (56)
    @Panic_039,                          // $0039 (57)
    @Panic_03A,                          // $003A (58)
    @Panic_03B,                          // $003B (59)
    @Panic_03C,                          // $003C (60)
    @Panic_03D,                          // $003D (61)
    @Panic_03E,                          // $003E (62)
    @Panic_03F,                          // $003F (63)
    @Panic_040,                          // $0040 (64)
    @Panic_041,                          // $0041 (65)
    @Panic_042,                          // $0042 (66)
    @Panic_043,                          // $0043 (67)
    @Panic_044,                          // $0044 (68)
    @Panic_045,                          // $0045 (69)
    @Panic_046,                          // $0046 (70)
    @Panic_047,                          // $0047 (71)
    @Panic_048,                          // $0048 (72)
    @Panic_049,                          // $0049 (73)
    @Panic_04A,                          // $004A (74)
    @Panic_04B,                          // $004B (75)
    @Panic_04C,                          // $004C (76)
    @Panic_04D,                          // $004D (77)
    @Panic_04E,                          // $004E (78)
    @Panic_04F,                          // $004F (79)
    @Panic_050,                          // $0050 (80)
    @Panic_051,                          // $0051 (81)
    @Panic_052,                          // $0052 (82)
    @Panic_053,                          // $0053 (83)
    @Panic_054,                          // $0054 (84)
    @Panic_055,                          // $0055 (85)
    @Panic_056,                          // $0056 (86)
    @Panic_057,                          // $0057 (87)
    @Panic_058,                          // $0058 (88)
    @Panic_059,                          // $0059 (89)
    @Panic_05A,                          // $005A (90)
    @Panic_05B,                          // $005B (91)
    @Panic_05C,                          // $005C (92)
    @Panic_05D,                          // $005D (93)
    @Panic_05E,                          // $005E (94)
    @Panic_05F,                          // $005F (95)
    @Panic_060,                          // $0060 (96)
    @Panic_061,                          // $0061 (97)
    @Panic_062,                          // $0062 (98)
    @Panic_063,                          // $0063 (99)
    @Panic_064,                          // $0064 (100)
    @Panic_065,                          // $0065 (101)
    @Panic_066,                          // $0066 (102)
    @Panic_067,                          // $0067 (103)
    @Panic_068,                          // $0068 (104)
    @Panic_069,                          // $0069 (105)
    @Panic_06A,                          // $006A (106)
    @Panic_06B,                          // $006B (107)
    @Panic_06C,                          // $006C (108)
    @Panic_06D,                          // $006D (109)
    @Panic_06E,                          // $006E (110)
    @Panic_06F,                          // $006F (111)
    @Panic_070,                          // $0070 (112)
    @Panic_071,                          // $0071 (113)
    @Panic_072,                          // $0072 (114)
    @Panic_073,                          // $0073 (115)
    @Panic_074,                          // $0074 (116)
    @Panic_075,                          // $0075 (117)
    @Panic_076,                          // $0076 (118)
    @Panic_077,                          // $0077 (119)
    @Panic_078,                          // $0078 (120)
    @Panic_079,                          // $0079 (121)
    @Panic_07A,                          // $007A (122)
    @Panic_07B,                          // $007B (123)
    @Panic_07C,                          // $007C (124)
    @Panic_07D,                          // $007D (125)
    @Panic_07E,                          // $007E (126)
    @Panic_07F,                          // $007F (127)
    @Panic_080,                          // $0080 (128)
    @Panic_081,                          // $0081 (129)
    @Panic_082,                          // $0082 (130)
    @Panic_083,                          // $0083 (131)
    @Panic_084,                          // $0084 (132)
    @Panic_085,                          // $0085 (133)
    @Panic_086,                          // $0086 (134)
    @Panic_087,                          // $0087 (135)
    @Panic_088,                          // $0088 (136)
    @Panic_089,                          // $0089 (137)
    @Panic_08A,                          // $008A (138)
    @Panic_08B,                          // $008B (139)
    @Panic_08C,                          // $008C (140)
    @Panic_08D,                          // $008D (141)
    @Panic_08E,                          // $008E (142)
    @Panic_08F,                          // $008F (143)
    @Panic_090,                          // $0090 (144)
    @Panic_091,                          // $0091 (145)
    @Panic_092,                          // $0092 (146)
    @Panic_093,                          // $0093 (147)
    @Panic_094,                          // $0094 (148)
    @Panic_095,                          // $0095 (149)
    @Panic_096,                          // $0096 (150)
    @Panic_097,                          // $0097 (151)
    @Panic_098,                          // $0098 (152)
    @Panic_099,                          // $0099 (153)
    @Panic_09A,                          // $009A (154)
    @Panic_09B,                          // $009B (155)
    @Panic_09C,                          // $009C (156)
    @Panic_09D,                          // $009D (157)
    @Panic_09E,                          // $009E (158)
    @Panic_09F,                          // $009F (159)
    @Panic_0A0,                          // $00A0 (160)
    @Panic_0A1,                          // $00A1 (161)
    @Panic_0A2,                          // $00A2 (162)
    @Panic_0A3,                          // $00A3 (163)
    @Panic_0A4,                          // $00A4 (164)
    @Panic_0A5,                          // $00A5 (165)
    @Panic_0A6,                          // $00A6 (166)
    @Panic_0A7,                          // $00A7 (167)
    @Panic_0A8,                          // $00A8 (168)
    @Panic_0A9,                          // $00A9 (169)
    @Panic_0AA,                          // $00AA (170)
    @Panic_0AB,                          // $00AB (171)
    @Panic_0AC,                          // $00AC (172)
    @Panic_0AD,                          // $00AD (173)
    @Panic_0AE,                          // $00AE (174)
    @Panic_0AF,                          // $00AF (175)
    @Panic_0B0,                          // $00B0 (176)
    @Panic_0B1,                          // $00B1 (177)
    @Panic_0B2,                          // $00B2 (178)
    @Panic_0B3,                          // $00B3 (179)
    @Panic_0B4,                          // $00B4 (180)
    @Panic_0B5,                          // $00B5 (181)
    @Panic_0B6,                          // $00B6 (182)
    @Panic_0B7,                          // $00B7 (183)
    @Panic_0B8,                          // $00B8 (184)
    @Panic_0B9,                          // $00B9 (185)
    @Panic_0BA,                          // $00BA (186)
    @Panic_0BB,                          // $00BB (187)
    @Panic_0BC,                          // $00BC (188)
    @Panic_0BD,                          // $00BD (189)
    @Panic_0BE,                          // $00BE (190)
    @Panic_0BF,                          // $00BF (191)
    @Panic_0C0,                          // $00C0 (192)
    @Panic_0C1,                          // $00C1 (193)
    @Panic_0C2,                          // $00C2 (194)
    @Panic_0C3,                          // $00C3 (195)
    @Panic_0C4,                          // $00C4 (196)
    @Panic_0C5,                          // $00C5 (197)
    @Panic_0C6,                          // $00C6 (198)
    @Panic_0C7,                          // $00C7 (199)
    @Panic_0C8,                          // $00C8 (200)
    @Panic_0C9,                          // $00C9 (201)
    @Panic_0CA,                          // $00CA (202)
    @Panic_0CB,                          // $00CB (203)
    @Panic_0CC,                          // $00CC (204)
    @Panic_0CD,                          // $00CD (205)
    @Panic_0CE,                          // $00CE (206)
    @Panic_0CF,                          // $00CF (207)
    @Panic_0D0,                          // $00D0 (208)
    @Panic_0D1,                          // $00D1 (209)
    @Panic_0D2,                          // $00D2 (210)
    @Panic_0D3,                          // $00D3 (211)
    @Panic_0D4,                          // $00D4 (212)
    @Panic_0D5,                          // $00D5 (213)
    @Panic_0D6,                          // $00D6 (214)
    @Panic_0D7,                          // $00D7 (215)
    @Panic_0D8,                          // $00D8 (216)
    @Panic_0D9,                          // $00D9 (217)
    @Panic_0DA,                          // $00DA (218)
    @Panic_0DB,                          // $00DB (219)
    @Panic_0DC,                          // $00DC (220)
    @Panic_0DD,                          // $00DD (221)
    @Panic_0DE,                          // $00DE (222)
    @Panic_0DF,                          // $00DF (223)
    @Panic_0E0,                          // $00E0 (224)
    @Panic_0E1,                          // $00E1 (225)
    @Panic_0E2,                          // $00E2 (226)
    @Panic_0E3,                          // $00E3 (227)
    @Panic_0E4,                          // $00E4 (228)
    @Panic_0E5,                          // $00E5 (229)
    @Panic_0E6,                          // $00E6 (230)
    @Panic_0E7,                          // $00E7 (231)
    @Panic_0E8,                          // $00E8 (232)
    @Panic_0E9,                          // $00E9 (233)
    @Panic_0EA,                          // $00EA (234)
    @Panic_0EB,                          // $00EB (235)
    @Panic_0EC,                          // $00EC (236)
    @Panic_0ED,                          // $00ED (237)
    @Panic_0EE,                          // $00EE (238)
    @Panic_0EF,                          // $00EF (239)
    @Panic_0F0,                          // $00F0 (240)
    @Panic_0F1,                          // $00F1 (241)
    @Panic_0F2,                          // $00F2 (242)
    @Panic_0F3,                          // $00F3 (243)
    @Panic_0F4,                          // $00F4 (244)
    @Panic_0F5,                          // $00F5 (245)
    @Panic_0F6,                          // $00F6 (246)
    @Panic_0F7,                          // $00F7 (247)
    @Panic_0F8,                          // $00F8 (248)
    @Panic_0F9,                          // $00F9 (249)
    @Panic_0FA,                          // $00FA (250)
    @Panic_0FB,                          // $00FB (251)
    @Panic_0FC,                          // $00FC (252)
    @Panic_0FD,                          // $00FD (253)
    @Panic_0FE,                          // $00FE (254)
    @Panic_0FF,                          // $00FF (255)
    @Panic_100,                          // $0100 (256)
    @Panic_101,                          // $0101 (257)
    @Panic_102,                          // $0102 (258)
    @Panic_103,                          // $0103 (259)
    @Panic_104,                          // $0104 (260)
    @Panic_105,                          // $0105 (261)
    @Panic_106,                          // $0106 (262)
    @Panic_107,                          // $0107 (263)
    @Panic_108,                          // $0108 (264)
    @Panic_109,                          // $0109 (265)
    @Panic_10A,                          // $010A (266)
    @Panic_10B,                          // $010B (267)
    @Panic_10C,                          // $010C (268)
    @Panic_10D,                          // $010D (269)
    @Panic_10E,                          // $010E (270)
    @Panic_10F,                          // $010F (271)
    @Panic_110,                          // $0110 (272)
    @Panic_111,                          // $0111 (273)
    @Panic_112,                          // $0112 (274)
    @Panic_113,                          // $0113 (275)
    @Panic_114,                          // $0114 (276)
    @Panic_115,                          // $0115 (277)
    @Panic_116,                          // $0116 (278)
    @Panic_117,                          // $0117 (279)
    @Panic_118,                          // $0118 (280)
    @Panic_119,                          // $0119 (281)
    @Panic_11A,                          // $011A (282)
    @Panic_11B,                          // $011B (283)
    @Panic_11C,                          // $011C (284)
    @Panic_11D,                          // $011D (285)
    @Panic_11E,                          // $011E (286)
    @Panic_11F,                          // $011F (287)
    @Panic_120,                          // $0120 (288)
    @Panic_121,                          // $0121 (289)
    @Panic_122,                          // $0122 (290)
    @Panic_123,                          // $0123 (291)
    @Panic_124,                          // $0124 (292)
    @Panic_125,                          // $0125 (293)
    @Panic_126,                          // $0126 (294)
    @Panic_127,                          // $0127 (295)
    @Panic_128,                          // $0128 (296)
    @Panic_129,                          // $0129 (297)
    @Panic_12A,                          // $012A (298)
    @Panic_12B,                          // $012B (299)
    @Panic_12C,                          // $012C (300)
    @Panic_12D,                          // $012D (301)
    @Panic_12E,                          // $012E (302)
    @Panic_12F,                          // $012F (303)
    @Panic_130,                          // $0130 (304)
    @Panic_131,                          // $0131 (305)
    @Panic_132,                          // $0132 (306)
    @Panic_133,                          // $0133 (307)
    @Panic_134,                          // $0134 (308)
    @Panic_135,                          // $0135 (309)
    @Panic_136,                          // $0136 (310)
    @Panic_137,                          // $0137 (311)
    @Panic_138,                          // $0138 (312)
    @Panic_139,                          // $0139 (313)
    @Panic_13A,                          // $013A (314)
    @Panic_13B,                          // $013B (315)
    @Panic_13C,                          // $013C (316)
    @Panic_13D,                          // $013D (317)
    @Panic_13E,                          // $013E (318)
    @Panic_13F,                          // $013F (319)
    @Panic_140,                          // $0140 (320)
    @Panic_141,                          // $0141 (321)
    @Panic_142,                          // $0142 (322)
    @Panic_143,                          // $0143 (323)
    @Panic_144,                          // $0144 (324)
    @Panic_145,                          // $0145 (325)
    @Panic_146,                          // $0146 (326)
    @Panic_147,                          // $0147 (327)
    @Panic_148,                          // $0148 (328)
    @Panic_149,                          // $0149 (329)
    @Panic_14A,                          // $014A (330)
    @Panic_14B,                          // $014B (331)
    @Panic_14C,                          // $014C (332)
    @Panic_14D,                          // $014D (333)
    @Panic_14E,                          // $014E (334)
    @Panic_14F,                          // $014F (335)
    @Panic_150,                          // $0150 (336)
    @Panic_151,                          // $0151 (337)
    @Panic_152,                          // $0152 (338)
    @Panic_153,                          // $0153 (339)
    @Panic_154,                          // $0154 (340)
    @Panic_155,                          // $0155 (341)
    @Panic_156,                          // $0156 (342)
    @Panic_157,                          // $0157 (343)
    @Panic_158,                          // $0158 (344)
    @Panic_159,                          // $0159 (345)
    @Panic_15A,                          // $015A (346)
    @Panic_15B,                          // $015B (347)
    @Panic_15C,                          // $015C (348)
    @Panic_15D,                          // $015D (349)
    @Panic_15E,                          // $015E (350)
    @Panic_15F,                          // $015F (351)
    @Panic_160,                          // $0160 (352)
    @Panic_161,                          // $0161 (353)
    @Panic_162,                          // $0162 (354)
    @Panic_163,                          // $0163 (355)
    @Panic_164,                          // $0164 (356)
    @Panic_165,                          // $0165 (357)
    @Panic_166,                          // $0166 (358)
    @Panic_167,                          // $0167 (359)
    @Panic_168,                          // $0168 (360)
    @Panic_169,                          // $0169 (361)
    @Panic_16A,                          // $016A (362)
    @Panic_16B,                          // $016B (363)
    @Panic_16C,                          // $016C (364)
    @Panic_16D,                          // $016D (365)
    @Panic_16E                           // $016E (366)
  );

function CxbxKrnl_KernelThunkTable: PThunkTable; stdcall;
begin
  Result := @KernelThunkTable;
end;

end.
