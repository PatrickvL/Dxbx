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
unit uExe;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  Dialogs, // MessageDlg
  Windows, // TImageDosHeader, TImageNtHeaders, TImageSectionHeader
  Classes,
  SysUtils,
  // Dxbx
  uTypes,
  uLog;

type
  DOSStub = array[0..183] of Byte;
  EndFilling1 = array[0..0] of Byte;
  EndFilling2 = array[0..0] of Byte;

  TExe = class(TObject)
  public
    DOSHeader: TImageDosHeader;
    NtHeaders: TImageNtHeaders;

    m_SectionHeader: array of TImageSectionHeader;

    m_bzSection: array of TVarByteArray;

    constructor Create(x_szFileName: string);
    function GetAddr(x_dwVirtualAddress: DWord): PByte;
    function doExport(const x_szExeFileName: string): Boolean;
    procedure ConstructorInit;
  end;

const
  bzDOSStub: DOSStub = ($4D, $5A, $90, $00, $03, $00, $00, $00,
    $04, $00, $00, $00, $FF, $FF, $00, $00,
    $B8, $00, $00, $00, $00, $00, $00, $00,
    $40, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $B8, $00, $00, $00,
    $0E, $1F, $BA, $0E, $00, $B4, $09, $CD,
    $21, $B8, $01, $4C, $CD, $21, $54, $68,
    $69, $73, $20, $70, $72, $6F, $67, $72,
    $61, $6D, $20, $63, $61, $6E, $6E, $6F,
    $74, $20, $62, $65, $20, $72, $75, $6E,
    $20, $69, $6E, $20, $44, $4F, $53, $20,
    $6D, $6F, $64, $65, $2E, $0D, $0D, $0A,
    $24, $00, $00, $00, $00, $00, $00, $00,
    $85, $E3, $B8, $DB, $C1, $82, $D6, $88,
    $C1, $82, $D6, $88, $C1, $82, $D6, $88,
    $C1, $82, $D7, $88, $C3, $82, $D6, $88,
    $3E, $A2, $D2, $88, $C2, $82, $D6, $88,
    $95, $A1, $E7, $88, $C0, $82, $D6, $88,
    $52, $69, $63, $68, $C1, $82, $D6, $88,
    $00, $00, $00, $00, $00, $00, $00, $00);

  bzEndFilling1: EndFilling1 = ($CC);
  bzEndFilling2: EndFilling2 = ($CD);


implementation

{ TExe }

//------------------------------------------------------------------------------

procedure TExe.ConstructorInit;
begin
  m_SectionHeader := nil;
  m_bzSection := nil;
end; // TExe.ConstructorInit

//------------------------------------------------------------------------------

constructor TExe.Create(x_szFileName: string);
var
  ExeFile: TFileStream;
begin
  ConstructorInit();

  ExeFile := TFileStream.Create(x_szFileName, fmOpenRead);
  try
    // verify exe file was opened
    if ExeFile.Size < 0 then
    begin
      WriteLog('Could not open Exe file.');
      Exit;
    end;

    WriteLog('Exe: Opening Exe file... Ok');

  // ignore dos stub (if it exists)
  try
    ExeFile.Read(DOSHeader.e_magic, SizeOf(DosHeader.e_magic));
  except
    MessageDlg('Unexpected read error while reading magic number', mtError, [mbOk], 0);
  end;

  if StrLComp(@DOSHeader.e_magic, 'MZ', 2) = 0 then
  try
    ExeFile.Read(DOSHeader.e_magic, SizeOf(DOSHeader) - SizeOf(DosHeader.e_magic));
    WriteLog('Exe: Reading DOS stub... OK');
  except
    WriteLog('Unexpected read error while reading DOS stub');
  end
  else
    WriteLog('Exe: Reading DOS stub... OK');


  // read pe header

    {
        printf('Exe::Exe: Reading PE header...');

        if (fread(&NtHeaders.FileHeader, SizeOf(NtHeaders.FileHeader), 1, ExeFile) != 1) then
        {
            SetError('Unexpected read error while reading PE header', True);
            goto cleanup;
        }



(*        if (NtHeaders.FileHeader.m_magic != *(uint32*)(*'PE\0\0') then
        {
            SetError('Invalid file (could not locate PE header)', True);
            goto cleanup;
        }

  (*      printf('OK');
    }



    // ******************************************************************
    // * read optional header
    // ******************************************************************
    {
        printf('Exe::Exe: Reading Optional Header...');

        if (fread(&m_OptionalHeader, SizeOf(m_OptionalHeader), 1, ExeFile) != 1) then
        {
            SetError('Unexpected read error while reading PE optional header', True);
            goto cleanup;
        }

        if (m_OptionalHeader.m_magic != 0x010B) then
        {
            SetError('Invalid file (could not locate PE optional header)', True);
            goto cleanup;
        }

         printf('OK');
    }

    // ******************************************************************
    // * read section headers
    // ******************************************************************
    {
        m_SectionHeader = new TImageSectionHeader[NtHeaders.FileHeader.NumberOfSections];

        printf('Exe::Exe: Reading Section Headers...');

        for(uint32 v=0;v<NtHeaders.FileHeader.NumberOfSections;v++)
        {
            printf('Exe::Exe: Reading Section Header 0x%.4X...', v);

            if (fread(&m_SectionHeader[v], SizeOf(TImageSectionHeader), 1, ExeFile) != 1) then
            {
                char buffer[255];
                sprintf(buffer, 'Could not read PE section header %d (%Xh)', v, v);
                SetError(buffer, True);
                goto cleanup;
            }

            printf('OK', v);
        }
    }

    // ******************************************************************
    // * read sections
    // ******************************************************************
    {
        printf('Exe::Exe: Reading Sections...');

        m_bzSection = new uint08*[NtHeaders.FileHeader.NumberOfSections];

        for(uint32 v=0;v<NtHeaders.FileHeader.NumberOfSections;v++)
        {
            printf('Exe::Exe: Reading Section 0x%.4X...', v);

            uint32 raw_size = m_SectionHeader[v].m_sizeof_raw;
            uint32 raw_addr = m_SectionHeader[v].m_raw_addr;

            m_bzSection[v] = new uint08[raw_size];

            memset(m_bzSection[v], 0, raw_size);

            if (raw_size == 0) then
            {
                printf('OK');
                Continue;
            }

            // ******************************************************************
            // * read current section from file (if raw_size > 0)
            // ******************************************************************
            {
                fseek(ExeFile, raw_addr, SEEK_SET);

                if (fread(m_bzSection[v], raw_size, 1, ExeFile) != 1) then
                {
                    char buffer[255];
                    sprintf(buffer, 'Could not read PE section %d (%Xh)', v, v);
                    SetError(buffer, True);
                    goto cleanup;
                }
            }

            printf('OK');
        }
    }

    printf('Exe::Exe: Exe was successfully opened.', x_szFileName);

cleanup:

    if (GetError() != 0) then
    {
        printf('FAILED!');
        printf('Exe::Exe: ERROR -> %s', GetError());
    }

    fclose(ExeFile);
        *)
  finally
    FreeAndNil({var}ExeFile);
  end;
end;

function TExe.doExport(const x_szExeFileName: string): Boolean;
var
  ExeFile: TFileStream;
  i: Integer;
  RawSize, RawAddr: DWord;

  function _Write(const aBlock; aSize: Integer; aDescription: string): Boolean;
  begin
    try
      ExeFile.Write(aBlock, aSize);
    except
      Result := False;
      MessageDlg('Could not write ' + aDescription, mtError, [mbOk], 0);
      WriteLog('Export: Could not write ' + aDescription);
      Exit;
    end;

    WriteLog('Export: Writing ' + aDescription + '...OK');
    Result := True;
  end;

begin
  try
    ExeFile := TFileStream.Create(x_szExeFileName, fmCreate);
  except
    Result := False;
    WriteLog('Export: Could not open .exe file.');
    Exit;
  end;

  try
    WriteLog('Export: Opening Exe file...OK');

    // write DOS header
    Result := _Write(bzDosStub, SizeOf(bzDosStub), 'DOS stub');
    if not Result then
      Exit;

    // write nt headers
    Result := _Write(NtHeaders, SizeOf(NtHeaders), 'NT headers');
    if not Result then
      Exit;

    // write section headers
    for i := 0 to NtHeaders.FileHeader.NumberOfSections - 1 do
    begin
      Result := _Write(m_Sectionheader[i], SizeOf(m_SectionHeader[i]), 'PE section header 0x' + IntToHex(i, 4));
      if not Result then
        Exit;
    end;

    WriteLog('Export: Writing Section Headers...OK');

    // write sections
    for i := 0 to NtHeaders.FileHeader.NumberOfSections - 1 do
    begin

      //Debug info of turok from cxbx
      //NtHeaders.FileHeader.NumberOfSections = 13
      //v = 0   RawwSize = 1578272  RawAddr = 4096
      //v = 1   RawwSize = 76960    RawAddr = 1582368
      //v = 2   RawwSize = 1344     RawAddr = 1659328
      //v = 3   RawwSize = 36544    RawAddr = 1660672
      //v = 4   RawwSize = 121280   RawAddr = 1697216
      //v = 5   RawwSize = 160448   RawAddr = 1818496
      //v = 6   RawwSize = 29024    RawAddr = 1978944
      //v = 7   RawwSize = 246624   RawAddr = 2007968
      //v = 8   RawwSize = 140192   RawAddr = 2254592
      //v = 9   RawwSize =  29056   RawAddr = 2394784
      //v = 10  RawwSize = 10240    RawAddr = 2423840
      //v = 11  RawwSize = 128      RawAddr = 2434080
      //v = 12  RawwSize = 69632    RawAddr = 2434208
      //v = 13  RawwSize = 577      RawAddr = 305
      RawSize := m_SectionHeader[i].SizeOfRawData;
      RawAddr := m_SectionHeader[i].PointerToRawData;

      ExeFile.Seek(RawAddr, soFromBeginning);

      if RawSize = 0 then
        Result := False
      else
        _Write(Pointer(m_bzSection[i])^, RawSize, 'PE section 0x' + IntToHex(i, 4));
    end;

    WriteLog('Export: Writing Sections...OK');

(*
    ExeFile.Position := 2437340;
    for i := 0 to 963 do
     ExeFile.Write(bzEndFilling1, Length(bzEndFilling1));

    for i := 0 to 65535 do
      ExeFile.Write(bzEndFilling2, Length(bzEndFilling2));
*)
  finally
    FreeAndNil({var}ExeFile);
  end;
end; // TExe.doExport

//------------------------------------------------------------------------------

function TExe.GetAddr(x_dwVirtualAddress: DWord): PByte;
var
  v: Integer;
  virt_addr: DWord;
  virt_size: DWord;
begin
  for v := 0 to NtHeaders.FileHeader.NumberOfSections - 1 do
  begin
    virt_addr := m_SectionHeader[v].VirtualAddress;
    virt_size := m_SectionHeader[v].Misc.VirtualSize;

    if (x_dwVirtualAddress >= virt_addr) and (x_dwVirtualAddress < (virt_addr + virt_size)) then
    begin
      Result := @(m_bzSection[v][x_dwVirtualAddress - virt_addr]);
      Exit;
    end;
  end;

  Result := nil;
end;

//------------------------------------------------------------------------------

end.
