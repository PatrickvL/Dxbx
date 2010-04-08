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
  Windows, // TImageDosHeader, TImageNtHeaders, TImageSectionHeader
  SysUtils,
  Classes,
  Dialogs, // MessageDlg
  // Dxbx
  uTypes,
  uLog;

type
  // This class captures all parts of a Windows PE file :
  TExe = class(TObject)
  public
    // DOS .EXE header :
    DOSHeader: TImageDosHeader;
    // DOS code block :
    DOSCode: TVarByteArray;
    // PE headers (combining Signature, IMAGE_NT_HEADERS and IMAGE_OPTIONAL_HEADER all into one) :
    NtHeaders: TImageNtHeaders;
    // Array of all section headers :
    SectionHeaders: array of TImageSectionHeader;
    // Array of all section contents :
    SectionContents: array of TRawSection;
  public
    procedure ConstructorInit;
    constructor Create(x_szFileName: string);

    function doExport(const x_szExeFileName: string): Boolean;
    function GetAddr(x_dwVirtualAddress: DWord): PByte;
  end;

const
  DOS_PAGESIZE = 512; // Used for TImageDosHeader.e_cblp and TImageDosHeader.e_cp

implementation

{ TExe }

procedure TExe.ConstructorInit;
begin
  SectionHeaders := nil;
  SectionContents := nil;
end; // TExe.ConstructorInit

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

  if DOSHeader.e_magic = IMAGE_DOS_SIGNATURE then
  try
    ExeFile.Read(DOSHeader.e_cblp, SizeOf(DOSHeader) - SizeOf(DosHeader.e_magic));
    WriteLog('Exe: Reading DOS header... OK');
  except
    WriteLog('Unexpected read error while reading DOS header');
  end
  else
    WriteLog('Exe: Reading DOS header... OK');


  // Read DOS code block :
  SetLength(DOSCode, DOSHeader._lfanew - SizeOf(TImageDosHeader));
  try
    ExeFile.Read(DOSCode[0], Length(DOSCode));
    WriteLog('Exe: Reading DOS code... OK');
  except
    WriteLog('Unexpected read error while reading DOS code');
  end;

  
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
        SetLength(SectionHeaders, NtHeaders.FileHeader.NumberOfSections);

        printf('Exe::Exe: Reading Section Headers...');

        for(uint32 v=0;v<NtHeaders.FileHeader.NumberOfSections;v++)
        {
            printf('Exe::Exe: Reading Section Header 0x%.4X...', v);

            if (fread(&SectionHeaders[v], SizeOf(TImageSectionHeader), 1, ExeFile) != 1) then
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

        SetLength(SectionContents, NtHeaders.FileHeader.NumberOfSections);

        for(uint32 v=0;v<NtHeaders.FileHeader.NumberOfSections;v++)
        {
            printf('Exe::Exe: Reading Section 0x%.4X...', v);

            uint32 raw_size = SectionHeaders[v].m_sizeof_raw;
            uint32 raw_addr = SectionHeaders[v].m_raw_addr;

            SetLength(SectionContents[v], raw_size);

            memset(SectionContents[v], 0, raw_size);

            if (raw_size == 0) then
            {
                printf('OK');
                continue;
            }

            // ******************************************************************
            // * read current section from file (if raw_size > 0)
            // ******************************************************************
            {
                fseek(ExeFile, raw_addr, SEEK_SET);

                if (fread(SectionContents[v], raw_size, 1, ExeFile) != 1) then
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
    Result := _Write(DOSHeader, SizeOf(DOSHeader), 'DOS header');
    if not Result then
      Exit;

    // write DOS code
    Result := _Write(DOSCode[0], Length(DOSCode), 'DOS code');
    if not Result then
      Exit;

    // write nt headers
    Result := _Write(NtHeaders, SizeOf(NtHeaders), 'NT headers');
    if not Result then
      Exit;

    // write section headers
    for i := 0 to NtHeaders.FileHeader.NumberOfSections - 1 do
    begin
      Result := _Write(SectionHeaders[i], SizeOf(SectionHeaders[i]), 'PE section header 0x' + IntToHex(i, 4));
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
      RawSize := SectionHeaders[i].SizeOfRawData;
      RawAddr := SectionHeaders[i].PointerToRawData;

      ExeFile.Seek(RawAddr, soFromBeginning);

      if RawSize = 0 then
        Result := False
      else
        _Write(Pointer(SectionContents[i])^, RawSize, 'PE section 0x' + IntToHex(i, 4));
    end;

    WriteLog('Export: Writing Sections...OK');

  finally
    FreeAndNil({var}ExeFile);
  end;
end; // TExe.doExport

function TExe.GetAddr(x_dwVirtualAddress: DWord): PByte;
var
  v: Integer;
  virt_addr: DWord;
  virt_size: DWord;
begin
  for v := 0 to NtHeaders.FileHeader.NumberOfSections - 1 do
  begin
    virt_addr := SectionHeaders[v].VirtualAddress;
    virt_size := SectionHeaders[v].Misc.VirtualSize;

    if (x_dwVirtualAddress >= virt_addr) and (x_dwVirtualAddress < (virt_addr + virt_size)) then
    begin
      Result := @(SectionContents[v][x_dwVirtualAddress - virt_addr]);
      Exit;
    end;
  end;

  Result := nil;
end;

end.
