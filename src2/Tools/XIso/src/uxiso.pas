{
   xISO
   Copyright 1984, 1986, 1989, 1992, 2000, 2001, 2002
   Free Software Foundation, Inc.

   This file is part of xISO, made it by Yursoft.com

   xISO is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Bison is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Bison; see the file COPYING.  If not, write to the Free
   Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.
}


unit uxiso;

interface

uses
  // Delphi
  Windows, Classes, SysUtils, Dialogs,
  // Dxbx
  CDROM;

const
  XBOX_MEDIA = 'MICROSOFT*XBOX*MEDIA';
  CD_SECTOR: Int64 = 32;
  IMG_SECTOR: Int64 = 32;
  DVD_SECTOR: Int64 = 198176;
  SECTOR_SIZE = 2048;

  XF_READONLY = $01;
  XF_HIDDEN = $02;
  XF_SYSTEM = $04;
  XF_DIRECTORY = $10;
  XF_FILE = $20;
  XF_NORMAL = $80;

type

{$A-}
  TxVD = record
    IDIn: array[0..19] of AnsiChar;
    RootDir: Integer;
    RootSize: Integer;
    DateTime: FILETIME;
    Unused: array[0..1991] of Byte;
    IDOut: array[0..19] of AnsiChar;
  end;
{$A+}

  PxFile = ^TxFile;
  TxFile = record
    pLeft: Word;
    pRight: Word;
    SectorIn: Integer;
    Size: Integer;
    Attributes: Byte;
    NameLength: Byte;
    Name: array[0..255] of AnsiChar;
    ParentDir: Integer;
    ChildDir: Integer;
  end;

  TxTablaDirectorio = record
  end;

  TxISO = record
    List: TList;
    xVD: TxVD;
    NrFiles: Integer;
    NrFolders: Integer;
    ISOSize: Int64;
  end;

var
  xISO: TxISO;
  CDDVD_VD: Int64;

function OpenXISO(Image: string): Boolean;
function ReadXDVD(HA, SCSI, LUN: Byte; CDUnit: TCDROM): Boolean;
procedure ExtractFile(aFile: string; grabar: string; Sector, Size: Int64);
procedure ExtractFileXDVD(HA, SCSI, LUN: Byte; CDUnit: TCDROM; grabar: string; Sector, Size: Int64);

implementation

{ SECCION DE LECTURA DESDE Image }

procedure ExtractFile(aFile: string; grabar: string; Sector, Size: Int64);
const
  BufferSize = 1024 * 128; // was 2048
var
  fxiso, OutputStream: TFileStream;
  EndPos: Int64;
  buffer: array of Byte;
begin
  SetLength(Buffer, BufferSize);
  Sector := Sector + IMG_SECTOR - 32;
  fxiso := TFileStream.Create(aFile, fmOpenRead or fmShareDenyNone);
  OutputStream := TFileStream.Create(grabar, fmCreate or fmShareDenyNone);
  fxiso.Seek(Sector * SECTOR_SIZE, soBeginning);
  EndPos := ((Size div BufferSize) * BufferSize) + Sector * SECTOR_SIZE;

  while (fxiso.Position < EndPos) do
  begin
    fxiso.Read(buffer[0], BufferSize);
    OutputStream.Write(buffer[0], BufferSize);
  end;

  if (Size mod BufferSize) <> 0 then
  begin
    fxiso.Read(buffer[0], Size mod BufferSize);
    OutputStream.Write(buffer[0], Size mod BufferSize);
  end;

  OutputStream.Free;
  fxiso.Free;
end;

// FALTA POR HACER
// UNFINISHED

procedure EditEntry(Sector: Int64; TamSector: Int64; var Number: Int64; var Entry: TxFile; aFile: TFileStream);
var
  MemBuf: TMemoryStream;
  xFile: TxFile;
  Offset: Int64;
  Total, j: Integer;
begin
  if Sector = $FFFFFFFFFFFFFFFF then
    Exit;

  offset := Sector * SECTOR_SIZE;
  aFile.Seek(offset, soBeginning);

  MemBuf := TMemoryStream.Create();
  try
    MemBuf.CopyFrom(aFile, TamSector);
  except
  end;
  
  MemBuf.Seek(0, soBeginning);

  Total := TamSector div 2048;
  if (TamSector mod 2048) <> 0 then
    Total := Total + 1;

  for j := 0 to Total - 1 do
  begin
    MemBuf.Seek(j * 2048, soBeginning);
    while not (MemBuf.Position >= (j + 1) * 2048) do
    begin
      FillChar(xFile, SizeOf(xFile), $00);

      xFile.pLeft := $FFFF;
      MemBuf.Read(xFile, 14);
      MemBuf.Read(xFile.Name, xFile.NameLength);

      if (xFile.pLeft = $FFFF) then
        Break;

      if  (Trim(string(PAnsiChar(@xFile.Name))) = Trim(string(PAnsiChar(@Entry.Name))))
      and (xFile.Size = Entry.Size)
      and (xFile.SectorIn = Entry.SectorIn)
      and (xFile.pRight = Entry.pRight) then
      begin
        // todo
      end;

      if ((xFile.Attributes and XF_DIRECTORY) = XF_DIRECTORY) then
      begin
        Number := Number + 1;
        if (xFile.Size <> 0) and (xFile.SectorIn <> 0) then
          EditEntry(xFile.SectorIn + IMG_SECTOR - 32, xFile.Size, {var}Number, {var}Entry, aFile);
      end;

      if ((MemBuf.Position mod 4) <> 0) then
        MemBuf.Seek(MemBuf.Position + 4 - (MemBuf.Position mod 4), soBeginning);
    end;
  end;

  MemBuf.Free;
end;

procedure ReadFileTable(Sector: Int64; TamSector: Int64; var Number: Int64; aFile: TFileStream);
var
  MemBuf: TMemoryStream;
  xFile: PxFile;
  i: Int64;
  Offset: Int64;
  Total, j: Integer;
begin
  if Sector = $FFFFFFFFFFFFFFFF then
    Exit;

  i := Number;
  offset := Sector * SECTOR_SIZE;
  aFile.Seek(offset, soBeginning);

  MemBuf := TMemoryStream.Create();
  try
    MemBuf.CopyFrom(aFile, TamSector);
  except end;
  MemBuf.Seek(0, soBeginning);

  Total := TamSector div 2048;
  if TamSector mod 2048 <> 0 then Total := Total + 1;

  for j := 0 to Total - 1 do
  begin
    MemBuf.Seek(j * 2048, soBeginning);
    while not (MemBuf.Position >= (j + 1) * 2048) do
    begin
      New(xFile);
      FillChar(xFile^, SizeOf(xFile^), $00);

      xFile.pLeft := $FFFF;

      MemBuf.Read(xFile^, 14);
      MemBuf.Read(xFile.Name, xFile.NameLength);
      xFile.ParentDir := i;

      if (xFile.pLeft = $FFFF) then
        Break;

      xISO.List.Add(xFile);

      if ((xFile.Attributes and XF_DIRECTORY) = XF_DIRECTORY) then
      begin
        xISO.NrFolders := xISO.NrFolders + 1;
        Number := Number + 1;
        if (xFile.Size <> 0) and (xFile.SectorIn <> 0) then
        begin
          xFile.ChildDir := Number;
          ReadFileTable(xFile.SectorIn + IMG_SECTOR - 32, xFile.Size, Number, aFile);
        end
        else
          xFile.ChildDir := -1;
      end
      else
      begin
        xISO.ISOSize := xISO.ISOSize + xFile.Size;
        xISO.NrFiles := xISO.NrFiles + 1;
      end;

      if ((MemBuf.Position mod 4) <> 0) then
        MemBuf.Seek(MemBuf.Position + 4 - (MemBuf.Position mod 4), soBeginning);
    end;
  end;

  MemBuf.Free;
end;

function OpenXISO(Image: string): Boolean;
var
  aFile: TFileStream;
  NumIni: Int64;
begin
  NumIni := 0;
  aFile := TFileStream.Create(Image, fmOpenRead or fmShareDenyNone);
  with aFile do
  begin
    Seek(IMG_SECTOR * SECTOR_SIZE, soBeginning);
    Read(xISO.xVD, SizeOf(xISO.xVD));

    if (xISO.xVD.IDIn <> XBOX_MEDIA) or
      (xISO.xVD.IDOut <> XBOX_MEDIA) then
    begin
      Result := False;
      aFile.Free;
      Exit;
    end;

    if xISO.List = nil then
      xISO.List := TList.Create
    else
      xISO.List.Clear;

    xISO.NrFiles := 0;
    xISO.NrFolders := 0;
    xISO.ISOSize := 0;
    ReadFileTable(xISO.xVD.RootDir + IMG_SECTOR - 32, xISO.xVD.RootSize, NumIni, aFile);
  end;
  
  Result := True;
  aFile.Free;
end;

{ FIN SECCION DE LECTURA DESDE Image }

{ SECCION DE LECTURA DESDE DVD }

procedure ExtractFileXDVD(HA, SCSI, LUN: Byte; CDUnit: TCDROM; grabar: string; Sector, Size: Int64);
var
  OutputStream: TFileStream;
  RoundedBytes, RestBytes, a: Int64;
  buffer: array[0..32767] of Byte;
  r: Integer;

  function Size2Sector(Size: Int64): Int64;
  begin
    Result := Size div SECTOR_SIZE;
    if (Size mod SECTOR_SIZE) <> 0 then
      Result := Result + 1;
  end;

begin
  Sector := Sector + CDDVD_VD - 32;
  OutputStream := TFileStream.Create(grabar, fmCreate or fmShareDenyNone);

  RoundedBytes := (Size2Sector(Size) div $10) * $10;
  RestBytes := (Size2Sector(Size) mod $10);

  a := Sector;
  if (RoundedBytes <> 0) then
    while (a < Sector + RoundedBytes) do
    begin
      if not CDUnit.Read12(HA, SCSI, LUN, a, a + $10, @Buffer) then
      begin
        MessageBox(0, 'Error de lectura.', 'Error', MB_OK or MB_ICONERROR);
        OutputStream.Free;
        Exit;
      end;
      
      OutputStream.Write(buffer, SizeOf(buffer));
      a := a + $10;
    end;

  if (RestBytes <> 0) then
  begin
    if not CDUnit.Read12(HA, SCSI, LUN, a, a + RestBytes, @Buffer) then
      Exit;
      
    r := (SECTOR_SIZE - (Size mod SECTOR_SIZE)) mod 2048;
    OutputStream.Write(buffer, RestBytes * SECTOR_SIZE - r);
  end;

  OutputStream.Free;
end;

procedure ReadFileTableXDVD(HA, SCSI, LUN: Byte; CDUnit: TCDROM; Sector: Int64; TamSector: Int64; var Number: Int64);
type
  PByteBuffer = ^TByteBuffer;
  TByteBuffer = array[0..65535] of Byte;
var
  MemBuf: TMemoryStream;
  xFile: PxFile;
  i: Int64;
  ReadSectors: Integer;
  Buffer: PByteBuffer;
  Total, j: Integer;
begin
  if Sector = $FFFFFFFFFFFFFFFF then
    Exit;

  i := Number;
  // Calculamos el Number de sectores a leer
  ReadSectors := TamSector div SECTOR_SIZE;
  if (TamSector mod SECTOR_SIZE) <> 0 then
    ReadSectors := ReadSectors + 1;

  New(Buffer);

  // Leemos el sector
  if not CDUnit.Read12(HA, SCSI, LUN, Sector, Sector + ReadSectors, Buffer) then
    Exit;

  MemBuf := TMemoryStream.Create();
  try
    MemBuf.Write(Buffer^, ReadSectors * SECTOR_SIZE);
  except
  end;

  MemBuf.Seek(0, soBeginning);

  Total := TamSector div 2048;
  if TamSector mod 2048 <> 0 then
    Total := Total + 1;

  for j := 0 to Total - 1 do
  begin
    MemBuf.Seek(j * 2048, soBeginning);
    while not (MemBuf.Position >= (j + 1) * 2048) do //MemBuf.Size) do
    begin
      New(xFile);
      FillChar(xFile^, SizeOf(xFile^), $00);
      xFile.pLeft := $FFFF;
      MemBuf.Read(xFile^, 14);
      MemBuf.Read(xFile.Name, xFile.NameLength);
      xFile.ParentDir := i;

      if (xFile.pLeft = $FFFF) then
        Break;
          
      xISO.List.Add(xFile);

      if ((xFile.Attributes and XF_DIRECTORY) = XF_DIRECTORY) then
      begin
        xISO.NrFolders := xISO.NrFolders + 1;
        Number := Number + 1;

                 // Si el Directorio NO esta Vacio entonces lo procesamos
        if (xFile.Size <> 0) and (xFile.SectorIn <> 0) then
        begin
          xFile.ChildDir := Number;
          ReadFileTableXDVD(HA, SCSI, LUN, CDUnit, xFile.SectorIn + CDDVD_VD - 32, xFile.Size, Number);
        end
        else
          xFile.ChildDir := -1;
      end
      else
      begin
        xISO.ISOSize := xISO.ISOSize + xFile.Size;
        xISO.NrFiles := xISO.NrFiles + 1;
      end;

              // Avanzamos hasta la posicion DWORD siguiente
      if ((MemBuf.Position mod 4) <> 0) then
        MemBuf.Seek(MemBuf.Position + 4 - (MemBuf.Position mod 4), soBeginning);
    end;
  end;

  MemBuf.Free;
  Dispose(Buffer);
end;

function ReadXDVD(HA, SCSI, LUN: Byte; CDUnit: TCDROM): Boolean;
var
  Mem: TMemoryStream;
  NumIni: Int64;
  Buffer: array[0..2047] of Byte;
  Alternative: Boolean;
begin
  NumIni := 0;
  Result := False;
  Alternative := False;

  CDDVD_VD := DVD_SECTOR;
  if CDUnit.Read12(HA, SCSI, LUN, CDDVD_VD, CDDVD_VD + 1, @Buffer) then
  begin
    Mem := TMemoryStream.Create;
    Mem.Write(Buffer, SizeOf(Buffer));
    Mem.Seek(0, soFromBeginning);
    Mem.Read(xISO.xVD, SizeOf(xISO.xVD));
    Mem.Free;
    if (xISO.xVD.IDIn <> XBOX_MEDIA) or (xISO.xVD.IDOut <> XBOX_MEDIA) then
      Alternative := True;
  end
  else
    Alternative := True;

  if Alternative then
  begin
    CDDVD_VD := CD_SECTOR;
    if not CDUnit.Read12(HA, SCSI, LUN, CDDVD_VD, CDDVD_VD + 1, @Buffer) then
      Exit;
      
    Mem := TMemoryStream.Create;
    try
      Mem.Write(Buffer, SizeOf(Buffer));
      Mem.Seek(0, soBeginning);
      Mem.Read(xISO.xVD, SizeOf(xISO.xVD));
    finally
      Mem.Free;
    end;

    if (xISO.xVD.IDIn <> XBOX_MEDIA) or (xISO.xVD.IDOut <> XBOX_MEDIA) then
      Exit;
  end;

  if xISO.List = nil then
    xISO.List := TList.Create
  else
    xISO.List.Clear;

  xISO.NrFiles := 0;
  xISO.NrFolders := 0;
  xISO.ISOSize := 0;
  ReadFileTableXDVD(HA, SCSI, LUN, CDUnit, xISO.xVD.RootDir + CDDVD_VD - 32, xISO.xVD.RootSize, NumIni);

  Result := True;
end;

{ FIN SECCION DE LECTURA DESDE DVD }

end.
