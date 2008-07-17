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
unit uPatternCodeGenerator;

interface

uses
  // Delphi
  SysUtils,
  Classes,
  // Dxbx
  uXboxLibraryUtils,
  uPatternScanner;
  
procedure PatternCodeGenerator_Main;

implementation

function FindFiles(const aFolder, aFileMask: TFileName; aFileNames: TStrings): Integer;
var
  Status: Integer;
  SearchRec: TSearchRec;
begin
  with aFileNames do
  begin
    BeginUpdate;
    try
      Clear;
      Status := FindFirst(IncludeTrailingPathDelimiter(aFolder) + aFileMask, faAnyFile, SearchRec);
      while Status = 0 do
      begin
        if (SearchRec.Attr and faDirectory) = 0 then
          Add(IncludeTrailingPathDelimiter(aFolder) + SearchRec.Name);

        Status := FindNext(SearchRec);
      end;

      FindClose(SearchRec);
    finally
      EndUpdate;
    end;

    Result := Count;
  end;
end;

function GeneratePatternUnit(const aPatternFiles: TStrings): Integer;

  function _ConvertPatternListToConstCode(const aPatternList: TList; const aPatternArrayName: string): string;
  var
    Pattern: PXboxLibraryFunction;
    i, j: Integer;
  begin
    with TStringStream.Create('') do
    try
      WriteString(#13#10);
      WriteString(aPatternArrayName);
      WriteString(': array [0..');
      WriteString(IntToStr(aPatternList.Count - 1));
      WriteString('] of RXboxLibraryFunction = (');
      for i := 1 to aPatternList.Count do
      begin
        Pattern := PXboxLibraryFunction(aPatternList.List^[i - 1]);
        WriteString(#13#10'  (Name:''');
        WriteString(Pattern.Name);
        WriteString(''';Pattern:(BytesToUseMask:');
        WriteString(IntToStr(Pattern.Pattern.BytesToUseMask));
        WriteString(';Bytes:(');
        for j := 0 to Length(Pattern.Pattern.Bytes) - 1 do
        begin
          if j > 0 then
            WriteString(',');
          WriteString(IntToStr(Pattern.Pattern.Bytes[j]));
        end;
        WriteString('));CRCLength:');
        WriteString(IntToStr(Pattern.CRCLength));
        WriteString(';CRCValue:');
        WriteString(IntToStr(Pattern.CRCValue));
        WriteString(';TotalLength:');
        WriteString(IntToStr(Pattern.TotalLength));
        WriteString(')');
        if i < aPatternList.Count then
          WriteString(',');
      end;
      WriteString(#13#10'  );');
    finally
      Result := DataString;
      Free;
    end;
  end;

const
  OutputUnitName = 'uPatterns'; // TODO : Make this configurable?
var
  UnitLines: TStringList;
  Input: TMemoryStream;
  i: Integer;
  PatternFilePath: string;
  PatternArrayName, PatternArraysInfo: string;
begin
  UnitLines := TStringList.Create;
  Input := TMemoryStream.Create;
  TStringList.Create;
  try
    UnitLines.Add('// This unit is auto-generated. Do not edit!');
    UnitLines.Add('unit ' + OutputUnitName + ';interface uses uXboxLibraryUtils;const');

    PatternArraysInfo := #13#10'AllPatternArrays: array[0..' + IntToStr(aPatternFiles.Count - 1) + '] of RXboxLibraryInfo = (';

    // Load and convert all pattern files :
    for i := 0 to aPatternFiles.Count - 1 do
    begin
      // Load pattern file :
      PatternFilePath := aPatternFiles[i];
      Input.LoadFromFile(PatternFilePath);

      // Parse patterns in this file :
      with ConvertPatternsToSortedList(Input.Memory) do
      try
        PatternArrayName := 'Patterns_' + ChangeFileExt(ExtractFileName(PatternFilePath), '');
        // Convert them to const-code into the output unit :
        // (There's no PatternList variable, so TList.Expand )
        UnitLines.Add(_ConvertPatternListToConstCode({TList.}Expand, PatternArrayName));

        // Append the information about this array to the overview :
        if i > 0 then
          PatternArraysInfo := PatternArraysInfo + ',';
        PatternArraysInfo := PatternArraysInfo +
          Format(#13#10'  (Name:''%s'';Version:''%s'';NrPatterns:%d;PatternArray:@%0:s)',
            [PatternArrayName, {Version=}'?', Count]);
      finally
        Free;
      end;
    end;

    // Write out the overview, finish up the unit and write it to disk :
    UnitLines.Add(PatternArraysInfo);
    UnitLines.Add(');'#13#10#13#10'implementation end.');
    UnitLines.SaveToFile(OutputUnitName + '.pas');

    Result := 0;
  finally
    FreeAndNil(Input);
    FreeAndNil(UnitLines);
  end;
end;

procedure PatternCodeGenerator_Main;
var
  PatternFiles: TStringList;
begin
  WriteLn(ChangeFileExt(ExtractFileName(ParamStr(0)), '') + ' - converts Xbox pattern files to an Object Pascal unit.');
  WriteLn('This tool is part of the Dxbx Xbox emulator project.');
  WriteLn('See http://sourceforge.net/projects/dxbx');
  WriteLn('');

  if ParamCount <> 1 then
  begin
    WriteLn('ERROR! Missing argument. Please supply a folder containing the Xbox *.pat files.');
    Exit;
  end;

  PatternFiles := TStringList.Create;
  try
    if FindFiles({PatternFolder=}ParamStr(1), {aFileMask=}'*.pat', PatternFiles) <= 0 then
    begin
      WriteLn('ERROR! No pattern files found!');
      Exit;
    end;

    ExitCode := GeneratePatternUnit(PatternFiles);
    if ExitCode = 0 then
      WriteLn('Done.')
    else
      WriteLn('ERROR! No pattern unit generated!');

  finally
    FreeAndNil(PatternFiles);
  end;
end;

end.

