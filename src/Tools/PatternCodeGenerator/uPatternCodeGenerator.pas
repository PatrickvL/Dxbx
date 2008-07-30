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

  function _ConvertPatternListToConstCode(
    const aPatternList: TList;
    const aPatternArrayName: string;
    out IsFullDebugLibrary: Boolean): string;
  var
    SeparatorChar: Char;
    InFirstPass: Boolean;
    i, j: Integer;
    Pattern: PXboxLibraryFunction;
    IsPatch: TBits;
    PreviousPattern: PXboxLibraryFunction;
    NrPatchFunctions, NrDebugFunctions: Integer;
  begin
    IsPatch := TBits.Create;
    with TStringStream.Create('') do
    try
      // First, count how many functions are purely for debugging purposes :
      NrPatchFunctions := 0;
      NrDebugFunctions := 0;
      PreviousPattern := nil;
      IsPatch.Size := aPatternList.Count;
      for i := 0 to aPatternList.Count - 1 do
      begin
        Pattern := PXboxLibraryFunction(aPatternList.List^[i]);

        // Skip patterns with a short name, and duplicates :
        if (Length(Pattern.Name) < 2) or (PatternList_NameCompare(Pattern, PreviousPattern) = 0) then
          Continue;

        PreviousPattern := Pattern;

        if IsXboxLibraryPatch(Pattern.Name) then
        begin
          IsPatch[i] := True;
          Inc(NrPatchFunctions);
        end
        else
          Inc(NrDebugFunctions);
      end;

      // Test if all functions are debug-only :
      IsFullDebugLibrary := (NrPatchFunctions = 0);
      if IsFullDebugLibrary then
        WriteString(#13#10'{$IFDEF _DEBUG_TRACE}');

      // Write the header of this array to the output :
      WriteString(#13#10);
      WriteString(aPatternArrayName);
      WriteString(': array [0..');
      if IsFullDebugLibrary then
        WriteString(IntToStr(NrDebugFunctions - 1))
      else
      begin
        WriteString('{$IFDEF _DEBUG_TRACE}');
        WriteString(IntToStr(NrPatchFunctions + NrDebugFunctions - 1));
        WriteString('{$ELSE}');
        WriteString(IntToStr(NrPatchFunctions - 1));
        WriteString('{$ENDIF}');
      end;
      WriteString('] of RXboxLibraryFunction = (');

      // Loop twice over the patterns, first for the functions to be patched
      // and next over the debug-functions :
      SeparatorChar := ' ';
      PreviousPattern := nil;
      for InFirstPass := True downto False do
      begin
        for i := 0 to aPatternList.Count - 1 do
        begin
          if InFirstPass = IsPatch[i] then
          begin
            Pattern := PXboxLibraryFunction(aPatternList.List^[i]);
            // Skip patterns with a short name, and duplicates :
            if (Length(Pattern.Name) < 2) or (PatternList_NameCompare(Pattern, PreviousPattern) = 0) then
              Continue;

            PreviousPattern := Pattern;

            WriteString(#13#10' ' + SeparatorChar + '(Name:''');
            SeparatorChar := ',';

            WriteString(Pattern.Name);
            WriteString(''';Pattern:(BytesToUseMask:$');
            WriteString(IntToHex(Pattern.Pattern.BytesToUseMask, 8));
            WriteString(';Bytes:($');
            for j := 0 to Length(Pattern.Pattern.Bytes) - 1 do
            begin
              if j > 0 then
                WriteString(',$');
              WriteString(IntToHex(Pattern.Pattern.Bytes[j], 2));
            end;
            WriteString('));CRCLength:');
            WriteString(IntToStr(Pattern.CRCLength));
            WriteString(';CRCValue:$');
            WriteString(IntToHex(Pattern.CRCValue, 4));
            WriteString(';TotalLength:');
            WriteString(IntToStr(Pattern.TotalLength));
            WriteString(')');
          end;
        end; // for aPatternList

        // At the end of the first pass, check if we need to start a debug-section
        // (only do this when there are both patched- and debug-functions present) :
        if  InFirstPass
        and (NrPatchFunctions > 0)
        and (NrDebugFunctions > 0) then
          WriteString(#13#10'{$IFDEF _DEBUG_TRACE}');
      end; // for IsFirstPass

      // If we started a debug-section, end it here :
      if  (NrPatchFunctions > 0)
      and (NrDebugFunctions > 0) then
        WriteString(#13#10'{$ENDIF ~_DEBUG_TRACE}');

      // Close off the array :
      WriteString(#13#10');');
      
      // If the complete array was in debug-mode, close that off now :
      if IsFullDebugLibrary then
        WriteString(#13#10'{$ENDIF ~_DEBUG_TRACE}');
    finally
      Result := DataString;
      Free;
      FreeAndNil(IsPatch);
    end;
  end;

const
  OutputUnitName = 'uPatterns'; // TODO : Make this configurable?
var
  UnitLines: TStringList;
  Input: TMemoryStream;
  i: Integer;
  PatternFilePath, LibVersionStr, LibName: string;
  PatternArrayName, PatternArraysInfo: string;
  SortedPatternList: TList;
  IsFullDebugLibrary: Boolean;
  SeparatorChar: Char;
  NrDebugLibraries: Integer;
  InDebugCode: Boolean;
begin
  UnitLines := TStringList.Create;
  Input := TMemoryStream.Create;
  try
    UnitLines.Add('// This unit is auto-generated. Do not edit!');
    UnitLines.Add('unit ' + OutputUnitName + ';interface uses uXboxLibraryUtils;const');
    UnitLines.Add('{$INCLUDE ..\..\Dxbx.inc}');

    PatternArraysInfo := '] of RXboxLibraryInfo = (';

    SeparatorChar := ' ';
    NrDebugLibraries := 0;
    InDebugCode := False;
    
    // Load and convert all pattern files :
    for i := 0 to aPatternFiles.Count - 1 do
    begin
      // Get the Pattern FilePath, and figure out for which library and version it is :
      PatternFilePath := aPatternFiles[i];
      LibName := ChangeFileExt(ExtractFileName(PatternFilePath), '');
      LibVersionStr := Copy(LibName, 1, 4);
      Delete(LibName, 1, 4);
      PatternArrayName := 'Patterns_' + LibVersionStr + '_' + LibName;

      // Load pattern file :
      Input.LoadFromFile(PatternFilePath);

      // Parse patterns in this file :
      SortedPatternList := ConvertPatternsToSortedList(Input.Memory);
      try
        // Convert them to const-code into the output unit :
        UnitLines.Add(_ConvertPatternListToConstCode(SortedPatternList, PatternArrayName, {out}IsFullDebugLibrary));

        // Check if we need to enter a Debug section :
        if IsFullDebugLibrary <> InDebugCode then
        begin
          if not InDebugCode then
            PatternArraysInfo := PatternArraysInfo + #13#10'{$IFDEF _DEBUG_TRACE}'
          else
            PatternArraysInfo := PatternArraysInfo + #13#10'{$ENDIF}';

          InDebugCode := not InDebugCode;
        end;

        // Append the information about this array to the overview :
        PatternArraysInfo := PatternArraysInfo +
          Format(#13#10' %s(LibVersion:%s;LibName:''%s'';PatternArray:@%s)',
            [SeparatorChar, LibVersionStr, LibName, PatternArrayName]);

        if IsFullDebugLibrary then
        begin
          // Debug lines should not set the PrefixSeparatorChar,
          // because there may not have been a non-debug line yet.
          // So put a comma after this line (unless it's the last one) :
          if (SeparatorChar = ' ') and (i < aPatternFiles.Count - 1) then
            PatternArraysInfo := PatternArraysInfo + ',';

          Inc(NrDebugLibraries);
        end
        else
          SeparatorChar := ',';

      finally
        FreeAndNil(SortedPatternList);
      end;
    end; // for

    if InDebugCode then
      PatternArraysInfo := PatternArraysInfo + #13#10'{$ENDIF}';

    // Write out the overview, finish up the unit and write it to disk :
    UnitLines.Add(#13#10'{$WRITEABLECONST ON}');
    if NrDebugLibraries > 0 then
      PatternArraysInfo := Format(#13#10'AllXboxLibraries: array[0..{$IFDEF _DEBUG_TRACE}%d{$ELSE}%d{$ENDIF}',
        [aPatternFiles.Count - 1, aPatternFiles.Count - NrDebugLibraries - 1]) + PatternArraysInfo
    else
      PatternArraysInfo := Format(#13#10'AllXboxLibraries: array[0..%d',
        [aPatternFiles.Count - 1]) + PatternArraysInfo;
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

