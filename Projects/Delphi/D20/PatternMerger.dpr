program PatternMerger;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes;

const
  Space = ' ';
  Comma = ',';
  Carret = '^'; // starts an xref
  Space_Length = 1;
  HexByte_Length = 2;
  Version_Length = 4; // 3912, etc

  StartPattern_Offset = 1;
  StartPattern_Length = 32 * HexByte_Length;
  CRC_Offset = 66;
  CRC_Length = HexByte_Length + Space_Length + (2 * HexByte_Length);
  Length_Offset = 74;
  Length_Length = 2 * HexByte_Length;
  SymbolName_Offset = 79;

type
  TOuputFormat = (ofPAT, ofCSV, ofJSON);

procedure ShowHelp;
begin
  WriteLn;
  WriteLn('Usage : PatternMerger [folder] [format]');
  WriteLn;
  WriteLn('Specified folder will be scanned for all pattern files,');
  WriteLn('their contents will be read, and written reordered into');
  WriteLn('an "all" file, with the specified format.');
  WriteLn;
  WriteLn('Only filenames starting with a version number and');
  WriteLn('a "pat" extension are considered (ex: "3912d3d8.pat").');
  WriteLn;
  WriteLn('Possible formats : PAT, CSV or JSON');
  WriteLn;
  WriteLn('(Hint : "Enquote folder name" if it contains spaces.)');
  WriteLn;
end;

function Hex2ToByte(const aStr: string): Byte;
begin
  Assert(Length(aStr) >= 2, 'Two (2) input characters required');

  HexToBin(PChar(aStr), @Result, 2);
end;

function Hex4ToWord(const aStr: string): Word;
var
  Buffer: array [0..1] of Byte;
begin
  Assert(Length(aStr) >= 4, 'Four (4) input characters required');

  HexToBin(PChar(aStr), @Buffer[0], 4);
  Result := (Buffer[0] * 256) + Buffer[1];
end;

var
  PatternsPath: string;
  OuputFormat: TOuputFormat;
  F: TSearchRec;
  Input: TStringList;
  Output: TStringList;
  i: Integer;
  VersionStr: string;
  LibStr: string;
  VersionAndLibStr: string;
  Line: string;
  p: Integer;
  StartPatternStr: string;
  CRCStr: string;
  SymbolStr: string;
  LengthStr: string;
  ReferencesStr: string;
  TrailingPatternStr: string;
begin
  try
    if ParamCount < 2 then
    begin
      WriteLn('Missing arguments');
      ShowHelp;
      Exit;
    end;

    Line := ParamStr(2);
    if SameText(Line, 'PAT') then
      OuputFormat := ofPAT
    else
    if SameText(Line, 'CSV') then
      OuputFormat := ofCSV
    else
    if SameText(Line, 'JSON') then
      OuputFormat := ofJSON
    else
    begin
      WriteLn('Unknown output format : ' + Line);
      ShowHelp;
      Exit;
    end;

    PatternsPath := IncludeTrailingBackslash(ParamStr(1));
    if not FindFirst(PatternsPath + '*.pat', faNormal, {var}F) = 0 then
    begin
      WriteLn('No .pat files in specified folder');
      ShowHelp;
      Exit;
    end;

    Input := TStringList.Create;
    Output := TStringList.Create;
    try
      repeat
        // Skip files not starting with a version number :
        if not CharInSet(F.Name[1], ['0'..'9']) then
          Continue;

        Input.LoadFromFile(PatternsPath + F.Name);
        VersionStr := Copy(F.Name, 1, Version_Length);
        LibStr := ChangeFileExt(Copy(F.Name, 5, MaxInt), '');
        case OuputFormat of
          ofPAT:
            VersionAndLibStr := Space + VersionStr + Space + LibStr + Space;
          ofCSV:
            VersionAndLibStr := Comma + VersionStr + Comma + LibStr + Comma;
          ofJson:
            VersionAndLibStr := '","version":' + VersionStr + ',"lib":"' + LibStr + '","length":';
        end;

        for i := 0 to Input.Count - 1 do
        begin
          Line := Trim(Input[i]);
          // Skip empty lines
          if Length(Line) = 0 then
            Continue;

          // Stop at '---' marker
          if Line[1] = '-' then
            Break;

          // Skip too short lines
          if Length(Line) <= SymbolName_Offset then
            Continue; // Error?

          // Skip comment lines
          if Line[1] = ';' then
            Continue;

          // Skip internal labels
          if Line[SymbolName_Offset] = '$' then
            Continue;

          // Decompose line into it's parts :
          StartPatternStr := Copy(Line, StartPattern_Offset, StartPattern_Length);
          CRCStr := Copy(Line, CRC_Offset, CRC_Length); // TODO : split up CRC into CRCLength and CRCValue
          LengthStr := Copy(Line, Length_Offset, Length_Length);
          ReferencesStr := '';
          p := Pos(Space, Line, SymbolName_Offset);
          if p > 0 then
          begin
            SymbolStr := Copy(Line, SymbolName_Offset, {SymbolName_Length=}p - SymbolName_Offset);
            TrailingPatternStr := Copy(Line, p + Space_Length, MaxInt);
            // Does the line contain xrefs before the trailing pattern ?
            p := Length(TrailingPatternStr);
            while (p > 0) and (TrailingPatternStr[p] <> Carret) do
              Dec(p);

            // Did we find a carret?
            if p > 0 then
            begin
              // Goto next space
              p := Pos(Space, TrailingPatternStr, p);
              Assert(p > 0);

              if Pos(Space, TrailingPatternStr, p + Space_Length) > 0 then
              begin
                p := Pos(Space, TrailingPatternStr, p + Space_Length);
                ReferencesStr := Copy(TrailingPatternStr, 1, p - Space_Length);
                Delete({var}TrailingPatternStr, 1, p + Space_Length);
              end
              else
              begin
                ReferencesStr := TrailingPatternStr;
                TrailingPatternStr := '';
              end;
            end;
          end
          else
          begin
            SymbolStr := Copy(Line, SymbolName_Offset, MaxInt);
            TrailingPatternStr := '';
          end;

          // Recompose and format output line :
          case OuputFormat of
            ofPAT:
            begin
              Line := SymbolStr + VersionAndLibStr + LengthStr + Space + StartPatternStr + Space + CRCStr;
              // PAT must not append trailing spaces
              if TrailingPatternStr <> '' then
                Line := Line + Space + TrailingPatternStr;

              if ReferencesStr  <> '' then
                Line := Line + Space + ReferencesStr;
            end;
            ofCSV:
            begin
              // Convert length from hex to integer :
              LengthStr := IntToStr(Hex4ToWord(LengthStr));
              // Convert CRCLength from hex to integer, separate CRCLength and CRCValue by a comma :
              p := Hex2ToByte(CRCStr);
              CRCStr := IntToStr(p) + ',' + Copy(CRCStr, 4, 4);
              // CSV allows empty values between commas
              Line := SymbolStr + VersionAndLibStr + LengthStr + Comma + StartPatternStr + Comma + CRCStr + Comma + TrailingPatternStr + Comma + ReferencesStr;
            end;
            ofJson:
            begin
              // Note : JSON allows absent values and supports integer values.

              // Get the length of the starting pattern, and trim it when possible :
              p := Hex4ToWord(LengthStr);
              if p * HexByte_Length < StartPattern_Length then
              begin
                SetLength(StartPatternStr, p * HexByte_Length);

                Assert(TrailingPatternStr = '', 'Trailing pattern shouldn''t be set when length is less than 32!');
              end;

              Line := '{"symbol":"' + SymbolStr +
                VersionAndLibStr + IntToStr(p) +
                ',"start_pattern":"' + StartPatternStr + '"';
              // The CRC might be useless if there's no trailing pattern :
              if (CRCStr <> '00 0000') or (TrailingPatternStr <> '') then
              begin
                // render CRCLength as a separate integer
                p := Hex2ToByte(CRCStr);
                Line := Line + ',"crc_length":' + IntToStr(p);
                if p > 0 then
                  // only when needed, render CRCValue as a HexString
                  Line := Line + ',"crc_value":"' + Copy(CRCStr, 4, 4) + '"';
              end;

              if TrailingPatternStr <> '' then
                Line := Line + ',"trailing_pattern":"' + TrailingPatternStr + '"';

              if ReferencesStr <> '' then
              begin
                // convert references into an array
                ReferencesStr := StringReplace(Space + ReferencesStr, Space + Carret, '"},{"offset":"', [rfReplaceAll]);
                // TODO : render xref offsets as integers instead of hexadecimal strings
                // TODO : convert 'R'elative/'D'irect indicators into separate values
                ReferencesStr := StringReplace(ReferencesStr, Space, '","xref":"', [rfReplaceAll]);
                Line := Line + ',"references":[' + Copy(ReferencesStr, 4, MaxInt) + '"}]';
              end;

              Line := Line + '},';
            end;
          end;

          Output.Add(Line);
        end; // for input lines

      until FindNext({var}F) <> 0; // repeat files
      FindClose({var}F);

      // Sort and write output file :
      if Output.Count > 0 then
      begin
        Output.Sort;
        case OuputFormat of
          ofPAT:
            Output.SaveToFile(PatternsPath + 'all.pat');
          ofCSV:
          begin
            // Insert csv header line :
            Output.Insert(0, 'Symbol,Version,Lib,Length,StartPattern,CRCLength,CRCValue,TrailingPattern,References');
            Output.SaveToFile(PatternsPath + 'all.csv');
          end;
          ofJSON:
          begin
            // Wrap output in a JSON array :
            Output.Insert(0, '[');
            Output.Add(']');
            Output.SaveToFile(PatternsPath + 'all.json');
          end;
        end;
      end;
    finally
      Output.Free;
      Input.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
