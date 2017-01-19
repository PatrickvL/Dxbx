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

  FirstPattern_Offset = 1;
  FirstPattern_Length = 32 * HexByte_Length;
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
  FirstPatternStr: string;
  CRCStr: string;
  SymbolStr: string;
  LengthStr: string;
  ReferencesStr: string;
  SecondPatternStr: string;
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
            VersionAndLibStr := '","version":' + VersionStr + ',"lib":"' + LibStr + '","length":"';
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
          FirstPatternStr := Copy(Line, FirstPattern_Offset, FirstPattern_Length);
          CRCStr := Copy(Line, CRC_Offset, CRC_Length); // TODO : Should we split up CRC into CRCLength and CRCValue?
          LengthStr := Copy(Line, Length_Offset, Length_Length); // TODO : Should we convert length from hexadecimal into decimal?
          ReferencesStr := '';
          p := Pos(Space, Line, SymbolName_Offset);
          if p > 0 then
          begin
            SymbolStr := Copy(Line, SymbolName_Offset, {SymbolName_Length=}p - SymbolName_Offset);
            SecondPatternStr := Copy(Line, p + Space_Length, MaxInt);
            // Does the line contain xrefs before the second pattern ?
            p := Length(SecondPatternStr);
            while (p > 0) and (SecondPatternStr[p] <> Carret) do
              Dec(p);

            // Did we find a carret?
            if p > 0 then
            begin
              // Goto next space
              p := Pos(Space, SecondPatternStr, p);
              Assert(p > 0);

              if Pos(Space, SecondPatternStr, p + Space_Length) > 0 then
              begin
                p := Pos(Space, SecondPatternStr, p + Space_Length);
                ReferencesStr := Copy(SecondPatternStr, 1, p - Space_Length);
                Delete({var}SecondPatternStr, 1, p + Space_Length);
              end
              else
              begin
                ReferencesStr := SecondPatternStr;
                SecondPatternStr := '';
              end;
            end;
          end
          else
          begin
            SymbolStr := Copy(Line, SymbolName_Offset, MaxInt);
            SecondPatternStr := '';
          end;

          // Recompose and format output line :
          case OuputFormat of
            ofPAT:
            begin
              Line := SymbolStr + VersionAndLibStr + LengthStr + Space + FirstPatternStr + Space + CRCStr;
              // PAT must not append trailing spaces
              if SecondPatternStr <> '' then
                Line := Line + Space + SecondPatternStr;

              if ReferencesStr  <> '' then
                Line := Line + Space + ReferencesStr;
            end;
            ofCSV:
              // CSV allows empty values between commas
              Line := SymbolStr + VersionAndLibStr + LengthStr + Comma + FirstPatternStr + Comma + CRCStr + Comma + SecondPatternStr + Comma + ReferencesStr;
            ofJson:
            begin
              Line := '{"symbol":"' + SymbolStr +
                VersionAndLibStr + LengthStr +
                '","first_pattern":"' + FirstPatternStr +
                '","crc":"' + CRCStr + '"';
              // JSON allows absent values :
              if SecondPatternStr <> '' then
                Line := Line + ',"second_pattern":"' + SecondPatternStr + '"';

              if ReferencesStr <> '' then
              begin
                // convert references into an array
                ReferencesStr := StringReplace(Space + ReferencesStr, Space + Carret, '"},{"offset":"', [rfReplaceAll]);
                // TODO : should we convert 'R'elative/'D'irect indicators into separate values?
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
            Output.Insert(0, 'Symbol,Version,Lib,Length,FirstPattern,CRC,SecondPattern,References');
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
