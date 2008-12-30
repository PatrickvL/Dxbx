(*
    This file is part of Dxbx - a XBox emulator written in Delphi (ported over from cxbx)
    Copyright (C) 2007 Shadow_tj and other members of the development team.

    This program is Free software: you can redistribute it and/or modify
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
unit uTranslationChecker;

interface

{$INCLUDE ..\..\Dxbx.inc}

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, IniFiles, Buttons,
  FileCtrl,
  // 3rd Party
  JclStrings,
  JclFileUtils;

type
  TForm1 = class(TForm)
    btnScanTranslation: TButton;
    memOutput: TMemo;
    edDxbxSrcPath: TEdit;
    edCxbxSrcPath: TEdit;
    lblDxbxSrcPath: TLabel;
    lblCxbxSrcPath: TLabel;
    btnDxbxSourcesPath: TSpeedButton;
    btnCxbxSourcesPath: TSpeedButton;
    btnSaveToXml: TButton;
    SaveDialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnCxbxSourcesPathClick(Sender: TObject);
    procedure btnDxbxSourcesPathClick(Sender: TObject);
    procedure btnSaveToXmlClick(Sender: TObject);
    procedure btnScanTranslationClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure ReadIni;
    procedure WriteIni;

    procedure Log(const aString: string);
    procedure HandleDxbxFile(const aPascalFilePath: string);
  end;

var
  Form1: TForm1;

implementation

function StrStartsWith(const aString, aStart: string): Boolean;
begin
  Result := (Length(aString) >= Length(aStart))
        and (StrCompareRange(aString, aStart, 1, Length(aStart)) = 0);
end;

const
  cIni = 'translationchecker.Ini';

{ TForm1 }

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReadIni;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIni;
end;

procedure TForm1.ReadIni;
var
  IniFile: TIniFile;
begin
  if not FileExists(ExtractFilePath(Application.ExeName) + cIni) then
    Exit;

  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + cIni);
  try
    edDxbxSrcPath.Text := IniFile.ReadString('SourcePath', 'Dxbx', '');
    edCxbxSrcPath.Text := IniFile.ReadString('SourcePath', 'Cxbx', '');
  finally
    FreeAndNil(IniFile);
  end;
end;

procedure TForm1.WriteIni;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + cIni);
  try
    IniFile.WriteString('SourcePath', 'Dxbx', edDxbxSrcPath.Text);
    IniFile.WriteString('SourcePath', 'Cxbx', edCxbxSrcPath.Text);
  finally
    FreeAndNil(IniFile);
  end;
end;

procedure TForm1.Log(const aString: string);
begin
  memOutput.Lines.Add(aString);
end;

procedure TForm1.btnCxbxSourcesPathClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := ExpandFileName(edCxbxSrcPath.Text);
  if SelectDirectory('Select Cxbx sources path', '', {var}Dir) then
    edCxbxSrcPath.Text := Dir;
end;

procedure TForm1.btnDxbxSourcesPathClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := ExpandFileName(edDxbxSrcPath.Text);
  if SelectDirectory('Select Dxbx sources path', '', {var}Dir) then
    edDxbxSrcPath.Text := Dir;
end;

procedure TForm1.btnSaveToXmlClick(Sender: TObject);
begin
  if SaveDialog.Execute then 
    memOutput.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TForm1.HandleDxbxFile(const aPascalFilePath: string);
type
  TPascalScanState = (ssNeedUnit, ssFindTranslationComment);
var
  i, j: Integer;
  ScanState: TPascalScanState;
  Line: string;
  UnitName: string;
  HadUnitTag: Boolean;
  SourceStr: string;
  BranchStr: string;
  RevisionStr: string;
  TranslatorStr: string;
  DoneStr: string;
  LineNr: Integer;
  SymbolName: string;

  function _StripStringAfterChars(const aInputStr: string; const EndingChars: string = ' '): string;
  var
    i, j: Integer;
  begin
    Result := aInputStr;

    for j := 1 to Length(EndingChars) do
    begin
      i := CharPos(Result, EndingChars[j]);
      if i > 1 then
        SetLength(Result, i - 1);
    end
  end;

  function _GetTextAfterPrefix(const aLine, aPrefix: string; const EndingChars: string = ' '): string;
  var
    i: Integer;
  begin
    i := StrIPos(aPrefix, aLine);
    if i <= 0 then
    begin
      Result := '';
      Exit;
    end;

    // Get the part after the prefix :
    Result := Trim(Copy(aLine, i + Length(aPrefix), MaxInt));

    // Remove everything after the ending chars :
    Result := _StripStringAfterChars(Result, EndingChars);
  end;

  function _GetFunctionName(aLine: string; var aFunctionName: string): Boolean;
  var
    Text: string;
  begin
    // Make sure the line is at least 3 characters long :
    aLine := Trim(aLine);
    Result := (Length(aLine) > 2);
    if not Result then
      Exit;

    // Remove comment start from the beginning of the line :
    if  (aLine[1] in ['(', '/'])
    and (aLine[2] in ['*', '/']) then
      Delete(aLine, 1, 2);

    // Scan for Delphi function names :
    Text := _GetTextAfterPrefix(aLine, 'function ', '(:');
    if Text = '' then
      Text := _GetTextAfterPrefix(aLine, 'procedure ', '(:;');

    // Scan for C function names :
    if Text = '' then
      Text := _GetTextAfterPrefix(aLine, ' WINAPI ', '(:;');

    Result := (Text <> '');
    if Result then
    begin
      {var}aFunctionName := Text;
      Exit;
    end;

    // Last resort, C functions should contain a dot ('.') or an opening parenthesis :
    Result := (Pos('.', aLine) > 0) or (Pos('(', aLine) > 0);
    if Result then
      {var}aFunctionName := _StripStringAfterChars(aLine, '(:;');
  end;

begin
  // Skip meself ;-) 
  if ExtractFileName(aPascalFilePath) = 'uTranslationChecker.pas' then
    Exit;

  HadUnitTag := False;
  memOutput.Lines.BeginUpdate;
  with TStringList.Create do
  try
    LoadFromFile(aPascalFilePath);

    ScanState := ssNeedUnit;
    for i := 0 to Count - 1 do
    begin
      // Read the line, trim it, and skip all that are less than 3 characters long :
      Line := Trim(Strings[i]);
      if Length(Line) < 3 then
        Continue;

      case ScanState of
        ssNeedUnit:
        begin
          if StrStartsWith(Line, 'unit ') then
          begin
            UnitName := Copy(Line, Length('unit ') + 1, CharPos(Line, ';', 6) - Length('unit ') - 1);
            ScanState := ssFindTranslationComment;
          end;
        end;
        ssFindTranslationComment:
        begin
          // Remove comment start from the beginning of the line :
          if  (Line[1] in ['(', '/'])
          and (Line[2] in ['*', '/']) then
            System.Delete(Line, 1, 2)
          else
            // Skip non-comment lines :
            Continue;

          // Scan the comment-parts :
          SourceStr := _GetTextAfterPrefix(Line, 'Source:');
          if SourceStr = '' then
            SourceStr := _GetTextAfterPrefix(Line, 'Source=');

          BranchStr := _GetTextAfterPrefix(Line, 'Branch:');
          if BranchStr = '' then
            BranchStr := _GetTextAfterPrefix(Line, 'Branch=');

          RevisionStr := _GetTextAfterPrefix(Line, 'Revision:');
          if RevisionStr = '' then
            RevisionStr := _GetTextAfterPrefix(Line, 'Revision=');

          TranslatorStr := _GetTextAfterPrefix(Line, 'Translator:');
          if TranslatorStr = '' then
            TranslatorStr := _GetTextAfterPrefix(Line, 'Translator=');
          if TranslatorStr = '' then
            TranslatorStr := _GetTextAfterPrefix(Line, 'Translation:');
          if TranslatorStr = '' then
            TranslatorStr := _GetTextAfterPrefix(Line, 'Translation=');

          DoneStr := _GetTextAfterPrefix(Line, 'Done:');
          if DoneStr = '' then
            DoneStr := _GetTextAfterPrefix(Line, 'Done=');

          // If Revision also contains Branch, split them up :
          j := Pos('#', RevisionStr);
          if (j > 0) and (BranchStr = '') then
          begin
            BranchStr := Copy(RevisionStr, 1, j - 1);
            System.Delete(RevisionStr, 1, j);
          end;

          // No hit when no branch & revision are given :
          if (BranchStr = '') and (RevisionStr = '') then
            Continue;

          // Note : The following case is a symbol-comment instead of a function-comment :
          // XBCtrlObject: Source=XBController.h Revision=martin#39 Translator=PatrickvL Done=100
          // .. but because the function-scanning below also handles the comment-line itself,
          // it's still picked up as a function; Not fully correct, but acceptable for now.

          // Scan for the symbol name, from 1 line after
          // until 6 lines before this translation-progress comment :
          SymbolName := '*undetermined symbol*';
          LineNr := i + 1;
          repeat
            if _GetFunctionName(Strings[LineNr], {var}SymbolName) then
              Break;

            Dec(LineNr);
          until (LineNr < i - 6) or (LineNr < 0);

          // Start the UNIT tag when not already done :
          if not HadUnitTag then
          begin
            Log(Format('  <UNIT name="%s">', [UnitName]));
            HadUnitTag := True;
          end;

          // Show what we've found :
          Log(Format('    <SYMBOL name="%s" line="%d" frombranch="%s" fromsource="%s" fromrevision="%s" translator="%s" done="%d"/>',
            [SymbolName, LineNr+1, BranchStr, SourceStr, RevisionStr, TranslatorStr, StrToIntDef(DoneStr, 0)]));
        end;
      end;
    end;
  finally
    Free;

    // Close the UNIT tag when it was opened :
    if HadUnitTag then
      Log('  </UNIT>');
      
    memOutput.Lines.EndUpdate;
  end;
end;

procedure TForm1.btnScanTranslationClick(Sender: TObject);
var
  FileList: TStringList;
  i: Integer;
  DxbxSrcPath: string;
//  CxbxSrcPath: string;
begin
  memOutput.Clear;

  FileList := TStringList.Create;
  try
    DxbxSrcPath := ExpandFileName(edDxbxSrcPath.Text);
    Log(Format('<DXBXTRANSLATION path="%s" timestamp="%s">', [DxbxSrcPath, DateTimeToStr(Now())]));

    JclFileUtils.AdvBuildFileList(DxbxSrcPath + '\*.pas', 0, FileList, amAny, [flRecursive, flFullNames]);

    for i := 0 to FileList.Count - 1 do
      HandleDxbxFile(FileList[i]);

//  CxbxSrcPath := ExpandFileName(edCxbxSrcPath.Text);
//  Log('Cxbx sources path : ' + CxbxSrcPath);

    Log('</DXBXTRANSLATION>');

  finally
    FreeAndNil(FileList);
  end;

end;

end.
