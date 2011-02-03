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

{$INCLUDE Dxbx.inc}

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, IniFiles, Buttons, FileCtrl, Spin,
  // 3rd Party
  JclStrings,
  JclFileUtils,
  // Dxbx
  uTypes;

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
    cbFilterDoneLess: TCheckBox;
    seFilterDone: TSpinEdit;
    lblFilterDone: TLabel;
    ebFilterTranslator: TEdit;
    lblFilterTranslator: TLabel;
    ebFilterBranch: TEdit;
    lblFilterBranch: TLabel;
    cbFilterNot: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnCxbxSourcesPathClick(Sender: TObject);
    procedure btnDxbxSourcesPathClick(Sender: TObject);
    procedure btnSaveToXmlClick(Sender: TObject);
    procedure btnScanTranslationClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    NrOfFoundSymbols: Integer;
    NrOfLoggedSymbols: Integer;

    procedure ReadIni;
    procedure WriteIni;
    procedure Log(const aString: string);
    procedure HandleDxbxFile(const aPascalFilePath: string);
  end;


procedure CreateProgressXml(DxbxSourcePath, CxbxSourcePath, OutputFileName: string);

var
  Form1: TForm1;

implementation

var
  memOut : TStrings;

procedure CreateProgressXml(DxbxSourcePath, CxbxSourcePath, OutputFileName: string);
begin
  if not Assigned(memOut) then
    memOut := TStringList.Create;

  try

  except
    writeln('Problems with writing progress to Xml');
  end;

  if Assigned(memOut) then
    FreeAndNil(memOut);
end;

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
  if not Assigned(memOut) then
    memOut := TStringList.Create;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIni;
  if Assigned(memOut) then
    FreeAndNil(memOut);
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
  UnitNameStr: string;
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

  function _GetSymbolName(aLine: string; var aSymbolName: string): Boolean;
  var
    Text: string;
    p: Integer;
  begin
    // Make sure the line is at least 3 characters long :
    aLine := Trim(aLine);
    Result := (Length(aLine) > 2);
    if not Result then
      Exit;

    // Remove {comment} block from the start of the line :
    if aLine[1] = '{' then
    begin
      p := Pos('}', aLine);
      Result := p < Length(aLine);
      if Result then
      begin
        Delete(aLine, 1, p);
        aLine := Trim(aLine);
        Result := (Length(aLine) > 2);
      end;

      if not Result then
        Exit;
    end;

    // Remove comment start from the beginning of the line :
    if  CharInSet(aLine[1], ['(', '/'])
    and CharInSet(aLine[2], ['*', '/']) then
    // Catches '(*', '/*' and '//', but also '(/' - which can be ignored
    begin
      Delete(aLine, 1, 2);
      aLine := Trim(aLine);
    end
    else
    begin
      // Remove further end of line comments :
      p := Pos('//', aLine);
      if p > 0 then
        Delete(aLine, p, MaxInt);
    end;

    // Scan for Delphi function names :
    Text := _GetTextAfterPrefix(aLine, 'function ', '(:');
    if Text = '' then
      Text := _GetTextAfterPrefix(aLine, 'procedure ', '(:;');
    if Text = '' then
      Text := _GetTextAfterPrefix(aLine, 'type ', '=');
// var and const detection interferes with arguments, so disabled for now :
//    if Text = '' then
//      Text := _GetTextAfterPrefix(aLine, 'var ', ':');
//    if Text = '' then
//      Text := _GetTextAfterPrefix(aLine, 'const ', ':');

    // Scan for C function names :
    if Text = '' then
      Text := _GetTextAfterPrefix(aLine, ' WINAPI ', '[(:;');
    if Text = '' then
      Text := _GetTextAfterPrefix(aLine, '_stdcall ', '[(:;');

    if Text = '' then
    begin
      // Last resort, C functions should contain a separator ('.' or '::') or an opening parenthesis :
      Result := (Pos('.', aLine) > 0)
             or (Pos('::', aLine) > 0)
             or (Pos('(', aLine) > 0);
      if not Result then
        Exit;

      Text := Trim(_StripStringAfterChars(aLine, '[(:;'));
    end;

    Text := Trim(Text);
    Result := Length(Text) >= 3;
    if not Result then
      Exit;

    // When detecting the start of a translation-comment, skip that :
    if SameText(Text, 'branch')
    or SameText(Text, 'source') then
    begin
      Result := False;
      Exit;
    end;

    // Well, we seem to have found a symbol!

    // Lastly, remove the part before (and including) the space (if present) :
    p := Pos(' ', Text);
    if p > 0 then
      Delete(Text, 1, p);

    {var}aSymbolName := Text;
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
            UnitNameStr := Copy(Line, Length('unit ') + 1, CharPos(Line, ';', 6) - Length('unit ') - 1);
            ScanState := ssFindTranslationComment;
          end;
        end;
        ssFindTranslationComment:
        begin
          // Remove comment start from the beginning of the line :
          if  CharInSet(Line[1], ['(', '/'])
          and CharInSet(Line[2], ['*', '/']) then
            System.Delete(Line, 1, 2)
          else
            if SameText(Line, 'end.') then
              Break
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
          // XBCtrlObject: Source=XBController.h  Revision=martin#39  Translator=PatrickvL  Done=100
          // .. but because the function-scanning below also handles the comment-line itself,
          // it's still picked up as a function; Not fully correct, but acceptable for now.

          // Scan for the symbol name, from 1 line after
          // until 10 lines before this translation-progress comment :
          SymbolName := '';
          LineNr := i - 1;
          if not _GetSymbolName(Strings[LineNr], {var}SymbolName) then
          begin
            LineNr := i + 1;
            repeat
              if _GetSymbolName(Strings[LineNr], {var}SymbolName) then
                Break;

              Dec(LineNr);
            until (LineNr < i - 12) or (LineNr < 0);
          end;
           
          if SymbolName = '' then
          begin
            SymbolName := '*undetermined symbol*';
            LineNr := i;
          end;

          // Start the UNIT tag when not already done :
          if not HadUnitTag then
          begin
            Log(Format('  <UNIT name="%s">', [UnitNameStr]));
            HadUnitTag := True;
          end;

          Inc(NrOfFoundSymbols);
          // Check Branch filter :
          if  not cbFilterNot.Checked and ((ebFilterBranch.Text <> '') and (Pos(LowerCase(ebFilterBranch.Text), LowerCase(BranchStr)) <= 0)) then
            Continue;

          // Check Translator filter :
          if (ebFilterTranslator.Text <> '') and (Pos(LowerCase(ebFilterTranslator.Text), LowerCase(TranslatorStr)) <= 0) then
            Continue;

          // Check done-percentage filter :
          if (StrToIntDef(DoneStr, 0) < seFilterDone.Value) xor cbFilterDoneLess.Checked then
            Continue;

          // Check Not filter :
          if cbFilterNot.Checked and Not ((ebFilterBranch.Text <> '') and (Pos(LowerCase(ebFilterBranch.Text), LowerCase(BranchStr)) <= 0)) then
            Continue;

          // Show what we've found :
          Log(Format('    <SYMBOL name="%s" line="%d" frombranch="%s" fromsource="%s" fromrevision="%s" translator="%s" done="%d"/>',
            [SymbolName, LineNr+1, BranchStr, SourceStr, RevisionStr, TranslatorStr, StrToIntDef(DoneStr, 0)]));
          Inc(NrOfLoggedSymbols);
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
begin
  memOutput.Clear;

  NrOfFoundSymbols := 0;
  NrOfLoggedSymbols := 0;
  FileList := TStringList.Create;
  try
    DxbxSrcPath := ExpandFileName(edDxbxSrcPath.Text);
    Log(Format('<DXBXTRANSLATION path="%s" timestamp="%s">', [DxbxSrcPath, DateTimeToStr(Now())]));

    JclFileUtils.AdvBuildFileList(DxbxSrcPath + '\*.pas', 0, FileList, amAny, [flRecursive, flFullNames]);

    for i := 0 to FileList.Count - 1 do
      HandleDxbxFile(FileList[i]);

    Log(Format('  <SUMMARY NrOfFoundSymbols="%d" NrOfLoggedSymbols="%d" />', [NrOfFoundSymbols, NrOfLoggedSymbols]));
    Log('</DXBXTRANSLATION>');
  finally
    FreeAndNil(FileList);
  end;
end;



end.
