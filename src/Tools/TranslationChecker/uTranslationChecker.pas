unit uTranslationChecker;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  // 3rd Party
  JclStrings,
  JclFileUtils;

type
  TForm1 = class(TForm)
    btnScanTranslation: TButton;
    memOutput: TMemo;
    OpenDialog1: TOpenDialog;
    edDxbxSrcPath: TEdit;
    edCxbxSrcPath: TEdit;
    lblDxbxSrcPath: TLabel;
    lblCxbxSrcPath: TLabel;
    procedure btnScanTranslationClick(Sender: TObject);
  private
    procedure Log(const aString: string);
    procedure HandleDxbxFile(const aPascalFilePath: string);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Log(const aString: string);
begin
  memOutput.Lines.Add(aString);
end;


function StrStartsWith(const aString, aStart: string): Boolean;
begin
  Result := (Length(aString) >= Length(aStart));
  Result := Result
        and (StrCompareRange(aString, aStart, 1, Length(aStart)) = 0);
end;

procedure TForm1.HandleDxbxFile(const aPascalFilePath: string);
type
  TPascalScanState = (ssNeedUnit, ssFindTranslationComment);
var
  i, j: Integer;
  ScanState: TPascalScanState;
  Line: string;
  UnitName: string;
  BranchStr: string;
  RevisionStr: string;
  TranslatorStr: string;
  DoneStr: string;
  LineNr: Integer;
  FunctionName: string;

  // Scan for the parts in this line :

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
    Result := Copy(aLine, i + Length(aPrefix), MaxInt);

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

    // Last resort, C functions should contain a dot ('.') :
    Result := (Pos('.', aLine) > 0);
    if Result then
      {var}aFunctionName := _StripStringAfterChars(aLine, '(:;');
  end;

begin
  memOutput.Lines.BeginUpdate;
  with TStringList.Create do
  try
    LoadFromFile(aPascalFilePath);

    ScanState := ssNeedUnit;
    for i := 0 to Count - 1 do
    begin
      Line := Strings[i];

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
          // Only handle comment lines :
          if not StrStartsWith(Line, '//') then
            Continue;

          // Scan and count the comment-parts :
          BranchStr := _GetTextAfterPrefix(Line, 'Branch:');
          RevisionStr := _GetTextAfterPrefix(Line, 'Revision:');
          TranslatorStr := _GetTextAfterPrefix(Line, 'Translator:');
          DoneStr := _GetTextAfterPrefix(Line, 'Done:');

          // No hit when less than 2 parts :
          if  (BranchStr = '') and (RevisionStr = '') then
            Continue;

          // Scan for the function name, from 6 lines before
          // until 2 lines after this translation-progress comment :
          FunctionName := '*undetermined function*';
          LineNr := i + 1;
          repeat
            if _GetFunctionName(Strings[LineNr], {var}FunctionName) then
              Break;

            Dec(LineNr);
          until (LineNr < i - 6) or (LineNr < 0);

          // Show what we've found :
          Log(Format('<TRANSLATION unit="%s" line="%d" function="%s" frombranch="%s" fromrevision="%s" translator="%s" done="%d"/>',
            [UnitName, LineNr+1, FunctionName, BranchStr, RevisionStr, TranslatorStr, StrToIntDef(DoneStr, 0)]));
        end;
      end;
    end;
  finally
    Free;
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
