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
  PosBranch: Integer;
  PosRevision: Integer;
  PosTranslator: Integer;
  PosDone: Integer;
  LineNr: Integer;
  FunctionName: string;

  function _IsFunction(const aLine: string; var aFunctionName: string): Boolean;
  begin
    Result := (Pos('function', aLine) > 0)
           or (Pos('procedure', aLine) > 0)
           or (Pos('.', aLine) > 0);

    if Result then
      {var}aFunctionName := aLine;
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
            UnitName := Copy(Line, 5, CharPos(Line, ';', 6) - 5);
            ScanState := ssFindTranslationComment;
          end;
        end;
        ssFindTranslationComment:
        begin
          // Only handle comment lines :
          if not StrStartsWith(Line, '//') then
            Continue;

          // Scan for the parts in this line :
          PosBranch := StrIPos('Branch:', Line);
          PosRevision := StrIPos('Revision:', Line);
          PosTranslator := StrIPos('Translator:', Line);
          PosDone := StrIPos('Done:', Line);

          // Count how many parts we've found :
          j := 0;
          if (PosBranch > 0) then Inc(j);
          if (PosRevision > 0) then Inc(j);
          if (PosTranslator > 0) then Inc(j);
          if (PosDone > 0) then Inc(j);
          // No hit when less than 2 parts :
          if j < 2 then
            Continue;

          // Scan for the function name, from 6 lines before
          // until 2 lines after this translation-progress comment :
          FunctionName := '*undetermined function*';
          LineNr := i + 1;
          repeat
            if _IsFunction(Strings[LineNr], {var}FunctionName) then
              Break;

            Dec(LineNr);
          until (LineNr < i - 6) or (LineNr < 0);

          // Show what we've found :
          Log(Format('[Translation Progress] %s.pas(%d) : %s - %s.',
            [UnitName, LineNr+1, FunctionName, Line]));
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
  DxbxSrcPath: string;
  CxbxSrcPath: string;
  FileList: TStringList;
  i: Integer;
begin

  DxbxSrcPath := ExpandFileName(edDxbxSrcPath.Text);
  CxbxSrcPath := ExpandFileName(edCxbxSrcPath.Text);
  Log('Dxbx sources path : ' + DxbxSrcPath);
  Log('Cxbx sources path : ' + CxbxSrcPath);

  FileList := TStringList.Create;
  try
    JclFileUtils.AdvBuildFileList(DxbxSrcPath + '\*.pas', 0, FileList, amAny, [flRecursive, flFullNames]);

    for i := 0 to FileList.Count - 1 do
      HandleDxbxFile(FileList[i]);

  finally
    FreeAndNil(FileList);
  end;

end;

end.
