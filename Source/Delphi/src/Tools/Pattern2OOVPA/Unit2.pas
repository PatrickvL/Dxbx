unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    EditName: TEdit;
    MemoInput: TMemo;
    EditVersion: TEdit;
    EditPattern: TEdit;
    MemoOutput: TMemo;
    Button1: TButton;
    procedure MemoInputChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    HasEdits: Boolean;
  public
    procedure ConvertEdits;
    procedure ParseInputs;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.ConvertEdits;
var
  PatternStr: string;
  NrHexOffsets: Integer;
  Pattern: PChar;
  i: Integer;
  HasValue: array of Boolean;
  NrOOVP: Integer;
  OOVP: array of Byte;
  o: Integer;
  InsertEmpty: Boolean;
begin
//  if not HasEdits then
//    Exit;

  PatternStr := EditPattern.Text;
  NrHexOffsets := Length(PatternStr) div 2;
  SetLength(HasValue, NrHexOffsets);
  SetLength(OOVP, NrHexOffsets);
  NrOOVP := 0;
  Pattern := PChar(PatternStr);
  for i := 0 to NrHexOffsets - 1 do
    if HexToBin(Pattern + (i * 2), @(OOVP[NrOOVP]), 1) = 1 then
    begin
      HasValue[i] := True;
      Inc(NrOOVP);
    end
    else
      HasValue[i] := false;

  MemoOutput.Lines.Add('');
  MemoOutput.Lines.Add(Format('OOVPA_NO_XREF(%s, %s, %d)', [EditName.Text, EditVersion.Text, NrOOVP]));
  o := 0;
  InsertEmpty := False;
  for i := 0 to NrHexOffsets - 1 do
    if HasValue[i] then
    begin
      if InsertEmpty then
      begin
        MemoOutput.Lines.Add('');
        InsertEmpty := False;
      end;

      MemoOutput.Lines.Add(Format(#9#9'{ 0x%.02X, 0x%.02X },', [i, OOVP[o]]));
      Inc(o);
    end
    else
      InsertEmpty := True;

  MemoOutput.Lines.Add('OOVPA_END;');
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  ConvertEdits;
end;

procedure TForm2.ParseInputs;
type
  TParseMode = (pmSeekName, pmSeekVersion, pmSeekPattern);
const
  MinLength: array [TParseMode] of Integer = (2, 4, 32);
var
  i: Integer;
  Line: string;
  ParseMode: TParseMode;
  p: Integer;
  Str: string;
begin
  ParseMode := pmSeekName;
  for i := 0 to MemoInput.Lines.Count - 1 do
  begin
    Line := MemoInput.Lines[i];
    while Line <> '' do
    begin
      p := Pos(' ', Line);
      if p > 0 then
      begin
        Str := Copy(Line, 1, p - 1);
        Delete(Line, 1, p);
      end
      else
      begin
        Str := Line;
        Line := '';
      end;

      if Length(Str) < MinLength[ParseMode] then
        Continue;

      case ParseMode of
        pmSeekName:
        begin
          EditName.Text := Str;
          ParseMode := pmSeekVersion;
        end;
        pmSeekVersion:
        begin
          EditVersion.Text := Str;
          ParseMode := pmSeekPattern;
        end;
        pmSeekPattern:
        begin
          EditPattern.Text := Str;
          ConvertEdits;
          ParseMode := pmSeekName;
        end;
      end;
    end;
  end;
end;

procedure TForm2.MemoInputChange(Sender: TObject);
begin
  ParseInputs;
end;

end.
