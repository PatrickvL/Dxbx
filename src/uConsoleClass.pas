unit uConsoleClass;

interface

uses
  Windows;

type
  TConsoleControl = class(TObject)
  private
    FhStdIn            : THandle;  // Handle to the standard input
    FhStdOut           : THandle;  // Handle to the standard output
    FhStdErr           : THandle;  // Handle to the standard error (Output)
    FbConsoleAllocated : Boolean;  // Creation Flag
    FBgAttrib          : Cardinal; // Currently set BackGround Attribs.
    FFgAttrib          : Cardinal; // Currently set ForeGround Attribs.
  public
    constructor Create;
    (* Creates a new consolewindow, or connects the current window *)
    destructor Destroy;override;
    (* Cleanup of the class structures *)
    (* Color properties:
       The console window does not handle the colors like known form delphi
       components. Each color will be created from a red,green,blue and a
       intensity part. In fact the resulting colors are the same as the well
       known 16 base colors (clwhite .. clBlack).
       Black ist if all flags are false, white if all flag are true.
       The following two functions will change the color for following writes *)
    procedure SetForegroundColor(bRed,bGreen,bBlue,bIntensity : Boolean);
    procedure SetBackgroundColor(bRed,bGreen,bBlue,bIntensity : Boolean);
    (* Writing functions :
      simple wrapper around WriteConsole
    *)
    procedure WriteText(const s: string);
    procedure WriteTextLine(const s: string);
    (* Change the Windowtitle of the command window. If the program has been
       executed from a CMD-box the title change is only active while the
       programs execution time *)
    procedure SetWindowTitle(const sTitle: string);
    (* some Cursor manipulation functions *)
    procedure ShowCursor(const aSize: DWord);
    procedure HideCursor;
    procedure GetCursorPos(var x, y: Integer);
    procedure SetCursorTo(x, y: Integer);
    (* screen operations:
       the screen ist the visible part of a cmd window. Behind the window there
       is a screenbuffer. The screenbuffer may be larger than the visible
       window *)
    procedure ClearScreen;
    function GetScreenLeft   : Integer;
    function GetScreenTop    : Integer;
    function GetScreenHeight : Integer;
    function GetScreenWidth  : Integer;
    (* screenbuffer operations *)
    procedure ClearBuffer;
    function GetBufferHeight : Integer;
    function GetBufferWidth  : Integer;
    (* sample to read characters from then screenbuffer *)
    procedure GetCharAtPos(x, y: Integer; var rCharInfo: Char);
  end;

var
  ConsoleControl: TConsoleControl;

implementation

{ TConsoleControl }

procedure TConsoleControl.ClearBuffer;
var
  SBInfo         : TConsoleScreenBufferInfo;
  ulWrittenChars : Cardinal;
  TopLeft        : TCoord;
begin
  TopLeft.X := 0;
  TopLeft.Y := 0;
  GetConsoleScreenBufferInfo(FhStdOut, SBInfo);
  FillConsoleOutputCharacter(FhStdOut,' ',
                             SBInfo.dwSize.X * SBInfo.dwSize.Y,
                             TopLeft,
                             ulWrittenChars);
  FillConsoleOutputAttribute(FhStdOut,
                             FOREGROUND_RED or FOREGROUND_BLUE or FOREGROUND_GREEN,
                             (SBInfo.srWindow.Right - SBInfo.srWindow.Left) *
                             (SBInfo.srWindow.Bottom - SBInfo.srWindow.Top),
                             TopLeft,
                             ulWrittenChars);
end;

procedure TConsoleControl.ClearScreen;
var
  SBInfo         : TConsoleScreenBufferInfo;
  ulWrittenChars : Cardinal;
  TopLeft        : TCoord;
begin
  GetConsoleScreenBufferInfo(FhStdOut,SBInfo);
  TopLeft.X := SBInfo.srWindow.Left;
  TopLeft.Y := SBInfo.srWindow.Top;
  FillConsoleOutputCharacter(FhStdOut, ' ',
                             (SBInfo.srWindow.Right - SBInfo.srWindow.Left) *
                             (SBInfo.srWindow.Bottom - SBInfo.srWindow.Top),
                             TopLeft,
                             ulWrittenChars);
  FillConsoleOutputAttribute(FhStdOut,FOREGROUND_RED or FOREGROUND_BLUE or FOREGROUND_GREEN,
                             (SBInfo.srWindow.Right - SBInfo.srWindow.Left) *
                             (SBInfo.srWindow.Bottom - SBInfo.srWindow.Top),
                             TopLeft,
                             ulWrittenChars);
end;

constructor TConsoleControl.Create;
begin
  inherited Create;

// A process can be associated with only one console, so the AllocConsole
// function fails if the calling process already has a console.
  FbConsoleAllocated := AllocConsole;
// initializing the needed handles
  FhStdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  FhStdErr := GetStdHandle(STD_ERROR_HANDLE);
  FhStdIn  := GetStdHandle(STD_INPUT_HANDLE);
end;

destructor TConsoleControl.Destroy;
begin
  if FbConsoleAllocated then
    FreeConsole;

  inherited Destroy;
end;

function TConsoleControl.GetBufferHeight: Integer;
var
  SBInfo : TConsoleScreenBufferInfo;
begin
  GetConsoleScreenBufferInfo(FhStdOut, SBInfo);
  Result := SBInfo.dwSize.Y;
end;

function TConsoleControl.GetBufferWidth: Integer;
var
  SBInfo : TConsoleScreenBufferInfo;
begin
  GetConsoleScreenBufferInfo(FhStdOut, SBInfo);
  Result := SBInfo.dwSize.X;
end;

procedure TConsoleControl.GetCharAtPos(x, y: Integer; var rCharInfo: Char);
var
  CharInfo : array [0..10] of Char;
  TopLeft  : TCoord;
  CharsRead : Cardinal;
begin
  TopLeft.x := X;
  TopLeft.Y := Y;
  ReadConsoleOutputCharacter(FhStdOut, CharInfo, 10, TopLeft, CharsRead);
  rCharInfo   := CharInfo[0];
end;

procedure TConsoleControl.GetCursorPos(var x, y: Integer);
var
  SBInfo : TConsoleScreenBufferInfo;
begin
  GetConsoleScreenBufferInfo(FhStdOut, SBInfo);
  x := SBInfo.dwCursorPosition.X;
  y := SBInfo.dwCursorPosition.Y;
end;

function TConsoleControl.GetScreenHeight: Integer;
var
  SBInfo : TConsoleScreenBufferInfo;
begin
  GetConsoleScreenBufferInfo(FhStdOut, SBInfo);
  Result := SBInfo.srWindow.Bottom - SBInfo.srWindow.Top;
end;

function TConsoleControl.GetScreenLeft: Integer;
var
  SBInfo : TConsoleScreenBufferInfo;
begin
  GetConsoleScreenBufferInfo(FhStdOut, SBInfo);
  Result := SBInfo.srWindow.Left;
end;

function TConsoleControl.GetScreenTop: Integer;
var
  SBInfo : TConsoleScreenBufferInfo;
begin
  GetConsoleScreenBufferInfo(FhStdOut, SBInfo);
  Result := SBInfo.srWindow.Top;
end;

function TConsoleControl.GetScreenWidth: Integer;
var
  SBInfo : TConsoleScreenBufferInfo;
begin
  GetConsoleScreenBufferInfo(FhStdOut, SBInfo);
  Result := SBInfo.srWindow.Right - SBInfo.srWindow.Left;
end;

procedure TConsoleControl.HideCursor;
var
  ConsoleCursorInfo : TConsoleCursorInfo;
begin
  GetConsoleCursorInfo(FhStdOut, ConsoleCursorInfo);
  if ConsoleCursorInfo.bVisible then
  begin
    ConsoleCursorInfo.bVisible := False;
    SetConsoleCursorInfo(FhStdOut, ConsoleCursorInfo);
  end;
end;

procedure TConsoleControl.SetBackgroundColor(bRed, bGreen, bBlue,
  bIntensity: Boolean);
begin
  FBgAttrib := 0;
  if bRed       then FBgAttrib := FBgAttrib or BACKGROUND_RED;
  if bGreen     then FBgAttrib := FBgAttrib or BACKGROUND_GREEN;
  if bBlue      then FBgAttrib := FBgAttrib or BACKGROUND_BLUE;
  if bIntensity then FBgAttrib := FBgAttrib or BACKGROUND_INTENSITY;
  SetConsoleTextAttribute(FhStdOut, FBgAttrib or FFgAttrib);
end;

procedure TConsoleControl.SetCursorTo(x, y: Integer);
var
  Coords : TCoord;
  SBInfo : TConsoleScreenBufferInfo;
begin
  GetConsoleScreenBufferInfo(FhStdOut, SBInfo);

  if x < 0 then Exit;
  if y < 0 then Exit;
  if x > SbInfo.dwSize.X then Exit;
  if y > SbInfo.dwSize.Y then Exit;

  Coords.X := x;
  Coords.Y := y;
  SetConsoleCursorPosition(FhStdOut, Coords);
end;

procedure TConsoleControl.SetForegroundColor(bRed, bGreen, bBlue,
  bIntensity: Boolean);
begin
  FFgAttrib := 0;
  if bRed       then FFgAttrib := FFgAttrib or FOREGROUND_RED;
  if bGreen     then FFgAttrib := FFgAttrib or FOREGROUND_GREEN;
  if bBlue      then FFgAttrib := FFgAttrib or FOREGROUND_BLUE;
  if bIntensity then FFgAttrib := FFgAttrib or FOREGROUND_INTENSITY;
  SetConsoleTextAttribute(FhStdOut, FBgAttrib or FFgAttrib);
end;

procedure TConsoleControl.SetWindowTitle(const sTitle: string);
begin
  SetConsoleTitle(PChar(sTitle));
end;

procedure TConsoleControl.ShowCursor(const aSize: DWord);
var
  ConsoleCursorInfo : TConsoleCursorInfo;
begin
  GetConsoleCursorInfo(FhStdOut, ConsoleCursorInfo);
  if (not ConsoleCursorInfo.bVisible)
  or (    ConsoleCursorInfo.dwSize <> aSize)  then
  begin
    ConsoleCursorInfo.bVisible := True;
    ConsoleCursorInfo.dwSize   := aSize;
    SetConsoleCursorInfo(FhStdOut, ConsoleCursorInfo);
  end;
end;

procedure TConsoleControl.WriteText(const s: string);
var
  ulLength : Cardinal;
begin
  WriteConsole(FhStdOut, PChar(s), Length(s), ulLength, nil);
end;

procedure TConsoleControl.WriteTextLine(const s: string);
begin
  WriteText(s + #13#10);
end;

end.
