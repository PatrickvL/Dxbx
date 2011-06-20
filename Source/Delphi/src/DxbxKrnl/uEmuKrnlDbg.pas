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
unit uEmuKrnlDbg;

{$INCLUDE Dxbx.inc}

interface

uses
  // Delphi
  SysUtils,
  // Jedi Win32API
  JwaWinType,
  JwaWinBase,
  JwaWinNT,
  JwaNative,
  JwaNTStatus,
  // OpenXDK
  XboxKrnl,
  // Dxbx
  uDxbxUtils,
  uLog,
  uEmuFS,
  uEmuKrnl,
  uDxbxKrnl;

procedure {005} xboxkrnl_DbgBreakPoint(
  ); stdcall;
procedure {006} xboxkrnl_DbgBreakPointWithStatus(
  Status: ULONG
  ); stdcall;
function {007} xboxkrnl_DbgLoadImageSymbols(
  Name: PANSI_STRING;
  Base: PVOID;
  ProcessId: ULONG_PTR
  ): NTSTATUS; stdcall;
function {008} xboxkrnl_DbgPrint(
  Format: PCCH
  ): ULONG; cdecl; // varargs;
function {010} xboxkrnl_DbgPrompt(
  Prompt: PCCH;
  Response: PCH; // OUT
  MaximumResponseLength: ULONG
  ): ULONG; stdcall;
procedure {011} xboxkrnl_DbgUnLoadImageSymbols(
  Name: PANSI_STRING;
  Base: PVOID;
  ProcessId: ULONG_PTR
  ); stdcall;

implementation

procedure {005} xboxkrnl_DbgBreakPoint(
  ); stdcall;
// Source:JwaNative.pas  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('DbgBreakPoint');
  EmuSwapFS(fsXbox);
end;

procedure {006} xboxkrnl_DbgBreakPointWithStatus(
  Status: ULONG
  ); stdcall;
// Source:JwaNative.pas  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('DbgBreakPointWithStatus');
  EmuSwapFS(fsXbox);
end;

function {007} xboxkrnl_DbgLoadImageSymbols(
  Name: PANSI_STRING;
  Base: PVOID;
  ProcessId: ULONG_PTR
  ): NTSTATUS; stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('DbgLoadImageSymbols');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

// Dxbx note : In C, this function uses varargs ('...'), which Delphi doesn't
// support directly (we have array of const, but those are put on the stack
// as TVarRec's, which is quite different from C's varargs).
// Luckily, there's still a way to get to these arguments using RVarArgsReader!
function {008} xboxkrnl_DbgPrint(
  Format: PCCH
  ): ULONG; cdecl; // varargs;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:95
var
  va: RVarArgsReader;
  cp: PAnsiChar;
  ResultStr: AnsiString;
  Arg: Pointer;
begin
  EmuSwapFS(fsWindows);

  // [From MSDN http://msdn.microsoft.com/en-us/library/ff543632(VS.85).aspx ]
  //
  // Specifies a pointer to the format string to print.
  // The Format string supports all the printf-style format specification fields.
  // However, the Unicode format codes (%C, %S, %lc, %ls, %wc, %ws, and %wZ) can only be used with IRQL = PASSIVE_LEVEL.
  //
  // [From MSDN http://msdn.microsoft.com/en-us/library/56e442dc.aspx ]
  //
  // A format specification, which consists of optional and required fields, has the following form:
  // %[flags] [width] [.precision] [{h | l | ll | I | I32 | I64}]type
  //
  // Flag Characters
  // – [default:Right align.]
  //   Left align the result within the given field width.
  //
  // + [default:Sign appears only for negative signed values (–).]
  //   Prefix the output value with a sign (+ or –) if the output value is of a signed type.
  //
  // 0 [default: No padding.]
  //   If width is prefixed with 0, zeros are added until the minimum width is reached. If 0 and – appear, the 0 is ignored.
  //   If 0 is specified with an integer format (i, u, x, X, o, d) and a precision specification is also present
  //   (for example, %04.d), the 0 is ignored.
  //
  // blank (' ') [default:No blank appears.]
  //   Prefix the output value with a blank if the output value is signed and positive; the blank is ignored if both the blank and + flags appear.
  //
  // # [default:No blank appears.]
  //   When used with the o, x, or X format, the # flag prefixes any nonzero output value with 0, 0x, or 0X, respectively.
  //
  // # [default:Decimal point appears only if digits follow it.]
  //   When used with the e, E, f, a or A format, the # flag forces the output value to contain a decimal point in all cases.
  //
  // # [default:Decimal point appears only if digits follow it. Trailing zeros are truncated.]
  //   When used with the g or G format, the # flag forces the output value to contain a decimal point in all cases and prevents
  //   the truncation of trailing zeros.
  //   Ignored when used with c, d, i, u, or s.
  //
  //
  // Type Field Characters
  // c: int or wint_t
  //    When used with printf functions, specifies a single-byte character; when used with wprintf functions, specifies a wide character.
  // C: int or wint_t
  //    When used with printf functions, specifies a wide character; when used with wprintf functions, specifies a single-byte character.
  // d: int
  //    Signed decimal integer.
  // i: int
  //    Signed decimal integer.
  // o: int
  //    Unsigned octal integer.
  // u: int
  //    Unsigned decimal integer.
  // x: int
  //    Unsigned hexadecimal integer, using "abcdef."
  // X: int
  //    Unsigned hexadecimal integer, using "ABCDEF."
  // e: double
  //    Signed value having the form [ – ]d.dddd e [sign]dd[d] where d is a single decimal digit, dddd is one or more decimal digits,
  //    dd[d] is two or three decimal digits depending on the output format and size of the exponent, and sign is + or –.
  // E: double
  //    Identical to the e format except that E rather than e introduces the exponent.
  // f: double
  //    Signed value having the form [ – ]dddd.dddd, where dddd is one or more decimal digits. The number of digits before the decimal
  //    point depends on the magnitude of the number, and the number of digits after the decimal point depends on the requested precision.
  // g: double
  //    Signed value printed in f or e format, whichever is more compact for the given value and precision. The e format is used only when
  //    the exponent of the value is less than –4 or greater than or equal to the precision argument. Trailing zeros are truncated, and the
  //    decimal point appears only if one or more digits follow it.
  // G: double
  //    Identical to the g format, except that E, rather than e, introduces the exponent (where appropriate).
  // a: double
  //    Signed hexadecimal double precision floating point value having the form [-]0xh.hhhh p±dd, where h.hhhh are the hex digits (using
  //    lower case letters) of the mantissa, and dd are one or more digits for the exponent. The precision specifies the number of digits
  //    after the point.
  // A: double
  //    Signed hexadecimal double precision floating point value having the form [-]0Xh.hhhh P±dd, where h.hhhh are the hex digits (using
  //    capital letters) of the mantissa, and dd are one or more digits for the exponent. The precision specifies the number of digits after
  //    the point.
  // n: Pointer to integer
  //    Number of characters successfully written so far to the stream or buffer; this value is stored in the integer whose address is given
  //    as the argument. See Security Note below.
  // p: Pointer to void
  //    Prints the argument as an address in hexadecimal digits.
  // s: String
  //    When used with printf functions, specifies a single-byte–character string; when used with wprintf functions, specifies a
  //    wide-character string. Characters are printed up to the first null character or until the precision value is reached.
  // S: String
  //    When used with printf functions, specifies a wide-character string; when used with wprintf functions, specifies a
  //    single-byte–character string. Characters are printed up to the first null character or until the precision value is reached.
  //    Note  If the argument corresponding to %s or %S is a null pointer, "(null)" will be printed.

  // Parse the varargs :
  va.Create(@Format, SizeOf(Format));
  cp := PAnsiChar(Format);
  ResultStr := '';
  while True do
  begin
    case cp^ of
      #0: Break;
      '%':
      begin
        // Skip the '%' (and any intermediate number for now) :
        repeat
          Inc(cp);
        until not (cp^ in [' ','#','+','-','.',',','0'..'9']);

        // Handle the various types of input :
        case cp^ of
          'c': // 'C':
            ResultStr := ResultStr + cp^;
          'd', 'i':
            ResultStr := ResultStr + AnsiString(IntToStr(va.ReadInt32));
          'o', 'u':
            ResultStr := ResultStr + AnsiString(IntToStr(DWORD(va.ReadInt32)));
          'x', 'p':
            ResultStr := ResultStr + AnsiString(IntToHex(va.ReadInt32, 8));
          'X':
            ResultStr := ResultStr + AnsiString(Uppercase(IntToHex(va.ReadInt32, 8)));
          'a', 'e', 'g', 'f',
          'A', 'E', 'G':
            ResultStr := ResultStr + AnsiString(FloatToStr(va.ReadDouble));
          's': // 'S':
          begin
            Arg := va.ReadPAnsiChar;
            if Arg = nil then
              ResultStr := ResultStr + '(null)'
            else
              ResultStr := ResultStr + AnsiString(PAnsiChar(Arg));
          end;
        else
          ResultStr := ResultStr + '!DXBX WARNING:UNSUPPORTED INPUT!';
        end;
      end;
    else
      // Add the character (could be done faster, but suffices for now) :
      ResultStr := ResultStr + cp^;
    end;

    Inc(cp);
  end;

  Result := STATUS_SUCCESS;

  // Write it to our log :
  if MayLog(lfKernel or lfDebug) then
    DbgPrintf('EmuKrnl : ' + string(ResultStr));

  EmuSwapFS(fsXbox);
end;

function {010} xboxkrnl_DbgPrompt(
  Prompt: PCCH;
  Response: PCH; // OUT
  MaximumResponseLength: ULONG
  ): ULONG; stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('DbgPrompt');
  Result := S_OK;
  EmuSwapFS(fsXbox);
end;

procedure {011} xboxkrnl_DbgUnLoadImageSymbols(
  Name: PANSI_STRING;
  Base: PVOID;
  ProcessId: ULONG_PTR
  ); stdcall;
// Source:ReactOS  Branch:Dxbx  Translator:PatrickvL  Done:0
begin
  EmuSwapFS(fsWindows);
  Unimplemented('DbgUnLoadImageSymbols');
  EmuSwapFS(fsXbox);
end;

end.
