{

Una simple unidad para manejo de bits de variables de 8, 16, 32 y 64 bits

Ejemplo:

var
  a: Byte;
begin
  a:=2; ----> binario 00000010
  SetBitEn(a, 1, 2); ----> a = binario 00000110
end;

Marcelo Limori - 14/FEB/07

}

unit uBitsOps;

interface

function GetBitEn(Variable: Byte; Position: Byte): Byte; overload;//8 bits
function GetBitEn(Variable: ShortInt; Position: Byte): Byte; overload;//(+/-) 8 bits

function GetBitEn(Variable: Word; Position: Byte): Byte; overload;//16 bits
function GetBitEn(Variable: SmallInt; Position: Byte): Byte; overload;//(+/-) 16 bits

function GetBitEn(Variable: Cardinal; Position: Byte): Byte; overload;//32 bits
function GetBitEn(Variable: Integer; Position: Byte): Byte; overload;//(+/-) 32 bits

implementation

uses
  // Delphi
  SysUtils;

//Procedimientos genéricos para asignar y leer--------------------------------------------------------------------------

procedure SetBitPtr(Variable: Pointer; Value, Position, Size: Byte);
var
  Mask: Cardinal;
begin
  if Position > (Size - 1) then
    raise ERangeError.Create('Accessing a bit outside the maximum range');

  Mask := 0;
  case Size of
    8: Mask := $FF;
    16: Mask := $FFFF;
    32: Mask := $FFFFFFFF;
  end;

  if Value = 1 then
    Cardinal(Variable^) := Cardinal(Variable^) or (1 shl Position)
  else
    Cardinal(Variable^) := Cardinal(Variable^) and ((1 shl Position) xor Mask);
end; // SetBitPtr

function GetBitPtr(Variable: Pointer; Position, Size: Byte): Byte;
begin
  if Position > (Size - 1) then
    raise ERangeError.Create('Accessing a bit outside the maximum range');

  if (Cardinal(Variable^) and (1 shl Position)) <> 0 then
    Result := 1
  else
    Result := 0;
end; // GetBitPtr


//8 bits----------------------------------------------------------------------------------------------------------------

function GetBitEn(Variable: Byte; Position: Byte): Byte;
begin
  Result := GetBitPtr(@Variable, Position, 8);
end;

function GetBitEn(Variable: ShortInt; Position: Byte): Byte;
begin
  Result := GetBitPtr(@Variable, Position, 8);
end;

//16 bits---------------------------------------------------------------------------------------------------------------

function GetBitEn(Variable: Word; Position: Byte): Byte;//16 bits
begin
  Result := GetBitPtr(@Variable, Position, 16);
end;

function GetBitEn(Variable: SmallInt; Position: Byte): Byte;//(+/-) 16 bits
begin
  Result := GetBitPtr(@Variable, Position, 16);
end;


//32 bits---------------------------------------------------------------------------------------------------------------


function GetBitEn(Variable: Cardinal; Position: Byte): Byte;//32 bits
begin
  Result := GetBitPtr(@Variable, Position, 32);
end;

function GetBitEn(Variable: Integer; Position: Byte): Byte;//(+/-) 32 bits
begin
  Result := GetBitPtr(@Variable, Position, 32);
end;

end.
