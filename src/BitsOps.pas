{

Una simple unidad para manejo de bits de variables de 8, 16, 32 y 64 bits

Ejemplo:

var a:byte;

begin
  a:=2; ----> binario 00000010
  SetBitEn(a, 1, 2); ----> a = binario 00000110
end;

Marcelo Limori - 14/FEB/07

}

unit BitsOps;

interface{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

procedure SetBitEn(var Variable:Byte;Valor,Posicion:byte);overload;//8 bits
procedure SetBitEn(var Variable:ShortInt;Valor,Posicion:byte);overload;//(+/-) 8 bits

procedure SetBitEn(var Variable:Word;Valor,Posicion:byte);overload;//16 bits
procedure SetBitEn(var Variable:SmallInt;Valor,Posicion:byte);overload;//(+/-) 16 bits

procedure SetBitEn(var Variable:Cardinal;Valor,Posicion:byte);overload;//32 bits
procedure SetBitEn(var Variable:Integer;Valor,Posicion:byte);overload;//(+/-) 32 bits

procedure SetBitEn(var Variable:Int64;Valor,Posicion:byte);overload;//(+/-) 64 bits


function GetBitEn(Variable:Byte;Posicion:Byte):Byte;overload;//8 bits
function GetBitEn(Variable:ShortInt;Posicion:Byte):Byte;overload;//(+/-) 8 bits

function GetBitEn(Variable:Word;Posicion:Byte):Byte;overload;//16 bits
function GetBitEn(Variable:SmallInt;Posicion:Byte):Byte;overload;//(+/-) 16 bits

function GetBitEn(Variable:Cardinal;Posicion:Byte):Byte;overload;//32 bits
function GetBitEn(Variable:Integer;Posicion:Byte):Byte;overload;//(+/-) 32 bits

function GetBitEn(Variable:Int64;Posicion:Byte):Byte;overload;//(+/-) 64 bits



implementation{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

uses SysUtils;

//Procedimientos genéricos para asignar y leer--------------------------------------------------------------------------

procedure SetBitPtr(Variable:pointer;Valor,Posicion,Longitud:byte);
var Mascara:Cardinal;
begin
  if Posicion>(Longitud-1) then raise ERangeError.Create('Acceso a bit fuera de rango,');
  Mascara:=0;
  case Longitud of
    8: Mascara:=$FF;
    16: Mascara:=$FFFF;
    32: Mascara:=$FFFFFFFF;
  end;

  if Valor=1 then
    Cardinal(Variable^):=Cardinal(Variable^) or (1 shl Posicion)
  else
    Cardinal(Variable^):=Cardinal(Variable^) and ((1 shl Posicion) xor Mascara);
end;//SetBitPtr

function GetBitPtr(Variable:pointer;Posicion,Longitud:byte):byte;
begin
  if Posicion>(Longitud-1) then raise ERangeError.Create('Acceso a bit fuera de rango,');
  if Cardinal(Variable^) and (1 shl Posicion)<>0 then
    Result:=1
  else
    Result:=0;
end;//GetBitPtr


//8 bits----------------------------------------------------------------------------------------------------------------

procedure SetBitEn(var Variable:Byte;Valor,Posicion:Byte);
begin
  SetBitPtr(@Variable,Valor,Posicion,8);
end;

function GetBitEn(Variable,Posicion:byte):byte;
begin
  Result:=GetBitPtr(@Variable,Posicion,8);
end;

procedure SetBitEn(var Variable:ShortInt;Valor,Posicion:Byte);
begin
  SetBitPtr(@Variable,Valor,Posicion,8);
end;

function GetBitEn(Variable:ShortInt;Posicion:byte):byte;
begin
  Result:=GetBitPtr(@Variable,Posicion,8);
end;

//16 bits---------------------------------------------------------------------------------------------------------------

procedure SetBitEn(var Variable:Word;Valor,Posicion:byte);//16 bits
begin
  SetBitPtr(@Variable,Valor,Posicion,16);
end;
procedure SetBitEn(var Variable:SmallInt;Valor,Posicion:byte);//(+/-) 16 bits
begin
  SetBitPtr(@Variable,Valor,Posicion,16);
end;

function GetBitEn(Variable:Word;Posicion:Byte):Byte;//16 bits
begin
  Result:=GetBitPtr(@Variable,Posicion,16);
end;

function GetBitEn(Variable:SmallInt;Posicion:Byte):Byte;//(+/-) 16 bits
begin
  Result:=GetBitPtr(@Variable,Posicion,16);
end;


//32 bits---------------------------------------------------------------------------------------------------------------


procedure SetBitEn(var Variable:Cardinal;Valor,Posicion:byte);//32 bits
begin
  SetBitPtr(@Variable,Valor,Posicion,32);
end;

procedure SetBitEn(var Variable:Integer;Valor,Posicion:byte);//(+/-) 32 bits
begin
  SetBitPtr(@Variable,Valor,Posicion,32);
end;

function GetBitEn(Variable:Cardinal;Posicion:Byte):Byte;//32 bits
begin
  Result:=GetBitPtr(@Variable,Posicion,32);
end;

function GetBitEn(Variable:Integer;Posicion:Byte):Byte;//(+/-) 32 bits
begin
  Result:=GetBitPtr(@Variable,Posicion,32);
end;


//64 bits---------------------------------------------------------------------------------------------------------------


procedure SetBitEn(var Variable:Int64;Valor,Posicion:byte);overload;//(+/-) 64 bits
begin
  if Posicion>(63) then raise ERangeError.Create('Acceso a bit fuera de rango,');

  if Valor=1 then
    Variable:=Variable or (1 shl Posicion)
  else
    Variable:=Variable and ((1 shl Posicion) xor $FFFFFFFFFFFF);
end;


function GetBitEn(Variable:Int64;Posicion:Byte):Byte;//(+/-) 64 bits
begin
  Result:=GetBitPtr(@Variable,Posicion,64);
end;

end.
