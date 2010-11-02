unit Func;

interface

uses DDDK;

function FuncIsGoodReadPtr(ABuf:Pointer;ASize:Cardinal):Boolean;

implementation

//
// this function checks user buffer for read access 
// returns true if the buffer is ok
//

function FuncIsGoodReadPtr(ABuf:Pointer;ASize:Cardinal):Boolean;
var
 LSum:Cardinal;
 LPC:PCardinal;
 LPB:PByte;
 LI:Integer;
begin
 DbgMsg('func.pas: FuncIsGoodReadPtr(ABuf:0x%.8X;ASize:0x%.8X)',[ABuf,ASize]);

 Result:=True;

 try 
  ProbeForRead(ABuf,ASize,SizeOf(Byte));
  LSum:=0;
  LPC:=ABuf;
  for LI:=1 to ASize div SizeOf(Cardinal) do
  begin
   Inc(LSum,LPC^);
   Inc(LPC);
  end;

  LPB:=Pointer(LPC);

  for LI:=1 to ASize mod SizeOf(Cardinal) do
  begin
   Inc(LSum,LPB^);
   Inc(LPB);
  end;
  if LSum=0 then ;
 except 
  DbgPrint('func.pas: FuncIsGoodReadPtr error: exception occurred',[]);
  Result:=False;
 end;

 DbgMsg('func.pas: FuncIsGoodReadPtr(-):%d',[Result]);
end;

end.
