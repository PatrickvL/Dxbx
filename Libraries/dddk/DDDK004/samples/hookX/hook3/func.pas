unit Func;

interface

uses DDDK;

const
//rules type
 RULE_TYPE_PROCESS_PROTECTION   = $00000001;

//errors
 ERROR_RULE_EXISTS              = $80000001;
 ERROR_RULE_DOES_NOT_EXIST      = $80000002;

type
//structure of rules list
 TFuncRuleStructProcess=packed record
  Pid:ULONG;
 end;

 PFuncRule=^TFuncRule;
 TFuncRule=packed record
  Prev,Next:PFuncRule;
  frType:Cardinal;
  case Byte of 
   0:(Process:TFuncRuleStructProcess);
 end;

//device extenstion structure
 PDeviceExtension=^TDeviceExtension;
 TDeviceExtension=packed record
  RulesMutex:TKMutex;
  FirstRule,LastRule:PFuncRule;
 end;

function FuncProtectProcess(ADevExt:PDeviceExtension;APid:ULONG;AEnable:Integer):Integer;
function FuncIsGoodReadPtr(ABuf:Pointer;ASize:Cardinal):Boolean;
function FuncFreeList(ADevExt:PDeviceExtension):Boolean;

implementation

//
// find rule returns pointer to rule object in the list of rules if appropriate
// rule with given type and id is found
//

function FuncFindRule(ADevExt:PDeviceExtension;AType:Cardinal;AId:ULONG):PFuncRule;
var
 LItem:PFuncRule;
begin
 DbgMsg('func.pas: FuncFindRule(ADevExt:0x%.8X;AType:0x%.8X;AId:0x%.8X)',[ADevExt,AType,AId]);

 Result:=nil;
 LItem:=ADevExt^.FirstRule;
 while LItem<>nil do
 begin
  if (LItem^.frType=AType) and (LItem^.Process.Pid=AId) then
  begin
   Result:=LItem;
   Break;
  end;

  LItem:=LItem^.Next;
 end;
 DbgMsg('func.pas: FuncFindRule(-):0x%.8X',[Result]);
end;


//
// add rule adds rule to the end of the list
//

function FuncAddRule(ADevExt:PDeviceExtension;ARule:PFuncRule):Boolean;
begin
 DbgMsg('func.pas: FuncAddRule(ADevExt:0x%.8X;ARule:0x%.8X)',[ADevExt,ARule]);

 Result:=True;

 if ADevExt^.LastRule<>nil then ADevExt^.LastRule^.Next:=ARule
 else ADevExt^.FirstRule:=ARule;

 ARule^.Prev:=ADevExt^.LastRule;
 Arule^.Next:=nil;
 ADevExt^.LastRule:=ARule;
 
 DbgMsg('func.pas: FuncAddRule(-):True',[]);
end;


//
// del rule deletes rule from the list
//

function FuncDelRule(ADevExt:PDeviceExtension;ARule:PFuncRule):Boolean;
begin
 DbgMsg('func.pas: FuncDelRule(ADevExt:0x%.8X;ARule:0x%.8X)',[ADevExt,ARule]);

 Result:=True;

 if ARule^.Next<>nil then ARule^.Next^.Prev:=ARule^.Prev
 else ADevExt^.LastRule:=ARule^.Prev;

 if ARule^.Prev<>nil then ARule^.Prev^.Next:=ARule^.Next
 else ADevExt^.FirstRule:=ARule^.Next;

 ExFreePool(ARule);

 DbgMsg('func.pas: FuncDelRule(-):True',[]);
end;


//
// free list deletes whole rule list and frees memory
//

function FuncFreeList(ADevExt:PDeviceExtension):Boolean;
var
 LStatus:NTSTATUS;
begin
 DbgMsg('func.pas: FuncFreeList(ADevExt:0x%.8X)',[ADevExt]);

 //
 // write access to the shared memory always requires protection 
 // we will use mutex to make such synchronization here
 //
 LStatus:=KeWaitForMutexObject(@ADevExt^.RulesMutex,Executive,KernelMode,False,nil);

 Result:=False;
 if not NT_SUCCESS(LStatus) then
 begin
  DbgMsg('func.pas: FuncFreeList error: KeWaitForMutexObject failed with status 0x%.8X',[LStatus]);
  DbgMsg('func.pas: FuncFreeList(-):False',[]);
  Exit;
 end;

 while ADevExt^.FirstRule<>nil do
  FuncDelRule(ADevExt,ADevExt^.FirstRule);

 KeReleaseMutex(@ADevExt^.RulesMutex,False);

 DbgMsg('func.pas: FuncFreeList(-):True',[]);
end;


{$IFDEF DEBUG}
//
// print rules prints all rules as debug messages 
//

procedure FuncPrintRules(ADevExt:PDeviceExtension);
var
 LRule:PFuncRule;
 LI:Integer;
begin
 DbgMsg('func.pas: FuncPrintRules(ADevExt:0x%.8X)',[ADevExt]);

 LRule:=ADevExt^.FirstRule;

 LI:=0;
 while LRule<>nil do
 begin
  case LRule^.frType of
   RULE_TYPE_PROCESS_PROTECTION:DbgMsg('%.3d) Addr:0x%.8X, frType:PROC PROT, Pid:%d',[LI,LRule,LRule^.Process.Pid]);
   else DbgMsg('%.3d) unknown rule',[LI]);
  end;

  LRule:=LRule^.Next;
  Inc(LI);
 end;

 DbgMsg('func.pas: FuncPrintRules(-)',[]);
end;
{$ENDIF}


//
// protect process manages list of rules 
// it enables/disables protection rule for specific process id
//

function FuncProtectProcess(ADevExt:PDeviceExtension;APid:ULONG;AEnable:Integer):Integer;
var
 LStatus:NTSTATUS;
 LType:Cardinal;
 LRule:PFuncRule;
begin
 DbgMsg('func.pas: FuncProtectProcess(ADevExt:0x%.8X,APid:0x%.8X,AEnable:%d)',[ADevExt,APid,AEnable]);

 //
 // write access to the shared memory always requires protection 
 // we will use mutex to make such synchronization here
 //
 LStatus:=KeWaitForMutexObject(@ADevExt^.RulesMutex,Executive,KernelMode,False,nil);

 Result:=Integer(False);

 if not NT_SUCCESS(LStatus) then
 begin
  DbgMsg('func.pas: FuncProtectProcess error: KeWaitForMutexObject failed with status 0x%.8X',[LStatus]);
  DbgMsg('func.pas: FuncProtectProcess(-):False',[]);
  Exit;
 end;


 LType:=RULE_TYPE_PROCESS_PROTECTION;

 LRule:=FuncFindRule(ADevExt,LType,APid);

 if Boolean(AEnable) then
 begin
  if LRule=nil then
  begin
   LRule:=ExAllocatePool(PagedPool,SizeOf(TFuncRule));
   if LRule<>nil then
   begin
    LRule^.frType:=LType;
    LRule^.Process.Pid:=APid;
    Result:=Integer(FuncAddRule(ADevExt,LRule));
   end else DbgMsg('func.pas: FuncProtectProcess error: ExAllocatePool failed',[]);
  end else
  begin
   DbgMsg('func.pas: FuncProtectProcess error: rule exists',[]);
   Result:=Integer(ERROR_RULE_EXISTS);
  end;
 end else
 begin
  if LRule=nil then
  begin
   DbgMsg('func.pas: FuncProtectProcess error: rule does not exist',[]);
   Result:=Integer(ERROR_RULE_DOES_NOT_EXIST);
  end else Result:=Integer(FuncDelRule(ADevExt,LRule));
 end;

{$IFDEF DEBUG}
 FuncPrintRules(ADevExt);
{$ENDIF}

 KeReleaseMutex(@ADevExt^.RulesMutex,False);
 DbgMsg('func.pas: FuncProtectProcess(-):%d',[Integer(Result)]);
end;


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
