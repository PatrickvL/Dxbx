unit Hooking;

interface

uses DDDK;

var
 HookActive:Boolean;

//this works only for original unpatched functions
function SystemService(AFunc:Pointer):PLONG;

function HookingHook:Integer; stdcall;
function HookingUnhook:Integer; stdcall;

implementation

uses HookedProc;

//var
// KeServiceDescriptorTable:TServiceDescriptorEntry;

function SystemService(AFunc:Pointer):PLONG;
begin
 Result:=PLONG(Cardinal(KeServiceDescriptorTable^.ServiceTableBase)+SizeOf(ULONG)*PULONG(ULONG(AFunc)+1)^);
end;


function HookingHook:Integer; stdcall;
begin
 DbgMsg('hooking.pas: HookingHook()',[]);

 if HookActive then
 begin
  DbgMsg('hooking.pas: HookingHook error: hook is already active',[]);
  DbgMsg('hooking.pas: HookingHook(-):False',[]);
  Result:=Integer(False);
  Exit;
 end;

 // 
 // now we are changing SDT
 // we have to use atomic operations because of multiprocessor machines
 //
 // we also have to disable WP bit for WXP SP2 and higher
 // to be able to write to SDT
 //

 asm                                            //disable WP bit
  mov eax,cr0                                   //move CR0 register into EAX
  and eax,not 000010000h                        //disable WP bit 
  mov cr0,eax                                   //write register back
 end;

 OldZwOpenProcess:=TZwOpenProcess(InterlockedExchange(SystemService(ZwOpenProcessAddr),LONG(@NewZwOpenProcess)));

 asm                                            //enable WP bit
  mov eax,cr0                                   //move CR0 register into EAX
  or eax,000010000h                             //enable WP bit         
  mov cr0,eax                                   //write register back           
 end;

 DbgMsg('OldZwOpenProcess=0x%.8X',[@OldZwOpenProcess]);
 DbgMsg('ZwOpenProcess=0x%.8X',[SystemService(ZwOpenProcessAddr)^]);

 HookActive:=True;

 Result:=Integer(True);

 DbgMsg('hooking.pas: HookingHook(-):True',[]);
end;


function HookingUnhook:Integer; stdcall;
begin
 DbgMsg('hooking.pas: HookingUnhook()',[]);

 if not HookActive then
 begin
  DbgMsg('hooking.pas: HookingUnhook error: hook is not active',[]);
  DbgMsg('hooking.pas: HookingUnhook(-):False',[]);
  Result:=Integer(False);
  Exit;
 end;

 DbgMsg('NewZwOpenProcess=0x%.8X',[@NewZwOpenProcess]);


 //
 // similar actions as in hooking_hook()
 //

 asm                                            //disable WP bit
  mov eax,cr0                                   //move CR0 register into EAX
  and eax,not 000010000h                        //disable WP bit 
  mov cr0,eax                                   //write register back
 end;

 InterlockedExchange(SystemService(ZwOpenProcessAddr),LONG(@OldZwOpenProcess));

 asm                                          //enable WP bit
  mov eax,cr0                                 //move CR0 register into EAX
  or eax,000010000h                           //enable WP bit         
  mov cr0,eax                                 //write register back           
 end;

 DbgMsg('ZwOpenProcess=0x%.8X',[SystemService(ZwOpenProcessAddr)^]);

 HookActive:=False;

 Result:=Integer(True);

 DbgMsg('hooking.pas: HookingUnhook(-):True',[]);
end;

end.
