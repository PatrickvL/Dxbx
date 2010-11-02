unit HookedProc;

interface

uses DDDK;

type
 TZwOpenProcess=function(ProcessHandle:PHandle;DesiredAccess:TAccessMask;ObjectAttributes:PObjectAttributes;ClientId:PClientId):NTSTATUS; stdcall;

var
 OldZwOpenProcess:TZwOpenProcess=nil;


function NewZwOpenProcess(ProcessHandle:PHandle;DesiredAccess:TAccessMask;ObjectAttributes:PObjectAttributes;ClientId:PClientId):NTSTATUS; stdcall;


implementation 

uses Func;

//
// our implementation of ZwOpenProcess, this time we only log it and call 
// original code, run taskman after it is hooked and watch DebugView
//

function NewZwOpenProcess(ProcessHandle:PHandle;DesiredAccess:TAccessMask;ObjectAttributes:PObjectAttributes;ClientId:PClientId):NTSTATUS; stdcall;
var
 LCidValid:Boolean;
begin
 DbgMsg('hookedproc.pas: NewZwOpenProcess(ProcessHandle:0x%.8X,DesiredAccess:0x%.8X,ObjectAttributes:0x%.8X,ClientId:0x%.8X)',
         [ProcessHandle,DesiredAccess,ObjectAttributes,ClientId]);

 LCidValid:=FuncIsGoodReadPtr(ClientId,SizeOf(TClientId));
 if LCidValid then
 begin
  DbgMsg('hookedproc.cpp: NewZwOpenProcess ClientId^.UniqueProcess=0x%.8X',[ClientId^.UniqueProcess]);
  DbgMsg('hookedproc.cpp: NewZwOpenProcess ClientId^.UniqueThread=0x%.8X',[ClientId^.UniqueThread]);
 end;

 Result:=OldZwOpenProcess(ProcessHandle,DesiredAccess,ObjectAttributes,ClientId);

 DbgMsg('hookedproc.cpp: NewZwOpenProcess(-):0x%.8X',[Result]);
end;

end.
