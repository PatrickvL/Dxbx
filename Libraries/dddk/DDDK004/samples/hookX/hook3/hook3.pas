//
// hook3 is driver with sample device extension and communication with user mode 
// application using DeviceIoControl, it maintain a list of rules for pids 
// in device extension but does nothing with the list yet
//
unit hook3;

interface

//
// the most important unit is DDDK which source is in inc directory
// it contains everything we need to work with kernel functions
//
uses DDDK;

//
// this is the must, when one say you can name your driver entry with your name 
// in DDK, you have to leave _DriverEntry name of entry in DDDK unless you know
// what you are dealing with
//
function _DriverEntry(ADriverObject:PDriverObject;ARegistryPath:PUnicodeString):NTSTATUS; stdcall;

function Hook3Create(ADeviceObject:PDeviceObject;AIrp:PIrp):NTSTATUS; stdcall;
function Hook3Close(ADeviceObject:PDeviceObject;AIrp:PIrp):NTSTATUS; stdcall;
function Hook3DeviceControl(ADeviceObject:PDeviceObject;AIrp:PIrp):NTSTATUS; stdcall;
procedure Hook3Unload(ADriverObject:PDriverObject); stdcall;


implementation

uses Hooking,DrvComm,Func;

var
// dos device name is global variable because we use it in unload too,
// we can always make another RtlInitUnicodeString if we don't like global vars
 DosDevName:TUnicodeString;


//
// create function is called everytime CreateFile is called on our device 
//
function Hook3Create(ADeviceObject:PDeviceObject;AIrp:PIrp):NTSTATUS; stdcall;
begin
 DbgMsg('hook3.pas: Hook3Create(ADeviceObject:0x%.8X,AIrp:0x%.8X)',[ADeviceObject,AIrp]);

 Result:=STATUS_SUCCESS;
 AIrp^.IoStatus.Status:=Result;
 IoCompleteRequest(AIrp,IO_NO_INCREMENT);

 DbgMsg('hook3.pas: Hook3Create(-):0x%.8X)',[Result]);
end;


//
// close function is called everytime CloseHandle is called on our device
// close is associated with IRP_MJ_CLOSE and it is NOT executed in the context 
// of the CloseHandle caller, if we want to make some cleanup in that context 
// we rather associate cleanup function with IRP_MJ_CLEANUP
//

function Hook3Close(ADeviceObject:PDeviceObject;AIrp:PIrp):NTSTATUS; stdcall;
begin
 DbgMsg('hook3.pas: Hook3Close(ADeviceObject:0x%.8X,AIrp:0x%.8X)',[ADeviceObject,AIrp]);

 Result:=STATUS_SUCCESS;
 AIrp^.IoStatus.Status:=Result;
 IoCompleteRequest(AIrp,IO_NO_INCREMENT);

 DbgMsg('hook3.pas: Hook3Close(-):0x%.8X)',[Result]);
end;


//
// device control function is called everytime DeviceIoControl is called on our
// device, it is common way how user mode app communicate with driver
//

function Hook3DeviceControl(ADeviceObject:PDeviceObject;AIrp:PIrp):NTSTATUS; stdcall;
var 
 LStack:PIoStackLocation;
 LBufIn,LBufOut:Pointer;
 LBufInLen,LBufOutLen,LCode,LRet:ULONG;
 LDevExt:PDeviceExtension;
 LBufReq:PDrvCommRequestBuffer;
 LBufRes:PDrvCommResponseBuffer;
begin
 DbgMsg('hook3.pas: Hook3DeviceControl(ADeviceObject:0x%.8X,AIrp:0x%.8X)',[ADeviceObject,AIrp]);

 LStack:=IoGetCurrentIrpStackLocation(AIrp);

 Result:=STATUS_SUCCESS;
 AIrp^.IoStatus.Information:=0;

 LCode:=LStack^.Parameters.DeviceIoControl.IoControlCode;

 //
 // for Buffered IO both input and output buffer are the same 
 // Irp->AssociatedIrp.SystemBuffer
 // 
 LBufIn:=AIrp^.AssociatedIrp.SystemBuffer;
 LBufOut:=LBufIn;
 LBufInLen:=LStack^.Parameters.DeviceIoControl.InputBufferLength;
 LBufOutLen:=LStack^.Parameters.DeviceIoControl.OutputBufferLength;

 DbgMsg('hook3.pas: Hook3DeviceControl: LCode:0x%.8X,LBufIn:0x%.8X,LBufInLen:0x%.8X,LBufOut:0x%.8X,LBufOutLen:0x%.8X',
        [LCode,LBufIn,LBufInLen,LBufOut,LBufOutLen]);  

 LBufReq:=LBufIn;
 LBufRes:=LBufOut;
 LDevExt:=ADeviceObject^.DeviceExtension;
 AIrp^.IoStatus.Information:=SizeOf(TDrvCommResponseBuffer);

 //
 // for every command we implement the functionality 
 //
 case LCode of 
  IOCTL_HOOK_START:begin
   LRet:=HookingHook;
   LBufRes^.Status:=LRet;
  end;

  IOCTL_HOOK_STOP:begin
   LRet:=HookingUnhook;
   LBufRes^.Status:=LRet;
  end;

  IOCTL_PROTECT_PROCESS:begin
   LRet:=FuncProtectProcess(LDevExt,LBufReq^.Parameters.ProtectProcess.Pid,LBufReq^.Parameters.ProtectProcess.Enable);
   LBufRes^.Status:=LRet;
  end;

  //
  // unknown codes should also be handled
  //
  else
   Result:=STATUS_INVALID_DEVICE_REQUEST;
   AIrp^.IoStatus.Information:=0;
 end;

 AIrp^.IoStatus.Status:=Result;
 IoCompleteRequest(AIrp,IO_NO_INCREMENT);

 DbgMsg('hook3.pas: Hook3DeviceControl(-):0x%.8X)',[Result]);
end;


//
// unload is called when driver is being unloaded, if we do not implement unload
// function them our driver can't be unloaded dynamically
//

procedure Hook3Unload(ADriverObject:PDriverObject); stdcall;
begin
 DbgMsg('hook3.pas: Hook3Unload(ADriverObject:0x%.8X)',[ADriverObject]);
  
 HookingUnhook;

 //free memory we've allocated for rules
 FuncFreeList(ADriverObject^.DeviceObject^.DeviceExtension);

 //cleanup everything our driver created - delete symlink and device 
 IoDeleteSymbolicLink(@DosDevName);
 IoDeleteDevice(ADriverObject^.DeviceObject);

 DbgMsg('hook3.pas: Hook3Unload(-)',[]);
end;


//
// DriverEntry is common driver entry point
//
function _DriverEntry(ADriverObject:PDriverObject;ARegistryPath:PUnicodeString):NTSTATUS; stdcall;
var
 LDevName:TUnicodeString;
 LDevObj:PDeviceObject;
 LDevExt:PDeviceExtension;
begin
 DbgMsg('hook3.pas: DriverEntry(ADriverObject:0x%.8X;ARegistryPath:0x%.8X)',[ADriverObject,ARegistryPath]);

 RtlInitUnicodeString(@LDevName,DeviceName);
 RtlInitUnicodeString(@DosDevName,DosDeviceName); 

 //
 // if we want our driver to be accessible we need to create device for it, 
 // one driver can have more devices 
 //
 Result:=IoCreateDevice(ADriverObject,SizeOf(TDeviceExtension),@LDevName,FILE_DEVICE_UNKNOWN,FILE_DEVICE_SECURE_OPEN,FALSE,LDevObj);

 if NT_SUCCESS(Result) then
 begin
  //
  // for some selected major functions we set handlers
  //
  ADriverObject^.MajorFunction[IRP_MJ_CREATE]          := @Hook3Create;
  ADriverObject^.MajorFunction[IRP_MJ_CLOSE]           := @Hook3Close;
  ADriverObject^.MajorFunction[IRP_MJ_DEVICE_CONTROL]  := @Hook3DeviceControl;
  ADriverObject^.DriverUnload                          := @Hook3Unload;

  //
  // there are few types of contexts, driver context, device context, 
  // instance context etc. 
  // second parameter of IoCreateDevice specifies size of device context 
  // we will use this memory to store our global list of rules
  //
  LDevExt:=LDevObj^.DeviceExtension;

  if LDevExt<>nil then
  begin
   //
   // we use mutex as a synchronization mechanism to maintain our list
   //
   KeInitializeMutex(@LDevExt^.RulesMutex,0);
   LDevExt^.FirstRule:=nil;
   LDevExt^.LastRule:=nil;

   //
   // this selects the method of IO, we use buffered IO as it is comfortable 
   // and effective for smaller packets
   //
   LDevObj^.Flags:=LDevObj^.Flags or DO_BUFFERED_IO;

   //
   // if we want user mode application to communicate our driver we need to make 
   // a dos device link
   //
   Result:=IoCreateSymbolicLink(@DosDevName,@LDevName);
   if not NT_SUCCESS(Result) then
   begin
    DbgMsg('hook3.pas: DriverEntry.IoCreateSymbolicLink failed with status 0x%.8X',[Result]);
    IoDeleteDevice(ADriverObject^.DeviceObject);
   end;
  end else
  begin
   DbgMsg('hook3.pas: DriverEntry error: no device extension',[]);
   IoDeleteDevice(ADriverObject^.DeviceObject);
   Result:=STATUS_NO_SUCH_DEVICE;
  end;
 end else DbgMsg('hook3.pas: DriverEntry.IoCreateDevice failed with status 0x%.8X',[Result]);

 DbgMsg('hook3.pas: DriverEntry(-):0x%.8X',[Result]);
end;

end.
