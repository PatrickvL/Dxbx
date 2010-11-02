//
// hook1 is basic driver with unload support, logs every action too DebugView
// enjoy your first lesson
//
unit hook1;

interface

//
// the most important unit is DDDK which source is in inc directory
// it contains everything we need to work with kernel functions
//
uses DDDK;

const 
 DeviceName='\Device\hook1';
 DosDeviceName='\DosDevices\hook1';


//
// this is the must, when one say you can name your driver entry with your name 
// in DDK, you have to leave _DriverEntry name of entry in DDDK unless you know
// what you are dealing with
//
function _DriverEntry(ADriverObject:PDriverObject;ARegistryPath:PUnicodeString):NTSTATUS; stdcall;

function Hook1Create(ADeviceObject:PDeviceObject;AIrp:PIrp):NTSTATUS; stdcall;
function Hook1Close(ADeviceObject:PDeviceObject;AIrp:PIrp):NTSTATUS; stdcall;
function Hook1DeviceControl(ADeviceObject:PDeviceObject;AIrp:PIrp):NTSTATUS; stdcall;
procedure Hook1Unload(ADriverObject:PDriverObject); stdcall;


implementation

var
// dos device name is global variable because we use it in unload too,
// we can always make another RtlInitUnicodeString if we don't like global vars
 DosDevName:TUnicodeString;


//
// create function is called everytime CreateFile is called on our device 
//
function Hook1Create(ADeviceObject:PDeviceObject;AIrp:PIrp):NTSTATUS; stdcall;
begin
 DbgMsg('hook1.pas: Hook1Create(ADeviceObject:0x%.8X,AIrp:0x%.8X)',[ADeviceObject,AIrp]);

 Result:=STATUS_SUCCESS;
 AIrp^.IoStatus.Status:=Result;
 IoCompleteRequest(AIrp,IO_NO_INCREMENT);

 DbgMsg('hook1.pas: Hook1Create(-):0x%.8X)',[Result]);
end;


//
// close function is called everytime CloseHandle is called on our device
// close is associated with IRP_MJ_CLOSE and it is NOT executed in the context 
// of the CloseHandle caller, if we want to make some cleanup in that context 
// we rather associate cleanup function with IRP_MJ_CLEANUP
//

function Hook1Close(ADeviceObject:PDeviceObject;AIrp:PIrp):NTSTATUS; stdcall;
begin
 DbgMsg('hook1.pas: Hook1Close(ADeviceObject:0x%.8X,AIrp:0x%.8X)',[ADeviceObject,AIrp]);

 Result:=STATUS_SUCCESS;
 AIrp^.IoStatus.Status:=Result;
 IoCompleteRequest(AIrp,IO_NO_INCREMENT);

 DbgMsg('hook1.pas: Hook1Close(-):0x%.8X)',[Result]);
end;


//
// device control function is called everytime DeviceIoControl is called on our
// device, it is common way how user mode app communicate with driver
//

function Hook1DeviceControl(ADeviceObject:PDeviceObject;AIrp:PIrp):NTSTATUS; stdcall;
begin
 DbgMsg('hook1.pas: Hook1DeviceControl(ADeviceObject:0x%.8X,AIrp:0x%.8X)',[ADeviceObject,AIrp]);

 Result:=STATUS_SUCCESS;
 AIrp^.IoStatus.Status:=Result;
 IoCompleteRequest(AIrp,IO_NO_INCREMENT);
 
 DbgMsg('hook1.pas: Hook1DeviceControl(-):0x%.8X)',[Result]);
end;


//
// unload is called when driver is being unloaded, if we do not implement unload
// function them our driver can't be unloaded dynamically
//

procedure Hook1Unload(ADriverObject:PDriverObject); stdcall;
begin
 DbgMsg('hook1.pas: Hook1Unload(ADriverObject:0x%.8X)',[ADriverObject]);
  
 //cleanup everything our driver created - delete symlink and device 
 IoDeleteSymbolicLink(@DosDevName);
 IoDeleteDevice(ADriverObject^.DeviceObject);

 DbgMsg('hook1.pas: Hook1Unload(-)',[]);
end;


//
// DriverEntry is common driver entry point
//
function _DriverEntry(ADriverObject:PDriverObject;ARegistryPath:PUnicodeString):NTSTATUS; stdcall;
var
 LDevName:TUnicodeString;
 LDevObj:PDeviceObject;
begin
 DbgMsg('hook1.pas: DriverEntry(ADriverObject:0x%.8X;ARegistryPath:0x%.8X)',[ADriverObject,ARegistryPath]);

 RtlInitUnicodeString(@LDevName,DeviceName);
 RtlInitUnicodeString(@DosDevName,DosDeviceName); 

 //
 // if we want our driver to be accessible we need to create device for it, 
 // one driver can have more devices 
 //
 Result:=IoCreateDevice(ADriverObject,0,@LDevName,FILE_DEVICE_UNKNOWN,FILE_DEVICE_SECURE_OPEN,FALSE,LDevObj);

 if NT_SUCCESS(Result) then
 begin
   //
   // for some selected major functions we set handlers
   //
   ADriverObject^.MajorFunction[IRP_MJ_CREATE]          := @Hook1Create;
   ADriverObject^.MajorFunction[IRP_MJ_CLOSE]           := @Hook1Close;
   ADriverObject^.MajorFunction[IRP_MJ_DEVICE_CONTROL]  := @Hook1DeviceControl;
   ADriverObject^.DriverUnload                          := @Hook1Unload;

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
    DbgMsg('hook1.pas: DriverEntry.IoCreateSymbolicLink failed with status 0x%.8X',[Result]);
    IoDeleteDevice(ADriverObject^.DeviceObject);
   end;
 end else DbgMsg('hook1.pas: DriverEntry.IoCreateDevice failed with status 0x%.8X',[Result]);

 DbgMsg('hook1.pas: DriverEntry(-):0x%.8X',[Result]);
end;

end.
