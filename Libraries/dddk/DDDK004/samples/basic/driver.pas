//
// first of all this is unit not program
// we need some exports to be working e.g. _DriverEntry - see below 
//
unit driver;

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
function _DriverEntry(DriverObject:PDriverObject;RegistryPath:PUnicodeString):NTSTATUS; stdcall;


implementation

//
// unload is called when driver is being unloaded, if we do not implement unload
// function them our driver can't be unloaded dynamically
//
procedure DriverUnload(DriverObject:PDriverObject); stdcall;
begin
 DbgPrint('DriverUnload(DriverObject:0x%.8X)',[DriverObject]);
 DbgPrint('DriverUnload(-)',[]);
end;


//
// DriverEntry is common driver entry point
//
function _DriverEntry(DriverObject:PDriverObject;RegistryPath:PUnicodeString):NTSTATUS; stdcall;
begin
 DbgPrint('DriverEntry(DriverObject:0x%.8X;RegistryPath:0x%.8X)',[DriverObject,RegistryPath]);

 DriverObject^.DriverUnload:=@DriverUnload;

 Result:=STATUS_SUCCESS;
 DbgPrint('DriverEntry(-):0x%.8X',[Result]);
end;

end.
