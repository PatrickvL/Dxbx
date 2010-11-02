unit DrvComm;

interface

uses DDDK;

const 
 DeviceName='\Device\hook3';
 DosDeviceName='\DosDevices\hook3';

//IO control codes
//HOOK_START initiates hooks
 IOCTL_HOOK_START       = $0022E000; //CTL_CODE(FILE_DEVICE_UNKNOWN,0x800,METHOD_BUFFERED,FILE_READ_DATA | FILE_WRITE_DATA)
//HOOK_STOP unhooks hooked functions
 IOCTL_HOOK_STOP        = $0022E004; //CTL_CODE(FILE_DEVICE_UNKNOWN,0x801,METHOD_BUFFERED,FILE_READ_DATA | FILE_WRITE_DATA)
//PROTECT_PROCESS adds/removes PID from rules
 IOCTL_PROTECT_PROCESS  = $0022E200; //CTL_CODE(FILE_DEVICE_UNKNOWN,0x880,METHOD_BUFFERED,FILE_READ_DATA | FILE_WRITE_DATA)

type
 TDrvCommRequestBufferStructParametersUnionProtectProcess=packed record
  Pid:ULONG;
  Enable:Integer;
 end;

 TDrvCommRequestBufferStructParameters=packed record
  case Byte of 
   0:(ProtectProcess:TDrvCommRequestBufferStructParametersUnionProtectProcess);
 end;

 PDrvCommRequestBuffer=^TDrvCommRequestBuffer;
 TDrvCommRequestBuffer=packed record
  Parameters:TDrvCommRequestBufferStructParameters;
 end;

 PDrvCommResponseBuffer=^TDrvCommResponseBuffer;
 TDrvCommResponseBuffer=packed record
  Status:ULONG;
 end;


implementation


end.
