unit DrvComm;

interface

uses DDDK;

const 
 DeviceName='\Device\hook2';
 DosDeviceName='\DosDevices\hook2';

//IO control codes
//HOOK_START initiates hooks
 IOCTL_HOOK_START       = $0022E000; //CTL_CODE(FILE_DEVICE_UNKNOWN,0x800,METHOD_BUFFERED,FILE_READ_DATA | FILE_WRITE_DATA)
//HOOK_STOP unhooks hooked functions
 IOCTL_HOOK_STOP        = $0022E004; //CTL_CODE(FILE_DEVICE_UNKNOWN,0x801,METHOD_BUFFERED,FILE_READ_DATA | FILE_WRITE_DATA)

type
 PDrvCommBuffer=^TDrvCommBuffer;
 TDrvCommBuffer=packed record
  Status:ULONG;
 end;
 DRVCOMM_BUFFER=TDrvCommBuffer;
 PDRVCOMM_BUFFER=^DRVCOMM_BUFFER;


implementation


end.