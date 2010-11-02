#ifndef __DRVCOMM_H__
#define __DRVCOMM_H__

//device names
#define DEVICE_NAME             L"\\Device\\hook2"
#define DOS_DEVICE_NAME         L"\\DosDevices\\hook2"


//IO control codes
//HOOK_START initiates hooks
#define IOCTL_HOOK_START CTL_CODE(FILE_DEVICE_UNKNOWN,0x800,METHOD_BUFFERED,FILE_READ_DATA | FILE_WRITE_DATA)
//HOOK_STOP unhooks hooked functions
#define IOCTL_HOOK_STOP  CTL_CODE(FILE_DEVICE_UNKNOWN,0x801,METHOD_BUFFERED,FILE_READ_DATA | FILE_WRITE_DATA)

//driver/app communication buffer
typedef struct DRVCOMM_BUFFER
{
  ULONG status;
} DRVCOMM_BUFFER,*PDRVCOMM_BUFFER;

#endif
