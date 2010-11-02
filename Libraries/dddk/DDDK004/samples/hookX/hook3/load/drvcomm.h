#ifndef __DRVCOMM_H__
#define __DRVCOMM_H__

//IO control codes
//HOOK_START initiates hooks
#define IOCTL_HOOK_START        CTL_CODE(FILE_DEVICE_UNKNOWN,0x800,METHOD_BUFFERED,FILE_READ_DATA | FILE_WRITE_DATA)
//HOOK_STOP unhooks hooked functions
#define IOCTL_HOOK_STOP         CTL_CODE(FILE_DEVICE_UNKNOWN,0x801,METHOD_BUFFERED,FILE_READ_DATA | FILE_WRITE_DATA)
//PROTECT_PROCESS adds/removes PID from rules
#define IOCTL_PROTECT_PROCESS   CTL_CODE(FILE_DEVICE_UNKNOWN,0x880,METHOD_BUFFERED,FILE_READ_DATA | FILE_WRITE_DATA)

//driver/app communication buffers
typedef struct DRVCOMM_REQUEST_BUFFER
{
  union
  {
    struct
    {
      ULONG pid;                                  //pid of process to protect/disable protection for
      int enable;                                 //protect on true, disable protection otherwise
    } protect_process;
  } parameters;
} DRVCOMM_REQUEST_BUFFER,*PDRVCOMM_REQUEST_BUFFER;

typedef struct DRVCOMM_RESPONSE_BUFFER
{
  ULONG status;
} DRVCOMM_RESPONSE_BUFFER,*PDRVCOMM_RESPONSE_BUFFER;

#endif
