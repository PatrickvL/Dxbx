/*
 driver loader is used to install the driver via service manager 
 it also sends some commands to driver
 usage is simple: load driver_name driver_display_name path
 you can use loadhook.bat 

 the code is very simple here and this is one way how to load the driver 
 there is not much to explain if you are familiar with windows services 
*/

#include <windows.h>
#include <stdio.h>
#include <ddk/ntddk.h>
#include "drvcomm.h"

int main(int argc,char **argv)
{
  printf("Opening service manager ...\n");
  HANDLE scm=OpenSCManager(NULL,NULL,SC_MANAGER_CREATE_SERVICE);
  
  if(scm)
  {
    printf("Creating service ...\n");

    HANDLE svc=CreateService(scm,argv[1],argv[2],SERVICE_START | DELETE | SERVICE_STOP,SERVICE_KERNEL_DRIVER,
                             SERVICE_DEMAND_START,SERVICE_ERROR_IGNORE,argv[3],NULL,NULL,NULL,NULL,NULL);

    if(!svc)
    {
      printf("Creating failed, trying to open service ...\n");
      svc=OpenService(scm,argv[1],SERVICE_START | DELETE | SERVICE_STOP);
    }

    if(svc)
    {
      printf("Starting service\n");

      StartService(svc,0,NULL);

      printf("Creating communication device ...\n");

      char buf[256];

      /*
       for MSTS we need to use Global namespace to access our device
      */
      if ((GetVersion()&0xFF)>=5) sprintf(buf,"\\\\.\\Global\\%s\0",argv[1]);
      else sprintf(buf,"\\\\.\\%s\0",argv[1]);

      HANDLE dev=CreateFile(buf,GENERIC_READ | GENERIC_WRITE,0,NULL,OPEN_EXISTING,0,NULL);
      
      if (dev==INVALID_HANDLE_VALUE) dev=0;
      if (dev)
      {
        printf("Press Enter to send IOCTL_HOOK_START\n");
        getchar();

        DRVCOMM_REQUEST_BUFFER buf_req;
        DRVCOMM_RESPONSE_BUFFER buf_res;
        DWORD bytes=0,ret;

        /*
         send command to our driver, no input buffer is used here
         output buffer contains status value only
        */
        memset(&buf_res,0,sizeof(buf_res));
        ret=DeviceIoControl(dev,IOCTL_HOOK_START,NULL,0,&buf_res,sizeof(buf_res),&bytes,NULL);
        printf("DeviceIoControl returned code 0x%.8X, status 0x%.8X and %d bytes\n",ret,buf_res.status,bytes);

        if (buf_res.status)
        {
          bytes=0;
          printf("Press Enter to send IOCTL_PROTECT_PROCESS with our pid\n");
          getchar();

          buf_req.parameters.protect_process.pid=GetCurrentProcessId();
          buf_req.parameters.protect_process.enable=TRUE;
          ret=DeviceIoControl(dev,IOCTL_PROTECT_PROCESS,&buf_req,sizeof(buf_req),&buf_res,sizeof(buf_res),&bytes,NULL);
          printf("DeviceIoControl returned code 0x%.8X, status 0x%.8X and %d bytes\n",ret,buf_res.status,bytes);


          bytes=0;
          printf("Press Enter to send IOCTL_HOOK_STOP\n");
          getchar();

          ret=DeviceIoControl(dev,IOCTL_HOOK_STOP,NULL,0,&buf_res,sizeof(buf_res),&bytes,NULL);
          printf("DeviceIoControl returned code 0x%.8X, status 0x%.8X and %d bytes\n",ret,buf_res.status,bytes);
        }

        printf("Press Enter to send close device handle\n");
        getchar();
        CloseHandle(dev);
      } else printf("error: unable to create communication device\n");

      printf("Press Enter to stop and delete service \n");
      getchar();

      SERVICE_STATUS status;
      ControlService(svc,SERVICE_CONTROL_STOP,&status);

      DeleteService(svc);
      CloseServiceHandle(svc);
    } else printf("error: unable to create/open service\n");

    CloseServiceHandle(scm);
  } else printf("error: unable to open manager\n");
  
  return 0;
}


