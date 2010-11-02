@echo off
bin\omf2d.exe %1.obj 2>nul
bin\omf2d.exe inc\DDDK.obj 2>nul
rem FOR EVERY SUPPORTED FUNCTION ONE CALL TO OMF2D MUST BE HERE
bin\omf2d.exe inc\DDDK.obj /U- /CEDbgPrint=_DbgPrint 2>nul
bin\omf2d.exe inc\DDDK.obj /U- /CEIoCreateDevice=_IoCreateDevice@28 2>nul
bin\omf2d.exe inc\DDDK.obj /U- /CEIoCompleteRequest=_IoCompleteRequest@8 2>nul
bin\omf2d.exe inc\DDDK.obj /U- /CEIoDeleteDevice=_IoDeleteDevice@4 2>nul
bin\omf2d.exe inc\DDDK.obj /U- /CEIoCreateSymbolicLink=_IoCreateSymbolicLink@8 2>nul
bin\omf2d.exe inc\DDDK.obj /U- /CEIoDeleteSymbolicLink=_IoDeleteSymbolicLink@4 2>nul
bin\omf2d.exe inc\DDDK.obj /U- /CERtlInitUnicodeString=_RtlInitUnicodeString@8 2>nul
bin\omf2d.exe inc\DDDK.obj /U- /CEInterlockedExchange=@InterlockedExchange@8 2>nul
bin\omf2d.exe inc\DDDK.obj /U- /CEZwOpenProcess=_ZwOpenProcess@16 2>nul
bin\omf2d.exe inc\DDDK.obj /U- /CEKeServiceDescriptorTable=_KeServiceDescriptorTable 2>nul
bin\omf2d.exe inc\DDDK.obj /U- /CEProbeForRead=_ProbeForRead@12 2>nul
bin\omf2d.exe inc\DDDK.obj /U- /CEExFreePool=_ExFreePool@4 2>nul
bin\omf2d.exe inc\DDDK.obj /U- /CEKeWaitForSingleObject=_KeWaitForSingleObject@20 2>nul 
bin\omf2d.exe inc\DDDK.obj /U- /CEExAllocatePool=_ExAllocatePool@8 2>nul
bin\omf2d.exe inc\DDDK.obj /U- /CEExAllocatePoolWithQuota=_ExAllocatePoolWithQuota@8 2>nul
bin\omf2d.exe inc\DDDK.obj /U- /CEExAllocatePoolWithTag=_ExAllocatePoolWithTag@12 2>nul
bin\omf2d.exe inc\DDDK.obj /U- /CEKeInitializeMutex=_KeInitializeMutex@8 2>nul
bin\omf2d.exe inc\DDDK.obj /U- /CEKeReleaseMutex=_KeReleaseMutex@8 2>nul

bin\link.exe /NOLOGO /ALIGN:32 /BASE:0x10000 /SUBSYSTEM:NATIVE /DRIVER /FORCE:UNRESOLVED /FORCE:MULTIPLE /ENTRY:DriverEntry inc\DDDK.obj %1.obj %3 %4 %5 %6 %7 %8 %9 /out:%1.sys %2
