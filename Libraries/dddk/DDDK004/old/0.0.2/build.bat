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
bin\link.exe /NOLOGO /ALIGN:32 /BASE:0x10000 /SUBSYSTEM:NATIVE /DRIVER /FORCE:UNRESOLVED /FORCE:MULTIPLE /ENTRY:DriverEntry inc\DDDK.obj %1.obj /out:%1.sys %2
