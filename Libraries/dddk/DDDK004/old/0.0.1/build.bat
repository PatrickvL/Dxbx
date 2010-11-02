bin\omf2d.exe driver.obj
bin\link.exe /NOLOGO /ALIGN:32 /BASE:0x10000 /SUBSYSTEM:NATIVE /DRIVER /FORCE:UNRESOLVED /FORCE:MULTIPLE /ENTRY:DriverEntry driver.obj /out:driver.sys lib\ntoskrnl.lib
