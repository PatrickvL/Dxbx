@echo off 
CLS
CLS
echo ***********************************************
echo **                                           **
echo ** Dxbx Backup Iso process.                  **
echo **                                           **
echo ***********************************************


REM Remove old Backup.Iso
Del Backup.Iso

REM Remove Bin Directory
want\deltree /fBackup /s
MD Backup
MD Backup\Docs 
MD Backup\DUnit 
MD Backup\Libraries 
MD Backup\resources 
MD Backup\Setup 
MD Backup\src 
MD Backup\Tools 
MD Backup\Want 

REM Copy backup files to Backup folder
Copy *.* Backup
XCopy Docs Backup\Docs /e
XCopy DUnit Backup\DUnit /e
XCopy Libraries Backup\Libraries /e
XCopy resource Backup\resources /e
XCopy Setup Backup\Setup /e
XCopy src Backup\src /e
XCopy Tools Backup\Tools /e
XCopy Want Backup\Want /e



echo Create Backup.iso
Tools\MKIsoFs\mkisofs -o Backup.iso -q -l -N -r -J Backup\

pause