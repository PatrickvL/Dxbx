@echo off
SET ORG_PATH=%CD%
cd ..\..\..\
rem FOR EVERY UNIT ONE CALL OMF LINE
call omf.bat %ORG_PATH%\hookedproc
call omf.bat %ORG_PATH%\drvcomm
call omf.bat %ORG_PATH%\hooking
call omf.bat %ORG_PATH%\func
rem FOR MAIN UNIT ONE BUILD LINE
build.bat %ORG_PATH%\hook2 lib\ntoskrnl.lib %ORG_PATH%\hookedproc.obj %ORG_PATH%\hooking.obj %ORG_PATH%\func.obj
cd %ORG_PATH%