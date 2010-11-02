@echo off
SET ORG_PATH=%CD%
cd ..\..\..\
build.bat %ORG_PATH%\hook1 lib\ntoskrnl.lib
cd %ORG_PATH%