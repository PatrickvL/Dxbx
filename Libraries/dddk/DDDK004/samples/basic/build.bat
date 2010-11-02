@echo off
SET ORG_PATH=%CD%
cd ..\..\
build.bat %ORG_PATH%\driver lib\ntoskrnl.lib
cd %ORG_PATH%