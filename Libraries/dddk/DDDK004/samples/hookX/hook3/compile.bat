@echo off
SET ORG_PATH=%CD%
cd ..\..\..
compile.bat %ORG_PATH%\hook3.pas
cd %ORG_PATH%