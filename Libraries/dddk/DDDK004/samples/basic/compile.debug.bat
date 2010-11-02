@echo off
SET ORG_PATH=%CD%
cd ..\..
compile.bat %ORG_PATH%\driver.pas DEBUG
cd %ORG_PATH%