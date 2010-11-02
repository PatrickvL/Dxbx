@echo off
SET ORG_PATH=%CD%
cd ..\..\..
compile.bat %ORG_PATH%\hook2.pas DEBUG
cd %ORG_PATH%