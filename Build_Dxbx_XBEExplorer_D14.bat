@echo off

:: Use local variables to prevent a mess in the environment variables
setlocal

echo --- Building Dxbx ---
set DEFINES=""
set SEARCH_PATH="..\..\src;..\..\Libraries\jwapi\Win32API;..\..\Libraries\Jcl\windows;..\..\Libraries\Jcl\common;..\..\Libraries\DirectX9"
set INCLUDE_PATH="..\..\src\include;..\..\libraries\jcl\include;..\..\Libraries\jwapi\Includes"

cd projects
cd d14

dcc32 -$D+ -$L- -W -H- -B -Q -GD -E"..\..\bin" -N"DCU" -R"Resources" -U"Units" XBEExplorer.dpr -D%DEFINES% -U%SEARCH_PATH% -I%INCLUDE_PATH% -N0..\..\dcu

