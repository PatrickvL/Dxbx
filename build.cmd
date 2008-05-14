@echo off 
CLS
CLS
echo ***********************************************
echo **                                           **
echo ** DXBX Build process.                       **
echo **                                           **
echo ***********************************************


echo Create Bin dir.
md Bin
echo Create Dcu dir.
md Dcu

echo Build Dxbx
want\want.exe -buildfile want\dxbx.xml 

echo Build CxbxKrnl.dll
want\want.exe -buildfile want\Cxbxkrnl.xml 

echo create Bin\Tools
md Bin\Tools

echo Build XdkTracker
want\want.exe -buildfile want\XdkTracker.xml 

echo Build XIso
want\want.exe -buildfile want\XIso.xml 

pause