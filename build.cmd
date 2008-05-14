@echo off 
CLS
CLS
echo ***********************************************
echo **                                           **
echo ** DXBX Build process.                       **
echo **                                           **
echo ***********************************************

echo Build Dxbx
want\want.exe -buildfile want\dxbx.xml 

echo Build CxbxKrnl.dll
want\want.exe -buildfile want\Cxbxkrnl.xml 

echo Build XdkTracker
want\want.exe -buildfile want\XdkTracker.xml 

echo Build XIso
want\want.exe -buildfile want\XIso.xml 

pause