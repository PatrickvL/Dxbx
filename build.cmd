@echo off 
CLS
CLS
echo ***********************************************
echo **                                           **
echo ** DXBX Build process.                       **
echo **                                           **
echo ***********************************************


echo Remove Bin dir for clean build
want\deltree /fBin /s

echo Remove Dcu dir for clean build
want\deltree /fDcu /s

echo Create Bin dir.
md Bin
echo Create Dcu dir.
md Dcu


echo Copy Want\msvcrtd.dll to Bin dir
copy Want\msvcrtd.dll Bin

echo Copy Want\CxbxKrnl.dll to Bin dir
copy Want\CxbxKrnl.dll Bin

echo Build Dxbx
want\want.exe -buildfile want\dxbx.xml 

echo Build DxbxKrnl.dll
want\want.exe -buildfile want\dxbxkrnl.xml  

echo Build DxbxDll.dll
want\want.exe -buildfile want\dxbxDll.xml  

echo create Bin\Tools
md Bin\Tools

echo Build XdkTracker
want\want.exe -buildfile want\XdkTracker.xml 

echo Build XIso
want\want.exe -buildfile want\XIso.xml 

echo Build PatternCodeGenerator
want\want.exe -buildfile want\PatternCodeGenerator.xml 



pause