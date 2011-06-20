@echo off 
CLS
CLS
echo ***********************************************
echo **                                           **
echo ** Dxbx rebuild patterns process.            **
echo **                                           **
echo ***********************************************


echo rebuild patterns
PatternTrieBuilder.exe ..\..\..\resource\Patterns

pause