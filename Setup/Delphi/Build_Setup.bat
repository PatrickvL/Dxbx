echo remove old Setup
del *.exe

call Build_Dxbx_All_D14_OldStyle
call Tools\InnoSetup\compil32.exe /cc setup\setup.iss
rename setup.exe setup_oldstyle.exe

call Build_Dxbx_All_D14 
call Tools\InnoSetup\compil32.exe /cc setup\setup.iss
rename setup.exe setup_newstyle_pathching.exe

call Build_Dxbx_All_D14 PUSHBUFFER_ONLY
call Tools\InnoSetup\compil32.exe /cc setup\setup.iss
rename setup.exe setup_newstyle_pushbuffer.exe