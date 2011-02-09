echo remove old Setup
del setup.exe

// Old setup 
call Build_Dxbx_All_D14_OldStyle
call Tools\InnoSetup\compil32.exe /cc setup\setup.iss

// New Setup with patching
call Build_Dxbx_All_D14 
call Tools\InnoSetup\compil32.exe /cc setup\setup.iss

// New Setup with PushBuffer Rendering
call Build_Dxbx_All_D14 
call Tools\InnoSetup\compil32.exe /cc setup\setup.iss