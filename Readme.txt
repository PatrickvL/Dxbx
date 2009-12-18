Dxbx, a Delphi port of Cxbx.


Introduction
------------
Dxbx (a Delphi port of Cxbx) is an emulator for running Xbox games on Windows 2000 and higher.
Emulating a Xbox is a daunting task, so there's no guarantee that any game will work.
Emulation is done by a High-Level abstraction of the Xbox system.
This means that any game that accesses the hardware directly will not work.
TODO : Start a compatibilty list


Emulation Notes
---------------
For now, if you want to play a game with Dxbx you'll have to use the CxbxKrnl in the options menu.
Copy CxbxKrnl.dll and msvcrtd.dll to your game folder and select a manual folder for the output.
Place the generated default.exe into the game folder (the same location where default.xbe resides).
Then press Start Emulation to start your game.

Note, that Dxbx has not yet implemented the controller code, so apart from starting a game, you won't be able to do much.

Also, when using the CxbxKrnl, compatibility will be the same as Cxbx.
The Dxbx project implements a different function detection method, which has both positive and negative consequences:
The nice thing is, that it will make it easier for us to debug non-working games (so, in theory we have an easier job in supporting new games).
The down side is, that our detection method won't work with so-called link-time optimized games - so we won't support those (yet).



Prerequisites
-------------
Dxbx only runs on 32-bit Windows 2000, XP and Vista. The 64-bit versions of Windows will not work due to lack of support for segmented addressing.

Running under Linux using Wine is not tested, but it might just work. (Please let us know your results!)


System requirements
-------------------
CPU: A 1Ghz Pentium or better should suffice.
Memory: Running with less than 256 MB is not recommended.
Video: TODO
Controller: TODO


Developer Notes
---------------
See Developer.txt


Special thanks
--------------
We would like to send our regards to :

Caustik and the other members of the Cxbx project, for creating the original source code of Cxbx!
Without this code and amazing high tech cxbx application, we would never have started this project.

Robert Yates for his Xbox FLIRT files.

The Jedi project members, for providing us with a wonderfull library of high-quality Delphi code!

N1ghtjar and NexXxus for making lots of xbe dumps for filling the XDKTracker.

Kingis85 for donating a lot a xbox games !!

Contributors
------------
Contributors to the Dxbx project are (in alphabetical order) :

PCucho (from Argentina) - XIso translation
PatrickvL (from Holland) - Coding Style, Relocation, Kernel API's, Function detection
Shadow_tj (from Holland) - Main Cxbx translation
Wayo (from Guatemala) - GetProcAddress trick with CxbxKrnlInit, (Unswizzled) Icon extraction
Zomby (from Canada) - Vista compatibility, lots of fixups