                      Delphi Driver Development Kit v0.0.1
                      ====================================
                      by The Hacker Defender Project team


Instructions
------------

To get .sys driver you have to run compile.but at first and then build.bat.
compile.bat runs Delphi part of the work creating .obj. build.bat then converts
this object file to MS supported format of OMF using OMF2D 1.02 written 
by EliCZ and then MS linker link.exe is used to assemble final .sys. 

As you can notice making drivers in Delphi is not supported by default so 
several hacks are made to make it working. One of these hacks is ignoring
some linker errors and so that we receive some other errors and warnigs during 
linking. Final .sys is working well so don't take these warnings seriously 
but of course if you make your own driver there can be lot of other warnings
and errors you should care about.


Versions
--------

0.0.1 - This is very first release of DDDK. It is just proof of concept showing
        the possibility of making working Delphi driver. However, this version 
        comes with working implementation of DbgPrint which can be very useful
        where it comes to the driver coding.



visit our site http://www.hxdef.org (http://hxdef.net.ru, 
http://hxdef.czweb.org, http://rootkit.host.sk)




