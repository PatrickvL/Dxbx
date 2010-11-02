MINGW=S:\ProgRaw\MinGW
GCC=$(MINGW)\bin\gcc
LIB=$(MINGW)\lib
CFLAGS=-O2 -masm=intel -std=c99
LINKFLAGS=-mconsole -nostartfiles -Wl,--strip-all 

load : load.o
        $(GCC) $(LINKFLAGS) -o load load.o $(LIB)\crt1.o -lcrtdll -lntdll 
  
load.o : load.c
        $(GCC) $(CFLAGS) -c load.c
  
