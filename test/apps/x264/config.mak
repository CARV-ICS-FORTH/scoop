prefix=/usr/local
exec_prefix=${prefix}
bindir=${exec_prefix}/bin
libdir=${exec_prefix}/lib
includedir=${prefix}/include
ARCH=PPC
SYS=LINUX
CC=ppu-gcc
SCC=spu-gcc
#CC=LD_PRELOAD=/spare/libfaketime-0.8/libfaketime.so.1 FAKETIME="2009-11-05"  ppuxlc
#SCC=LD_PRELOAD=/spare/libfaketime-0.8/libfaketime.so.1 FAKETIME="2009-03-01"  spuxlc


##################### SPU ##########################

SPUFLAGS=   -O2 -c -ffast-math  -fmodulo-sched -ftree-vectorize 		\
            -fomit-frame-pointer -ffunction-sections  -fvect-cost-model  	\
	    -I. -I../../libtpc/include 						\
	    -DHAVE_MALLOC_H -DSYS_LINUX -DWORDS_BIGENDIAN 			\
	    -DDEBLOCK  -DENTROPY   -DNDEBUG -DVEC 


##################### PPU ##########################

CFLAGS=   -maltivec  -m32 -c -O4  -mtune=cell -pipe -Wall										\
          -fmodulo-sched  -fvect-cost-model -ftree-vectorize -ffast-math  -fprefetch-loop-arrays -funroll-loops -fomit-frame-pointer 	\
 	  -I.  -I../../libtpc/include													\
          -DHAVE_MALLOC_H -DARCH_PPC -DSYS_LINUX -DWORDS_BIGENDIAN  -DHAVE_PTHREAD							\
	  -DDEBLOCK -DMEMORY_MANAGE -DENTROPY -DFRAME_COPY  -DTPC   -DHPAGE   								\

################## ALTIVEC #######################

ALTIVECFLAGS= -maltivec -mabi=altivec

##################### LD ##########################

LDFLAGS=   -lspe2    -lpthread -lm   ../../libtpc/libtpc_ppe.a   -O3 -m32  -lX11
SLDFLAGS= -O5 -qipa=overlay -qipa=partition=large 


AS=
ASFLAGS=
GTK=no
EXE=
VIS=no
HAVE_GETOPT_LONG=1
DEVNULL=/dev/null
ECHON=echo -n


