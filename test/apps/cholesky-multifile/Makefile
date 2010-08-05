# Makefile

# available LIBTPC options:
#   libtpc
#   libtpc_opt
#   libtpc_old
LIBTPC=libtpc


CFLAGS = -O3 -ftree-vectorize -funroll-loops   -ffast-math -fno-tree-pre -g  -m32 -maltivec  -I../../../$(LIBTPC)/include/
SPUFLAGS=  -O3 -funroll-loops -DVEC  -ffast-math  -fomit-frame-pointer   -fmodulo-sched -ftree-vectorize -I. -I./spe -I../../../$(LIBTPC)/include/

CC=ppu-gcc
SCC=spu-gcc

SRCSPU= cholesky_func.c   \
        spe/spu_sgemm_tile.c \
        spe/spu_spotrf_tile.c \
        spe/spu_ssyrk_tile.c \
        spe/spu_strsm_tile.c 

SRCS = cholesky.c

HEADERS =   spe/spu_sgemm_tile.h \
        spe/spu_spotrf_tile.h \
        spe/spu_ssyrk_tile.h \
        spe/spu_strsm_tile.h 




EMBEDBIN=tpc_spe_binary-embed.o
LDFLAGS= ../../../$(LIBTPC)/libtpc_ppe.a -lspe2 -lpthread 


###############

OBJSPU = $(SRCSPU:%.c=%.o)
OBJS = $(SRCS:%.c=%.o)


default: cholesky 

$(OBJSPU): %.o: %.c
	$(SCC) $(SPUFLAGS) $< -c -o $@


$(OBJS):  %.o: %.c
	$(CC) -DTPC $(CFLAGS) $(INCLUDES) -c $< -o $@





###############

default: cholesky #cholesky-serial 


cholesky: $(OBJS) $(EMBEDBIN) 
	$(CC) -m32 -DTPC  -o $@ $+    $(LDFLAGS)  


tpc_spe_binary-embed.o:  $(OBJSPU) $(HEADERS)
	$(SCC)  $(SLDFLAGS)  $(OBJSPU) -o tpc_spe_binary  ../../../$(LIBTPC)/libtpc_spe.a
	ppu-embedspu -m32 tpc_spe_binary tpc_spe_binary tpc_spe_binary-embed.o




clean: 
	rm -f $(PROG) $(OBJS)
	rm -f $(OBJSPU) tpc_spe_binary* cholesky

