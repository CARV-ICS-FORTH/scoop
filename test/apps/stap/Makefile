CFLAGS = -O3 -ftree-vectorize -funroll-loops   -g  -m32 -maltivec -DTPC -I. -Wall  -ftree-vectorize -fno-tree-pre -fvect-cost-model -I../../libtpc/include  
SPUFLAGS= -O3 -funroll-loops -fomit-frame-pointer -ftree-vectorize -ffast-math -fno-tree-pre \
          -fvect-cost-model -ftree-vectorizer-verbose=0 -fbuiltin  -I../../libtpc/include -I../ -I.

LDFLAGS= ../../libtpc/libtpc_ppe.a -lspe2 -lpthread -lm
SLDFLAGS= -lm

CC=ppu-gcc
SCC=spu-gcc

SRCSPU= spe/func.c   

SRCS = stap_bench_trans.c

HEADERS = common.h 




OBJSPU = $(SRCSPU:%.c=%.o)
OBJS = $(SRCS:%.c=%.o)


default: stap

$(OBJSPU): %.o: %.c
	$(SCC) $(SPUFLAGS) $< -c -o $@


$(OBJS):  %.o: %.c
	$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $@


###############



stap: $(OBJS) $(EMBEDBIN) tpc_spe_binary-embed.o  $(HEADERS)
	$(CC) -m32   -o $@ $+    $(LDFLAGS)  


tpc_spe_binary-embed.o:  $(OBJSPU) $(HEADERS)
	$(SCC) $(OBJSPU) -o tpc_spe_binary   $(SLDFLAGS)   ../../libtpc/libtpc_spe.a
	ppu-embedspu -m32 tpc_spe_binary tpc_spe_binary tpc_spe_binary-embed.o



clean: 
	rm -f $(PROG) $(OBJS)
	rm -f $(OBJSPU) tpc_spe_binary* stap

run:
	./stap


