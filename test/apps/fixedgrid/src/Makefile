CFLAGS = -O3 -ftree-vectorize -funroll-loops   -ffast-math -fno-tree-pre -g -lm -m32 -maltivec -DTPC  
SPUFLAGS=  -O3 -fomit-frame-pointer -ftree-vectorize -ffast-math -fno-tree-pre -fvect-cost-model -ftree-vectorizer-verbose=4 -DVECTORIZED 
CC=ppu-gcc
SCC=spu-gcc
LDFLAGS= -lm -lspe2 ../../../libtpc/libtpc_ppe.a


CHEM   = chem
UTIL   = util
CONFIG = config
CDIR = $(SQPROGDIR)

INCLUDES = -I. \
	   -I$(CHEM) \
	   -I$(UTIL) \
	   -I$(CONFIG) \
           -I../../../libtpc/include

SRCS = fixedgrid.c \
       transport.c \
       chemistry.c \
       $(CHEM)/saprc99_Monitor.c \
       $(CHEM)/saprc99_Integrator.c \
       $(CHEM)/saprc99_Rates.c  \
       $(CHEM)/saprc99_LinearAlgebra.c  \
       $(CHEM)/saprc99_JacobianSP.c  \
       $(CHEM)/saprc99_Jacobian.c  \
       $(CHEM)/saprc99_Function.c  \
       $(CHEM)/saprc99_Initialize.c \
       $(UTIL)/fileio.c \
       $(UTIL)/timer.c 


SRCSPU=spe/func.c

OBJS = $(SRCS:%.c=%.o)
OBJSPU = $(SRCSPU:%.c=%.o)
EMBEDBIN = tpc_spe_binary-embed.o


OBJSPU = $(SRCSPU:%.c=%.o)

default: fixedgrid

fixedgrid: $(OBJS) $(EMBEDBIN) 
	$(CC) -m32   -o $@ $+    $(LDFLAGS)  

$(OBJSPU): %.o: %.c
	$(SCC) $(SPUFLAGS)   $(INCLUDES)   $< -c -o $@



$(OBJS):  %.o: %.c
	$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $@


tpc_spe_binary-embed.o: $(SRCSPU) $(OBJSPU)
	$(SCC) -O3 $(OBJSPU) -o tpc_spe_binary -lm -lsimdmath ../../../libtpc/libtpc_spe.a
	ppu-embedspu -m32 tpc_spe_binary tpc_spe_binary tpc_spe_binary-embed.o


clean: 
	rm -f $(PROG) $(OBJS)
	rm -f $(OBJSPU) tpc_spe_binary* fixedgrid



