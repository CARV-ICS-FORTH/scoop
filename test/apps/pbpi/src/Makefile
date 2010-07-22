VERSION=1.0.0.5 Production Release
BUILD=Midnight Horror

# Common options
#C_OPTS = -O3 -g -funroll-loops -fmodulo-sched -ftree-vectorize -ffast-math #-DNO_STATISTICS
C_OPTS = -O3 -ftree-vectorize -funroll-loops -fmodulo-sched -ffast-math  -fno-tree-pre -fomit-frame-pointer -fvect-cost-model 

S_OPTS = -DSPU_ONLY -fno-tree-pre -fomit-frame-pointer -fvect-cost-model  -ftree-vectorize 

# PPU options
P_OPTS = -maltivec

#INCLUDES = /opt/ibm/cell-sdk/prototype/sysroot/usr/include
#LIBS = /opt/ibm/cell-sdk/prototype/sysroot/usr/lib
#CELL_LIB = /opt/ibm/cell-sdk/prototype/src/lib/misc/
RPC_SPE = ../../../libtpc/
RPC_PPE = ../../../libtpc/
RPC_HEADERS = ../../../libtpc/include/

TPC_SRC = likelihood.c
SRC = tpc.app.c
TPC_BIN = likelihood.o

# compiler and comiple option
#CC=gcc
#System X
MPICC =mpicc 
CC=ppu-gcc -m32
CFLAGS= -O3 -ftree-vectorize -funroll-loops -fmodulo-sched   -fno-tree-pre -fomit-frame-pointer -fvect-cost-model 

# MPI library locatioon
MPI_INCLUDE=/usr/local/share/mpich-1.2.7p1/32_noext/include/
#MPI_LIB=/usr/local/share/mpich-1.2.7p1/32_noext/lib/
IBM_INCLUDE=/opt/cell/sdk/usr/include
GCC_INCLUDE=/usr/lib/gcc/ppu/4.1.1/include/
#System X
#MPI_PATH=/nfs/compilers/mpich-1.2.5
#MPI_INCLUDE=$(MPI_PATH)/include
#MPI_LIB=-L$(MPI_PATH)/lib


SRCS=align_alloc.c main.c mcmc.c dataset.c token.c tree.c rand.c error.c assert_aligned.c ppu.app.c\
        model.c util.c xmltoken.c proposal.c runsetting.c parallel.c sumt.c rand2.c timing.c stlpc.c

HEADERS=align_alloc.h pbpi.h mcmc.h dataset.h likelihood.h token.h tree.h rand.h error.h assert_aligned.h ppu.app.h\
	log/log2_v.h log/log_v.h log/log.h model.h util.h xmltoken.h proposal.h runsetting.h \
	parallel.h sumt.h rand2.h timing.h stlpc.h

OBJS=align_alloc.o main.o mcmc.o dataset.o token.o tree.o rand.o error.o spu_binary_embed.o assert_aligned.o ppu.app.o\
        model.o util.o xmltoken.o proposal.o runsetting.o parallel.o sumt.o rand2.o timing.o stlpc.o

EXES=pbpi

default: pbpi

pbpi:	$(TPC_BIN) $(OBJS)
	$(MPICC) $(CFLAGS) -o pbpi $(TPC_BIN) $(OBJS) -I$(MPI_INCLUDE) -I$(RPC_HEADERS) $(MPI_LIB) $(RPC_PPE)libtpc_ppe.a -lm -lspe2

likelihood.o: ppu.app.h likelihood.h ppu.app.c likelihood.c
	spu-gcc ${C_OPTS} ${S_OPTS} -I$(RPC_HEADERS) -I$(MPI_INCLUDE) \
		-I$(IBM_INCLUDE) ${SRC} ${RPC_SPE}libtpc_spe.a -o tpc_spe_binary
	ppu-embedspu -m32 tpc_spe_binary tpc_spe_binary spu_binary_embed.o
	ppu32-gcc ${CFLAGS} ${P_OPTS} -I$(MPI_INCLUDE) -I$(RPC_HEADERS) -c -o $(TPC_BIN) -lspe2 -lm ${TPC_SRC} \
		${RPC_PPE}libtpc_ppe.a spu_binary_embed.o



.c.o:
	$(CC) $(CFLAGS) -maltivec -I$(RPC_HEADERS) -I$(GCC_INCLUDE) -I$(MPI_INCLUDE) -c $<

install:
	cp pbpi /home/fengx/bin

package: $(SRCS) $(HEADERS) Makefile
	tar cvfz pbpi_src.$(VERSION)$(BUILD).tar.gz $(SRCS) $(HEADERS) Makefile

clean:
	rm -rf *.o pbpi tpc_spe_binary

cls:
	rm *.o *~ *.log

rmperfres :
	rm *.pres *.jgr *.ps
	rm ../datasets/*.p ../datasets/*.t
