
all: LU

LU: lu.c lu_func.c lu.h
	spu-gcc -I../../libtpc/include -Wall -O3 -fmodulo-sched -ftree-vectorize  -funroll-loops lu_func.c ../../libtpc/libtpc_spe.a -o lu_func
	ppu-embedspu -m32 tpc_spe_binary lu_func lu_func-embed.o
	ppu32-gcc  -I../../libtpc/include -Wall -ftree-vectorize -O3 lu.c ../../libtpc/libtpc_ppe.a lu_func-embed.o -o LU -lm -lspe2

clean:
	rm -rf lu_func lu_func-embed.o LU
