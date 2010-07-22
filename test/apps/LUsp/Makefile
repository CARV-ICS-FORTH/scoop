
all: LU

LU: lusp.c lusp_func.c lusp.h
	spu-gcc -I../../libtpc/include -Wall -O3 -fmodulo-sched -funroll-loops lusp_func.c ../../libtpc/libtpc_spe.a -o lusp_func
	ppu-embedspu -m32 tpc_spe_binary lusp_func lusp_func-embed.o
	ppu32-gcc  -I../../libtpc/include -Wall -O3 lusp.c ../../libtpc/libtpc_ppe.a lusp_func-embed.o -o LUsp -lm -lspe2

clean:
	rm -rf lusp_func lusp_func-embed.o LUsp
