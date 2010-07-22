COMMON_OPTS= -g -Wall -O3 -fmodulo-sched -funroll-loops -ftree-vectorize -I../../libtpc/include

all: saxpy

saxpy: saxpy.c saxpy_func.c saxpy.h ../../libtpc/libtpc_spe.a ../../libtpc/libtpc_ppe.a
	spu-gcc ${COMMON_OPTS} saxpy_func.c ../../libtpc/libtpc_spe.a -o saxpy_func
	ppu-embedspu -m32 tpc_spe_binary saxpy_func saxpy_func-embed.o
	ppu32-gcc ${COMMON_OPTS} saxpy.c ../../libtpc/libtpc_ppe.a saxpy_func-embed.o -o saxpy -lm -lspe2

clean:
	rm -rf saxpy_func saxpy_func-embed.o saxpy
