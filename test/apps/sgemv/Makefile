
all: sgemv

sgemv: sgemv.c sgemv_func.c sgemv.h
	spu-gcc -I../../libtpc/include -Wall -O3 -fmodulo-sched -funroll-loops -ftree-vectorize  sgemv_func.c ../../libtpc/libtpc_spe.a -o sgemv_func
	ppu-embedspu -m32 tpc_spe_binary sgemv_func sgemv_func-embed.o
	ppu32-gcc  -I../../libtpc/include -Wall -O3 sgemv.c ../../libtpc/libtpc_ppe.a sgemv_func-embed.o -o sgemv -lm -lspe2

clean:
	rm -rf sgemv_func sgemv_func-embed.o sgemv
