
all: FFTsp

FFTsp: fftsp.c fftsp_func.c fftsp.h ../../libtpc/libtpc_spe.a ../../libtpc/libtpc_ppe.a
	spu-gcc -I../../libtpc/include -Wall -g -O3 -fmodulo-sched -funroll-loops -ftree-vectorize fftsp_func.c ../../libtpc/libtpc_spe.a -o fftsp_func
	ppu-embedspu -m32 tpc_spe_binary fftsp_func fftsp_func-embed.o
	ppu32-gcc  -I../../libtpc/include -g -Wall -O3 -fmodulo-sched -funroll-loops -ftree-vectorize fftsp.c transposesp.c ../../libtpc/libtpc_ppe.a fftsp_func-embed.o -o FFTsp -lm -lspe2

clean:
	rm -rf fftsp_func fftsp_func-embed.o FFTsp
