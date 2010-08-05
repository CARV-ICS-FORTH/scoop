
all: FFT

FFT: fft.c fft_func.c fft_spe.c fft.h ../../../libtpc/libtpc_spe.a ../../../libtpc/libtpc_ppe.a
	spu-gcc -I../../../libtpc/include -Wall -g -O3 -fmodulo-sched -funroll-loops -ftree-vectorize fft_spe.c fft_func.c ../../../libtpc/libtpc_spe.a -o fft_func
	ppu-embedspu -m32 tpc_spe_binary fft_func fft_func-embed.o
	ppu32-gcc  -I../../../libtpc/include -g -Wall -O3 -fmodulo-sched -funroll-loops -ftree-vectorize fft.c transpose.c ../../../libtpc/libtpc_ppe.a fft_func-embed.o -o FFT -lm -lspe2

clean:
	rm -rf fft_func fft_func-embed.o FFT
