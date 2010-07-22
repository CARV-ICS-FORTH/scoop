
all: conv2d

conv2d: conv2d.c conv2d_func.c conv2d.h conv2d_leaf_gcell_9x9.c
	spu-gcc -I../../libtpc/include/ -Wall -O3 -ftree-vectorize -fmodulo-sched -funroll-loops conv2d_leaf_gcell_9x9.c conv2d_func.c ../../libtpc/libtpc_spe.a -o conv2d_func
	ppu-embedspu -m32 tpc_spe_binary conv2d_func conv2d_func-embed.o
	ppu32-gcc -I../../libtpc/include/ -Wall -O3 -ftree-vectorize  conv2d.c ../../libtpc/libtpc_ppe.a conv2d_func-embed.o -o conv2d -lm -lspe2

clean:
	rm -rf conv2d_func conv2d_func-embed.o conv2d
