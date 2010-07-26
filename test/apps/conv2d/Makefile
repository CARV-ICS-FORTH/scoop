MAX_QUEUE_ENTRIES=16U
COMMON_OPS=-I../../include -DSTATISTICS -DMAX_QUEUE_ENTRIES=${MAX_QUEUE_ENTRIES} -Wall -O3 -g -ftree-vectorize

all: conv2d

conv2d: conv2d.c conv2d_func.c conv2d_spe.c conv2d.h conv2d_leaf_gcell_9x9.c
	spu-gcc ${COMMON_OPS} -fmodulo-sched -funroll-loops conv2d_leaf_gcell_9x9.c conv2d_func.c conv2d_spe.c ../../libtpc_spe.a -o conv2d_func
	ppu-embedspu -m32 tpc_spe_binary conv2d_func conv2d_func-embed.o
	ppu32-gcc ${COMMON_OPS} conv2d.c ../../libtpc_ppe.a conv2d_func-embed.o -o conv2d -lm -lspe2

clean:
	rm -rf conv2d_func conv2d_func-embed.o conv2d
