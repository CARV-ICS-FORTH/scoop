make -j2

#-finline-functions, -funswitch-loops and           -fgcse-after-reload
spu-gcc  -O2 -DVEC -DDEBLOCK   -DENTROPY -ffast-math  -fomit-frame-pointer -ffunction-sections  -fmodulo-sched -ftree-vectorize  -fno-tree-pre -fvect-cost-model  -I.   -DNDEBUG  -fvect-cost-model -ftree-vectorizer-verbose=1   -ffast-math  -fomit-frame-pointer  -fmodulo-sched -ftree-vectorize -I. -DVEC  -DHAVE_MALLOC_H -DSYS_LINUX -DWORDS_BIGENDIAN   tpc/spe/main.c common/spu/pixel_tpc.c common/spu/mc_tpc.c common/spu/macroblock_tpc.c encoder/spu/me_tpc.c encoder/spu/analyse_tpc.c common/spu/predict_tpc.c encoder/spu/macroblock_tpc.c common/spu/dct_tpc.c common/spu/quant_tpc.c common/spu/frame_tpc.c encoder/spu/rdo_tpc.o common/spu/vlc_tpc.o -o tpc_spe_binary   ../libtpc/libtpc_spe.a
#spu-gcc  -Os -DVEC -DDEBLOCK  -ffast-math  -fomit-frame-pointer -ffunction-sections  -fmodulo-sched -ftree-vectorize  -fno-tree-pre -fvect-cost-model  -I.   -DNDEBUG  -fvect-cost-model -ftree-vectorizer-verbose=1   -ffast-math  -fomit-frame-pointer  -fmodulo-sched -ftree-vectorize -I. -DVEC  -DHAVE_MALLOC_H -DSYS_LINUX -DWORDS_BIGENDIAN   tpc/spe/main.c common/spu/pixel_tpc.c common/spu/mc_tpc.c common/spu/macroblock_tpc.c encoder/spu/me_tpc.c encoder/spu/analyse_tpc.c common/spu/predict_tpc.c encoder/spu/macroblock_tpc.c common/spu/dct_tpc.c common/spu/quant_tpc.c common/spu/frame_tpc.c encoder/spu/rdo_tpc.o common/spu/vlc_tpc.o -o tpc_spe_binary   ../libtpc/libtpc_spe.a



ppu-embedspu -m32 tpc_spe_binary tpc_spe_binary tpc_spe_binary-embed.o
ar rc libx264.a common/mc.o common/predict.o common/pixel.o common/macroblock.o common/frame.o common/dct.o common/cpu.o common/cabac.o common/common.o common/mdate.o common/set.o common/quant.o common/vlc.o common/macroblock_tpc.o common/cp_hugemem.o encoder/analyse.o encoder/me.o encoder/ratecontrol.o encoder/set.o encoder/macroblock.o encoder/cabac.o encoder/cavlc.o encoder/encoder.o encoder/me_tpc.o tpc/call_tpc.o tpc/stats.o common/ppc/mc.o common/ppc/pixel.o common/ppc/dct.o common/ppc/quant.o common/ppc/deblock.o common/ppc/predict.o tpc_spe_binary-embed.o 
ranlib libx264.a
make



