Readme file for x264c
---------------------

Michail Alvanos - malvanos <at> gmail.com 
http://sites.google.com/site/malvanos/


| 0. Intro
+----------

x264c is Cell implementation of x264 open source video encoder (http://www.videolan.org/developers/x264.html) based in 20080920-2245 snapshot.



| 1. Compilation
+-----------------

Type make to compile the x264c. You must have installed ppu-gcc 4.1 or 4.3 and spu-gcc 4.1 (some compile errors with 4.3). 
Compilation flags included from the file config.mak, including definitions:


ppu-gcc preprocessor options:

HAVE_MALLOC_H                   -+   
ARCH_PPC                         |
SYS_LINUX                        +---> enabled always 
WORDS_BIGENDIAN                  |
TPC                              |
HAVE_PTHREAD			-+			

DEBLOCK                         # Offloading of deblocking filter
MEMORY_MANAGE                   # Memory recycler
ENTROPY                         # Entropy encoder (CAVLC)
FRAME_COPY                      # Offload memory copy of raw frames
HPAGE   	                # Large pages
LARGE_PAGES_PATH                # File path for large pages (default is /huge). Refer to linux manual for the usage of large pages.



spu-gcc preprocessor options:

HAVE_MALLOC_H                   -+   
SYS_LINUX                        +---> enabled always 
WORDS_BIGENDIAN                 -+

DEBLOCK                         # Offloading of deblocking filter
ENTROPY                         # Entropy encoder (CAVLC)
DNDEBUG                         # Disable runtime checks in code
DVEC                            # Vectorized kernels




Finally you must tune the runtime (edit libtpc/include/*h files):
 
MAX_ARGS                must be at least 16
MAX_QUEUE_ENTRIES       must be 1 for max performance  (can run with higher number but performance decreases)
MAX_DMA_LISTSZ          must be (MAX_ARGS+640U) 
MAX_FETCHING 1U         must be 1 (unstable with more)





| 2. Encoding videos
+--------------------

Repository version includes an example script for running video encoder. 
Parameters explanation (example from first entry of bench3.sh):

--threads 1                     # Threads should be always 1
--progress                      # Optional: prints the progress in screen. You should disable it before running benchmarks 
--spes 6                        # Number of SPEs: 1-6 ( up to 8 in blade - you need to need to disable remote vector stores in runtime for more than 8 SPEs)
-A none                         # Only 16x16 partitions are supported: it should be always none
--direct none                   # No direct support
--bframes 4                     # Number of b-frames: varies from 0 up to 16, a good value is 4
-m 5                            # Analysis mode: almost everything is disabled so you will not see difference, 5 is a good value
--no-cabac                      # CAVLC only support: you must disable CABAC
--b-adapt 0                     # Adaptive number of b-frames becomes bottleneck for 1 or 2 values: just leave it 0 (static number of b-frames between p-frames)
--ref 1                         # Number of ref frames: we support only 1 (actually 2 in B-frames - past and future pic - and 1 in P-frames), you should leave it 1
--frames 100                    # Optional: number of frames to encode, if disabled encoder will stop at the end of file
--qp=26                         # Quatization value: from 0 (loseless) to 56. 26 is the default value
--me umh                        # Motion estimation algorithm: umh (best results more computing), hex only, dia (worst results less computing)
--merange 48                    # Range for motion estimation: from 0 (worse quality) up to 48 (best quality). Larger region is available using only p-frames
--no-psnr                       # No psnr calculation  (disabled in code)
--no-ssim                       # No ssim calculation  (disabled in code)
-o ~/tmp/out.h264               # Output file can be .h264 or .mkv, i suggest .h264
/spare/... /..._blue_sky.yuv    # Input file must be uncompressed YUV 4:2:0
720x576                         # Resolution of input video









