/*****************************************************************************
 * common.h: h264 encoder
 *****************************************************************************
 * Copyright (C) 2003-2008 x264 project
 *
 * Authors: Laurent Aimar <fenrir@via.ecp.fr>
 *          Loren Merritt <lorenm@u.washington.edu>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02111, USA.
 *****************************************************************************/

#ifndef X264_COMMON_H
#define X264_COMMON_H

/****************************************************************************
 * Macros
 ****************************************************************************/
#define X264_MIN(a,b) ( (a)<(b) ? (a) : (b) )
#define X264_MAX(a,b) ( (a)>(b) ? (a) : (b) )
#define X264_MIN3(a,b,c) X264_MIN((a),X264_MIN((b),(c)))
#define X264_MAX3(a,b,c) X264_MAX((a),X264_MAX((b),(c)))
#define X264_MIN4(a,b,c,d) X264_MIN((a),X264_MIN3((b),(c),(d)))
#define X264_MAX4(a,b,c,d) X264_MAX((a),X264_MAX3((b),(c),(d)))
#define XCHG(type,a,b) do{ type t = a; a = b; b = t; } while(0)
#define FIX8(f) ((int)(f*(1<<8)+.5))

#define CHECKED_MALLOC( var, size )\
{\
    var = x264_malloc( size );\
    if( !var )\
    {\
        x264_log( h, X264_LOG_ERROR, "malloc failed\n" );\
        goto fail;\
    }\
}

#define X264_BFRAME_MAX 64
#define X264_THREAD_MAX 128
#define X264_SLICE_MAX 4
#define X264_NAL_MAX (4 + X264_SLICE_MAX)
#define X264_PCM_COST (386*8)

// number of pixels (per thread) in progress at any given time.
// 16 for the macroblock in progress + 3 for deblocking + 3 for motion compensation filter + 2 for extra safety
#define X264_THREAD_HEIGHT 24


/// Number of macroblocks 
/// Limits the resolution of video
/// to 1920x1088 !
#if 0
#define X264_MAX_MBS 34560
#define X264_MAX_ALLOC 34560 
#else
#define X264_MAX_MBS 8160
#define X264_MAX_ALLOC 8160

#endif


#define X264_START_MBS_THR (0.35)
#define ALIGN_SZ 128


#if 1


#if 0

#define LOCK_VAR pthread_mutex_t

#define UNLOCK(x) {             \
pthread_mutex_unlock(x);        \
}
#define LOCK(x) {               \
pthread_mutex_lock(x);          \
}

#define LOCK_INIT(x,y){                         \
        pthread_mutex_init( (x) , (y) );        \
}

#else

#define LOCK_VAR pthread_spinlock_t

#define UNLOCK(x) {             \
pthread_spin_unlock(x);         \
}

#define LOCK(x) {               \
pthread_spin_lock(x);           \
}

#define LOCK_INIT(x,y){                         \
        pthread_spin_init( (x) , PTHREAD_PROCESS_PRIVATE );     \
}

#endif



#else


#define LOCK_VAR  uint32_t
#define UNLOCK(x) {}
#define LOCK(x)  {}
#define LOCK_INIT(x,y) {}

#endif





/****************************************************************************
 * Includes
 ****************************************************************************/
#include "osdep.h"
#include <ppu_intrinsics.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <aio.h>
#include "x264.h"
#include "bs.h"
#include "set.h"
#include "predict.h"
#include "pixel.h"
#include "mc.h"
#include "frame.h"
#include "dct.h"
#include "cabac.h"
#include "quant.h"
#include "x264_common_tpc.h"
#include "cp_hugemem.h"

/****************************************************************************
 * Generals functions
 ****************************************************************************/
/* x264_malloc : will do or emulate a memalign
 * you have to use x264_free for buffers allocated with x264_malloc */
void *x264_malloc( int );
void *x264_realloc( void *p, int i_size );
void  x264_free( void * );

/* x264_slurp_file: malloc space for the whole file and read it */
char *x264_slurp_file( const char *filename );

/* mdate: return the current date in microsecond */
int64_t x264_mdate( void );

/* x264_param2string: return a (malloced) string containing most of
 * the encoding options */
char *x264_param2string( x264_param_t *p, int b_res );

/* log */
void x264_log( x264_t *h, int i_level, const char *psz_fmt, ... );

void x264_reduce_fraction( int *n, int *d );

static inline uint8_t x264_clip_uint8( int x )
{
    return x&(~255) ? (-x)>>31 : x;
}

static inline int x264_clip3( int v, int i_min, int i_max )
{
    return ( (v < i_min) ? i_min : (v > i_max) ? i_max : v );
}

static inline double x264_clip3f( double v, double f_min, double f_max )
{
    return ( (v < f_min) ? f_min : (v > f_max) ? f_max : v );
}

static inline int x264_median( int a, int b, int c )
{
    int t = (a-b)&((a-b)>>31);
    a -= t;
    b += t;
    b -= (b-c)&((b-c)>>31);
    b += (a-b)&((a-b)>>31);
    return b;
}

static inline void x264_median_mv( int16_t *dst, int16_t *a, int16_t *b, int16_t *c )
{
    dst[0] = x264_median( a[0], b[0], c[0] );
    dst[1] = x264_median( a[1], b[1], c[1] );
}

static inline int x264_predictor_difference( int16_t (*mvc)[2], intptr_t i_mvc )
{
    int sum = 0, i;
    for( i = 0; i < i_mvc-1; i++ )
    {
        sum += abs( mvc[i][0] - mvc[i+1][0] )
             + abs( mvc[i][1] - mvc[i+1][1] );
    }
    return sum;
}

/****************************************************************************
 *
 ****************************************************************************/
enum slice_type_e
{
    SLICE_TYPE_P  = 0,
    SLICE_TYPE_B  = 1,
    SLICE_TYPE_I  = 2,
    SLICE_TYPE_SP = 3,
    SLICE_TYPE_SI = 4
};



enum mb_state_e {
        UNFETCHED_STATE=0,
        ISSUED_STATE=1,
        DONE_STATE=2
};



static const char slice_type_to_char[] = { 'P', 'B', 'I', 'S', 'S' };

typedef struct
{
    x264_sps_t *sps;
    x264_pps_t *pps;

    int i_type;
    int i_first_mb;
    int i_last_mb;

    int i_pps_id;

    int i_frame_num;

    int b_mbaff;
    int b_field_pic;
    int b_bottom_field;

    int i_idr_pic_id;   /* -1 if nal_type != 5 */

    int i_poc_lsb;
    int i_delta_poc_bottom;

    int i_delta_poc[2];
    int i_redundant_pic_cnt;

    int b_direct_spatial_mv_pred;

    int b_num_ref_idx_override;
    int i_num_ref_idx_l0_active;
    int i_num_ref_idx_l1_active;

    int b_ref_pic_list_reordering_l0;
    int b_ref_pic_list_reordering_l1;
    struct {
        int idc;
        int arg;
    } ref_pic_list_order[2][16];

    int i_cabac_init_idc;

    int i_qp;
    int i_qp_delta;
    int b_sp_for_swidth;
    int i_qs_delta;

    /* deblocking filter */
    int i_disable_deblocking_filter_idc;
    int i_alpha_c0_offset;
    int i_beta_offset;

} x264_slice_header_t;

/* From ffmpeg
 */
#define X264_SCAN8_SIZE (6*8)
#define X264_SCAN8_0 (4+1*8)

static const int x264_scan8[16+2*4] =
{
    /* Luma */
    4+1*8, 5+1*8, 4+2*8, 5+2*8,
    6+1*8, 7+1*8, 6+2*8, 7+2*8,
    4+3*8, 5+3*8, 4+4*8, 5+4*8,
    6+3*8, 7+3*8, 6+4*8, 7+4*8,

    /* Cb */
    1+1*8, 2+1*8,
    1+2*8, 2+2*8,

    /* Cr */
    1+4*8, 2+4*8,
    1+5*8, 2+5*8,
};
/*
   0 1 2 3 4 5 6 7
 0
 1   B B   L L L L
 2   B B   L L L L
 3         L L L L
 4   R R   L L L L
 5   R R
*/

typedef struct x264_ratecontrol_t   x264_ratecontrol_t;

/* MB DCT coeffs */
struct dct {
        DECLARE_ALIGNED_16( int16_t luma16x16_dc[16] );
        DECLARE_ALIGNED_16( int16_t chroma_dc[2][4] );
        // FIXME share memory?
        DECLARE_ALIGNED_16( int16_t luma8x8[4][64] );
        DECLARE_ALIGNED_16( int16_t luma4x4[16+8][16] );
} __attribute__ ((aligned (128))); 

typedef struct dct x264_dct_t;



/* cache */
 struct x264_mb_cache {


            /* real intra4x4_pred_mode if I_4X4 or I_8X8, I_PRED_4x4_DC if mb available, -1 if not */
            int8_t  intra4x4_pred_mode[X264_SCAN8_SIZE];

            /* i_non_zero_count if available else 0x80 */
            DECLARE_ALIGNED_16(uint8_t non_zero_count[X264_SCAN8_SIZE]  );

            /* -1 if unused, -2 if unavailable */
            DECLARE_ALIGNED_4( int8_t ref[2][X264_SCAN8_SIZE] );

            /* 0 if not available */
            DECLARE_ALIGNED_16( int16_t mv[2][X264_SCAN8_SIZE][2] );
            DECLARE_ALIGNED_8( int16_t mvd[2][X264_SCAN8_SIZE][2] );

            /* 1 if SKIP or DIRECT. set only for B-frames + CABAC */
            DECLARE_ALIGNED_4( int8_t skip[X264_SCAN8_SIZE] );

            DECLARE_ALIGNED_16( int16_t direct_mv[2][X264_SCAN8_SIZE][2] );
            DECLARE_ALIGNED_4( int8_t  direct_ref[2][X264_SCAN8_SIZE] );
            DECLARE_ALIGNED_4( int16_t pskip_mv[2] );

            /* number of neighbors (top and left) that used 8x8 dct */
            int     i_neighbour_transform_size;
            int     i_neighbour_interlaced;
  } ;
typedef struct x264_mb_cache x264_mb_cache_t ;


/* MB table and cache for current frame/mb */
struct mb
{
    int     i_mb_count;                 /* number of mbs in a frame */

    /* Strides */
    int     i_mb_stride;
    int     i_b8_stride;
    int     i_b4_stride;

    /* Current index */
    int     i_mb_x;
    int     i_mb_y;
    int     i_mb_xy;
    int     i_b8_xy;
    int     i_b4_xy;

    /* Search parameters */
    int     i_me_method;
    int     i_subpel_refine;
    int     b_chroma_me;
    int     b_trellis;
    int     b_noise_reduction;
    int     i_psy_rd; /* Psy RD strength--fixed point value*/
    int     i_psy_trellis; /* Psy trellis strength--fixed point value*/

    int     b_interlaced;

    /* Allowed qpel MV range to stay within the picture + emulated edge pixels */
    int     mv_min[2];
    int     mv_max[2];
    /* Subpel MV range for motion search.
     * same mv_min/max but includes levels' i_mv_range. */
    int     mv_min_spel[2];
    int     mv_max_spel[2];
    /* Fullpel MV range for motion search */
    int     mv_min_fpel[2];
    int     mv_max_fpel[2];

    /* neighboring MBs */
    unsigned int i_neighbour;
    unsigned int i_neighbour8[4];       /* neighbours of each 8x8 or 4x4 block that are available */
    unsigned int i_neighbour4[16];      /* at the time the block is coded */
    int     i_mb_type_top;
    int     i_mb_type_left;
    int     i_mb_type_topleft;
    int     i_mb_type_topright;
    int     i_mb_prev_xy;
    int     i_mb_top_xy;

    /**** thread synchronization ends here ****/
    /* subsequent variables are either thread-local or constant,
     * and won't be copied from one thread to another */

    /* mb table */
    int8_t  *type;                      /* mb type */
    int8_t  *qp;                        /* mb qp */
    int16_t *cbp;                       /* mb cbp: 0x0?: luma, 0x?0: chroma, 0x100: luma dc, 0x0200 and 0x0400: chroma dc  (all set for PCM)*/
    int8_t  (*intra4x4_pred_mode)[8];   /* intra4x4 pred mode. for non I4x4 set to I_PRED_4x4_DC(2) */
                                        /* actually has only 7 entries; set to 8 for write-combining optimizations */
  //  uint8_t (*non_zero_count)[16+4+4+8];  /* nzc. for I_PCM set to 16 */
    uint8_t (*non_zero_count)[32];  /* nzc. for I_PCM set to 16 */


    int8_t  *chroma_pred_mode;          /* chroma_pred_mode. cabac only. for non intra I_PRED_CHROMA_DC(0) */

    int16_t (*mv[2])[2];                /* mb mv. set to 0 for intra mb */
    int16_t (*mvd[2])[2];               /* mb mv difference with predict. set to 0 if intra. cabac only */

    int8_t   *ref[2];                   /* mb ref. set to -1 if non used (intra or Lx only) */
    int16_t (*mvr[2][32])[2];           /* 16x16 mv for each possible ref */
    int8_t  *skipbp;                    /* block pattern for SKIP or DIRECT (sub)mbs. B-frames + cabac only */
    int8_t  *mb_transform_size;         /* transform_size_8x8_flag of each mb */
    uint8_t *intra_border_backup[2][3]; /* bottom pixels of the previous mb row, used for intra prediction after the framebuffer has been deblocked */
    uint8_t (*nnz_backup)[16];          /* when using cavlc + 8x8dct, the deblocker uses a modified nnz */

    /* current value */
    int     i_type;
    int     i_partition;
    int     i_sub_partition[4];
    int     b_transform_8x8;

    int     i_cbp_luma;
    int     i_cbp_chroma;

    int     i_intra16x16_pred_mode;
    int     i_chroma_pred_mode;

    /* skip flags for i4x4 and i8x8
     * 0 = encode as normal.
     * 1 (non-RD only) = the DCT is still in h->dct, restore fdec and skip reconstruction.
     * 2 (RD only) = the DCT has since been overwritten by RD; restore that too. */
    int i_skip_intra;
    /* skip flag for motion compensation */
    /* if we've already done MC, we don't need to do it again */
    int b_skip_mc;

    struct
    {
       /* space for p_fenc and p_fdec */
#define FENC_STRIDE 16
#define FDEC_STRIDE 32
       DECLARE_ALIGNED_16( uint8_t fenc_buf[24*FENC_STRIDE] );
       DECLARE_ALIGNED_16( uint8_t fdec_buf[27*FDEC_STRIDE] );

       /* i4x4 and i8x8 backup data, for skipping the encode stage when possible */
       DECLARE_ALIGNED_16( uint8_t i4x4_fdec_buf[16*16] );
            DECLARE_ALIGNED_16( uint8_t i8x8_fdec_buf[16*16] );
            DECLARE_ALIGNED_16( int16_t i8x8_dct_buf[3][64] );
            DECLARE_ALIGNED_16( int16_t i4x4_dct_buf[15][16] );

            /* Psy trellis DCT data */
            DECLARE_ALIGNED_16( int16_t fenc_dct8[4][64] );
            DECLARE_ALIGNED_16( int16_t fenc_dct4[16][16] );

            /* Psy RD SATD scores */
            int fenc_satd[4][4];
            int fenc_satd_sum;
            int fenc_sa8d[2][2];
            int fenc_sa8d_sum;

            /* pointer over mb of the frame to be compressed */
            uint8_t *p_fenc[3];

            /* pointer over mb of the frame to be reconstructed  */
            uint8_t *p_fdec[3];

            /* pointer over mb of the references */
            int i_fref[2];
            uint8_t *p_fref[2][32][4+2]; /* last: lN, lH, lV, lHV, cU, cV */
            uint16_t *p_integral[2][16];

            /* fref stride */
            DECLARE_ALIGNED_16(   int     i_stride[3] );
        } pic;

	struct {


            /* real intra4x4_pred_mode if I_4X4 or I_8X8, I_PRED_4x4_DC if mb available, -1 if not */
            int8_t  intra4x4_pred_mode[X264_SCAN8_SIZE];

            /* i_non_zero_count if available else 0x80 */
            DECLARE_ALIGNED_16(uint8_t non_zero_count[X264_SCAN8_SIZE]  );

            /* -1 if unused, -2 if unavailable */
            DECLARE_ALIGNED_4( int8_t ref[2][X264_SCAN8_SIZE] );

            /* 0 if not available */
            DECLARE_ALIGNED_16( int16_t mv[2][X264_SCAN8_SIZE][2] );
            DECLARE_ALIGNED_8( int16_t mvd[2][X264_SCAN8_SIZE][2] );

            /* 1 if SKIP or DIRECT. set only for B-frames + CABAC */
            DECLARE_ALIGNED_4( int8_t skip[X264_SCAN8_SIZE] );

            DECLARE_ALIGNED_16( int16_t direct_mv[2][X264_SCAN8_SIZE][2] );
            DECLARE_ALIGNED_4( int8_t  direct_ref[2][X264_SCAN8_SIZE] );
            DECLARE_ALIGNED_4( int16_t pskip_mv[2] );

            /* number of neighbors (top and left) that used 8x8 dct */
            int     i_neighbour_transform_size;
            int     i_neighbour_interlaced;
   } cache;

    /* */
    int     i_qp;       /* current qp */
    int     i_chroma_qp;
    int     i_last_qp;  /* last qp */
    int     i_last_dqp; /* last delta qp */
    int     b_variable_qp; /* whether qp is allowed to vary per macroblock */
    int     b_lossless;
    int     b_direct_auto_read; /* take stats for --direct auto from the 2pass log */
    int     b_direct_auto_write; /* analyse direct modes, to use and/or save */

    /* B_direct and weighted prediction */
    int16_t dist_scale_factor[16][2];
    int16_t bipred_weight[32][4];
    /* maps fref1[0]'s ref indices into the current list0 */
    int8_t  map_col_to_list0_buf[2]; // for negative indices
    int8_t  map_col_to_list0[16];


} __attribute__ ((aligned (128)));

typedef struct mb x264_mb_t;



struct x264_t
{
    /* encoder parameters */
    x264_param_t    param;

    x264_t          *thread[X264_THREAD_MAX];
    x264_pthread_t  thread_handle;
    int             b_thread_active;

#ifdef ENTROPY_THREAD
    int 	    entropy_thread;
    int 	    entropy_finish;
    LOCK_VAR 	    entropy;
#endif

    int             i_thread_phase; /* which thread to use for the next frame */

    /* bitstream output */
    struct
    {
        int         i_nal;
        x264_nal_t  nal[X264_NAL_MAX];
        int         i_bitstream;    /* size of p_bitstream */
        uint8_t     *p_bitstream;   /* will hold data for all nal */
        bs_t        bs;
        int         i_frame_size;
    } out;

    /**** thread synchronization starts here ****/

    /* frame number/poc */
    int             i_frame;

    int             i_frame_offset; /* decoding only */
    int             i_frame_num;    /* decoding only */
    int             i_poc_msb;      /* decoding only */
    int             i_poc_lsb;      /* decoding only */
    int             i_poc;          /* decoding only */

    int             i_thread_num;   /* threads only */
    int             i_nal_type;     /* threads only */
    int             i_nal_ref_idc;  /* threads only */

    /* We use only one SPS and one PPS */
    x264_sps_t      sps_array[1];
    x264_sps_t      *sps;
    x264_pps_t      pps_array[1];
    x264_pps_t      *pps;
    int             i_idr_pic_id;

    /* quantization matrix for decoding, [cqm][qp%6][coef_y][coef_x] */
    int             (*dequant4_mf[4])[4][4]; /* [4][6][4][4] */
    int             (*dequant8_mf[2])[8][8]; /* [2][6][8][8] */
    /* quantization matrix for trellis, [cqm][qp][coef] */
    int             (*unquant4_mf[4])[16];   /* [4][52][16] */
    int             (*unquant8_mf[2])[64];   /* [2][52][64] */
    /* quantization matrix for deadzone */
    uint16_t        (*quant4_mf[4])[16];     /* [4][52][16] */
    uint16_t        (*quant8_mf[2])[64];     /* [2][52][64] */
    uint16_t        (*quant4_bias[4])[16];   /* [4][52][16] */
    uint16_t        (*quant8_bias[2])[64];   /* [2][52][64] */

    const uint8_t   *chroma_qp_table; /* includes both the nonlinear luma->chroma mapping and chroma_qp_offset */

    DECLARE_ALIGNED_16( uint32_t nr_residual_sum[2][64] );
    DECLARE_ALIGNED_16( uint16_t nr_offset[2][64] );
    uint32_t        nr_count[2];

    /* Slice header */
    x264_slice_header_t sh;

    /* cabac context */
    x264_cabac_t    cabac;

    struct
    {
        /* Frames to be encoded (whose types have been decided) */
        x264_frame_t *current[X264_BFRAME_MAX*4+3];
        /* Temporary buffer (frames types not yet decided) */
        x264_frame_t *next[X264_BFRAME_MAX*4+3];
        /* Unused frames */
        x264_frame_t *unused[X264_BFRAME_MAX*4 + X264_THREAD_MAX*2 + 16+4];
        /* For adaptive B decision */
        x264_frame_t *last_nonb;

        /* frames used for reference + sentinels */
        x264_frame_t *reference[16+2];

        int i_last_idr; /* Frame number of the last IDR */

        int i_input;    /* Number of input frames already accepted */

        int i_max_dpb;  /* Number of frames allocated in the decoded picture buffer */
        int i_max_ref0;
        int i_max_ref1;
        int i_delay;    /* Number of frames buffered for B reordering */
        int b_have_lowres;  /* Whether 1/2 resolution luma planes are being used */
    } frames;

    /* current frame being encoded */
    x264_frame_t    *fenc;

    /* frame being reconstructed */
    x264_frame_t    *fdec;

    /* references lists */
    int             i_ref0;
    x264_frame_t    *fref0[16+3];     /* ref list 0 */
    int             i_ref1;
    x264_frame_t    *fref1[16+3];     /* ref list 1 */
    int             b_ref_reorder[2];


#if 0
    /* Current MB DCT coeffs */
    struct
    {
        DECLARE_ALIGNED_16( int16_t luma16x16_dc[16] );
        DECLARE_ALIGNED_16( int16_t chroma_dc[2][4] );
        // FIXME share memory?
        DECLARE_ALIGNED_16( int16_t luma8x8[4][64] );
        DECLARE_ALIGNED_16( int16_t luma4x4[16+8][16] );
    } dct;


    /* MB table and cache for current frame/mb */
    struct
    {
        int     i_mb_count;                 /* number of mbs in a frame */

        /* Strides */
        int     i_mb_stride;
        int     i_b8_stride;
        int     i_b4_stride;

        /* Current index */
        int     i_mb_x;
        int     i_mb_y;
        int     i_mb_xy;
        int     i_b8_xy;
        int     i_b4_xy;

        /* Search parameters */
        int     i_me_method;
        int     i_subpel_refine;
        int     b_chroma_me;
        int     b_trellis;
        int     b_noise_reduction;
        int     i_psy_rd; /* Psy RD strength--fixed point value*/
        int     i_psy_trellis; /* Psy trellis strength--fixed point value*/

        int     b_interlaced;

        /* Allowed qpel MV range to stay within the picture + emulated edge pixels */
        int     mv_min[2];
        int     mv_max[2];
        /* Subpel MV range for motion search.
         * same mv_min/max but includes levels' i_mv_range. */
        int     mv_min_spel[2];
        int     mv_max_spel[2];
        /* Fullpel MV range for motion search */
        int     mv_min_fpel[2];
        int     mv_max_fpel[2];

        /* neighboring MBs */
        unsigned int i_neighbour;
        unsigned int i_neighbour8[4];       /* neighbours of each 8x8 or 4x4 block that are available */
        unsigned int i_neighbour4[16];      /* at the time the block is coded */
        int     i_mb_type_top;
        int     i_mb_type_left;
        int     i_mb_type_topleft;
        int     i_mb_type_topright;
        int     i_mb_prev_xy;
        int     i_mb_top_xy;

        /**** thread synchronization ends here ****/
        /* subsequent variables are either thread-local or constant,
         * and won't be copied from one thread to another */

        /* mb table */
        int8_t  *type;                      /* mb type */
        int8_t  *qp;                        /* mb qp */
        int16_t *cbp;                       /* mb cbp: 0x0?: luma, 0x?0: chroma, 0x100: luma dc, 0x0200 and 0x0400: chroma dc  (all set for PCM)*/
        int8_t  (*intra4x4_pred_mode)[8];   /* intra4x4 pred mode. for non I4x4 set to I_PRED_4x4_DC(2) */
                                            /* actually has only 7 entries; set to 8 for write-combining optimizations */
        uint8_t (*non_zero_count)[16+4+4];  /* nzc. for I_PCM set to 16 */
        int8_t  *chroma_pred_mode;          /* chroma_pred_mode. cabac only. for non intra I_PRED_CHROMA_DC(0) */
        int16_t (*mv[2])[2];                /* mb mv. set to 0 for intra mb */
        int16_t (*mvd[2])[2];               /* mb mv difference with predict. set to 0 if intra. cabac only */
        int8_t   *ref[2];                   /* mb ref. set to -1 if non used (intra or Lx only) */
        int16_t (*mvr[2][32])[2];           /* 16x16 mv for each possible ref */
        int8_t  *skipbp;                    /* block pattern for SKIP or DIRECT (sub)mbs. B-frames + cabac only */
        int8_t  *mb_transform_size;         /* transform_size_8x8_flag of each mb */
        uint8_t *intra_border_backup[2][3]; /* bottom pixels of the previous mb row, used for intra prediction after the framebuffer has been deblocked */
        uint8_t (*nnz_backup)[16];          /* when using cavlc + 8x8dct, the deblocker uses a modified nnz */

        /* current value */
        int     i_type;
        int     i_partition;
        int     i_sub_partition[4];
        int     b_transform_8x8;

        int     i_cbp_luma;
        int     i_cbp_chroma;

        int     i_intra16x16_pred_mode;
        int     i_chroma_pred_mode;

        /* skip flags for i4x4 and i8x8
         * 0 = encode as normal.
         * 1 (non-RD only) = the DCT is still in h->dct, restore fdec and skip reconstruction.
         * 2 (RD only) = the DCT has since been overwritten by RD; restore that too. */
        int i_skip_intra;
        /* skip flag for motion compensation */
        /* if we've already done MC, we don't need to do it again */
        int b_skip_mc;

        struct
        {
            /* space for p_fenc and p_fdec */
#define FENC_STRIDE 16
#define FDEC_STRIDE 32
            DECLARE_ALIGNED_16( uint8_t fenc_buf[24*FENC_STRIDE] );
            DECLARE_ALIGNED_16( uint8_t fdec_buf[27*FDEC_STRIDE] );

            /* i4x4 and i8x8 backup data, for skipping the encode stage when possible */
            DECLARE_ALIGNED_16( uint8_t i4x4_fdec_buf[16*16] );
            DECLARE_ALIGNED_16( uint8_t i8x8_fdec_buf[16*16] );
            DECLARE_ALIGNED_16( int16_t i8x8_dct_buf[3][64] );
            DECLARE_ALIGNED_16( int16_t i4x4_dct_buf[15][16] );

            /* Psy trellis DCT data */
            DECLARE_ALIGNED_16( int16_t fenc_dct8[4][64] );
            DECLARE_ALIGNED_16( int16_t fenc_dct4[16][16] );

            /* Psy RD SATD scores */
            int fenc_satd[4][4];
            int fenc_satd_sum;
            int fenc_sa8d[2][2];
            int fenc_sa8d_sum;

            /* pointer over mb of the frame to be compressed */
            uint8_t *p_fenc[3];

            /* pointer over mb of the frame to be reconstructed  */
            uint8_t *p_fdec[3];

            /* pointer over mb of the references */
            int i_fref[2];
            uint8_t *p_fref[2][32][4+2]; /* last: lN, lH, lV, lHV, cU, cV */
            uint16_t *p_integral[2][16];

            /* fref stride */
            int     i_stride[3];
        } pic;

        /* cache */
        struct
        {
            /* real intra4x4_pred_mode if I_4X4 or I_8X8, I_PRED_4x4_DC if mb available, -1 if not */
            int8_t  intra4x4_pred_mode[X264_SCAN8_SIZE];

            /* i_non_zero_count if available else 0x80 */
            uint8_t non_zero_count[X264_SCAN8_SIZE];

            /* -1 if unused, -2 if unavailable */
            DECLARE_ALIGNED_4( int8_t ref[2][X264_SCAN8_SIZE] );

            /* 0 if not available */
            DECLARE_ALIGNED_16( int16_t mv[2][X264_SCAN8_SIZE][2] );
            DECLARE_ALIGNED_8( int16_t mvd[2][X264_SCAN8_SIZE][2] );

            /* 1 if SKIP or DIRECT. set only for B-frames + CABAC */
            DECLARE_ALIGNED_4( int8_t skip[X264_SCAN8_SIZE] );

            DECLARE_ALIGNED_16( int16_t direct_mv[2][X264_SCAN8_SIZE][2] );
            DECLARE_ALIGNED_4( int8_t  direct_ref[2][X264_SCAN8_SIZE] );
            DECLARE_ALIGNED_4( int16_t pskip_mv[2] );

            /* number of neighbors (top and left) that used 8x8 dct */
            int     i_neighbour_transform_size;
            int     i_neighbour_interlaced;
        } cache;

        /* */
        int     i_qp;       /* current qp */
        int     i_chroma_qp;
        int     i_last_qp;  /* last qp */
        int     i_last_dqp; /* last delta qp */
        int     b_variable_qp; /* whether qp is allowed to vary per macroblock */
        int     b_lossless;
        int     b_direct_auto_read; /* take stats for --direct auto from the 2pass log */
        int     b_direct_auto_write; /* analyse direct modes, to use and/or save */

        /* B_direct and weighted prediction */
        int16_t dist_scale_factor[16][2];
        int16_t bipred_weight[32][4];
        /* maps fref1[0]'s ref indices into the current list0 */
        int8_t  map_col_to_list0_buf[2]; // for negative indices
        int8_t  map_col_to_list0[16];
    } mb;
#else

    x264_mb_t mb;
    x264_dct_t dct;

#endif

#ifdef TPC

    x264_mb_t *mb_table[X264_MAX_MBS] ;
    x264_data_t *x264_data_table[X264_MAX_MBS] ;
    x264_filter_data_t *x264_filter_data_table_luma[X264_MAX_MBS] ;
    x264_filter_data_t *x264_filter_data_table_chroma[X264_MAX_MBS] ;
    x264_dct_t *dct_table[X264_MAX_MBS] ;
    
    uint32_t  tpc_id_table[X264_MAX_MBS] ;




#endif

    /* rate control encoding only */
    x264_ratecontrol_t *rc;

    /* stats */
    struct
    {
        /* Current frame stats */
        struct
        {
            /* MV bits (MV+Ref+Block Type) */
            int i_mv_bits;
            /* Texture bits (DCT coefs) */
            int i_tex_bits;
            /* ? */
            int i_misc_bits;
            /* MB type counts */
            int i_mb_count[19];
            int i_mb_count_i;
            int i_mb_count_p;
            int i_mb_count_skip;
            int i_mb_count_8x8dct[2];
            int i_mb_count_ref[2][32];
            int i_mb_partition[17];
            /* Estimated (SATD) cost as Intra/Predicted frame */
            /* XXX: both omit the cost of MBs coded as P_SKIP */
            int i_intra_cost;
            int i_inter_cost;
            int i_mbs_analysed;
            /* Adaptive direct mv pred */
            int i_direct_score[2];
            /* Metrics */
            int64_t i_ssd[3];
            double f_ssim;
        } frame;

        /* Cumulated stats */

        /* per slice info */
        int     i_slice_count[5];
        int64_t i_slice_size[5];
        double  f_slice_qp[5];
        int     i_consecutive_bframes[X264_BFRAME_MAX+1];
        /* */
        int64_t i_ssd_global[5];
        double  f_psnr_average[5];
        double  f_psnr_mean_y[5];
        double  f_psnr_mean_u[5];
        double  f_psnr_mean_v[5];
        double  f_ssim_mean_y[5];
        /* */
        int64_t i_mb_count[5][19];
        int64_t i_mb_partition[2][17];
        int64_t i_mb_count_8x8dct[2];
        int64_t i_mb_count_ref[2][2][32];
        /* */
        int     i_direct_score[2];
        int     i_direct_frames[2];

    } stat;

    /* CPU functions dependents */
    x264_predict_t      predict_16x16[4+3];
    x264_predict_t      predict_8x8c[4+3];
    x264_predict8x8_t   predict_8x8[9+3];
    x264_predict_t      predict_4x4[9+3];

    x264_pixel_function_t pixf;
    x264_mc_functions_t   mc;
    x264_dct_function_t   dctf;
    x264_zigzag_function_t zigzagf;
    x264_quant_function_t quantf;
    x264_deblock_function_t loopf;

#ifdef TPC
    /* Cell - number of SPEs to use */
    int spes;
    int idx;

    /* copy data */
    x264_copy_data_t cp_luma_data;
    x264_copy_data_t cp_chroma_data;


#ifdef MEMORY_MANAGE
    /* Dynamic memory managment */
    int i_pool_tail;
    int i_pool_head;
    /* Each entry is a mb+dct+data struct */
    void *mem_pool[X264_MAX_ALLOC];

    /* stats for dynamic memory usage */
    int i_pool_count;
#endif

    /* Bitmaps for tasks */
//    uint8_t  *mb_done_map;
//    uint8_t  *mb_call_map;
//    uint8_t  *mb_filter_map;
    uint8_t *mb_state_map;
    /* current index of MB for entropy encoding */
    uint32_t entropy_idx;
    uint32_t filter_idx;
    int i_skip;
    
#endif

#if 0
#define SAMPLE_TIME 3192
#define SAMPLES 10000
    uint64_t last_time_sample;
    uint32_t outstanding[SAMPLES];
    uint32_t pos;
#endif

#if VISUALIZE
    struct visualize_t *visualize;
#endif
#ifdef HAVE_PTHREAD2
    LOCK_VAR mb_encode_mutex;
#endif


} ;




#ifdef STAT
#define MAX_STAT 11
#define MAX_FRAME_TYPE 5

/* [SLICE_TYPE][FUNCTION]  */
extern uint64_t stats[MAX_FRAME_TYPE][MAX_STAT];
extern uint64_t stats2[MAX_STAT];
extern uint64_t en_stats[MAX_FRAME_TYPE][MAX_STAT];

enum stat_type {
        CALL=0,
	SYNC,
	ENTROPYS,
        ENTROPY_PARALLEL,
	FILTER,
        CACHE_LOAD,
        CACHE_SAVE,
        ALLOCATOR,
        MANAGE,
        FRAME_READ,
	I_FRAME

};

#endif


// included at the end because it needs x264_t
#include "macroblock.h"

#ifdef HAVE_MMX
#include "x86/util.h"
#endif



#ifdef TPC

void   x264_frame_deblock_row_tpc( x264_t *h, int mb_y, x264_mb_t *mb );
void   x264_macroblock_call_analyse_tpc(  x264_t *h, x264_mb_t *mb, int mb_i_xy, x264_dct_t *dct, int i_mb_x, int i_mb_y  );
void   x264_macroblock_call_cavlc_tpc(  x264_t *h, int mb_i_xy, x264_mb_t *mb, x264_dct_t *dct  );

void   quant_spu_init( x264_t *h);
void   x264_macroblock_call_write_cavlc_tpc( x264_t *h, bs_t *s, x264_mb_t *mb, x264_dct_t *dct );
void   x264_macroblock_call_copy_intra( x264_t *h, int j, int mb_y, x264_mb_t *mb  );
void   x264_macroblock_call_copy_plane( x264_t *h, uint8_t *dst,  uint8_t *src,  x264_copy_data_t *cp_data);

void x264_macroblock_call_deblock_luma( x264_t *h, uint8_t *pix, int mb_y );
void x264_macroblock_call_deblock_chroma( x264_t *h, uint8_t *pixu, uint8_t *pixv, int mb_y );

void x264_macroblock_call_memset( x264_t *h,  void  *dst, int size);

void x264_macroblock_cache_reload_tpc_mv( x264_t *h, int i_mb_x, int i_mb_y, x264_mb_t *mb );

extern  uint8_t mb_non_zero_count[4][X264_SCAN8_SIZE];

void print_spe_stats(void);

void x264_mb_cache_init(x264_t *h,  int k );


extern x264_copy_data_t cp_luma_data;
extern x264_copy_data_t cp_chroma_data;


#define MAX_CONTEXT 1


#include  "tpc_common.h"
#include  "tpc_ppe.h"
//#include "tpc/stats.h"


#endif

#define ALIGN(x,a) (((x)+((a)-1))&~((a)-1))

#if 1

//Cache Block Touch
#define PREFETCH(x) __dcbt (x);


//Cache Block Touch
#define CACHE_PREFETCH(x) __dcbt (x);
//Cache Block Touch for Store
#define CACHE_STORE(x) __dcbtst(x);
//Cache Block zero (destination)
#define CACHE_ZERO(x) __dcbz(x);

#define CACHE_FLUSH(x) __dcbf(x);

// __dcbt_TH1000: Set Up Streaming Data
// id: id
// eatrunc : effective address  
// d: describes which direction 1: forward 0: back
// ug: The argument ug says if the stream is unlimited in bounds or not
#define CACHE_STREAM_PREPARE(eatrunc, d, ug, id)  __dcbt_TH1000(eatrunc, d, ug, id);


// __dcbt_TH1010: Start or Stop Streaming Data

// If go is set, the program will  soon load from all nascent data streams that have been completely described, and it will probably no longer load
// from any other nascent data streams; all the rest of the arguments are ignored in this case. 
// If s is ‘10’, the stream  associated with id will stop and all other arguments except for id are ignored. 
// If s is ‘11’, all streams IDs are stopped and all other arguments are ignored. 

// unitcnt specifies the number of units in a data stream.
// t tells if the program’s need for each block of the data stream is likely to be transient. 
// u tells if the data stream is unlimited and  the unitcnt argument is ignored. 
#define CACHE_START_STOP_STREAM(go, s, unitcnt, t, u, id) __dcbt_TH1010(go, s, unitcnt, t, u, id);

#define CHACHE_START_STOP_STREAM( s, unitcnt, t, u, id) __dcbt_TH1010(1, s, unitcnt, t, u, id);
#define CACHE_STOP_STREAM( s, unitcnt, t, u, id) __dcbt_TH1010(2, s, unitcnt, t, u, id);




#else

#define PREFETCH(x) {}
#define CACHE_PREFETCH(x) {}
#define CACHE_STORE(x) {}
#define CACHE_ZERO(x) {}
#define CACHE_FLUSH(x) {}
#define CACHE_STREAM_PREPARE(eatrunc, d, ug, id)  {};
#define CACHE_START_STOP_STREAM(go, s, unitcnt, t, u, id) {};
#define CHACHE_START_STOP_STREAM( s, unitcnt, t, u, id) {};
#define CACHE_STOP_STREAM( s, unitcnt, t, u, id) {} ;



#endif




/* for pics */
extern  x264_frame_t *Gfenc_next;
extern  x264_picture_t *Gpics[2];

#endif

