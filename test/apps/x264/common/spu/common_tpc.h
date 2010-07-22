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

#ifdef DEBUG


#define P_ASSERT(a){                                  \
  __asm__ __volatile__ ("sync" : : : "memory");       \
  assert(a);                                          \
  __asm__ __volatile__ ("sync" : : : "memory");       \
                                                      \
}                                                  

#else

#define P_ASSERT(a,b)     

#endif

#define SYNC()  __asm__ __volatile__ ("sync" : : : "memory")

#ifdef HAVE_PTHREAD2

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


#define LOCK_VAR { } 
#define UNLOCK(x) { }
#define LOCK(x) { }
#define LOCK_INIT(x,y) {}

#endif




#define X264_BFRAME_MAX 64
#define X264_THREAD_MAX 128
#define X264_SLICE_MAX 4
#define X264_NAL_MAX (4 + X264_SLICE_MAX)
#define X264_PCM_COST (386*8)

// number of pixels (per thread) in progress at any given time.
// 16 for the macroblock in progress + 3 for deblocking + 3 for motion compensation filter + 2 for extra safety
#define X264_THREAD_HEIGHT 24

#define FENC_STRIDE 16
#define FDEC_STRIDE 32

/// Number of macroblocks 
/// Limits the resolution of video
/// to 4096x2160 !
#define X264_MAX_MBS 34560

/****************************************************************************
 * Includes
 ****************************************************************************/
#include <stdint.h>

#include "osdep_tpc.h"
#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "x264_tpc.h"
#include "set_tpc.h"
#include "predict_tpc.h"
#include "pixel_tpc.h"
#include "mc_tpc.h"
#include "frame_tpc.h"
// #include "dct_tpc.h"
// #include "cabac_tpc.h"
// #include "quant_tpc.h"
#include "x264_common_tpc.h"
#include "common/spu/bs_tpc.h"

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
} ;

typedef struct dct x264_dct_t;

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
            DECLARE_ALIGNED_16(   int     i_stride[3] );
        } pic;

        /* cache */
        struct
        {

            /* real intra4x4_pred_mode if I_4X4 or I_8X8, I_PRED_4x4_DC if mb available, -1 if not */
            int8_t  intra4x4_pred_mode[X264_SCAN8_SIZE];

            /* i_non_zero_count if available else 0x80 */
            DECLARE_ALIGNED_16(uint8_t non_zero_count[X264_SCAN8_SIZE]);

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


     /** Cast this var in volatile ? */

#ifdef STAT
     /** ticks for analyse  */      
     uint64_t i_analyse; 
     /** ticks for encoding */
     uint64_t i_encode;
     /** ticks for entropy encoding */
     uint64_t i_entropy;

#endif

  

} __attribute__ ((aligned (16)));

typedef struct mb x264_mb_t;





#include "macroblock_tpc.h"


/* quantization matrix for decoding, [cqm][qp%6][coef_y][coef_x] */
extern int             dequant4_mf[4][6][4][4]; /* [4][6][4][4] */
extern int             dequant8_mf[2][6][8][8]; /* [2][6][8][8] */
/* quantization matrix for trellis, [cqm][qp][coef] */
extern int             unquant4_mf[4][1][16];   /* [4][52][16] */
extern int             unquant8_mf[2][1][64];   /* [2][52][64] */

/* quantization matrix for deadzone */
extern uint16_t        quant4_mf[4][1][16];     /* [4][52][16] */
extern uint16_t        quant8_mf[2][1][64];     /* [2][52][64] */
extern uint16_t        quant4_bias[4][1][16];   /* [4][52][16] */
extern uint16_t        quant8_bias[2][1][64];   /* [2][52][64] */

extern uint32_t        nr_count[2];

#define ALIGN(x,a) (((x)+((a)-1))&~((a)-1))
#endif

