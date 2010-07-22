/*****************************************************************************
 * analyse.c: h264 encoder library
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

//#include <math.h>
#include <limits.h>


#include "common/spu/common_tpc.h"
#include "encoder/spu/me_tpc.h"

#include "encoder/spu/macroblock_tpc.h"
#include "common/spu/macroblock_tpc.h"



typedef struct
{
    /* 16x16 */
    int i_ref;
    int       i_rd16x16;
    x264_me_t me16x16;

    /* 8x8 */
    int       i_cost8x8;
    /* [ref][0] is 16x16 mv, [ref][1..4] are 8x8 mv from partition [0..3] */
    DECLARE_ALIGNED_4( int16_t mvc[32][5][2] );
    x264_me_t me8x8[4];

    /* Sub 4x4 */
    int       i_cost4x4[4]; /* cost per 8x8 partition */
    x264_me_t me4x4[4][4];

    /* Sub 8x4 */
    int       i_cost8x4[4]; /* cost per 8x8 partition */
    x264_me_t me8x4[4][2];

    /* Sub 4x8 */
    int       i_cost4x8[4]; /* cost per 8x8 partition */
    x264_me_t me4x8[4][2];

    /* 16x8 */
    int       i_cost16x8;
    x264_me_t me16x8[2];

    /* 8x16 */
    int       i_cost8x16;
    x264_me_t me8x16[2];

} x264_mb_analysis_list_t;

typedef struct
{
    /* conduct the analysis using this lamda and QP */
    int i_lambda;
    int i_lambda2;
    int i_qp;
    int16_t *p_cost_mv;
    int b_mbrd;


    /* I: Intra part */
    /* Take some shortcuts in intra search if intra is deemed unlikely */
    int b_fast_intra;
    int b_try_pskip;

    /* Luma part */
    int i_satd_i16x16;
    int i_satd_i16x16_dir[7];
    int i_predict16x16;

    int i_satd_i8x8;
    int i_satd_i8x8_dir[12][4];
    int i_predict8x8[4];

    int i_satd_i4x4;
    int i_predict4x4[16];

    int i_satd_pcm;

    /* Chroma part */
    int i_satd_i8x8chroma;
    int i_satd_i8x8chroma_dir[4];
    int i_predict8x8chroma;

    /* II: Inter part P/B frame */
    x264_mb_analysis_list_t l0;
    x264_mb_analysis_list_t l1;

    int i_cost16x16bi; /* used the same ref and mv as l0 and l1 (at least for now) */
    int i_cost16x16direct;
    int i_cost8x8bi;
    int i_cost8x8direct[4];
    int i_cost16x8bi;
    int i_cost8x16bi;
    int i_rd16x16bi;
    int i_rd16x16direct;
    int i_rd16x8bi;
    int i_rd8x16bi;
    int i_rd8x8bi;

    int i_mb_partition16x8[2]; /* mb_partition_e */
    int i_mb_partition8x16[2];
    int i_mb_type16x8; /* mb_class_e */
    int i_mb_type8x16;

    int b_direct_available;

} x264_mb_analysis_t;

/* lambda = pow(2,qp/6-2) */
const int __attribute__((aligned(128)))  x264_lambda_tab[52] = {
   1, 1, 1, 1, 1, 1, 1, 1,  /*  0-7 */
   1, 1, 1, 1,              /*  8-11 */
   1, 1, 1, 1, 2, 2, 2, 2,  /* 12-19 */
   3, 3, 3, 4, 4, 4, 5, 6,  /* 20-27 */
   6, 7, 8, 9,10,11,13,14,  /* 28-35 */
  16,18,20,23,25,29,32,36,  /* 36-43 */
  40,45,51,57,64,72,81,91   /* 44-51 */
};

/* lambda2 = pow(lambda,2) * .9 * 256 */
const int __attribute__((aligned(128)))  x264_lambda2_tab[52] = {
    14,      18,      22,      28,     36,     45,     57,     72, /*  0 -  7 */
    91,     115,     145,     182,    230,    290,    365,    460, /*  8 - 15 */
   580,     731,     921,    1161,   1462,   1843,   2322,   2925, /* 16 - 23 */
  3686,    4644,    5851,    7372,   9289,  11703,  14745,  18578, /* 24 - 31 */
 23407,   29491,   37156,   46814,  58982,  74313,  93628, 117964, /* 32 - 39 */
148626,  187257,  235929,  297252, 374514, 471859, 594505, 749029, /* 40 - 47 */
943718, 1189010, 1498059, 1887436                                  /* 48 - 51 */
};

/* TODO: calculate CABAC costs */
static const int __attribute__((aligned(16)))  i_mb_b_cost_table[X264_MBTYPE_MAX] = {
    9, 9, 9, 9, 0, 0, 0, 1, 3, 7, 7, 7, 3, 7, 7, 7, 5, 9, 0
};
static const int __attribute__((aligned(16)))  i_mb_b16x8_cost_table[17] = {
    0, 0, 0, 0, 0, 0, 0, 0, 5, 7, 7, 7, 5, 7, 9, 9, 9
};
static const int __attribute__((aligned(16)))  i_sub_mb_b_cost_table[13] = {
    7, 5, 5, 3, 7, 5, 7, 3, 7, 7, 7, 5, 1
};
static const int __attribute__((aligned(16)))  i_sub_mb_p_cost_table[4] = {
    5, 3, 3, 1
};


/* RDO need encoding support - no space ATM */
#if 0
#include "encoder/spu/rdo.c"
#endif


static void x264_analyse_update_cache_tpc( x264_data_t *data, x264_mb_analysis_t *a, x264_mb_t *mb );
uint16_t __attribute__((aligned(128)))  *x264_cost_mv_fpel[52][4];


#define C_SIZE 48

DECLARE_ALIGNED_128(static int16_t buffer0[C_SIZE*2]);
DECLARE_ALIGNED_128( static uint16_t buffer1[4][C_SIZE*2]);

/* Pre-calculate fenc satd scores for psy RD, minus DC coefficients */
static inline void x264_mb_cache_fenc_satd_tpc(  x264_mb_t *mb)
{
    DECLARE_ALIGNED_16(uint8_t zero[16]) = {0};
    uint8_t *fenc;
    int x, y, satd_sum = 0, sa8d_sum = 0;


    if( !mb->i_psy_rd )
        return;
    for( y = 0; y < 4; y++ )
        for( x = 0; x < 4; x++ )
        {
            fenc = mb->pic.p_fenc[0]+x*4+y*4*FENC_STRIDE;
            mb->pic.fenc_satd[y][x] = x264_pixel_satd_4x4( zero, 0, fenc, FENC_STRIDE )
                                      - (  x264_pixel_sad_4x4( zero, 0, fenc, FENC_STRIDE )>>1);
            satd_sum += mb->pic.fenc_satd[y][x];
        }
    for( y = 0; y < 2; y++ )
        for( x = 0; x < 2; x++ )
        {
            fenc = mb->pic.p_fenc[0]+x*8+y*8*FENC_STRIDE;
            mb->pic.fenc_sa8d[y][x] =  x264_pixel_sa8d_8x8( zero, 0, fenc, FENC_STRIDE )
                                      - ( x264_pixel_sad_8x8( zero, 0, fenc, FENC_STRIDE )>>2);
            sa8d_sum += mb->pic.fenc_sa8d[y][x];
        }
    mb->pic.fenc_satd_sum = satd_sum;
    mb->pic.fenc_sa8d_sum = sa8d_sum;
}


/* initialize an array of lambda*nbits for all possible mvs */
static void x264_mb_analyse_load_costs(  x264_data_t *data, x264_mb_analysis_t *a )
{
    static int16_t *p_cost_mv[52];
    int i, j;

    if( !p_cost_mv[a->i_qp] )
    {
#if 0
        p_cost_mv[a->i_qp] = malloc( (4*4*2048 + 1) * sizeof(int16_t) );
        p_cost_mv[a->i_qp] += 2*4*2048;
        for( i = 0; i <= 2*4*2048; i++ )
#endif
        /* could be faster, but isn't called many times */
        /* factor of 4 from qpel, 2 from sign, and 2 because mv can be opposite from mvp */
        // p_cost_mv[a->i_qp] = malloc( (2*128 + 1) * sizeof(int16_t) );
        p_cost_mv[a->i_qp] = &buffer0[0] ;
        p_cost_mv[a->i_qp] += C_SIZE;
        for( i = 0; i <= C_SIZE; i++ )
        {
            p_cost_mv[a->i_qp][-i] =
            p_cost_mv[a->i_qp][i]  = a->i_lambda * bs_size_se( i );
        }
    }
    a->p_cost_mv = p_cost_mv[a->i_qp];

    /* FIXME is this useful for all me methods? */
    if( data->i_me_method >= X264_ME_ESA && !x264_cost_mv_fpel[a->i_qp][0] )
    {
#if 0
        for( j=0; j<4; j++ )
        {
            x264_cost_mv_fpel[a->i_qp][j] = x264_malloc( (4*2048 + 1) * sizeof(int16_t) );
            x264_cost_mv_fpel[a->i_qp][j] += 2*2048;
            for( i = -2*2048; i < 2*2048; i++ )
                x264_cost_mv_fpel[a->i_qp][j][i] = p_cost_mv[a->i_qp][i*4+j];
        }
#else
        for( j=0; j<4; j++ )
        {
//            x264_cost_mv_fpel[a->i_qp][j] = malloc( (2*128 + 1) * sizeof(int16_t) );
            x264_cost_mv_fpel[a->i_qp][j] = &buffer1[j][0] ;
            x264_cost_mv_fpel[a->i_qp][j] += C_SIZE;
            for( i = -C_SIZE; i < C_SIZE; i++ )
                x264_cost_mv_fpel[a->i_qp][j][i] = p_cost_mv[a->i_qp][i*4+j];
        }
#endif


    }

}

/*
 * Handle intra mb
 */
/* Max = 4 */
static void predict_16x16_mode_available( unsigned int i_neighbour, int *mode, int *pi_count )
{

    if( i_neighbour & MB_TOPLEFT )
    {
        /* top and left available */
        *mode++ = I_PRED_16x16_V;
        *mode++ = I_PRED_16x16_H;
        *mode++ = I_PRED_16x16_DC;
        *mode++ = I_PRED_16x16_P;
        *pi_count = 4;
    }
    else if( i_neighbour & MB_LEFT )
    {
        /* left available*/
        *mode++ = I_PRED_16x16_DC_LEFT;
        *mode++ = I_PRED_16x16_H;
        *pi_count = 2;
    }
    else if( i_neighbour & MB_TOP )
    {
        /* top available*/
        *mode++ = I_PRED_16x16_DC_TOP;
        *mode++ = I_PRED_16x16_V;
        *pi_count = 2;
    }
    else
    {
        /* none available */
        *mode = I_PRED_16x16_DC_128;
        *pi_count = 1;
    }
}

/* Max = 4 */
static void predict_8x8chroma_mode_available( unsigned int i_neighbour, int *mode, int *pi_count )
{
    if( i_neighbour & MB_TOPLEFT )
    {
        /* top and left available */
        *mode++ = I_PRED_CHROMA_V;
        *mode++ = I_PRED_CHROMA_H;
        *mode++ = I_PRED_CHROMA_DC;
        *mode++ = I_PRED_CHROMA_P;
        *pi_count = 4;
    }
    else if( i_neighbour & MB_LEFT )
    {
        /* left available*/
        *mode++ = I_PRED_CHROMA_DC_LEFT;
        *mode++ = I_PRED_CHROMA_H;
        *pi_count = 2;
    }
    else if( i_neighbour & MB_TOP )
    {
        /* top available*/
        *mode++ = I_PRED_CHROMA_DC_TOP;
        *mode++ = I_PRED_CHROMA_V;
        *pi_count = 2;
    }
    else
    {
        /* none available */
        *mode = I_PRED_CHROMA_DC_128;
        *pi_count = 1;
    }
}


#define LOAD_FENC( m, src, xoff, yoff) \
    (m)->i_stride[0] = mb->pic.i_stride[0]; \
    (m)->i_stride[1] = mb->pic.i_stride[1]; \
    (m)->p_fenc[0] = &(src)[0][(xoff)+(yoff)*FENC_STRIDE]; \
    (m)->p_fenc[1] = &(src)[1][((xoff)>>1)+((yoff)>>1)*FENC_STRIDE]; \
    (m)->p_fenc[2] = &(src)[2][((xoff)>>1)+((yoff)>>1)*FENC_STRIDE];


#define LOAD_HPELS(m, src, list, ref, xoff, yoff) \
    (m)->p_fref[0] = &(src)[0][(xoff)+(yoff)*(m)->i_stride[0]]; \
    (m)->p_fref[1] = &(src)[1][(xoff)+(yoff)*(m)->i_stride[0]]; \
    (m)->p_fref[2] = &(src)[2][(xoff)+(yoff)*(m)->i_stride[0]]; \
    (m)->p_fref[3] = &(src)[3][(xoff)+(yoff)*(m)->i_stride[0]]; \
    (m)->p_fref[4] = &(src)[4][((xoff)>>1)+((yoff)>>1)*(m)->i_stride[1]]; \
    (m)->p_fref[5] = &(src)[5][((xoff)>>1)+((yoff)>>1)*(m)->i_stride[1]];

#define REF_COST(list, ref) \
    (a->i_lambda * bs_size_te( data->i_num_ref_idx_l##list##_active - 1, ref ))





static void x264_mb_analyse_init_tpc( x264_data_t *data, x264_mb_analysis_t *a,  x264_mb_t *mb )
{
    int i_qp = mb->i_qp;
    /* conduct the analysis using this lamda and QP */
    a->i_qp =  i_qp;


    /* FIXME: TODO: Chroma qp !*/
    mb->i_chroma_qp = data->i_chroma_qp;

    /* Try this */
    /* Note thath we don't support qp index offset  */
    const uint8_t   *chroma_qp_table =  i_chroma_qp_table + 12 ;
    mb->i_chroma_qp = chroma_qp_table[i_qp ];

    a->i_lambda = x264_lambda_tab[i_qp];
    a->i_lambda2 = x264_lambda2_tab[i_qp];
#if 0
    a->b_mbrd = data-> i_subpel_refine >= 6 &&
                ( h->sh.i_type != SLICE_TYPE_B || h->param.analyse.b_bframe_rdo );
    mb->b_trellis = h->param.analyse.i_trellis > 1 && a->b_mbrd;
    mb->i_skip_intra =
        mb->b_lossless ? 0 :
        a->b_mbrd ? 2 :
        !h->param.analyse.i_trellis && !h->param.analyse.i_noise_reduction;
#else
     a->b_mbrd = 0;
     mb->b_trellis = 0;
     mb->i_skip_intra = 1;
#endif

    mb->i_me_method = data->i_me_method;
    mb->i_subpel_refine = data->i_subpel_refine;
    mb->b_chroma_me = data->b_chroma_me && mb->i_type == SLICE_TYPE_P
                        && mb->i_subpel_refine >= 5;


    mb->b_transform_8x8 = 0;
    mb->b_noise_reduction = 0;

    /* I: Intra part */
    a->i_satd_i16x16 =
    a->i_satd_i8x8   =
    a->i_satd_i4x4   =
    a->i_satd_i8x8chroma = COST_MAX;

    /* non-RD PCM decision is inaccurate (as is psy-rd), so don't do it */
    a->i_satd_pcm = !mb->i_psy_rd && a->b_mbrd ? ((uint64_t)X264_PCM_COST*a->i_lambda2 + 128) >> 8 : COST_MAX;

    a->b_fast_intra = 0;



    /* II: Inter part P/B frame */
    if( mb->i_type != SLICE_TYPE_I )
    {
        int i, j;
        int i_fmv_range = 4 * data->i_mv_range;
        // limit motion search to a slightly smaller range than the theoretical limit,
        // since the search may go a few iterations past its given range
//        int i_fpel_border = 5; // umh unconditional radius
        int i_fpel_border = 4; // umh unconditional radius
        int i_spel_border = 8; // 1.5 for subpel_satd, 1.5 for subpel_rd, 2 for bime, round up

        /* Calculate max allowed MV range */
#define CLIP_FMV(mv) x264_clip3( mv, -i_fmv_range, i_fmv_range-1 )



        mb->mv_min[0] =  X264_MAX( 4*( -16*(mb->i_mb_x) - 24 ) ,   -data->i_me_range  );
        mb->mv_max[0] = X264_MIN( 4*( 16*( data->i_mb_width - mb->i_mb_x - 1 ) + 24 ) ,   data->i_me_range + 32 );
        mb->mv_min_spel[0] = CLIP_FMV( mb->mv_min[0] ) ;
        mb->mv_max_spel[0] = CLIP_FMV( mb->mv_max[0] ) ;
        mb->mv_min_fpel[0] = (mb->mv_min_spel[0]>>2) + i_fpel_border ;
        mb->mv_max_fpel[0] = (mb->mv_max_spel[0]>>2) - i_fpel_border ;


       // if( mb->i_mb_x == 0)
        {
            int mb_y = mb->i_mb_y >> data->b_mbaff;
            int mb_height = data->i_mb_height >> data->b_mbaff;
            int thread_mvy_range = i_fmv_range;


            mb->mv_min[1] = 4*( -16*mb_y - 24 );
            mb->mv_max[1] = 4*( 16*( mb_height - mb_y - 1 ) + 24 );


            mb->mv_min_spel[1] = X264_MAX(  CLIP_FMV( mb->mv_min[1] ),  -data->i_me_range      );
            mb->mv_max_spel[1] = X264_MIN(  CLIP_FMV( mb->mv_max[1] ),   data->i_me_range + 32   );


            mb->mv_min_fpel[1] = (mb->mv_min_spel[1]>>2) + i_fpel_border;
            mb->mv_max_fpel[1] = (mb->mv_max_spel[1]>>2) - i_fpel_border;
        }





#undef CLIP_FMV

        a->l0.me16x16.cost =
        a->l0.i_rd16x16    =
        a->l0.i_cost8x8    = COST_MAX;

        for( i = 0; i < 4; i++ )
        {
            a->l0.i_cost4x4[i] =
            a->l0.i_cost8x4[i] =
            a->l0.i_cost4x8[i] = COST_MAX;
        }

        a->l0.i_cost16x8   =
        a->l0.i_cost8x16   = COST_MAX;
        if( mb->i_type == SLICE_TYPE_B )
        {
            a->l1.me16x16.cost =
            a->l1.i_rd16x16    =
            a->l1.i_cost8x8    = COST_MAX;

            for( i = 0; i < 4; i++ )
            {
                a->l1.i_cost4x4[i] =
                a->l1.i_cost8x4[i] =
                a->l1.i_cost4x8[i] =
                a->i_cost8x8direct[i] = COST_MAX;
            }

            a->l1.i_cost16x8   =
            a->l1.i_cost8x16   =
            a->i_rd16x16bi     =
            a->i_rd16x16direct =
            a->i_rd8x8bi       =
            a->i_rd16x8bi      =
            a->i_rd8x16bi      =
            a->i_cost16x16bi   =
            a->i_cost16x16direct =
            a->i_cost8x8bi     =
            a->i_cost16x8bi    =
            a->i_cost8x16bi    = COST_MAX;
        }
        mb->b_skip_mc = 0;
    }

}


#if 1

static void x264_mb_analyse_intra_tpc(   x264_data_t *data, x264_mb_analysis_t *a, int i_satd_inter, x264_mb_t *mb )
{
    uint8_t  *p_src = mb->pic.p_fenc[0];
    uint8_t  *p_dst = mb->pic.p_fdec[0];

    int i, idx;
    int i_max;
    int predict_mode[9];

    /*---------------- Try all mode and calculate their score ---------------*/

    /* 16x16 prediction selection */
    predict_16x16_mode_available( mb->i_neighbour, predict_mode, &i_max );

        for( i = 0; i < i_max; i++ )
        {
            int i_satd;
            int i_mode = predict_mode[i];
            predictf[i_mode]( p_dst );

            i_satd = satdf[PIXEL_16x16]( p_dst, FDEC_STRIDE, p_src, FENC_STRIDE ) +
                    a->i_lambda * bs_size_ue( x264_mb_pred_mode16x16_fix[i_mode] );


            COPY2_IF_LT( a->i_satd_i16x16, i_satd, a->i_predict16x16, i_mode );
            a->i_satd_i16x16_dir[i_mode] = i_satd;
        }


    if( data->i_type == SLICE_TYPE_B )
        /* cavlc mb type prefix */
        a->i_satd_i16x16 += a->i_lambda * i_mb_b_cost_table[I_16x16];

    if( a->b_fast_intra && a->i_satd_i16x16 > 2*i_satd_inter )
        return;



}

#endif

#if 0
static void x264_mb_analyse_inter_p16x16_tpc(  x264_data_t *data, x264_mb_analysis_t *a, x264_mb_t *mb )
{
    x264_me_t m;
    int i_ref, i_mvc;
    DECLARE_ALIGNED_4( int16_t mvc[8][2] );
    int i_halfpel_thresh = INT_MAX;
    int *p_halfpel_thresh = mb->pic.i_fref[0]>1 ? &i_halfpel_thresh : NULL;

    /* 16x16 Search on all ref frame */
    m.i_pixel = PIXEL_16x16;
    m.p_cost_mv = a->p_cost_mv;
    LOAD_FENC( &m, mb->pic.p_fenc, 0, 0 );

    a->l0.me16x16.cost = INT_MAX;


    for( i_ref = 0; i_ref < mb->pic.i_fref[0]; i_ref++ )
    {
        const int i_ref_cost = REF_COST( 0, i_ref );
        i_halfpel_thresh -= i_ref_cost;
        m.i_ref_cost = i_ref_cost;
        m.i_ref = i_ref;

        /* search with ref */
        LOAD_HPELS( &m, mb->pic.p_fref[0][i_ref], 0, i_ref, 0, 0 );
        x264_mb_predict_mv_16x16_tpc(  0, i_ref, m.mvp, mb );

        x264_mb_predict_mv_ref16x16_tpc( data, 0, i_ref, mvc, &i_mvc, mb);
        x264_me_search_ref_tpc( data, &m, mvc, i_mvc, p_halfpel_thresh, mb );

        /* early termination
         * SSD threshold would probably be better than SATD */
        if( i_ref == 0
            && a->b_try_pskip
            && m.cost-m.cost_mv < 300*a->i_lambda
            &&  abs(m.mv[0]-mb->cache.pskip_mv[0])
              + abs(m.mv[1]-mb->cache.pskip_mv[1]) <= 1
            && x264_macroblock_probe_pskip_tpc( data, mb ) )
        {
            mb->i_type = P_SKIP;

       	    x264_analyse_update_cache_tpc( data, a, mb );
             return;
        }

        m.cost += i_ref_cost;
        i_halfpel_thresh += i_ref_cost;

        if( m.cost < a->l0.me16x16.cost )
          memcpy( &a->l0.me16x16, &m, sizeof(x264_me_t) );

        /* save mv for predicting neighbors */
        *(uint32_t*)a->l0.mvc[i_ref][0] = *(uint32_t*)m.mv;
      //  *(uint32_t*)mb->mvr[0][i_ref][mb->i_mb_xy] = *(uint32_t*)m.mv;
    }

    x264_macroblock_cache_ref_tpc( 0, 0, 4, 4, 0, a->l0.me16x16.i_ref, mb );

    mb->i_type = P_L0;
    if( a->b_mbrd )
    {
        x264_mb_cache_fenc_satd_tpc( mb );
        if( a->l0.me16x16.i_ref == 0 && *(uint32_t*)a->l0.me16x16.mv == *(uint32_t*)mb->cache.pskip_mv )
        {
            mb->i_partition = D_16x16;
            x264_macroblock_cache_mv_ptr_tpc(  0, 0, 4, 4, 0, a->l0.me16x16.mv, mb );
  //          a->l0.i_rd16x16 = x264_rd_cost_mb( h, a->i_lambda2 );
        }
    }

}


static void x264_mb_analyse_inter_b16x16_tpc( x264_data_t *data, x264_mb_analysis_t *a, x264_mb_t *mb )
{
    DECLARE_ALIGNED_16( uint8_t pix0[16*16] );
    DECLARE_ALIGNED_16( uint8_t pix1[16*16] );
    uint8_t *src0, *src1;
    int stride0 = 16, stride1 = 16;

    x264_me_t m;
    int i_ref, i_mvc;
    DECLARE_ALIGNED_16( int16_t mvc[9][2] );
    int i_halfpel_thresh = INT_MAX;
    int *p_halfpel_thresh = mb->pic.i_fref[0]>1 ? &i_halfpel_thresh : NULL;

    /* 16x16 Search on all ref frame */
    m.i_pixel = PIXEL_16x16;
    m.p_cost_mv = a->p_cost_mv;
    LOAD_FENC( &m, mb->pic.p_fenc, 0, 0 );
    /* ME for List 0 */
    a->l0.me16x16.cost = INT_MAX;
    for( i_ref = 0; i_ref < mb->pic.i_fref[0]; i_ref++ )
    {

        /* search with ref */
        LOAD_HPELS( &m, mb->pic.p_fref[0][i_ref], 0, i_ref, 0, 0 );
        x264_mb_predict_mv_16x16_tpc(  0, i_ref, m.mvp, mb );
        x264_mb_predict_mv_ref16x16_tpc( data, 0, i_ref, mvc, &i_mvc, mb );

        x264_me_search_ref_tpc( data,  &m, mvc, i_mvc, p_halfpel_thresh, mb );

        /* add ref cost */
        m.cost += REF_COST( 0, i_ref );

        if( m.cost < a->l0.me16x16.cost )
        {
            a->l0.i_ref = i_ref;
            memcpy(&a->l0.me16x16, &m, sizeof(x264_me_t));

        }

	/* TODO: FIX ME - Used by cabac */
        /* save mv for predicting neighbors */
       // *(uint32_t*)mb->mvr[0][i_ref][mb->i_mb_xy] = *(uint32_t*)m.mv;
    }
    /* subtract ref cost, so we don't have to add it for the other MB types */
    a->l0.me16x16.cost -= REF_COST( 0, a->l0.i_ref );
    /* ME for list 1 */
    i_halfpel_thresh = INT_MAX;
    p_halfpel_thresh = mb->pic.i_fref[1]>1 ? &i_halfpel_thresh : NULL;
    a->l1.me16x16.cost = INT_MAX;
    for( i_ref = 0; i_ref < mb->pic.i_fref[1]; i_ref++ )
    {
        /* search with ref */
        LOAD_HPELS( &m, mb->pic.p_fref[1][i_ref], 1, i_ref, 0, 0 );
        x264_mb_predict_mv_16x16_tpc( 1, i_ref, m.mvp, mb );
        x264_mb_predict_mv_ref16x16_tpc( data, 1, i_ref, mvc, &i_mvc, mb );
        x264_me_search_ref_tpc(data, &m, mvc, i_mvc, p_halfpel_thresh, mb );
        /* add ref cost */
        m.cost += REF_COST( 1, i_ref );

        if( m.cost < a->l1.me16x16.cost )
        {
            a->l1.i_ref = i_ref;
            memcpy( &a->l1.me16x16, &m, sizeof(x264_me_t) );
        }

	/* TODO: FIX ME */
        /* save mv for predicting neighbors */
//        *(uint32_t*)mb->mvr[1][i_ref][mb->i_mb_xy] = *(uint32_t*)m.mv;
    }
    /* subtract ref cost, so we don't have to add it for the other MB types */
    a->l1.me16x16.cost -= REF_COST( 1, a->l1.i_ref );

    /* Set global ref, needed for other modes? */
    x264_macroblock_cache_ref_tpc( 0, 0, 4, 4, 0, a->l0.i_ref, mb );

    x264_macroblock_cache_ref_tpc( 0, 0, 4, 4, 1, a->l1.i_ref, mb );

    /* get cost of BI mode */
    src0 = get_ref( pix0, &stride0,
                           mb->pic.p_fref[0][a->l0.i_ref], mb->pic.i_stride[0],
                           a->l0.me16x16.mv[0], a->l0.me16x16.mv[1], 16, 16 );
    src1 = get_ref( pix1, &stride1,
                           mb->pic.p_fref[1][a->l1.i_ref], mb->pic.i_stride[0],
                           a->l1.me16x16.mv[0], a->l1.me16x16.mv[1], 16, 16 );


    pixel_avg_16x16( pix0, 16, src0, stride0, src1, stride1, mb->bipred_weight[a->l0.i_ref][a->l1.i_ref] );

    a->i_cost16x16bi = mbcmpf[PIXEL_16x16]( mb->pic.p_fenc[0], FENC_STRIDE, pix0, 16 )
                     + REF_COST( 0, a->l0.i_ref )
                     + REF_COST( 1, a->l1.i_ref )
                     + a->l0.me16x16.cost_mv
                     + a->l1.me16x16.cost_mv;
    /* mb type cost */
    a->i_cost16x16bi   += a->i_lambda * i_mb_b_cost_table[B_BI_BI];

    a->l0.me16x16.cost += a->i_lambda * i_mb_b_cost_table[B_L0_L0];
    a->l1.me16x16.cost += a->i_lambda * i_mb_b_cost_table[B_L1_L1];


}
#else

static void x264_mb_analyse_inter_p16x16_tpc(  x264_data_t *data, x264_mb_analysis_t *a, x264_mb_t *mb )
{
    x264_me_t m;
    int  i_mvc;
    DECLARE_ALIGNED_4( int16_t mvc[8][2] );
    int i_halfpel_thresh = INT_MAX;
    int *p_halfpel_thresh = mb->pic.i_fref[0]>1 ? &i_halfpel_thresh : NULL;

    /* 16x16 Search on all ref frame */
    m.i_pixel = PIXEL_16x16;
    m.p_cost_mv = a->p_cost_mv;
    LOAD_FENC( &m, mb->pic.p_fenc, 0, 0 );

    a->l0.me16x16.cost = INT_MAX;


    const int i_ref_cost = REF_COST( 0, 0 );
    i_halfpel_thresh -= i_ref_cost;
    m.i_ref_cost = i_ref_cost;
    m.i_ref = 0;

    /* search with ref */
    LOAD_HPELS( &m, mb->pic.p_fref[0][0], 0, 0, 0, 0 );
    x264_mb_predict_mv_16x16_tpc(  0, 0, m.mvp, mb );

    x264_mb_predict_mv_ref16x16_tpc( data, 0, 0, mvc, &i_mvc, mb);
    x264_me_search_ref_tpc( data, &m, mvc, i_mvc, p_halfpel_thresh, mb );

    /* early termination
     * SSD threshold would probably be better than SATD */
    if( 
            a->b_try_pskip
            && m.cost-m.cost_mv < 300*a->i_lambda
            &&  abs(m.mv[0]-mb->cache.pskip_mv[0])
            + abs(m.mv[1]-mb->cache.pskip_mv[1]) <= 1
            && x264_macroblock_probe_pskip_tpc( data, mb ) )
    {
        mb->i_type = P_SKIP;

        x264_analyse_update_cache_tpc( data, a, mb );
        return;
    }

    m.cost += i_ref_cost;
    i_halfpel_thresh += i_ref_cost;

    memcpy( &a->l0.me16x16, &m, sizeof(x264_me_t) );

    /* save mv for predicting neighbors */
    *(uint32_t*)a->l0.mvc[0][0] = *(uint32_t*)m.mv;

    x264_macroblock_cache_ref_tpc( 0, 0, 4, 4, 0, 0, mb );

    mb->i_type = P_L0;
    if( a->b_mbrd )
    {
        x264_mb_cache_fenc_satd_tpc( mb );
        if( a->l0.me16x16.i_ref == 0 && *(uint32_t*)a->l0.me16x16.mv == *(uint32_t*)mb->cache.pskip_mv )
        {
            mb->i_partition = D_16x16;
            x264_macroblock_cache_mv_ptr_tpc(  0, 0, 4, 4, 0, a->l0.me16x16.mv, mb );
            //          a->l0.i_rd16x16 = x264_rd_cost_mb( h, a->i_lambda2 );
        }
    }

}


static void x264_mb_analyse_inter_b16x16_tpc( x264_data_t *data, x264_mb_analysis_t *a, x264_mb_t *mb )
{
    DECLARE_ALIGNED_16( uint8_t pix0[16*16] );
    DECLARE_ALIGNED_16( uint8_t pix1[16*16] );
    uint8_t *src0, *src1;
    int stride0 = 16, stride1 = 16;

    x264_me_t m;
    int  i_mvc;
    DECLARE_ALIGNED_16( int16_t mvc[9][2] );
    int i_halfpel_thresh = INT_MAX;
    int *p_halfpel_thresh = mb->pic.i_fref[0]>1 ? &i_halfpel_thresh : NULL;

    /* 16x16 Search on all ref frame */
    m.i_pixel = PIXEL_16x16;
    m.p_cost_mv = a->p_cost_mv;
    LOAD_FENC( &m, mb->pic.p_fenc, 0, 0 );
    /* ME for List 0 */
    a->l0.me16x16.cost = INT_MAX;

    /* search with ref */
    LOAD_HPELS( &m, mb->pic.p_fref[0][0], 0, 0, 0, 0 );
    x264_mb_predict_mv_16x16_tpc(  0, 0, m.mvp, mb );
    x264_mb_predict_mv_ref16x16_tpc( data, 0, 0, mvc, &i_mvc, mb );

    x264_me_search_ref_tpc( data,  &m, mvc, i_mvc, p_halfpel_thresh, mb );

    /* add ref cost */
    m.cost += REF_COST( 0, 0 );

    a->l0.i_ref = 0;
    memcpy(&a->l0.me16x16, &m, sizeof(x264_me_t));


    /* subtract ref cost, so we don't have to add it for the other MB types */
    a->l0.me16x16.cost -= REF_COST( 0, 0 );
    /* ME for list 1 */
    i_halfpel_thresh = INT_MAX;
    p_halfpel_thresh = mb->pic.i_fref[1]>1 ? &i_halfpel_thresh : NULL;
    a->l1.me16x16.cost = INT_MAX;
    /* search with ref */
    LOAD_HPELS( &m, mb->pic.p_fref[1][0], 1, 0, 0, 0 );
    x264_mb_predict_mv_16x16_tpc( 1, 0, m.mvp, mb );
    x264_mb_predict_mv_ref16x16_tpc( data, 1, 0, mvc, &i_mvc, mb );
    x264_me_search_ref_tpc(data, &m, mvc, i_mvc, p_halfpel_thresh, mb );
    /* add ref cost */
    m.cost += REF_COST( 1, 0 );

    a->l1.i_ref = 0;
    memcpy( &a->l1.me16x16, &m, sizeof(x264_me_t) );


    /* subtract ref cost, so we don't have to add it for the other MB types */
    a->l1.me16x16.cost -= REF_COST( 1, 0 );

    /* Set global ref, needed for other modes? */
    x264_macroblock_cache_ref_tpc( 0, 0, 4, 4, 0, 0, mb );

    x264_macroblock_cache_ref_tpc( 0, 0, 4, 4, 1, 0, mb );

    /* get cost of BI mode */
    src0 = get_ref( pix0, &stride0,
            mb->pic.p_fref[0][0], mb->pic.i_stride[0],
            a->l0.me16x16.mv[0], a->l0.me16x16.mv[1], 16, 16 );
    src1 = get_ref( pix1, &stride1,
            mb->pic.p_fref[1][0], mb->pic.i_stride[0],
            a->l1.me16x16.mv[0], a->l1.me16x16.mv[1], 16, 16 );


    pixel_avg_16x16( pix0, 16, src0, stride0, src1, stride1, mb->bipred_weight[0][0] );

    a->i_cost16x16bi = mbcmpf[PIXEL_16x16]( mb->pic.p_fenc[0], FENC_STRIDE, pix0, 16 )
        + REF_COST( 0, 0 )
        + REF_COST( 1, 0 )
        + a->l0.me16x16.cost_mv
        + a->l1.me16x16.cost_mv;
    /* mb type cost */
    a->i_cost16x16bi   += a->i_lambda * i_mb_b_cost_table[B_BI_BI];

    a->l0.me16x16.cost += a->i_lambda * i_mb_b_cost_table[B_L0_L0];
    a->l1.me16x16.cost += a->i_lambda * i_mb_b_cost_table[B_L1_L1];


}

#endif

static void x264_mb_analyse_intra_chroma_tpc( x264_data_t *data, x264_mb_analysis_t *a, x264_mb_t *mb )
{
    int i;
    int i_max;
    int predict_mode[4];

    uint8_t *p_dstc[2], *p_srcc[2];

    if( a->i_satd_i8x8chroma < COST_MAX )
        return;

    /* 8x8 prediction selection for chroma */
    p_dstc[0] = mb->pic.p_fdec[1];
    p_dstc[1] = mb->pic.p_fdec[2];
    p_srcc[0] = mb->pic.p_fenc[1];
    p_srcc[1] = mb->pic.p_fenc[2];

    predict_8x8chroma_mode_available( mb->i_neighbour, predict_mode, &i_max );
    a->i_satd_i8x8chroma = COST_MAX;

        for( i=0; i<i_max; i++ )
        {
            int i_satd;
            int i_mode = predict_mode[i];

            /* we do the prediction */
            predict_8x8cf[i_mode]( p_dstc[0] );
            predict_8x8cf[i_mode]( p_dstc[1] );

            /* we calculate the cost */
            i_satd = mbcmpf[PIXEL_8x8]( p_dstc[0], FDEC_STRIDE,
                                               p_srcc[0], FENC_STRIDE ) +
                     mbcmpf[PIXEL_8x8]( p_dstc[1], FDEC_STRIDE,
                                               p_srcc[1], FENC_STRIDE ) +
                     a->i_lambda * bs_size_ue( x264_mb_pred_mode8x8c_fix[i_mode] );

            a->i_satd_i8x8chroma_dir[i] = i_satd;
            COPY2_IF_LT( a->i_satd_i8x8chroma, i_satd, a->i_predict8x8chroma, i_mode );
        }
   
    mb->i_chroma_pred_mode = a->i_predict8x8chroma;
}


#if 1
/*****************************************************************************
 * x264_macroblock_analyse ( all ? ):                         * 
 *****************************************************************************/
void x264_macroblock_analyse_tpc( x264_data_t *data, x264_mb_t *mb, int mb_i_xy, x264_dct_t *dct )
{
    int i_cost = COST_MAX;
    int i;
    x264_mb_analysis_t analysis_tmp; 
    x264_mb_analysis_t *analysis = &analysis_tmp;

    x264_mb_analyse_init_tpc(data, analysis, mb );
    /*--------------------------- Do the analysis ---------------------------*/
    if( data->i_type == SLICE_TYPE_P )
    {
        int b_skip = 0;
        int i_intra_cost, i_intra_type;

        /* Fast P_SKIP detection */
        analysis->b_try_pskip = 0;

        if( data->b_fast_pskip )
        {
            if( data->i_subpel_refine >= 3 )
                analysis->b_try_pskip = 1;
            else if( mb->i_mb_type_left == P_SKIP ||
                    mb->i_mb_type_top == P_SKIP ||
                    mb->i_mb_type_topleft == P_SKIP ||
                    mb->i_mb_type_topright == P_SKIP )
                b_skip = x264_macroblock_probe_pskip_tpc( data, mb );
        }

        if( b_skip )
        {
            mb->i_type = P_SKIP;
            mb->i_partition = D_16x16;
        }
        else
        {
            int i_type;
            int i_partition;
            int i_thresh16x8;
            int i_satd_inter, i_satd_intra;

            x264_mb_analyse_load_costs( data, analysis );

            x264_mb_analyse_inter_p16x16_tpc( data, analysis, mb );

            if( mb->i_type == P_SKIP )
                return;


            /* Select best inter mode */
            i_type = P_L0;
            i_partition = D_16x16;
            i_cost = analysis->l0.me16x16.cost;


            mb->i_partition = i_partition;

            /* refine qpel */
            //FIXME mb_type costs?
            if( analysis->b_mbrd )
            {

                /* refine later */
            }
            else if( i_partition == D_16x16 )
            {
                x264_me_refine_qpel_tpc( data, &analysis->l0.me16x16, mb);
                i_cost = analysis->l0.me16x16.cost;
            }
#ifndef NDEBUG
	    else{
	        fprintf(stderr,"internal error (!8x16 && !16x8 && !8x8 && !4x4 )\n" );
                SYNC();
            }
#endif

            i_satd_inter = i_cost;

            i_satd_intra = analysis->i_satd_i16x16;

            i_intra_type = I_16x16;
            i_intra_cost = analysis->i_satd_i16x16;

#if 0
            COPY2_IF_LT( i_cost, i_intra_cost, i_type, i_intra_type );
#endif

            if( i_intra_cost == COST_MAX )
                i_intra_cost = i_cost * i_satd_intra / i_satd_inter + 1;

            mb->i_type = i_type;



            /* TODO: Check if we can enable any of this even subpel >= 7 isn't common
             * option.
             *
             *  */
#if 1
            if( mb->i_subpel_refine >= 7 && mb->i_type != I_PCM )
            {
   /*             if( IS_INTRA( mb->i_type ) )
                {
                    x264_intra_rd_refine_tpc( h, analysis, mb, dct );
                    x264_intra_rd_refine( h, analysis );
                }
                else
 */
                if( i_partition == D_16x16 )
                {
                    x264_macroblock_cache_ref_tpc( 0, 0, 4, 4, 0, analysis->l0.me16x16.i_ref, mb );
                    x264_me_refine_qpel_rd_tpc( data, &analysis->l0.me16x16, analysis->i_lambda2, 0, mb );
                }
            }
#endif

            mb->i_type = i_type;

        }


    }
    else if( data->i_type == SLICE_TYPE_B )
    {

        int i_bskip_cost = COST_MAX;
        int b_skip = 0;
        mb->i_type = B_SKIP;

        analysis->b_direct_available = x264_mb_predict_mv_direct16x16_tpc(  NULL, mb );
        analysis->b_direct_available = 0; // x264_mb_predict_mv_direct16x16_tpc(  NULL, mb );

#if 0
        if( 1 )
        {
            //if( !mb->b_direct_auto_write )
	//	x264_mb_mc_tpc(data,mb);

            if( analysis->b_mbrd )
            {
                i_bskip_cost = ssd_mb_tpc(data,mb );
                /* 6 = minimum cavlc cost of a non-skipped MB */
                b_skip = mb->b_skip_mc = i_bskip_cost <= ((6 * analysis->i_lambda2 + 128) >> 8);
            }
            else if( mb->b_direct_auto_write )
            {
                /* Conditioning the probe on neighboring block types
                 * doesn't seem to help speed or quality. */
            }
		b_skip = x264_macroblock_probe_bskip_tpc(data,mb);
        }
#endif
        /* Here we enter in main analyse */
        if( !b_skip )
        {

            int i_type;
            int i_partition;
            int i_satd_inter = 0; // shut up uninitialized warning
            mb->b_skip_mc = 0;

            x264_mb_analyse_load_costs( data, analysis );

            x264_mb_analyse_inter_b16x16_tpc( data, analysis, mb );
            i_type = B_L0_L0;
            i_partition = D_16x16;
            i_cost = analysis->l0.me16x16.cost;
            COPY2_IF_LT( i_cost, analysis->l1.me16x16.cost, i_type, B_L1_L1 );
            COPY2_IF_LT( i_cost, analysis->i_cost16x16bi, i_type, B_BI_BI );




            if( analysis->b_mbrd )
            {
                /* refine later */
            }
            /* refine qpel */
            else if( i_partition == D_16x16 )
            {
                analysis->l0.me16x16.cost -= analysis->i_lambda * i_mb_b_cost_table[B_L0_L0];
                analysis->l1.me16x16.cost -= analysis->i_lambda * i_mb_b_cost_table[B_L1_L1];
                if( i_type == B_L0_L0 )
                {
                    x264_me_refine_qpel_tpc( data, &analysis->l0.me16x16, mb );
                    i_cost = analysis->l0.me16x16.cost
                           + analysis->i_lambda * i_mb_b_cost_table[B_L0_L0];
                }
                else if( i_type == B_L1_L1 )
                {
                     x264_me_refine_qpel_tpc( data, &analysis->l1.me16x16, mb );
                     i_cost = analysis->l1.me16x16.cost
                           + analysis->i_lambda * i_mb_b_cost_table[B_L1_L1];
                }
                else if( i_type == B_BI_BI )
                {
                     x264_me_refine_qpel_tpc( data, &analysis->l0.me16x16, mb );
                     x264_me_refine_qpel_tpc( data, &analysis->l1.me16x16, mb );
                }
                mb->i_partition = i_partition;
            }


           mb->i_type = i_type;
           mb->i_partition = i_partition;

    }


     }

    x264_analyse_update_cache_tpc( data, analysis, mb );

    mb->b_trellis = data->i_trellis;
    mb->b_noise_reduction = !! data->i_noise_reduction;


}

#endif

/********************************************************/
/*-------------------- Update MB from the analysis ----------------------*/
static void x264_analyse_update_cache_tpc( x264_data_t *data, x264_mb_analysis_t *a, x264_mb_t *mb )
{




    int i;
    switch( mb->i_type )
    {

        case I_16x16:
            mb->i_intra16x16_pred_mode = a->i_predict16x16;
            x264_mb_analyse_intra_chroma_tpc( data, a, mb );
            break;

        case I_PCM:
            break;

        case P_L0:
            switch( mb->i_partition )
            {
                case D_16x16:
                    x264_macroblock_cache_ref_tpc( 0, 0, 4, 4, 0, a->l0.me16x16.i_ref, mb );
                    x264_macroblock_cache_mv_ptr_tpc( 0, 0, 4, 4, 0, a->l0.me16x16.mv, mb );

                   break;
                default:

#ifndef NDEBUG
                    fprintf(stderr,  "internal error P_L0 and partition=%d\n", mb->i_partition );
                    SYNC();
#endif
                    break;
            }
            break;

        case P_SKIP:
            mb->i_partition = D_16x16;
            x264_macroblock_cache_ref_tpc(  0, 0, 4, 4, 0, 0, mb );
            x264_macroblock_cache_mv_ptr_tpc(  0, 0, 4, 4, 0, mb->cache.pskip_mv, mb );
            break;
        


        default: /* the rest of the B types */
            switch( mb->i_partition )
            {
            case D_16x16:
                switch( mb->i_type )
                {
                case B_L0_L0:
                    x264_macroblock_cache_ref_tpc   ( 0, 0, 4, 4, 0, a->l0.i_ref,      mb );
                    x264_macroblock_cache_mv_ptr_tpc( 0, 0, 4, 4, 0, a->l0.me16x16.mv, mb );

                    x264_macroblock_cache_ref_tpc( 0, 0, 4, 4, 1, -1, mb );
                    x264_macroblock_cache_mv_tpc ( 0, 0, 4, 4, 1,  0, mb );
                    x264_macroblock_cache_mvd_tpc( 0, 0, 4, 4, 1,  0, mb );
                    break;
                case B_L1_L1:
                    x264_macroblock_cache_ref_tpc( 0, 0, 4, 4, 0, -1, mb );
                    x264_macroblock_cache_mv_tpc ( 0, 0, 4, 4, 0,  0, mb );
                    x264_macroblock_cache_mvd_tpc( 0, 0, 4, 4, 0,  0, mb );

                    x264_macroblock_cache_ref_tpc( 0, 0, 4, 4, 1, a->l1.i_ref, mb );
                    x264_macroblock_cache_mv_ptr_tpc( 0, 0, 4, 4, 1, a->l1.me16x16.mv, mb );
                    break;
                case B_BI_BI:
                    x264_macroblock_cache_ref_tpc( 0, 0, 4, 4, 0, a->l0.i_ref, mb );
                    x264_macroblock_cache_mv_ptr_tpc( 0, 0, 4, 4, 0, a->l0.me16x16.mv, mb );

                    x264_macroblock_cache_ref_tpc( 0, 0, 4, 4, 1, a->l1.i_ref, mb );
                    x264_macroblock_cache_mv_ptr_tpc( 0, 0, 4, 4, 1, a->l1.me16x16.mv, mb );

                    break;
                }
                break;
            default:

#ifndef NDEBUG
                fprintf(stderr, "internal error (invalid MB type): %d\n", mb->i_partition );
		SYNC();
#endif
                break;
            }
    }


/* This is for debug: Cehck the rangfe of the mv */
#if 0

    if( !IS_INTRA(mb->i_type) )
    {
        int l;
        for( l=0; l <= (data->i_type == SLICE_TYPE_B); l++ )
        {
             int ref = mb->cache.ref[l][x264_scan8[0]];
             int mv0 = mb->cache.mv[l][x264_scan8[15]][0];
             int mv1 = mb->cache.mv[l][x264_scan8[15]][1];

	     if (  
			mv0 < (-data->i_me_range )     ||
			mv0 > ( data->i_me_range + 16) ||
			mv1 < (-data->i_me_range )     ||
			mv1 > ( data->i_me_range + 16 )      
			
		){
            	 fprintf(stderr, "--------------------------------------------\n");
            	 fprintf(stderr, "mb type: %d \n", mb->i_type);
            	 fprintf(stderr, "mv: list%dref%d (%d,%d) \n", l, ref,
            	                  mb->cache.mv[l][x264_scan8[15]][0],
            	                  mb->cache.mv[l][x264_scan8[15]][1] );
            	 fprintf(stderr, "limit: spel: %d %d range:%d \n", mb->mv_min_spel[1], mb->mv_max_spel[1], data->i_me_range);
            	 fprintf(stderr, "mb_xy: %d,%d \n", mb->i_mb_x, mb->i_mb_y);
            	 fprintf(stderr, "--------------------------------------------\n");

	    }
        }
    }
#endif


}




