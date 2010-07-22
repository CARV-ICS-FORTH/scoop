/*****************************************************************************
 * analyse.c: h264 encoder library
 *****************************************************************************
 * Copyright (C) 2003-2008 x264 project
 *
 * Authors: Laurent Aimar <fenrir@via.ecp.fr>
 *          Loren Merritt <lorenm@u.washington.edu>
 *

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

        for( j=0; j<4; j++ )
        {
//            x264_cost_mv_fpel[a->i_qp][j] = malloc( (2*128 + 1) * sizeof(int16_t) );
            x264_cost_mv_fpel[a->i_qp][j] = &buffer1[j][0] ;
            x264_cost_mv_fpel[a->i_qp][j] += C_SIZE;
            for( i = -C_SIZE; i < C_SIZE; i++ )
                x264_cost_mv_fpel[a->i_qp][j][i] = p_cost_mv[a->i_qp][i*4+j];
        }


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
     a->b_mbrd = 0;
     mb->b_trellis = 0;
     mb->i_skip_intra = 1;

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
#endif


}




