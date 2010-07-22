/*****************************************************************************
 * macroblock_tpc.c: h264 encoder library
 *****************************************************************************
 * Copyright (C) 2003-2008 x264 project
 *
 * Authors: Laurent Aimar <fenrir@via.ecp.fr>
 *          Loren Merritt <lorenm@u.washington.edu>
 *          Jason Garrett-Glaser <darkshikari@gmail.com>
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


#include "common.h"
#include "macroblock_tpc.h"
#include <assert.h>

#ifdef TPC

#include <ppu_intrinsics.h>
#include <altivec.h>




void x264_mb_cache_init(x264_t *h,  int k ){

      int i,j;
      x264_mb_t *mb = h->mb_table[k];
      mb->i_mb_stride = h->sps->i_mb_width;
      mb->i_b8_stride = h->sps->i_mb_width * 2;
      mb->i_b4_stride = h->sps->i_mb_width * 4;


      mb->b_interlaced = h->param.b_interlaced;

      if( h->param.b_cabac )
      {
          mb->chroma_pred_mode = h->mb.chroma_pred_mode ;
          mb->mvd[0] = h->mb.mvd[0] ;
          mb->mvd[1] = h->mb.mvd[1] ;
      }

      mb->type = h->mb.type;

      mb->qp = h->mb.qp;
      mb->cbp = h->mb.cbp;
      mb->skipbp = h->mb.skipbp;
      mb->mb_transform_size = h->mb.mb_transform_size;


      /* 0 -> 3 top(4), 4 -> 6 : left(3) */
      mb->intra4x4_pred_mode = h->mb.intra4x4_pred_mode  ;


    /* all coeffs */
    mb->non_zero_count = (h->mb.non_zero_count);
    mb->nnz_backup = (h->mb.nnz_backup);





    for( i=0; i<2; i++ )
    {
        int i_refs = X264_MIN(16, (i ? 1 : h->param.i_frame_reference) + h->param.b_bframe_pyramid) << h->param.b_interlaced;
        for( j=0; j < i_refs; j++ )
             (mb->mvr[i][j]) = (h->mb.mvr[i][j]);
    }


    for( i=0; i<=h->param.b_interlaced; i++ )
        for( j=0; j<3; j++ )
        {
            mb->intra_border_backup[i][j] = h->mb.intra_border_backup[i][j];
            mb->intra_border_backup[i][j] += 8;
        }



    /* init with not available (for top right idx=7,15) */
    memset( mb->cache.ref[0], -2, X264_SCAN8_SIZE * sizeof( int8_t ) );
    memset( mb->cache.ref[1], -2, X264_SCAN8_SIZE * sizeof( int8_t ) );


      /* fdec:      fenc:
       * yyyyyyy
       * yYYYY      YYYY
       * yYYYY      YYYY
       * yYYYY      YYYY
       * yYYYY      YYYY
       * uuu vvv    UUVV
       * uUU vVV    UUVV
       * uUU vVV
       */
       mb->pic.p_fenc[0] = mb->pic.fenc_buf;
       mb->pic.p_fenc[1] = mb->pic.fenc_buf + 16*FENC_STRIDE;
       mb->pic.p_fenc[2] = mb->pic.fenc_buf + 16*FENC_STRIDE + 8;
       mb->pic.p_fdec[0] = mb->pic.fdec_buf + 2*FDEC_STRIDE;
       mb->pic.p_fdec[1] = mb->pic.fdec_buf + 19*FDEC_STRIDE;
       mb->pic.p_fdec[2] = mb->pic.fdec_buf + 19*FDEC_STRIDE + 16;

       mb->i_neighbour4[6] =
       mb->i_neighbour4[9] =
       mb->i_neighbour4[12] =
       mb->i_neighbour4[14] = MB_LEFT|MB_TOP|MB_TOPLEFT|MB_TOPRIGHT;
       mb->i_neighbour4[3] =
       mb->i_neighbour4[7] =
       mb->i_neighbour4[11] =
       mb->i_neighbour4[13] =
       mb->i_neighbour4[15] =
       mb->i_neighbour8[3] = MB_LEFT|MB_TOP|MB_TOPLEFT;


      /* Other */
      int i_mb_y = k/mb->i_mb_stride;
      int i_mb_x = k%mb->i_mb_stride;
      int i_mb_xy = i_mb_y * mb->i_mb_stride + i_mb_x;
      int i_mb_4x4 = 4*(i_mb_y * mb->i_b4_stride + i_mb_x);
      int i_mb_8x8 = 2*(i_mb_y * mb->i_b8_stride + i_mb_x);
      int i_top_y = i_mb_y - (1 << mb->b_interlaced);
      int i_top_xy = i_top_y * mb->i_mb_stride + i_mb_x;


    /* init index - TODO: This can be executed in init */
    mb->i_mb_x = i_mb_x;
    mb->i_mb_y = i_mb_y;
    mb->i_mb_xy = i_mb_xy;
    mb->i_b8_xy = i_mb_8x8;
    mb->i_b4_xy = i_mb_4x4;
    mb->i_mb_top_xy = i_top_xy;
    mb->i_neighbour = 0;
    mb->i_neighbour4[0] =
    mb->i_neighbour8[0] = (mb->i_neighbour & (MB_TOP|MB_LEFT|MB_TOPLEFT))
                            | ((mb->i_neighbour & MB_TOP) ? MB_TOPRIGHT : 0);
    mb->i_neighbour4[4] =
    mb->i_neighbour4[1] = MB_LEFT | ((mb->i_neighbour & MB_TOP) ? (MB_TOP|MB_TOPLEFT|MB_TOPRIGHT) : 0);
    mb->i_neighbour4[2] =
    mb->i_neighbour4[8] =
    mb->i_neighbour4[10] =
    mb->i_neighbour8[2] = MB_TOP|MB_TOPRIGHT | ((mb->i_neighbour & MB_LEFT) ? (MB_LEFT|MB_TOPLEFT) : 0);
    mb->i_neighbour4[5] =
    mb->i_neighbour8[1] = MB_LEFT | (mb->i_neighbour & MB_TOPRIGHT)
                            | ((mb->i_neighbour & MB_TOP) ? MB_TOP|MB_TOPLEFT : 0);


    int mb_y = k / h->sps->i_mb_width;
    int i_fmv_range = 4 * h->param.analyse.i_mv_range;
    // limit motion search to a slightly smaller range than the theoretical limit,
    // since the search may go a few iterations past its given range
    int i_fpel_border = 5; // umh unconditional radius
    int i_spel_border = 8; // 1.5 for subpel_satd, 1.5 for subpel_rd, 2 for bime, round up

    int mb_height = h->sps->i_mb_height >> h->sh.b_mbaff;
    int thread_mvy_range = i_fmv_range;
    //if  (mb_x==0)   thread_mvy_range = i_fmv_range;

    /* Calculate max allowed MV range */
#define CLIP_FMV(mv) x264_clip3( mv, -i_fmv_range, i_fmv_range-1 )

    mb->mv_min[0] =  X264_MAX( 4*( -16*(h->mb.i_mb_x) - 24 ) ,    -h->param.analyse.i_me_range  );
    mb->mv_min_spel[0] = CLIP_FMV( h->mb.mv_min[0] );
    mb->mv_max_spel[0] = CLIP_FMV( h->mb.mv_max[0] );
    mb->mv_min_fpel[0] = (mb->mv_min_spel[0]>>2) + i_fpel_border;
    mb->mv_max_fpel[0] = (mb->mv_max_spel[0]>>2) - i_fpel_border;
    mb->mv_min[1] = 4*( -16*mb_y - 24 );
    mb->mv_max[1] = 4*( 16*( mb_height - mb_y - 1 ) + 24 );


    mb->mv_min_spel[1] = x264_clip3( mb->mv_min[0], X264_MAX(4*(-512+i_spel_border), -i_fmv_range), i_fmv_range );
    mb->mv_max_spel[1] = CLIP_FMV( mb->mv_max[0] );
    mb->mv_max_spel[1] = X264_MIN( mb->mv_max_spel[0], thread_mvy_range*4 );




#undef CLIP_FMV


}

#ifdef MEMORY_MANAGE
int x264_macroblock_cache_init_tpc( x264_t *h )
{

    return 0;
}
#else
int x264_macroblock_cache_init_tpc( x264_t *h )
{
    int i, j, k;
    int i_mb_count = h->mb.i_mb_count;

    x264_mb_t *mb;



    for ( k=0; k< i_mb_count; k++){
  	x264_mb_cache_init(h,k);
   }
    return 0;
}
#endif
/* Set up a lookup table for delta pocs to reduce an IDIV to an IMUL */
static void setup_inverse_delta_pocs( x264_t *h )
{
    int i;
    for( i = 0; i < h->i_ref0; i++ )
    {
        int delta = h->fdec->i_poc - h->fref0[i]->i_poc;
        h->fdec->inv_ref_poc[i] = (256 + delta/2) / delta;
    }
}

void x264_mb_predict_mv_16x16_tpc(  int i_list, int i_ref, int16_t mvp[2], x264_mb_t *mb )
{
    int     i_refa = mb->cache.ref[i_list][X264_SCAN8_0 - 1];
    int16_t *mv_a  = mb->cache.mv[i_list][X264_SCAN8_0 - 1];
    int     i_refb = mb->cache.ref[i_list][X264_SCAN8_0 - 8];
    int16_t *mv_b  = mb->cache.mv[i_list][X264_SCAN8_0 - 8];
    int     i_refc = mb->cache.ref[i_list][X264_SCAN8_0 - 8 + 4];
    int16_t *mv_c  = mb->cache.mv[i_list][X264_SCAN8_0 - 8 + 4];

    int i_count;

    if( i_refc == -2 )
    {
        i_refc = mb->cache.ref[i_list][X264_SCAN8_0 - 8 - 1];
        mv_c   = mb->cache.mv[i_list][X264_SCAN8_0 - 8 - 1];
    }

    i_count = 0;
    if( i_refa == i_ref ) i_count++;
    if( i_refb == i_ref ) i_count++;
    if( i_refc == i_ref ) i_count++;

    if( i_count > 1 )
        x264_median_mv( mvp, mv_a, mv_b, mv_c );
    else if( i_count == 1 )
    {
        if( i_refa == i_ref )
            *(uint32_t*)mvp = *(uint32_t*)mv_a;
        else if( i_refb == i_ref )
            *(uint32_t*)mvp = *(uint32_t*)mv_b;
        else
            *(uint32_t*)mvp = *(uint32_t*)mv_c;
    }
    else if( i_refb == -2 && i_refc == -2 && i_refa != -2 )
        *(uint32_t*)mvp = *(uint32_t*)mv_a;
    else
        x264_median_mv( mvp, mv_a, mv_b, mv_c );
}

/* This just improves encoder performance, it's not part of the spec */
void x264_mb_predict_mv_ref16x16_tpc( x264_t *h, int i_list, int i_ref, int16_t mvc[9][2], int *i_mvc, x264_mb_t *mb )
{

    int16_t (*mvr)[2] = mb->mvr[i_list][i_ref];
    int i = 0;
#define SET_MVP(mvp) { \
        *(uint32_t*)mvc[i] = *(uint32_t*)mvp; \
        i++; \
    }

    /* b_direct */
    if( h->sh.i_type == SLICE_TYPE_B
        && mb->cache.ref[i_list][x264_scan8[12]] == i_ref )
    {
        SET_MVP( mb->cache.mv[i_list][x264_scan8[12]] );
    }

    if( i_ref == 0 && h->frames.b_have_lowres )
    {
        int16_t (*lowres_mv)[2] = i_list ? h->fenc->lowres_mvs[1][h->fref1[0]->i_frame-h->fenc->i_frame-1]
                                         : h->fenc->lowres_mvs[0][h->fenc->i_frame-h->fref0[0]->i_frame-1];
        if( lowres_mv[0][0] != 0x7fff ) *(uint32_t*)mvc[i++] = (*(uint32_t*)lowres_mv[mb->i_mb_xy]*2)&0xfffeffff;
    }

    /* spatial predictors */
    if( mb->i_neighbour & MB_LEFT )
    {
        int i_mb_l = mb->i_mb_xy - 1;
        /* skip MBs didn't go through the whole search process, so mvr is undefined */
        if( !IS_SKIP( mb->type[i_mb_l] ) )
            SET_MVP( mvr[i_mb_l] );
    }
    if( mb->i_neighbour & MB_TOP )
    {
        int i_mb_t = mb->i_mb_top_xy;
        if( !IS_SKIP( mb->type[i_mb_t] ) )
            SET_MVP( mvr[i_mb_t] );

        if( mb->i_neighbour & MB_TOPLEFT && !IS_SKIP( mb->type[i_mb_t - 1] ) )
            SET_MVP( mvr[i_mb_t-1] );
        if( mb->i_mb_x < mb->i_mb_stride - 1 && !IS_SKIP( mb->type[i_mb_t + 1] ) )
            SET_MVP( mvr[i_mb_t+1] );
    }
#undef SET_MVP

    /* temporal predictors */
    /* FIXME temporal scaling w/ interlace */
    if( h->fref0[0]->i_ref[0] > 0 && !h->sh.b_mbaff )
    {
        x264_frame_t *l0 = h->fref0[0];

#define SET_TMVP(dx, dy) { \
            int i_b4 = mb->i_b4_xy + dx*4 + dy*4*mb->i_b4_stride; \
            int i_b8 = mb->i_b8_xy + dx*2 + dy*2*mb->i_b8_stride; \
            int ref_col = l0->ref[0][i_b8]; \
            if( ref_col >= 0 ) \
            { \
                int scale = (h->fdec->i_poc - h->fdec->ref_poc[0][i_ref]) * l0->inv_ref_poc[ref_col];\
                mvc[i][0] = (l0->mv[0][i_b4][0]*scale + 128) >> 8;\
                mvc[i][1] = (l0->mv[0][i_b4][1]*scale + 128) >> 8;\
                i++; \
            } \
        }

        SET_TMVP(0,0);
        if( mb->i_mb_x < h->sps->i_mb_width-1 )
            SET_TMVP(1,0);
        if( mb->i_mb_y < h->sps->i_mb_height-1 )
            SET_TMVP(0,1);
#undef SET_TMVP
    }

    if(i == 0)
        *(uint32_t*)mvc[i] = 0;

    *i_mvc = i;
}



static inline void x264_mb_mc_1xywh_tpc( x264_t *h, int x, int y, int width, int height, x264_mb_t *mb )
{
    const int i8 = x264_scan8[0]+x+8*y;
    const int i_ref = mb->cache.ref[1][i8];
    const int mvx   = x264_clip3( mb->cache.mv[1][i8][0], mb->mv_min[0], mb->mv_max[0] );
    int       mvy   = x264_clip3( mb->cache.mv[1][i8][1], mb->mv_min[1], mb->mv_max[1] );

    h->mc.mc_luma( &mb->pic.p_fdec[0][4*y*FDEC_STRIDE+4*x], FDEC_STRIDE,
                   mb->pic.p_fref[1][i_ref], mb->pic.i_stride[0],
                   mvx + 4*4*x, mvy + 4*4*y, 4*width, 4*height );

    if( mb->b_interlaced & i_ref )
        mvy += (mb->i_mb_y & 1)*4 - 2;

    h->mc.mc_chroma( &mb->pic.p_fdec[1][2*y*FDEC_STRIDE+2*x], FDEC_STRIDE,
                     &mb->pic.p_fref[1][i_ref][4][2*y*mb->pic.i_stride[1]+2*x], mb->pic.i_stride[1],
                     mvx, mvy, 2*width, 2*height );

    h->mc.mc_chroma( &mb->pic.p_fdec[2][2*y*FDEC_STRIDE+2*x], FDEC_STRIDE,
                     &mb->pic.p_fref[1][i_ref][5][2*y*mb->pic.i_stride[2]+2*x], mb->pic.i_stride[2],
                     mvx, mvy, 2*width, 2*height );
}


static inline void x264_mb_mc_01xywh_tpc( x264_t *h, int x, int y, int width, int height, x264_mb_t *mb )
{
    const int i8 = x264_scan8[0]+x+8*y;
    const int i_ref0 = mb->cache.ref[0][i8];
    const int i_ref1 = mb->cache.ref[1][i8];
    const int weight = mb->bipred_weight[i_ref0][i_ref1];
    const int mvx0   = x264_clip3( mb->cache.mv[0][i8][0], mb->mv_min[0], mb->mv_max[0] );
    const int mvx1   = x264_clip3( mb->cache.mv[1][i8][0], mb->mv_min[0], mb->mv_max[0] );
    int       mvy0   = x264_clip3( mb->cache.mv[0][i8][1], mb->mv_min[1], mb->mv_max[1] );
    int       mvy1   = x264_clip3( mb->cache.mv[1][i8][1], mb->mv_min[1], mb->mv_max[1] );
    int       i_mode = x264_size2pixel[height][width];
    int       i_stride0 = 16, i_stride1 = 16;
    DECLARE_ALIGNED_16( uint8_t tmp0[16*16] );
    DECLARE_ALIGNED_16( uint8_t tmp1[16*16] );
    uint8_t *src0, *src1;
    src0 = h->mc.get_ref( tmp0, &i_stride0, mb->pic.p_fref[0][i_ref0], mb->pic.i_stride[0],
                          mvx0 + 4*4*x, mvy0 + 4*4*y, 4*width, 4*height );
    src1 = h->mc.get_ref( tmp1, &i_stride1, mb->pic.p_fref[1][i_ref1], mb->pic.i_stride[0],
                          mvx1 + 4*4*x, mvy1 + 4*4*y, 4*width, 4*height );
    h->mc.avg[i_mode]( &mb->pic.p_fdec[0][4*y*FDEC_STRIDE+4*x], FDEC_STRIDE,
                       src0, i_stride0, src1, i_stride1, weight );

    if( mb->b_interlaced & i_ref0 )
        mvy0 += (mb->i_mb_y & 1)*4 - 2;
    if( mb->b_interlaced & i_ref1 )
        mvy1 += (mb->i_mb_y & 1)*4 - 2;

    h->mc.mc_chroma( tmp0, 16, &mb->pic.p_fref[0][i_ref0][4][2*y*mb->pic.i_stride[1]+2*x], mb->pic.i_stride[1],
                     mvx0, mvy0, 2*width, 2*height );
    h->mc.mc_chroma( tmp1, 16, &mb->pic.p_fref[1][i_ref1][4][2*y*mb->pic.i_stride[1]+2*x], mb->pic.i_stride[1],
                     mvx1, mvy1, 2*width, 2*height );
    h->mc.avg[i_mode+3]( &mb->pic.p_fdec[1][2*y*FDEC_STRIDE+2*x], FDEC_STRIDE, tmp0, 16, tmp1, 16, weight );
    h->mc.mc_chroma( tmp0, 16, &mb->pic.p_fref[0][i_ref0][5][2*y*mb->pic.i_stride[2]+2*x], mb->pic.i_stride[2],
                     mvx0, mvy0, 2*width, 2*height );
    h->mc.mc_chroma( tmp1, 16, &mb->pic.p_fref[1][i_ref1][5][2*y*mb->pic.i_stride[2]+2*x], mb->pic.i_stride[2],
                     mvx1, mvy1, 2*width, 2*height );
    h->mc.avg[i_mode+3]( &mb->pic.p_fdec[2][2*y*FDEC_STRIDE+2*x], FDEC_STRIDE, tmp0, 16, tmp1, 16, weight );
}


void x264_macroblock_cache_end_tpc( x264_t *h)
{
#if 0
    int i, j, k;

    int i_mb_count = ( (h->param.i_width)/16 )* ( ( h->param.i_height)/16 );

    for ( k=0; k< i_mb_count; k++){

       for( i=0; i<=h->param.b_interlaced; i++ )
           for( j=0; j<3; j++ )
               x264_free( h->mb_table[k]->intra_border_backup[i][j] - 8 );
       for( i=0; i<2; i++ )
           for( j=0; j<32; j++ )
               x264_free( h->mb_table[k]->mvr[i][j] );
       if( h->param.b_cabac )
       {
           x264_free( h->mb_table[k]->chroma_pred_mode );
           x264_free( h->mb_table[k]->mvd[0] );
           x264_free( h->mb_table[k]->mvd[1] );
       }
       x264_free( h->mb_table[k]->intra4x4_pred_mode );
       x264_free( h->mb_table[k]->non_zero_count );
       x264_free( h->mb_table[k]->nnz_backup );
       x264_free( h->mb_table[k]->mb_transform_size );
       x264_free( h->mb_table[k]->skipbp );
       x264_free( h->mb_table[k]->cbp );
       x264_free( h->mb_table[k]->qp );
    }
#endif
}

static int x264_mb_predict_mv_direct16x16_temporal_tpc( x264_t *h, x264_mb_t *mb )
{
    int i_mb_4x4 = 16 * mb->i_mb_stride * mb->i_mb_y + 4 * mb->i_mb_x;
    int i_mb_8x8 =  4 * mb->i_mb_stride * mb->i_mb_y + 2 * mb->i_mb_x;
    int i8, i4;
    int b8x8;
    const int type_col = h->fref1[0]->mb_type[ mb->i_mb_xy ];

    x264_macroblock_cache_ref_tpc(  0, 0, 4, 4, 1, 0, mb );

    if( IS_INTRA( type_col ) )
    {
        x264_macroblock_cache_ref_tpc(  0, 0, 4, 4, 0, 0, mb );
        x264_macroblock_cache_mv_tpc (  0, 0, 4, 4, 0, 0, mb );
        x264_macroblock_cache_mv_tpc (  0, 0, 4, 4, 1, 0, mb );
        return 1;
    }
    b8x8 = h->sps->b_direct8x8_inference ||
           (type_col != P_8x8 && type_col != B_SKIP && type_col != B_DIRECT && type_col != B_8x8);

    for( i8 = 0; i8 < 4; i8++ )
    {
        const int x8 = i8%2;
        const int y8 = i8/2;
        const int i_part_8x8 = i_mb_8x8 + x8 + y8 * mb->i_b8_stride;
        const int i_ref = mb->map_col_to_list0[ h->fref1[0]->ref[0][ i_part_8x8 ] ];

        if( i_ref >= 0 )
        {
            const int dist_scale_factor = mb->dist_scale_factor[i_ref][0];

            x264_macroblock_cache_ref_tpc( 2*x8, 2*y8, 2, 2, 0, i_ref, mb );

            if( b8x8 )
            {
                const int16_t *mv_col = h->fref1[0]->mv[0][ i_mb_4x4 + 3*x8 + 3*y8 * mb->i_b4_stride];
                const int l0x = ( dist_scale_factor * mv_col[0] + 128 ) >> 8;
                const int l0y = ( dist_scale_factor * mv_col[1] + 128 ) >> 8;
                x264_macroblock_cache_mv_tpc( 2*x8, 2*y8, 2, 2, 0, pack16to32_mask(l0x, l0y), mb );
                x264_macroblock_cache_mv_tpc( 2*x8, 2*y8, 2, 2, 1, pack16to32_mask(l0x-mv_col[0], l0y-mv_col[1]), mb );
            }
            else
            {
                for( i4 = 0; i4 < 4; i4++ )
                {
                    const int x4 = i4%2 + 2*x8;
                    const int y4 = i4/2 + 2*y8;
                    const int16_t *mv_col = h->fref1[0]->mv[0][ i_mb_4x4 + x4 + y4 * mb->i_b4_stride ];
                    const int l0x = ( dist_scale_factor * mv_col[0] + 128 ) >> 8;
                    const int l0y = ( dist_scale_factor * mv_col[1] + 128 ) >> 8;
                    x264_macroblock_cache_mv_tpc( x4, y4, 1, 1, 0, pack16to32_mask(l0x, l0y), mb );
                    x264_macroblock_cache_mv_tpc( x4, y4, 1, 1, 1, pack16to32_mask(l0x-mv_col[0], l0y-mv_col[1]), mb );
                }
            }
        }
        else
        {
            /* the collocated ref isn't in the current list0 */
            /* FIXME: we might still be able to use direct_8x8 on some partitions */
            /* FIXME: with B-pyramid + extensive ref list reordering
             *   (not currently used), we would also have to check
             *   l1mv1 like in spatial mode */
            return 0;
        }
    }

    if( h->param.i_threads > 1 )
    {
        int di = b8x8 ? 4 : 1;
        for( i4=0; i4<16; i4+=di )
        {
            if( mb->cache.mv[0][x264_scan8[i4]][1] > mb->mv_max_spel[1]
             || mb->cache.mv[1][x264_scan8[i4]][1] > mb->mv_max_spel[1] )
            {
#if 0
                fprintf(stderr, "direct_temporal: (%d,%d) (%d,%d) > %d \n",
                        mb->cache.mv[0][x264_scan8[i4]][0],
                        mb->cache.mv[0][x264_scan8[i4]][1],
                        mb->cache.mv[1][x264_scan8[i4]][0],
                        mb->cache.mv[1][x264_scan8[i4]][1],
                        mb->mv_max_spel[1]);
#endif
                return 0;
            }
        }
    }

    return 1;
}

static int x264_mb_predict_mv_direct16x16_spatial_tpc( x264_t *h, x264_mb_t *mb )
{
    int ref[2];
    DECLARE_ALIGNED_8( int16_t mv[2][2] );
    int i_list;
    int i8, i4;
    int b8x8;
    const int8_t *l1ref0 = &h->fref1[0]->ref[0][ mb->i_b8_xy ];
    const int8_t *l1ref1 = &h->fref1[0]->ref[1][ mb->i_b8_xy ];
    const int16_t (*l1mv0)[2] = (const int16_t (*)[2]) &h->fref1[0]->mv[0][ mb->i_b4_xy ];
    const int16_t (*l1mv1)[2] = (const int16_t (*)[2]) &h->fref1[0]->mv[1][ mb->i_b4_xy ];
    const int type_col = h->fref1[0]->mb_type[ mb->i_mb_xy ];

    for( i_list=0; i_list<2; i_list++ )
    {
        int i_refa = mb->cache.ref[i_list][X264_SCAN8_0 - 1];
        int i_refb = mb->cache.ref[i_list][X264_SCAN8_0 - 8];
        int i_refc = mb->cache.ref[i_list][X264_SCAN8_0 - 8 + 4];
        if( i_refc == -2 )
            i_refc = mb->cache.ref[i_list][X264_SCAN8_0 - 8 - 1];

        ref[i_list] = i_refa;
        if( ref[i_list] < 0 || ( i_refb < ref[i_list] && i_refb >= 0 ))
            ref[i_list] = i_refb;
        if( ref[i_list] < 0 || ( i_refc < ref[i_list] && i_refc >= 0 ))
            ref[i_list] = i_refc;
        if( ref[i_list] < 0 )
            ref[i_list] = -1;
    }

    if( ref[0] < 0 && ref[1] < 0 )
    {
        ref[0] =
        ref[1] = 0;
        *(uint64_t*)mv[0] = 0;
    }
    else
    {
        for( i_list=0; i_list<2; i_list++ )
        {
            if( ref[i_list] >= 0 )
                x264_mb_predict_mv_16x16_tpc( i_list, ref[i_list], mv[i_list], mb );
            else
                *(uint32_t*)mv[i_list] = 0;
        }
    }

    x264_macroblock_cache_ref_tpc(  0, 0, 4, 4, 0, ref[0], mb );
    x264_macroblock_cache_ref_tpc(  0, 0, 4, 4, 1, ref[1], mb );
    x264_macroblock_cache_mv_ptr_tpc(  0, 0, 4, 4, 0, mv[0], mb );
    x264_macroblock_cache_mv_ptr_tpc(  0, 0, 4, 4, 1, mv[1], mb );

    if( IS_INTRA( type_col ) )
        return 1;

    if( h->param.i_threads > 1
        && ( mv[0][1] > mb->mv_max_spel[1]
          || mv[1][1] > mb->mv_max_spel[1] ) )
    {
#if 0
        fprintf(stderr, "direct_spatial: (%d,%d) (%d,%d) > %d \n",
                mv[0][0], mv[0][1], mv[1][0], mv[1][1],
                mb->mv_max_spel[1]);
#endif
        return 0;
    }

    b8x8 = h->sps->b_direct8x8_inference ||
           (type_col != P_8x8 && type_col != B_SKIP && type_col != B_DIRECT && type_col != B_8x8);

    /* col_zero_flag */
    for( i8=0; i8<4; i8++ )
    {
        const int x8 = i8%2;
        const int y8 = i8/2;
        const int o8 = x8 + y8 * mb->i_b8_stride;
        if( l1ref0[o8] == 0 || ( l1ref0[o8] < 0 && l1ref1[o8] == 0 ) )
        {
            const int16_t (*l1mv)[2] = (l1ref0[o8] == 0) ? l1mv0 : l1mv1;
            if( b8x8 )
            {
                const int16_t *mvcol = l1mv[3*x8 + 3*y8 * mb->i_b4_stride];
                if( abs( mvcol[0] ) <= 1 && abs( mvcol[1] ) <= 1 )
                {
                    if( ref[0] == 0 )
                        x264_macroblock_cache_mv_tpc(  2*x8, 2*y8, 2, 2, 0, 0, mb );
                    if( ref[1] == 0 )
                        x264_macroblock_cache_mv_tpc(  2*x8, 2*y8, 2, 2, 1, 0, mb );
                }
            }
            else
            {
                for( i4=0; i4<4; i4++ )
                {
                    const int x4 = i4%2 + 2*x8;
                    const int y4 = i4/2 + 2*y8;
                    const int16_t *mvcol = l1mv[x4 + y4 * mb->i_b4_stride];
                    if( abs( mvcol[0] ) <= 1 && abs( mvcol[1] ) <= 1 )
                    {
                        if( ref[0] == 0 )
                            x264_macroblock_cache_mv_tpc(  x4, y4, 1, 1, 0, 0, mb );
                        if( ref[1] == 0 )
                            x264_macroblock_cache_mv_tpc(  x4, y4, 1, 1, 1, 0, mb );
                    }
                }
            }
        }
    }

    return 1;
}

int x264_mb_predict_mv_direct16x16_tpc( x264_t *h, int *b_changed, x264_mb_t *mb )
{
    int b_available;
    if( h->param.analyse.i_direct_mv_pred == X264_DIRECT_PRED_NONE )
        return 0;
    else if( h->sh.b_direct_spatial_mv_pred )
        b_available = x264_mb_predict_mv_direct16x16_spatial_tpc( h, mb );
    else
        b_available = x264_mb_predict_mv_direct16x16_temporal_tpc( h, mb );

    if( b_changed != NULL && b_available )
    {
        int type_col = h->fref1[0]->mb_type[ mb->i_mb_xy ];
        if( IS_INTRA(type_col) || type_col == P_SKIP )
        {
            *b_changed = mb->cache.direct_ref[0][0] != mb->cache.ref[0][X264_SCAN8_0]
                      || mb->cache.direct_ref[1][0] != mb->cache.ref[1][X264_SCAN8_0]
                      || *(uint32_t*)mb->cache.direct_mv[0][X264_SCAN8_0] != *(uint32_t*)mb->cache.mv[0][X264_SCAN8_0]
                      || *(uint32_t*)mb->cache.direct_mv[1][X264_SCAN8_0] != *(uint32_t*)mb->cache.mv[1][X264_SCAN8_0];
        }
        else
        {
            int i, l;
            *b_changed = 0;
            for( l = 0; l < 2; l++ )
                for( i = 0; i < 4; i++ )
                    *b_changed |= mb->cache.direct_ref[l][i] != mb->cache.ref[l][x264_scan8[i*4]];
            *b_changed = *b_changed || memcmp(mb->cache.direct_mv, mb->cache.mv, sizeof(mb->cache.mv));
        }
        if( !*b_changed )
            return b_available;
    }

    /* cache ref & mv */
    if( b_available )
    {
        int i, l;
        for( l = 0; l < 2; l++ )
            for( i = 0; i < 4; i++ )
                mb->cache.direct_ref[l][i] = mb->cache.ref[l][x264_scan8[i*4]];
        h->mc.memcpy_aligned(mb->cache.direct_mv, mb->cache.mv, sizeof(mb->cache.mv));
    }

    return b_available;
}

static inline void x264_mb_mc_0xywh_tpc( x264_t *h, int x, int y, int width, int height, x264_mb_t *mb )
{
    const int i8 = x264_scan8[0]+x+8*y;
    const int i_ref = mb->cache.ref[0][i8];
    const int mvx   = x264_clip3( mb->cache.mv[0][i8][0], mb->mv_min[0], mb->mv_max[0] );
    int       mvy   = x264_clip3( mb->cache.mv[0][i8][1], mb->mv_min[1], mb->mv_max[1] );

    h->mc.mc_luma( &mb->pic.p_fdec[0][4*y*FDEC_STRIDE+4*x], FDEC_STRIDE,
                   mb->pic.p_fref[0][i_ref], mb->pic.i_stride[0],
                   mvx + 4*4*x, mvy + 4*4*y, 4*width, 4*height );

    // chroma is offset if MCing from a field of opposite parity
    if( mb->b_interlaced & i_ref )
        mvy += (mb->i_mb_y & 1)*4 - 2;

    h->mc.mc_chroma( &mb->pic.p_fdec[1][2*y*FDEC_STRIDE+2*x], FDEC_STRIDE,
                     &mb->pic.p_fref[0][i_ref][4][2*y*mb->pic.i_stride[1]+2*x], mb->pic.i_stride[1],
                     mvx, mvy, 2*width, 2*height );

    h->mc.mc_chroma( &mb->pic.p_fdec[2][2*y*FDEC_STRIDE+2*x], FDEC_STRIDE,
                     &mb->pic.p_fref[0][i_ref][5][2*y*mb->pic.i_stride[2]+2*x], mb->pic.i_stride[2],
                     mvx, mvy, 2*width, 2*height );
}

static void x264_mb_mc_direct8x8_tpc( x264_t *h, int x, int y, x264_mb_t *mb )
{
    const int i8 = x264_scan8[0] + x + 8*y;

    /* FIXME: optimize based on current block size, not global settings? */
    if( h->sps->b_direct8x8_inference )
    {
        if( mb->cache.ref[0][i8] >= 0 )
            if( mb->cache.ref[1][i8] >= 0 )
                x264_mb_mc_01xywh_tpc( h, x, y, 2, 2, mb );
            else
                x264_mb_mc_0xywh_tpc( h, x, y, 2, 2, mb );
        else
            x264_mb_mc_1xywh_tpc( h, x, y, 2, 2, mb );
    }
    else
    {
        if( mb->cache.ref[0][i8] >= 0 )
        {
            if( mb->cache.ref[1][i8] >= 0 )
            {
                x264_mb_mc_01xywh_tpc( h, x+0, y+0, 1, 1, mb );
                x264_mb_mc_01xywh_tpc( h, x+1, y+0, 1, 1, mb );
                x264_mb_mc_01xywh_tpc( h, x+0, y+1, 1, 1, mb );
                x264_mb_mc_01xywh_tpc( h, x+1, y+1, 1, 1, mb );
            }
            else
            {
                x264_mb_mc_0xywh_tpc( h, x+0, y+0, 1, 1, mb );
                x264_mb_mc_0xywh_tpc( h, x+1, y+0, 1, 1, mb );
                x264_mb_mc_0xywh_tpc( h, x+0, y+1, 1, 1, mb );
                x264_mb_mc_0xywh_tpc( h, x+1, y+1, 1, 1, mb );
            }
        }
        else
        {
            x264_mb_mc_1xywh_tpc( h, x+0, y+0, 1, 1, mb );
            x264_mb_mc_1xywh_tpc( h, x+1, y+0, 1, 1, mb );
            x264_mb_mc_1xywh_tpc( h, x+0, y+1, 1, 1, mb );
            x264_mb_mc_1xywh_tpc( h, x+1, y+1, 1, 1, mb );
        }
    }
}

void x264_mb_mc_8x8_tpc( x264_t *h, int i8, x264_mb_t *mb)
{
    const int x = 2*(i8&1);
    const int y = 2*(i8>>1);
    switch( mb->i_sub_partition[i8] )
    {
        case D_L0_8x8:
            x264_mb_mc_0xywh_tpc( h, x, y, 2, 2, mb );
            break;
        case D_L0_8x4:
            x264_mb_mc_0xywh_tpc( h, x, y+0, 2, 1, mb );
            x264_mb_mc_0xywh_tpc( h, x, y+1, 2, 1, mb );
            break;
        case D_L0_4x8:
            x264_mb_mc_0xywh_tpc( h, x+0, y, 1, 2, mb );
            x264_mb_mc_0xywh_tpc( h, x+1, y, 1, 2, mb );
            break;
        case D_L0_4x4:
            x264_mb_mc_0xywh_tpc( h, x+0, y+0, 1, 1, mb );
            x264_mb_mc_0xywh_tpc( h, x+1, y+0, 1, 1, mb );
            x264_mb_mc_0xywh_tpc( h, x+0, y+1, 1, 1, mb );
            x264_mb_mc_0xywh_tpc( h, x+1, y+1, 1, 1, mb );
            break;
        case D_L1_8x8:
            x264_mb_mc_1xywh_tpc( h, x, y, 2, 2, mb );
            break;
        case D_L1_8x4:
            x264_mb_mc_1xywh_tpc( h, x, y+0, 2, 1, mb );
            x264_mb_mc_1xywh_tpc( h, x, y+1, 2, 1, mb );
            break;
        case D_L1_4x8:
            x264_mb_mc_1xywh_tpc( h, x+0, y, 1, 2, mb );
            x264_mb_mc_1xywh_tpc( h, x+1, y, 1, 2, mb );
            break;
        case D_L1_4x4:
            x264_mb_mc_1xywh_tpc( h, x+0, y+0, 1, 1, mb );
            x264_mb_mc_1xywh_tpc( h, x+1, y+0, 1, 1, mb );
            x264_mb_mc_1xywh_tpc( h, x+0, y+1, 1, 1, mb );
            x264_mb_mc_1xywh_tpc( h, x+1, y+1, 1, 1, mb );
            break;
        case D_BI_8x8:
            x264_mb_mc_01xywh_tpc( h, x, y, 2, 2, mb );
            break;
        case D_BI_8x4:
            x264_mb_mc_01xywh_tpc( h, x, y+0, 2, 1, mb );
            x264_mb_mc_01xywh_tpc( h, x, y+1, 2, 1, mb);
            break;
        case D_BI_4x8:
            x264_mb_mc_01xywh_tpc( h, x+0, y, 1, 2, mb );
            x264_mb_mc_01xywh_tpc( h, x+1, y, 1, 2, mb );
            break;
        case D_BI_4x4:
            x264_mb_mc_01xywh_tpc( h, x+0, y+0, 1, 1, mb );
            x264_mb_mc_01xywh_tpc( h, x+1, y+0, 1, 1, mb );
            x264_mb_mc_01xywh_tpc( h, x+0, y+1, 1, 1, mb );
            x264_mb_mc_01xywh_tpc( h, x+1, y+1, 1, 1, mb );
            break;
        case D_DIRECT_8x8:
            x264_mb_mc_direct8x8_tpc( h, x, y, mb );
            break;
    }
}

void x264_mb_mc_tpc( x264_t *h, x264_mb_t *mb )
{
    if( mb->i_type == P_L0 )
    {
        if( mb->i_partition == D_16x16 )
        {
            x264_mb_mc_0xywh_tpc( h, 0, 0, 4, 4, mb );
        }
        else if( mb->i_partition == D_16x8 )
        {
            x264_mb_mc_0xywh_tpc( h, 0, 0, 4, 2, mb );
            x264_mb_mc_0xywh_tpc( h, 0, 2, 4, 2, mb );
        }
        else if( mb->i_partition == D_8x16 )
        {
            x264_mb_mc_0xywh_tpc( h, 0, 0, 2, 4, mb );
            x264_mb_mc_0xywh_tpc( h, 2, 0, 2, 4, mb );
        }
    }
    else if( mb->i_type == P_8x8 || mb->i_type == B_8x8 )
    {
        int i;
        for( i = 0; i < 4; i++ )
            x264_mb_mc_8x8_tpc( h, i, mb );
    }
    else if( mb->i_type == B_SKIP || mb->i_type == B_DIRECT )
    {
        x264_mb_mc_direct8x8_tpc( h, 0, 0, mb );
        x264_mb_mc_direct8x8_tpc( h, 2, 0, mb );
        x264_mb_mc_direct8x8_tpc( h, 0, 2, mb );
        x264_mb_mc_direct8x8_tpc( h, 2, 2, mb );
    }
    else    /* B_*x* */
    {
        int b_list0[2];
        int b_list1[2];

        int i;

        /* init ref list utilisations */
        for( i = 0; i < 2; i++ )
        {
            b_list0[i] = x264_mb_type_list0_table[mb->i_type][i];
            b_list1[i] = x264_mb_type_list1_table[mb->i_type][i];
        }
        if( mb->i_partition == D_16x16 )
        {
            if( b_list0[0] && b_list1[0] ) x264_mb_mc_01xywh_tpc( h, 0, 0, 4, 4, mb );
            else if( b_list0[0] )          x264_mb_mc_0xywh_tpc ( h, 0, 0, 4, 4, mb );
            else if( b_list1[0] )          x264_mb_mc_1xywh_tpc ( h, 0, 0, 4, 4, mb );
        }
        else if( mb->i_partition == D_16x8 )
        {
            if( b_list0[0] && b_list1[0] ) x264_mb_mc_01xywh_tpc( h, 0, 0, 4, 2, mb );
            else if( b_list0[0] )          x264_mb_mc_0xywh_tpc ( h, 0, 0, 4, 2, mb );
            else if( b_list1[0] )          x264_mb_mc_1xywh_tpc ( h, 0, 0, 4, 2, mb );

            if( b_list0[1] && b_list1[1] ) x264_mb_mc_01xywh_tpc( h, 0, 2, 4, 2, mb );
            else if( b_list0[1] )          x264_mb_mc_0xywh_tpc ( h, 0, 2, 4, 2, mb );
            else if( b_list1[1] )          x264_mb_mc_1xywh_tpc ( h, 0, 2, 4, 2, mb );
        }
        else if( mb->i_partition == D_8x16 )
        {
            if( b_list0[0] && b_list1[0] ) x264_mb_mc_01xywh_tpc( h, 0, 0, 2, 4, mb );
            else if( b_list0[0] )          x264_mb_mc_0xywh_tpc ( h, 0, 0, 2, 4, mb );
            else if( b_list1[0] )          x264_mb_mc_1xywh_tpc ( h, 0, 0, 2, 4, mb );

            if( b_list0[1] && b_list1[1] ) x264_mb_mc_01xywh_tpc( h, 2, 0, 2, 4, mb );
            else if( b_list0[1] )          x264_mb_mc_0xywh_tpc ( h, 2, 0, 2, 4, mb );
            else if( b_list1[1] )          x264_mb_mc_1xywh_tpc ( h, 2, 0, 2, 4, mb );
        }
    }
}


void x264_mb_load_mv_direct8x8_tpc(  int idx, x264_mb_t *mb )
{
    const int x = 2*(idx%2);
    const int y = 2*(idx/2);

    x264_macroblock_cache_ref_tpc( x, y, 2, 2, 0, mb->cache.direct_ref[0][idx], mb );
    x264_macroblock_cache_ref_tpc( x, y, 2, 2, 1, mb->cache.direct_ref[1][idx], mb);

    *(uint64_t*)mb->cache.mv[0][x264_scan8[idx*4]] =
    *(uint64_t*)mb->cache.direct_mv[0][x264_scan8[idx*4]];
    *(uint64_t*)mb->cache.mv[0][x264_scan8[idx*4]+8] =
    *(uint64_t*)mb->cache.direct_mv[0][x264_scan8[idx*4]+8];
    *(uint64_t*)mb->cache.mv[1][x264_scan8[idx*4]] =
    *(uint64_t*)mb->cache.direct_mv[1][x264_scan8[idx*4]];
    *(uint64_t*)mb->cache.mv[1][x264_scan8[idx*4]+8] =
    *(uint64_t*)mb->cache.direct_mv[1][x264_scan8[idx*4]+8];
}



static void ALWAYS_INLINE x264_macroblock_load_pic_pointers_tpc( x264_t *h, int i_mb_x, int i_mb_y, int i, x264_mb_t *mb)
{


    const int w = (i == 0 ? 16 : 8);
    const int i_stride = h->fdec->i_stride[i];
    const int i_stride2 = i_stride << mb->b_interlaced;

    const int i_pix_offset = mb->b_interlaced
                           ? w * (i_mb_x + (i_mb_y&~1) * i_stride) + (i_mb_y&1) * i_stride
                           : w * (i_mb_x + i_mb_y * i_stride);



    int ref_pix_offset[2] = { i_pix_offset, i_pix_offset };



    x264_frame_t **fref[2] = { h->fref0, h->fref1 };
    int j, k;

    mb->pic.i_stride[i] = i_stride2;


    for( j = 0; j < mb->pic.i_fref[0]; j++ )
    {
        mb->pic.p_fref[0][j][i==0 ? 0:i+3] = &fref[0][j >> mb->b_interlaced]->plane[i][ref_pix_offset[j&1]];
        if( i == 0 )
            for( k = 1; k < 4; k++ )
                mb->pic.p_fref[0][j][k] = &fref[0][j >> mb->b_interlaced]->filtered[k][ref_pix_offset[j&1]];
    }
    if( h->sh.i_type == SLICE_TYPE_B )
        for( j = 0; j < mb->pic.i_fref[1]; j++ )
        {
            mb->pic.p_fref[1][j][i==0 ? 0:i+3] = &fref[1][j >> mb->b_interlaced]->plane[i][ref_pix_offset[j&1]];
            if( i == 0 )
                for( k = 1; k < 4; k++ )
                    mb->pic.p_fref[1][j][k] = &fref[1][j >> mb->b_interlaced]->filtered[k][ref_pix_offset[j&1]];
        }
}


static inline void x264_mb_predict_mv_pskip_tpc( x264_t *h, int16_t mv[2], x264_mb_t *mb )
{
    int     i_refa = mb->cache.ref[0][X264_SCAN8_0 - 1];
    int     i_refb = mb->cache.ref[0][X264_SCAN8_0 - 8];
    int16_t *mv_a  = mb->cache.mv[0][X264_SCAN8_0 - 1];
    int16_t *mv_b  = mb->cache.mv[0][X264_SCAN8_0 - 8];

    if( i_refa == -2 || i_refb == -2 ||
        ( i_refa == 0 && *(uint32_t*)mv_a == 0 ) ||
        ( i_refb == 0 && *(uint32_t*)mv_b == 0 ) )
    {
        *(uint32_t*)mv = 0;
    }
    else
    {
        x264_mb_predict_mv_16x16_tpc(  0, 0, mv, mb );
    }
}


#undef CLIP_FMV

//void x264_macroblock_cache_load_tpc( x264_t *h, int i_mb_x, int i_mb_y, x264_mb_t *mb)
void x264_macroblock_cache_load_tpc( x264_t *h, x264_mb_t *mb)
{

#ifdef STAT
	uint64_t   tmp_time1 = __mftb();
#endif

	int i_mb_x = mb->i_mb_x;
	int i_mb_y = mb->i_mb_y;

	int i_mb_xy = mb->i_mb_xy;

	CACHE_ZERO( mb->cache.mv[0]);
	CACHE_ZERO( mb->cache.ref[0]);
	int i_mb_8x8 = 2*(i_mb_y * mb->i_b8_stride + i_mb_x);


	int i_top_y = i_mb_y - (1 << mb->b_interlaced);
	int i_top_xy = i_top_y * mb->i_mb_stride + i_mb_x;
	int i_top_8x8 = (2*i_top_y+1) * mb->i_b8_stride + 2*i_mb_x;

	int i_mb_4x4 = 4*(i_mb_y * mb->i_b4_stride + i_mb_x);
	int i_top_4x4 = (4*i_top_y+3) * mb->i_b4_stride + 4*i_mb_x;

 	///////// PREFETCH


    {
        const int s8x8 = mb->i_b8_stride;
        const int s4x4 = mb->i_b4_stride;

        int i_list;

        for( i_list = 0; i_list < (h->sh.i_type == SLICE_TYPE_B ? 2  : 1 ); i_list++ )
        {
            if( i_mb_x < mb->i_mb_stride - 1 && i_top_xy + 1 >= 0 )
            {
                const int ir = i_top_8x8 + 2;
                const int iv = i_top_4x4 + 4;

                const void *ptr0 = ( &mb->ref[i_list][ir]);
                const void *ptr1 = ( &mb->mv[i_list][iv]);

                CACHE_PREFETCH( ptr0 );
                CACHE_PREFETCH( ptr1 );

            }
    	    if( i_mb_x > 0 && i_top_xy - 1 >= 0 )
            {

                const int ir = i_top_8x8 - 1;
                const int iv = i_top_4x4 - 1;
                const void *ptr0 = (  &mb->ref[i_list][ir]);
                const void *ptr1 = (  &mb->mv[i_list][iv ]);

                CACHE_PREFETCH( ptr0 );
                CACHE_PREFETCH( ptr1 );

            }

	    if( i_top_xy >= 0 )
            {
                const int ir = i_top_8x8;
                const int iv = i_top_4x4;
                const void *ptr0 = (  &mb->ref[i_list][ir + 0]);
                const void *ptr1 = (  &mb->ref[i_list][ir + 1]);
                const void *ptr2 = (  &mb->mv[i_list][iv+0]);
                const void *ptr3 = (  &mb->mv[i_list][iv+2]);


                CACHE_PREFETCH( ptr0 );
                CACHE_PREFETCH( ptr1 );
                CACHE_PREFETCH( ptr2 );
                CACHE_PREFETCH( ptr3 );
            }


            

    	    if( i_mb_x > 0 && i_mb_xy > 0 )
            {
                const int ir = i_mb_8x8 - 1;
                const int iv = i_mb_4x4 - 1 ;

                const void *ptr0 = (&mb->ref[i_list][ir + 0*s8x8]);
		const void *ptr1 = (&mb->ref[i_list][ir + 1*s8x8]);

		const void *ptr2 = (&mb->mv[i_list][iv + 0*s4x4]);
		const void *ptr3 = (&mb->mv[i_list][iv + 1*s4x4]);
		const void *ptr4 = (&mb->mv[i_list][iv + 2*s4x4]);
		const void *ptr5 = (&mb->mv[i_list][iv + 3*s4x4]);


                CACHE_PREFETCH( ptr0 );
                CACHE_PREFETCH( ptr1 );
                CACHE_PREFETCH( ptr2 );
                CACHE_PREFETCH( ptr3 );
                CACHE_PREFETCH( ptr4 );
                CACHE_PREFETCH( ptr5 );

            } 


          }


    }






	////////////////////
	/* load ref/mv/mvd */
    {
        const int s8x8 = mb->i_b8_stride;
        const int s4x4 = mb->i_b4_stride;

        int i_list;

        for( i_list = 0; i_list < (h->sh.i_type == SLICE_TYPE_B ? 2  : 1 ); i_list++ )
        {
            // if( mb->i_neighbour & MB_TOPRIGHT )
            if( i_mb_x < mb->i_mb_stride - 1 && i_top_xy + 1 >= 0 )
            {
                const int i8 = x264_scan8[0] + 4 - 1*8;
                const int ir = i_top_8x8 + 2;
                const int iv = i_top_4x4 + 4;
                mb->cache.ref[i_list][i8]  = mb->ref[i_list][ir];
                *(uint32_t*)mb->cache.mv[i_list][i8] = *(uint32_t*)mb->mv[i_list][iv];
            }
            // if( mb->i_neighbour & MB_TOPLEFT )
    	    if( i_mb_x > 0 && i_top_xy - 1 >= 0 )
            {

                const int i8 = x264_scan8[0] - 1 - 1*8;
                const int ir = i_top_8x8 - 1;
                const int iv = i_top_4x4 - 1;
                mb->cache.ref[i_list][i8]  = mb->ref[i_list][ir];
                *(uint32_t*)mb->cache.mv[i_list][i8] = *(uint32_t*)mb->mv[i_list][iv ];

            }

            //if( mb->i_neighbour & MB_TOP )
	    if( i_top_xy >= 0 )
            {
                const int i8 = x264_scan8[0] - 8;
                const int ir = i_top_8x8;
                const int iv = i_top_4x4;
                mb->cache.ref[i_list][i8+0] =
                mb->cache.ref[i_list][i8+1] = mb->ref[i_list][ir + 0];
                mb->cache.ref[i_list][i8+2] =
                mb->cache.ref[i_list][i8+3] = mb->ref[i_list][ir + 1];
                *(uint64_t*)mb->cache.mv[i_list][i8+0] = *(uint64_t*)mb->mv[i_list][iv+0];
                *(uint64_t*)mb->cache.mv[i_list][i8+2] = *(uint64_t*)mb->mv[i_list][iv+2];
            }


            

            //if( mb->i_neighbour & MB_LEFT )
    	    if( i_mb_x > 0 && i_mb_xy > 0 )
            {
                const int i8 = x264_scan8[0] - 1;
                const int ir = i_mb_8x8 - 1;
                const int iv = i_mb_4x4 - 1 ;
                mb->cache.ref[i_list][i8+0*8] =
                mb->cache.ref[i_list][i8+1*8] = mb->ref[i_list][ir + 0*s8x8];
                mb->cache.ref[i_list][i8+2*8] =
                mb->cache.ref[i_list][i8+3*8] = mb->ref[i_list][ir + 1*s8x8];

                *(uint32_t*)mb->cache.mv[i_list][i8+0*8] = *(uint32_t*)mb->mv[i_list][iv + 0*s4x4];
                *(uint32_t*)mb->cache.mv[i_list][i8+1*8] = *(uint32_t*)mb->mv[i_list][iv + 1*s4x4];
                *(uint32_t*)mb->cache.mv[i_list][i8+2*8] = *(uint32_t*)mb->mv[i_list][iv + 2*s4x4];
                *(uint32_t*)mb->cache.mv[i_list][i8+3*8] = *(uint32_t*)mb->mv[i_list][iv + 3*s4x4];
            } 


          }


    }
         /* load picture pointers */
    x264_macroblock_load_pic_pointers_tpc( h, i_mb_x, i_mb_y, 0, mb );
    x264_macroblock_load_pic_pointers_tpc( h, i_mb_x, i_mb_y, 1, mb);
    x264_macroblock_load_pic_pointers_tpc( h, i_mb_x, i_mb_y, 2, mb );
#ifdef STAT
     stats[h->sh.i_type][CACHE_LOAD] += ( __mftb() - tmp_time1 );
#endif




}


void x264_macroblock_cache_save_tpc( x264_t *h, x264_mb_t *mb )
{


#ifdef STAT
    uint64_t   tmp_time1 = __mftb();
#endif
    const void *ptr0 =  mb->cache.ref[0];
    const void *ptr1 =  mb->cache.mv[0];
    const void *ptr2 =  mb->cache.ref[1];
    const void *ptr3 =  mb->cache.mv[1];
    const void *ptr4 = &mb->cache.non_zero_count[0];

    CACHE_PREFETCH( ptr0 );
    CACHE_PREFETCH( ptr1 );
    CACHE_PREFETCH( ptr2 );
    CACHE_PREFETCH( ptr3 );
    CACHE_PREFETCH( ptr4);


    const int i_mb_xy = mb->i_mb_xy;
    uint8_t *non_zero_count = mb->non_zero_count[i_mb_xy];
    const int s8x8 = mb->i_b8_stride;
    const int s4x4 = mb->i_b4_stride;
    const int i_mb_4x4 = mb->i_b4_xy;
    const int i_mb_8x8 = mb->i_b8_xy;

    int  y;




    mb->ref[0][i_mb_8x8+0+0*s8x8] = mb->cache.ref[0][x264_scan8[0]];
    mb->ref[0][i_mb_8x8+1+0*s8x8] = mb->cache.ref[0][x264_scan8[4]];
    mb->ref[0][i_mb_8x8+0+1*s8x8] = mb->cache.ref[0][x264_scan8[8]];
    mb->ref[0][i_mb_8x8+1+1*s8x8] = mb->cache.ref[0][x264_scan8[12]];

    for(y=0;y<4;y++)
         *(unsigned vector char *)mb->mv[0][i_mb_4x4+y*s4x4+0] = *(unsigned vector char *)mb->cache.mv[0][x264_scan8[0]+8*y+0];

    if( h->sh.i_type == SLICE_TYPE_B )
    {


	    mb->ref[1][i_mb_8x8+0+0*s8x8] = mb->cache.ref[1][x264_scan8[0]];
	    mb->ref[1][i_mb_8x8+1+0*s8x8] = mb->cache.ref[1][x264_scan8[4]];
	    mb->ref[1][i_mb_8x8+0+1*s8x8] = mb->cache.ref[1][x264_scan8[8]];
	    mb->ref[1][i_mb_8x8+1+1*s8x8] = mb->cache.ref[1][x264_scan8[12]];

	for (y=0;y<4;y++)
	    *(unsigned vector char *)mb->mv[1][i_mb_4x4+y*s4x4+0] = *(unsigned vector char *)mb->cache.mv[1][x264_scan8[0]+8*y+0];

    }

    *(uint32_t*)&non_zero_count[0*4] = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[0]+0*8];
    *(uint32_t*)&non_zero_count[1*4] = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[0]+1*8];
    *(uint32_t*)&non_zero_count[2*4] = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[0]+2*8];
    *(uint32_t*)&non_zero_count[3*4] = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[0]+3*8];
    *(uint16_t*)&non_zero_count[16+0*2] = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[16+0*2]-1] >> 8;
    *(uint16_t*)&non_zero_count[16+1*2] = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[16+1*2]-1] >> 8;
    *(uint16_t*)&non_zero_count[16+2*2] = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[16+2*2]-1] >> 8;
    *(uint16_t*)&non_zero_count[16+3*2] = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[16+3*2]-1] >> 8;

#if VISUALIZE
   if( h->param.b_visualize )
	x264_visualize_show_tpc( h, mb,  (h->tpc_id_table[ mb->i_mb_xy ]) >> 28 );
#endif


#ifdef STAT
    stats[h->sh.i_type][CACHE_SAVE] += ( __mftb() - tmp_time1 );
#endif


}

void x264_macroblock_cache_resave_tpc( x264_t *h, x264_mb_t *mb )
{
#ifdef STAT
   uint64_t   tmp_time1 = __mftb();
#endif

    const int i_mb_xy = mb->i_mb_xy;

    /* GCC pessimizes direct stores to heap-allocated 8-bit arrays due to aliasing.*/
    /* By only dereferencing them once, we avoid this issue. */
    uint8_t *non_zero_count = mb->non_zero_count[i_mb_xy];


    {
        /* save non zero count */
        *(uint32_t*)&non_zero_count[0*4] = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[0]+0*8];
        *(uint32_t*)&non_zero_count[1*4] = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[0]+1*8];
        *(uint32_t*)&non_zero_count[2*4] = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[0]+2*8];
        *(uint32_t*)&non_zero_count[3*4] = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[0]+3*8];

        *(uint16_t*)&non_zero_count[16+0*2] = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[16+0*2]-1] >> 8;
        *(uint16_t*)&non_zero_count[16+1*2] = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[16+1*2]-1] >> 8;
        *(uint16_t*)&non_zero_count[16+2*2] = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[16+2*2]-1] >> 8;
        *(uint16_t*)&non_zero_count[16+3*2] = *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[16+3*2]-1] >> 8;
    }



#ifdef STAT
     stats[h->sh.i_type][CACHE_SAVE] += ( __mftb() - tmp_time1 );
#endif




}

void x264_macroblock_slice_init_tpc( x264_t *h, x264_mb_t *mb )
{
    int i;

    mb->mv[0] = h->fdec->mv[0];
    mb->mv[1] = h->fdec->mv[1];
    mb->ref[0] = h->fdec->ref[0];
    mb->ref[1] = h->fdec->ref[1];
    mb->type = h->fdec->mb_type;



    if( h->sh.i_type == SLICE_TYPE_B )
    {


        mb->map_col_to_list0[-1] = -1;
        mb->map_col_to_list0[-2] = -2;
        for( i = 0; i < h->fref1[0]->i_ref[0]; i++ )
        {
            mb->map_col_to_list0[i] = h->mb.map_col_to_list0[i];
        }
    }


    setup_inverse_delta_pocs( h );
}

void x264_mb_predict_mv_tpc( x264_t *h, int i_list, int idx, int i_width, int16_t mvp[2], x264_mb_t *mb )
{
    const int i8 = x264_scan8[idx];
    const int i_ref= mb->cache.ref[i_list][i8];
    int     i_refa = mb->cache.ref[i_list][i8 - 1];
    int16_t *mv_a  = mb->cache.mv[i_list][i8 - 1];
    int     i_refb = mb->cache.ref[i_list][i8 - 8];
    int16_t *mv_b  = mb->cache.mv[i_list][i8 - 8];
    int     i_refc = mb->cache.ref[i_list][i8 - 8 + i_width ];
    int16_t *mv_c  = mb->cache.mv[i_list][i8 - 8 + i_width];

    int i_count;

    if( (idx&0x03) == 3 || ( i_width == 2 && (idx&0x3) == 2 )|| i_refc == -2 )
    {
        i_refc = mb->cache.ref[i_list][i8 - 8 - 1];
        mv_c   = mb->cache.mv[i_list][i8 - 8 - 1];
    }

    if( mb->i_partition == D_16x8 )
    {
        if( idx == 0 && i_refb == i_ref )
        {
            *(uint32_t*)mvp = *(uint32_t*)mv_b;
            return;
        }
        else if( idx != 0 && i_refa == i_ref )
        {
            *(uint32_t*)mvp = *(uint32_t*)mv_a;
            return;
        }
    }
    else if( mb->i_partition == D_8x16 )
    {
        if( idx == 0 && i_refa == i_ref )
        {
            *(uint32_t*)mvp = *(uint32_t*)mv_a;
            return;
        }
        else if( idx != 0 && i_refc == i_ref )
        {
            *(uint32_t*)mvp = *(uint32_t*)mv_c;
            return;
        }
    }

    i_count = 0;
    if( i_refa == i_ref ) i_count++;
    if( i_refb == i_ref ) i_count++;
    if( i_refc == i_ref ) i_count++;

    if( i_count > 1 )
        x264_median_mv( mvp, mv_a, mv_b, mv_c );
    else if( i_count == 1 )
    {
        if( i_refa == i_ref )
            *(uint32_t*)mvp = *(uint32_t*)mv_a;
        else if( i_refb == i_ref )
            *(uint32_t*)mvp = *(uint32_t*)mv_b;
        else
            *(uint32_t*)mvp = *(uint32_t*)mv_c;
    }
    else if( i_refb == -2 && i_refc == -2 && i_refa != -2 )
        *(uint32_t*)mvp = *(uint32_t*)mv_a;
    else
        x264_median_mv( mvp, mv_a, mv_b, mv_c );
}




void x264_macroblock_cache_reload_tpc_mv( x264_t *h, int i_mb_x, int i_mb_y, x264_mb_t *mb )
{
#ifdef STAT
   uint64_t   tmp_time1 = __mftb();
#endif


    const int i_mb_xy = mb->i_mb_xy;
    const int i_top_y = i_mb_y - (1 << mb->b_interlaced);
    const int i_top_xy = i_top_y * mb->i_mb_stride + i_mb_x;
    const int i_left_xy = i_mb_xy - 1;
    CACHE_PREFETCH( mb->cache.non_zero_count)

    /************************************/
    if( i_mb_x > 0 && i_mb_xy > 0 )
	CACHE_PREFETCH( &mb->non_zero_count[i_left_xy][0]);

    /* load cache */
     if( i_top_xy >= 0  )
     {

        /* load non_zero_count */
        *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[0] - 8] = *(uint32_t*)&mb->non_zero_count[i_top_xy][12];
        /* shift because x264_scan8[16] is misaligned */
        *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[16] - 9] = *(uint16_t*)&mb->non_zero_count[i_top_xy][18] << 8;
        *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[16+4] - 9] = *(uint16_t*)&mb->non_zero_count[i_top_xy][22] << 8;
    }
    else
    {

        /* load non_zero_count */
        mb->cache.non_zero_count[x264_scan8[0] - 8] =
        mb->cache.non_zero_count[x264_scan8[1] - 8] =
        mb->cache.non_zero_count[x264_scan8[4] - 8] =
        mb->cache.non_zero_count[x264_scan8[5] - 8] =
        mb->cache.non_zero_count[x264_scan8[16+0] - 8] =
        mb->cache.non_zero_count[x264_scan8[16+1] - 8] =
        mb->cache.non_zero_count[x264_scan8[16+4+0] - 8] =
        mb->cache.non_zero_count[x264_scan8[16+4+1] - 8] = 0x80;
    }
    if( i_mb_x > 0 && i_mb_xy > 0 )
    {

        /* load non_zero_count */
        mb->cache.non_zero_count[x264_scan8[0 ] - 1] = mb->non_zero_count[i_left_xy][3];
        mb->cache.non_zero_count[x264_scan8[2 ] - 1] = mb->non_zero_count[i_left_xy][7];
        mb->cache.non_zero_count[x264_scan8[8 ] - 1] = mb->non_zero_count[i_left_xy][11];
        mb->cache.non_zero_count[x264_scan8[10] - 1] = mb->non_zero_count[i_left_xy][15];

        mb->cache.non_zero_count[x264_scan8[16+0] - 1] = mb->non_zero_count[i_left_xy][16+1];
        mb->cache.non_zero_count[x264_scan8[16+2] - 1] = mb->non_zero_count[i_left_xy][16+3];

        mb->cache.non_zero_count[x264_scan8[16+4+0] - 1] = mb->non_zero_count[i_left_xy][16+4+1];
        mb->cache.non_zero_count[x264_scan8[16+4+2] - 1] = mb->non_zero_count[i_left_xy][16+4+3];

    }
    else
    {
        mb->i_mb_type_left = -1;

        /* load non_zero_count */
        mb->cache.non_zero_count[x264_scan8[0 ] - 1] =
        mb->cache.non_zero_count[x264_scan8[2 ] - 1] =
        mb->cache.non_zero_count[x264_scan8[8 ] - 1] =
        mb->cache.non_zero_count[x264_scan8[10] - 1] =
        mb->cache.non_zero_count[x264_scan8[16+0] - 1] =
        mb->cache.non_zero_count[x264_scan8[16+2] - 1] =
        mb->cache.non_zero_count[x264_scan8[16+4+0] - 1] =
        mb->cache.non_zero_count[x264_scan8[16+4+2] - 1] = 0x80;
    }

#ifdef STAT
     stats[h->sh.i_type][CACHE_LOAD] += ( __mftb() - tmp_time1 );
#endif


}



void x264_macroblock_cache_reload_tpc( x264_t *h, int i_mb_x, int i_mb_y, x264_mb_t *mb )
{

#ifdef STAT
  uint64_t   tmp_time1 = __mftb();
#endif

  int i_left_xy = -1;
  const int i_mb_xy = i_mb_y * mb->i_mb_stride + i_mb_x;
  const int i_top_y = i_mb_y - (1 << mb->b_interlaced);
  const int i_top_xy = i_top_y * mb->i_mb_stride + i_mb_x;
  CACHE_PREFETCH(   mb->cache.non_zero_count);


  /* load cache */
  if( i_top_xy >= h->sh.i_first_mb )
  {

    /* load non_zero_count */
    *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[0] - 8] = *(uint32_t*)&mb->non_zero_count[i_top_xy][12];
    /* shift because x264_scan8[16] is misaligned */
    *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[16] - 9] = *(uint16_t*)&mb->non_zero_count[i_top_xy][18] << 8;
    *(uint32_t*)&mb->cache.non_zero_count[x264_scan8[16+4] - 9] = *(uint16_t*)&mb->non_zero_count[i_top_xy][22] << 8;

  }
  else
  {
    mb->i_mb_type_top = -1;
    /* load non_zero_count */
    mb->cache.non_zero_count[x264_scan8[0] - 8] =
      mb->cache.non_zero_count[x264_scan8[1] - 8] =
      mb->cache.non_zero_count[x264_scan8[4] - 8] =
      mb->cache.non_zero_count[x264_scan8[5] - 8] =
      mb->cache.non_zero_count[x264_scan8[16+0] - 8] =
      mb->cache.non_zero_count[x264_scan8[16+1] - 8] =
      mb->cache.non_zero_count[x264_scan8[16+4+0] - 8] =
      mb->cache.non_zero_count[x264_scan8[16+4+1] - 8] = 0x80;
  }
  if( i_mb_x > 0 && i_mb_xy > h->sh.i_first_mb )
  {
    i_left_xy = i_mb_xy - 1;




    /* load non_zero_count */
    mb->cache.non_zero_count[x264_scan8[0 ] - 1] = mb->non_zero_count[i_left_xy][3];
    mb->cache.non_zero_count[x264_scan8[2 ] - 1] = mb->non_zero_count[i_left_xy][7];
    mb->cache.non_zero_count[x264_scan8[8 ] - 1] = mb->non_zero_count[i_left_xy][11];
    mb->cache.non_zero_count[x264_scan8[10] - 1] = mb->non_zero_count[i_left_xy][15];

    mb->cache.non_zero_count[x264_scan8[16+0] - 1] = mb->non_zero_count[i_left_xy][16+1];
    mb->cache.non_zero_count[x264_scan8[16+2] - 1] = mb->non_zero_count[i_left_xy][16+3];

    mb->cache.non_zero_count[x264_scan8[16+4+0] - 1] = mb->non_zero_count[i_left_xy][16+4+1];
    mb->cache.non_zero_count[x264_scan8[16+4+2] - 1] = mb->non_zero_count[i_left_xy][16+4+3];

  }
  else
  {

    /* load non_zero_count */
    mb->cache.non_zero_count[x264_scan8[0 ] - 1] =
      mb->cache.non_zero_count[x264_scan8[2 ] - 1] =
      mb->cache.non_zero_count[x264_scan8[8 ] - 1] =
      mb->cache.non_zero_count[x264_scan8[10] - 1] =
      mb->cache.non_zero_count[x264_scan8[16+0] - 1] =
      mb->cache.non_zero_count[x264_scan8[16+2] - 1] =
      mb->cache.non_zero_count[x264_scan8[16+4+0] - 1] =
      mb->cache.non_zero_count[x264_scan8[16+4+2] - 1] = 0x80;
  }

#ifdef STAT
  stats[h->sh.i_type][CACHE_LOAD] += ( __mftb() - tmp_time1 );
#endif


}



void x264_macroblock_bipred_init_tpc( x264_t *h )
{
    x264_mb_t *mb;
    int i;

    for ( i=0; i<h->mb.i_mb_count; i++){
    
    mb = h->mb_table[i];
    if (!mb) return;
    int i_ref0, i_ref1;

    for( i_ref0 = 0; i_ref0 < h->i_ref0; i_ref0++ )
    {

     //   int poc0 = h->fref0[i_ref0]->i_poc;
        for( i_ref1 = 0; i_ref1 < h->i_ref1; i_ref1++ )
        {
#if 0
                int dist_scale_factor;
                int poc1 = h->fref1[i_ref1]->i_poc;
                int td = x264_clip3( poc1 - poc0, -128, 127 );
                if( td == 0 /* || pic0 is a long-term ref */ )
                    dist_scale_factor = 256;
                else
                {
                int tb = x264_clip3( h->fdec->i_poc - poc0, -128, 127 );
                tb = x264_clip3( h->fdec->i_poc - poc0, -28, 27 );
                int tx = (16384 + (abs(td) >> 1)) / td;
                   //dist_scale_factor = x264_clip3( (tb * tx + 32) >> 6, -1024, 1023 );
                   dist_scale_factor = x264_clip3( (tb * tx + 32) >> 6, -24, 23 );
                }
                mb->dist_scale_factor[i_ref0][i_ref1] = dist_scale_factor;
                mb->dist_scale_factor[i_ref0][i_ref1] = -16;

                dist_scale_factor >>= 2;
                if( h->param.analyse.b_weighted_bipred
                     && dist_scale_factor >= -64
                     && dist_scale_factor <= 128 )
                   mb->bipred_weight[i_ref0][i_ref1] = 64 - dist_scale_factor;
               else
                   mb->bipred_weight[i_ref0][i_ref1] = 32;
#else
               mb->dist_scale_factor[i_ref0][i_ref1] = h->mb.dist_scale_factor[i_ref0][i_ref1];
               mb->bipred_weight[i_ref0][i_ref1] =  h->mb.bipred_weight[i_ref0][i_ref1];
#endif
           }
       }
       if( h->sh.b_mbaff )
       {
           for( i_ref0 = 2*h->i_ref0-1; i_ref0 >= 0; i_ref0-- )
               for( i_ref1 = 2*h->i_ref1-1; i_ref1 >= 0; i_ref1-- )
                   mb->bipred_weight[i_ref0][i_ref1] = h->mb.bipred_weight[i_ref0>>1][i_ref1>>1];
       }

    } 

}

#endif

