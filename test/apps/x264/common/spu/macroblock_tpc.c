/*****************************************************************************
 * macroblock.c: h264 encoder library
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


#include "common/spu/common_tpc.h"
#include "macroblock_tpc.h"
#include "x264_common_tpc.h"


static NOINLINE void copy_column8( uint8_t *dst, uint8_t *src )
{
    int i;
    for(i=0; i<8; i++)
        dst[i*FDEC_STRIDE] = src[i*FDEC_STRIDE];
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
void x264_mb_predict_mv_ref16x16_tpc( x264_data_t *data , int i_list, int i_ref, int16_t mvc[9][2], int *i_mvc, x264_mb_t *mb )
{
    int16_t (*mvr)[2] = mb->mvr[i_list][i_ref];
    int i = 0;

#define SET_MVP(mvp) { \
        *(uint32_t*)mvc[i] = *(uint32_t*)mvp; \
        i++; \
    }

    /* b_direct */
    if( mb->i_type == SLICE_TYPE_B
        && mb->cache.ref[i_list][x264_scan8[12]] == i_ref )
    {
        SET_MVP( mb->cache.mv[i_list][x264_scan8[12]] );
    }


/* 
 * FIXME: We don't have values for MB_LEFT and MB_TOP so ignore them
 * FIXME: We don't have values for 
 * h->fref0[0]->i_ref[0] and h->sh.b_mbaff
 * TODO: get them from DATA
 * */

#if 0
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
#endif



    if(i == 0)
        *(uint32_t*)mvc[i] = 0;

    *i_mvc = i;

}


static inline void x264_mb_mc_1xywh_tpc( int x, int y, int width, int height, x264_mb_t *mb )
{
    const int i8 = x264_scan8[0]+x+8*y;
    const int i_ref = mb->cache.ref[1][i8];
    const int mvx   = x264_clip3( mb->cache.mv[1][i8][0], mb->mv_min[0], mb->mv_max[0] );
    int       mvy   = x264_clip3( mb->cache.mv[1][i8][1], mb->mv_min[1], mb->mv_max[1] );

    mc_luma( &mb->pic.p_fdec[0][4*y*FDEC_STRIDE+4*x], FDEC_STRIDE,
                   mb->pic.p_fref[1][i_ref], mb->pic.i_stride[0],
                   mvx + 4*4*x, mvy + 4*4*y, 4*width, 4*height );

    if( mb->b_interlaced & i_ref )
        mvy += (mb->i_mb_y & 1)*4 - 2;

    mc_chroma( &mb->pic.p_fdec[1][2*y*FDEC_STRIDE+2*x], FDEC_STRIDE,
                     &mb->pic.p_fref[1][i_ref][4][2*y*mb->pic.i_stride[1]+2*x], mb->pic.i_stride[1],
                     mvx, mvy, 2*width, 2*height );

    mc_chroma( &mb->pic.p_fdec[2][2*y*FDEC_STRIDE+2*x], FDEC_STRIDE,
                     &mb->pic.p_fref[1][i_ref][5][2*y*mb->pic.i_stride[2]+2*x], mb->pic.i_stride[2],
                     mvx, mvy, 2*width, 2*height );
}


static inline void x264_mb_mc_01xywh_tpc( int x, int y, int width, int height, x264_mb_t *mb )
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

    src0 = get_ref( tmp0, &i_stride0, mb->pic.p_fref[0][i_ref0], mb->pic.i_stride[0],
                          mvx0 + 4*4*x, mvy0 + 4*4*y, 4*width, 4*height );
    src1 = get_ref( tmp1, &i_stride1, mb->pic.p_fref[1][i_ref1], mb->pic.i_stride[0],
                          mvx1 + 4*4*x, mvy1 + 4*4*y, 4*width, 4*height );



    avgf[i_mode]( &mb->pic.p_fdec[0][4*y*FDEC_STRIDE+4*x], FDEC_STRIDE,
                       src0, i_stride0, src1, i_stride1, weight );



    if( mb->b_interlaced & i_ref0 )
        mvy0 += (mb->i_mb_y & 1)*4 - 2;
    if( mb->b_interlaced & i_ref1 )
        mvy1 += (mb->i_mb_y & 1)*4 - 2;

    mc_chroma( tmp0, 16, &mb->pic.p_fref[0][i_ref0][4][2*y*mb->pic.i_stride[1]+2*x], mb->pic.i_stride[1],
                     mvx0, mvy0, 2*width, 2*height );
    mc_chroma( tmp1, 16, &mb->pic.p_fref[1][i_ref1][4][2*y*mb->pic.i_stride[1]+2*x], mb->pic.i_stride[1],
                     mvx1, mvy1, 2*width, 2*height );
    avgf[i_mode+3]( &mb->pic.p_fdec[1][2*y*FDEC_STRIDE+2*x], FDEC_STRIDE, tmp0, 16, tmp1, 16, weight );
    mc_chroma( tmp0, 16, &mb->pic.p_fref[0][i_ref0][5][2*y*mb->pic.i_stride[2]+2*x], mb->pic.i_stride[2],
                     mvx0, mvy0, 2*width, 2*height );
    mc_chroma( tmp1, 16, &mb->pic.p_fref[1][i_ref1][5][2*y*mb->pic.i_stride[2]+2*x], mb->pic.i_stride[2],
                     mvx1, mvy1, 2*width, 2*height );
    avgf[i_mode+3]( &mb->pic.p_fdec[2][2*y*FDEC_STRIDE+2*x], FDEC_STRIDE, tmp0, 16, tmp1, 16, weight );


}

#if 0


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

    x264_macroblock_cache_ref( h, 0, 0, 4, 4, 0, ref[0] );
    x264_macroblock_cache_ref( h, 0, 0, 4, 4, 1, ref[1] );
    x264_macroblock_cache_mv_ptr( h, 0, 0, 4, 4, 0, mv[0] );
    x264_macroblock_cache_mv_ptr( h, 0, 0, 4, 4, 1, mv[1] );

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
                        x264_macroblock_cache_mv( h, 2*x8, 2*y8, 2, 2, 0, 0 );
                    if( ref[1] == 0 )
                        x264_macroblock_cache_mv( h, 2*x8, 2*y8, 2, 2, 1, 0 );
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
                            x264_macroblock_cache_mv( h, x4, y4, 1, 1, 0, 0 );
                        if( ref[1] == 0 )
                            x264_macroblock_cache_mv( h, x4, y4, 1, 1, 1, 0 );
                    }
                }
            }
        }
    }

    return 1;
}
#endif



int x264_mb_predict_mv_direct16x16_tpc(  int *b_changed, x264_mb_t *mb )
{
    /* NO Direct mv prediction available */
    return 0;

#if 0
    int b_available;
    if(data->i_direct_mv_pred == X264_DIRECT_PRED_NONE )
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
#endif
	
}
static inline void x264_mb_mc_0xywh_tpc( int x, int y, int width, int height, x264_mb_t *mb )
{
    const int i8 = x264_scan8[0]+x+8*y;
    const int i_ref = mb->cache.ref[0][i8];
    const int mvx   = x264_clip3( mb->cache.mv[0][i8][0], mb->mv_min[0], mb->mv_max[0] );
    int       mvy   = x264_clip3( mb->cache.mv[0][i8][1], mb->mv_min[1], mb->mv_max[1] );

    mc_luma( &mb->pic.p_fdec[0][4*y*FDEC_STRIDE+4*x], FDEC_STRIDE,
                   mb->pic.p_fref[0][i_ref], mb->pic.i_stride[0],
                   mvx + 4*4*x, mvy + 4*4*y, 4*width, 4*height );

    // chroma is offset if MCing from a field of opposite parity
    if( mb->b_interlaced & i_ref )
        mvy += (mb->i_mb_y & 1)*4 - 2;

    mc_chroma( &mb->pic.p_fdec[1][2*y*FDEC_STRIDE+2*x], FDEC_STRIDE,
                     &mb->pic.p_fref[0][i_ref][4][2*y*mb->pic.i_stride[1]+2*x], mb->pic.i_stride[1],
                     mvx, mvy, 2*width, 2*height );

    mc_chroma( &mb->pic.p_fdec[2][2*y*FDEC_STRIDE+2*x], FDEC_STRIDE,
                     &mb->pic.p_fref[0][i_ref][5][2*y*mb->pic.i_stride[2]+2*x], mb->pic.i_stride[2],
                     mvx, mvy, 2*width, 2*height );
}

#if 0
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
#endif
void x264_mb_mc_tpc(x264_data_t *data, x264_mb_t *mb )
{
    if( mb->i_type == P_L0 )
    {
        if( mb->i_partition == D_16x16 )
        {
            x264_mb_mc_0xywh_tpc( 0, 0, 4, 4, mb );
        }else{
	    fprintf(stderr,"Internal error: P_L0 not a 16x16 MB!\n");
	    SYNC();
	}
	

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
            if( b_list0[0] && b_list1[0] ) x264_mb_mc_01xywh_tpc( 0, 0, 4, 4, mb );
            else if( b_list0[0] )          x264_mb_mc_0xywh_tpc (  0, 0, 4, 4, mb );
            else if( b_list1[0] )          x264_mb_mc_1xywh_tpc (  0, 0, 4, 4, mb );
        }

    }
}

#if 0
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

#endif





void x264_mb_predict_mv_tpc( x264_data_t *data, int i_list, int idx, int i_width, int16_t mvp[2], x264_mb_t *mb )
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


